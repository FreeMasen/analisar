/// This module provides and ast and parser for a more context aware
// parsing scenarios. Primarily this means additional token information
// like the spans that represent a keyword or operator along with any
// comments being attached to a [`crate::aware::ast::Statement`]
use std::{borrow::Cow, collections::VecDeque};

use ast::{
    Args, Attr, BinaryOperator, Block, BlockWithComments, ElseIf, ExpListItem, Expression, Field,
    FieldSep, ForInLoop, ForLoop, FuncBody, FuncName, FuncNamePart, FunctionCall, If,
    LiteralString, Name, NameListPart, Numeral, ParList, ParListPart, RetStatement, Statement,
    StatementWithComments, SuffixSep, Suffixed, SuffixedProperty, Table, UnaryOperator,
};

use lex_lua::{Item, Keyword, Punct, Span, SpannedLexer, Token};
use log::trace;

mod ast;
use crate::error::Error;
use crate::R;

/// A parser that will provide a context-ful ast
pub struct Parser<'a> {
    lex: SpannedLexer<'a>,
    look_ahead: Option<Item<'a>>,
    look_ahead2: Option<Item<'a>>,
    comment_buffer: VecDeque<Item<'a>>,
    bytes: &'a [u8],
}

impl<'a> Parser<'a> {
    pub fn new(bytes: &'a [u8]) -> Self {
        let mut lex = SpannedLexer::new(bytes);
        let mut look_ahead = None;
        let mut look_ahead2 = None;
        let mut comment_buffer = VecDeque::new();
        while let Some(i) = lex.next() {
            if let Token::Comment(_) = &i.token {
                comment_buffer.push_back(i);
            } else {
                look_ahead = Some(i);
                break;
            }
        }
        while let Some(i) = lex.next() {
            if let Token::Comment(_) = &i.token {
                comment_buffer.push_back(i);
            } else {
                look_ahead2 = Some(i);
                break;
            }
        }
        Self {
            lex,
            look_ahead,
            look_ahead2,
            comment_buffer,
            bytes,
        }
    }

    /// Get the next logical section of any lua code as a [`StatementWithComments`]
    pub fn next(&mut self) -> Option<R<StatementWithComments<'a>>> {
        if self.look_ahead.is_none() {
            None
        } else {
            let ret = match self.statement() {
                Ok(statement) => {
                    // if we are at the last block, all comments should be considered
                    // part of this block.
                    let comments = if self.look_ahead.is_none() {
                        std::mem::replace(&mut self.comment_buffer, VecDeque::new()).into()
                    } else {
                        let end = if let Some(end) = statement.end() {
                            end
                        } else {
                            self.look_ahead
                                .as_ref()
                                .map(|i| i.span.end)
                                .unwrap_or(self.bytes.len())
                        };
                        let mut ret = Vec::new();
                        while let Some(item) = self.comment_buffer.front() {
                            if item.span.start < end {
                                ret.push(self.comment_buffer.pop_front().unwrap());
                            } else {
                                // If there is no new line character between the end of our statement and the start
                                // of the next comment in the buffer, we want to include it with this block. A new
                                // line character would push the next comment into the next block.
                                if let Some(bytes) = self.bytes.get(end..item.span.start) {
                                    if !bytes.iter().any(|&b| b == b'\n' || b == b'\r' || b == 0xff)
                                    {
                                        ret.push(self.comment_buffer.pop_front().unwrap());
                                    }
                                }
                                break;
                            }
                        }
                        ret
                    };
                    Ok(StatementWithComments {
                        statement,
                        comments,
                    })
                }
                Err(e) => Err(e),
            };
            Some(ret)
        }
    }

    /// This method will return a full list of the next [`ast::StatementWithComments`]s
    /// if parsing a file, this will parse all the way until the end of the file.
    pub fn block(&mut self) -> R<BlockWithComments<'a>> {
        let mut statements = Vec::new();
        while let Some(stmt) = self.next() {
            match stmt {
                Ok(stmt) => statements.push(stmt),
                Err(e) => return Err(e),
            }
        }
        Ok(BlockWithComments(statements))
    }

    fn block_(&mut self) -> R<Block<'a>> {
        trace!("block {:?}, {:?}", self.look_ahead, self.look_ahead2);
        let mut statements = Vec::new();
        while !self.at_block_end() {
            statements.push(self.statement()?);
        }

        Ok(Block(statements))
    }

    fn statement(&mut self) -> R<Statement<'a>> {
        trace!("statement {:?}, {:?}", self.look_ahead, self.look_ahead2);
        match self.look_ahead() {
            Some(Token::Punct(Punct::SemiColon)) => {
                let semi = self.next_token();
                Ok(Statement::Empty(semi.unwrap().span))
            }
            Some(Token::Keyword(Keyword::Break)) => self.break_stmt(),
            Some(Token::Keyword(Keyword::GoTo)) => self.go_to(),
            Some(Token::Keyword(Keyword::Do)) => self.do_stmt(),
            Some(Token::Keyword(Keyword::While)) => self.while_stmt(),
            Some(Token::Keyword(Keyword::Repeat)) => self.repeat(),
            Some(Token::Keyword(Keyword::If)) => self.if_stmt(),
            Some(Token::Keyword(Keyword::For)) => self.for_loop(),
            Some(Token::Keyword(Keyword::Function)) => self.function(None),
            Some(Token::Keyword(Keyword::Local)) => {
                let local_span = self.expect_keyword(Keyword::Local)?;
                if self.at(Token::Keyword(Keyword::Function)) {
                    self.function(Some(local_span))
                } else {
                    self.assignment(Some(local_span))
                }
            }
            Some(Token::Punct(Punct::DoubleColon)) => self.label(),
            Some(Token::Keyword(Keyword::Return)) => Ok(Statement::Return(self.ret_stat()?)),
            _ => self.exp_stat(),
        }
    }

    fn ret_stat(&mut self) -> R<RetStatement<'a>> {
        trace!("ret_stat {:?}, {:?}", self.look_ahead, self.look_ahead2);
        let return_span = self.expect_keyword(Keyword::Return)?;
        let exprs = if self.eat_punct(Punct::SemiColon).is_some() || self.at_block_end() {
            Vec::new()
        } else {
            self.exp_list()?
        };
        self.eat_punct(Punct::SemiColon);
        Ok(RetStatement { return_span, exprs })
    }

    fn exp_stat(&mut self) -> R<Statement<'a>> {
        trace!("exp_stat {:?}, {:?}", self.look_ahead, self.look_ahead2);
        let base = self.suffixed_exp()?;
        if matches!(
            self.look_ahead(),
            Some(Token::Punct(Punct::Equal)) | Some(Token::Punct(Punct::Comma))
        ) {
            self.assign_cont(None, base)
        } else {
            // TODO Validate function call
            Ok(Statement::Expression(base))
        }
    }

    fn assignment(&mut self, local_span: Option<Span>) -> R<Statement<'a>> {
        trace!(
            "assignment({}) {:?}, {:?}",
            local_span.is_some(),
            self.look_ahead,
            self.look_ahead2
        );
        let mut start = self.suffixed_exp()?;
        if let Expression::Name(n) = &mut start {
            n.attr = self.eat_name_attr()?;
        }
        self.assign_cont(local_span, start)
    }

    fn assign_cont(&mut self, local_span: Option<Span>, start: Expression<'a>) -> R<Statement<'a>> {
        trace!(
            "assign_cont({:?}, {:?}) {:?}, {:?}",
            local_span.is_some(),
            start,
            self.look_ahead,
            self.look_ahead2
        );
        let mut targets = vec![ExpListItem::Expr(start)];
        while let Some(comma_span) = self.eat_punct(Punct::Comma) {
            targets.push(ExpListItem::Comma(comma_span));
            let mut next = self.suffixed_exp()?;
            if let Expression::Name(n) = &mut next {
                n.attr = self.eat_name_attr()?;
            }
            targets.push(ExpListItem::Expr(next))
        }
        let eq_span = self.eat_punct(Punct::Equal);
        let values = if eq_span.is_some() {
            self.exp_list()?
        } else {
            Vec::new()
        };
        Ok(Statement::Assignment {
            local_span,
            targets,
            eq_span,
            values,
        })
    }

    fn label(&mut self) -> R<Statement<'a>> {
        trace!("label {:?}, {:?}", self.look_ahead, self.look_ahead2);
        let colons1_span = self.expect_punct(Punct::DoubleColon)?;
        let name = self.name()?;
        let colons2_span = self.expect_punct(Punct::DoubleColon)?;
        Ok(Statement::Label {
            colons1_span,
            name,
            colons2_span,
        })
    }

    fn break_stmt(&mut self) -> R<Statement<'a>> {
        trace!("break_stmt {:?}, {:?}", self.look_ahead, self.look_ahead2);
        let span = self.expect_keyword(Keyword::Break)?;
        Ok(ast::Statement::Break(span))
    }

    fn go_to(&mut self) -> R<Statement<'a>> {
        trace!("go_to {:?}, {:?}", self.look_ahead, self.look_ahead2);
        let goto_span = self.expect_keyword(Keyword::GoTo)?;
        let label = self.name()?;
        Ok(Statement::GoTo { goto_span, label })
    }

    fn do_stmt(&mut self) -> R<Statement<'a>> {
        trace!("do_stmt {:?}, {:?}", self.look_ahead, self.look_ahead2);
        let do_span = self.expect_keyword(Keyword::Do)?;
        let block = self.block_()?;
        let end_span = self.expect_keyword(Keyword::End)?;
        Ok(Statement::Do {
            do_span,
            block,
            end_span,
        })
    }

    fn while_stmt(&mut self) -> R<Statement<'a>> {
        trace!("while_stmt {:?}, {:?}", self.look_ahead, self.look_ahead2);
        let while_span = self.expect_keyword(Keyword::While)?;
        let exp = self.exp()?;
        let do_span = self.expect_keyword(Keyword::Do)?;
        let block = self.block_()?;
        let end_span = self.expect_keyword(Keyword::End)?;
        Ok(Statement::While {
            while_span,
            exp,
            do_span,
            block,
            end_span,
        })
    }

    fn repeat(&mut self) -> R<Statement<'a>> {
        trace!("repeat {:?}, {:?}", self.look_ahead, self.look_ahead2);
        let repeat_span = self.expect_keyword(Keyword::Repeat)?;
        let block = self.block_()?;
        let until_span = self.expect_keyword(Keyword::Until)?;
        let exp = self.exp()?;
        Ok(Statement::Repeat {
            repeat_span,
            block,
            until_span,
            exp,
        })
    }

    fn if_stmt(&mut self) -> R<Statement<'a>> {
        trace!("if_stmt {:?}, {:?}", self.look_ahead, self.look_ahead2);
        let if_span = self.expect_keyword(Keyword::If)?;
        let test = self.exp()?;
        let then_span = self.expect_keyword(Keyword::Then)?;
        let block = self.block_()?;
        let mut else_ifs = Vec::new();
        while self.at(Token::Keyword(Keyword::ElseIf)) {
            else_ifs.push(self.else_if()?)
        }
        let else_span = self.eat_keyword(Keyword::Else);
        let catch_all = if else_span.is_some() {
            let block = self.block_()?;
            Some(block)
        } else {
            None
        };
        let end_span = self.expect_keyword(Keyword::End)?;
        Ok(Statement::If(If {
            if_span,
            test,
            then_span,
            block,
            else_ifs,
            else_span,
            catch_all,
            end_span,
        }))
    }

    fn else_if(&mut self) -> R<ElseIf<'a>> {
        trace!("else_if {:?}, {:?}", self.look_ahead, self.look_ahead2);
        let else_if_span = self.expect_keyword(Keyword::ElseIf)?;
        let test = self.exp()?;
        let then_span = self.expect_keyword(Keyword::Then)?;
        let block = self.block_()?;
        Ok(ElseIf {
            else_if_span,
            test,
            then_span,
            block,
        })
    }

    fn for_loop(&mut self) -> R<Statement<'a>> {
        trace!("for_loop {:?}, {:?}", self.look_ahead, self.look_ahead2);
        let for_span = self.expect_keyword(Keyword::For)?;
        let init_name = self.name()?;
        if !self.at(Token::Punct(Punct::Equal)) {
            return self.for_in_loop(for_span, init_name);
        }
        let eq_span = self.expect_punct(Punct::Equal)?;
        let exp = self.exp()?;
        let comma1_span = self.expect_punct(Punct::Comma)?;
        let exp2 = self.exp()?;
        let comma2_span = self.eat_punct(Punct::Comma);
        let exp3 = if comma2_span.is_some() {
            Some(self.exp()?)
        } else {
            None
        };
        let do_span = self.expect_keyword(Keyword::Do)?;
        let block = self.block_()?;
        let end_span = self.expect_keyword(Keyword::End)?;
        Ok(Statement::For(ForLoop {
            for_span,
            init_name,
            eq_span,
            init: exp,
            comma1_span,
            limit: exp2,
            comma2_span,
            step: exp3,
            do_span,
            block,
            end_span,
        }))
    }

    fn for_in_loop(&mut self, for_span: Span, first_name: Name<'a>) -> R<Statement<'a>> {
        trace!(
            "for_in_loop ({:?}) {:?}, {:?}",
            first_name,
            self.look_ahead,
            self.look_ahead2
        );
        let name_list = self.name_list_cont(first_name)?;
        let in_span = self.expect_keyword(Keyword::In)?;
        let exp_list = self.exp_list()?;
        let do_span = self.expect_keyword(Keyword::Do)?;
        let block = self.block_()?;
        let end_span = self.expect_keyword(Keyword::End)?;
        Ok(Statement::ForIn(ForInLoop {
            for_span,
            name_list,
            in_span,
            exp_list,
            do_span,
            block,
            end_span,
        }))
    }

    fn name_list_cont(&mut self, first_name: Name<'a>) -> R<Vec<NameListPart<'a>>> {
        trace!(
            "name_list_cont({:?}) {:?}, {:?}",
            first_name,
            self.look_ahead,
            self.look_ahead2
        );
        let mut ret = vec![NameListPart::Name(first_name)];
        while let Some(span) = self.eat_punct(Punct::Comma) {
            ret.push(NameListPart::Comma(span));
            let name = self.name()?;
            ret.push(NameListPart::Name(name));
        }
        Ok(ret)
    }

    fn function(&mut self, local: Option<Span>) -> R<Statement<'a>> {
        trace!("function {:?}, {:?}", self.look_ahead, self.look_ahead2);
        let function = self.expect_keyword(Keyword::Function)?;
        let name = self.func_name()?;
        let body = self.func_body()?;
        Ok(Statement::Function {
            local,
            function,
            name,
            body,
        })
    }

    fn func_name(&mut self) -> R<FuncName<'a>> {
        trace!("func_name {:?}, {:?}", self.look_ahead, self.look_ahead2);
        let mut dot_separated = vec![FuncNamePart::Name(self.name()?)];
        while let Some(span) = self.eat_punct(Punct::Dot) {
            dot_separated.push(FuncNamePart::Dot(span));
            let name = self.name()?;
            dot_separated.push(FuncNamePart::Name(name));
        }
        if let Some(span) = self.eat_punct(Punct::Colon) {
            dot_separated.push(FuncNamePart::Colon(span));
            dot_separated.push(FuncNamePart::Name(self.name()?))
        }
        Ok(FuncName {
            segments: dot_separated,
        })
    }

    fn func_body(&mut self) -> R<FuncBody<'a>> {
        trace!("func_body {:?}, {:?}", self.look_ahead, self.look_ahead2);
        let open_paren_span = self.expect_punct(Punct::OpenParen)?;

        let par_list = self.par_list()?;
        let close_paren_span = self.expect_punct(Punct::CloseParen)?;
        let block = self.block_()?;
        let end = self.expect_keyword(Keyword::End)?;
        Ok(FuncBody {
            open_paren_span,
            par_list,
            close_paren_span,
            block,
            end_span: end,
        })
    }

    fn func_args(&mut self) -> R<Args<'a>> {
        trace!("func_args {:?}, {:?}", self.look_ahead, self.look_ahead2);
        match self.look_ahead() {
            Some(Token::Punct(Punct::OpenParen)) => {
                let open_paren = self.expect_punct(Punct::OpenParen)?;
                let exprs = if matches!(self.look_ahead(), Some(Token::Punct(Punct::CloseParen))) {
                    Vec::new()
                } else {
                    self.exp_list()?
                };
                let close_paren = self.expect_punct(Punct::CloseParen)?;
                Ok(Args::ExpList {
                    open_paren,
                    exprs,
                    close_paren,
                })
            }
            Some(Token::Punct(Punct::OpenBrace)) => {
                let args = self.table_ctor()?;
                Ok(Args::Table(args))
            }
            Some(Token::LiteralString(s)) => {
                let value = s.clone();
                let i = self.next_token().unwrap();
                let arg = LiteralString {
                    span: i.span,
                    value,
                };
                Ok(Args::String(arg))
            }
            _ => self.unexpected_token(Token::Punct(Punct::OpenParen)),
        }
    }

    fn par_list(&mut self) -> R<ParList<'a>> {
        trace!("par_list {:?}, {:?}", self.look_ahead, self.look_ahead2);
        if matches!(self.look_ahead(), Some(Token::Punct(Punct::CloseParen))) {
            return Ok(ParList::empty());
        }
        let mut parts = Vec::new();

        while !matches!(self.look_ahead(), Some(Token::Punct(Punct::CloseParen))) {
            parts.push(self.par_list_entry()?);
            if let Some(comma_span) = self.eat_punct(Punct::Comma) {
                parts.push(ParListPart::Comma(comma_span));
            }
        }

        Ok(ParList { parts })
    }

    fn par_list_entry(&mut self) -> R<ParListPart<'a>> {
        if let Ok(name) = self.name() {
            Ok(ParListPart::Name(name))
        } else {
            let dots_span = self.expect_punct(Punct::Ellipsis)?;
            Ok(ParListPart::VarArgs(dots_span))
        }
    }

    fn name(&mut self) -> R<Name<'a>> {
        trace!("name {:?}, {:?}", self.look_ahead, self.look_ahead2);
        let span = self.look_ahead.as_ref().map(|i| i.span.clone());
        let name = self.expect_name()?;
        Ok(Name {
            // if expect_name provides a value, there will be a span
            name_span: span.unwrap(),
            name,
            attr: None,
        })
    }

    fn eat_name_attr(&mut self) -> R<Option<Attr<'a>>> {
        trace!(
            "eat_name_attr {:?}, {:?}",
            self.look_ahead,
            self.look_ahead2
        );
        if matches!(self.look_ahead(), Some(Token::Punct(Punct::LessThan)))
            && matches!(self.look_ahead2(), Some(Token::Name(_)))
        {
            let open_angle = self.expect_punct(Punct::LessThan)?;
            let value = self.expect_name()?;
            let close_angle = self.expect_punct(Punct::GreaterThan)?;
            Ok(Some(Attr {
                open_angle,
                value,
                close_angle,
            }))
        } else {
            Ok(None)
        }
    }

    fn expect_name(&mut self) -> R<Cow<'a, str>> {
        trace!("expect_name {:?}, {:?}", self.look_ahead, self.look_ahead2);
        let name = if let Some(Token::Name(name)) = self.look_ahead() {
            name.clone()
        } else {
            return if let Some(lh) = &self.look_ahead {
                Err(Error::UnexpectedToken(
                    lh.span.start,
                    format!("Expected name found {:?}", lh.token),
                ))
            } else {
                Err(Error::UnexpectedEof)
            };
        };
        let _ = self.next_token();
        Ok(name)
    }

    fn exp_list(&mut self) -> R<Vec<ExpListItem<'a>>> {
        trace!("exp_list {:?}, {:?}", self.look_ahead, self.look_ahead2);
        let first = self.exp()?;
        let mut ret = vec![ExpListItem::Expr(first)];
        while let Some(span) = self.eat_punct(Punct::Comma) {
            ret.push(ExpListItem::Comma(span));
            ret.push(ExpListItem::Expr(self.exp()?));
        }
        Ok(ret)
    }

    fn exp(&mut self) -> R<Expression<'a>> {
        self.sub_exp(0)
    }

    fn sub_exp(&mut self, limit: u8) -> R<Expression<'a>> {
        trace!(
            "sub_exp({}) {:?}, {:?}",
            limit,
            self.look_ahead,
            self.look_ahead2
        );
        let mut base = if let Some(op) = self.try_get_unary_op() {
            let exp = self.sub_exp(12)?;
            Expression::UnaryOp {
                op,
                exp: Box::new(exp),
            }
        } else {
            self.simple_exp()?
        };
        while let Some(op) = self.try_get_binary_op(limit) {
            let right = self.sub_exp(op.priority().1)?;
            base = Expression::BinOp {
                left: Box::new(base),
                op,
                right: Box::new(right),
            };
        }
        Ok(base)
    }

    fn simple_exp(&mut self) -> R<Expression<'a>> {
        trace!("simple_exp {:?}, {:?}", self.look_ahead, self.look_ahead2);
        let lh = if let Some(lh) = self.look_ahead.as_ref() {
            lh
        } else {
            return Err(Error::UnexpectedEof);
        };
        match &lh.token {
            Token::Numeral(n) => {
                let exp = Expression::Numeral(Numeral {
                    span: lh.span,
                    numeral: n.clone(),
                });
                let _n_tok = self.next_token();
                Ok(exp)
            }
            Token::LiteralString(s) => {
                let exp = Expression::LiteralString(LiteralString {
                    span: lh.span,
                    value: s.clone(),
                });
                let _s_tok = self.next_token();
                Ok(exp)
            }
            Token::Keyword(Keyword::Nil) => {
                let span = lh.span.clone();
                self.expect_keyword(Keyword::Nil)?;
                Ok(Expression::Nil(span))
            }
            Token::Keyword(Keyword::True) => {
                let span = lh.span.clone();
                self.expect_keyword(Keyword::True)?;
                Ok(Expression::True(span))
            }
            Token::Keyword(Keyword::False) => {
                let span = lh.span.clone();
                self.expect_keyword(Keyword::False)?;
                Ok(Expression::False(span))
            }
            Token::Punct(Punct::Ellipsis) => {
                let span = lh.span.clone();
                //TODO: validate in fn
                self.expect_punct(Punct::Ellipsis)?;
                Ok(Expression::VarArgs(span))
            }
            Token::Punct(Punct::OpenBrace) => {
                let inner = self.table_ctor()?;
                Ok(Expression::TableCtor(Box::new(inner)))
            }
            Token::Keyword(Keyword::Function) => {
                self.expect_keyword(Keyword::Function)?;
                let body = self.func_body()?;
                Ok(Expression::FunctionDef(body))
            }
            _ => self.suffixed_exp(),
        }
    }

    fn suffixed_exp(&mut self) -> R<Expression<'a>> {
        trace!("suffixed_exp {:?}, {:?}", self.look_ahead, self.look_ahead2);
        let mut expr = self.primary_exp()?;
        loop {
            match &self.look_ahead() {
                Some(Token::Punct(Punct::Dot)) | Some(Token::Punct(Punct::Colon)) => {
                    expr = self.field(expr)?
                }
                Some(Token::Punct(Punct::OpenBracket)) => expr = self.index(expr)?,
                Some(Token::Punct(Punct::OpenParen))
                | Some(Token::Punct(Punct::OpenBrace))
                | Some(Token::LiteralString(_)) => expr = self.func_call(expr)?,
                _ => return Ok(expr),
            }
        }
    }

    fn field(&mut self, expr: Expression<'a>) -> R<Expression<'a>> {
        trace!("field {:?}, {:?}", self.look_ahead, self.look_ahead2);
        let property = if let Some(span) = self.eat_punct(Punct::Colon) {
            let sep = SuffixSep::Colon(span);
            let name = self.name()?;
            SuffixedProperty::Name { sep, name }
        } else if let Some(span) = self.eat_punct(Punct::Dot) {
            let sep = SuffixSep::Dot(span);
            let name = self.name()?;
            SuffixedProperty::Name { sep, name }
        } else {
            let open_bracket = self.expect_punct(Punct::OpenBracket)?;
            let expr = self.exp()?;
            let close_bracket = self.expect_punct(Punct::CloseBracket)?;
            SuffixedProperty::Computed {
                open_bracket,
                expr,
                close_bracket,
            }
        };
        let inner = Suffixed {
            subject: expr,
            property,
        };
        Ok(Expression::Suffixed(Box::new(inner)))
    }

    fn index(&mut self, expr: Expression<'a>) -> R<Expression<'a>> {
        trace!("index {:?}, {:?}", self.look_ahead, self.look_ahead2);
        let open_bracket = self.expect_punct(Punct::OpenBracket)?;
        let idx = self.exp()?;
        let close_bracket = self.expect_punct(Punct::CloseBracket)?;
        let inner = Suffixed {
            subject: expr,
            property: SuffixedProperty::Computed {
                open_bracket,
                expr: idx,
                close_bracket,
            },
        };
        Ok(Expression::Suffixed(Box::new(inner)))
    }

    fn func_call(&mut self, expr: Expression<'a>) -> R<Expression<'a>> {
        trace!("func_call {:?}, {:?}", self.look_ahead, self.look_ahead2);
        let args = self.func_args()?;

        let call = FunctionCall {
            prefix: Box::new(expr),
            args,
        };
        Ok(Expression::FuncCall(call))
    }

    fn primary_exp(&mut self) -> R<Expression<'a>> {
        trace!("primary_exp {:?}, {:?}", self.look_ahead, self.look_ahead2);
        match &self.look_ahead() {
            Some(Token::Punct(Punct::OpenParen)) => {
                let open_span = self.expect_punct(Punct::OpenParen)?;
                let inner = self.exp()?;
                let close_span = self.expect_punct(Punct::CloseParen)?;
                Ok(Expression::Parened {
                    open_span,
                    expr: Box::new(inner),
                    close_span,
                })
            }
            Some(Token::Name(_)) => {
                let name = self.name()?;
                Ok(Expression::Name(name))
            }
            _ => self.unexpected_token(Token::Punct(Punct::OpenParen))?,
        }
    }

    fn table_ctor(&mut self) -> R<Table<'a>> {
        trace!("table_ctor {:?}, {:?}", self.look_ahead, self.look_ahead2);
        let open_brace = self.expect_punct(Punct::OpenBrace)?;
        let mut field_list = Vec::new();
        let mut close_brace = self.eat_punct(Punct::CloseBrace);
        while close_brace.is_none() {
            field_list.push(self.field_init()?);
            if self.look_ahead.is_none() {
                return Err(Error::UnexpectedEof);
            }
            close_brace = self.eat_punct(Punct::CloseBrace);
        }
        Ok(Table {
            open_brace,
            field_list,
            close_brace: close_brace.unwrap(),
        })
    }

    fn field_init(&mut self) -> R<Field<'a>> {
        trace!("field_init {:?}, {:?}", self.look_ahead, self.look_ahead2);
        let field = if self.at_name() {
            if matches!(self.look_ahead2(), Some(Token::Punct(Punct::Equal))) {
                self.record_field()
            } else {
                self.list_field()
            }
        } else if self.at(Token::Punct(Punct::OpenBracket)) {
            self.record_field()
        } else {
            self.list_field()
        }?;
        Ok(field)
    }

    fn record_field(&mut self) -> R<Field<'a>> {
        trace!("record_field {:?}, {:?}", self.look_ahead, self.look_ahead2);
        let name = if self.at_name() {
            let name = self.name()?;
            Expression::Name(name)
        } else {
            self.expect_punct(Punct::OpenBracket)?;
            let exp = self.exp()?;
            self.expect_punct(Punct::CloseBracket)?;
            exp
        };
        let eq = self.expect_punct(Punct::Equal)?;
        let value = self.exp()?;
        let sep = self.eat_field_sep();
        Ok(Field::Record {
            name,
            eq,
            value,
            sep,
        })
    }

    fn list_field(&mut self) -> R<Field<'a>> {
        trace!("list_field {:?}, {:?}", self.look_ahead, self.look_ahead2);
        let value = self.exp()?;
        let sep = self.eat_field_sep();
        Ok(Field::List { value, sep })
    }

    fn at_block_end(&self) -> bool {
        matches!(
            self.look_ahead(),
            None | Some(Token::Keyword(Keyword::Else))
                | Some(Token::Keyword(Keyword::ElseIf))
                | Some(Token::Keyword(Keyword::End))
                | Some(Token::Keyword(Keyword::Until))
        )
    }

    fn expect_keyword(&mut self, k: Keyword) -> R<Span> {
        if let Some(span) = self.eat_keyword(k) {
            Ok(span)
        } else {
            self.unexpected_token(Token::Keyword(k))
        }
    }

    fn eat_keyword(&mut self, k: Keyword) -> Option<Span> {
        if let Some(Token::Keyword(lh)) = self.look_ahead() {
            if *lh == k {
                let i = self.next_token().unwrap();
                return Some(i.span);
            }
        }
        None
    }

    fn expect_punct(&mut self, p: Punct) -> R<Span> {
        if let Some(span) = self.eat_punct(p) {
            Ok(span)
        } else {
            self.unexpected_token(Token::Punct(p))
        }
    }

    fn eat_field_sep(&mut self) -> Option<FieldSep> {
        if let Some(span) = self.eat_punct(Punct::Comma) {
            Some(FieldSep::Comma(span))
        } else if let Some(span) = self.eat_punct(Punct::SemiColon) {
            Some(FieldSep::Semi(span))
        } else {
            None
        }
    }

    fn eat_punct(&mut self, p: Punct) -> Option<Span> {
        if let Some(Token::Punct(lh)) = self.look_ahead() {
            if *lh == p {
                let i = self.next_token().unwrap();
                return Some(i.span);
            }
        }
        None
    }

    fn at_name(&mut self) -> bool {
        matches!(self.look_ahead(), Some(Token::Name(_)))
    }

    fn at(&mut self, t: Token<'a>) -> bool {
        if let Some(lh) = self.look_ahead() {
            match (lh, &t) {
                (Token::Keyword(lhs), Token::Keyword(rhs)) => lhs == rhs,
                (Token::Punct(lhs), Token::Punct(rhs)) => lhs == rhs,
                (Token::Name(lhs), Token::Name(rhs)) => lhs == rhs,
                (Token::Numeral(lhs), Token::Numeral(rhs)) => lhs == rhs,
                (Token::LiteralString(lhs), Token::LiteralString(rhs)) => lhs == rhs,
                _ => false,
            }
        } else {
            false
        }
    }

    fn next_token(&mut self) -> Option<Item<'a>> {
        use std::mem::replace;
        let mut next2 = dbg!(self.lex.next());
        if let Some(item) = next2.as_ref() {
            if let Token::Comment(_) = &item.token {
                self.comment_buffer.push_back(item.clone());
            }
        }
        while matches!(next2.as_ref().map(|i| &i.token), Some(Token::Comment(_))) {
            next2 = self.lex.next();
            if let Some(item) = next2.as_ref() {
                if let Token::Comment(_) = &item.token {
                    self.comment_buffer.push_back(item.clone());
                }
            }
        }
        replace(&mut self.look_ahead, replace(&mut self.look_ahead2, next2))
    }

    fn try_get_unary_op(&mut self) -> Option<UnaryOperator> {
        let lh_span = self.look_ahead.as_ref().map(|i| i.span.clone());
        let op = match self.look_ahead()? {
            Token::Punct(Punct::Minus) => UnaryOperator::Negate(lh_span.unwrap()),
            Token::Keyword(Keyword::Not) => UnaryOperator::Not(lh_span.unwrap()),
            Token::Punct(Punct::Hash) => UnaryOperator::Length(lh_span.unwrap()),
            Token::Punct(Punct::Tilde) => UnaryOperator::BitwiseNot(lh_span.unwrap()),
            _ => return None,
        };
        let _op_token = self.next_token();
        Some(op)
    }

    fn try_get_binary_op(&mut self, limit: u8) -> Option<BinaryOperator> {
        let span = self.look_ahead.as_ref().map(|i| i.span.clone());
        let op = match self.look_ahead()? {
            Token::Punct(Punct::Plus) => BinaryOperator::Add(span.unwrap()),
            Token::Punct(Punct::Minus) => BinaryOperator::Subtract(span.unwrap()),
            Token::Punct(Punct::Asterisk) => BinaryOperator::Multiply(span.unwrap()),
            Token::Punct(Punct::ForwardSlash) => BinaryOperator::Divide(span.unwrap()),
            Token::Punct(Punct::DoubleForwardSlash) => BinaryOperator::FloorDivide(span.unwrap()),
            Token::Punct(Punct::Caret) => BinaryOperator::Power(span.unwrap()),
            Token::Punct(Punct::Percent) => BinaryOperator::Modulo(span.unwrap()),
            Token::Punct(Punct::Ampersand) => BinaryOperator::BitwiseAnd(span.unwrap()),
            Token::Punct(Punct::Tilde) => BinaryOperator::BitwiseXor(span.unwrap()),
            Token::Punct(Punct::Pipe) => BinaryOperator::BitwiseOr(span.unwrap()),
            Token::Punct(Punct::DoubleGreaterThan) => BinaryOperator::RightShift(span.unwrap()),
            Token::Punct(Punct::DoubleLessThan) => BinaryOperator::LeftShift(span.unwrap()),
            Token::Punct(Punct::DoubleDot) => BinaryOperator::Concatenate(span.unwrap()),
            Token::Punct(Punct::LessThan) => BinaryOperator::LessThan(span.unwrap()),
            Token::Punct(Punct::GreaterThan) => BinaryOperator::GreaterThan(span.unwrap()),
            Token::Punct(Punct::LessThanEqual) => BinaryOperator::LessThanEqual(span.unwrap()),
            Token::Punct(Punct::GreaterThanEqual) => {
                BinaryOperator::GreaterThanEqual(span.unwrap())
            }
            Token::Punct(Punct::DoubleEqual) => BinaryOperator::Equal(span.unwrap()),
            Token::Punct(Punct::TildeEqual) => BinaryOperator::NotEqual(span.unwrap()),
            Token::Keyword(Keyword::And) => BinaryOperator::And(span.unwrap()),
            Token::Keyword(Keyword::Or) => BinaryOperator::Or(span.unwrap()),
            _ => return None,
        };
        if op.priority().0 > limit {
            let _op_tok = self.next_token();
            Some(op)
        } else {
            None
        }
    }

    fn look_ahead(&self) -> Option<&Token<'a>> {
        self.look_ahead.as_ref().map(|i| &i.token)
    }

    fn look_ahead2(&self) -> Option<&Token<'a>> {
        self.look_ahead2.as_ref().map(|i| &i.token)
    }

    fn unexpected_token<T>(&self, t: Token) -> R<T> {
        if let Some(lh) = &self.look_ahead {
            Err(Error::UnexpectedToken(
                lh.span.start,
                format!("Expected {:?} found {:?}", t, lh.token),
            ))
        } else {
            Err(Error::UnexpectedEof)
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use ast::Block;

    #[test]
    fn print() {
        pretty_env_logger::try_init().ok();
        let lua = "print('hello world')";
        let name = Expression::name_from("print", 0);
        let arg = Expression::string_from("'hello world'", 6);
        let args = Args::ExpList {
            open_paren: Span { start: 5, end: 6 },
            exprs: vec![ExpListItem::Expr(arg)],
            close_paren: Span {
                start: lua.len() - 1,
                end: lua.len(),
            },
        };
        parse_and_compare(
            lua,
            Block(vec![Statement::Expression(Expression::FuncCall(
                FunctionCall {
                    prefix: Box::new(name),
                    args: args,
                },
            ))]),
        );
    }
    #[test]
    fn require() {
        pretty_env_logger::try_init().ok();
        let lua = "require 'lib'";
        let name = Expression::name_from("require", 0);
        let arg = LiteralString {
            span: Span {
                start: 8,
                end: lua.len(),
            },
            value: Cow::Borrowed("'lib'".into()),
        };
        let args = Args::String(arg);
        parse_and_compare(
            lua,
            Block(vec![Statement::Expression(Expression::FuncCall(
                FunctionCall {
                    prefix: Box::new(name),
                    args: args,
                },
            ))]),
        );
    }

    #[test]
    fn callback() {
        pretty_env_logger::try_init().ok();
        let lua = "pcall(function () end)";
        let name = Expression::name_from("pcall", 0);
        let cb = Expression::FunctionDef(FuncBody {
            block: Block::empty(),
            open_paren_span: Span { start: 15, end: 16 },
            par_list: ParList { parts: Vec::new() },
            close_paren_span: Span { start: 16, end: 17 },
            end_span: Span { start: 18, end: 21 },
        });
        let args = Args::ExpList {
            open_paren: Span { start: 5, end: 6 },
            exprs: vec![ExpListItem::Expr(cb)],
            close_paren: Span {
                start: lua.len() - 1,
                end: lua.len(),
            },
        };
        parse_and_compare(
            lua,
            Block(vec![Statement::Expression(Expression::FuncCall(
                FunctionCall {
                    prefix: Box::new(name),
                    args: args,
                },
            ))]),
        );
    }
    #[test]
    fn nested_calls() {
        pretty_env_logger::try_init().ok();
        let lua = "print(error())";
        let name = Expression::name_from("print", 0);
        let name2 = Expression::name_from("error", 6);
        let inner_call = Expression::FuncCall(FunctionCall {
            prefix: Box::new(name2),
            args: Args::ExpList {
                open_paren: Span { start: 11, end: 12 },
                exprs: Vec::new(),
                close_paren: Span { start: 12, end: 13 },
            },
        });
        parse_and_compare(
            lua,
            Block(vec![Statement::Expression(Expression::FuncCall(
                FunctionCall {
                    prefix: Box::new(name),
                    args: Args::ExpList {
                        open_paren: Span { start: 5, end: 6 },
                        exprs: vec![ExpListItem::Expr(inner_call)],
                        close_paren: Span { start: 13, end: 14 },
                    },
                },
            ))]),
        );
    }

    #[test]
    fn chained_calls() {
        pretty_env_logger::try_init().ok();
        let lua = "f()()";
        let name = Expression::name_from("f", 0);
        let call = Expression::FuncCall(FunctionCall {
            prefix: Box::new(name),
            args: Args::ExpList {
                open_paren: Span { start: 1, end: 2 },
                exprs: vec![],
                close_paren: Span { start: 2, end: 3 },
            },
        });
        let call2 = Statement::Expression(Expression::FuncCall(FunctionCall {
            prefix: Box::new(call),
            args: Args::ExpList {
                open_paren: Span { start: 3, end: 4 },
                exprs: vec![],
                close_paren: Span { start: 4, end: 5 },
            },
        }));
        parse_and_compare(lua, Block(vec![call2]));
    }
    #[test]
    fn if_elseif_else() {
        pretty_env_logger::try_init().ok();
        let lua = "if true then elseif false then else end";
        let stmt = Statement::If(If {
            if_span: Span { start: 0, end: 2 },
            test: Expression::True(Span { start: 3, end: 7 }),
            then_span: Span { start: 8, end: 12 },
            block: Block::empty(),
            else_ifs: vec![ElseIf {
                else_if_span: Span { start: 13, end: 19 },
                test: Expression::False(Span { start: 20, end: 25 }),
                then_span: Span { start: 26, end: 30 },
                block: Block::empty(),
            }],
            else_span: Some(Span { start: 31, end: 35 }),
            catch_all: Some(Block::empty()),
            end_span: Span { start: 36, end: 39 },
        });
        parse_and_compare(lua, Block(vec![stmt]));
    }

    #[test]
    fn local_function() {
        pretty_env_logger::try_init().ok();
        let lua = "local function thing()
            local a = 0
            return a
        end";
        parse_and_compare(
            lua,
            Block(vec![Statement::Function {
                local: Some(Span { start: 0, end: 5 }),
                function: Span { start: 6, end: 14 },
                name: FuncName {
                    segments: vec![FuncNamePart::Name(Name::from_str("thing", 15))],
                },
                body: FuncBody {
                    open_paren_span: Span { start: 20, end: 21 },
                    par_list: ParList::empty(),
                    close_paren_span: Span { start: 21, end: 22 },
                    block: Block(vec![
                        Statement::Assignment {
                            local_span: Some(Span { start: 35, end: 40 }),
                            targets: vec![ExpListItem::Expr(Expression::name_from("a", 41))],
                            eq_span: Some(Span { start: 43, end: 44 }),
                            values: vec![ExpListItem::Expr(Expression::numeral_from("0", 45))],
                        },
                        Statement::Return(RetStatement {
                            return_span: Span { start: 59, end: 65 },
                            exprs: vec![ExpListItem::Expr(Expression::name_from("a", 66))],
                        }),
                    ]),
                    end_span: Span { start: 76, end: 79 },
                },
            }]),
        );
    }

    #[test]
    fn module_return() {
        let lua = "local a = 0
        local b = 1
        local c = nil
        
        return {
            a = a,
            b = b,
            c = c,
        }";
        parse_and_compare(
            lua,
            Block(vec![
                Statement::Assignment {
                    local_span: Some(Span { start: 0, end: 5 }),
                    targets: vec![ExpListItem::Expr(Expression::name_from("a", 6))],
                    eq_span: Some(Span { start: 8, end: 9 }),
                    values: vec![ExpListItem::Expr(Expression::numeral_from("0", 10))],
                },
                Statement::Assignment {
                    local_span: Some(Span { start: 20, end: 25 }),
                    targets: vec![ExpListItem::Expr(Expression::name_from("b", 26))],
                    eq_span: Some(Span { start: 28, end: 29 }),
                    values: vec![ExpListItem::Expr(Expression::numeral_from("1", 30))],
                },
                Statement::Assignment {
                    local_span: Some(Span { start: 40, end: 45 }),
                    targets: vec![ExpListItem::Expr(Expression::name_from("c", 46))],
                    eq_span: Some(Span { start: 48, end: 49 }),
                    values: vec![ExpListItem::Expr(Expression::Nil(Span {
                        start: 50,
                        end: 53,
                    }))],
                },
                Statement::Return(RetStatement {
                    return_span: Span { start: 71, end: 77 },
                    exprs: vec![ExpListItem::Expr(Expression::TableCtor(Box::new(Table {
                        open_brace: Span { start: 78, end: 79 },
                        field_list: vec![
                            Field::Record {
                                name: Expression::name_from("a", 92),
                                eq: Span { start: 94, end: 95 },
                                value: Expression::name_from("a", 96),
                                sep: Some(FieldSep::Comma(Span { start: 97, end: 98 })),
                            },
                            Field::Record {
                                name: Expression::name_from("b", 111),
                                eq: Span {
                                    start: 113,
                                    end: 114,
                                },
                                value: Expression::name_from("b", 115),
                                sep: Some(FieldSep::Comma(Span {
                                    start: 116,
                                    end: 117,
                                })),
                            },
                            Field::Record {
                                name: Expression::name_from("c", 130),
                                eq: Span {
                                    start: 132,
                                    end: 133,
                                },
                                value: Expression::name_from("c", 134),
                                sep: Some(FieldSep::Comma(Span {
                                    start: 135,
                                    end: 136,
                                })),
                            },
                        ],
                        close_brace: Span {
                            start: 145,
                            end: 146,
                        },
                    })))],
                }),
            ]),
        );
    }
    #[test]
    fn assignments() {
        let lua = "local a = {}
        a.b = 1
        a['c'] = 2
        ";
        parse_and_compare(
            lua,
            Block(vec![
                Statement::Assignment {
                    local_span: Some(Span { start: 0, end: 5 }),
                    targets: vec![ExpListItem::Expr(Expression::name_from("a", 6))],
                    eq_span: Some(Span { start: 8, end: 9 }),
                    values: vec![ExpListItem::Expr(Expression::TableCtor(Box::new(Table {
                        open_brace: Span { start: 10, end: 11 },
                        field_list: vec![],
                        close_brace: Span { start: 11, end: 12 },
                    })))],
                },
                Statement::Assignment {
                    local_span: None,
                    targets: vec![ExpListItem::Expr(Expression::Suffixed(Box::new(
                        Suffixed {
                            subject: Expression::name_from("a", 21),
                            property: SuffixedProperty::Name {
                                sep: SuffixSep::Dot(Span { start: 22, end: 23 }),
                                name: Name {
                                    name: Cow::Borrowed("b"),
                                    name_span: Span { start: 23, end: 24 },
                                    attr: None,
                                },
                            },
                        },
                    )))],
                    eq_span: Some(Span { start: 25, end: 26 }),
                    values: vec![ExpListItem::Expr(Expression::numeral_from("1", 27))],
                },
                Statement::Assignment {
                    local_span: None,
                    targets: vec![ExpListItem::Expr(Expression::Suffixed(Box::new(
                        Suffixed {
                            subject: Expression::name_from("a", 37),
                            property: SuffixedProperty::Computed {
                                open_bracket: Span { start: 38, end: 39 },
                                expr: Expression::string_from("'c'", 39),
                                close_bracket: Span { start: 42, end: 43 },
                            },
                        },
                    )))],
                    eq_span: Some(Span { start: 44, end: 45 }),
                    values: vec![ExpListItem::Expr(Expression::numeral_from("2", 46))],
                },
            ]),
        );
    }

    #[test]
    fn table_ctor_and_access() {
        pretty_env_logger::try_init().ok();
        let lua = "a = {
            one = 'one',
            two = 'two',
        }
        print(a.one)
        print(a.two)
        ";
        parse_and_compare(
            lua,
            Block(vec![
                Statement::Assignment {
                    local_span: None,
                    targets: vec![ExpListItem::Expr(Expression::name_from("a", 0))],
                    eq_span: Some(Span { start: 2, end: 3 }),
                    values: vec![ExpListItem::Expr(Expression::TableCtor(Box::new(Table {
                        open_brace: Span { start: 4, end: 5 },
                        field_list: vec![
                            Field::Record {
                                name: Expression::name_from("one", 18),
                                eq: Span { start: 22, end: 23 },
                                value: Expression::string_from("'one'", 24),
                                sep: Some(FieldSep::Comma(Span { start: 29, end: 30 })),
                            },
                            Field::Record {
                                name: Expression::name_from("two", 43),
                                eq: Span { start: 47, end: 48 },
                                value: Expression::string_from("'two'", 49),
                                sep: Some(FieldSep::Comma(Span { start: 54, end: 55 })),
                            },
                        ],
                        close_brace: Span { start: 64, end: 65 },
                    })))],
                },
                Statement::Expression(Expression::FuncCall(FunctionCall {
                    prefix: Box::new(Expression::name_from("print", 74)),
                    args: Args::ExpList {
                        open_paren: Span { start: 79, end: 80 },
                        exprs: vec![ExpListItem::Expr(Expression::Suffixed(Box::new(
                            Suffixed {
                                subject: Expression::name_from("a", 80),
                                property: SuffixedProperty::Name {
                                    sep: SuffixSep::Dot(Span { start: 81, end: 82 }),
                                    name: Name::from_str("one", 82),
                                },
                            },
                        )))],
                        close_paren: Span { start: 85, end: 86 },
                    },
                })),
                Statement::Expression(Expression::FuncCall(FunctionCall {
                    prefix: Box::new(Expression::name_from("print", 95)),
                    args: Args::ExpList {
                        open_paren: Span {
                            start: 100,
                            end: 101,
                        },
                        exprs: vec![ExpListItem::Expr(Expression::Suffixed(Box::new(
                            Suffixed {
                                subject: Expression::name_from("a", 101),
                                property: SuffixedProperty::Name {
                                    sep: SuffixSep::Dot(Span {
                                        start: 102,
                                        end: 103,
                                    }),
                                    name: Name::from_str("two", 103),
                                },
                            },
                        )))],
                        close_paren: Span {
                            start: 106,
                            end: 107,
                        },
                    },
                })),
            ]),
        );
    }

    #[test]
    fn label_and_goto() {
        let lua = "
        ::top::
        print('loop')
        goto top
        ";
        let label = Statement::Label {
            colons1_span: Span { start: 9, end: 11 },
            name: Name::from_str("top", 11),
            colons2_span: Span { start: 14, end: 16 },
        };

        let call = Statement::Expression(Expression::FuncCall(FunctionCall {
            prefix: Box::new(Expression::name_from("print", 25)),
            args: Args::ExpList {
                open_paren: Span { start: 30, end: 31 },
                exprs: vec![ExpListItem::Expr(Expression::string_from("'loop'", 31))],
                close_paren: Span { start: 37, end: 38 },
            },
        }));
        let goto = Statement::GoTo {
            goto_span: Span { start: 47, end: 51 },
            label: Name::from_str("top", 52),
        };
        parse_and_compare(lua, Block(vec![label, call, goto]));
    }

    #[test]
    fn for_and_break() {
        let lua = "
        -- early comment
        for i = 0, 10, 1 do
            break
        end
        ";
        parse_and_compare(
            lua,
            Block(vec![Statement::For(ForLoop {
                for_span: Span { start: 34, end: 37 },
                init_name: Name::from_str("i", 38),
                eq_span: Span { start: 40, end: 41 },
                init: Expression::numeral_from("0", 42),
                comma1_span: Span { start: 43, end: 44 },
                limit: Expression::numeral_from("10", 45),
                comma2_span: Some(Span { start: 47, end: 48 }),
                step: Some(Expression::numeral_from("1", 49)),
                do_span: Span { start: 51, end: 53 },
                block: Block(vec![Statement::Break(Span { start: 66, end: 71 })]),
                end_span: Span { start: 80, end: 83 },
            })]),
        );
    }

    #[test]
    fn for_in() {
        let lua = "
        for i, v in ipairs({1,2,3}) do
            print(i)
        end
        ";
        let table = Expression::TableCtor(Box::new(Table {
            open_brace: Span { start: 28, end: 29 },
            field_list: vec![
                Field::List {
                    value: Expression::numeral_from("1", 29),
                    sep: Some(FieldSep::Comma(Span { start: 30, end: 31 })),
                },
                Field::List {
                    value: Expression::numeral_from("2", 31),
                    sep: Some(FieldSep::Comma(Span { start: 32, end: 33 })),
                },
                Field::List {
                    value: Expression::numeral_from("3", 33),
                    sep: None,
                },
            ],
            close_brace: Span { start: 34, end: 35 },
        }));
        let args = Args::ExpList {
            open_paren: Span { start: 27, end: 28 },
            exprs: vec![ExpListItem::Expr(table)],
            close_paren: Span { start: 35, end: 36 },
        };
        parse_and_compare(
            lua,
            Block(vec![Statement::ForIn(ForInLoop {
                for_span: Span { start: 9, end: 12 },
                name_list: vec![
                    NameListPart::Name(Name::from_str("i", 13)),
                    NameListPart::Comma(Span { start: 14, end: 15 }),
                    NameListPart::Name(Name::from_str("v", 16)),
                ],
                in_span: Span { start: 18, end: 20 },
                exp_list: vec![ExpListItem::Expr(Expression::FuncCall(FunctionCall {
                    prefix: Box::new(Expression::name_from("ipairs", 21)),
                    args: args,
                }))],
                do_span: Span { start: 37, end: 39 },
                block: Block(vec![Statement::Expression(Expression::FuncCall(
                    FunctionCall {
                        prefix: Box::new(Expression::name_from("print", 52)),
                        args: Args::ExpList {
                            open_paren: Span { start: 57, end: 58 },
                            exprs: vec![ExpListItem::Expr(Expression::name_from("i", 58))],
                            close_paren: Span { start: 59, end: 60 },
                        },
                    },
                ))]),
                end_span: Span { start: 69, end: 72 },
            })]),
        );
    }

    #[test]
    fn while_loop() {
        let lua = "
        while true do
            print('loop')
        end
        ";
        parse_and_compare(
            lua,
            Block(vec![Statement::While {
                while_span: Span { start: 9, end: 14 },
                exp: Expression::True(Span { start: 15, end: 19 }),
                do_span: Span { start: 20, end: 22 },
                block: Block(vec![Statement::Expression(Expression::FuncCall(
                    FunctionCall {
                        prefix: Box::new(Expression::name_from("print", 35)),
                        args: Args::ExpList {
                            open_paren: Span { start: 40, end: 41 },
                            exprs: vec![ExpListItem::Expr(Expression::string_from("'loop'", 41))],
                            close_paren: Span { start: 47, end: 48 },
                        },
                    },
                ))]),
                end_span: Span { start: 57, end: 60 },
            }]),
        );
    }

    #[test]
    fn repeat_loop() {
        let lua = "
        repeat
            print('loop')
        until true
        ";
        parse_and_compare(
            lua,
            Block(vec![Statement::Repeat {
                repeat_span: Span { start: 9, end: 15 },
                exp: Expression::True(Span { start: 56, end: 60 }),
                block: Block(vec![Statement::Expression(Expression::FuncCall(
                    FunctionCall {
                        prefix: Box::new(Expression::name_from("print", 28)),
                        args: Args::ExpList {
                            open_paren: Span { start: 33, end: 34 },
                            exprs: vec![ExpListItem::Expr(Expression::string_from("'loop'", 34))],
                            close_paren: Span { start: 40, end: 41 },
                        },
                    },
                ))]),
                until_span: Span { start: 50, end: 55 },
            }]),
        );
    }

    #[test]
    fn ifs() {
        let lua = "
        if false then
            print('never')
        elseif false then
            print('never again')
        else
            print('always')
        end
        ";
        parse_and_compare(
            lua,
            Block(vec![Statement::If(If {
                if_span: Span { start: 9, end: 11 },
                test: Expression::False(Span { start: 12, end: 17 }),
                then_span: Span { start: 18, end: 22 },
                block: Block(vec![Statement::Expression(Expression::FuncCall(
                    FunctionCall {
                        prefix: Box::new(Expression::name_from("print", 35)),
                        args: Args::ExpList {
                            open_paren: Span { start: 40, end: 41 },
                            exprs: vec![ExpListItem::Expr(Expression::string_from("'never'", 41))],
                            close_paren: Span { start: 48, end: 49 },
                        },
                    },
                ))]),
                else_ifs: vec![ElseIf {
                    else_if_span: Span { start: 58, end: 64 },
                    test: Expression::False(Span { start: 65, end: 70 }),
                    then_span: Span { start: 71, end: 75 },
                    block: Block(vec![Statement::Expression(Expression::FuncCall(
                        FunctionCall {
                            prefix: Box::new(Expression::name_from("print", 88)),
                            args: Args::ExpList {
                                open_paren: Span { start: 93, end: 94 },
                                exprs: vec![ExpListItem::Expr(Expression::string_from(
                                    "'never again'",
                                    94,
                                ))],
                                close_paren: Span {
                                    start: 107,
                                    end: 108,
                                },
                            },
                        },
                    ))]),
                }],
                else_span: Some(Span {
                    start: 117,
                    end: 121,
                }),
                catch_all: Some(Block(vec![Statement::Expression(Expression::FuncCall(
                    FunctionCall {
                        prefix: Box::new(Expression::name_from("print", 134)),
                        args: Args::ExpList {
                            open_paren: Span {
                                start: 139,
                                end: 140,
                            },
                            exprs: vec![ExpListItem::Expr(Expression::string_from(
                                "'always'", 140,
                            ))],
                            close_paren: Span {
                                start: 148,
                                end: 149,
                            },
                        },
                    },
                ))])),
                end_span: Span {
                    start: 158,
                    end: 161,
                },
            })]),
        );
    }

    #[test]
    fn assignment() {
        parse_and_compare(
            "a = 1",
            Block(vec![Statement::Assignment {
                local_span: None,
                targets: vec![ExpListItem::Expr(Expression::name_from("a", 0))],
                eq_span: Some(Span { start: 2, end: 3 }),
                values: vec![ExpListItem::Expr(Expression::numeral_from("1", 4))],
            }]),
        );
    }

    #[test]
    fn multi_assignment() {
        parse_and_compare(
            "a, b = 1, 2",
            Block(vec![Statement::Assignment {
                local_span: None,
                targets: vec![
                    ExpListItem::Expr(Expression::name_from("a", 0)),
                    ExpListItem::Comma(Span { start: 1, end: 2 }),
                    ExpListItem::Expr(Expression::name_from("b", 3)),
                ],
                eq_span: Some(Span { start: 5, end: 6 }),
                values: vec![
                    ExpListItem::Expr(Expression::numeral_from("1", 7)),
                    ExpListItem::Comma(Span { start: 8, end: 9 }),
                    ExpListItem::Expr(Expression::numeral_from("2", 10)),
                ],
            }]),
        );
    }

    #[test]
    fn do_stmt() {
        let lua = "
        do
            print(1 + 1)
        end";

        parse_and_compare(
            lua,
            Block(vec![Statement::Do {
                do_span: Span { start: 9, end: 11 },
                block: Block(vec![Statement::Expression(Expression::FuncCall(
                    FunctionCall {
                        prefix: Box::new(Expression::name_from("print", 24)),
                        args: Args::ExpList {
                            open_paren: Span { start: 29, end: 30 },
                            exprs: vec![ExpListItem::Expr(Expression::BinOp {
                                left: Box::new(Expression::numeral_from("1", 30)),
                                op: BinaryOperator::Add(Span { start: 32, end: 33 }),
                                right: Box::new(Expression::numeral_from("1", 34)),
                            })],
                            close_paren: Span { start: 35, end: 36 },
                        },
                    },
                ))]),
                end_span: Span { start: 45, end: 48 },
            }]),
        )
    }

    #[test]
    fn global_func() {
        let lua = "function thing(a, ...)
            return -1, ...
        end";
        parse_and_compare(
            lua,
            Block(vec![Statement::Function {
                local: None,
                function: Span { start: 0, end: 8 },
                name: FuncName {
                    segments: vec![FuncNamePart::Name(Name::from_str("thing", 9))],
                },
                body: FuncBody {
                    open_paren_span: Span { start: 14, end: 15 },
                    par_list: ParList {
                        parts: vec![
                            ParListPart::Name(Name::from_str("a", 15)),
                            ParListPart::Comma(Span { start: 16, end: 17 }),
                            ParListPart::VarArgs(Span { start: 18, end: 21 }),
                        ],
                    },
                    close_paren_span: Span { start: 21, end: 22 },
                    block: Block(vec![Statement::Return(RetStatement {
                        return_span: Span { start: 35, end: 41 },
                        exprs: vec![
                            ExpListItem::Expr(Expression::UnaryOp {
                                op: UnaryOperator::Negate(Span { start: 42, end: 43 }),
                                exp: Box::new(Expression::numeral_from("1", 43)),
                            }),
                            ExpListItem::Comma(Span { start: 44, end: 45 }),
                            ExpListItem::Expr(Expression::VarArgs(Span { start: 46, end: 49 })),
                        ],
                    })]),
                    end_span: Span { start: 58, end: 61 },
                },
            }]),
        )
    }

    #[test]
    fn computed_table_field() {
        let lua = "return {
            ['a'] = (1)
        }";
        parse_and_compare(
            lua,
            Block(vec![Statement::Return(RetStatement {
                return_span: Span { start: 0, end: 6 },
                exprs: vec![ExpListItem::Expr(Expression::TableCtor(Box::new(Table {
                    open_brace: Span { start: 7, end: 8 },
                    field_list: vec![Field::Record {
                        name: Expression::string_from("'a'", 22),
                        eq: Span { start: 27, end: 28 },
                        value: Expression::Parened {
                            open_span: Span { start: 29, end: 30 },
                            expr: Box::new(Expression::numeral_from("1", 30)),
                            close_span: Span { start: 31, end: 32 },
                        },
                        sep: None,
                    }],
                    close_brace: Span { start: 41, end: 42 },
                })))],
            })]),
        );
    }

    #[test]
    fn list_table_field() {
        let lua = "return {
            1
        }";
        parse_and_compare(
            lua,
            Block(vec![Statement::Return(RetStatement {
                exprs: vec![ExpListItem::Expr(Expression::TableCtor(Box::new(Table {
                    open_brace: Span { start: 7, end: 8 },
                    field_list: vec![Field::List {
                        value: Expression::numeral_from("1", 21),
                        sep: None,
                    }],
                    close_brace: Span { start: 31, end: 32 },
                })))],
                return_span: Span { start: 0, end: 6 },
            })]),
        );
    }

    #[test]
    fn empty() {
        let lua = ";";
        parse_and_compare(
            lua,
            Block(vec![Statement::Empty(Span { start: 0, end: 1 })]),
        )
    }

    #[test]
    fn parened() {
        let lua = "a = (1 // 2) % 3";
        parse_and_compare(
            lua,
            Block(vec![Statement::Assignment {
                local_span: None,
                targets: vec![ExpListItem::Expr(Expression::name_from("a", 0))],
                eq_span: Some(Span { start: 2, end: 3 }),
                values: vec![ExpListItem::Expr(Expression::BinOp {
                    left: Box::new(Expression::Parened {
                        open_span: Span { start: 4, end: 5 },
                        expr: Box::new(Expression::BinOp {
                            left: Box::new(Expression::numeral_from("1", 5)),
                            op: BinaryOperator::FloorDivide(Span { start: 7, end: 9 }),
                            right: Box::new(Expression::numeral_from("2", 10)),
                        }),
                        close_span: Span { start: 11, end: 12 },
                    }),
                    op: BinaryOperator::Modulo(Span { start: 13, end: 14 }),
                    right: Box::new(Expression::numeral_from("3", 15)),
                })],
            }]),
        );
    }

    #[test]
    fn comment_handling() {
        pretty_env_logger::try_init().ok();
        let lua = "#!/bin/lua
        ---EmmyLua style doc comment
        ---@class
        local Thing = {}
        Thing.__index = Thing

        ---Create a new thing
        ---@returns Thing
        function Thing.new()
            local base = {
                ---@field number
                one = 0,
                ---@field string,
                two = '',
            }
            setmetatable(base, Thing)
            return thing
        end -- this should be included in the first one

        ---Get the stuff from the thing
        ---@returns number, string
        function Thing:stuff() 
            return self.one, self.two
        end

        -- weird comment all the way at the end...";
        let mut p = Parser::new(lua.as_bytes());
        let class = p.next().unwrap().unwrap();
        assert_eq!(
            &class.statement,
            &Statement::Assignment {
                local_span: Some(Span { start: 74, end: 79 }),
                targets: vec![ExpListItem::Expr(Expression::name_from("Thing", 80)),],
                eq_span: Some(Span { start: 86, end: 87 }),
                values: vec![ExpListItem::Expr(Expression::TableCtor(Box::new(Table {
                    open_brace: Span { start: 88, end: 89 },
                    field_list: vec![],
                    close_brace: Span { start: 89, end: 90 }
                }))),],
            }
        );
        assert_eq!(
            class.comments.len(),
            3,
            "expected class comments to have 3: {:?}",
            class.comments
        );
        let index = p.next().unwrap().unwrap();
        assert_eq!(
            &index.statement,
            &Statement::Assignment {
                local_span: None,
                targets: vec![ExpListItem::Expr(Expression::Suffixed(Box::new(
                    Suffixed {
                        subject: Expression::name_from("Thing", 99),
                        property: SuffixedProperty::Name {
                            sep: SuffixSep::Dot(Span {
                                start: 104,
                                end: 105
                            }),
                            name: Name::from_str("__index", 105),
                        }
                    }
                )))],
                eq_span: Some(Span {
                    start: 113,
                    end: 114
                }),
                values: vec![ExpListItem::Expr(Expression::name_from("Thing", 115))],
            }
        );
        assert_eq!(
            index.comments.len(),
            0,
            "expected __index to have 0 comments: {:?}",
            index.comments
        );
        let ctor = p.next().unwrap().unwrap();
        assert_eq!(
            ctor.comments.len(),
            5,
            "expected ctor comments to have 3: {:?}",
            ctor.comments
        );
        let method = p.next().unwrap().unwrap();
        assert_eq!(
            method.comments.len(),
            3,
            "expected method comments to have 3: {:?}",
            method.comments
        );
    }

    #[track_caller]
    fn parse_and_compare(test: &str, target: Block) {
        let mut p = Parser::new(test.as_bytes());
        let block = p.block_().unwrap();
        compare_blocs(block, target);
    }

    #[track_caller]
    fn compare_blocs(test: Block, target: Block) {
        for (lhs, rhs) in test.0.iter().zip(target.0.iter()) {
            assert_eq!(
                lhs, rhs,
                "Invalid statement, expected {:?}, found {:?}",
                rhs, lhs
            )
        }
    }
}
