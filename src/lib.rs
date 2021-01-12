use std::borrow::Cow;

use ast::{
    Args, BinaryOperator, Block, Chunk, ElseIf, Expression, Field, ForInLoop, ForLoop, FuncBody,
    FuncName, FunctionCall, If, LiteralString, Name, NameList, Numeral, ParList, PrefixExp,
    RetStatement, Statement, Suffixed, Table, UnaryOperator,
};
use lex_lua::{Item, Keyword, Punct, SpannedLexer, Token};
use log::trace;

mod ast;
pub mod error;
pub use error::Error;

type R<T> = Result<T, Error>;

pub struct Parser<'a> {
    lex: SpannedLexer<'a>,
    look_ahead: Option<Item<'a>>,
    look_ahead2: Option<Item<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(bytes: &'a [u8]) -> Self {
        let mut lex = SpannedLexer::new(bytes);
        let mut look_ahead = None;
        let mut look_ahead2 = None;
        while let Some(i) = lex.next() {
            if !matches!(i.token, Token::Comment(_)) {
                look_ahead = Some(i);
                break;
            }
        }
        while let Some(i) = lex.next() {
            if !matches!(i.token, Token::Comment(_)) {
                look_ahead2 = Some(i);
                break;
            }
        }
        Self {
            lex,
            look_ahead,
            look_ahead2,
        }
    }

    pub fn next(&mut self) -> Option<R<Chunk<'a>>> {
        if self.look_ahead.is_none() {
            return None;
        }
        Some(self.chunk())
    }

    pub fn chunk(&mut self) -> R<Chunk<'a>> {
        Ok(Chunk(self.block()?))
    }

    pub fn block(&mut self) -> R<Block<'a>> {
        trace!("block {:?}, {:?}", self.look_ahead, self.look_ahead2);
        let mut statements = Vec::new();
        let mut ret_stat = None;
        while !self.at_block_end() {
            if matches!(self.look_ahead(), Some(Token::Keyword(Keyword::Return))) {
                ret_stat = Some(self.ret_stat()?);
                
                break;
            } else {
                statements.push(self.statement()?);
            }
        }

        Ok(Block {
            statements,
            ret_stat,
        })
    }

    pub fn statement(&mut self) -> R<Statement<'a>> {
        trace!("statement {:?}, {:?}", self.look_ahead, self.look_ahead2);
        match self.look_ahead() {
            Some(Token::Punct(Punct::SemiColon)) => {
                let _ = self.next_token();
                Ok(Statement::Empty)
            }
            Some(Token::Keyword(Keyword::Break)) => self.break_stmt(),
            Some(Token::Keyword(Keyword::GoTo)) => self.go_to(),
            Some(Token::Keyword(Keyword::Do)) => self.do_stmt(),
            Some(Token::Keyword(Keyword::While)) => self.while_stmt(),
            Some(Token::Keyword(Keyword::Repeat)) => self.repeat(),
            Some(Token::Keyword(Keyword::If)) => self.if_stmt(),
            Some(Token::Keyword(Keyword::For)) => self.for_loop(),
            Some(Token::Keyword(Keyword::Function)) => self.function(false),
            Some(Token::Keyword(Keyword::Local)) => {
                self.expect_keyword(Keyword::Local)?;
                if self.at(Token::Keyword(Keyword::Function)) {
                    self.function(true)
                } else {
                    self.assignment(true)
                }
            }
            Some(Token::Punct(Punct::DoubleColon)) => self.label(),
            Some(Token::Keyword(Keyword::Return)) => {
                Ok(Statement::Return(self.ret_stat()?))
            }
            _ => self.exp_stat(),
        }
    }

    fn ret_stat(&mut self) -> R<RetStatement<'a>> {
        trace!("ret_stat {:?}, {:?}", self.look_ahead, self.look_ahead2);
        self.expect_keyword(Keyword::Return)?;
        let exps = if self.eat_punct(Punct::SemiColon) || self.at_block_end() {
            Vec::new()
        } else { 
            self.exp_list()?
        };
        self.eat_punct(Punct::SemiColon);
        Ok(RetStatement(exps))
    }

    fn exp_stat(&mut self) -> R<Statement<'a>> {
        trace!("exp_stat {:?}, {:?}", self.look_ahead, self.look_ahead2);
        let base = self.suffixed_exp()?;
        if matches!(
            self.look_ahead(),
            Some(Token::Punct(Punct::Equal)) | Some(Token::Punct(Punct::Comma))
        ) {
            self.assign_cont(false, base)
        } else {
            // TODO Validate function call
            Ok(Statement::Expression(base))
        }
    }

    pub fn assignment(&mut self, local: bool) -> R<Statement<'a>> {
        trace!(
            "assignment({}) {:?}, {:?}",
            local,
            self.look_ahead,
            self.look_ahead2
        );
        let mut start = self.suffixed_exp()?;
        if let Expression::Name(n) = &mut start {
            n.attr = self.eat_name_attr()?;
        }
        self.assign_cont(local, start)
    }

    pub fn assign_cont(&mut self, local: bool, start: Expression<'a>) -> R<Statement<'a>> {
        trace!(
            "assign_cont({:?}, {:?}) {:?}, {:?}",
            local,
            start,
            self.look_ahead,
            self.look_ahead2
        );
        let mut targets = vec![start];
        while self.eat_punct(Punct::Comma) {
            let mut next = self.suffixed_exp()?;
            if let Expression::Name(n) = &mut next {
                n.attr = self.eat_name_attr()?;
            }
            targets.push(next)
        }
        let values = if self.eat_punct(Punct::Equal) {
            self.exp_list()?
        } else {
            Vec::new()
        };
        Ok(Statement::Assignment {
            local,
            targets,
            values,
        })
    }

    pub fn label(&mut self) -> R<Statement<'a>> {
        trace!("label {:?}, {:?}", self.look_ahead, self.look_ahead2);
        self.expect_punct(Punct::DoubleColon)?;
        let name = self.name()?;
        self.expect_punct(Punct::DoubleColon)?;
        Ok(Statement::Label(name))
    }

    pub fn break_stmt(&mut self) -> R<Statement<'a>> {
        trace!("break_stmt {:?}, {:?}", self.look_ahead, self.look_ahead2);
        self.expect_keyword(Keyword::Break)?;
        Ok(ast::Statement::Break)
    }

    pub fn go_to(&mut self) -> R<Statement<'a>> {
        trace!("go_to {:?}, {:?}", self.look_ahead, self.look_ahead2);
        self.expect_keyword(Keyword::GoTo)?;
        let name = self.name()?;
        Ok(Statement::GoTo(name))
    }

    pub fn do_stmt(&mut self) -> R<Statement<'a>> {
        trace!("do_stmt {:?}, {:?}", self.look_ahead, self.look_ahead2);
        self.expect_keyword(Keyword::Do)?;
        let block = self.block()?;
        self.expect_keyword(Keyword::End)?;
        Ok(Statement::Do {
            block: Box::new(block),
        })
    }

    fn while_stmt(&mut self) -> R<Statement<'a>> {
        trace!("while_stmt {:?}, {:?}", self.look_ahead, self.look_ahead2);
        self.expect_keyword(Keyword::While)?;
        let exp = self.exp()?;
        self.expect_keyword(Keyword::Do)?;
        let block = self.block()?;
        self.expect_keyword(Keyword::End)?;
        Ok(Statement::While {
            exp,
            block: Box::new(block),
        })
    }

    fn repeat(&mut self) -> R<Statement<'a>> {
        trace!("repeat {:?}, {:?}", self.look_ahead, self.look_ahead2);
        self.expect_keyword(Keyword::Repeat)?;
        let block = self.block()?;
        self.expect_keyword(Keyword::Until)?;
        let exp = self.exp()?;
        Ok(Statement::Repeat {
            block: Box::new(block),
            exp,
        })
    }

    fn if_stmt(&mut self) -> R<Statement<'a>> {
        trace!("if_stmt {:?}, {:?}", self.look_ahead, self.look_ahead2);
        self.expect_keyword(Keyword::If)?;
        let exp = self.exp()?;
        self.expect_keyword(Keyword::Then)?;
        let block = self.block()?;
        let mut else_ifs = Vec::new();
        while self.at(Token::Keyword(Keyword::ElseIf)) {
            else_ifs.push(self.else_if()?)
        }
        let catch_all = if self.eat_keyword(Keyword::Else) {
            let block = self.block()?;
            Some(Box::new(block))
        } else {
            None
        };
        self.expect_keyword(Keyword::End)?;
        Ok(Statement::If(If {
            test: exp,
            block: Box::new(block),
            else_ifs,
            catch_all,
        }))
    }

    fn else_if(&mut self) -> R<ElseIf<'a>> {
        trace!("else_if {:?}, {:?}", self.look_ahead, self.look_ahead2);
        self.expect_keyword(Keyword::ElseIf)?;
        let exp = self.exp()?;
        self.expect_keyword(Keyword::Then)?;
        let block = self.block()?;
        Ok(ElseIf { test: exp, block })
    }

    fn for_loop(&mut self) -> R<Statement<'a>> {
        trace!("for_loop {:?}, {:?}", self.look_ahead, self.look_ahead2);
        self.expect_keyword(Keyword::For)?;
        let name = self.name()?;
        if !self.at(Token::Punct(Punct::Equal)) {
            return self.for_in_loop(name);
        }
        self.expect_punct(Punct::Equal)?;
        let exp = self.exp()?;
        self.expect_punct(Punct::Comma)?;
        let exp2 = self.exp()?;
        let exp3 = if self.eat_punct(Punct::Comma) {
            Some(self.exp()?)
        } else {
            None
        };
        self.expect_keyword(Keyword::Do)?;
        let block = self.block()?;
        self.expect_keyword(Keyword::End)?;
        Ok(Statement::For(ForLoop {
            init_name: name,
            init: exp,
            limit: exp2,
            step: exp3,
            block: Box::new(block),
        }))
    }

    fn for_in_loop(&mut self, first_name: Name<'a>) -> R<Statement<'a>> {
        trace!(
            "for_in_loop ({:?}) {:?}, {:?}",
            first_name,
            self.look_ahead,
            self.look_ahead2
        );
        let name_list = self.name_list_cont(first_name)?;
        self.expect_keyword(Keyword::In)?;
        let exp_list = self.exp_list()?;
        self.expect_keyword(Keyword::Do)?;
        let block = self.block()?;
        self.expect_keyword(Keyword::End)?;
        Ok(Statement::ForIn(ForInLoop {
            name_list,
            exp_list,
            block: Box::new(block),
        }))
    }

    fn name_list_cont(&mut self, first_name: Name<'a>) -> R<NameList<'a>> {
        trace!(
            "name_list_cont({:?}) {:?}, {:?}",
            first_name,
            self.look_ahead,
            self.look_ahead2
        );
        let mut ret = Vec::new();
        ret.push(first_name);
        while self.eat_punct(Punct::Comma) {
            let name = self.name()?;
            ret.push(name);
        }
        Ok(NameList(ret))
    }

    fn function(&mut self, local: bool) -> R<Statement<'a>> {
        trace!("function {:?}, {:?}", self.look_ahead, self.look_ahead2);
        self.expect_keyword(Keyword::Function)?;
        let name = self.func_name()?;
        let body = self.func_body()?;
        Ok(Statement::Function { local, name, body })
    }

    fn func_name(&mut self) -> R<FuncName<'a>> {
        trace!("func_name {:?}, {:?}", self.look_ahead, self.look_ahead2);
        let mut dot_separated = vec![self.name()?];
        while self.eat_punct(Punct::Dot) {
            let name = self.name()?;
            dot_separated.push(name);
        }
        let method = if self.eat_punct(Punct::Colon) {
            Some(self.name()?)
        } else {
            None
        };
        Ok(FuncName {
            dot_separated,
            method,
        })
    }

    pub fn func_body(&mut self) -> R<FuncBody<'a>> {
        trace!("func_body {:?}, {:?}", self.look_ahead, self.look_ahead2);
        self.expect_punct(Punct::OpenParen)?;
        let par_list = self.par_list()?;
        self.expect_punct(Punct::CloseParen)?;
        let block = self.block()?;
        self.expect_keyword(Keyword::End)?;
        Ok(FuncBody { par_list, block })
    }

    fn func_args(&mut self) -> R<Args<'a>> {
        trace!("func_args {:?}, {:?}", self.look_ahead, self.look_ahead2);
        match self.look_ahead() {
            Some(Token::Punct(Punct::OpenParen)) => {
                self.expect_punct(Punct::OpenParen)?;
                let args = if matches!(self.look_ahead(), Some(Token::Punct(Punct::CloseParen))) {
                    Vec::new()
                } else {
                    self.exp_list()?
                };
                self.expect_punct(Punct::CloseParen)?;
                Ok(Args::ExpList(args))
            }
            Some(Token::Punct(Punct::OpenBrace)) => {
                let args = self.table_ctor()?;
                Ok(Args::Table(args))
            }
            Some(Token::LiteralString(s)) => {
                let arg = LiteralString(s.clone());
                let _s = self.next_token();
                Ok(Args::String(arg))
            }
            _ => self.unexpected_token(Token::Punct(Punct::OpenParen)),
        }
    }

    pub fn par_list(&mut self) -> R<ParList<'a>> {
        trace!("par_list {:?}, {:?}", self.look_ahead, self.look_ahead2);
        let mut names = Vec::new();
        let var_args = loop {
            if self.at_name() {
                let name = self.name()?;
                names.push(name);
                if !self.eat_punct(Punct::Comma) {
                    break false;
                }
            } else if self.eat_punct(Punct::Ellipsis) {
                break true;
            } else {
                break false;
            }
        };
        Ok(ParList {
            names: NameList(names),
            var_args,
        })
    }

    pub fn name(&mut self) -> R<Name<'a>> {
        trace!("name {:?}, {:?}", self.look_ahead, self.look_ahead2);
        let name = self.expect_name()?;
        Ok(Name { name, attr: None })
    }

    pub fn eat_name_attr(&mut self) -> R<Option<Cow<'a, str>>> {
        trace!("eat_name_attr {:?}, {:?}", self.look_ahead, self.look_ahead2);
        if matches!(self.look_ahead(), Some(Token::Punct(Punct::LessThan)))
        && matches!(self.look_ahead2(), Some(Token::Name(_))) {
            self.expect_punct(Punct::LessThan)?;
            let name = self.expect_name()?;
            self.expect_punct(Punct::GreaterThan)?;
            Ok(Some(name))
        } else {
            Ok(None)
        }
    }

    pub fn expect_name(&mut self) -> R<Cow<'a, str>> {
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

    pub fn exp_list(&mut self) -> R<Vec<Expression<'a>>> {
        trace!("exp_list {:?}, {:?}", self.look_ahead, self.look_ahead2);
        let first = self.exp()?;
        let mut ret = vec![first];
        while self.eat_punct(Punct::Comma) {
            ret.push(self.exp()?)
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
            base = Expression::binary(base, op, right);
        }
        Ok(base)
    }

    fn simple_exp(&mut self) -> R<Expression<'a>> {
        trace!("simple_exp {:?}, {:?}", self.look_ahead, self.look_ahead2);
        match self.look_ahead().unwrap() {
            Token::Numeral(n) => {
                let exp = Expression::Numeral(Numeral(n.clone()));
                let _n_tok = self.next_token();
                Ok(exp)
            }
            Token::LiteralString(s) => {
                let exp = Expression::LiteralString(LiteralString(s.clone()));
                let _s_tok = self.next_token();
                Ok(exp)
            }
            Token::Keyword(Keyword::Nil) => {
                self.expect_keyword(Keyword::Nil)?;
                Ok(Expression::Nil)
            }
            Token::Keyword(Keyword::True) => {
                self.expect_keyword(Keyword::True)?;
                Ok(Expression::True)
            }
            Token::Keyword(Keyword::False) => {
                self.expect_keyword(Keyword::False)?;
                Ok(Expression::False)
            }
            Token::Punct(Punct::Ellipsis) => {
                //TODO: validate in fn
                self.expect_punct(Punct::Ellipsis)?;
                Ok(Expression::VarArgs)
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
                Some(Token::Punct(Punct::Dot)) | Some(Token::Punct(Punct::Colon)) => expr = self.field(expr)?,
                Some(Token::Punct(Punct::OpenBracket)) => expr = self.index(expr)?,
                Some(Token::Punct(Punct::OpenParen))
                | Some(Token::Punct(Punct::OpenBrace))
                | Some(Token::LiteralString(_)) => expr = self.func_call(expr, false)?,
                _ => return Ok(expr),
            }
        }
    }

    fn field(&mut self, expr: Expression<'a>) -> R<Expression<'a>> {
        trace!("field {:?}, {:?}", self.look_ahead, self.look_ahead2);
        let method = self.eat_punct(Punct::Colon);
        if !method {
            self.expect_punct(Punct::Dot)?;
        }
        let inner = Suffixed {
            subject: expr,
            property: self.exp()?,
            computed: false,
            method,
        };
        Ok(Expression::Suffixed(Box::new(inner)))
    }

    fn index(&mut self, expr: Expression<'a>) -> R<Expression<'a>> {
        trace!("index {:?}, {:?}", self.look_ahead, self.look_ahead2);
        self.expect_punct(Punct::OpenBracket)?;
        let property = self.exp()?;
        self.expect_punct(Punct::CloseBracket)?;
        let inner = Suffixed {
            subject: expr,
            property,
            computed: true,
            method: false,
        };
        Ok(Expression::Suffixed(Box::new(inner)))
    }

    fn func_call(&mut self, expr: Expression<'a>, method: bool) -> R<Expression<'a>> {
        trace!("func_call {:?}, {:?}", self.look_ahead, self.look_ahead2);
        let args = self.func_args()?;

        let call = FunctionCall {
            prefix: Box::new(PrefixExp::Exp(Box::new(expr))),
            args,
            method,
        };
        let pre = PrefixExp::FunctionCall(call);
        Ok(Expression::Prefix(pre))
    }

    fn primary_exp(&mut self) -> R<Expression<'a>> {
        trace!("primary_exp {:?}, {:?}", self.look_ahead, self.look_ahead2);
        match &self.look_ahead() {
            Some(Token::Punct(Punct::OpenParen)) => {
                self.expect_punct(Punct::OpenParen)?;
                let inner = self.exp()?;
                self.expect_punct(Punct::CloseParen)?;
                Ok(Expression::Prefix(PrefixExp::Exp(Box::new(inner))))
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
        self.expect_punct(Punct::OpenBrace)?;
        let mut field_list = Vec::new();
        while !self.eat_punct(Punct::CloseBrace) {
            field_list.push(self.field_init()?)
        }
        Ok(Table { field_list })
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
        self.eat_punct(Punct::Comma);
        self.eat_punct(Punct::SemiColon);
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
        self.expect_punct(Punct::Equal)?;
        Ok(Field::Record {
            name,
            value: self.exp()?,
        })
    }

    fn list_field(&mut self) -> R<Field<'a>> {
        trace!("list_field {:?}, {:?}", self.look_ahead, self.look_ahead2);
        let exp = self.exp()?;
        Ok(Field::List(exp))
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

    fn expect_keyword(&mut self, k: Keyword) -> R<()> {
        if self.eat_keyword(k) {
            Ok(())
        } else {
            self.unexpected_token(Token::Keyword(k))
        }
    }

    fn eat_keyword(&mut self, k: Keyword) -> bool {
        if let Some(Token::Keyword(lh)) = self.look_ahead() {
            if *lh == k {
                self.next_token();
                return true;
            }
        }
        false
    }

    fn expect_punct(&mut self, p: Punct) -> R<()> {
        if self.eat_punct(p) {
            Ok(())
        } else {
            self.unexpected_token(Token::Punct(p))
        }
    }

    fn eat_punct(&mut self, p: Punct) -> bool {
        if let Some(Token::Punct(lh)) = self.look_ahead() {
            if *lh == p {
                self.next_token();
                return true;
            }
        }
        false
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

    pub fn next_token(&mut self) -> Option<Item<'a>> {
        use std::mem::replace;
        let mut next2 = self.lex.next();
        while matches!(next2.as_ref().map(|i| &i.token), Some(Token::Comment(_))) {
            next2 = self.lex.next();
        }
        replace(&mut self.look_ahead, replace(&mut self.look_ahead2, next2))
    }

    fn try_get_unary_op(&mut self) -> Option<UnaryOperator> {
        let op = match self.look_ahead()? {
            Token::Punct(Punct::Minus) => UnaryOperator::Negate,
            Token::Keyword(Keyword::Not) => UnaryOperator::Not,
            Token::Punct(Punct::Hash) => UnaryOperator::Length,
            Token::Punct(Punct::Tilde) => UnaryOperator::BitwiseNot,
            _ => return None,
        };
        let _op_token = self.next_token();
        Some(op)
    }

    fn try_get_binary_op(&mut self, limit: u8) -> Option<BinaryOperator> {
        let op = match self.look_ahead()? {
            Token::Punct(Punct::Plus) => BinaryOperator::Add,
            Token::Punct(Punct::Minus) => BinaryOperator::Subtract,
            Token::Punct(Punct::Asterisk) => BinaryOperator::Multiply,
            Token::Punct(Punct::ForwardSlash) => BinaryOperator::Divide,
            Token::Punct(Punct::DoubleForwardSlash) => BinaryOperator::FloorDivide,
            Token::Punct(Punct::Caret) => BinaryOperator::Power,
            Token::Punct(Punct::Percent) => BinaryOperator::Modulo,
            Token::Punct(Punct::Ampersand) => BinaryOperator::BitwiseAnd,
            Token::Punct(Punct::Tilde) => BinaryOperator::BitwiseXor,
            Token::Punct(Punct::Pipe) => BinaryOperator::BitwiseOr,
            Token::Punct(Punct::DoubleGreaterThan) => BinaryOperator::RightShift,
            Token::Punct(Punct::DoubleLessThan) => BinaryOperator::LeftShift,
            Token::Punct(Punct::DoubleDot) => BinaryOperator::Concatenate,
            Token::Punct(Punct::LessThan) => BinaryOperator::LessThan,
            Token::Punct(Punct::GreaterThan) => BinaryOperator::GreaterThan,
            Token::Punct(Punct::LessThanEqual) => BinaryOperator::LessThanEqual,
            Token::Punct(Punct::GreaterThanEqual) => BinaryOperator::GreaterThanEqual,
            Token::Punct(Punct::DoubleEqual) => BinaryOperator::Equal,
            Token::Punct(Punct::TildeEqual) => BinaryOperator::NotEqual,
            Token::Keyword(Keyword::And) => BinaryOperator::And,
            Token::Keyword(Keyword::Or) => BinaryOperator::Or,
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
    use ast::Block;

    use super::*;

    #[test]
    fn print() {
        pretty_env_logger::try_init().ok();
        let lua = "print('hello world')";
        let name = Expression::Name(Name::new(Cow::Borrowed("print")));
        let arg = Expression::string("'hello world'");
        let args = Args::ExpList(vec![arg]);
        parse_and_compare(
            lua,
            Block {
                statements: vec![Statement::func_call(name, args)],
                ret_stat: None,
            },
        )
    }
    #[test]
    fn require() {
        pretty_env_logger::try_init().ok();
        let lua = "require 'lib'";
        let name = Expression::Name(Name::new(Cow::Borrowed("require")));
        let arg = "'lib'".into();
        let args = Args::String(arg);
        parse_and_compare(
            lua,
            Block {
                statements: vec![Statement::func_call(name, args)],
                ret_stat: None,
            },
        )
    }

    #[test]
    fn callback() {
        pretty_env_logger::try_init().ok();
        let lua = "pcall(function () end)";
        let name = Expression::Name(Name::new(Cow::Borrowed("pcall")));
        let cb = Expression::FunctionDef(FuncBody {
            block: Block::empty(),
            par_list: ParList::empty(),
        });
        let args = Args::ExpList(vec![cb]);
        parse_and_compare(
            lua,
            Block {
                statements: vec![Statement::func_call(name, args)],
                ret_stat: None,
            },
        )
    }
    #[test]
    fn nested_calls() {
        pretty_env_logger::try_init().ok();
        let lua = "print(error())";
        let name = Expression::Name(Name::new(Cow::Borrowed("print")));
        let name2 = Expression::Name(Name::new(Cow::Borrowed("error")));
        let inner_call = Expression::func_call(name2, Args::empty());
        parse_and_compare(
            lua,
            Block {
                statements: vec![Statement::func_call(name, Args::exp_list(vec![inner_call]))],
                ret_stat: None,
            },
        )
    }
    
    #[test]
    fn chained_calls() {
        pretty_env_logger::try_init().ok();
        let lua = "f()()";
        let name = Expression::Name(Name::new(Cow::Borrowed("f")));
        let call = Expression::func_call(name, Args::empty());
        parse_and_compare(
            lua,
            Block {
                statements: vec![Statement::func_call(call, Args::exp_list(vec![]))],
                ret_stat: None,
            },
        )
    }
    #[test]
    fn if_elseif_else() {
        pretty_env_logger::try_init().ok();
        let lua = "if true then elseif false then else end";
        let stmt = Statement::If(If {
            test: Expression::True,
            block: Box::new(Block::empty()),
            else_ifs: vec![
                ElseIf {
                    test: Expression::False,
                    block: Block::empty(),
                }
            ],
            catch_all: Some(Box::new(Block::empty()))
        });
        parse_and_compare(
            lua,
            Block {
                statements: vec![stmt],
                ret_stat: None,
            },
        )
    }

    #[test]
    fn local_function() {
        let lua = "local function thing()
            local a = 0
            return a
        end";
        parse_and_compare(lua, Block {
            statements: vec![
                Statement::Function {
                    local: true,
                    body: FuncBody {
                        par_list: ParList::empty(),
                        block: Block {
                            statements: vec![
                                Statement::Assignment {
                                    local: true,
                                    targets: vec![
                                        Expression::Name(Name::new(Cow::Borrowed("a")))
                                    ],
                                    values: vec![
                                        Expression::Numeral(Numeral(Cow::Borrowed("0")))
                                    ]
                                }
                            ],
                            ret_stat: Some(RetStatement(vec![
                                Expression::Name(Name::new(Cow::Borrowed("a")))
                            ]))
                        },
                    },
                    name: FuncName {
                        dot_separated: vec![Name::new(Cow::Borrowed("thing"))],
                        method: None,
                    }
                }
            ],
            ret_stat: None
        })
    }


    fn parse_and_compare(test: &str, target: Block) {
        let mut p = Parser::new(test.as_bytes());
        let block = p.block().unwrap();
        compare_blocs(block, target);
    }

    fn compare_blocs(test: Block, target: Block) {
        for (lhs, rhs) in test.statements.iter().zip(target.statements.iter()) {
            assert_eq!(
                lhs, rhs,
                "Invalid statement, expected {:?}, found {:?}",
                rhs, lhs
            )
        }
    }
}
