//! # Analisar
//! 
//! A Lua parser for Rust
//! 
//! ## Usage
//! 
//! This crate provides 3 different APIs for parsing lua. 
//! 
//! ### Parser
//! 
//! The first is a fairly standard parser over a fairly standard AST which provides
//! no context at all about whitespace or the position of punctuation or keywords.
//! The provided AST is designed to represent the intent of the program over anything else
//! 
//! This kind of parser could be used to build a tree walking interpreter. Here is an example:
//! 
//! ```rust
//! use analisar::{Parser, ast::{Statement, Expression}};
//! 
//! let lua = "print('hello world')";
//! let mut p = Parser::new(lua.as_bytes());
//! let stmt = p.next().unwrap().unwrap();
//! assert!(matches!(stmt, Statement::Expression(Expression::FuncCall(_))));
//! ```
//! 
//! ### TokenBufferParser
//! 
//! This is one is a bit of a hybrid of the other two parsers provided by this crate. It provides
//! both a tree of Statement/Expressions but also the raw tokens represented by a given Statement.
//! 
//! Here is an example of this kind of parser:
//! 
//! ```rust
//! use analisar::{TokenBufferParser, ast::{Statement, Expression}};
//! 
//! let lua = "
//! -- print out hello world to stdout
//! print(--[[ why!!! ]]'hello world' --[[seriously???]])";
//! let mut p = TokenBufferParser::new(lua.as_bytes());
//! let (tokens, stmt) = p.next().unwrap().unwrap();
//! assert_eq!(tokens.len(), 7);
//! assert!(matches!(stmt, Statement::Expression(Expression::FuncCall(_))));
//! ```
//! 
//! As you can see the output of the `Statement::Expression` is exactly the same as before,
//! however there is also a `Vec` of tokens provided. 
//! 
//! ### aware::Parser
//! 
//! The final parser that this crate provides is a fully context aware parser. Let's look
//! at an example of that the output of this one looks like.
//! 
//! ```rust
//! use analisar::aware::{Parser, ast::{Statement, Expression}};
//! 
//! let lua = "print(('hello world')) -- print the string 'hello world' to stdout";
//! let mut p = Parser::new(lua.as_bytes());
//! let stmt = p.next().unwrap().unwrap();
//! assert!(matches!(stmt.statement, Statement::Expression(Expression::FuncCall(_))));
//! ```
//!
//! Notice this one looks quite a bit different from the other two. First of all the function call's name
//! has an associated `Span` which represents to byte offsets for the token in the original string, you'll
//! notice similar spans across each entry in this tree. Another thing it provides is a `Parened` expression,
//! for representing when an expression has been put in parentheses. Finally we see the comments that apply to
//! this statement are also provided. With all 3 of these additions it would be possible to fully
//! reconstruct the tokens into the order they appeared originally, which would be handy it you were building
//! a code formatter or a document generator.
//! 
use std::borrow::Cow;

use ast::{
    Args, BinaryOperator, Block, ElseIf, Expression, Field, ForInLoop, ForLoop, FuncBody, FuncName,
    FunctionCall, If, LiteralString, Name, NameList, Numeral, ParList, RetStatement, Statement,
    Suffixed, Table, UnaryOperator,
};
use lex_lua::{Item, Keyword, Punct, SpannedLexer, Token};
use log::trace;

pub mod ast;
/// A module for whitespace & comment aware parsing
pub mod aware;
pub mod error;
pub use error::Error;

type R<T> = Result<T, Error>;

/// This parser will provide an AST
/// without any whitespace or comment
/// context provided.
pub struct Parser<'a> {
    lex: SpannedLexer<'a>,
    look_ahead: Option<Item<'a>>,
    look_ahead2: Option<Item<'a>>,
    token_buffer: Vec<Item<'a>>,
    capture_tokens: bool,
}

impl<'a> Parser<'a> {
    /// Construct a new parser with the provided bytes of lua
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
            token_buffer: Vec::new(),
            capture_tokens: false,
        }
    }

    fn new_with_token_buffer(bytes: &'a [u8]) -> Self {
        let mut lex = SpannedLexer::new(bytes);
        let mut look_ahead = None;
        let mut look_ahead2 = None;
        let mut token_buffer = Vec::new();
        while let Some(i) = lex.next() {
            token_buffer.push(i.clone());
            if !matches!(i.token, Token::Comment(_)) {
                look_ahead = Some(i);
                break;
            }
        }
        while let Some(i) = lex.next() {
            token_buffer.push(i.clone());
            if !matches!(i.token, Token::Comment(_)) {
                look_ahead2 = Some(i);
                break;
            }
        }
        Self {
            lex,
            look_ahead,
            look_ahead2,
            token_buffer,
            capture_tokens: true,
        }
    }
    /// Get the next lua statement from the lua text provided
    pub fn next(&mut self) -> Option<R<Statement<'a>>> {
        if self.look_ahead.is_none() {
            return None;
        }
        Some(self.statement())
    }

    /// Calling this at the top level of a file will end up parsing
    /// the entire file. If you want logical sections of that file,
    /// use `Parser::next` instead
    pub fn block(&mut self) -> R<Block<'a>> {
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
            Some(Token::Keyword(Keyword::Return)) => Ok(Statement::Return(self.ret_stat()?)),
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

    fn assignment(&mut self, local: bool) -> R<Statement<'a>> {
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

    fn assign_cont(&mut self, local: bool, start: Expression<'a>) -> R<Statement<'a>> {
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

    fn label(&mut self) -> R<Statement<'a>> {
        trace!("label {:?}, {:?}", self.look_ahead, self.look_ahead2);
        self.expect_punct(Punct::DoubleColon)?;
        let name = self.name()?;
        self.expect_punct(Punct::DoubleColon)?;
        Ok(Statement::Label(name))
    }

    fn break_stmt(&mut self) -> R<Statement<'a>> {
        trace!("break_stmt {:?}, {:?}", self.look_ahead, self.look_ahead2);
        self.expect_keyword(Keyword::Break)?;
        Ok(ast::Statement::Break)
    }

    fn go_to(&mut self) -> R<Statement<'a>> {
        trace!("go_to {:?}, {:?}", self.look_ahead, self.look_ahead2);
        self.expect_keyword(Keyword::GoTo)?;
        let name = self.name()?;
        Ok(Statement::GoTo(name))
    }

    fn do_stmt(&mut self) -> R<Statement<'a>> {
        trace!("do_stmt {:?}, {:?}", self.look_ahead, self.look_ahead2);
        self.expect_keyword(Keyword::Do)?;
        let block = self.block()?;
        self.expect_keyword(Keyword::End)?;
        Ok(Statement::Do { block })
    }

    fn while_stmt(&mut self) -> R<Statement<'a>> {
        trace!("while_stmt {:?}, {:?}", self.look_ahead, self.look_ahead2);
        self.expect_keyword(Keyword::While)?;
        let exp = self.exp()?;
        self.expect_keyword(Keyword::Do)?;
        let block = self.block()?;
        self.expect_keyword(Keyword::End)?;
        Ok(Statement::While { exp, block })
    }

    fn repeat(&mut self) -> R<Statement<'a>> {
        trace!("repeat {:?}, {:?}", self.look_ahead, self.look_ahead2);
        self.expect_keyword(Keyword::Repeat)?;
        let block = self.block()?;
        self.expect_keyword(Keyword::Until)?;
        let exp = self.exp()?;
        Ok(Statement::Repeat { block, exp })
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
            Some(block)
        } else {
            None
        };
        self.expect_keyword(Keyword::End)?;
        Ok(Statement::If(If {
            test: exp,
            block,
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
            block,
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
            block,
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

    fn func_body(&mut self) -> R<FuncBody<'a>> {
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

    fn par_list(&mut self) -> R<ParList<'a>> {
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

    fn name(&mut self) -> R<Name<'a>> {
        trace!("name {:?}, {:?}", self.look_ahead, self.look_ahead2);
        let name = self.expect_name()?;
        Ok(Name { name, attr: None })
    }

    fn eat_name_attr(&mut self) -> R<Option<Cow<'a, str>>> {
        trace!(
            "eat_name_attr {:?}, {:?}",
            self.look_ahead,
            self.look_ahead2
        );
        if matches!(self.look_ahead(), Some(Token::Punct(Punct::LessThan)))
            && matches!(self.look_ahead2(), Some(Token::Name(_)))
        {
            self.expect_punct(Punct::LessThan)?;
            let name = self.expect_name()?;
            self.expect_punct(Punct::GreaterThan)?;
            Ok(Some(name))
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

    fn exp_list(&mut self) -> R<Vec<Expression<'a>>> {
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
                Some(Token::Punct(Punct::Dot)) | Some(Token::Punct(Punct::Colon)) => {
                    expr = self.field(expr)?
                }
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
            prefix: Box::new(expr),
            args,
            method,
        };
        Ok(Expression::FuncCall(call))
    }

    fn primary_exp(&mut self) -> R<Expression<'a>> {
        trace!("primary_exp {:?}, {:?}", self.look_ahead, self.look_ahead2);
        match &self.look_ahead() {
            Some(Token::Punct(Punct::OpenParen)) => {
                self.expect_punct(Punct::OpenParen)?;
                let inner = self.exp()?;
                self.expect_punct(Punct::CloseParen)?;
                Ok(inner)
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

    fn next_token(&mut self) -> Option<Item<'a>> {
        use std::mem::replace;
        let mut next2 = self.lex.next();
        if self.capture_tokens {
            if let Some(item) = next2.as_ref() {
                self.token_buffer.push(item.clone());
            }
        }
        while matches!(next2.as_ref().map(|i| &i.token), Some(Token::Comment(_))) {
            next2 = self.lex.next();
            if self.capture_tokens {
                if let Some(item) = next2.as_ref() {
                    self.token_buffer.push(item.clone());
                }
            }
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

/// Similar to the Parser, on a call to `next`
/// will return a vector of the raw tokens from [`lex_lua`](https://docs.rs/lex_lua)
/// along with the parsed [`Statement`]
pub struct TokenBufferParser<'a> {
    inner: Parser<'a>,
}

impl<'a> TokenBufferParser<'a> {
    pub fn new(b: &'a [u8]) -> Self {
        let inner = Parser::new_with_token_buffer(b);
        Self { inner }
    }
    /// Returns a tuple, the first element will be a `Vec` of the raw
    /// [`lex_lua::Item`](https://docs.rs/lex_lua)s and the second
    /// element will be the parsed [`Statement`]
    pub fn next(&mut self) -> Option<R<(Vec<Item<'a>>, Statement<'a>)>> {
        let inner_next = self.inner.next()?;
        match inner_next {
            Ok(next) => {
                let mut tokens = Vec::with_capacity(self.inner.token_buffer.len());
                std::mem::swap(&mut self.inner.token_buffer, &mut tokens);
                Some(Ok((tokens, next)))
            }
            Err(e) => Some(Err(e)),
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
        let name = Expression::name_from("print");
        let arg = Expression::string("'hello world'");
        let args = Args::ExpList(vec![arg]);
        parse_and_compare(lua, Block(vec![Statement::func_call(name, args)]));
    }
    #[test]
    fn require() {
        pretty_env_logger::try_init().ok();
        let lua = "require 'lib'";
        let name = Expression::name_from("require");
        let arg = "'lib'".into();
        let args = Args::String(arg);
        parse_and_compare(lua, Block(vec![Statement::func_call(name, args)]));
    }

    #[test]
    fn callback() {
        pretty_env_logger::try_init().ok();
        let lua = "pcall(function () end)";
        let name = Expression::name_from("pcall");
        let cb = Expression::FunctionDef(FuncBody {
            block: Block::empty(),
            par_list: ParList::empty(),
        });
        let args = Args::ExpList(vec![cb]);
        parse_and_compare(lua, Block(vec![Statement::func_call(name, args)]));
    }
    #[test]
    fn nested_calls() {
        pretty_env_logger::try_init().ok();
        let lua = "print(error())";
        let name = Expression::name_from("print");
        let name2 = Expression::name_from("error");
        let inner_call = Expression::func_call(name2, Args::empty());
        parse_and_compare(
            lua,
            Block(vec![Statement::func_call(
                name,
                Args::exp_list(vec![inner_call]),
            )]),
        );
    }

    #[test]
    fn chained_calls() {
        pretty_env_logger::try_init().ok();
        let lua = "f()()";
        let name = Expression::name_from("f");
        let call = Expression::func_call(name, Args::empty());
        parse_and_compare(
            lua,
            Block(vec![Statement::func_call(call, Args::exp_list(vec![]))]),
        );
    }
    #[test]
    fn if_elseif_else() {
        pretty_env_logger::try_init().ok();
        let lua = "if true then elseif false then else end";
        let stmt = Statement::If(If {
            test: Expression::True,
            block: Block::empty(),
            else_ifs: vec![ElseIf {
                test: Expression::False,
                block: Block::empty(),
            }],
            catch_all: Some(Block::empty()),
        });
        parse_and_compare(lua, Block(vec![stmt]));
    }

    #[test]
    fn local_function() {
        let lua = "local function thing()
            local a = 0
            return a
        end";
        parse_and_compare(
            lua,
            Block(vec![Statement::Function {
                local: true,
                body: FuncBody {
                    par_list: ParList::empty(),
                    block: Block(vec![
                        Statement::Assignment {
                            local: true,
                            targets: vec![Expression::name_from("a")],
                            values: vec![Expression::numeral_from("0")],
                        },
                        Statement::Return(RetStatement(vec![Expression::name_from("a")])),
                    ]),
                },
                name: FuncName {
                    dot_separated: vec!["thing".into()],
                    method: None,
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
                    local: true,
                    targets: vec![Expression::name_from("a")],
                    values: vec![Expression::numeral_from("0")],
                },
                Statement::Assignment {
                    local: true,
                    targets: vec![Expression::name_from("b")],
                    values: vec![Expression::numeral_from("1")],
                },
                Statement::Assignment {
                    local: true,
                    targets: vec![Expression::name_from("c")],
                    values: vec![Expression::Nil],
                },
                Statement::Return(RetStatement(vec![Expression::TableCtor(Box::new(Table {
                    field_list: vec![
                        Field::Record {
                            name: Expression::name_from("a"),
                            value: Expression::name_from("a"),
                        },
                        Field::Record {
                            name: Expression::name_from("b"),
                            value: Expression::name_from("b"),
                        },
                        Field::Record {
                            name: Expression::name_from("c"),
                            value: Expression::name_from("c"),
                        },
                    ],
                }))])),
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
                    local: true,
                    targets: vec![Expression::name_from("a")],
                    values: vec![Expression::TableCtor(Box::new(Table {
                        field_list: vec![],
                    }))],
                },
                Statement::Assignment {
                    local: false,
                    targets: vec![Expression::Suffixed(Box::new(Suffixed {
                        subject: Expression::name_from("a"),
                        property: Expression::name_from("b"),
                        computed: false,
                        method: false,
                    }))],
                    values: vec![Expression::Numeral(Numeral(Cow::Borrowed("1")))],
                },
                Statement::Assignment {
                    local: false,
                    targets: vec![Expression::Suffixed(Box::new(Suffixed {
                        subject: Expression::name_from("a"),
                        property: Expression::string("'c'"),
                        computed: true,
                        method: false,
                    }))],
                    values: vec![Expression::numeral_from("2")],
                },
            ]),
        );
    }

    #[test]
    fn table_ctor_and_access() {
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
                    local: false,
                    targets: vec![Expression::name_from("a")],
                    values: vec![Expression::TableCtor(Box::new(Table {
                        field_list: vec![
                            Field::Record {
                                name: Expression::name_from("one"),
                                value: Expression::LiteralString("'one'".into()),
                            },
                            Field::Record {
                                name: Expression::name_from("two"),
                                value: Expression::LiteralString("'two'".into()),
                            },
                        ],
                    }))],
                },
                Statement::Expression(Expression::FuncCall(FunctionCall {
                    prefix: Box::new(Expression::name_from("print")),
                    method: false,
                    args: Args::ExpList(vec![Expression::Suffixed(Box::new(Suffixed {
                        method: false,
                        computed: false,
                        subject: Expression::name_from("a"),
                        property: Expression::name_from("one"),
                    }))]),
                })),
                Statement::Expression(Expression::FuncCall(FunctionCall {
                    prefix: Box::new(Expression::name_from("print")),
                    method: false,
                    args: Args::ExpList(vec![Expression::Suffixed(Box::new(Suffixed {
                        method: false,
                        computed: false,
                        subject: Expression::name_from("a"),
                        property: Expression::name_from("two"),
                    }))]),
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

        parse_and_compare(
            lua,
            Block(vec![
                Statement::Label("top".into()),
                Statement::Expression(Expression::FuncCall(FunctionCall {
                    prefix: Box::new(Expression::name_from("print")),
                    method: false,
                    args: Args::ExpList(vec![Expression::string("'loop'")]),
                })),
                Statement::GoTo("top".into()),
            ]),
        );
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
                init_name: "i".into(),
                init: Expression::numeral_from("0"),
                limit: Expression::numeral_from("10"),
                step: Some(Expression::numeral_from("1")),
                block: Block(vec![Statement::Break]),
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
        parse_and_compare(
            lua,
            Block(vec![Statement::ForIn(ForInLoop {
                name_list: NameList(vec!["i".into(), "v".into()]),
                exp_list: vec![Expression::FuncCall(FunctionCall {
                    prefix: Box::new(Expression::name_from("ipairs")),
                    args: Args::ExpList(vec![Expression::TableCtor(Box::new(Table {
                        field_list: vec![
                            Field::List(Expression::numeral_from("1")),
                            Field::List(Expression::numeral_from("2")),
                            Field::List(Expression::numeral_from("3")),
                        ],
                    }))]),
                    method: false,
                })],
                block: Block(vec![Statement::Expression(Expression::FuncCall(
                    FunctionCall {
                        prefix: Box::new(Expression::name_from("print")),
                        method: false,
                        args: Args::ExpList(vec![Expression::name_from("i")]),
                    },
                ))]),
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
                exp: Expression::True,
                block: Block(vec![Statement::Expression(Expression::FuncCall(
                    FunctionCall {
                        prefix: Box::new(Expression::name_from("print")),
                        method: false,
                        args: Args::ExpList(vec![Expression::string("'loop'")]),
                    },
                ))]),
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
                exp: Expression::True,
                block: Block(vec![Statement::Expression(Expression::FuncCall(
                    FunctionCall {
                        prefix: Box::new(Expression::name_from("print")),
                        method: false,
                        args: Args::ExpList(vec![Expression::string("'loop'")]),
                    },
                ))]),
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
                test: Expression::False,
                block: Block(vec![Statement::Expression(Expression::FuncCall(
                    FunctionCall {
                        prefix: Box::new(Expression::name_from("print")),
                        method: false,
                        args: Args::ExpList(vec![Expression::string("'never'")]),
                    },
                ))]),
                else_ifs: vec![ElseIf {
                    test: Expression::False,
                    block: Block(vec![Statement::Expression(Expression::FuncCall(
                        FunctionCall {
                            prefix: Box::new(Expression::name_from("print")),
                            method: false,
                            args: Args::ExpList(vec![Expression::string("'never again'")]),
                        },
                    ))]),
                }],
                catch_all: Some(Block(vec![Statement::Expression(Expression::FuncCall(
                    FunctionCall {
                        prefix: Box::new(Expression::name_from("print")),
                        method: false,
                        args: Args::ExpList(vec![Expression::string("'always'")]),
                    },
                ))])),
            })]),
        );
    }

    #[test]
    fn assignment() {
        parse_and_compare(
            "a = 1",
            Block(vec![Statement::Assignment {
                local: false,
                targets: vec![Expression::name_from("a")],
                values: vec![Expression::numeral_from("1")],
            }]),
        );
    }

    #[test]
    fn multi_assignment() {
        parse_and_compare(
            "a, b = 1, 2",
            Block(vec![Statement::Assignment {
                local: false,
                targets: vec![Expression::name_from("a"), Expression::name_from("b")],
                values: vec![Expression::numeral_from("1"), Expression::numeral_from("2")],
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
                block: Block(vec![Statement::Expression(Expression::FuncCall(
                    FunctionCall {
                        prefix: Box::new(Expression::name_from("print")),
                        args: Args::ExpList(vec![Expression::BinOp {
                            left: Box::new(Expression::numeral_from("1")),
                            op: BinaryOperator::Add,
                            right: Box::new(Expression::numeral_from("1")),
                        }]),
                        method: false,
                    },
                ))]),
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
                local: false,
                name: FuncName {
                    dot_separated: vec!["thing".into()],
                    method: None,
                },
                body: FuncBody {
                    par_list: ParList {
                        names: NameList(vec!["a".into()]),
                        var_args: true,
                    },
                    block: Block(vec![Statement::Return(RetStatement(vec![
                        Expression::UnaryOp {
                            op: UnaryOperator::Negate,
                            exp: Box::new(Expression::numeral_from("1")),
                        },
                        Expression::VarArgs,
                    ]))]),
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
            Block(vec![Statement::Return(RetStatement(vec![
                Expression::TableCtor(Box::new(Table {
                    field_list: vec![Field::Record {
                        name: Expression::string("'a'"),
                        value: Expression::numeral_from("1"),
                    }],
                })),
            ]))]),
        );
    }

    #[test]
    fn list_table_field() {
        let lua = "return {
            1
        }";
        parse_and_compare(
            lua,
            Block(vec![Statement::Return(RetStatement(vec![
                Expression::TableCtor(Box::new(Table {
                    field_list: vec![Field::List(Expression::numeral_from("1"))],
                })),
            ]))]),
        );
    }

    #[test]
    fn empty() {
        let lua = ";";
        parse_and_compare(lua, Block(vec![Statement::Empty]))
    }

    #[track_caller]
    fn parse_and_compare(test: &str, target: Block) {
        let mut p = Parser::new(test.as_bytes());
        let block = p.block().unwrap();
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
