use std::todo;

use ast::{ElseIf, Expression, ForInLoop, ForLoop, FuncBody, FuncName, If, Name, NameList, RetStatement, Statement};
use bstr::BStr;
use lex_lua::{Lexer, Token, Keyword, Punct};

mod ast;

pub struct Parser<'a> {
    lex: Lexer<'a>,
    look_ahead: Option<lex_lua::Token<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(bytes: &'a [u8]) -> Self {
        let mut lex = Lexer::new(bytes);
        let look_ahead = lex.next();
        Self {
            lex,
            look_ahead,
        }
    }
    
    pub fn chunk(&mut self) -> ast::Chunk<'a> {
        ast::Chunk(self.block())
    }

    pub fn block(&mut self) -> ast::Block<'a> {
        let statement = self.statement();
        let retstat = if matches!(self.look_ahead, Some(Token::Keyword(Keyword::Return))) {
            let _return = self.next_token();
            let exps = self.exp_list();
            Some(RetStatement(exps))
        } else {
            None
        };
        ast::Block {
            statement,
            retstat,
        }
    }

    pub fn statement(&mut self) -> Statement<'a> {
        todo!()
    }

    pub fn function_call(&mut self) -> Statement<'a> {
        todo!()
    }

    pub fn label(&mut self) -> Statement<'a> {
        todo!()
    }

    pub fn break_stmt(&mut self) -> Statement<'a> {
        let _break = self.next_token();
        ast::Statement::Break
    }

    pub fn go_to(&mut self) -> Statement<'a> {
        let _goto = self.next_token();
        let name = self.name();
        Statement::GoTo(name)
    }

    pub fn do_stmt(&mut self) -> Statement<'a> {
        let _do = self.next_token();
        let block = self.block();
        let _end = self.next_token();
        Statement::Do {
            block: Box::new(block),
        }
    }

    fn while_stmt(&mut self) -> Statement<'a> {
        let _while = self.next_token();
        let exp = self.exp();
        let _do = self.next_token();
        let block = self.block();
        Statement::While {
            exp,
            block: Box::new(block),
        }
    }

    fn repeat(&mut self) -> Statement<'a> {
        let _repeat = self.next_token();
        let block = self.block();
        let _until = self.next_token();
        let exp = self.exp();
        Statement::Repeat {
            block: Box::new(block),
            exp,
        }
    }

    fn if_stmt(&mut self) -> Statement<'a> {
        let _if = self.next_token();
        let exp = self.exp();
        let _then = self.next_token();
        let block = self.block();
        let mut else_ifs = Vec::new();
        while matches!(self.look_ahead, Some(Token::Keyword(lex_lua::Keyword::ElseIf))) {
            else_ifs.push(self.else_if())
        }
        let catch_all = if matches!(self.look_ahead, Some(Token::Keyword(lex_lua::Keyword::Else))) {
            let _else = self.next_token();
            let block = self.block();
            Some(Box::new(block))
        } else {
            None
        };
        let _end = self.next_token();
        Statement::If(If {
            test: exp,
            block: Box::new(block),
            else_ifs,
            catch_all
        })
    }

    fn else_if(&mut self) -> ElseIf<'a> {
        let _elseif = self.next_token();
        let exp = self.exp();
        let block = self.block();
        ElseIf {
            test: exp,
            block,
        }
    }

    fn for_loop(&mut self) -> Statement<'a> {
        let _for = self.next_token();
        let name = self.name();
        let _eq = self.next_token();
        let exp = self.exp();
        let _comma = self.next_token();
        let exp2 = self.exp();
        let exp3 = if matches!(self.look_ahead, Some(Token::Punct(Punct::Comma))) {
            let _comma = self.next_token();
            Some(self.exp())
        } else {
            None
        };
        let _do = self.next_token();
        let block = self.block();
        let _end = self.next_token();
        Statement::For(ForLoop {
            init_name: name,
            init: exp,
            limit: exp2,
            step: exp3
        })
    }

    fn for_in_loop(&mut self) -> Statement<'a> {
        let _for = self.next_token();
        let mut name_list = self.name_list();
        let _in = self.next_token();
        let exp_list = self.exp_list();
        let _do = self.next_token();
        let block = self.block();
        let _end = self.next_token();
        Statement::ForIn(ForInLoop {
            name_list,
            exp_list,
            block: Box::new(block),
        })
    }
    fn name_list(&mut self) -> NameList<'a> {
        let mut ret = Vec::new();
        let name = self.name();
        ret.push(name);
        while matches!(self.look_ahead, Some(Token::Punct(Punct::Comma))) {
            let _comma = self.next_token();
            let name = self.name();
            ret.push(name);
        }
        NameList(ret)
    }

    pub fn function(&mut self, local: bool) -> Statement<'a> {
        let _function = self.next_token();
        let name = self.func_name();
        let body = self.func_body();
        Statement::Function {
            local,
            name,
            body
        }
    }

    pub fn func_name(&mut self) -> FuncName<'a> {
        let mut dot_separated = vec![self.name()];
        while matches!(self.look_ahead, Some(Token::Punct(lex_lua::Punct::Dot))) {
            let _dot = self.next_token();
            let name = self.name();
            dot_separated.push(name);
        }
        let method = if matches!(self.look_ahead, Some(Token::Punct(lex_lua::Punct::Colon))) {
            let _colon = self.next_token();
            Some(self.name())
        } else {
            None
        };
        FuncName {
            dot_separated,
            method,
        }
    }

    pub fn func_body(&mut self) -> FuncBody<'a> {
        todo!()
    }

    pub fn name(&mut self) -> Name<'a> {
        let tok = self.next_token().expect("Expected name...");
        if let Token::Name(name) = &tok {
            Name(name.clone())
        } else {
            panic!("expected name!, found {:?}", tok);
        }
    }

    pub fn exp_list(&mut self) -> Vec<Expression<'a>> {
        todo!()
    }

    fn exp(&mut self) -> Expression<'a> {
        todo!()
    }

    pub fn next_token(&mut self) -> Option<Token<'a>> {
        std::mem::replace(&mut self.look_ahead, self.lex.next())
    }
}


