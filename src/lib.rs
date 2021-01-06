use std::{borrow::Cow, todo};

use ast::{Args, BinaryOperator, ElseIf, Expression, ForInLoop, ForLoop, FuncBody, FuncName, FunctionCall, If, LiteralString, Name, NameList, Numeral, PrefixExp, RetStatement, Statement, Suffixed, UnaryOperator, Var};
use lex_lua::{Keyword, Lexer, Punct, Token};

mod ast;

pub struct Parser<'a> {
    lex: Lexer<'a>,
    look_ahead: Option<lex_lua::Token<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(bytes: &'a [u8]) -> Self {
        let mut lex = Lexer::new(bytes);
        let look_ahead = lex.next();
        Self { lex, look_ahead }
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
        ast::Block { statement, retstat }
    }

    pub fn statement(&mut self) -> Statement<'a> {
        match self.look_ahead {
            Some(Token::Keyword(Keyword::Break)) => self.break_stmt(),
            Some(Token::Keyword(Keyword::GoTo)) => self.go_to(),
            Some(Token::Keyword(Keyword::Do)) => self.do_stmt(),
            Some(Token::Keyword(Keyword::While)) => self.while_stmt(),
            Some(Token::Keyword(Keyword::Repeat)) => self.repeat(),
            Some(Token::Keyword(Keyword::If)) => self.if_stmt(),
            Some(Token::Keyword(Keyword::For)) => self.for_loop(),
            Some(Token::Keyword(Keyword::Function)) => self.function(false),
            Some(Token::Keyword(Keyword::Local)) => {
                let _local = self.next_token();
                if matches!(self.look_ahead, Some(Token::Keyword(Keyword::Function))) {
                    self.function(true)
                } else {
                    self.assignment(true)
                }
            }
            Some(Token::Punct(Punct::DoubleColon)) => self.label(),
            Some(Token::Name(_)) => self.assignment(false),
            _ => panic!("Unknown lookahead: {:?}", self.look_ahead),
        }
    }

    pub fn assignment(&mut self, local: bool) -> Statement<'a> {
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
        while matches!(
            self.look_ahead,
            Some(Token::Keyword(lex_lua::Keyword::ElseIf))
        ) {
            else_ifs.push(self.else_if())
        }
        let catch_all = if matches!(
            self.look_ahead,
            Some(Token::Keyword(lex_lua::Keyword::Else))
        ) {
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
            catch_all,
        })
    }

    fn else_if(&mut self) -> ElseIf<'a> {
        let _elseif = self.next_token();
        let exp = self.exp();
        let block = self.block();
        ElseIf { test: exp, block }
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
            step: exp3,
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
        Statement::Function { local, name, body }
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

    fn func_args(&mut self) -> Args<'a> {
        todo!()
    }

    pub fn name(&mut self) -> Name<'a> {
        let name = self.expect_name();
        let attr = if matches!(self.look_ahead, Some(Token::Punct(Punct::LessThan))) {
            let _angle = self.next_token();
            let name = self.expect_name();
            let _angle = self.next_token();
            Some(name)
        } else {
            None
        };
        Name { name, attr }
    }

    pub fn expect_name(&mut self) -> Cow<'a, str> {
        let name = if let Some(Token::Name(name)) = &self.look_ahead {
            name.clone()
        } else {
            panic!("expected name!, found {:?}", self.look_ahead);
        };
        let _ = self.next_token();
        name
    }

    pub fn exp_list(&mut self) -> Vec<Expression<'a>> {
        todo!()
    }

    fn exp(&mut self) -> Expression<'a> {
        self.sub_exp(0)
    }

    fn sub_exp(&mut self, limit: u8) -> Expression<'a> {
        let mut base = if let Some(op) = self.try_get_unary_op() {
            let exp = self.sub_exp(12);
            Expression::UnaryOp {
                op,
                exp: Box::new(exp)
            }
        } else {
            self.simple_exp()
        };        
        while let Some(op) = self.try_get_binary_op(limit) {
            let right = self.sub_exp(op.priority().1);
            base = Expression::binary(base, op, right);
        }
        base
    }

    fn simple_exp(&mut self) -> Expression<'a> {
        match self.look_ahead.as_ref().unwrap() {
            Token::Numeral(n) => {
                let exp = Expression::Numeral(Numeral(n.clone()));
                let _n_tok = self.next_token();
                exp
            },
            Token::LiteralString(s) => {
                let exp = Expression::LiteralString(LiteralString(s.clone()));
                let _s_tok = self.next_token();
                exp
            },
            Token::Keyword(Keyword::Nil) => {
                self.next_token();
                Expression::Nil
            },
            Token::Keyword(Keyword::True) => {
                self.next_token();
                Expression::True
            },
            Token::Keyword(Keyword::False) => {
                self.next_token();
                Expression::False
            },
            Token::Punct(Punct::Ellipsis) => {
                //TODO: validate in fn
                self.next_token();
                Expression::VarArgs
            },
            Token::Punct(Punct::OpenBrace) => {
                self.table_ctor()
            },
            _ => self.suffixed_exp(),
        }
    }

    fn suffixed_exp(&mut self) -> Expression<'a> {
        let expr = self.primary_exp();
        match &self.look_ahead {
            Some(Token::Punct(Punct::Dot)) => {
                self.field(expr)
            },
            Some(Token::Punct(Punct::OpenBracket)) => {
                self.index(expr)
            }
            Some(Token::Punct(Punct::Colon)) => {
                self.func_call(expr)
            },
            Some(Token::Punct(Punct::OpenParen))
            | Some(Token::Punct(Punct::OpenBrace))
            | Some(Token::LiteralString(_)) => {
                self.func_args();
                todo!()
            }
            _ => expr
        }
    }

    fn field(&mut self, expr: Expression<'a>) -> Expression<'a> {
        let colon_or_dot = self.next_token();
        let inner = Suffixed {
            subject: expr,
            property: self.exp(),
            computed: false,
            method: matches!(colon_or_dot, Some(Token::Punct(Punct::Colon))),
        };
        Expression::Suffixed(Box::new(inner))
    }

    fn index(&mut self, expr: Expression<'a>) -> Expression<'a> {
        let _bracket = self.next_token();
        let property = self.exp();
        let _bracket = self.next_token();
        let inner = Suffixed {
            subject: expr,
            property,
            computed: true,
            method: false,
        };
        Expression::Suffixed(Box::new(inner))
    }

    fn func_call(&mut self, expr: Expression<'a>) -> Expression<'a> {
        todo!()
    }

    fn primary_exp(&mut self) -> Expression<'a> {
        match &self.look_ahead {
            Some(Token::Punct(Punct::OpenParen)) => {
                let _paren = self.next_token();
                let inner = self.exp();
                Expression::Prefix(PrefixExp::Exp(Box::new(inner)))
            },
            _ => todo!()
        }
    }

    fn prefix_exp(&mut self) -> Expression<'a> {
        if matches!(self.look_ahead, Some(Token::Punct(Punct::OpenParen))) {
            let _paren = self.next_token();
            let inner = self.exp();
            let _paren = self.next_token();
            Expression::Prefix(PrefixExp::Exp(Box::new(inner)))
        } else {
            todo!()
        }
    }

    fn var(&mut self) -> Var<'a> {
        todo!()
    }

    fn table_ctor(&mut self) -> Expression<'a> {
        todo!()
    }

    pub fn next_token(&mut self) -> Option<Token<'a>> {
        std::mem::replace(&mut self.look_ahead, self.lex.next())
    }

    fn try_get_unary_op(&mut self) -> Option<UnaryOperator> {
        let op = match self.look_ahead.as_ref()? {
            Token::Punct(Punct::Minus) => UnaryOperator::Negate,
            Token::Keyword(Keyword::Not) => UnaryOperator::Not,
            Token::Punct(Punct::Hash) => UnaryOperator::Length,
            Token::Punct(Punct::Tilde) => UnaryOperator::BitwiseNot,
            _ => return None
        };
        let _op_token = self.next_token();
        Some(op)
    }
    fn try_get_binary_op(&mut self, limit: u8) -> Option<BinaryOperator> {
        let op = match self.look_ahead.as_ref()? {
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

    
}
