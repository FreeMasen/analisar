use std::borrow::Cow;

use ast::{
    Args, BinaryOperator, ElseIf, Expression, Field, ForInLoop, ForLoop, FuncBody, FuncName,
    FunctionCall, If, LiteralString, Name, NameList, Numeral, ParList, PrefixExp, RetStatement,
    Statement, Suffixed, Table, UnaryOperator
};
use lex_lua::{Keyword, Lexer, Punct, Token};

mod ast;

pub struct Parser<'a> {
    lex: Lexer<'a>,
    look_ahead: Option<lex_lua::Token<'a>>,
    look_ahead2: Option<lex_lua::Token<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(bytes: &'a [u8]) -> Self {
        let mut lex = Lexer::new(bytes);
        let look_ahead = lex.next();
        let look_ahead2 = lex.next();
        Self {
            lex,
            look_ahead,
            look_ahead2,
        }
    }

    pub fn chunk(&mut self) -> ast::Chunk<'a> {
        ast::Chunk(self.block())
    }

    pub fn block(&mut self) -> ast::Block<'a> {
        let mut statements = Vec::new();
        let mut ret_stat = None;
        while !self.at_block_end() {
            if matches!(self.look_ahead, Some(Token::Keyword(Keyword::Return))) {
                ret_stat = Some(self.ret_stat());
                break;
            } else {
                statements.push(self.statement());
            }
        }

        ast::Block {
            statements,
            ret_stat,
        }
    }

    pub fn statement(&mut self) -> Statement<'a> {
        match self.look_ahead {
            Some(Token::Punct(Punct::SemiColon)) => {
                let _ = self.next_token();
                Statement::Empty
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
                let _local = self.next_token();
                if matches!(self.look_ahead, Some(Token::Keyword(Keyword::Function))) {
                    self.function(true)
                } else {
                    self.assignment(true)
                }
            }
            Some(Token::Punct(Punct::DoubleColon)) => self.label(),
            Some(Token::Keyword(Keyword::Return)) => {
                let _return = self.next_token();
                Statement::Return(self.ret_stat())
            }
            _ => self.exp_stat(),
        }
    }

    fn ret_stat(&mut self) -> RetStatement<'a> {
        let _return = self.next_token();
        let exps = self.exp_list();
        RetStatement(exps)
    }

    fn exp_stat(&mut self) -> Statement<'a> {
        let base = self.suffixed_exp();
        if matches!(
            self.look_ahead,
            Some(Token::Punct(Punct::Equal)) | Some(Token::Punct(Punct::Comma))
        ) {
            self.assign_cont(false, base)
        } else {
            // TODO Validate function call
            Statement::Expression(base)
        }
    }

    pub fn assignment(&mut self, local: bool) -> Statement<'a> {
        let start = self.suffixed_exp();
        self.assign_cont(local, start)
    }

    pub fn assign_cont(&mut self, local: bool, start: Expression<'a>) -> Statement<'a> {
        let mut targets = vec![start];
        while self.eat_punct(Punct::Comma) {
            targets.push(self.suffixed_exp())
        }
        assert!(self.eat_punct(Punct::Equal));
        let values = self.exp_list();
        Statement::Assignment {
            local,
            targets,
            values,
        }
    }

    pub fn label(&mut self) -> Statement<'a> {
        let _colons = self.next_token();
        let name = self.name();
        let _colons = self.next_token();
        Statement::Label(name)
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
        if !matches!(self.look_ahead, Some(Token::Punct(Punct::Equal))) {
            return self.for_in_loop(name);
        }
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
            block: Box::new(block),
        })
    }

    fn for_in_loop(&mut self, first_name: Name<'a>) -> Statement<'a> {
        let _for = self.next_token();
        let name_list = self.name_list_cont(first_name);
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

    fn name_list_cont(&mut self, first_name: Name<'a>) -> NameList<'a> {
        let mut ret = Vec::new();
        ret.push(first_name);
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
        let _paren = self.next_token();
        let par_list = self.par_list();
        let _paren = self.next_token();
        let block = self.block();
        let _end = self.next_token();
        FuncBody { par_list, block }
    }

    fn func_args(&mut self) -> Args<'a> {
        match &self.look_ahead {
            Some(Token::Punct(Punct::OpenParen)) => {
                let _paren = self.next_token();
                let args = if matches!(self.look_ahead, Some(Token::Punct(Punct::CloseParen))) {
                    Vec::new()
                } else {
                    self.exp_list()
                };
                let _paren = self.next_token();
                Args::ExpList(args)
            }
            Some(Token::Punct(Punct::OpenBrace)) => {
                let args = self.table_ctor();
                Args::Table(args)
            }
            Some(Token::LiteralString(s)) => {
                let arg = LiteralString(s.clone());
                let _s = self.next_token();
                Args::String(arg)
            }
            _ => panic!("Invalid func args"),
        }
    }

    pub fn par_list(&mut self) -> ParList<'a> {
        let mut names = Vec::new();
        let var_args = loop {
            match &self.look_ahead {
                Some(Token::Name(_)) => {
                    let name = self.name();
                    names.push(name);
                    if !matches!(self.look_ahead, Some(Token::Punct(Punct::Comma))) {
                        break false;
                    }
                    let _comma = self.next_token();
                }
                Some(Token::Punct(Punct::Ellipsis)) => {
                    let _ellipsis = self.next_token();
                    break true;
                }
                _ => panic!("Invalid par list"),
            }
        };
        ParList {
            names: NameList(names),
            var_args,
        }
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
        let first = self.exp();
        let mut ret = vec![first];
        while matches!(self.look_ahead, Some(Token::Punct(Punct::Comma))) {
            let _comma = self.next_token();
            ret.push(self.exp())
        }
        ret
    }

    fn exp(&mut self) -> Expression<'a> {
        self.sub_exp(0)
    }

    fn sub_exp(&mut self, limit: u8) -> Expression<'a> {
        let mut base = if let Some(op) = self.try_get_unary_op() {
            let exp = self.sub_exp(12);
            Expression::UnaryOp {
                op,
                exp: Box::new(exp),
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
            }
            Token::LiteralString(s) => {
                let exp = Expression::LiteralString(LiteralString(s.clone()));
                let _s_tok = self.next_token();
                exp
            }
            Token::Keyword(Keyword::Nil) => {
                self.next_token();
                Expression::Nil
            }
            Token::Keyword(Keyword::True) => {
                self.next_token();
                Expression::True
            }
            Token::Keyword(Keyword::False) => {
                self.next_token();
                Expression::False
            }
            Token::Punct(Punct::Ellipsis) => {
                //TODO: validate in fn
                self.next_token();
                Expression::VarArgs
            }
            Token::Punct(Punct::OpenBrace) => {
                let inner = self.table_ctor();
                Expression::TableCtor(Box::new(inner))
            }
            _ => self.suffixed_exp(),
        }
    }

    fn suffixed_exp(&mut self) -> Expression<'a> {
        let expr = self.primary_exp();
        match &self.look_ahead {
            Some(Token::Punct(Punct::Dot)) => self.field(expr),
            Some(Token::Punct(Punct::OpenBracket)) => self.index(expr),
            Some(Token::Punct(Punct::Colon)) => self.func_call(expr, true),
            Some(Token::Punct(Punct::OpenParen))
            | Some(Token::Punct(Punct::OpenBrace))
            | Some(Token::LiteralString(_)) => {
                self.func_call(expr, false)
            }
            _ => expr,
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

    fn func_call(&mut self, expr: Expression<'a>, method: bool) -> Expression<'a> {
        let _paren = self.next_token();
        let args = self.func_args();
        let _paren = self.next_token();
        
        let call = FunctionCall {
            prefix: Box::new(PrefixExp::Exp(Box::new(expr))),
            args,
            method,
        };
        let pre = PrefixExp::FunctionCall(call);
        Expression::Prefix(pre)
    }

    fn primary_exp(&mut self) -> Expression<'a> {
        match &self.look_ahead {
            Some(Token::Punct(Punct::OpenParen)) => {
                let _paren = self.next_token();
                let inner = self.exp();
                Expression::Prefix(PrefixExp::Exp(Box::new(inner)))
            }
            Some(Token::Name(n)) => {
                let name = Name::new(n.clone());
                let _ = self.next_token();
                Expression::Name(name)
            }
            _ => panic!("Invalid primary expression {:?}", self.look_ahead),
        }
    }

    fn table_ctor(&mut self) -> Table<'a> {
        let _brace = self.next_token();
        let mut field_list = Vec::new();
        while !self.eat_punct(Punct::CloseBrace) {
            field_list.push(self.field_init())
        }
        Table { field_list }
    }

    fn field_init(&mut self) -> Field<'a> {
        match &self.look_ahead {
            Some(Token::Name(_)) => {
                if matches!(self.look_ahead2, Some(Token::Punct(Punct::Equal))) {
                    self.record_field()
                } else {
                    self.list_field()
                }
            }
            Some(Token::Punct(Punct::OpenBracket)) => self.record_field(),
            _ => self.list_field(),
        }
    }

    fn record_field(&mut self) -> Field<'a> {
        let name = if matches!(&self.look_ahead, Some(Token::Name(_))) {
            let name = self.name();
            Expression::Name(name)
        } else {
            let _open = self.next_token();
            let exp = self.exp();
            let _close = self.next_token();
            exp
        };
        let _eq = self.next_token();
        Field::Record {
            name,
            value: self.exp()
        }
    }

    fn list_field(&mut self) -> Field<'a> {
        let exp = self.exp();
        Field::List(exp)
    }

    fn at_block_end(&self) -> bool {
        matches!(
            self.look_ahead,
            None | Some(Token::Keyword(Keyword::Else))
                | Some(Token::Keyword(Keyword::ElseIf))
                | Some(Token::Keyword(Keyword::End))
        )
    }

    fn eat_punct(&mut self, _p: Punct) -> bool {
        if matches!(&self.look_ahead, _p) {
            self.next_token();
            true
        } else {
            false
        }
    }

    pub fn next_token(&mut self) -> Option<Token<'a>> {
        use std::mem::replace;
        replace(
            &mut self.look_ahead,
            replace(&mut self.look_ahead2, self.lex.next()),
        )
    }

    fn try_get_unary_op(&mut self) -> Option<UnaryOperator> {
        let op = match self.look_ahead.as_ref()? {
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

#[cfg(test)]
mod test {
    use ast::Block;

    use super::*;

    #[test]
    fn print() {
        let lua = "print('hello world')";
        let mut parser = Parser::new(lua.as_bytes());
        let block = parser.block();
        let name = Expression::Name(Name::new(Cow::Borrowed("print")));
        let arg = Expression::string("'hello world'");
        let args = Args::ExpList(vec![arg]);
        compare_blocs(block, Block {
            statements: vec![
                Statement::func_call(
                    name, args
                )
            ],
            ret_stat: None,
        })
    }

    #[track_caller]
    fn compare_blocs(test: Block, target: Block) {
        for (lhs, rhs) in test.statements.iter().zip(target.statements.iter()) {
            assert!(matches!(lhs, rhs), "Invalid statement, expected {:?}, found {:?}", rhs, lhs)
        }
    }
}
