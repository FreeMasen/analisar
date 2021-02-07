use bstr::BStr;
use lex_lua::{Item, Span};
use std::borrow::Cow;

#[derive(Debug, PartialEq, Clone)]
pub struct Block<'a>(pub Vec<Statement<'a>>);
impl<'a> Block<'a> {
    pub fn empty() -> Self {
        Self(Vec::new())
    }
    pub fn start(&self) -> Option<usize> {
        let stmt = self.0.first()?;
        stmt.start()
    }
    pub fn end(&self) -> Option<usize> {
        let stmt = self.0.last()?;
        stmt.end()
    }
}

pub struct BlockWithComments<'a>(pub Vec<StatementWithComments<'a>>);

impl<'a> BlockWithComments<'a> {
    pub fn empty() -> Self {
        Self(Vec::new())
    }

    pub fn start(&self) -> Option<usize> {
        let stmt = self.0.first()?;
        stmt.start()
    }

    pub fn end(&self) -> Option<usize> {
        let stmt = self.0.last()?;
        stmt.end()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct StatementWithComments<'a> {
    pub statement: Statement<'a>,
    pub comments: Vec<Item<'a>>,
}

impl<'a> StatementWithComments<'a> {
    pub fn start(&self) -> Option<usize> {
        self.statement.start()
    }

    pub fn end(&self) -> Option<usize> {
        self.statement.end()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement<'a> {
    Empty(Span),
    Expression(Expression<'a>),
    Assignment {
        local_span: Option<Span>,
        targets: Vec<ExpListItem<'a>>,
        eq_span: Option<Span>,
        values: Vec<ExpListItem<'a>>,
    },
    Label {
        colons1_span: Span,
        name: Name<'a>,
        colons2_span: Span,
    },
    Break(Span),
    GoTo {
        goto_span: Span,
        label: Name<'a>,
    },
    Do {
        do_span: Span,
        block: Block<'a>,
        end_span: Span,
    },
    While {
        while_span: Span,
        exp: Expression<'a>,
        do_span: Span,
        block: Block<'a>,
        end_span: Span,
    },
    Repeat {
        repeat_span: Span,
        block: Block<'a>,
        until_span: Span,
        exp: Expression<'a>,
    },
    If(If<'a>),
    For(ForLoop<'a>),
    ForIn(ForInLoop<'a>),
    Function {
        local: Option<Span>,
        function: Span,
        name: FuncName<'a>,
        body: FuncBody<'a>,
    },
    Return(RetStatement<'a>),
}

impl<'a> Statement<'a> {
    pub fn start(&self) -> Option<usize> {
        match self {
            Self::Expression(expr) => Some(expr.start()),
            Self::Assignment {
                local_span,
                targets,
                ..
            } => {
                if let Some(local) = local_span {
                    Some(local.start)
                } else {
                    let first = targets.first()?;
                    Some(first.start())
                }
            }

            Self::Function {
                local, function, ..
            } => {
                if let Some(local) = local {
                    Some(local.start)
                } else {
                    Some(function.start)
                }
            }
            Self::Return(ret_stat) => Some(
                ret_stat.return_span.start
                
            ),
            Self::Empty(span)
            | Self::Break(span)
            | Self::Label {
                colons1_span: span, ..
            }
            | Self::Repeat {
                repeat_span: span, ..
            }
            | Self::GoTo {
                goto_span: span, ..
            }
            | Self::Do { do_span: span, .. }
            | Self::While {
                while_span: span, ..
            }
            | Self::If(If { if_span: span, .. })
            | Self::For(ForLoop { for_span: span, .. })
            | Self::ForIn(ForInLoop { for_span: span, .. }) => Some(span.start),
        }
    }
    pub fn end(&self) -> Option<usize> {
        match self {
            Self::Empty(Span { end, .. }) => Some(*end),
            Self::Expression(expr) => Some(expr.end()),
            Self::Assignment {
                local_span,
                targets,
                eq_span,
                values,
            } => {
                if let Some(eq_span) = eq_span {
                    Some(values.last().map(ExpListItem::end).unwrap_or(eq_span.end))
                } else {
                    if let Some(end) = targets.last().map(ExpListItem::end) {
                        Some(end)
                    } else {
                        local_span.map(|s| s.end)
                    }
                }
            }
            Self::Label { colons2_span, .. } => Some(colons2_span.end),
            Self::Break(Span { end, .. }) => Some(*end),
            Self::GoTo { label, .. } => Some(label.end()),
            Self::Repeat { exp, .. } => Some(exp.end()),
            Self::Do { end_span, .. }
            | Self::While { end_span, .. }
            | Self::If(If { end_span, .. })
            | Self::For(ForLoop { end_span, .. })
            | Self::ForIn(ForInLoop { end_span, .. })
            | Self::Function {
                body: FuncBody { end_span, .. },
                ..
            } => Some(end_span.end),
            Self::Return(ret_stat) => Some(
                ret_stat
                    .exprs
                    .last()
                    .map(ExpListItem::end)
                    .unwrap_or(ret_stat.return_span.end),
            ),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct LiteralString<'a> {
    pub span: Span,
    pub value: Cow<'a, BStr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FuncName<'a> {
    pub segments: Vec<FuncNamePart<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum FuncNamePart<'a> {
    Name(Name<'a>),
    Dot(Span),
    Colon(Span),
}

#[derive(Debug, PartialEq, Clone)]
pub enum NameListPart<'a> {
    Name(Name<'a>),
    Comma(Span),
}

#[derive(Debug, PartialEq, Clone)]
pub struct If<'a> {
    pub if_span: Span,
    pub test: Expression<'a>,
    pub then_span: Span,
    pub block: Block<'a>,
    pub else_ifs: Vec<ElseIf<'a>>,
    pub else_span: Option<Span>,
    pub catch_all: Option<Block<'a>>,
    pub end_span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ElseIf<'a> {
    pub else_if_span: Span,
    pub test: Expression<'a>,
    pub then_span: Span,
    pub block: Block<'a>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ForLoop<'a> {
    pub for_span: Span,
    pub init_name: Name<'a>,
    pub eq_span: Span,
    pub init: Expression<'a>,
    pub comma1_span: Span,
    pub limit: Expression<'a>,
    pub comma2_span: Option<Span>,
    pub step: Option<Expression<'a>>,
    pub do_span: Span,
    pub block: Block<'a>,
    pub end_span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ForInLoop<'a> {
    pub for_span: Span,
    pub name_list: Vec<NameListPart<'a>>,
    pub in_span: Span,
    pub exp_list: Vec<ExpListItem<'a>>,
    pub do_span: Span,
    pub block: Block<'a>,
    pub end_span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct RetStatement<'a> {
    pub return_span: Span,
    pub exprs: Vec<ExpListItem<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression<'a> {
    Nil(Span),
    False(Span),
    True(Span),
    Numeral(Numeral<'a>),
    LiteralString(LiteralString<'a>),
    Name(Name<'a>),
    VarArgs(Span),
    FunctionDef(FuncBody<'a>),
    TableCtor(Box<Table<'a>>),
    Parened {
        open_span: Span,
        expr: Box<Expression<'a>>,
        close_span: Span,
    },
    BinOp {
        left: Box<Expression<'a>>,
        op: BinaryOperator,
        right: Box<Expression<'a>>,
    },
    UnaryOp {
        op: UnaryOperator,
        exp: Box<Expression<'a>>,
    },
    FuncCall(FunctionCall<'a>),
    Suffixed(Box<Suffixed<'a>>),
}

impl<'a> Expression<'a> {
    pub fn start(&self) -> usize {
        match self {
            Self::Parened { open_span, .. } => open_span.start,
            Self::Numeral(Numeral { span, .. }) => span.start,
            Self::LiteralString(LiteralString { span, .. }) => span.start,
            Self::Name(name) => name.start(),
            Self::Nil(span) => span.start,
            Self::Suffixed(suff) => suff.subject.start(),
            Self::TableCtor(table) => table.open_brace.start,
            Self::True(span) => span.start,
            Self::UnaryOp { op, .. } => op.start(),
            Self::BinOp { left, .. } => left.start(),
            Self::VarArgs(span) => span.start,
            Self::False(span) => span.start,
            Self::FunctionDef(body) => body.open_paren_span.start,
            Self::FuncCall(call) => call.prefix.start(),
        }
    }
    pub fn end(&self) -> usize {
        match self {
            Self::Parened { close_span, .. } => close_span.end,
            Self::Numeral(Numeral { span, .. }) => span.end,
            Self::LiteralString(LiteralString { span, .. }) => span.end,
            Self::Name(name) => name.end(),
            Self::Nil(span) => span.end,
            Self::Suffixed(suff) => match &suff.property {
                SuffixedProperty::Name { name, .. } => name.end(),
                SuffixedProperty::Computed { close_bracket, .. } => close_bracket.end,
            },
            Self::TableCtor(table) => table.close_brace.end,
            Self::True(span) => span.end,
            Self::UnaryOp { exp, .. } => exp.end(),
            Self::BinOp { right, .. } => right.end(),
            Self::VarArgs(span) => span.end,
            Self::False(span) => span.end,
            Self::FunctionDef(body) => body.end_span.end,
            Self::FuncCall(call) => call.args.end(),
        }
    }

    pub fn name_from(s: &'a str, start: usize) -> Self {
        Self::Name(Name {
            name_span: Span {
                start,
                end: start + s.len(),
            },
            name: Cow::Borrowed(s),
            attr: None,
        })
    }

    pub fn string_from(s: &'a str, start: usize) -> Self {
        Self::LiteralString(LiteralString {
            span: Span {
                start,
                end: start + s.len(),
            },
            value: Cow::Borrowed(s.into()),
        })
    }
    pub fn numeral_from(s: &'a str, start: usize) -> Self {
        Self::Numeral(Numeral {
            span: Span {
                start,
                end: start + s.len(),
            },
            numeral: Cow::Borrowed(s.into()),
        })
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Suffixed<'a> {
    pub subject: Expression<'a>,
    pub property: SuffixedProperty<'a>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum SuffixedProperty<'a> {
    Name {
        sep: SuffixSep,
        name: Name<'a>,
    },
    Computed {
        open_bracket: Span,
        expr: Expression<'a>,
        close_bracket: Span,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum SuffixSep {
    Dot(Span),
    Colon(Span),
}

#[derive(Debug, PartialEq, Clone)]
pub struct ParList<'a> {
    pub parts: Vec<ParListPart<'a>>,
}

impl<'a> ParList<'a> {
    pub fn empty() -> Self {
        Self { parts: Vec::new() }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ParListPart<'a> {
    Name(Name<'a>),
    Comma(Span),
    VarArgs(Span),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Numeral<'a> {
    pub numeral: Cow<'a, str>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FuncBody<'a> {
    pub open_paren_span: Span,
    pub par_list: ParList<'a>,
    pub close_paren_span: Span,
    pub block: Block<'a>,
    pub end_span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Table<'a> {
    pub open_brace: Span,
    pub field_list: Vec<Field<'a>>,
    pub close_brace: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Field<'a> {
    Record {
        name: Expression<'a>,
        eq: Span,
        value: Expression<'a>,
        sep: Option<FieldSep>,
    },
    List {
        value: Expression<'a>,
        sep: Option<FieldSep>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum FieldSep {
    Comma(Span),
    Semi(Span),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Name<'a> {
    pub name_span: Span,
    pub name: Cow<'a, str>,
    pub attr: Option<Attr<'a>>,
}

impl<'a> Name<'a> {
    pub fn start(&self) -> usize {
        self.name_span.start
    }
    pub fn end(&self) -> usize {
        if let Some(attr) = &self.attr {
            attr.close_angle.end
        } else {
            self.name_span.end
        }
    }
    pub fn from_str(s: &'a str, start: usize) -> Self {
        Name {
            name_span: Span {
                start,
                end: start + s.len(),
            },
            name: Cow::Borrowed(s),
            attr: None,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Attr<'a> {
    pub open_angle: Span,
    pub value: Cow<'a, str>,
    pub close_angle: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionCall<'a> {
    pub prefix: Box<Expression<'a>>,
    pub args: Args<'a>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Args<'a> {
    ExpList {
        open_paren: Span,
        exprs: Vec<ExpListItem<'a>>,
        close_paren: Span,
    },
    Table(Table<'a>),
    String(LiteralString<'a>),
}

impl<'a> Args<'a> {
    pub fn end(&self) -> usize {
        match self {
            Args::ExpList { close_paren, .. } => close_paren.end,
            Args::Table(t) => t.close_brace.end,
            Args::String(s) => s.span.end,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExpListItem<'a> {
    Expr(Expression<'a>),
    Comma(Span),
}

impl<'a> ExpListItem<'a> {
    fn start(&self) -> usize {
        match self {
            Self::Expr(expr) => expr.start(),
            Self::Comma(comma) => comma.start,
        }
    }
    fn end(&self) -> usize {
        match self {
            Self::Expr(expr) => expr.end(),
            Self::Comma(comma) => comma.end,
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BinaryOperator {
    Add(Span),
    Subtract(Span),
    Multiply(Span),
    Divide(Span),
    FloorDivide(Span),
    Power(Span),
    Modulo(Span),
    BitwiseAnd(Span),
    BitwiseXor(Span),
    BitwiseOr(Span),
    RightShift(Span),
    LeftShift(Span),
    Concatenate(Span),
    GreaterThan(Span),
    GreaterThanEqual(Span),
    LessThan(Span),
    LessThanEqual(Span),
    Equal(Span),
    NotEqual(Span),
    And(Span),
    Or(Span),
}

impl BinaryOperator {
    pub fn priority(&self) -> (u8, u8) {
        match self {
            Self::Power(_) => (14, 13),
            Self::Multiply(_) | Self::Modulo(_) | Self::Divide(_) | Self::FloorDivide(_) => {
                (11, 11)
            }
            Self::Add(_) | Self::Subtract(_) => (10, 10),
            Self::Concatenate(_) => (9, 8),
            Self::RightShift(_) | Self::LeftShift(_) => (7, 7),
            Self::BitwiseAnd(_) => (6, 6),
            Self::BitwiseXor(_) => (5, 5),
            Self::BitwiseOr(_) => (4, 4),
            Self::GreaterThan(_)
            | Self::GreaterThanEqual(_)
            | Self::LessThan(_)
            | Self::LessThanEqual(_)
            | Self::Equal(_)
            | Self::NotEqual(_) => (3, 3),
            Self::And(_) => (2, 2),
            Self::Or(_) => (1, 1),
        }
    }

    pub fn span(&self) -> &Span {
        match self {
            Self::Add(span)
            | Self::Subtract(span)
            | Self::Multiply(span)
            | Self::Divide(span)
            | Self::FloorDivide(span)
            | Self::Power(span)
            | Self::Modulo(span)
            | Self::BitwiseAnd(span)
            | Self::BitwiseXor(span)
            | Self::BitwiseOr(span)
            | Self::RightShift(span)
            | Self::LeftShift(span)
            | Self::Concatenate(span)
            | Self::GreaterThan(span)
            | Self::GreaterThanEqual(span)
            | Self::LessThan(span)
            | Self::LessThanEqual(span)
            | Self::Equal(span)
            | Self::NotEqual(span)
            | Self::And(span)
            | Self::Or(span) => span,
        }
    }

    pub fn start(&self) -> usize {
        self.span().start
    }
    pub fn end(&self) -> usize {
        self.span().end
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum UnaryOperator {
    Negate(Span),
    Not(Span),
    Length(Span),
    BitwiseNot(Span),
}

impl UnaryOperator {
    pub fn span(&self) -> Span {
        match self {
            Self::Negate(span) | Self::Not(span) | Self::Length(span) | Self::BitwiseNot(span) => {
                *span
            }
        }
    }
    pub fn start(&self) -> usize {
        self.span().start
    }
    pub fn end(&self) -> usize {
        self.span().end
    }
}

#[cfg(test)]
mod test {
    use core::panic;

    use super::*;

    #[test]
    fn assign_bin_op_span() {
        let block = parse_lua("i = 1 - 1");
        assert_eq!(block.start(), Some(0));
        assert_eq!(block.end(), Some(9));
        let first_stmt = block.0.first().unwrap();
        assert_eq!(first_stmt.start(), Some(0));
        assert_eq!(first_stmt.end(), Some(9));
        match &first_stmt.statement {
            Statement::Assignment {
                targets,
                eq_span,
                values,
                ..
            } => {
                let i = targets.first().unwrap();
                assert_eq!(i.start(), 0);
                assert_eq!(i.end(), 1);
                let eq = eq_span.unwrap();
                assert_eq!(eq.start, 2);
                assert_eq!(eq.end, 3);
                let bin = values.first().unwrap();
                match bin {
                    ExpListItem::Expr(exp) => match exp {
                        Expression::BinOp { left, op, right } => {
                            assert_eq!(left.start(), 4);
                            assert_eq!(left.end(), 5);
                            assert_eq!(op.start(), 6);
                            assert_eq!(op.end(), 7);
                            assert_eq!(right.start(), 8);
                            assert_eq!(right.end(), 9);
                        }
                        _ => panic!("Expected binary operation"),
                    },
                    _ => panic!("Expected Expression"),
                }
            }
            _ => panic!("Expected Statement::Assignment"),
        }
    }
    #[test]
    fn if_break_goto() {
        let block = parse_lua(
            "if 1 // 2 == 99 then
    break
else
    goto top
end",
        );
        assert_eq!(block.start(), Some(0));
        assert_eq!(block.end(), Some(52));
        let first_stmt = block.0.first().unwrap();
        assert_eq!(first_stmt.statement.start(), Some(0));
        assert_eq!(first_stmt.statement.end(), Some(52));
        match &first_stmt.statement {
            Statement::If(If {
                if_span,
                test,
                then_span,
                block,
                else_span,
                catch_all,
                end_span,
                ..
            }) => {
                assert_eq!(if_span.start, 0);
                assert_eq!(if_span.end, 2);
                assert_eq!(test.start(), 3);
                assert_eq!(test.end(), 15);
                if let Expression::BinOp { left, op, right } = test {
                    assert_eq!(left.start(), 3);
                    assert_eq!(left.end(), 9);
                    if let Expression::BinOp {
                        left: left2,
                        op: op2,
                        right: right2,
                    } = left.as_ref()
                    {
                        assert_eq!(left2.start(), 3);
                        assert_eq!(left2.end(), 4);
                        assert_eq!(op2.start(), 5);
                        assert_eq!(op2.end(), 7);
                        assert_eq!(right2.start(), 8);
                        assert_eq!(right2.end(), 9);
                    }
                    assert_eq!(op.start(), 10);
                    assert_eq!(op.end(), 12);
                    assert_eq!(right.start(), 13);
                    assert_eq!(right.end(), 15);
                } else {
                    panic!("Expected test to be BinOp");
                }
                assert_eq!(then_span.start, 16);
                assert_eq!(then_span.end, 20);
                assert_eq!(block.start(), Some(25));
                assert_eq!(block.end(), Some(30));
                let el = else_span.unwrap();
                assert_eq!(el.start, 31);
                assert_eq!(el.end, 35);

                if let Statement::GoTo { goto_span, label } =
                    catch_all.as_ref().unwrap().0.first().unwrap()
                {
                    assert_eq!(goto_span.start, 40);
                    assert_eq!(goto_span.end, 44);
                    assert_eq!(label.start(), 45);
                    assert_eq!(label.end(), 48);
                } else {
                    panic!("Expected GoTo");
                }
                assert_eq!(end_span.start, 49);
                assert_eq!(end_span.end, 52);
            }
            _ => panic!("Expected Statement::If"),
        }
    }

    #[test]
    fn func_spans() {
        let block = parse_lua(
            "local function name(arg1, arg2)
    return not (arg1 > arg2)
end",
        );
        assert_eq!(block.start(), Some(0));
        assert_eq!(block.end(), Some(64));
        let func = &block.0.first().unwrap().statement;
        assert_eq!(func.start(), Some(0));
        assert_eq!(func.end(), Some(64));
        if let Statement::Function {
            local,
            function,
            name,
            body,
        } = func
        {
            let l = local.as_ref().unwrap();
            assert_eq!(l.start, 0);
            assert_eq!(l.end, 5);
            assert_eq!(function.start, 6);
            assert_eq!(function.end, 14);
            let n = name.segments.first().unwrap();
            if let FuncNamePart::Name(n) = n {
                assert_eq!(n.start(), 15);
                assert_eq!(n.end(), 19);
            } else {
                panic!("expected name");
            }
            assert_eq!(body.open_paren_span.start, 19);
            assert_eq!(body.open_paren_span.end, 20);
            let mut pars = body.par_list.parts.iter();
            if let ParListPart::Name(a1) = pars.next().unwrap() {
                assert_eq!(a1.start(), 20);
                assert_eq!(a1.end(), 24);
            } else {
                panic!("expected name");
            }
            if let ParListPart::Comma(sp) = pars.next().unwrap() {
                assert_eq!(sp.start, 24);
                assert_eq!(sp.end, 25);
            } else {
                panic!("expected comma");
            }
            if let ParListPart::Name(a2) = pars.next().unwrap() {
                assert_eq!(a2.start(), 26);
                assert_eq!(a2.end(), 30);
            } else {
                panic!("expected name");
            }
            assert_eq!(body.close_paren_span.start, 30);
            assert_eq!(body.close_paren_span.end, 31);
            assert_eq!(body.block.0.len(), 1);
            let ret_stat = body.block.0.first().unwrap();
            assert_eq!(ret_stat.start(), Some(36));
            assert_eq!(ret_stat.end(), Some(60));

            if let Statement::Return(ret) = ret_stat {
                assert_eq!(ret.return_span.start, 36);
                assert_eq!(ret.return_span.end, 42);
                let val = ret.exprs.first().unwrap();
                if let ExpListItem::Expr(Expression::UnaryOp { op, exp }) = val {
                    assert_eq!(op.start(), 43);
                    assert_eq!(op.end(), 46);
                    if let Expression::Parened {open_span, expr: bin, close_span} = exp.as_ref() {
                        assert_eq!(open_span.start, 47);
                        assert_eq!(open_span.end, 48);
                        if let Expression::BinOp { left, op, right } = bin.as_ref() {
                            assert_eq!(left.start(), 48);
                            assert_eq!(left.end(), 52);
                            assert_eq!(op.start(), 53);
                            assert_eq!(op.end(), 54);
                            assert_eq!(right.start(), 55);
                            assert_eq!(right.end(), 59);
                        } else {
                            panic!("Expected binary operation")
                        }
                        assert_eq!(close_span.start, 59);
                        assert_eq!(close_span.end, 60);
                    } else {
                        panic!("expected unary operation");
                    }
                } else {
                    panic!("expected unary op found {:#?}", val);
                }
            } else {
                panic!()
            }
            assert_eq!(body.end_span.start, 61);
            assert_eq!(body.end_span.end, 64);
        } else {
            panic!("expected function");
        }
    }

    fn parse_lua<'a>(lua: &'a str) -> BlockWithComments<'a> {
        use crate::aware::Parser;
        let mut p = Parser::new(lua.as_bytes());
        p.block().unwrap()
    }
}
