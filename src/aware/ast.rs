use bstr::BStr;
use lex_lua::{Item, Span};
use std::borrow::Cow;

#[derive(Debug, PartialEq, Clone)]
pub struct Block<'a>(pub Vec<Statement<'a>>);
impl<'a> Block<'a> {
    pub fn empty() -> Self {
        Self(Vec::new())
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
                ret_stat
                    .exprs
                    .last()
                    .map(ExpListItem::end)
                    .unwrap_or(ret_stat.return_span.end),
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
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum UnaryOperator {
    Negate(Span),
    Not(Span),
    Length(Span),
    BitwiseNot(Span),
}

impl UnaryOperator {
    pub fn start(&self) -> usize {
        match self {
            Self::Negate(span) | Self::Not(span) | Self::Length(span) | Self::BitwiseNot(span) => {
                span.start
            }
        }
    }
}
