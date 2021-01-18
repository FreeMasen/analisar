use bstr::BStr;
use lex_lua::Span;
use std::borrow::Cow;

#[derive(Debug, PartialEq, Clone)]
pub struct Block<'a>(pub Vec<Statement<'a>>);
impl<'a> Block<'a> {
    pub fn empty() -> Self {
        Self(Vec::new())
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
        name: FuncName<'a>,
        body: FuncBody<'a>,
    },
    Return(RetStatement<'a>),
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

#[derive(Debug, PartialEq, Clone)]
pub enum ExpListItem<'a> {
    Expr(Expression<'a>),
    Comma(Span),
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
        exprs: Vec<ExpListItem<'a>>,
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
