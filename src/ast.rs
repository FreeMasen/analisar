use std::{borrow::Cow, path::Prefix};

use bstr::BStr;

pub struct Chunk<'a>(pub Block<'a>);

pub struct Block<'a> {
    pub statements: Vec<Statement<'a>>,
    pub ret_stat: Option<RetStatement<'a>>,
}

pub enum Statement<'a> {
    Empty,
    Expression(Expression<'a>),
    Assignment {
        local: bool,
        targets: Vec<Expression<'a>>,
        values: Vec<Expression<'a>>,
    },
    FunctionCall,
    Label(Name<'a>),
    Break,
    GoTo(Name<'a>),
    Do {
        block: Box<Block<'a>>,
    },
    While {
        exp: Expression<'a>,
        block: Box<Block<'a>>,
    },
    Repeat {
        block: Box<Block<'a>>,
        exp: Expression<'a>,
    },
    If(If<'a>),
    For(ForLoop<'a>),
    ForIn(ForInLoop<'a>),
    Function {
        local: bool,
        name: FuncName<'a>,
        body: FuncBody<'a>,
    },
    AttrNameList {},
    Return(RetStatement<'a>),
}

struct VarList<'a>(Vec<Var<'a>>);
pub enum Var<'a> {
    Name(Name<'a>),
    Index {
        prefix: Box<PrefixExp<'a>>,
        index: Box<Expression<'a>>,
    },
    Field {
        prefix: Box<PrefixExp<'a>>,
        field: Name<'a>,
    },
}

pub enum PrefixExp<'a> {
    Var(Var<'a>),
    FunctionCall(FunctionCall<'a>),
    Exp(Box<Expression<'a>>),
}

pub struct FunctionCall<'a> {
    pub prefix: Box<PrefixExp<'a>>,
    pub args: Args<'a>,
    pub method: bool,
}

pub enum Args<'a> {
    ExpList(Vec<Expression<'a>>),
    Table(Table<'a>),
    String(LiteralString<'a>),
}

pub struct FuncName<'a> {
    pub dot_separated: Vec<Name<'a>>,
    pub method: Option<Name<'a>>,
}

pub struct If<'a> {
    pub test: Expression<'a>,
    pub block: Box<Block<'a>>,
    pub else_ifs: Vec<ElseIf<'a>>,
    pub catch_all: Option<Box<Block<'a>>>,
}

pub struct ElseIf<'a> {
    pub test: Expression<'a>,
    pub block: Block<'a>,
}

pub struct ForLoop<'a> {
    pub init_name: Name<'a>,
    pub init: Expression<'a>,
    pub limit: Expression<'a>,
    pub step: Option<Expression<'a>>,
    pub block: Box<Block<'a>>,
}

pub struct ForInLoop<'a> {
    pub name_list: NameList<'a>,
    pub exp_list: Vec<Expression<'a>>,
    pub block: Box<Block<'a>>,
}

pub struct RetStatement<'a>(pub Vec<Expression<'a>>);

pub enum Expression<'a> {
    Nil,
    False,
    True,
    Numeral(Numeral<'a>),
    LiteralString(LiteralString<'a>),
    Name(Name<'a>),
    VarArgs,
    FunctionDef(FuncBody<'a>),
    TableCtor(Box<Table<'a>>),
    BinOp {
        left: Box<Expression<'a>>,
        op: BinaryOperator,
        right: Box<Expression<'a>>,
    },
    UnaryOp {
        op: UnaryOperator,
        exp: Box<Expression<'a>>,
    },
    Prefix(PrefixExp<'a>),
    Suffixed(Box<Suffixed<'a>>),
}

impl<'a> Expression<'a> {
    pub fn binary(left: Expression<'a>, op: BinaryOperator, right: Expression<'a>) -> Self {
        Self::BinOp {
            left: Box::new(left),
            op,
            right: Box::new(right),
        }
    }

    pub fn unary(op: UnaryOperator, exp: Expression<'a>) -> Self {
        Self::UnaryOp {
            op,
            exp: Box::new(exp),
        }
    }
}

pub struct Suffixed<'a> {
    pub subject: Expression<'a>,
    pub property: Expression<'a>,
    pub computed: bool,
    pub method: bool,
}

pub struct NameList<'a>(pub Vec<Name<'a>>);

pub struct ParList<'a> {
    pub names: NameList<'a>,
    pub var_args: bool,
}

pub struct LiteralString<'a>(pub Cow<'a, BStr>);
pub struct Numeral<'a>(pub Cow<'a, str>);

pub struct FuncBody<'a> {
    pub par_list: ParList<'a>,
    pub block: Block<'a>,
}

pub struct Table<'a> {
    pub field_list: Vec<Field<'a>>,
}

pub enum Field<'a> {
    Record {
        name: Expression<'a>,
        value: Expression<'a>,
    },
    List(Expression<'a>),
}

pub struct Name<'a> {
    pub name: Cow<'a, str>,
    pub attr: Option<Cow<'a, str>>,
}

impl<'a> Name<'a> {
    pub fn new(name: Cow<'a, str>) -> Self {
        Self { name, attr: None }
    }

    pub fn new_with_attr(name: Cow<'a, str>, attr: Cow<'a, str>) -> Self {
        Self {
            name,
            attr: Some(attr),
        }
    }
}

pub enum FieldSeperator {
    Comma,
    Colon,
}
#[derive(Debug, Clone, Copy)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    FloorDivide,
    Power,
    Modulo,
    BitwiseAnd,
    BitwiseXor,
    BitwiseOr,
    RightShift,
    LeftShift,
    Concatenate,
    GreaterThan,
    GreaterThanEqual,
    LessThan,
    LessThanEqual,
    Equal,
    NotEqual,
    And,
    Or,
}

impl BinaryOperator {
    pub fn priority(self) -> (u8, u8) {
        match self {
            Self::Power => (14, 13),
            Self::Multiply | Self::Modulo | Self::Divide | Self::FloorDivide => (11, 11),
            Self::Add | Self::Subtract => (10, 10),
            Self::Concatenate => (9, 8),
            Self::RightShift | Self::LeftShift => (7, 7),
            Self::BitwiseAnd => (6, 6),
            Self::BitwiseXor => (5, 5),
            Self::BitwiseOr => (4, 4),
            Self::GreaterThan
            | Self::GreaterThanEqual
            | Self::LessThan
            | Self::LessThanEqual
            | Self::Equal
            | Self::NotEqual => (3, 3),
            Self::And => (2, 2),
            Self::Or => (1, 1),
        }
    }
}

pub enum UnaryOperator {
    Negate,
    Not,
    Length,
    BitwiseNot,
}
