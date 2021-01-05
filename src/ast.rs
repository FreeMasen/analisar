use std::borrow::Cow;

use bstr::BStr;

pub struct Chunk<'a>(pub Block<'a>);

pub struct Block<'a> {
    pub statement: Statement<'a>,
    pub retstat: Option<RetStatement<'a>>,
}

pub enum Statement<'a> {
    Assignment {
        local: bool,
        name_list: NameList<'a>,
        exp_list: Vec<Expression<'a>>,
    },
    FunctionCall,
    Label(Name<'a>),
    Break,
    GoTo(Name<'a>),
    Do {
        block: Box<Block<'a>>
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
    AttrNameList {
        
    }
}

struct VarList<'a>(Vec<Var<'a>>);
pub enum Var<'a> {
    Name(Name<'a>),
    Index {
        prefix: Box<PrefixExp<'a>>,
    }
}

enum PrefixExp<'a> {
    Var(Var<'a>),
    FunctionCall(FunctionCall<'a>),
    Exp(Expression<'a>)
}

struct FunctionCall<'a> {
    prefix: Box<PrefixExp<'a>>,
    method: Name<'a>,
    args: Args<'a>,
}

enum Args<'a> {
    ExpList(Vec<Expression<'a>>),
    Table,
    String,
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
    }
}

pub struct NameList<'a>(pub Vec<Name<'a>>);

pub struct LiteralString<'a>(Cow<'a, BStr>);
pub struct Numeral<'a>(Cow<'a, str>);

pub struct FuncBody<'a> {
    pub args: NameList<'a>,
    pub var_args: bool,
}

pub struct Table<'a> {
    pub field_list: Vec<Field<'a>>,
}

pub struct Field<'a> {
    name: Expression<'a>
}

pub struct Name<'a>(pub Cow<'a, str>);

pub enum FieldSeperator {
    Comma,
    Colon,
}
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
    Not,
}

pub enum UnaryOperator {
    Negate,
    Not,
    Length,
    BitwiseNot,
}
