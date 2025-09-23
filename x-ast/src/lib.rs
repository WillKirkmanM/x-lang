use std::{collections::HashMap, hash::Hash};

#[derive(Debug, Clone, PartialEq)]
pub enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    ShiftLeft,
    ShiftRight,
    BitAnd,
    Xor,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
    Equal,
    NotEqual,
    Or,
    And,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Int(i64),
    Float(f64),
    String(StringLiteral),
    Boolean(bool),
    Identifier(String),
    TypeLiteral(Type),
    BinaryOp {
        left: Box<Expr>,
        op: Operator,
        right: Box<Expr>,
    },
    FunctionCall {
        name: String,
        args: Vec<Expr>,
    },
    AnonymousFunction {
        params: Vec<String>,
        body: Vec<Statement>,
        return_type: Option<Type>,
    },
    Array(Vec<Expr>),
    ArrayAccess {
        array: Box<Expr>,
        index: Box<Expr>,
    },
    Assignment {
        target: Box<Expr>,
        value: Box<Expr>,
    },
    StructInstantiate(StructInit),
    FieldAccess {
        object: Box<Expr>,
        field: String,
    },
    UnaryOp {
        op: UnaryOperator,
        expr: Box<Expr>,
    },
    AddressOf {
        is_mut: bool,
        expr: Box<Expr>,
    },
    Deref {
        expr: Box<Expr>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct StringLiteral {
    pub parts: Vec<StringPart>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StringPart {
    Text(String),
    Interpolation(Box<Expr>),
}

impl ToString for StringPart {
    fn to_string(&self) -> String {
        match self {
            StringPart::Text(text) => text.clone(),
            StringPart::Interpolation(expr) => match expr.as_ref() {
                Expr::String(str_lit) => str_lit
                    .parts
                    .iter()
                    .map(|part| part.to_string())
                    .collect::<String>(),
                Expr::Identifier(name) => name.clone(),
                _ => format!("{:?}", expr),
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Expression {
        expr: Expr,
    },
    VariableDecl {
        name: String,
        type_ann: Option<Type>,
        value: Expr,
    },
    Import {
        module: String,
        item: String,
    },
    FileImport {
        path: String,
    },
    Function {
        name: String,
        generic_params: Option<Vec<String>>,
        params: Vec<Param>,
        return_type: Type,
        body: Option<Box<Vec<Statement>>>,
        is_pure: bool,
        is_memoised: bool,
        is_multi: bool,
        is_throws: bool,
    },
    ExternFunctionDecl {
        name: String,
        params: Vec<ExternParam>,
        return_type: Option<String>,
    },
    Block {
        statements: Vec<Statement>,
    },
    Become {
        call: Expr,
    },
    Comment(String),
    ForRangeLoop {
        var: String,
        start: Box<Expr>,
        end: Box<Expr>,
        body: Vec<Statement>,
    },
    If {
        condition: Box<Expr>,
        then_block: Vec<Statement>,
        else_block: Option<Vec<Statement>>,
    },
    WhileLoop {
        condition: Expr,
        body: Vec<Statement>,
    },
    ForEachLoop {
        var: String,
        iterator: Box<Expr>,
        body: Vec<Statement>,
    },

    StructDecl(StructDef),
    TraitDecl(TraitDef),
    ImplDecl(ImplDef),

    Return {
        value: Option<Expr>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExternParam {
    pub name: String,
    pub type_name: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Copy, Default)]
pub enum Layout {
    #[default]
    AoS, // Array of Structs
    SoA, // Struct of Arrays
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Param {
    pub name: String,
    pub ty: Type,
    pub is_static: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDef {
    pub name: String,
    pub generic_params: Option<Vec<String>>,
    pub fields: Vec<(String, Type)>,
    pub invariant: Option<Box<Expr>>,
    pub layout: Layout,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructInit {
    pub name: String,
    pub fields: Vec<(String, Expr)>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperator {
    Negate,
    LogicalNot,
    BitwiseNot,
    PreIncrement,
    PreDecrement,
    PostIncrement,
    PostDecrement,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Int,
    Float,
    String,
    Bool,
    Void,
    Custom(String),
    Unknown,
    Array(Box<Type>),
    TypeParameter(String),
    GenericInstance {
        name: String,
        type_args: Vec<Type>,
    },
    Ref {
        is_mut: bool,
        is_unique: bool,
        inner: Box<Type>,
    },
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Int => write!(f, "i32"),
            Type::Float => write!(f, "f64"),
            Type::Bool => write!(f, "bool"),
            Type::String => write!(f, "str"),
            Type::Void => write!(f, "void"),
            Type::Custom(s) => write!(f, "{}", s),
            Type::Unknown => write!(f, "unknown"),
            Type::Array(inner) => write!(f, "{}[]", inner),
            Type::TypeParameter(name) => write!(f, "{}", name),
            Type::GenericInstance { name, type_args } => {
                let args = type_args
                    .iter()
                    .map(|t| format!("{}", t))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "{}<{}>", name, args)
            }
            Type::Ref {
                is_mut,
                is_unique,
                inner,
            } => {
                write!(
                    f,
                    "&{}{}{}",
                    if *is_unique { "unique " } else { "" },
                    if *is_mut { "mut " } else { "" },
                    inner
                )
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TraitDef {
    pub name: String,
    pub methods: HashMap<String, Option<Statement>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImplDef {
    pub trait_name: String,
    pub type_name: Type,
    pub methods: Vec<Statement>,
}
