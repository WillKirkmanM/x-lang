#[derive(Debug, Clone, PartialEq)]
pub enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
    Equal,
    NotEqual,
    Assign,
    Or,
    And,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Number(f64),
    String(StringLiteral),
    Identifier(String),
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
        params: Vec<String>,
        body: Box<Vec<Statement>>,
    },
    ExternFunctionDecl {
        name: String,
        params: Vec<ExternParam>,
        return_type: Option<String>,
    },
    Block {
        statements: Vec<Statement>,
    },
    Comment(String),
    ForLoop {
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
    StructDecl(StructDef),
    Return {
        value: Option<Expr>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExternParam {
    pub name: String,
    pub type_name: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDef {
    pub name: String,
    pub fields: Vec<String>,
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
