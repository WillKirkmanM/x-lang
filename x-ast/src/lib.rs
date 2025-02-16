#[derive(Debug, Clone, PartialEq)]
pub enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Number(i64),
    String(String),
    Identifier(String),
    BinaryOp {
        left: Box<Expr>,
        op: Operator,
        right: Box<Expr>
    },
    FunctionCall {
        name: String,
        args: Vec<Expr>
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Expression { expr: Expr },
    VariableDecl { name: String, value: Expr },
    Import { module: String, item: String },
    Function { name: String, params: Vec<String>, body: Box<Statement> },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>
}