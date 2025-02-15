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
    Add(Box<Expr>, Box<Expr>),
    Multiply(Box<Expr>, Box<Expr>),
    BinaryOp {
        left: Box<Expr>,
        op: Operator,
        right: Box<Expr>,
    },
    Identifier(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Expression { expr: Expr },
    VariableDecl { name: String, value: Expr },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>
}