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
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Number(i64),
    String(StringLiteral),
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
                Expr::String(str_lit) => str_lit.parts.iter()
                    .map(|part| part.to_string())
                    .collect::<String>(),
                Expr::Identifier(name) => name.clone(),
                _ => format!("{:?}", expr),
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Expression { expr: Expr },
    VariableDecl { name: String, value: Expr },
    Import { module: String, item: String },
    Function { name: String, params: Vec<String>, body: Box<Vec<Statement>> },
    Block { statements: Vec<Statement> },
    Comment(String),
    ForLoop {
        var: String,
        start: Box<Expr>,
        end: Box<Expr>,
        body: Vec<Statement>
    },
    If {
        condition: Box<Expr>,
        then_block: Vec<Statement>,
        else_block: Option<Vec<Statement>>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>
}