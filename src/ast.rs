#[derive(Debug, Clone)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    AddAssign, // +=
    SubAssign, // -=
    MulAssign, // *=
    DivAssign, // /=
    Gt,        // Greater than >
    Lt,        // Less than <
    Ge,        // Greater equals >=
    Le,        // Less equals <=
    Eq,        // Equals ==
    Ne,        // Not equals !=
    And,       // Add logical AND operator
    Or,        // Logical OR ||
    BitAnd,    // Bitwise AND &
    BitOr,     // Bitwise OR |
    BitXor,    // Bitwise XOR ^
}

#[derive(Debug)]
pub enum Type {
    Int,
    Float,
    Bool,
    String,
    Array(Box<Type>),
    Void,
}

#[derive(Debug)]
pub enum Expr {
    Let(String, Box<Expr>),
    Int(i32),
    Binary(BinaryOp, Box<Expr>, Box<Expr>),
    Print(Box<Expr>),
    String(String),
    Var(String),
    For(String, Box<Expr>, Box<Expr>, Vec<Expr>),
    Assign(String, Box<Expr>),
    If(
        Box<Expr>,
        Vec<Expr>,
        Vec<(Expr, Vec<Expr>)>,
        Option<Vec<Expr>>,
    ), // condition, then block, else-if blocks with conditions, else block
    Continue,
    Function(String, Vec<String>, Vec<Expr>), // name, params, body
    Call(String, Vec<Box<Expr>>), // name, arguments
    Bool(bool),
    Float(f64),
    Array(Vec<Box<Expr>>),
    Null,
    While(Box<Expr>, Vec<Expr>),
    Break,
    Return(Option<Box<Expr>>),
    Global(String, Box<Expr>),
    Const(String, Box<Expr>),
    Lambda(Vec<(String, Type)>, Box<Type>, Vec<Expr>), // params, return type, body
    FnRef(String), // Function reference
}

