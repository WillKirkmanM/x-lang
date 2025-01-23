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
}

