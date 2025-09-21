use crate::Rule;
use pest::iterators::Pair;
use x_ast::Expr;

pub fn parse_boolean(pair: Pair<Rule>) -> Expr {
    let bool_val = pair.as_str().parse::<bool>().unwrap();
    Expr::Boolean(bool_val)
}
