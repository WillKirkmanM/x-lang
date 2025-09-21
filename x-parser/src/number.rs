use crate::Rule;
use pest::iterators::Pair;
use x_ast::Expr;

pub fn parse_number(pair: Pair<Rule>) -> Expr {
    let num_str = pair.as_str();
    if num_str.contains('.') {
        Expr::Float(num_str.parse::<f64>().unwrap_or_else(|e| {
            panic!("Failed to parse float '{}': {}", num_str, e);
        }))
    } else {
        Expr::Int(num_str.parse::<i64>().unwrap_or_else(|e| {
            panic!("Failed to parse integer '{}': {}", num_str, e);
        }))
    }
}
