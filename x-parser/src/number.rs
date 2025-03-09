use pest::iterators::Pair;
use x_ast::Expr;
use crate::Rule;

pub fn parse_number(pair: Pair<Rule>) -> Expr {
    let num_str = pair.as_str();
    if num_str.contains('.') {
        Expr::Number(num_str.parse::<f64>().unwrap_or_else(|e| {
            panic!("Failed to parse float '{}': {}", num_str, e);
        }))
    } else {
        let int_val: i64 = num_str.parse().unwrap_or_else(|e| {
            panic!("Failed to parse integer '{}': {}", num_str, e);
        });
        Expr::Number(int_val as f64)
    }
}