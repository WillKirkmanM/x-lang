use pest::iterators::Pair;
use x_ast::Expr;

use crate::{expression::parse_expr, Rule};

pub fn parse_array_literal(pair: Pair<Rule>) -> Expr {
    let elements = pair.into_inner()
        .map(parse_expr)
        .collect();
    Expr::Array(elements)
}