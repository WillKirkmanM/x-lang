use pest::iterators::Pair;
use x_ast::Expr;
use crate::Rule;

pub fn parse_identifier(pair: Pair<Rule>) -> Expr {
    Expr::Identifier(pair.as_str().to_string())
}