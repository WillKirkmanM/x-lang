use pest::iterators::Pair;
use x_ast::{Expr, Statement};

use crate::{block::parse_block, Rule};

pub fn parse_anonymous_fn(pair: Pair<Rule>) -> Expr {
    let mut inner = pair.into_inner();
    
    let params = if let Some(params_pair) = inner.next().filter(|p| p.as_rule() == Rule::params) {
        params_pair.into_inner()
            .map(|param| param.as_str().to_string())
            .collect()
    } else {
        Vec::new()
    };
    
    let body_pair = inner.next().unwrap();
    let body = match parse_block(body_pair) {
        Statement::Block { statements } => statements,
        single_statement => vec![single_statement],
    };
    
    Expr::AnonymousFunction { params, body }
}