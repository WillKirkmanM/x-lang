use pest::iterators::Pair;
use x_ast::Statement;

use crate::{block::parse_block, expression::parse_expr, Rule};

pub fn parse_while_loop(pair: Pair<Rule>) -> Statement {
    let mut inner = pair.into_inner();
    let condition = parse_expr(inner.next().unwrap());
    let body_stmt = parse_block(inner.next().unwrap());
    
    let body = match body_stmt {
        Statement::Block { statements } => statements,
        single_statement => vec![single_statement],
    };
    
    Statement::WhileLoop { condition, body }
}

