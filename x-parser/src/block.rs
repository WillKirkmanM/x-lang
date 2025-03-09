use crate::{expression::parse_expr, statement::parse_statement, Rule};

use pest::iterators::Pair;
use x_ast::Statement;

pub fn parse_block(pair: Pair<Rule>) -> Statement {
    let mut statements = Vec::new();
    let mut final_expr = None;
    
    for pair in pair.into_inner() {
        match pair.as_rule() {
            Rule::statement => statements.push(parse_statement(pair)),
            Rule::block_expr => {
                final_expr = Some(parse_expr(pair.into_inner().next().unwrap()));
            },
            _ => unreachable!("Unexpected rule in block: {:?}", pair.as_rule())
        }
    }
    
    if let Some(expr) = final_expr {
        statements.push(Statement::Expression { expr });
    }
    
    Statement::Block { statements }
}