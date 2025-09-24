use crate::{block::parse_block, expression::parse_expr, Rule};
use pest::iterators::Pair;
use x_ast::Statement;

pub fn parse_for_range_loop(pair: Pair<Rule>) -> Statement {
    let mut inner = pair.into_inner().peekable();

    // Check for the optional 'parallel' keyword.
    let is_parallel = inner.peek().map_or(false, |p| p.as_rule() == Rule::parallel_kw);
    if is_parallel {
        inner.next(); // Consume the keyword if it's present.
    }

    let var = inner.next().unwrap().as_str().to_string();
    let start = Box::new(parse_expr(inner.next().unwrap()));
    let end = Box::new(parse_expr(inner.next().unwrap()));
    let body = match parse_block(inner.next().unwrap()) {
        Statement::Block { statements } => statements,
        _ => unreachable!(),
    };

    Statement::ForRangeLoop {
        var,
        start,
        end,
        body,
        is_parallel,
    }
}

pub fn parse_for_each_loop(pair: Pair<Rule>) -> Statement {
    let mut inner = pair.into_inner().peekable();

    // Check for the optional 'parallel' keyword.
    let is_parallel = inner.peek().map_or(false, |p| p.as_rule() == Rule::parallel_kw);
    if is_parallel {
        inner.next(); // Consume the keyword if it's present.
    }

    let var = inner.next().unwrap().as_str().to_string();
    let iterator = Box::new(parse_expr(inner.next().unwrap()));
    let body = match parse_block(inner.next().unwrap()) {
        Statement::Block { statements } => statements,
        _ => unreachable!(),
    };
    
    Statement::ForEachLoop {
        var,
        iterator,
        body,
        is_parallel, // Set the flag in the AST.
    }
}