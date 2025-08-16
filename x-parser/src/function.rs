use pest::iterators::Pair;
use x_ast::{Expr, Statement};

use crate::{block::parse_block, expression::parse_expr, Rule};

pub fn parse_function_def(pair: Pair<Rule>) -> Statement {
    let mut inner = pair.into_inner();

    let mut is_memoised = false;
    let mut is_pure = false;

    while let Some(token) = inner.peek() {
        match token.as_rule() {
            Rule::memoised_keyword => {
                is_memoised = true;
                inner.next();
            }
            Rule::pure_keyword => {
                is_pure = true;
                inner.next();
            }
            _ => break,
        }
    }

    let name = inner.next().unwrap().as_str().to_string();

    let params = inner
        .peek()
        .map(|pair| {
            if pair.as_rule() == Rule::params {
                pair.into_inner()
                    .map(|param| param.as_str().to_string())
                    .collect()
            } else {
                Vec::new()
            }
        })
        .unwrap_or_else(Vec::new);

    if !params.is_empty() {
        inner.next();
    }

    let body = parse_block(inner.next().unwrap());

    let body_statements = match body {
        Statement::Block { statements } => statements,
        single_statement => vec![single_statement],
    };

    Statement::Function {
        name,
        params,
        body: Box::new(body_statements),
        is_pure,
        is_memoised,
    }
}

pub fn parse_function_call(pair: Pair<Rule>) -> Expr {
    let mut inner = pair.into_inner();
    let name = inner.next().unwrap().as_str().to_string();

    let args = if let Some(args_pair) = inner.next() {
        args_pair
            .into_inner()
            .map(|arg_pair| parse_expr(arg_pair))
            .collect()
    } else {
        Vec::new()
    };

    Expr::FunctionCall { name, args }
}
