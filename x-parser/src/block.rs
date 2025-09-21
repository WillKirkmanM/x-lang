use crate::{expression::parse_expr, statement::parse_statement, Rule};
use pest::iterators::Pair;
use x_ast::Statement;

pub fn parse_block(pair: Pair<Rule>) -> Statement {
    let mut statements = Vec::new();
    let mut final_expr = None;

    for inner_pair in pair.into_inner() {
        match inner_pair.as_rule() {
            // A block can contain any number of statements.
            Rule::statement => statements.push(parse_statement(inner_pair)),

            // A block can end with an optional final expression.
            Rule::expr => {
                final_expr = Some(parse_expr(inner_pair));
            }

            // Be tolerant of small grammar ambiguities (e.g. anonymous-fn param tokens
            // leaking into the block parse). Attempt to recover:
            Rule::identifier
            | Rule::primary
            | Rule::term
            | Rule::function_call
            | Rule::string
            | Rule::number => {
                // Try to parse it as an expression and treat as final expression.
                final_expr = Some(parse_expr(inner_pair));
            }
            // Otherwise just skip unknown/irrelevant tokens instead of panicking.
            _ => {
                // ignore unexpected rule to allow parser to be resilient
                // e.g. anonymous-fn parameter artifacts that shouldn't abort parsing
                continue;
            }
        }
    }

    // If there was a final expression, wrap it in a `Statement::Expression`
    // and add it as the last statement. This represents the block's return value.
    if let Some(expr) = final_expr {
        statements.push(Statement::Expression { expr });
    }

    Statement::Block { statements }
}
