use crate::{block::parse_block, expression::parse_expr, Rule};
use pest::iterators::Pair;
use x_ast::Statement;

pub fn parse_if_statement(pair: Pair<Rule>) -> Statement {
    let mut inner = pair.into_inner();

    let condition = Box::new(parse_expr(inner.next().unwrap()));

    let then_block = match parse_block(inner.next().unwrap()) {
        Statement::Block { statements } => statements,
        _ => unreachable!("A 'then' clause must be a block"),
    };

    let else_block = inner.next().map(|else_pair| match else_pair.as_rule() {
        Rule::block => match parse_block(else_pair) {
            Statement::Block { statements } => statements,
            _ => unreachable!("An 'else' clause must be a block"),
        },
        Rule::if_stmt => vec![parse_if_statement(else_pair)],
        _ => {
            let else_inner = else_pair.into_inner().next();
            match else_inner {
                Some(inner_pair) => {
                    println!("else inner rule: {:?}", inner_pair.as_rule());
                    match inner_pair.as_rule() {
                        Rule::block => match parse_block(inner_pair) {
                            Statement::Block { statements } => statements,
                            _ => unreachable!("An 'else' clause must be a block"),
                        },
                        Rule::if_stmt => vec![parse_if_statement(inner_pair)],
                        _ => panic!(
                            "Expected a block or if_stmt after 'else', got: {:?}",
                            inner_pair.as_rule()
                        ),
                    }
                }
                None => panic!("Expected content after 'else', but found none"),
            }
        }
    });

    Statement::If {
        condition,
        then_block,
        else_block,
    }
}
