use expression::parse_expr;
use statement::parse_statement;
use x_ast::Program;
use pest::{Parser, iterators::Pair};
use pest_derive::Parser;

pub mod anonymous_function;
pub mod statement;
pub mod r#struct;
pub mod expression;
pub mod function;
pub mod block;
pub mod array;
pub mod r#return;
pub mod string;
pub mod r#while;
pub mod term;
pub mod number;
pub mod identifier;
pub mod postfix_op;
pub mod prefix_op;

#[derive(Parser)]
#[grammar = "x.pest"]
struct XParser;

pub fn parse(input: &str) -> Result<Program, String> {
    let pairs = XParser::parse(Rule::program, input)
        .map_err(|e| {
            println!("Parsing error: {}", e);
            e.to_string()
        })?;

    let program_pair = pairs.peek().ok_or("Empty input")?;
    Ok(parse_program(program_pair))
}

fn parse_program(pair: Pair<Rule>) -> Program {
    let mut statements = Vec::new();
    
    for pair in pair.into_inner() {
        match pair.as_rule() {
            Rule::statement => statements.push(parse_statement(pair)),
            Rule::EOI => break,
            _ => unreachable!()
        }
    }
    
    Program { statements }
}