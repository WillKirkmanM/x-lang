use expression::parse_expr;
use pest::{iterators::Pair, Parser};
use pest_derive::Parser;
use statement::parse_statement;
use x_ast::Program;

pub mod anonymous_function;
pub mod array;
pub mod block;
pub mod expression;
pub mod extern_fn;
pub mod function;
pub mod identifier;
pub mod number;
pub mod postfix_op;
pub mod prefix_op;
pub mod r#return;
pub mod statement;
pub mod string;
pub mod r#struct;
pub mod term;
pub mod r#while;

#[derive(Parser)]
#[grammar = "x.pest"]
struct XParser;

pub fn parse(input: &str) -> Result<Program, String> {
    let pairs = XParser::parse(Rule::program, input).map_err(|e| {
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
            _ => unreachable!(),
        }
    }

    Program { statements }
}
