use x_ast::{Program, Statement, Expr};
use pest::{Parser, iterators::Pair};
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "x.pest"]
struct XParser;

fn parse_term(pair: Pair<Rule>) -> Expr {
    match pair.as_rule() {
        Rule::term => {
            let inner = pair.into_inner().next().unwrap();
            if inner.as_rule() == Rule::number {
                let num: i64 = inner.as_str().parse().unwrap();
                Expr::Number(num)
            } else {
                panic!("Expected number inside term")
            }
        }
        _ => panic!("Expected term rule")
    }
}

fn parse_expr(pair: Pair<Rule>) -> Expr {
    let mut pairs = pair.into_inner().peekable();
    
    let mut expr = parse_term(pairs.next().unwrap());
    
    while let Some(op) = pairs.next() {
        let right = parse_term(pairs.next().unwrap());
        
        match op.as_rule() {
            Rule::add => {
                expr = Expr::Add(Box::new(expr), Box::new(right));
            }
            Rule::multiply => {
                expr = Expr::Multiply(Box::new(expr), Box::new(right));
            }
            _ => panic!("Unexpected operator")
        }
    }
    
    expr
}

pub fn parse(input: &str) -> Result<Program, String> {
    println!("Parsing input: {}", input);
    
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

fn parse_statement(pair: Pair<Rule>) -> Statement {
    let expr = pair.into_inner()
        .find(|p| p.as_rule() == Rule::expr)
        .map(parse_expr)
        .expect("Statement should contain an expression");
        
    Statement { expr }
}