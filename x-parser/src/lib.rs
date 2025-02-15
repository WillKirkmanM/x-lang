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
            match inner.as_rule() {
                Rule::number => {
                    let num: i64 = inner.as_str().parse().unwrap();
                    Expr::Number(num)
                }
                Rule::identifier => {
                    Expr::Identifier(inner.as_str().to_string())
                }
                _ => panic!("Expected number or identifier inside term")
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
    let inner = pair.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::expr => Statement::Expression {
            expr: parse_expr(inner)
        },
        Rule::var_decl => {
            let mut inner_rules = inner.into_inner();
            let name = inner_rules.next().unwrap().as_str().to_string();
            let expr = parse_expr(inner_rules.next().unwrap());
            Statement::VariableDecl { name, value: expr }
        },
        _ => unreachable!()
    }
}