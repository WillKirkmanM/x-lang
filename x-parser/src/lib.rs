use x_ast::{Expr, Operator, Program, Statement};
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
                Rule::string => {
                    let s = inner.as_str();
                    let s = &s[1..s.len()-1];
                    Expr::String(s.to_string())
                }
                Rule::identifier => {
                    Expr::Identifier(inner.as_str().to_string())
                }
                Rule::function_call => parse_function_call(inner),
                _ => panic!("Expected number, string, identifier, or function call inside term")
            }
        }
        _ => panic!("Expected term rule")
    }
}

fn parse_function_call(pair: Pair<Rule>) -> Expr {
    let mut inner = pair.into_inner();
    let name = inner.next().unwrap().as_str().to_string();
    
    let args = if let Some(args_pair) = inner.next() {
        args_pair.into_inner()
            .map(|arg| parse_expr(arg))
            .collect()
    } else {
        Vec::new()
    };
    
    Expr::FunctionCall { name, args }
}

fn parse_expr(pair: Pair<Rule>) -> Expr {
    let mut pairs = pair.into_inner().peekable();
    
    let mut expr = parse_term(pairs.next().unwrap());
    
    while let Some(op) = pairs.next() {
        let right = parse_term(pairs.next().unwrap());
        
        let operator = match op.as_rule() {
            Rule::add => Operator::Add,
            Rule::multiply => Operator::Multiply,
            Rule::subtract => Operator::Subtract,
            Rule::divide => Operator::Divide,
            _ => panic!("Unexpected operator")
        };
        
        expr = Expr::BinaryOp {
            left: Box::new(expr),
            op: operator,
            right: Box::new(right)
        };
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
        Rule::import => {
            let mut inner_rules = inner.into_inner();
            let module = inner_rules.next().unwrap().as_str().to_string();
            let item = inner_rules.next().unwrap().as_str().to_string();
            Statement::Import { module, item }
        },
        Rule::function_def => parse_function_def(inner),
        _ => unreachable!()
    }
}

fn parse_function_def(pair: Pair<Rule>) -> Statement {
    let mut inner = pair.into_inner();
    let name = inner.next().unwrap().as_str().to_string();
    
    let params = if let Some(params_pair) = inner.next() {
        params_pair.into_inner()
            .map(|param| param.as_str().to_string())
            .collect()
    } else {
        Vec::new()
    };
    
    let body = parse_block(inner.next().unwrap());
    
    Statement::Function {
        name,
        params,
        body: Box::new(body)
    }
}

fn parse_block(pair: Pair<Rule>) -> Statement {
    let statements: Vec<Statement> = pair.into_inner()
        .map(parse_statement)
        .collect();
    
    if let Some(last_stmt) = statements.last() {
        last_stmt.clone()
    } else {
        Statement::Expression { 
            expr: Expr::Number(0) 
        }
    }
}