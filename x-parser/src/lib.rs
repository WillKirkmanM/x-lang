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
            parse_term(inner)
        },
        Rule::function_call => parse_function_call(pair),
        Rule::identifier => Expr::Identifier(pair.as_str().to_string()),
        Rule::number => Expr::Number(pair.as_str().parse().unwrap()),
        Rule::string => {
            let s = pair.as_str();
            let s = &s[1..s.len()-1]; // Remove quotes
            Expr::String(s.to_string())
        },
        Rule::expr => parse_expr(pair),
        _ => unreachable!("Unexpected rule in term: {:?}", pair.as_rule()),
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
    match pair.as_rule() {
        Rule::expr => {
            let mut pairs = pair.into_inner().peekable();
            let mut left = parse_term(pairs.next().unwrap());

            while let Some(op_pair) = pairs.next() {
                let op = parse_operator(op_pair.as_str());
                if let Some(right_pair) = pairs.next() {
                    let right = parse_term(right_pair);
                    left = Expr::BinaryOp {
                        left: Box::new(left),
                        op,
                        right: Box::new(right),
                    };
                }
            }
            left
        },
        _ => parse_term(pair),
    }
}

fn parse_operator(op: &str) -> Operator {
    match op {
        "+" => Operator::Add,
        "-" => Operator::Subtract,
        "*" => Operator::Multiply,
        "/" => Operator::Divide,
        "<" => Operator::LessThan,
        ">" => Operator::GreaterThan,
        "<=" => Operator::LessThanOrEqual,
        ">=" => Operator::GreaterThanOrEqual,
        "==" => Operator::Equal,
        "!=" => Operator::NotEqual,
        _ => unreachable!("Unknown operator: {}", op),
    }
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
    match pair.as_rule() {
        Rule::statement => {
            let inner = pair.into_inner().next().unwrap();
            match inner.as_rule() {
                Rule::if_stmt => {
                    let mut inner_if = inner.into_inner();
                    let condition = Box::new(parse_expr(inner_if.next().unwrap()));
                    let then_block = inner_if.next().unwrap().into_inner()
                        .map(parse_statement)
                        .collect();
                    let else_block = inner_if.next().map(|else_pair| {
                        else_pair.into_inner()
                            .map(parse_statement)
                            .collect()
                    });
                    
                    Statement::If {
                        condition,
                        then_block,
                        else_block
                    }
                },
                Rule::for_loop => {
                    let mut inner_loop = inner.into_inner();
                    let var = inner_loop.next().unwrap().as_str().to_string();
                    let start = Box::new(parse_expr(inner_loop.next().unwrap()));
                    let end = Box::new(parse_expr(inner_loop.next().unwrap()));
                    let body = inner_loop.next().unwrap().into_inner()
                        .map(parse_statement)
                        .collect();
                    
                    Statement::ForLoop { var, start, end, body }
                },
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
                Rule::COMMENT => Statement::Comment(inner.as_str().trim_start_matches("//").trim().to_string()),
                _ => unreachable!("Unexpected rule in statement: {:?}", inner.as_rule())
            }
        },
        Rule::EOI => panic!("EOI should be handled in parse_program"),
        _ => unreachable!("Unexpected top-level rule: {:?}", pair.as_rule())
    }
}

fn parse_function_def(pair: Pair<Rule>) -> Statement {
    let mut inner = pair.into_inner();
    let name = inner.next().unwrap().as_str().to_string();
    
    let params = inner.peek()
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
        body: Box::new(body_statements)
    }
}

fn parse_block(pair: Pair<Rule>) -> Statement {
    let statements: Vec<Statement> = pair.into_inner()
        .map(parse_statement)
        .collect();
    
    if statements.len() == 1 {
        statements[0].clone()
    } else {
        Statement::Block { statements }
    }
}