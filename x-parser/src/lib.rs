use x_ast::{Expr, Operator, Program, Statement, StringLiteral, StringPart, StructDef, StructInit};
use pest::{Parser, iterators::Pair};
use pest_derive::Parser;
use pest::pratt_parser::{PrattParser, Assoc, Op};

#[derive(Parser)]
#[grammar = "x.pest"]
struct XParser;

fn parse_term(pair: Pair<Rule>) -> Expr {
    let mut base = match pair.as_rule() {
        Rule::term => {
            let mut expr = None;
            let mut pairs = pair.clone().into_inner().peekable();
            
            if let Some(primary_pair) = pairs.next() {
                expr = Some(parse_term(primary_pair));
                
                while let Some(postfix) = pairs.next() {
                    if postfix.as_rule() == Rule::postfix {
                        let inner_expr = expr.take().unwrap();
                        let inner = postfix.into_inner().next().unwrap();
                        match inner.as_rule() {
                            Rule::expr => {
                                let index_expr = parse_expr(inner);
                                expr = Some(Expr::ArrayAccess {
                                    array: Box::new(inner_expr),
                                    index: Box::new(index_expr)
                                });
                            },
                            Rule::identifier => {
                                let field = inner.as_str().to_string();
                                expr = Some(Expr::FieldAccess {
                                    object: Box::new(inner_expr),
                                    field,
                                });
                            },
                            _ => unreachable!("Unexpected rule in postfix: {:?}", inner.as_rule()),
                        }
                    }
                }
            }
            
            expr.unwrap_or_else(|| unreachable!("Term should have at least a primary expression"))
        },
        Rule::primary => {
            let inner = pair.clone().into_inner().next().unwrap();
            parse_term(inner)
        },
        Rule::function_call => parse_function_call(pair.clone()),
        Rule::identifier => Expr::Identifier(pair.as_str().to_string()),
        Rule::number => {
            let num_str = pair.as_str();
            if num_str.contains('.') {
                Expr::Number(num_str.parse::<f64>().unwrap_or_else(|e| {
                    panic!("Failed to parse float '{}': {}", num_str, e);
                }))
            } else {
                let int_val: i64 = num_str.parse().unwrap_or_else(|e| {
                    panic!("Failed to parse integer '{}': {}", num_str, e);
                });
                Expr::Number(int_val as f64)
            }
        },
        Rule::string => parse_string(pair.clone()),
        Rule::expr => parse_expr(pair.clone()),
        Rule::anonymous_fn => parse_anonymous_fn(pair.clone()),
        Rule::array_literal => parse_array_literal(pair.clone()),
        Rule::struct_instantiate => parse_struct_instantiate(pair.clone()),
        _ => unreachable!("Unexpected rule in term: {:?}", pair.as_rule()),
    };
    
    if matches!(pair.as_rule(), Rule::term | Rule::primary) {
        for postfix in pair.into_inner().skip(1) {
            match postfix.as_rule() {
                Rule::postfix => {
                    let inner = postfix.into_inner().next().unwrap();
                    match inner.as_rule() {
                        Rule::expr => {
                            // Array access
                            let index = parse_expr(inner);
                            base = Expr::ArrayAccess {
                                array: Box::new(base),
                                index: Box::new(index),
                            };
                        }
                        Rule::identifier => {
                            // Field access
                            let field = inner.as_str().to_string();
                            base = Expr::FieldAccess {
                                object: Box::new(base),
                                field,
                            };
                        }
                        _ => unreachable!("Unexpected rule in postfix: {:?}", inner.as_rule()),
                    }
                }
                _ => unreachable!("Unexpected rule following primary: {:?}", postfix.as_rule()),
            }
        }
    }
    
    base
}

fn parse_anonymous_fn(pair: Pair<Rule>) -> Expr {
    let mut inner = pair.into_inner();
    
    let params = if let Some(params_pair) = inner.next().filter(|p| p.as_rule() == Rule::params) {
        params_pair.into_inner()
            .map(|param| param.as_str().to_string())
            .collect()
    } else {
        Vec::new()
    };
    
    let body_pair = inner.next().unwrap();
    let body = match parse_block(body_pair) {
        Statement::Block { statements } => statements,
        single_statement => vec![single_statement],
    };
    
    Expr::AnonymousFunction { params, body }
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
    let pratt = PrattParser::new()
        .op(Op::infix(Rule::eq_op, Assoc::Left) | Op::infix(Rule::neq_op, Assoc::Left))
        .op(Op::infix(Rule::lt_op, Assoc::Left) | 
            Op::infix(Rule::gt_op, Assoc::Left) | 
            Op::infix(Rule::le_op, Assoc::Left) | 
            Op::infix(Rule::ge_op, Assoc::Left))
        .op(Op::infix(Rule::add_op, Assoc::Left) | Op::infix(Rule::sub_op, Assoc::Left))
        .op(Op::infix(Rule::mul_op, Assoc::Left) | Op::infix(Rule::div_op, Assoc::Left))
        .op(Op::infix(Rule::assign_op, Assoc::Right));

    pratt.map_primary(|pair| {
        match pair.as_rule() {
            Rule::term => parse_term(pair),
            _ => unreachable!("Unexpected rule in primary: {:?}", pair.as_rule()),
        }
    })
    .map_infix(|lhs, op, rhs| {
        let operator = match op.as_rule() {
            Rule::add_op => Operator::Add,
            Rule::sub_op => Operator::Subtract,
            Rule::mul_op => Operator::Multiply,
            Rule::div_op => Operator::Divide,
            Rule::lt_op => Operator::LessThan,
            Rule::gt_op => Operator::GreaterThan,
            Rule::le_op => Operator::LessThanOrEqual,
            Rule::ge_op => Operator::GreaterThanOrEqual,
            Rule::eq_op => Operator::Equal,
            Rule::neq_op => Operator::NotEqual,
            Rule::assign_op => Operator::Assign,
            _ => unreachable!("Unknown operator rule: {:?}", op.as_rule()),
        };
        
        Expr::BinaryOp {
            left: Box::new(lhs),
            op: operator,
            right: Box::new(rhs),
        }
    })
    .parse(pair.into_inner())
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
                Rule::while_loop => parse_while_loop(inner),
                Rule::block_expr => Statement::Expression {
                    expr: parse_expr(inner)
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
                Rule::struct_decl => parse_struct_decl(inner),
                Rule::COMMENT => Statement::Comment(inner.as_str().trim_start_matches("//").trim().to_string()),
                _ => unreachable!("Unexpected rule in statement: {:?}", inner.as_rule())
            }
        },
        Rule::block_expr => Statement::Expression {
            expr: parse_expr(pair.into_inner().next().unwrap())
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
    let mut statements = Vec::new();
    let mut final_expr = None;
    
    for pair in pair.into_inner() {
        match pair.as_rule() {
            Rule::statement => statements.push(parse_statement(pair)),
            Rule::block_expr => {
                final_expr = Some(parse_expr(pair.into_inner().next().unwrap()));
            },
            _ => unreachable!("Unexpected rule in block: {:?}", pair.as_rule())
        }
    }
    
    if let Some(expr) = final_expr {
        statements.push(Statement::Expression { expr });
    }
    
    Statement::Block { statements }
}

fn parse_string(pair: Pair<Rule>) -> Expr {
    let text = pair.as_str();
    let content = &text[1..text.len()-1];
    let mut parts = Vec::new();
    let mut current_text = String::new();
    let mut chars = content.chars().peekable();
    
    while let Some(c) = chars.next() {
        if c == '{' {
            if !current_text.is_empty() {
                parts.push(StringPart::Text(current_text));
                current_text = String::new();
            }
            
            let mut var_name = String::new();
            while let Some(c) = chars.next() {
                if c == '}' {
                    break;
                }
                var_name.push(c);
            }
            
            parts.push(StringPart::Interpolation(Box::new(
                Expr::Identifier(var_name)
            )));
        } else {
            current_text.push(c);
        }
    }
    
    if !current_text.is_empty() {
        parts.push(StringPart::Text(current_text));
    }
    
    Expr::String(StringLiteral { parts })
}

fn parse_array_literal(pair: Pair<Rule>) -> Expr {
    let elements = pair.into_inner()
        .map(parse_expr)
        .collect();
    Expr::Array(elements)
}

fn parse_while_loop(pair: Pair<Rule>) -> Statement {
    let mut inner = pair.into_inner();
    let condition = parse_expr(inner.next().unwrap());
    let body_stmt = parse_block(inner.next().unwrap());
    
    let body = match body_stmt {
        Statement::Block { statements } => statements,
        single_statement => vec![single_statement],
    };
    
    Statement::WhileLoop { condition, body }
}

fn parse_struct_decl(pair: Pair<Rule>) -> Statement {
    let mut inner = pair.into_inner();
    let name = inner.next().unwrap().as_str().to_string();
    
    let fields = if let Some(fields_pair) = inner.next() {
        fields_pair.into_inner()
            .map(|field| field.as_str().to_string())
            .collect()
    } else {
        Vec::new()
    };
    
    Statement::StructDecl(StructDef { name, fields })
}

fn parse_struct_instantiate(pair: Pair<Rule>) -> Expr {
    let mut inner = pair.into_inner();
    let name = inner.next().unwrap().as_str().to_string();
    
    let fields = if let Some(fields_pair) = inner.next() {
        fields_pair.into_inner()
            .map(|field_pair| {
                let mut field_inner = field_pair.into_inner();
                let field_name = field_inner.next().unwrap().as_str().to_string();
                let value = parse_expr(field_inner.next().unwrap());
                (field_name, value)
            })
            .collect()
    } else {
        Vec::new()
    };
    
    Expr::StructInstantiate(StructInit { name, fields })
}
