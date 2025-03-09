use pest::iterators::Pair;
use x_ast::Statement;

use crate::{expression::parse_expr, function::parse_function_def, r#return::parse_return_statement, r#struct::parse_struct_decl, r#while::parse_while_loop, Rule};

pub fn parse_statement(pair: Pair<Rule>) -> Statement {
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
                Rule::return_stmt => parse_return_statement(inner),
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

