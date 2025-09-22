use crate::{expression::parse_expr, statement::parse_statement, types::parse_type, Rule};
use pest::iterators::Pair;
use x_ast::{Expr, Statement, Type};

pub fn parse_function_def(pair: Pair<Rule>) -> Statement {
    let mut inner = pair.into_inner();

    let mut is_memoised = false;
    let mut is_pure = false;
    let mut is_multi = false;
    let mut is_throws = false;
    while let Some(p) = inner.peek() {
        if p.as_rule() == Rule::modifiers {
            let m = inner.next().unwrap();
            match m.as_str() {
                "memoised" | "memoized" => is_memoised = true,
                "pure" => is_pure = true,
                "multi" => is_multi = true,
                "throws" => is_throws = true,
                _ => {}
            }
        } else {
            break;
        }
    }

    let name = inner.next().unwrap().as_str().to_string();
    if inner.peek().map(|p| p.as_rule()) == Some(Rule::generic_param_def_list) {
        inner.next();
    }

    let mut params: Vec<(String, Type)> = Vec::new();
    let params_pair = inner
        .next()
        .expect("Function definition is missing parameter list");
    assert_eq!(params_pair.as_rule(), Rule::fn_param_list);
    if let Some(list) = params_pair.into_inner().next() {
        for p in list.into_inner() {
            let param_item = p.into_inner().next().unwrap();
            match param_item.as_rule() {
                Rule::param_normal => {
                    let mut pin = param_item.into_inner().peekable();
                    if pin.peek().map_or(false, |x| x.as_rule() == Rule::kw_mut) {
                        pin.next();
                    }
                    let name = pin.next().unwrap().as_str().to_string();
                    let ty = if let Some(type_rule) = pin.next() {
                        parse_type(type_rule)
                    } else {
                        Type::Int
                    };
                    params.push((name, ty));
                }
                Rule::param_self => {
                    let mut s = param_item.into_inner().peekable();
                    let is_mut = s.peek().map_or(false, |x| x.as_rule() == Rule::kw_mut);
                    let ty = Type::Ref {
                        is_mut,
                        is_unique: false,
                        inner: Box::new(Type::TypeParameter("Self".to_string())),
                    };
                    params.push(("self".to_string(), ty));
                }
                _ => unreachable!(),
            }
        }
    }

    let mut return_type = Type::Void;
    if inner.peek().map(|p| p.as_rule()) == Some(Rule::return_type_ann) {
        return_type = parse_type(inner.next().unwrap());
    }

    let body = if inner.peek().map(|p| p.as_rule()) == Some(Rule::block) {
        let body_pair = inner.next().unwrap();
        let body_stmt = crate::block::parse_block(body_pair);
        match body_stmt {
            Statement::Block { statements } => Some(Box::new(statements)),
            _ => unreachable!(),
        }
    } else {
        None
    };

    Statement::Function {
        name,
        generic_params: None,
        params,
        return_type,
        body,
        is_pure,
        is_memoised,
        is_multi,
        is_throws,
    }
}

pub fn parse_block(pair: Pair<Rule>) -> Vec<Statement> {
    assert_eq!(pair.as_rule(), Rule::block);
    let mut statements = Vec::new();
    let mut inner = pair.into_inner().peekable();

    while let Some(part) = inner.next() {
        if inner.peek().is_none() {
            match part.as_rule() {
                Rule::expr => statements.push(Statement::Return {
                    value: Some(parse_expr(part)),
                }),
                Rule::statement => statements.push(parse_statement(part)),
                _ => unreachable!("Unexpected rule at end of block: {:?}", part.as_rule()),
            }
        } else {
            statements.push(parse_statement(part));
        }
    }
    statements
}

pub fn parse_function_call(pair: Pair<Rule>) -> Expr {
    let mut inner = pair.into_inner();
    let name = inner.next().unwrap().as_str().to_string();

    let args = if let Some(args_pair) = inner.next() {
        args_pair
            .into_inner()
            .map(|arg_pair| parse_expr(arg_pair))
            .collect()
    } else {
        Vec::new()
    };

    Expr::FunctionCall { name, args }
}
