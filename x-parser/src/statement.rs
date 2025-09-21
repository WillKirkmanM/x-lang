use crate::{
    expression::parse_expr,
    function::{parse_function_call, parse_function_def},
    types::parse_type,
    Rule,
};
use pest::iterators::Pair;
use x_ast::{Expr, ImplDef, Statement, TraitDef, Type};

/// This is the main dispatcher for parsing any statement.
/// It expects a `statement` pair from the grammar, unwraps it,
/// and calls the appropriate helper for the specific statement type inside.
pub fn parse_statement(pair: Pair<Rule>) -> Statement {
    let inner_pair = pair.into_inner().next().unwrap();

    match inner_pair.as_rule() {
        Rule::if_stmt => crate::r#if::parse_if_statement(inner_pair),
        Rule::var_decl => parse_var_decl(inner_pair),
        Rule::import => crate::import::parse_import(inner_pair),

        Rule::function_def => parse_function_def(inner_pair),
        Rule::struct_decl => crate::r#struct::parse_struct_decl(inner_pair),
        Rule::become_stmt => parse_become_statement(inner_pair),
        Rule::return_stmt => parse_return_statement(inner_pair),
        Rule::for_each_loop => crate::r#for::parse_for_each_loop(inner_pair),
        Rule::for_range_loop => crate::r#for::parse_for_range_loop(inner_pair),
        Rule::while_loop => crate::r#while::parse_while_loop(inner_pair),
        Rule::block => crate::block::parse_block(inner_pair),

        Rule::assignment_stmt => parse_assignment_statement(inner_pair),

        Rule::trait_decl => parse_trait_decl(inner_pair),
        Rule::impl_decl => parse_impl_decl(inner_pair),

        Rule::expr => Statement::Expression {
            expr: parse_expr(inner_pair),
        },

        rule => unreachable!("Unexpected rule inside statement: {:?}", rule),
    }
}

pub fn parse_trait_decl(pair: Pair<Rule>) -> Statement {
    let mut inner = pair.into_inner();
    let name = inner.next().unwrap().as_str().to_string();

    let methods = inner
        .map(|item_pair| {
            let mut inner_item = item_pair.into_inner();
            let func_stmt = parse_function_def(inner_item.next().unwrap());
            let name = match &func_stmt {
                Statement::Function { name, .. } => name.clone(),
                _ => unreachable!("Trait item must be a function definition"),
            };
            (name, Some(func_stmt))
        })
        .into_iter()
        .collect();

    Statement::TraitDecl(TraitDef { name, methods })
}

pub fn parse_impl_decl(pair: Pair<Rule>) -> Statement {
    let mut inner = pair.into_inner();
    let trait_name = inner.next().unwrap().as_str().to_string();
    let type_name = parse_type(inner.next().unwrap());

    let methods = inner
        .map(|item_pair| {
            let function_def_pair = item_pair.into_inner().next().unwrap();
            parse_function_def(function_def_pair)
        })
        .collect();

    Statement::ImplDecl(ImplDef {
        trait_name,
        type_name,
        methods,
    })
}

fn parse_var_decl(pair: Pair<Rule>) -> Statement {
    let mut inner = pair.into_inner().peekable();

    if inner.peek().map_or(false, |p| p.as_rule() == Rule::kw_mut) {
        inner.next();
    }

    let name = inner.next().unwrap().as_str().to_string();

    let mut type_ann: Option<Type> = None;
    let mut value: Option<Expr> = None;

    if let Some(next) = inner.peek() {
        match next.as_rule() {
            Rule::r#type => {
                let ty = parse_type(inner.next().unwrap());
                type_ann = Some(ty.clone());
                if inner.peek().is_some() {
                    value = Some(parse_expr(inner.next().unwrap()));
                }
            }
            Rule::expr => {
                value = Some(parse_expr(inner.next().unwrap()));
            }
            _ => {}
        }
    }

    let value = value.unwrap_or_else(|| {
        if let Some(ref t) = type_ann {
            Expr::TypeLiteral(t.clone())
        } else {
            Expr::TypeLiteral(Type::Unknown)
        }
    });

    Statement::VariableDecl {
        name,
        type_ann,
        value,
    }
}

fn parse_become_statement(pair: Pair<Rule>) -> Statement {
    let call_pair = pair.into_inner().next().unwrap();
    Statement::Become {
        call: parse_function_call(call_pair),
    }
}

fn parse_return_statement(pair: Pair<Rule>) -> Statement {
    let value = pair.into_inner().next().map(parse_expr);
    Statement::Return { value }
}

fn parse_assignment_statement(pair: Pair<Rule>) -> Statement {
    let mut inner = pair.into_inner();

    let lval_pair = inner.next().expect("assignment must have l_value");
    let _assign = inner.next();
    let rhs_pair = inner.next().expect("assignment must have rhs expr");

    let target = parse_l_value(lval_pair);
    let value = parse_expr(rhs_pair);

    Statement::Expression {
        expr: Expr::Assignment {
            target: Box::new(target),
            value: Box::new(value),
        },
    }
}

fn parse_l_value(pair: Pair<Rule>) -> Expr {
    assert_eq!(pair.as_rule(), Rule::l_value);
    let mut inner = pair.into_inner();

    let base_primary = inner.next().expect("l_value must start with primary");
    let base = match base_primary.as_rule() {
        Rule::identifier | Rule::expr => base_primary,
        Rule::primary => {
            let mut pinner = base_primary.into_inner();
            pinner
                .next()
                .expect("primary in l_value must contain an inner node")
        }
        other => unreachable!("Invalid lvalue base: {:?}", other),
    };

    let mut expr = match base.as_rule() {
        Rule::identifier => {
            let mut id_inner = base.into_inner();
            let name = id_inner.next().unwrap().as_str().to_string();
            Expr::Identifier(name)
        }
        Rule::expr => parse_expr(base),
        other => unreachable!(
            "l_value base must be identifier or parenthesised expr, got {:?}",
            other
        ),
    };

    for post in inner {
        assert_eq!(post.as_rule(), Rule::postfix);
        let mut pinner = post.into_inner();
        let part = pinner.next().unwrap();
        match part.as_rule() {
            Rule::expr => {
                let index = parse_expr(part);
                expr = Expr::ArrayAccess {
                    array: Box::new(expr),
                    index: Box::new(index),
                };
            }
            Rule::identifier => {
                let mut id_inner = part.into_inner();
                let field = id_inner.next().unwrap().as_str().to_string();
                expr = Expr::FieldAccess {
                    object: Box::new(expr),
                    field,
                };
            }
            other => unreachable!("Invalid lvalue postfix: {:?}", other),
        }
    }

    expr
}
