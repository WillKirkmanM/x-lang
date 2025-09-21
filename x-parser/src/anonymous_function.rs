use pest::iterators::Pair;
use x_ast::{Expr, Statement, Type};

use crate::{block::parse_block, types::parse_type, Rule};

/// Parse an anonymous_fn pair produced by the `anonymous_fn` rule:
/// grammar: "|" (param_list_nonempty)? "|" (return_type_ann)? (block | expr)
pub fn parse_anonymous_fn(pair: Pair<Rule>) -> Expr {
    let mut params: Vec<(String, Option<Type>)> = Vec::new();
    let mut return_type: Option<Type> = None;
    let mut body_stmts: Vec<Statement> = Vec::new();

    for inner in pair.into_inner() {
        match inner.as_rule() {
            Rule::param_list_nonempty => {
                // Each child here is a param_item (param_self | param_normal).
                for param_item in inner.into_inner() {
                    match param_item.as_rule() {
                        Rule::param_normal => {
                            // param_normal = kw_mut? ~ identifier ~ (":" ~ type_specifier)?
                            let mut iter = param_item.into_inner();
                            let first = iter.next();
                            // skip 'mut' if present
                            let (name_pair, type_pair) = if let Some(p) = first {
                                if p.as_rule() == Rule::kw_mut {
                                    // next should be identifier
                                    let name_p = iter.next().unwrap();
                                    (name_p, iter.next())
                                } else {
                                    (p, iter.next())
                                }
                            } else {
                                continue;
                            };
                            let param_name = name_pair.as_str().to_string();
                            let param_type = if let Some(tp) = type_pair {
                                // type_pair is ":" ~ type_specifier in grammar — pick the type child
                                let ty_pair = tp
                                    .into_inner()
                                    .find(|p| p.as_rule() == Rule::r#type)
                                    .unwrap_or_else(|| panic!("expected type"));
                                Some(parse_type(ty_pair))
                            } else {
                                None
                            };
                            params.push((param_name, param_type));
                        }
                        Rule::param_self => {
                            // "&" ~ kw_mut? ~ "self"
                            // represent as a special parameter name "self" with no type
                            params.push(("self".to_string(), None));
                        }
                        _ => {
                            // be tolerant: try to extract identifier + optional type
                            let mut it = param_item.into_inner();
                            if let Some(id) = it.next() {
                                let nm = id.as_str().to_string();
                                let ty = it.next().and_then(|tp| {
                                    tp.into_inner()
                                        .find(|p| p.as_rule() == Rule::r#type)
                                        .map(parse_type)
                                });
                                params.push((nm, ty));
                            }
                        }
                    }
                }
            }
            Rule::return_type_ann => {
                // return_type_ann = "->" ~ type_specifier
                if let Some(type_pair) = inner.into_inner().find(|p| p.as_rule() == Rule::r#type) {
                    return_type = Some(parse_type(type_pair));
                }
            }
            Rule::block => {
                // parse_block returns a Statement::Block — extract its statements
                let block_stmt = parse_block(inner);
                if let Statement::Block { statements } = block_stmt {
                    body_stmts = statements;
                } else {
                    // fallback: wrap whatever the helper returned
                    body_stmts.push(block_stmt);
                }
            }
            Rule::expr => {
                // expression-bodied closure: treat expr as final expression inside body
                let expr = crate::expression::parse_expr(inner);
                body_stmts.push(Statement::Expression { expr });
            }
            _ => {
                // ignore unexpected tokens quietly
            }
        }
    }

    let params = params
        .into_iter()
        .map(|(name, _)| name)
        .collect::<Vec<String>>();
    Expr::AnonymousFunction {
        params,
        return_type,
        body: body_stmts,
    }
}
