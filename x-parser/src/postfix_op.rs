use pest::iterators::Pair;
use x_ast::{Expr, UnaryOperator};
use crate::{Rule, term::parse_term, expression::parse_expr};

pub fn parse_postfix_op(pair: Pair<Rule>) -> Expr {
    let mut inner = pair.into_inner();
    let term = parse_term(inner.next().unwrap());
    let op_rule = inner.next().unwrap();
    
    match op_rule.as_rule() {
        Rule::post_inc_op => {
            Expr::UnaryOp { 
                op: UnaryOperator::PostIncrement, 
                expr: Box::new(term) 
            }
        },
        Rule::post_dec_op => {
            Expr::UnaryOp { 
                op: UnaryOperator::PostDecrement, 
                expr: Box::new(term) 
            }
        },
        Rule::postfix => {
            parse_postfix(term, op_rule)
        },
        _ => unreachable!("Unknown postfix operator rule: {:?}", op_rule.as_rule()),
    }
}

pub fn parse_postfix(base: Expr, postfix: Pair<Rule>) -> Expr {
    let inner = postfix.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::expr => {
            let index = parse_expr(inner);
            Expr::ArrayAccess {
                array: Box::new(base),
                index: Box::new(index),
            }
        },
        Rule::identifier => {
            let field = inner.as_str().to_string();
            Expr::FieldAccess {
                object: Box::new(base),
                field,
            }
        },
        _ => unreachable!("Unexpected rule in postfix: {:?}", inner.as_rule()),
    }
}