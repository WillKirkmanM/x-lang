use pest::iterators::Pair;
use x_ast::{Expr, UnaryOperator};
use crate::{Rule, term::parse_term};

pub fn parse_prefix_op(pair: Pair<Rule>) -> Expr {
    let mut inner = pair.into_inner();
    let op_rule = inner.next().unwrap();
    let expr_pair = inner.next().unwrap();
    
    match op_rule.as_rule() {
        Rule::neg_op => {
            Expr::UnaryOp { 
                op: UnaryOperator::Negate, 
                expr: Box::new(parse_term(expr_pair)) 
            }
        },
        Rule::not_op => {
            Expr::UnaryOp { 
                op: UnaryOperator::LogicalNot, 
                expr: Box::new(parse_term(expr_pair)) 
            }
        },
        Rule::bit_not_op => {
            Expr::UnaryOp { 
                op: UnaryOperator::BitwiseNot, 
                expr: Box::new(parse_term(expr_pair)) 
            }
        },
        Rule::pre_inc_op => {
            Expr::UnaryOp { 
                op: UnaryOperator::PreIncrement, 
                expr: Box::new(parse_term(expr_pair)) 
            }
        },
        Rule::pre_dec_op => {
            Expr::UnaryOp { 
                op: UnaryOperator::PreDecrement, 
                expr: Box::new(parse_term(expr_pair)) 
            }
        },
        _ => unreachable!("Unknown prefix operator rule: {:?}", op_rule.as_rule()),
    }
}