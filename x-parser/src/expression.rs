use pest::{iterators::Pair, pratt_parser::{Assoc, Op, PrattParser}};
use x_ast::{Expr, Operator};

use crate::{term::parse_term, Rule};

pub fn parse_expr(pair: Pair<Rule>) -> Expr {
    let pratt = PrattParser::new()
        .op(Op::infix(Rule::or_op, Assoc::Left))
        .op(Op::infix(Rule::and_op, Assoc::Left))
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
            Rule::or_op => Operator::Or,
            Rule::and_op => Operator::And,
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