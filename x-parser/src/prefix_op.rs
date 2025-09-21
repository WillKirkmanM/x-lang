use crate::{term::parse_term, Rule};
use pest::iterators::Pair;
use x_ast::{Expr, UnaryOperator};

pub fn parse_prefix_op(pair: Pair<Rule>) -> Expr {
    let mut inner = pair.into_inner();
    let first = inner.next().expect("prefix_op missing inner");

    match first.as_rule() {
        // & [mut]? term
        Rule::addr_of => {
            let mut ainner = first.into_inner().peekable();
            let is_mut = ainner.peek().map_or(false, |p| p.as_rule() == Rule::kw_mut);
            if is_mut {
                ainner.next();
            }
            let target = parse_term(ainner.next().expect("& must be followed by a term"));
            Expr::AddressOf {
                is_mut,
                expr: Box::new(target),
            }
        }

        // * term
        Rule::deref => {
            let mut dinner = first.into_inner();
            let target = parse_term(dinner.next().expect("* must be followed by a term"));
            Expr::Deref {
                expr: Box::new(target),
            }
        }

        Rule::neg_op => {
            let expr_pair = inner.next().expect("neg_op must be followed by a term");
            Expr::UnaryOp {
                op: UnaryOperator::Negate,
                expr: Box::new(parse_term(expr_pair)),
            }
        }
        Rule::not_op => {
            let expr_pair = inner.next().expect("not_op must be followed by a term");
            Expr::UnaryOp {
                op: UnaryOperator::LogicalNot,
                expr: Box::new(parse_term(expr_pair)),
            }
        }
        Rule::bit_not_op => {
            let expr_pair = inner.next().expect("bit_not_op must be followed by a term");
            Expr::UnaryOp {
                op: UnaryOperator::BitwiseNot,
                expr: Box::new(parse_term(expr_pair)),
            }
        }
        Rule::pre_inc_op => {
            let expr_pair = inner.next().expect("pre_inc_op must be followed by a term");
            Expr::UnaryOp {
                op: UnaryOperator::PreIncrement,
                expr: Box::new(parse_term(expr_pair)),
            }
        }
        Rule::pre_dec_op => {
            let expr_pair = inner.next().expect("pre_dec_op must be followed by a term");
            Expr::UnaryOp {
                op: UnaryOperator::PreDecrement,
                expr: Box::new(parse_term(expr_pair)),
            }
        }

        other => unreachable!("Unknown prefix operator rule: {:?}", other),
    }
}
