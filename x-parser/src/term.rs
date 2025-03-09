use pest::iterators::Pair;
use x_ast::Expr;

use crate::{
    anonymous_function::parse_anonymous_fn,
    array::parse_array_literal,
    expression::parse_expr,
    function::parse_function_call,
    string::parse_string,
    r#struct::parse_struct_instantiate,
    Rule,
    number::parse_number,
    prefix_op::parse_prefix_op,
    postfix_op::{parse_postfix_op, parse_postfix},
    identifier::parse_identifier,
};

pub fn parse_term(pair: Pair<Rule>) -> Expr {
    let mut base = match pair.as_rule() {
        Rule::term => {
            let mut expr = None;
            let mut pairs = pair.clone().into_inner().peekable();
            
            if let Some(primary_pair) = pairs.next() {
                expr = Some(parse_term(primary_pair));
                
                while let Some(postfix) = pairs.next() {
                    if postfix.as_rule() == Rule::postfix {
                        let inner_expr = expr.take().unwrap();
                        expr = Some(parse_postfix(inner_expr, postfix));
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
        Rule::identifier => parse_identifier(pair.clone()),
        Rule::number => parse_number(pair.clone()),
        Rule::string => parse_string(pair.clone()),
        Rule::expr => parse_expr(pair.clone()),
        Rule::anonymous_fn => parse_anonymous_fn(pair.clone()),
        Rule::array_literal => parse_array_literal(pair.clone()),
        Rule::struct_instantiate => parse_struct_instantiate(pair.clone()),
        Rule::prefix_op => parse_prefix_op(pair.clone()),
        Rule::postfix_op => parse_postfix_op(pair.clone()),
        _ => unreachable!("Unexpected rule in term: {:?}", pair.as_rule()),
    };
    
    if matches!(pair.as_rule(), Rule::term | Rule::primary) {
        for postfix in pair.into_inner().skip(1) {
            match postfix.as_rule() {
                Rule::postfix => {
                    base = parse_postfix(base, postfix);
                }
                _ => unreachable!("Unexpected rule following primary: {:?}", postfix.as_rule()),
            }
        }
    }
    
    base
}