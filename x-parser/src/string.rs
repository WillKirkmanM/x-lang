use pest::iterators::Pair;
use x_ast::{Expr, StringLiteral, StringPart};

use crate::Rule;

pub fn parse_string(pair: Pair<Rule>) -> Expr {
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