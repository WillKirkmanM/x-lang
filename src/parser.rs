use crate::ast::{BinaryOp, Expr};
use std::iter::Peekable;

const COMPOUND_OPS: &[&str; 4] = &["+=", "-=", "*=", "/="];
const BASIC_OPS: &[&str; 5] = &["+", "-", "*", "/", "%"];
const COMP_OPS: &[&str; 6] = &[">", "<", ">=", "<=", "==", "!="];

fn parse_value(value_str: &str) -> Box<Expr> {
    if value_str.starts_with('"') && value_str.ends_with('"') {
        Box::new(Expr::String(value_str[1..value_str.len() - 1].to_string()))
    } else if let Ok(num) = value_str.parse::<i32>() {
        Box::new(Expr::Int(num))
    } else {
        Box::new(Expr::Var(value_str.to_string()))
    }
}
fn parse_binary_operation(line: &str) -> Option<Expr> {
    let line = line.trim();
    if line.starts_with('(') && line.ends_with(')') {
        return parse_binary_operation(&line[1..line.len() - 1]);
    }
    if line.contains("&&") {
        let parts: Vec<&str> = line.split("&&").collect();
        if parts.len() == 2 {
            let left = parts[0].trim();
            let right = parts[1].trim();
            let left_expr = parse_binary_operation(left).unwrap_or_else(|| *parse_value(left));
            let right_expr = parse_binary_operation(right).unwrap_or_else(|| *parse_value(right));
            return Some(Expr::Binary(
                BinaryOp::And,
                Box::new(left_expr),
                Box::new(right_expr),
            ));
        }
    }
    for &op in COMP_OPS {
        if line.contains(op) {
            let parts: Vec<&str> = line.split(op).collect();
            if parts.len() == 2 {
                let left = parts[0].trim();
                let right = parts[1].trim();
                let left_expr = parse_binary_operation(left).unwrap_or_else(|| *parse_value(left));
                let right_expr =
                    parse_binary_operation(right).unwrap_or_else(|| *parse_value(right));
                let binary_op = match op {
                    "==" => BinaryOp::Eq,
                    "!=" => BinaryOp::Ne,
                    ">" => BinaryOp::Gt,
                    "<" => BinaryOp::Lt,
                    ">=" => BinaryOp::Ge,
                    "<=" => BinaryOp::Le,
                    _ => return None,
                };
                return Some(Expr::Binary(
                    binary_op,
                    Box::new(left_expr),
                    Box::new(right_expr),
                ));
            }
        }
    }
    for &op in BASIC_OPS {
        if line.contains(op) {
            let parts: Vec<&str> = line.split(op).collect();
            if parts.len() == 2 {
                let left = parts[0].trim();
                let right = parts[1].trim().trim_end_matches(';');
                let binary_op = match op {
                    "+" => BinaryOp::Add,
                    "-" => BinaryOp::Sub,
                    "*" => BinaryOp::Mul,
                    "/" => BinaryOp::Div,
                    "%" => BinaryOp::Mod,
                    _ => return None,
                };
                return Some(Expr::Binary(
                    binary_op,
                    parse_value(left),
                    parse_value(right),
                ));
            }
        }
    }
    if !line.is_empty() {
        return Some(*parse_value(line));
    }
    None
}
fn parse_for_loop(line: &str, content_lines: &mut Peekable<std::str::Lines>) -> Option<Expr> {
    if let Some(iter_range) = line.strip_prefix("for ").and_then(|s| s.split_once(" in ")) {
        let iter = iter_range.0.trim();
        let range_parts: Vec<&str> = iter_range.1.split("..").collect();
        if range_parts.len() == 2 {
            let start = range_parts[0].trim();
            let end = range_parts[1].trim().trim_end_matches(" {");
            let body = parse_block(content_lines);
            return Some(Expr::For(
                iter.to_string(),
                Box::new(Expr::Int(start.parse().unwrap_or(0))),
                Box::new(Expr::Int(end.parse().unwrap_or(0))),
                body,
            ));
        }
    }
    None
}
fn parse_if_statement(
    line: &str,
    content_lines: &mut std::iter::Peekable<std::str::Lines>,
) -> Option<Expr> {
    if let Some(condition) = line.strip_prefix("if ").map(|s| s.trim_end_matches(" {")) {
        let condition_expr = parse_binary_operation(condition)?;
        let then_block = parse_block(content_lines);
        Some(Expr::If(Box::new(condition_expr), then_block, vec![], None))
    } else {
        None
    }
}
fn parse_block(content_lines: &mut std::iter::Peekable<std::str::Lines>) -> Vec<Expr> {
    let mut body = Vec::new();
    let mut brace_count = 1;
    while let Some(line) = content_lines.next() {
        let trimmed = line.trim();
        if trimmed.is_empty() || trimmed.starts_with("//") {
            continue;
        }
        if trimmed == "}" {
            brace_count -= 1;
            if brace_count == 0 {
                break;
            }
            continue;
        }
        if let Some(expr) = parse_line(trimmed) {
            body.push(expr);
        }
    }
    body
}
pub fn parse_line(line: &str) -> Option<Expr> {
    let line = line.trim();
    if let Some(remainder) = line.strip_prefix("let ") {
        let parts: Vec<&str> = remainder.splitn(2, '=').collect();
        if parts.len() == 2 {
            let var_name = parts[0].trim().to_string();
            let value_str = parts[1].trim().trim_end_matches(';');
            if let Some(expr) = parse_binary_operation(value_str) {
                return Some(Expr::Let(var_name, Box::new(expr)));
            } else {
                return Some(Expr::Let(var_name, parse_value(value_str)));
            }
        }
    }
    if line.is_empty() || line.starts_with("//") {
        return None;
    }
    let line = if let Some(comment_idx) = line.find("//") {
        line[..comment_idx].trim()
    } else {
        line
    };
    if line.is_empty() {
        return None;
    }
    if line == "continue" {
        return Some(Expr::Continue);
    }
    if line.starts_with("print") {
        if let Some(arg) = line
            .strip_prefix("print(")
            .and_then(|s| s.strip_suffix(")"))
        {
            let arg = arg.trim();
            return Some(Expr::Print(if arg.starts_with('"') && arg.ends_with('"') {
                Box::new(Expr::String(arg[1..arg.len() - 1].to_string()))
            } else if let Some(expr) = parse_binary_operation(arg) {
                Box::new(expr)
            } else {
                parse_value(arg)
            }));
        }
        return None; // Invalid print syntax
    }
    for op in COMPOUND_OPS {
        if line.contains(op) {
            let parts: Vec<&str> = line.split(op).collect();
            if parts.len() == 2 {
                let var_name = parts[0].trim();
                let value_str = parts[1].trim().trim_end_matches(';');
                let binary_op = match *op {
                    "+=" => BinaryOp::AddAssign,
                    "-=" => BinaryOp::SubAssign,
                    "*=" => BinaryOp::MulAssign,
                    "/=" => BinaryOp::DivAssign,
                    _ => unreachable!(),
                };
                return Some(Expr::Binary(
                    binary_op,
                    Box::new(Expr::String(var_name.to_string())),
                    parse_value(value_str),
                ));
            }
        }
    }
    if line.contains('=') && !line.starts_with("let ") {
        let parts: Vec<&str> = line.splitn(2, '=').collect();
        if parts.len() == 2 {
            let var_name = parts[0].trim();
            let value_str = parts[1].trim().trim_end_matches(';');
            return Some(Expr::Assign(var_name.to_string(), parse_value(value_str)));
        }
    }
    if !line.starts_with("print") {
        if let Some(expr) = parse_binary_operation(line) {
            return Some(expr);
        }
    }
    None
}
pub fn parse_file(content: &str) -> Vec<Expr> {
    let mut exprs = Vec::new();
    let mut content_lines = content.lines().peekable();
    let mut brace_count = 0;

    while let Some(line) = content_lines.next() {
        let trimmed = line.trim();
        if trimmed.is_empty() || trimmed.starts_with("//") {
            continue;
        }
        if trimmed.starts_with("if ") {
            if let Some(expr) = parse_if_statement(trimmed, &mut content_lines) {
                exprs.push(expr);
                continue;
            }
        }
        if trimmed.starts_with("for ") {
            if let Some(expr) = parse_for_loop(trimmed, &mut content_lines) {
                exprs.push(expr);
                continue;
            }
        }
        if trimmed.ends_with("{") {
            brace_count += 1;
        }
        if trimmed == "}" {
            brace_count -= 1;
            continue;
        }
        if let Some(expr) = parse_line(trimmed) {
            exprs.push(expr);
        }
    }
    exprs
}
