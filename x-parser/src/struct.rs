use pest::iterators::Pair;
use x_ast::{Expr, Statement, StructDef, StructInit};

use crate::{parse_expr, Rule};

pub fn parse_struct_decl(pair: Pair<Rule>) -> Statement {
    let mut inner = pair.into_inner();
    let name = inner.next().unwrap().as_str().to_string();
    
    let fields = if let Some(fields_pair) = inner.next() {
        fields_pair.into_inner()
            .map(|field| field.as_str().to_string())
            .collect()
    } else {
        Vec::new()
    };
    
    Statement::StructDecl(StructDef { name, fields })
}

pub fn parse_struct_instantiate(pair: Pair<Rule>) -> Expr {
    let mut inner = pair.into_inner();
    let name = inner.next().unwrap().as_str().to_string();
    
    let fields = if let Some(fields_pair) = inner.next() {
        fields_pair.into_inner()
            .map(|field_pair| {
                let mut field_inner = field_pair.into_inner();
                let field_name = field_inner.next().unwrap().as_str().to_string();
                let value = parse_expr(field_inner.next().unwrap());
                (field_name, value)
            })
            .collect()
    } else {
        Vec::new()
    };
    
    Expr::StructInstantiate(StructInit { name, fields })
}