use pest::iterators::Pair;
use x_ast::{Expr, Layout, Statement, StructDef, StructInit};

use crate::{parse_expr, types::parse_type, Rule};

pub fn parse_struct_decl(pair: Pair<Rule>) -> Statement {
    let mut inner = pair.into_inner().peekable();
    let mut layout = Layout::AoS;

    if inner
        .peek()
        .map_or(false, |p| p.as_rule() == Rule::attribute)
    {
        let attr = inner.next().unwrap();
        let attr_text = attr.as_str();
        if attr_text.contains("layout(SoA)") {
            layout = Layout::SoA;
        }
    }

    let parts: Vec<Pair<Rule>> = inner.collect();

    let mut iter = parts.into_iter().peekable();

    let name = iter.next().unwrap().as_str().to_string();

    let generic_params = if iter
        .peek()
        .map_or(false, |p| p.as_rule() == Rule::generic_param_def_list)
    {
        Some(
            iter.next()
                .unwrap()
                .into_inner()
                .map(|p| p.as_str().to_string())
                .collect::<Vec<_>>(),
        )
    } else {
        None
    };

    let fields = if let Some(fields_pair) = iter.next() {
        fields_pair
            .into_inner()
            .map(|field_pair| {
                let mut field_inner = field_pair.into_inner();
                let field_name = field_inner.next().unwrap().as_str().to_string();
                let field_type = parse_type(field_inner.next().unwrap());
                (field_name, field_type)
            })
            .collect()
    } else {
        Vec::new()
    };

    let invariant = if iter
        .peek()
        .map_or(false, |p| p.as_rule() == Rule::invariant_block)
    {
        let invariant_pair = iter.next().unwrap();
        let block_pair = invariant_pair.into_inner().next().unwrap();

        let block_stmt = crate::block::parse_block(block_pair);
        let statements = if let Statement::Block { statements } = block_stmt {
            statements
        } else {
            vec![]
        };

        if let Some(Statement::Expression { expr }) = statements.get(0) {
            Some(Box::new(expr.clone()))
        } else {
            None
        }
    } else {
        None
    };

    Statement::StructDecl(StructDef {
        name,
        generic_params,
        fields,
        invariant,
        layout,
    })
}

pub fn parse_struct_instantiate(pair: Pair<Rule>) -> Expr {
    let mut inner = pair.into_inner();
    let name = inner.next().unwrap().as_str().to_string();

    let fields = if let Some(fields_pair) = inner.next() {
        fields_pair
            .into_inner()
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
