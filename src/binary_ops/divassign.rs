use inkwell::values::{IntValue, FunctionValue};
use crate::ast::Expr;
use crate::codegen::CodeGen;

pub fn build_divassign<'ctx>(
    codegen: &mut CodeGen<'ctx>,
    left: &Box<Expr>, 
    right: &Box<Expr>,
    function: FunctionValue<'ctx>,
    printf: FunctionValue<'ctx>, 
    debug: bool,
    in_print: bool,
) -> Option<IntValue<'ctx>> {
    let lhs = codegen.generate_expr(left, function, printf, debug, in_print)?;
    let rhs = codegen.generate_expr(right, function, printf, debug, in_print)?;
    let new_val = codegen
    .builder
    .build_int_signed_div(lhs, rhs, "divassign")
    .unwrap();
    if let Expr::String(name) = &**left {
    if let Some(&ptr) = codegen.variables.get(name) {
    codegen.builder.build_store(ptr, new_val).unwrap();
    }
    }
    Some(new_val)
}