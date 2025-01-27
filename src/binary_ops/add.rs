use inkwell::values::{IntValue, FunctionValue};
use crate::ast::Expr;
use crate::codegen::CodeGen;

pub fn build_add<'ctx>(
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
    Some(codegen.builder.build_int_add(lhs, rhs, "add").unwrap())
}