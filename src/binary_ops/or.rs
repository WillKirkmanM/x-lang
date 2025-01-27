use inkwell::values::{IntValue, FunctionValue};
use crate::ast::Expr;
use crate::codegen::CodeGen;

pub fn build_or<'ctx>(
    codegen: &mut CodeGen<'ctx>,
    left: &Box<Expr>, 
    right: &Box<Expr>,
    function: FunctionValue<'ctx>,
    printf: FunctionValue<'ctx>, 
    debug: bool,
    in_print: bool,
) -> Option<IntValue<'ctx>> {
    let lhs = codegen.generate_expr(left, function, printf, debug, in_print)?;
    let right_bb = codegen.context.append_basic_block(function, "or_right");
    let merge_bb = codegen.context.append_basic_block(function, "merge");
    
    let left_bool = codegen
    .builder
    .build_int_truncate_or_bit_cast(lhs, codegen.context.bool_type(), "to_bool")
    .unwrap();
    
    codegen.builder
    .build_conditional_branch(left_bool, merge_bb, right_bb)
    .unwrap();
    
    codegen.builder.position_at_end(right_bb);
    let rhs = codegen.generate_expr(right, function, printf, debug, in_print)?;
    let right_bool = codegen
    .builder
    .build_int_truncate_or_bit_cast(rhs, codegen.context.bool_type(), "to_bool")
    .unwrap();
    codegen.builder.build_unconditional_branch(merge_bb).unwrap();
    
    codegen.builder.position_at_end(merge_bb);
    let phi = codegen
    .builder
    .build_phi(codegen.context.bool_type(), "or_result")
    .unwrap();
    phi.add_incoming(&[
    (&codegen.context.bool_type().const_int(1, false), merge_bb),
    (&right_bool, right_bb),
    ]);
    
    Some(
    codegen.builder
    .build_int_z_extend(
    phi.as_basic_value().into_int_value(),
    codegen.context.i32_type(),
    "to_i32",
    )
    .unwrap(),
    )
}