use inkwell::values::{IntValue, FunctionValue};
use inkwell::IntPredicate;
use crate::ast::Expr;
use crate::codegen::CodeGen;

pub fn build_bitmod<'ctx>(
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

    let zero = codegen.context.i32_type().const_int(0, false);
    let is_zero = codegen.builder
        .build_int_compare(IntPredicate::EQ, rhs, zero, "is_zero")
        .unwrap();

    let safe_bb = codegen.context.append_basic_block(function, "safe_mod");
    let error_bb = codegen.context.append_basic_block(function, "mod_error");
    let merge_bb = codegen.context.append_basic_block(function, "merge");

    codegen.builder
        .build_conditional_branch(is_zero, error_bb, safe_bb)
        .unwrap();

    codegen.builder.position_at_end(safe_bb);
    let mod_result = codegen.builder.build_int_signed_rem(lhs, rhs, "mod").unwrap();
    codegen.builder.build_unconditional_branch(merge_bb).unwrap();

    codegen.builder.position_at_end(error_bb);
    let fmt = codegen.builder
        .build_global_string_ptr("Error: Division by zero\n", "error_fmt")
        .unwrap();
    codegen.builder
        .build_call(printf, &[fmt.as_pointer_value().into()], "printf")
        .unwrap();
    codegen.builder.build_unconditional_branch(merge_bb).unwrap();

    codegen.builder.position_at_end(merge_bb);
    Some(mod_result)
}