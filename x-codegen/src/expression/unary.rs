use inkwell::{values::BasicValueEnum, FloatPredicate};
use x_ast::{Expr, Type, UnaryOperator};

use crate::CodeGen;

impl<'ctx> CodeGen<'ctx> {
    pub fn handle_unary_op(
        &mut self,
        op: UnaryOperator,
        expr: &Expr,
        self_type: Option<&Type>,
    ) -> Result<BasicValueEnum<'ctx>, String> {
        match op {
            UnaryOperator::Negate => {
                let val = self.gen_expr(expr, self_type)?;

                let result = match val {
                    BasicValueEnum::IntValue(int_val) => self
                        .builder
                        .build_int_neg(int_val, "int_negtmp")
                        .map_err(|e| e.to_string())?
                        .into(),
                    BasicValueEnum::FloatValue(float_val) => self
                        .builder
                        .build_float_neg(float_val, "float_negtmp")
                        .map_err(|e| e.to_string())?
                        .into(),
                    _ => {
                        return Err(
                            "Negation operator can only be applied to integers and floats."
                                .to_string(),
                        );
                    }
                };

                Ok(result)
            }
            UnaryOperator::LogicalNot => {
                let val = self.gen_expr(expr, self_type)?.into_float_value();
                let is_zero = self
                    .builder
                    .build_float_compare(
                        FloatPredicate::OEQ,
                        val,
                        self.context.f64_type().const_float(0.0),
                        "is_zero",
                    )
                    .map_err(|e| e.to_string())?;
                Ok(self
                    .builder
                    .build_unsigned_int_to_float(is_zero, self.context.f64_type(), "bool_to_float")
                    .map_err(|e| e.to_string())?
                    .into())
            }
            UnaryOperator::BitwiseNot => {
                let val = self.gen_expr(expr, self_type)?.into_float_value();
                let int_val = self
                    .builder
                    .build_float_to_signed_int(val, self.context.i64_type(), "float_to_int")
                    .map_err(|e| e.to_string())?;
                let not_val = self
                    .builder
                    .build_not(int_val, "bitnottmp")
                    .map_err(|e| e.to_string())?;
                Ok(self
                    .builder
                    .build_signed_int_to_float(not_val, self.context.f64_type(), "int_to_float")
                    .map_err(|e| e.to_string())?
                    .into())
            }
            UnaryOperator::PreIncrement => self.gen_increment(expr, true, true),
            UnaryOperator::PreDecrement => self.gen_increment(expr, true, false),
            UnaryOperator::PostIncrement => self.gen_increment(expr, false, true),
            UnaryOperator::PostDecrement => self.gen_increment(expr, false, false),
        }
    }
}
