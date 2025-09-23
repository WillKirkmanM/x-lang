use inkwell::{values::BasicValueEnum, FloatPredicate, IntPredicate};
use x_ast::{Expr, Operator, Type};

use crate::CodeGen;

impl<'ctx> CodeGen<'ctx> {
    pub fn handle_binary_op(
        &mut self,
        left: &Expr,
        op: Operator,
        right: &Expr,
        self_type: Option<&Type>,
    ) -> Result<BasicValueEnum<'ctx>, String> {
        let lhs_val = self.gen_expr(left, self_type)?;
        let rhs_val = self.gen_expr(right, self_type)?;

        let result = match (lhs_val, rhs_val) {
            // Pointer / Pointer comparisons (e.g. string pointers)
            (BasicValueEnum::PointerValue(lhs_ptr), BasicValueEnum::PointerValue(rhs_ptr)) => {
                match op {
                    Operator::Equal | Operator::NotEqual => {
                        // compare pointers by casting to integer and comparing
                        let lhs_int = self
                            .builder
                            .build_ptr_to_int(lhs_ptr, self.context.i64_type(), "ptr_to_int_l")
                            .map_err(|e| e.to_string())?;
                        let rhs_int = self
                            .builder
                            .build_ptr_to_int(rhs_ptr, self.context.i64_type(), "ptr_to_int_r")
                            .map_err(|e| e.to_string())?;

                        let pred = if matches!(op, Operator::Equal) {
                            IntPredicate::EQ
                        } else {
                            IntPredicate::NE
                        };

                        let cmp = self
                            .builder
                            .build_int_compare(pred, lhs_int, rhs_int, "ptr_cmptmp")
                            .map_err(|e| e.to_string())?;
                        let f = self
                            .builder
                            .build_unsigned_int_to_float(cmp, self.context.f64_type(), "booltmp")
                            .map_err(|e| e.to_string())?;
                        Ok(f.into())
                    }
                    _ => Err(
                        "Only equality/inequality comparisons are supported for pointers"
                            .to_string(),
                    ),
                }
            }
            // Integer / Integer
            (BasicValueEnum::IntValue(lhs_int), BasicValueEnum::IntValue(rhs_int)) => {
                let zero_i64 = self.context.i64_type().const_zero();
                match op {
                    Operator::Add => {
                        let v = self
                            .builder
                            .build_int_add(lhs_int, rhs_int, "addtmp")
                            .map_err(|e| e.to_string())?;
                        Ok(v.into())
                    }
                    Operator::Subtract => {
                        let v = self
                            .builder
                            .build_int_sub(lhs_int, rhs_int, "subtmp")
                            .map_err(|e| e.to_string())?;
                        Ok(v.into())
                    }
                    Operator::Multiply => {
                        let v = self
                            .builder
                            .build_int_mul(lhs_int, rhs_int, "multmp")
                            .map_err(|e| e.to_string())?;
                        Ok(v.into())
                    }
                    Operator::Divide => {
                        let v = self
                            .builder
                            .build_int_signed_div(lhs_int, rhs_int, "divtmp")
                            .map_err(|e| e.to_string())?;
                        Ok(v.into())
                    }
                    Operator::Modulo => {
                        let int_val = self
                            .builder
                            .build_int_signed_rem(lhs_int, rhs_int, "modtmp")
                            .map_err(|e| e.to_string())?;
                        let float_val = self
                            .builder
                            .build_signed_int_to_float(
                                int_val,
                                self.context.f64_type(),
                                "mod_to_float",
                            )
                            .map_err(|e| e.to_string())?;
                        Ok(float_val.into())
                    }
                    Operator::ShiftLeft => {
                        let int_val = self
                            .builder
                            .build_left_shift(lhs_int, rhs_int, "shltmp")
                            .map_err(|e| e.to_string())?;
                        let float_val = self
                            .builder
                            .build_signed_int_to_float(
                                int_val,
                                self.context.f64_type(),
                                "shl_to_float",
                            )
                            .map_err(|e| e.to_string())?;
                        Ok(float_val.into())
                    }
                    Operator::ShiftRight => {
                        let int_val = self
                            .builder
                            .build_right_shift(lhs_int, rhs_int, true, "shrtmp")
                            .map_err(|e| e.to_string())?;
                        let float_val = self
                            .builder
                            .build_signed_int_to_float(
                                int_val,
                                self.context.f64_type(),
                                "shr_to_float",
                            )
                            .map_err(|e| e.to_string())?;
                        Ok(float_val.into())
                    }
                    Operator::BitAnd => {
                        let int_val = self
                            .builder
                            .build_and(lhs_int, rhs_int, "bitandtmp")
                            .map_err(|e| e.to_string())?;
                        let float_val = self
                            .builder
                            .build_signed_int_to_float(
                                int_val,
                                self.context.f64_type(),
                                "bitand_to_float",
                            )
                            .map_err(|e| e.to_string())?;
                        Ok(float_val.into())
                    }
                    Operator::Xor => {
                        let int_val = self
                            .builder
                            .build_xor(lhs_int, rhs_int, "xortmp")
                            .map_err(|e| e.to_string())?;
                        let float_val = self
                            .builder
                            .build_signed_int_to_float(
                                int_val,
                                self.context.f64_type(),
                                "xor_to_float",
                            )
                            .map_err(|e| e.to_string())?;
                        Ok(float_val.into())
                    }
                    Operator::LessThan => {
                        let cmp = self
                            .builder
                            .build_int_compare(IntPredicate::SLT, lhs_int, rhs_int, "cmptmp")
                            .map_err(|e| e.to_string())?;
                        let f = self
                            .builder
                            .build_unsigned_int_to_float(cmp, self.context.f64_type(), "booltmp")
                            .map_err(|e| e.to_string())?;
                        Ok(f.into())
                    }
                    Operator::GreaterThan => {
                        let cmp = self
                            .builder
                            .build_int_compare(IntPredicate::SGT, lhs_int, rhs_int, "cmptmp")
                            .map_err(|e| e.to_string())?;
                        let f = self
                            .builder
                            .build_unsigned_int_to_float(cmp, self.context.f64_type(), "booltmp")
                            .map_err(|e| e.to_string())?;
                        Ok(f.into())
                    }
                    Operator::LessThanOrEqual => {
                        let cmp = self
                            .builder
                            .build_int_compare(IntPredicate::SLE, lhs_int, rhs_int, "cmptmp")
                            .map_err(|e| e.to_string())?;
                        let f = self
                            .builder
                            .build_unsigned_int_to_float(cmp, self.context.f64_type(), "booltmp")
                            .map_err(|e| e.to_string())?;
                        Ok(f.into())
                    }
                    Operator::GreaterThanOrEqual => {
                        let cmp = self
                            .builder
                            .build_int_compare(IntPredicate::SGE, lhs_int, rhs_int, "cmptmp")
                            .map_err(|e| e.to_string())?;
                        let f = self
                            .builder
                            .build_unsigned_int_to_float(cmp, self.context.f64_type(), "booltmp")
                            .map_err(|e| e.to_string())?;
                        Ok(f.into())
                    }
                    Operator::Equal => {
                        let cmp = self
                            .builder
                            .build_int_compare(IntPredicate::EQ, lhs_int, rhs_int, "cmptmp")
                            .map_err(|e| e.to_string())?;
                        let f = self
                            .builder
                            .build_unsigned_int_to_float(cmp, self.context.f64_type(), "booltmp")
                            .map_err(|e| e.to_string())?;
                        Ok(f.into())
                    }
                    Operator::NotEqual => {
                        let cmp = self
                            .builder
                            .build_int_compare(IntPredicate::NE, lhs_int, rhs_int, "cmptmp")
                            .map_err(|e| e.to_string())?;
                        let f = self
                            .builder
                            .build_unsigned_int_to_float(cmp, self.context.f64_type(), "booltmp")
                            .map_err(|e| e.to_string())?;
                        Ok(f.into())
                    }
                    Operator::Or => {
                        let lhs_bool = self
                            .builder
                            .build_int_compare(IntPredicate::NE, lhs_int, zero_i64, "lhs_bool")
                            .map_err(|e| e.to_string())?;
                        let rhs_bool = self
                            .builder
                            .build_int_compare(IntPredicate::NE, rhs_int, zero_i64, "rhs_bool")
                            .map_err(|e| e.to_string())?;
                        let or_bool = self
                            .builder
                            .build_or(lhs_bool, rhs_bool, "or_bool")
                            .map_err(|e| e.to_string())?;
                        let f = self
                            .builder
                            .build_unsigned_int_to_float(
                                or_bool,
                                self.context.f64_type(),
                                "bool_to_float",
                            )
                            .map_err(|e| e.to_string())?;
                        Ok(f.into())
                    }
                    Operator::And => {
                        let lhs_bool = self
                            .builder
                            .build_int_compare(IntPredicate::NE, lhs_int, zero_i64, "lhs_bool")
                            .map_err(|e| e.to_string())?;
                        let rhs_bool = self
                            .builder
                            .build_int_compare(IntPredicate::NE, rhs_int, zero_i64, "rhs_bool")
                            .map_err(|e| e.to_string())?;
                        let and_bool = self
                            .builder
                            .build_and(lhs_bool, rhs_bool, "and_bool")
                            .map_err(|e| e.to_string())?;
                        let f = self
                            .builder
                            .build_unsigned_int_to_float(
                                and_bool,
                                self.context.f64_type(),
                                "bool_to_float",
                            )
                            .map_err(|e| e.to_string())?;
                        Ok(f.into())
                    }
                }
            }

            // Float / Float
            (BasicValueEnum::FloatValue(lhs_float), BasicValueEnum::FloatValue(rhs_float)) => {
                match op {
                    Operator::Add => Ok(self
                        .builder
                        .build_float_add(lhs_float, rhs_float, "addtmp")
                        .map_err(|e| e.to_string())?
                        .into()),
                    Operator::Subtract => Ok(self
                        .builder
                        .build_float_sub(lhs_float, rhs_float, "subtmp")
                        .map_err(|e| e.to_string())?
                        .into()),
                    Operator::Multiply => Ok(self
                        .builder
                        .build_float_mul(lhs_float, rhs_float, "multmp")
                        .map_err(|e| e.to_string())?
                        .into()),
                    Operator::Divide => Ok(self
                        .builder
                        .build_float_div(lhs_float, rhs_float, "divtmp")
                        .map_err(|e| e.to_string())?
                        .into()),
                    Operator::Modulo => Ok(self
                        .builder
                        .build_float_rem(lhs_float, rhs_float, "fmodtmp")
                        .map_err(|e| e.to_string())?
                        .into()),
                    Operator::LessThan => self
                        .gen_comparison(FloatPredicate::OLT, lhs_float, rhs_float)
                        .map(|fv| fv.into()),
                    Operator::GreaterThan => self
                        .gen_comparison(FloatPredicate::OGT, lhs_float, rhs_float)
                        .map(|fv| fv.into()),
                    Operator::LessThanOrEqual => self
                        .gen_comparison(FloatPredicate::OLE, lhs_float, rhs_float)
                        .map(|fv| fv.into()),
                    Operator::GreaterThanOrEqual => self
                        .gen_comparison(FloatPredicate::OGE, lhs_float, rhs_float)
                        .map(|fv| fv.into()),
                    Operator::Equal => self
                        .gen_comparison(FloatPredicate::OEQ, lhs_float, rhs_float)
                        .map(|fv| fv.into()),
                    Operator::NotEqual => self
                        .gen_comparison(FloatPredicate::ONE, lhs_float, rhs_float)
                        .map(|fv| fv.into()),
                    Operator::Or => {
                        let entry_block = self.builder.get_insert_block().unwrap();
                        let function = entry_block.get_parent().unwrap();
                        let rhs_block = self.context.append_basic_block(function, "or_rhs");
                        let merge_block = self.context.append_basic_block(function, "or_merge");

                        let lhs_bool = self
                            .builder
                            .build_float_compare(
                                FloatPredicate::ONE,
                                lhs_float,
                                self.context.f64_type().const_float(0.0),
                                "lhs_bool",
                            )
                            .map_err(|e| e.to_string())?;
                        self.builder
                            .build_conditional_branch(lhs_bool, merge_block, rhs_block)
                            .map_err(|e| e.to_string())?;

                        self.builder.position_at_end(rhs_block);
                        let rhs_bool = self
                            .builder
                            .build_float_compare(
                                FloatPredicate::ONE,
                                rhs_float,
                                self.context.f64_type().const_float(0.0),
                                "rhs_bool",
                            )
                            .map_err(|e| e.to_string())?;
                        let rhs_end = self.builder.get_insert_block().unwrap();
                        self.builder
                            .build_unconditional_branch(merge_block)
                            .map_err(|e| e.to_string())?;

                        self.builder.position_at_end(merge_block);
                        let phi = self
                            .builder
                            .build_phi(self.context.bool_type(), "or_result")
                            .map_err(|e| e.to_string())?;
                        phi.add_incoming(&[
                            (&self.context.bool_type().const_int(1, false), entry_block),
                            (&rhs_bool, rhs_end),
                        ]);

                        let converted = self
                            .builder
                            .build_unsigned_int_to_float(
                                phi.as_basic_value().into_int_value(),
                                self.context.f64_type(),
                                "bool_to_float",
                            )
                            .map_err(|e| e.to_string())?;
                        Ok(converted.into())
                    }
                    Operator::And => {
                        let entry_block = self.builder.get_insert_block().unwrap();
                        let function = entry_block.get_parent().unwrap();
                        let rhs_block = self.context.append_basic_block(function, "and_rhs");
                        let merge_block = self.context.append_basic_block(function, "and_merge");

                        let lhs_bool = self
                            .builder
                            .build_float_compare(
                                FloatPredicate::ONE,
                                lhs_float,
                                self.context.f64_type().const_float(0.0),
                                "lhs_bool",
                            )
                            .map_err(|e| e.to_string())?;
                        self.builder
                            .build_conditional_branch(lhs_bool, rhs_block, merge_block)
                            .map_err(|e| e.to_string())?;

                        self.builder.position_at_end(rhs_block);
                        let rhs_bool = self
                            .builder
                            .build_float_compare(
                                FloatPredicate::ONE,
                                rhs_float,
                                self.context.f64_type().const_float(0.0),
                                "rhs_bool",
                            )
                            .map_err(|e| e.to_string())?;
                        let rhs_end = self.builder.get_insert_block().unwrap();
                        self.builder
                            .build_unconditional_branch(merge_block)
                            .map_err(|e| e.to_string())?;

                        self.builder.position_at_end(merge_block);
                        let phi = self
                            .builder
                            .build_phi(self.context.bool_type(), "and_result")
                            .map_err(|e| e.to_string())?;
                        phi.add_incoming(&[
                            (&self.context.bool_type().const_int(0, false), entry_block),
                            (&rhs_bool, rhs_end),
                        ]);

                        let converted = self
                            .builder
                            .build_unsigned_int_to_float(
                                phi.as_basic_value().into_int_value(),
                                self.context.f64_type(),
                                "bool_to_float",
                            )
                            .map_err(|e| e.to_string())?;
                        Ok(converted.into())
                    }
                    _ => Err("Bitwise operators are not supported for floats.".to_string()),
                }
            }

            // Mixed: Int left, Float right
            (BasicValueEnum::IntValue(lhs_int), BasicValueEnum::FloatValue(rhs_float)) => {
                let lhs_f = self
                    .builder
                    .build_signed_int_to_float(lhs_int, self.context.f64_type(), "int_to_float")
                    .map_err(|e| e.to_string())?;
                match op {
                    Operator::Add => Ok(self
                        .builder
                        .build_float_add(lhs_f, rhs_float, "addtmp")
                        .map_err(|e| e.to_string())?
                        .into()),
                    Operator::Subtract => Ok(self
                        .builder
                        .build_float_sub(lhs_f, rhs_float, "subtmp")
                        .map_err(|e| e.to_string())?
                        .into()),
                    Operator::Multiply => Ok(self
                        .builder
                        .build_float_mul(lhs_f, rhs_float, "multmp")
                        .map_err(|e| e.to_string())?
                        .into()),
                    Operator::Divide => Ok(self
                        .builder
                        .build_float_div(lhs_f, rhs_float, "divtmp")
                        .map_err(|e| e.to_string())?
                        .into()),
                    Operator::Modulo => Ok(self
                        .builder
                        .build_float_rem(lhs_f, rhs_float, "fmodtmp")
                        .map_err(|e| e.to_string())?
                        .into()),
                    Operator::LessThan => self
                        .gen_comparison(FloatPredicate::OLT, lhs_f, rhs_float)
                        .map(|fv| fv.into()),
                    Operator::GreaterThan => self
                        .gen_comparison(FloatPredicate::OGT, lhs_f, rhs_float)
                        .map(|fv| fv.into()),
                    Operator::LessThanOrEqual => self
                        .gen_comparison(FloatPredicate::OLE, lhs_f, rhs_float)
                        .map(|fv| fv.into()),
                    Operator::GreaterThanOrEqual => self
                        .gen_comparison(FloatPredicate::OGE, lhs_f, rhs_float)
                        .map(|fv| fv.into()),
                    Operator::Equal => self
                        .gen_comparison(FloatPredicate::OEQ, lhs_f, rhs_float)
                        .map(|fv| fv.into()),
                    Operator::NotEqual => self
                        .gen_comparison(FloatPredicate::ONE, lhs_f, rhs_float)
                        .map(|fv| fv.into()),
                    _ => Err(format!(
                        "Operator '{:?}' not supported for mixed int/float.",
                        op
                    )),
                }
            }

            // Mixed: Float left, Int right
            (BasicValueEnum::FloatValue(lhs_float), BasicValueEnum::IntValue(rhs_int)) => {
                let rhs_f = self
                    .builder
                    .build_signed_int_to_float(rhs_int, self.context.f64_type(), "int_to_float")
                    .map_err(|e| e.to_string())?;
                match op {
                    Operator::Add => Ok(self
                        .builder
                        .build_float_add(lhs_float, rhs_f, "addtmp")
                        .map_err(|e| e.to_string())?
                        .into()),
                    Operator::Subtract => Ok(self
                        .builder
                        .build_float_sub(lhs_float, rhs_f, "subtmp")
                        .map_err(|e| e.to_string())?
                        .into()),
                    Operator::Multiply => Ok(self
                        .builder
                        .build_float_mul(lhs_float, rhs_f, "multmp")
                        .map_err(|e| e.to_string())?
                        .into()),
                    Operator::Divide => Ok(self
                        .builder
                        .build_float_div(lhs_float, rhs_f, "divtmp")
                        .map_err(|e| e.to_string())?
                        .into()),
                    Operator::Modulo => Ok(self
                        .builder
                        .build_float_rem(lhs_float, rhs_f, "fmodtmp")
                        .map_err(|e| e.to_string())?
                        .into()),
                    Operator::LessThan => self
                        .gen_comparison(FloatPredicate::OLT, lhs_float, rhs_f)
                        .map(|fv| fv.into()),
                    Operator::GreaterThan => self
                        .gen_comparison(FloatPredicate::OGT, lhs_float, rhs_f)
                        .map(|fv| fv.into()),
                    Operator::LessThanOrEqual => self
                        .gen_comparison(FloatPredicate::OLE, lhs_float, rhs_f)
                        .map(|fv| fv.into()),
                    Operator::GreaterThanOrEqual => self
                        .gen_comparison(FloatPredicate::OGE, lhs_float, rhs_f)
                        .map(|fv| fv.into()),
                    Operator::Equal => self
                        .gen_comparison(FloatPredicate::OEQ, lhs_float, rhs_f)
                        .map(|fv| fv.into()),
                    Operator::NotEqual => self
                        .gen_comparison(FloatPredicate::ONE, lhs_float, rhs_f)
                        .map(|fv| fv.into()),
                    _ => Err(format!(
                        "Operator '{:?}' not supported for mixed float/int.",
                        op
                    )),
                }
            }

            _ => Err(format!(
                "Type mismatch: Cannot apply operator '{:?}' to mismatched or unsupported types.",
                op
            )),
        }?;

        Ok(result)
    }
}
