use inkwell::values::BasicValueEnum;
use inkwell::FloatPredicate;
use x_ast::{Expr, Statement, Type};

use crate::CodeGen;

impl<'ctx> CodeGen<'ctx> {
    pub fn gen_if(
        &mut self,
        condition: &Expr,
        then_block: &[Statement],
        else_block: &Option<Vec<Statement>>,
        self_type: Option<&Type>,
    ) -> Result<Option<BasicValueEnum<'ctx>>, String> {
        if let Some(Type::Custom(struct_name)) = self_type {
            if let Some(struct_def) = self.ast_structs.get(struct_name) {
                if let Some(invariant_expr) = &struct_def.invariant {
                    // If the 'if' condition is identical to the invariant, it's always true.
                    if **invariant_expr == *condition {
                        // Optimisation: Eliminate the branch entirely.
                        // Only generate the code for the `then` block.
                        let mut last_val = None;
                        for stmt in then_block {
                            last_val = self.gen_statement(stmt, self_type)?;
                        }
                        return Ok(last_val);
                    }
                }
            }
        }

        let cond_val = self.gen_expr(condition, self_type)?;

        let condition_bool = match cond_val {
            BasicValueEnum::IntValue(iv) => {
                let zero_int = iv.get_type().const_zero();
                self.builder
                    .build_int_compare(inkwell::IntPredicate::NE, iv, zero_int, "ifcond_int")
                    .unwrap()
            }
            BasicValueEnum::FloatValue(fv) => {
                let zero_float = fv.get_type().const_zero();
                self.builder
                    .build_float_compare(FloatPredicate::ONE, fv, zero_float, "ifcond_float")
                    .unwrap()
            }
            _ => return Err("If condition must be a number (int or float).".to_string()),
        };

        let function = self
            .current_function
            .ok_or("Not inside a function context")?;

        let then_bb = self.context.append_basic_block(function, "then");
        let else_bb = self.context.append_basic_block(function, "else");
        let merge_bb = self.context.append_basic_block(function, "ifcont");

        self.builder
            .build_conditional_branch(condition_bool, then_bb, else_bb)
            .unwrap();

        // --- THEN BLOCK ---
        self.builder.position_at_end(then_bb);
        let mut then_val_opt: Option<BasicValueEnum<'ctx>> = None;
        for stmt in then_block {
            then_val_opt = self.gen_statement(stmt, self_type)?;
        }
        let then_terminated = self
            .builder
            .get_insert_block()
            .and_then(|bb| bb.get_terminator())
            .is_some();
        let then_end_bb = self.builder.get_insert_block().unwrap_or(then_bb);

        // --- ELSE BLOCK ---
        self.builder.position_at_end(else_bb);
        let mut else_val_opt: Option<BasicValueEnum<'ctx>> = None;
        if let Some(else_stmts) = else_block {
            for stmt in else_stmts {
                else_val_opt = self.gen_statement(stmt, self_type)?;
            }
        }
        let else_terminated = self
            .builder
            .get_insert_block()
            .and_then(|bb| bb.get_terminator())
            .is_some();
        let else_end_bb = self.builder.get_insert_block().unwrap_or(else_bb);

        // --- MERGE BLOCK (PHI NODE) ---

        // If both branches are terminated (e.g., with returns), there's no code to merge.
        if then_terminated && else_terminated {
            self.builder.position_at_end(merge_bb);
            let _ = self.builder.build_unreachable(); // This block should not be reachable.
            return Ok(None);
        }

        // Determine the common result type for the PHI node.
        let phi_type = match (&then_val_opt, &else_val_opt) {
            (Some(t), Some(e)) => {
                if t.get_type() == e.get_type() {
                    t.get_type()
                } else if t.is_float_value() || e.is_float_value() {
                    self.context.f64_type().into()
                } else {
                    t.get_type()
                }
            }
            (Some(t), None) => t.get_type(),
            (None, Some(e)) => e.get_type(),
            (None, None) => self.context.f64_type().into(), // Default if neither branch yields a value.
        };

        // Finalise the 'then' branch: coerce value and add terminator.
        let mut incoming = vec![];
        if !then_terminated {
            self.builder.position_at_end(then_end_bb);
            let val = then_val_opt.unwrap_or_else(|| phi_type.const_zero());
            let coerced = self.coerce_value_to_type(val, phi_type, "phi_then_coerce")?;
            incoming.push((coerced, then_end_bb));
            self.builder.build_unconditional_branch(merge_bb).unwrap();
        }

        // Finalise the 'else' branch: coerce value and add terminator.
        if !else_terminated {
            self.builder.position_at_end(else_end_bb);
            let val = else_val_opt.unwrap_or_else(|| phi_type.const_zero());
            let coerced = self.coerce_value_to_type(val, phi_type, "phi_else_coerce")?;
            incoming.push((coerced, else_end_bb));
            self.builder.build_unconditional_branch(merge_bb).unwrap();
        }

        // Now, build the PHI node in the merge block.
        self.builder.position_at_end(merge_bb);
        if !incoming.is_empty() {
            let phi = self.builder.build_phi(phi_type, "ifresult").unwrap();
            for (val, block) in incoming {
                phi.add_incoming(&[(&val, block)]);
            }
            Ok(Some(phi.as_basic_value()))
        } else {
            Ok(None)
        }
    }
}
