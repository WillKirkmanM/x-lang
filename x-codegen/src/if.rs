use inkwell::values::BasicValueEnum;
use inkwell::FloatPredicate;
use x_ast::{Expr, Statement};

use crate::CodeGen;

impl<'ctx> CodeGen<'ctx> {
    pub fn gen_if(
        &mut self,
        condition: &Expr,
        then_block: &[Statement],
        else_block: &Option<Vec<Statement>>,
    ) -> Result<Option<BasicValueEnum<'ctx>>, String> {
        let cond_val = self.gen_expr(condition)?;

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

        let parent_block = self
            .builder
            .get_insert_block()
            .ok_or("Builder has no insert block before if")?;
        let function = parent_block
            .get_parent()
            .ok_or("Parent block has no function")?;

        let then_bb = self.context.append_basic_block(function, "then");
        let else_bb = self.context.append_basic_block(function, "else");
        let merge_bb = self.context.append_basic_block(function, "ifcont");

        self.builder
            .build_conditional_branch(condition_bool, then_bb, else_bb)
            .unwrap();

        self.builder.position_at_end(then_bb);
        let mut then_val_opt: Option<BasicValueEnum<'ctx>> = None;
        for stmt in then_block {
            then_val_opt = self.gen_statement(stmt)?;
        }
        let then_terminated = self
            .builder
            .get_insert_block()
            .and_then(|bb| bb.get_terminator())
            .is_some();
        let then_end_bb = self.builder.get_insert_block().unwrap_or(then_bb);
        if !then_terminated {
            self.builder.build_unconditional_branch(merge_bb).unwrap();
        }

        self.builder.position_at_end(else_bb);
        let mut else_val_opt: Option<BasicValueEnum<'ctx>> = None;
        if let Some(else_stmts) = else_block {
            for stmt in else_stmts {
                else_val_opt = self.gen_statement(stmt)?;
            }
        }
        let else_terminated = self
            .builder
            .get_insert_block()
            .and_then(|bb| bb.get_terminator())
            .is_some();
        let else_end_bb = self.builder.get_insert_block().unwrap_or(else_bb);
        if !else_terminated {
            self.builder.build_unconditional_branch(merge_bb).unwrap();
        }

        self.builder.position_at_end(merge_bb);

        let phi_type = match then_val_opt {
            Some(val) => val.get_type(),
            None => match else_val_opt {
                Some(val) => val.get_type(),
                None => self.context.f64_type().into(),
            },
        };

        if !then_terminated || !else_terminated {
            let phi = self.builder.build_phi(phi_type, "ifresult").unwrap();

            if !then_terminated {
                let then_incoming_val = then_val_opt.unwrap_or_else(|| phi_type.const_zero());
                phi.add_incoming(&[(&then_incoming_val, then_end_bb)]);
            }

            if !else_terminated {
                let else_incoming_val = else_val_opt.unwrap_or_else(|| phi_type.const_zero());
                phi.add_incoming(&[(&else_incoming_val, else_end_bb)]);
            }

            Ok(Some(phi.as_basic_value()))
        } else {
            Ok(None)
        }
    }
}
