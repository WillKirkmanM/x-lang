use inkwell::values::BasicValueEnum;
use inkwell::IntPredicate;
use x_ast::{Expr, Statement, Type};

use crate::CodeGen;

impl<'ctx> CodeGen<'ctx> {
    pub(crate) fn gen_for_range_loop(
        &mut self,
        var_name: &str,
        start_expr: &Expr,
        end_expr: &Expr,
        body: &[Statement],
    ) -> Result<(), String> {
        let function = self
            .builder
            .get_insert_block()
            .and_then(|bb| bb.get_parent())
            .ok_or("Cannot generate for loop outside a function")?;

        let start_val = self.gen_expr(start_expr)?;
        let i32_type = self.context.i32_type();
        let start_val_i32 = match start_val {
            BasicValueEnum::IntValue(iv) => self
                .builder
                .build_int_cast(iv, i32_type, "for_start_icast")
                .unwrap(),
            BasicValueEnum::FloatValue(fv) => self
                .builder
                .build_float_to_signed_int(fv, i32_type, "for_start_fcast")
                .unwrap(),
            _ => return Err("For loop start must be a number".to_string()),
        };

        let loop_var_alloca = self.builder.build_alloca(i32_type, var_name).unwrap();
        self.builder
            .build_store(loop_var_alloca, start_val_i32)
            .unwrap();

        let loop_cond_bb = self.context.append_basic_block(function, "for_cond");
        let loop_body_bb = self.context.append_basic_block(function, "for_body");
        let loop_inc_bb = self.context.append_basic_block(function, "for_inc");
        let loop_after_bb = self.context.append_basic_block(function, "for_after");

        self.builder
            .build_unconditional_branch(loop_cond_bb)
            .unwrap();

        self.builder.position_at_end(loop_cond_bb);
        let current_val = self
            .builder
            .build_load(i32_type, loop_var_alloca, &format!("{}_load", var_name))
            .unwrap()
            .into_int_value();
        let end_val = self.gen_expr(end_expr)?;
        let end_val_i32 = match end_val {
            BasicValueEnum::IntValue(iv) => self
                .builder
                .build_int_cast(iv, i32_type, "for_end_icast")
                .unwrap(),
            BasicValueEnum::FloatValue(fv) => self
                .builder
                .build_float_to_signed_int(fv, i32_type, "for_end_fcast")
                .unwrap(),
            _ => return Err("For loop end must be a number".to_string()),
        };

        let condition = self
            .builder
            .build_int_compare(IntPredicate::SLT, current_val, end_val_i32, "for_cmp")
            .unwrap();
        self.builder
            .build_conditional_branch(condition, loop_body_bb, loop_after_bb)
            .unwrap();

        self.builder.position_at_end(loop_body_bb);

        let old_vars = self.variables.clone();
        let old_var_types = self.variable_types.clone();
        self.variables.insert(var_name.to_string(), loop_var_alloca);
        self.variable_types
            .insert(var_name.to_string(), x_ast::Type::Int);

        for stmt in body {
            self.gen_statement(stmt)?;
            if self
                .builder
                .get_insert_block()
                .and_then(|bb| bb.get_terminator())
                .is_some()
            {
                break;
            }
        }

        self.variables = old_vars;
        self.variable_types = old_var_types;

        if self
            .builder
            .get_insert_block()
            .and_then(|bb| bb.get_terminator())
            .is_none()
        {
            self.builder
                .build_unconditional_branch(loop_inc_bb)
                .unwrap();
        }

        self.builder.position_at_end(loop_inc_bb);
        let current_val_inc = self
            .builder
            .build_load(i32_type, loop_var_alloca, &format!("{}_load_inc", var_name))
            .unwrap()
            .into_int_value();
        let one = i32_type.const_int(1, false);
        let next_val = self
            .builder
            .build_int_add(current_val_inc, one, "for_next")
            .unwrap();
        self.builder.build_store(loop_var_alloca, next_val).unwrap();
        self.builder
            .build_unconditional_branch(loop_cond_bb)
            .unwrap();

        self.builder.position_at_end(loop_after_bb);

        Ok(())
    }

    pub(crate) fn gen_for_each_loop(
        &mut self,
        var_name: &str,
        iterator_expr: &Expr,
        body: &[Statement],
    ) -> Result<(), String> {
        let function = self
            .builder
            .get_insert_block()
            .and_then(|b| b.get_parent())
            .unwrap();

        let array_struct_val = self.gen_expr(iterator_expr)?.into_struct_value();

        let data_ptr = self
            .builder
            .build_extract_value(array_struct_val, 0, "array_data_ptr")
            .unwrap()
            .into_pointer_value();
        let array_len = self
            .builder
            .build_extract_value(array_struct_val, 1, "array_len")
            .unwrap()
            .into_int_value();

        let i64_type = self.context.i64_type();
        let index_ptr = self.builder.build_alloca(i64_type, "i").unwrap();
        self.builder
            .build_store(index_ptr, i64_type.const_int(0, false))
            .unwrap();

        let cond_bb = self.context.append_basic_block(function, "fe_cond");
        let body_bb = self.context.append_basic_block(function, "fe_body");
        let inc_bb = self.context.append_basic_block(function, "fe_inc");
        let after_bb = self.context.append_basic_block(function, "fe_after");

        self.builder.build_unconditional_branch(cond_bb).unwrap();

        self.builder.position_at_end(cond_bb);
        let current_index = self
            .builder
            .build_load(i64_type, index_ptr, "i_val")
            .unwrap()
            .into_int_value();
        let condition = self
            .builder
            .build_int_compare(IntPredicate::ULT, current_index, array_len, "fe_cmp")
            .unwrap();
        self.builder
            .build_conditional_branch(condition, body_bb, after_bb)
            .unwrap();

        self.builder.position_at_end(body_bb);
        let array_element_type = self.context.f64_type();

        let item_ptr = unsafe {
            self.builder
                .build_gep(array_element_type, data_ptr, &[current_index], "elem_ptr")
                .unwrap()
        };
        let item_val = self
            .builder
            .build_load(array_element_type, item_ptr, "item_val")
            .unwrap();

        let loop_var_alloca = self
            .builder
            .build_alloca(array_element_type, var_name)
            .unwrap();
        self.builder.build_store(loop_var_alloca, item_val).unwrap();

        let old_vars = self.variables.clone();
        let old_var_types = self.variable_types.clone();
        self.variables.insert(var_name.to_string(), loop_var_alloca);
        self.variable_types
            .insert(var_name.to_string(), Type::Float);

        for stmt in body {
            self.gen_statement(stmt)?;
            if self
                .builder
                .get_insert_block()
                .and_then(|bb| bb.get_terminator())
                .is_some()
            {
                break;
            }
        }

        self.variables = old_vars;
        self.variable_types = old_var_types;

        self.builder.build_unconditional_branch(inc_bb).unwrap();

        self.builder.position_at_end(inc_bb);
        let one = i64_type.const_int(1, false);
        let next_index = self
            .builder
            .build_int_add(current_index, one, "next_i")
            .unwrap();
        self.builder.build_store(index_ptr, next_index).unwrap();
        self.builder.build_unconditional_branch(cond_bb).unwrap();

        self.builder.position_at_end(after_bb);
        Ok(())
    }
}
