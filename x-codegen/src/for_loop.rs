use inkwell::module::Linkage;
use inkwell::types::{AnyType, BasicType};
use inkwell::values::{BasicValueEnum, FunctionValue};
use inkwell::IntPredicate;
use std::collections::HashMap;
use x_ast::{Expr, Statement, Type};

use crate::CodeGen;

impl<'ctx> CodeGen<'ctx> {
    /// Dispatches to either sequential or parallel for-loop generation.
    pub(crate) fn gen_for_range_loop(
        &mut self,
        var_name: &str,
        start_expr: &Expr,
        end_expr: &Expr,
        body: &[Statement],
        is_parallel: bool,
        self_type: Option<&Type>,
    ) -> Result<(), String> {
        if is_parallel {
            self.gen_parallel_for_range_loop(var_name, start_expr, end_expr, body, self_type)
        } else {
            self.gen_sequential_for_range_loop(var_name, start_expr, end_expr, body, self_type)
        }
    }

    /// Generates a standard, sequential for-loop.
    fn gen_sequential_for_range_loop(
        &mut self,
        var_name: &str,
        start_expr: &Expr,
        end_expr: &Expr,
        body: &[Statement],
        self_type: Option<&Type>,
    ) -> Result<(), String> {
        let function = self
            .current_function
            .ok_or("Cannot generate for loop outside a function")?;

        let start_val = self.gen_expr(start_expr, self_type)?;
        let i32_type = self.context.i32_type();
        let start_val_i32 = match start_val {
            BasicValueEnum::IntValue(iv) => self
                .builder
                .build_int_cast(iv, i32_type, "for_start_icast")
                .map_err(|e| format!("LLVM builder error: {:?}", e))?,
            BasicValueEnum::FloatValue(fv) => self
                .builder
                .build_float_to_signed_int(fv, i32_type, "for_start_fcast")
                .map_err(|e| format!("LLVM builder error: {:?}", e))?,
            _ => return Err("For loop start must be a number".to_string()),
        };

        let loop_var_alloca = self
            .builder
            .build_alloca(i32_type, var_name)
            .map_err(|e| format!("LLVM builder error: {:?}", e))?;
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
            .map_err(|e| format!("LLVM builder error: {:?}", e))?
            .into_int_value();
        let end_val = self.gen_expr(end_expr, self_type)?;
        let end_val_i32 = match end_val {
            BasicValueEnum::IntValue(iv) => self
                .builder
                .build_int_cast(iv, i32_type, "for_end_icast")
                .map_err(|e| format!("LLVM builder error: {:?}", e))?,
            BasicValueEnum::FloatValue(fv) => self
                .builder
                .build_float_to_signed_int(fv, i32_type, "for_end_fcast")
                .map_err(|e| format!("LLVM builder error: {:?}", e))?,
            _ => return Err("For loop end must be a number".to_string()),
        };

        let condition = self
            .builder
            .build_int_compare(IntPredicate::SLT, current_val, end_val_i32, "for_cmp")
            .map_err(|e| format!("LLVM builder error: {:?}", e))?;
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
            self.gen_statement(stmt, self_type)?;
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
            .map_err(|e| format!("LLVM builder error: {:?}", e))?
            .into_int_value();
        let one = i32_type.const_int(1, false);
        let next_val = self
            .builder
            .build_int_add(current_val_inc, one, "for_next")
            .map_err(|e| format!("LLVM builder error: {:?}", e))?;
        self.builder.build_store(loop_var_alloca, next_val).unwrap();
        self.builder
            .build_unconditional_branch(loop_cond_bb)
            .unwrap();

        self.builder.position_at_end(loop_after_bb);

        Ok(())
    }

    /// Generates a call to a parallel runtime to execute a for-loop.
    fn gen_parallel_for_range_loop(
        &mut self,
        var_name: &str,
        start_expr: &Expr,
        end_expr: &Expr,
        body: &[Statement],
        self_type: Option<&Type>,
    ) -> Result<(), String> {
        let runtime_fn_name = "dispatch_parallel_for";
        let i64_type = self.context.i64_type();
        let ptr_type = self.context.ptr_type(inkwell::AddressSpace::default());
        let runtime_fn_type = self.context.void_type().fn_type(
            &[
                i64_type.into(), // start
                i64_type.into(), // end
                ptr_type.into(), // work_fn pointer
            ],
            false,
        );
        let runtime_fn = self
            .module
            .add_function(runtime_fn_name, runtime_fn_type, None);

        // 3. Generate the values for the start and end of the range (as i64).
        let start_val_any = self.gen_expr(start_expr, self_type)?;
        let end_val_any = self.gen_expr(end_expr, self_type)?;
        let start_i64 = self.cast_to_i64(start_val_any, "start_i64")?;
        let end_i64 = self.cast_to_i64(end_val_any, "end_i64")?;

        let mut array_name_to_alloc: Option<String> = None;
        if let Some(Statement::Expression { expr }) = body.first() {
            if let Expr::Assignment { target, .. } = expr {
                if let Expr::ArrayAccess { array, .. } = &**target {
                    if let Expr::Identifier(name) = &**array {
                        array_name_to_alloc = Some(name.clone());
                    }
                }
            }
        }

        if let Some(array_name) = array_name_to_alloc.clone() {
            if let Some(slice_alloca) = self.variables.get(&array_name) {
                let array_type = self
                    .variable_types
                    .get(&array_name)
                    .ok_or_else(|| format!("Unknown type for array {}", array_name))?;

                let elem_ty = match array_type {
                    Type::Array(inner) => match **inner {
                        Type::Int => self.context.i32_type().as_basic_type_enum(),
                        Type::Float => self.context.f64_type().as_basic_type_enum(),
                        _ => return Err(format!("Unsupported array element type: {:?}", inner)),
                    },
                    _ => return Err(format!("{} is not an array type", array_name)),
                };

                let field_types: &[inkwell::types::BasicTypeEnum<'ctx>] = &[
                    self.context
                        .ptr_type(inkwell::AddressSpace::default())
                        .into(), // data ptr
                    i64_type.into(), // length
                ];
                let slice_struct_type = self.context.struct_type(field_types, false);

                let buf_ptr = self
                    .builder
                    .build_array_alloca(elem_ty, end_i64, &format!("{}_buf", array_name))
                    .map_err(|e| format!("LLVM builder error: {:?}", e))?;

                let data_ptr_field = self
                    .builder
                    .build_struct_gep(
                        slice_struct_type,
                        *slice_alloca,
                        0,
                        &format!("{}_data_gep", array_name),
                    )
                    .map_err(|e| format!("LLVM builder error: {:?}", e))?;
                self.builder
                    .build_store(data_ptr_field, buf_ptr)
                    .map_err(|e| format!("LLVM builder error: {:?}", e))?;

                let len_field = self
                    .builder
                    .build_struct_gep(
                        slice_struct_type,
                        *slice_alloca,
                        1,
                        &format!("{}_len_gep", array_name),
                    )
                    .map_err(|e| format!("LLVM builder error: {:?}", e))?;
                self.builder
                    .build_store(len_field, end_i64)
                    .map_err(|e| format!("LLVM builder error: {:?}", e))?;
            }
        }

        let work_fn = self.outline_loop_body(var_name, body, self_type, &[])?;
        let work_ptr = work_fn.as_global_value().as_pointer_value();
        self.builder
            .build_call(
                runtime_fn,
                &[start_i64.into(), end_i64.into(), work_ptr.into()],
                "dispatch_parallel",
            )
            .map_err(|e| e.to_string())?;
        Ok(())
    }

    /// Extracts the loop body into a new, private function for parallel execution.
    fn outline_loop_body(
        &mut self,
        iv_name: &str,
        body: &[Statement],
        self_type: Option<&Type>,
        _captured_vars: &[String],
    ) -> Result<FunctionValue<'ctx>, String> {
        let original_block = self.builder.get_insert_block();
        let original_vars = self.variables.clone();
        let original_var_types = self.variable_types.clone();
        let captures: Vec<String> = original_vars
            .keys()
            .filter(|k| k.as_str() != iv_name)
            .cloned()
            .collect();
        let mut capture_globals: HashMap<String, inkwell::values::PointerValue<'ctx>> =
            HashMap::new();

        if original_block.is_some() {
            for name in &captures {
                if let Some(alloca_ptr) = original_vars.get(name) {
                    let ptr_ty = alloca_ptr.get_type();
                    let elem_any = ptr_ty.as_any_type_enum();
                    use inkwell::types::AnyTypeEnum;
                    let elem_bt = match elem_any {
                        AnyTypeEnum::StructType(st) => st.as_basic_type_enum(),
                        AnyTypeEnum::IntType(it) => it.as_basic_type_enum(),
                        AnyTypeEnum::FloatType(ft) => ft.as_basic_type_enum(),
                        AnyTypeEnum::PointerType(pt) => pt.as_basic_type_enum(),
                        AnyTypeEnum::ArrayType(at) => at.as_basic_type_enum(),
                        AnyTypeEnum::VectorType(vt) => vt.as_basic_type_enum(),
                        other => {
                            return Err(format!(
                                "Unsupported captured variable type for '{}': {:?}",
                                name, other
                            ));
                        }
                    };

                    let global_name = format!("__capt_{}_{}", name, self.get_unique_id());
                    let global = self.module.add_global(elem_bt, None, &global_name);
                    global.set_linkage(Linkage::Internal);
                    global.set_initializer(&elem_bt.const_zero());

                    // Load the original alloca value (the struct/ptr) and store into the global.
                    let loaded = self
                        .builder
                        .build_load(elem_bt, *alloca_ptr, &format!("{}_capt_load", name))
                        .map_err(|e| format!("LLVM builder error: {:?}", e))?;
                    self.builder
                        .build_store(global.as_pointer_value(), loaded)
                        .unwrap();
                    capture_globals.insert(name.clone(), global.as_pointer_value());
                }
            }
        }

        let i64_type = self.context.i64_type();
        let work_fn_type = self.context.void_type().fn_type(&[i64_type.into()], false);
        let work_fn_name = format!("__work_fn_{}", self.get_unique_id());
        let work_fn = self.module.add_function(&work_fn_name, work_fn_type, None);
        let entry = self.context.append_basic_block(work_fn, "entry");
        self.builder.position_at_end(entry);

        // A parallel function has its own variable scope. Start with an empty scope and
        // populate with captured globals (so accesses resolve to loads from globals),
        // plus the induction variable we create locally.
        self.variables = HashMap::new();
        self.variable_types = original_var_types.clone(); // keep types for lookups

        // Insert captured globals into the local variable map (pointer to global memory)
        for (name, gv_ptr) in &capture_globals {
            self.variables.insert(name.clone(), (*gv_ptr).into());
        }

        let induction_var_i64 = work_fn.get_nth_param(0).unwrap().into_int_value();
        let i32_type = self.context.i32_type();
        let induction_var_i32 = self
            .builder
            .build_int_cast(induction_var_i64, i32_type, "iv_trunc")
            .map_err(|e| format!("LLVM builder error: {:?}", e))?;
        let alloca = self
            .builder
            .build_alloca(i32_type, iv_name)
            .map_err(|e| format!("LLVM builder error: {:?}", e))?;
        self.builder
            .build_store(alloca, induction_var_i32)
            .map_err(|e| format!("LLVM builder error: {:?}", e))?;
        self.variables.insert(iv_name.to_string(), alloca);
        self.variable_types
            .insert(iv_name.to_string(), x_ast::Type::Int);

        for stmt in body {
            self.gen_statement(stmt, self_type)?;
            if self
                .builder
                .get_insert_block()
                .and_then(|bb| bb.get_terminator())
                .is_some()
            {
                break;
            }
        }
        self.builder
            .build_return(None)
            .map_err(|e| format!("LLVM builder error: {:?}", e))?;
        self.variables = original_vars;
        self.variable_types = original_var_types;
        if let Some(block) = original_block {
            self.builder.position_at_end(block);
        }
        Ok(work_fn)
    }

    fn cast_to_i64(
        &self,
        val: BasicValueEnum<'ctx>,
        name: &str,
    ) -> Result<inkwell::values::IntValue<'ctx>, String> {
        let i64_type = self.context.i64_type();
        match val {
            BasicValueEnum::IntValue(iv) => self
                .builder
                .build_int_cast(iv, i64_type, name)
                .map_err(|e| format!("LLVM builder error: {:?}", e)),
            BasicValueEnum::FloatValue(fv) => self
                .builder
                .build_float_to_signed_int(fv, i64_type, name)
                .map_err(|e| format!("LLVM builder error: {:?}", e)),
            _ => Err("Loop range value must be a number".to_string()),
        }
    }

    pub(crate) fn gen_for_each_loop(
        &mut self,
        var_name: &str,
        iterator_expr: &Expr,
        body: &[Statement],
        self_type: Option<&Type>,
    ) -> Result<(), String> {
        let function = self
            .current_function
            .ok_or("Cannot generate for-each loop outside a function")?;

        let array_struct_val = self.gen_expr(iterator_expr, self_type)?.into_struct_value();

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
            self.gen_statement(stmt, self_type)?;
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
