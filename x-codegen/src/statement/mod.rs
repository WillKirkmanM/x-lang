use crate::CodeGen;
use inkwell::values::{BasicValue, BasicValueEnum};
use x_ast::{Expr, Statement};

impl<'ctx> CodeGen<'ctx> {
    pub(crate) fn gen_statement(
        &mut self,
        stmt: &Statement,
    ) -> Result<Option<BasicValueEnum<'ctx>>, String> {
        match stmt {
            Statement::Expression { expr } => {
                self.gen_expr(expr).map(Some)
            }
            Statement::VariableDecl { name, value } => {
                self.current_binding_name = Some(name.clone());

                let alloca = self
                    .builder
                    .build_alloca(self.context.f64_type(), name)
                    .map_err(|e| e.to_string())?;

                let val = self.gen_expr(value)?;

                if let Expr::StructInstantiate(struct_init) = value {
                    self.variable_types
                        .insert(name.clone(), struct_init.name.clone());
                }

                self.current_binding_name = None;

                self.builder
                    .build_store(alloca, val)
                    .map_err(|e| e.to_string())?;

                self.variables.insert(name.clone(), alloca);

                Ok(None)
            }
            Statement::Import { module, item } => {
                self.process_import(module, item)?;
                Ok(None)
            }
            Statement::FileImport { path } => {
                self.process_file_import(path)?;
                Ok(None)
            }
            Statement::Function { .. } | Statement::ExternFunctionDecl { .. } => {
                Ok(None)
            }
            Statement::Block { statements } => {
                let mut last_value = None;
                for statement in statements {
                    last_value = self.gen_statement(statement)?;
                }
                Ok(last_value)
            }
            Statement::Comment(_) => Ok(None),
            Statement::ForLoop { var, start, end, body, } => {
                self.gen_for_loop(var, start, end, body)?;
                Ok(None)
            }
            Statement::If { condition, then_block, else_block, } => {
                self.gen_if(condition, then_block, else_block).map(|opt_float_value| opt_float_value.map(|float_value| float_value.into()))
            }
            Statement::WhileLoop { condition, body } => {
                self.gen_while_loop(condition, body)?;
                Ok(None)
            }
            Statement::StructDecl(struct_def) => {
                self.gen_struct_decl(struct_def)?;
                Ok(None)
            }
            Statement::Return { value } => {
                let current_fn = self.builder.get_insert_block()
                    .and_then(|bb| bb.get_parent())
                    .ok_or("Failed to get current function for return statement")?;
                let return_type = current_fn.get_type().get_return_type();

                let mut return_val_basic: Option<BasicValueEnum<'ctx>> = None;

                match value {
                    Some(expr) => {
                        let expr_val = self.gen_expr(expr)?;
                        if let Some(ret_type) = return_type {
                            if ret_type == expr_val.get_type() {
                                return_val_basic = Some(expr_val);
                            } else {
                                if ret_type.is_float_type() && expr_val.is_int_value() {
                                    eprintln!(
                                        "[CodeGen Warning] Coercing return value (int -> float) in function '{}'.",
                                        current_fn.get_name().to_str().unwrap_or("?")
                                    );
                                    return_val_basic = Some(self.builder.build_signed_int_to_float(expr_val.into_int_value(), ret_type.into_float_type(), "ret_coerce_i2f").unwrap().into());
                                } else if ret_type.is_int_type() && expr_val.is_float_value() {
                                     eprintln!(
                                        "[CodeGen Warning] Coercing return value (float -> int) in function '{}'.",
                                        current_fn.get_name().to_str().unwrap_or("?")
                                    );
                                    return_val_basic = Some(self.builder.build_float_to_signed_int(expr_val.into_float_value(), ret_type.into_int_type(), "ret_coerce_f2i").unwrap().into());
                                }
                                else {
                                    return_val_basic = Some(self.builder.build_bit_cast(expr_val, ret_type, "unsafe_ret_cast").unwrap());
                                }
                            }
                        } else {
                            eprintln!(
                                "[CodeGen Warning] Ignoring return value provided for void function '{}'.",
                                current_fn.get_name().to_str().unwrap_or("?")
                            );
                        }
                    }
                    None => {
                        if return_type.is_some() {
                            return Err(format!(
                                "Must return a value from non-void function '{}'.",
                                current_fn.get_name().to_str().unwrap_or("?")
                            ));
                        }
                    }
                };

                self.builder.build_return(return_val_basic.as_ref().map(|v| v as &dyn BasicValue<'ctx>))
                    .map_err(|e| format!("Failed to build return instruction: {}", e))?;

                if self.builder.get_insert_block().and_then(|bb| bb.get_terminator()).is_none() {
                    let unreachable_block = self.context.append_basic_block(current_fn, "after_return");
                    self.builder.position_at_end(unreachable_block);
                }

                Ok(None)
            }
        }
    }
}
