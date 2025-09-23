use std::collections::HashMap;

use crate::CodeGen;
use inkwell::values::{BasicValue, BasicValueEnum};
use x_ast::{Expr, Statement, StructDef, Type};

use x_logging::warn;

impl<'ctx> CodeGen<'ctx> {
    pub(crate) fn gen_statement(
        &mut self,
        stmt: &Statement,
        self_type: Option<&Type>,
    ) -> Result<Option<BasicValueEnum<'ctx>>, String> {
        match stmt {
            Statement::Expression { expr } => self.gen_expr(expr, self_type).map(Some),
            Statement::VariableDecl {
                name,
                type_ann,
                value,
            } => {
                if let Expr::TypeLiteral(_) = value {
                    let var_ast_type = match type_ann {
                        Some(ann_type) => ann_type.clone(),
                        None => self.infer_ast_type_from_expr(value)?,
                    };
                    let var_llvm_type = self.map_ast_type_to_llvm(&var_ast_type);

                    let alloca = self.builder.build_alloca(var_llvm_type, name).unwrap();

                    self.variables.insert(name.clone(), alloca);
                    self.variable_types.insert(name.clone(), var_ast_type);

                    Ok(None)
                } else {
                    // For non-TypeLiteral RHS we prefer to generate the RHS first when no
                    // annotation is provided so we can pick the correct LLVM type (avoids
                    // defaulting Unknown -> i32 and ptr<->int coercions for closures/function ptrs).
                    // Treat an explicit Unknown annotation as if there were no annotation.
                    if type_ann.is_none()
                        || matches!(type_ann.as_ref(), Some(x) if *x == x_ast::Type::Unknown)
                    {
                        // Generate the RHS value first to discover its LLVM type.
                        let rhs_val = self.gen_expr(value, self_type)?;
                        let rhs_llvm_type = rhs_val.get_type();

                        // Allocate a slot of the exact LLVM type the RHS produced.
                        let alloca = self.builder.build_alloca(rhs_llvm_type, name).unwrap();

                        // Store the RHS into the slot. If necessary, coerce (should be same type).
                        let coerced =
                            self.coerce_value_to_type(rhs_val, rhs_llvm_type, "assign_coerce")?;
                        self.builder.build_store(alloca, coerced).unwrap();

                        // Record variable info. Try to infer an AST type but fall back to Unknown.
                        let var_ast_type = match self.infer_ast_type_from_expr(value) {
                            Ok(t) => t,
                            Err(_) => x_ast::Type::Unknown,
                        };

                        self.variables.insert(name.clone(), alloca);
                        self.variable_types.insert(name.clone(), var_ast_type);

                        Ok(None)
                    } else {
                        let var_ast_type = match type_ann {
                            Some(ann_type) => ann_type.clone(),
                            None => unreachable!(),
                        };
                        let var_llvm_type = self.map_ast_type_to_llvm(&var_ast_type);

                        let alloca = self.builder.build_alloca(var_llvm_type, name).unwrap();

                        let value_val = self.gen_expr(value, self_type)?;
                        let coerced =
                            self.coerce_value_to_type(value_val, var_llvm_type, "assign_coerce")?;
                        self.builder.build_store(alloca, coerced).unwrap();

                        self.variables.insert(name.clone(), alloca);
                        self.variable_types.insert(name.clone(), var_ast_type);

                        Ok(None)
                    }
                }
            }

            Statement::Import { module, item } => {
                self.process_import(module, item)?;
                Ok(None)
            }
            Statement::FileImport { path } => {
                self.process_file_import(path)?;
                Ok(None)
            }
            Statement::Function { .. } | Statement::ExternFunctionDecl { .. } => Ok(None),
            Statement::Block { statements } => {
                let mut last_value = None;
                for statement in statements {
                    last_value = self.gen_statement(statement, self_type)?;
                }
                Ok(last_value)
            }
            Statement::Comment(_) => Ok(None),
            Statement::ForRangeLoop {
                var,
                start,
                end,
                body,
            } => {
                self.gen_for_range_loop(var, start, end, body, self_type)?;
                Ok(None)
            }
            Statement::ForEachLoop {
                var,
                iterator,
                body,
            } => {
                self.gen_for_each_loop(var, iterator, body, self_type)?;
                Ok(None)
            }
            Statement::If {
                condition,
                then_block,
                else_block,
            } => self
                .gen_if(condition, then_block, else_block, self_type)
                .map(|opt_float_value| opt_float_value.map(|float_value| float_value.into())),
            Statement::WhileLoop { condition, body } => {
                self.gen_while_loop(condition, body, self_type)?;
                Ok(None)
            }
            Statement::StructDecl(struct_def) => {
                self.gen_struct_decl(struct_def)?;
                Ok(None)
            }
            Statement::Return { value } => {
                let current_fn = self
                    .builder
                    .get_insert_block()
                    .and_then(|bb| bb.get_parent())
                    .ok_or("Failed to get current function for return statement")?;
                let return_type = current_fn.get_type().get_return_type();

                let mut return_val_basic: Option<BasicValueEnum<'ctx>> = None;

                match value {
                    Some(expr) => {
                        let expr_val = self.gen_expr(expr, self_type)?;
                        if let Some(ret_type) = return_type {
                            if ret_type == expr_val.get_type() {
                                return_val_basic = Some(expr_val);
                            } else {
                                if ret_type.is_float_type() && expr_val.is_int_value() {
                                    warn!(
                                        "Coercing return value (int -> float) in function '{}'.",
                                        current_fn.get_name().to_str().unwrap_or("?")
                                    );
                                    return_val_basic = Some(
                                        self.builder
                                            .build_signed_int_to_float(
                                                expr_val.into_int_value(),
                                                ret_type.into_float_type(),
                                                "ret_coerce_i2f",
                                            )
                                            .unwrap()
                                            .into(),
                                    );
                                } else if ret_type.is_int_type() && expr_val.is_float_value() {
                                    warn!(
                                        "Coercing return value (float -> int) in function '{}'.",
                                        current_fn.get_name().to_str().unwrap_or("?")
                                    );
                                    return_val_basic = Some(
                                        self.builder
                                            .build_float_to_signed_int(
                                                expr_val.into_float_value(),
                                                ret_type.into_int_type(),
                                                "ret_coerce_f2i",
                                            )
                                            .unwrap()
                                            .into(),
                                    );
                                } else {
                                    return Err(format!(
                                        "Cannot return type {:?} when function '{}' expects {:?}.",
                                        expr_val.get_type(),
                                        current_fn.get_name().to_str().unwrap_or("?"),
                                        ret_type,
                                    ));
                                }
                            }
                        } else {
                            warn!(
                                "Ignoring return value provided for void function '{}'.",
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

                self.builder
                    .build_return(
                        return_val_basic
                            .as_ref()
                            .map(|v| v as &dyn BasicValue<'ctx>),
                    )
                    .map_err(|e| format!("Failed to build return instruction: {}", e))?;

                if self
                    .builder
                    .get_insert_block()
                    .and_then(|bb| bb.get_terminator())
                    .is_none()
                {
                    let unreachable_block =
                        self.context.append_basic_block(current_fn, "after_return");
                    self.builder.position_at_end(unreachable_block);
                }

                Ok(None)
            }
            Statement::Become { call } => {
                self.gen_become(call, self_type)?;
                Ok(None)
            }
            Statement::ImplDecl(impl_def) => {
                for method_stmt in &impl_def.methods {
                    if let Statement::Function {
                        name,
                        params,
                        return_type,
                        body,
                        ..
                    } = method_stmt
                    {
                        self.gen_function_def(name, params.as_slice(), return_type, body, None)?;
                    } else {
                        return Err("Impl block can only contain function definitions.".to_string());
                    }
                }
                Ok(None)
            }
            Statement::TraitDecl(_) => Ok(None),
        }
    }

    pub fn infer_ast_type_from_expr(&self, expr: &Expr) -> Result<x_ast::Type, String> {
        match expr {
            Expr::Int(_) => Ok(x_ast::Type::Int),
            Expr::Float(_) => Ok(x_ast::Type::Float),
            Expr::Boolean(_) => Ok(x_ast::Type::Bool),
            Expr::String(_) => Ok(x_ast::Type::String),
            Expr::Array(_) => Ok(x_ast::Type::Array(Box::new(x_ast::Type::Unknown))),
            Expr::TypeLiteral(t) => Ok(t.clone()),
            Expr::ArrayAccess { array, .. } => {
                let array_type = self.infer_ast_type_from_expr(array)?;
                match array_type {
                    // Case 1: Direct array type, e.g., `let a: i32[] = ...; a[0]`
                    x_ast::Type::Array(inner) => Ok(*inner),
                    // Case 2: Reference to an array, e.g., `fn f(p: &mut i32[]) { p[0] }`
                    x_ast::Type::Ref { inner, .. } => {
                        if let x_ast::Type::Array(inner_inner) = *inner {
                            Ok(*inner_inner)
                        } else {
                            Err(format!(
                                "Array access on a reference to a non-array type: {:?}",
                                inner
                            ))
                        }
                    }
                    _ => Err(format!("Array access on non-array type: {:?}", array_type)),
                }
            }
            Expr::FieldAccess {
                object,
                field: _field,
            } => {
                let object_type = self.infer_ast_type_from_expr(object)?;
                let base_type = match &object_type {
                    x_ast::Type::Ref { inner, .. } => &**inner,
                    _ => &object_type,
                };

                if let x_ast::Type::Custom(_struct_name) = base_type {
                    return Ok(x_ast::Type::Unknown);
                }

                Err(format!(
                    "Could not infer type for field access on {:?}",
                    object_type
                ))
            }

            Expr::Identifier(name) => self
                .variable_types
                .get(name)
                .cloned()
                .ok_or_else(|| format!("Cannot infer type from unknown variable '{}'", name)),
            Expr::StructInstantiate(init) => Ok(x_ast::Type::Custom(init.name.clone())),
            Expr::FunctionCall { name, .. } => {
                if self.functions.contains_key(name) {
                    Ok(x_ast::Type::Unknown)
                } else {
                    Err(format!(
                        "Cannot infer return type of unknown function '{}'",
                        name
                    ))
                }
            }
            _ => Err(format!(
                "Could not infer type for expression {:?}. Please add a type annotation.",
                expr
            )),
        }
    }

    pub fn check_and_rewrite_statement(
        &mut self,
        stmt: &mut Statement,
        gen_structs: &HashMap<String, StructDef>,
        gen_funcs: &HashMap<
            String,
            (
                Vec<(String, Type)>,
                Type,
                Box<Vec<Statement>>,
                bool,
                bool,
                Vec<String>,
            ),
        >,
        concrete_statements: &mut Vec<Statement>,
    ) -> Result<(), String> {
        match stmt {
            Statement::VariableDecl {
                name,
                type_ann,
                value,
            } => {
                // First, rewrite the expression on the right-hand side.
                self.check_and_rewrite_expr(value, gen_structs, gen_funcs, concrete_statements)?;

                // Now, add the new variable's type to our pre-pass scope.
                if let Some(ann) = type_ann {
                    *type_ann = Some(self.resolve_type(ann, gen_structs, concrete_statements)?);
                    // Insert the resolved type into the scope for subsequent statements to see.
                    self.monomorph_scope
                        .insert(name.clone(), type_ann.as_ref().unwrap().clone());
                }
            }

            Statement::Function { body, .. } => {
                // Entering a new function, so create a new lexical scope.
                let saved_scope = self.monomorph_scope.clone();
                self.monomorph_scope.clear();

                for s in body.as_mut().unwrap().iter_mut() {
                    self.check_and_rewrite_statement(
                        s,
                        gen_structs,
                        gen_funcs,
                        concrete_statements,
                    )?;
                }

                // Exiting the function, so restore the parent scope.
                self.monomorph_scope = saved_scope;
            }
            Statement::Expression { expr } => {
                self.check_and_rewrite_expr(expr, gen_structs, gen_funcs, concrete_statements)?;
            }
            _ => {}
        }
        Ok(())
    }
}
