use std::collections::HashMap;

use crate::CodeGen;
use inkwell::{
    module::Linkage,
    values::{BasicMetadataValueEnum, BasicValueEnum, FloatValue, FunctionValue},
    AddressSpace,
};
use x_ast::{Expr, Statement, StringLiteral, StringPart, StructDef, Type};

mod array;
mod binary;
mod function;
mod unary;

use x_logging::debug;

impl<'ctx> CodeGen<'ctx> {
    pub(crate) fn gen_expr(&mut self, expr: &Expr) -> Result<BasicValueEnum<'ctx>, String> {
        match expr {
            Expr::Int(n) => {
                let i32_type = self.context.i32_type();
                Ok(i32_type.const_int(*n as u64, true).into())
            }
            Expr::Float(n) => {
                let f64_type = self.context.f64_type();
                Ok(f64_type.const_float(*n).into())
            }
            Expr::Boolean(b) => {
                let bv = self
                    .context
                    .bool_type()
                    .const_int(if *b { 1 } else { 0 }, false);
                Ok(bv.into())
            }
            Expr::Identifier(name) => {
                if let Some(&ptr_val) = self.variables.get(name) {
                    let var_ast_type = self
                        .variable_types
                        .get(name)
                        .ok_or_else(|| format!("Type for variable '{}' not found.", name))?
                        .clone();

                    debug!(
                        identifier = %name,
                        ast_type = %var_ast_type,
                        "Loading identifier"
                    );

                    // Differentiate between handling value types and reference types.
                    if matches!(var_ast_type, Type::Ref { .. }) {
                        // If the variable is a reference (like `arr` in bubble_sort),
                        // its value is the pointer stored in its alloca. We need to load that pointer.
                        let llvm_ref_type = self.map_ast_type_to_llvm(&var_ast_type);
                        let loaded_ptr = self
                            .builder
                            .build_load(llvm_ref_type, ptr_val, &format!("{}_ref", name))
                            .map_err(|e| e.to_string())?;
                        Ok(loaded_ptr)
                    } else {
                        // If it's a value type (Int, Float, Array, Struct), load the value from its pointer.
                        let var_llvm_type = self.map_ast_type_to_llvm(&var_ast_type);
                        let loaded_val = self
                            .builder
                            .build_load(var_llvm_type, ptr_val, name)
                            .map_err(|e| format!("Failed to load variable {}: {}", name, e))?;
                        Ok(loaded_val)
                    }
                } else if let Some(func) = self.module.get_function(name) {
                    Ok(func.as_global_value().as_pointer_value().into())
                } else if let Some(func) = self.functions.get(name) {
                    Ok(func.as_global_value().as_pointer_value().into())
                } else {
                    Err(format!("Undefined variable or function: {}", name))
                }
            }
            Expr::BinaryOp { left, op, right } => self.handle_binary_op(left, op.clone(), right),
            Expr::FunctionCall { name, args } => self.gen_function_call(name, args),
            Expr::String(str_lit) => {
                let s = str_lit
                    .parts
                    .iter()
                    .map(|p| p.to_string())
                    .collect::<String>();
                let global_str = self
                    .builder
                    .build_global_string_ptr(&s, "str_const")
                    .map_err(|e| e.to_string())?;
                Ok(global_str.as_pointer_value().into())
            }
            Expr::AnonymousFunction {
                params,
                body,
                return_type: _return_type,
            } => self.handle_anonymous_function(params, body),
            Expr::Array(elements) => self.handle_array_literal(elements),
            Expr::ArrayAccess { array, index } => self.gen_array_access(array, index),
            Expr::Assignment { target, value } => self.gen_assignment(target, value),
            Expr::StructInstantiate(struct_init) => self.gen_struct_instantiate(struct_init),
            Expr::FieldAccess { object, field } => self.gen_field_access(object, field),
            Expr::UnaryOp { op, expr } => self.handle_unary_op(op.clone(), expr),
            Expr::TypeLiteral(_) => {
                Err("Type literal cannot be used as a runtime expression".to_string())
            }
            Expr::AddressOf {
                is_mut: _is_mut,
                expr,
            } => match &**expr {
                Expr::Identifier(name) => {
                    if let Some(&ptr) = self.variables.get(name) {
                        Ok(ptr.into())
                    } else {
                        Err(format!(
                            "Cannot take address of undefined variable: {}",
                            name
                        ))
                    }
                }
                _ => Err("Address-of is currently only supported for identifiers".to_string()),
            },
            Expr::Deref { expr } => {
                let val = self.gen_expr(expr)?;
                let ptr = val.into_pointer_value();

                self.builder
                    .build_load(self.context.f64_type(), ptr, "deref")
                    .map_err(|e| e.to_string())
            }
        }
    }

    pub fn get_printf_fn(&self) -> Result<FunctionValue<'ctx>, String> {
        if let Some(func) = self.module.get_function("printf") {
            Ok(func)
        } else {
            let printf_type = self.context.i32_type().fn_type(
                &[self.context.ptr_type(AddressSpace::default()).into()],
                true,
            );

            let printf = self
                .module
                .add_function("printf", printf_type, Some(Linkage::External));
            Ok(printf)
        }
    }

    pub fn gen_print_str(&mut self, string: &StringLiteral) -> Result<FloatValue<'ctx>, String> {
        let printf = self.get_printf_fn()?;
        let mut fmt_str = String::new();
        let mut args: Vec<BasicValueEnum<'ctx>> = Vec::new();

        for part in &string.parts {
            match part {
                StringPart::Text(text) => {
                    let escaped = text.replace("%", "%%");
                    fmt_str.push_str(&escaped);
                }
                StringPart::Interpolation(expr) => match expr.as_ref() {
                    Expr::String(str_lit) => {
                        fmt_str.push_str("%s");
                        let full_string = str_lit
                            .parts
                            .iter()
                            .map(|p| p.to_string())
                            .collect::<String>();

                        let str_val = self
                            .builder
                            .build_global_string_ptr(&full_string, "str_const")
                            .map_err(|e| e.to_string())?;
                        args.push(str_val.as_pointer_value().into());
                    }
                    Expr::Float(n) => {
                        fmt_str.push_str("%g");
                        args.push(self.context.i64_type().const_int(*n as u64, true).into());
                    }
                    Expr::Int(n) => {
                        fmt_str.push_str("%g");
                        args.push(self.context.f64_type().const_float(*n as f64).into());
                    }

                    Expr::Identifier(name) => {
                        if let Some(ptr) = self.variables.get(name) {
                            if self
                                .variable_types
                                .get(name)
                                .map_or(false, |t| *t == x_ast::Type::String)
                            {
                                fmt_str.push_str("%s");

                                let val = self
                                    .builder
                                    .build_load(
                                        self.context.ptr_type(AddressSpace::default()),
                                        *ptr,
                                        name,
                                    )
                                    .map_err(|e| e.to_string())?;
                                args.push(val);
                            } else {
                                fmt_str.push_str("%g");
                                let val = self
                                    .builder
                                    .build_load(self.context.f64_type(), *ptr, name)
                                    .map_err(|e| e.to_string())?;
                                args.push(val);
                            }
                        } else {
                            return Err(format!("Undefined variable in interpolation: {}", name));
                        }
                    }
                    _ => {
                        if self.is_string_expression(expr)? {
                            fmt_str.push_str("%s");
                        } else {
                            fmt_str.push_str("%g");
                        }
                        let val = self.gen_expr(expr)?;
                        args.push(val);
                    }
                },
            }
        }

        fmt_str.push_str("\n\0");

        let fmt_ptr = self
            .builder
            .build_global_string_ptr(&fmt_str, "fmt_str")
            .map_err(|e| e.to_string())?;

        let mut printf_args: Vec<BasicMetadataValueEnum<'ctx>> =
            vec![fmt_ptr.as_pointer_value().into()];
        printf_args.extend(args.iter().map(|arg| BasicMetadataValueEnum::from(*arg)));

        self.builder
            .build_call(printf, &printf_args, "printf_call")
            .map_err(|e| e.to_string())?;

        Ok(self.context.f64_type().const_float(0.0))
    }

    pub fn is_string_expression(&self, expr: &Expr) -> Result<bool, String> {
        match expr {
            Expr::String(_) => Ok(true),
            Expr::Identifier(name) => Ok(self
                .variable_types
                .get(name)
                .map_or(false, |t| *t == x_ast::Type::String)),
            _ => Ok(false),
        }
    }

    pub fn gen_print(&mut self, args: &[Expr]) -> Result<FloatValue<'ctx>, String> {
        if let Some(&print_fn) = self.imported_functions.get("print") {
            let compiled_args: Vec<_> = args
                .iter()
                .map(|arg| self.gen_expr(arg))
                .collect::<Result<Vec<_>, _>>()?
                .into_iter()
                .map(|val| val.into())
                .collect();

            Ok(self
                .builder
                .build_call(print_fn, &compiled_args, "calltmp")
                .map_err(|e| e.to_string())?
                .try_as_basic_value()
                .left()
                .unwrap()
                .into_float_value())
        } else {
            Err("print function not imported".to_string())
        }
    }

    fn gen_increment(
        &mut self,
        expr: &Expr,
        is_pre: bool,
        is_increment: bool,
    ) -> Result<BasicValueEnum<'ctx>, String> {
        if let Expr::Identifier(name) = expr {
            if let Some(&alloca) = self.variables.get(name) {
                let current_val = self
                    .builder
                    .build_load(self.context.f64_type(), alloca, name)
                    .map_err(|e| e.to_string())?
                    .into_float_value();

                let one = self.context.f64_type().const_float(1.0);
                let new_val = if is_increment {
                    self.builder.build_float_add(current_val, one, "inc_tmp")
                } else {
                    self.builder.build_float_sub(current_val, one, "dec_tmp")
                }
                .map_err(|e| e.to_string())?;

                self.builder
                    .build_store(alloca, new_val)
                    .map_err(|e| e.to_string())?;

                Ok(if is_pre {
                    new_val.into()
                } else {
                    current_val.into()
                })
            } else {
                Err(format!(
                    "Cannot increment/decrement undefined variable: {}",
                    name
                ))
            }
        } else {
            Err("Can only increment/decrement variables".to_string())
        }
    }

    pub fn check_and_rewrite_expr(
        &mut self,
        expr: &mut Expr,
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
        if let Expr::FunctionCall { name, args } = expr {
            if gen_funcs.contains_key(name) {
                let arg_types = args
                    .iter()
                    .map(|arg| self.get_expr_type(arg))
                    .collect::<Result<Vec<_>, _>>()?;

                if arg_types.len() != 1 {
                    return Err(
                        "This example only supports single-arg generic inference".to_string()
                    );
                }
                let arg_type = &arg_types[0];
                let concrete_type_args = match arg_type {
                    Type::Custom(mangled_name) => {
                        // Generic parsing for names likl "Pair_i32_bool"
                        let parts: Vec<&str> = mangled_name.split('_').collect();
                        if parts.len() < 2 {
                            return Err(format!(
                                "Cannot infer generic args from malformed name: {}",
                                mangled_name
                            ));
                        }

                        // Convert string type names ("i32", "bool") back to ast::Type variants.
                        let type_args_str = &parts[1..];
                        type_args_str
                            .iter()
                            .map(|&s| match s {
                                "i32" => Ok(Type::Int),
                                "bool" => Ok(Type::Bool),
                                "f64" => Ok(Type::Float),
                                // Add other primitive type mappings as needed
                                _ => Err(format!("Unknown type '{}' in mangled name", s)),
                            })
                            .collect::<Result<Vec<_>, _>>()?
                    }
                    _ => return Err("Cannot infer from non-custom type".to_string()),
                };

                let mangled_name = self.instantiate_function(
                    name,
                    &concrete_type_args,
                    gen_funcs,
                    gen_structs,
                    concrete_statements,
                )?;
                *name = mangled_name;
            }
        }
        Ok(())
    }
}
