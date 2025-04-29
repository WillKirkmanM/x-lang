use crate::CodeGen;
use inkwell::{
    module::Linkage,
    types::{BasicType, BasicTypeEnum},
    values::{BasicMetadataValueEnum, BasicValueEnum, FloatValue, FunctionValue},
    AddressSpace, FloatPredicate,
};
use x_ast::{Expr, Operator, StringLiteral, StringPart, UnaryOperator};

impl<'ctx> CodeGen<'ctx> {
    pub(crate) fn gen_expr(&mut self, expr: &Expr) -> Result<BasicValueEnum<'ctx>, String> {
        match expr {
            Expr::Number(n) => Ok(self.context.f64_type().const_float(*n as f64).into()),
            Expr::Identifier(name) => {
                if let Some(ptr) = self.variables.get(name) {
                    let default_type = "f64".to_string();
                    let var_type_name = self.variable_types.get(name).unwrap_or(&default_type);

                    let var_llvm_type = self.map_type(var_type_name)?;

                    println!(
                        "[CodeGen] Loading identifier '{}' with type {:?} ({})",
                        name, var_llvm_type, var_type_name
                    );

                    let loaded_val = self
                        .builder
                        .build_load(var_llvm_type, *ptr, name)
                        .map_err(|e| format!("Failed to load variable {}: {}", name, e))?;

                    Ok(loaded_val)
                } else if let Some(func) = self.module.get_function(name) {
                    Ok(func.as_global_value().as_pointer_value().into())
                } else if let Some(func) = self.functions.get(name) {
                    Ok(func.as_global_value().as_pointer_value().into())
                } else {
                    Err(format!("Undefined variable or function: {}", name))
                }
            }
            Expr::BinaryOp { left, op, right } => {
                if let Operator::Add = op {
                    if let Expr::BinaryOp {
                        left: inner_left,
                        op: inner_op,
                        right: inner_right,
                    } = left.as_ref()
                    {
                        if let Operator::Assign = inner_op {
                            if let Expr::Identifier(name) = inner_left.as_ref() {
                                if let Expr::Identifier(right_name) = inner_right.as_ref() {
                                    if name == right_name {
                                        if let Some(&ptr) = self.variables.get(name) {
                                            let var_val = self
                                                .builder
                                                .build_load(self.context.f64_type(), ptr, name)
                                                .map_err(|e| e.to_string())?
                                                .into_float_value();

                                            let rhs = self.gen_expr(right)?.into_float_value();
                                            let add_result = self
                                                .builder
                                                .build_float_add(var_val, rhs, "addtmp")
                                                .map_err(|e| e.to_string())?;

                                            self.builder
                                                .build_store(ptr, add_result)
                                                .map_err(|e| e.to_string())?;

                                            return Ok(add_result.into());
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                if let Operator::Assign = op {
                    match left.as_ref() {
                        Expr::Identifier(name) => {
                            if let Some(&ptr) = self.variables.get(name) {
                                let rhs_val = self.gen_expr(right)?;
                                self.builder
                                    .build_store(ptr, rhs_val)
                                    .map_err(|e| e.to_string())?;
                                return Ok(rhs_val);
                            } else {
                                return Err(format!(
                                    "Cannot assign to undefined variable: {}",
                                    name
                                ));
                            }
                        }
                        Expr::ArrayAccess { array, index } => {
                            let value_val = self.gen_expr(right)?;

                            let gep = self.get_array_element_ptr(array, index)?;

                            self.builder
                                .build_store(gep, value_val)
                                .map_err(|e| e.to_string())?;

                            return Ok(value_val);
                        }
                        Expr::FieldAccess {
                            object: _,
                            field: _,
                        } => {
                            let rhs_val = self.gen_expr(right)?.into_float_value();

                            fn get_base_object_and_field(expr: &Expr) -> Option<(String, String)> {
                                match expr {
                                    Expr::FieldAccess { object, field } => match object.as_ref() {
                                        Expr::Identifier(name) => {
                                            Some((name.clone(), field.clone()))
                                        }
                                        Expr::FieldAccess {
                                            object: inner_obj,
                                            field: _,
                                        } => match inner_obj.as_ref() {
                                            Expr::Identifier(name) => {
                                                Some((name.clone(), field.clone()))
                                            }
                                            _ => None,
                                        },
                                        _ => None,
                                    },
                                    _ => None,
                                }
                            }

                            if let Some((obj_name, field_name)) = get_base_object_and_field(left) {
                                if let Some(&ptr) = self.variables.get(&obj_name) {
                                    let struct_name = match self.variable_types.get(&obj_name) {
                                        Some(t) => t.clone(),
                                        None => {
                                            return Err(format!(
                                                "Variable {} is not a struct",
                                                obj_name
                                            ))
                                        }
                                    };

                                    let (struct_type, field_names) =
                                        match self.struct_types.get(&struct_name) {
                                            Some(t) => t,
                                            None => {
                                                return Err(format!(
                                                    "Unknown struct type: {}",
                                                    struct_name
                                                ))
                                            }
                                        };

                                    let field_index =
                                        match field_names.iter().position(|f| f == &field_name) {
                                            Some(idx) => idx as u32,
                                            None => {
                                                return Err(format!(
                                                    "Unknown field '{}' in struct '{}'",
                                                    field_name, struct_name
                                                ))
                                            }
                                        };

                                    let struct_ptr_val = self
                                        .builder
                                        .build_load(
                                            self.context.ptr_type(AddressSpace::default()),
                                            ptr,
                                            "struct_ptr",
                                        )
                                        .map_err(|e| e.to_string())?
                                        .into_pointer_value();

                                    let field_ptr = {
                                        self.builder
                                            .build_struct_gep(
                                                *struct_type,
                                                struct_ptr_val,
                                                field_index,
                                                &format!("{}.{}", obj_name, field_name),
                                            )
                                            .map_err(|e| e.to_string())?
                                    };

                                    self.builder
                                        .build_store(field_ptr, rhs_val)
                                        .map_err(|e| e.to_string())?;

                                    return Ok(rhs_val.into());
                                } else {
                                    return Err(format!("Unknown variable: {}", obj_name));
                                }
                            } else {
                                return Err(
                                    "Field assignment requires a valid struct field".to_string()
                                );
                            }
                        }
                        _ => return Err("Invalid assignment target".to_string()),
                    }
                }

                let lhs = self.gen_expr(left)?.into_float_value();
                let rhs = self.gen_expr(right)?.into_float_value();

                let result = match op {
                    Operator::Add => self
                        .builder
                        .build_float_add(lhs, rhs, "addtmp")
                        .map_err(|e| e.to_string()),
                    Operator::Subtract => self
                        .builder
                        .build_float_sub(lhs, rhs, "subtmp")
                        .map_err(|e| e.to_string()),
                    Operator::Multiply => self
                        .builder
                        .build_float_mul(lhs, rhs, "multmp")
                        .map_err(|e| e.to_string()),
                    Operator::Divide => self
                        .builder
                        .build_float_div(lhs, rhs, "divtmp")
                        .map_err(|e| e.to_string()),
                    Operator::LessThan => self.gen_comparison(FloatPredicate::OLT, lhs, rhs),
                    Operator::GreaterThan => self.gen_comparison(FloatPredicate::OGT, lhs, rhs),
                    Operator::LessThanOrEqual => self.gen_comparison(FloatPredicate::OLE, lhs, rhs),
                    Operator::GreaterThanOrEqual => {
                        self.gen_comparison(FloatPredicate::OGE, lhs, rhs)
                    }
                    Operator::Equal => self.gen_comparison(FloatPredicate::OEQ, lhs, rhs),
                    Operator::NotEqual => self.gen_comparison(FloatPredicate::ONE, lhs, rhs),
                    Operator::Or => {
                        let entry_block = self.builder.get_insert_block().unwrap();
                        let function = entry_block.get_parent().unwrap();
                        let rhs_block = self.context.append_basic_block(function, "or_rhs");
                        let merge_block = self.context.append_basic_block(function, "or_merge");

                        let lhs_bool = self
                            .builder
                            .build_float_compare(
                                FloatPredicate::ONE,
                                lhs,
                                self.context.f64_type().const_float(0.0),
                                "lhs_bool",
                            )
                            .map_err(|e| e.to_string())?;

                        self.builder
                            .build_conditional_branch(lhs_bool, merge_block, rhs_block)
                            .map_err(|e| e.to_string())?;

                        self.builder.position_at_end(rhs_block);
                        let rhs_val = self.gen_expr(right)?.into_float_value();
                        let rhs_bool = self
                            .builder
                            .build_float_compare(
                                FloatPredicate::ONE,
                                rhs_val,
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

                        self.builder
                            .build_unsigned_int_to_float(
                                phi.as_basic_value().into_int_value(),
                                self.context.f64_type(),
                                "bool_to_float",
                            )
                            .map_err(|e| e.to_string())
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
                                lhs,
                                self.context.f64_type().const_float(0.0),
                                "lhs_bool",
                            )
                            .map_err(|e| e.to_string())?;

                        self.builder
                            .build_conditional_branch(lhs_bool, rhs_block, merge_block)
                            .map_err(|e| e.to_string())?;

                        self.builder.position_at_end(rhs_block);
                        let rhs_val = self.gen_expr(right)?.into_float_value();
                        let rhs_bool = self
                            .builder
                            .build_float_compare(
                                FloatPredicate::ONE,
                                rhs_val,
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

                        self.builder
                            .build_unsigned_int_to_float(
                                phi.as_basic_value().into_int_value(),
                                self.context.f64_type(),
                                "bool_to_float",
                            )
                            .map_err(|e| e.to_string())
                    }
                    Operator::Assign => unreachable!("Assignment should have been handled already"),
                }?;

                Ok(result.into())
            }
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
            Expr::AnonymousFunction { params, body } => {
                let closure_name = self.get_unique_id();

                let param_types: Vec<BasicTypeEnum<'ctx>> = params
                    .iter()
                    .map(|_| self.context.f64_type().into())
                    .collect();

                let param_metadata_types: Vec<inkwell::types::BasicMetadataTypeEnum<'ctx>> =
                    param_types.iter().map(|&ty| ty.into()).collect();

                let fn_type = self
                    .context
                    .f64_type()
                    .fn_type(&param_metadata_types, false);

                let closure_name_str = format!("closure_{}", closure_name);
                let function = self.module.add_function(&closure_name_str, fn_type, None);
                let entry_block = self.context.append_basic_block(function, "entry");

                let old_builder_pos = self.builder.get_insert_block();
                let old_vars = self.variables.clone();
                let old_var_types = self.variable_types.clone();

                self.builder.position_at_end(entry_block);
                self.variables.clear();
                self.variable_types.clear();

                for (i, param_name) in params.iter().enumerate() {
                    let param_value = function.get_nth_param(i as u32).unwrap();
                    let param_type = param_types[i];
                    param_value.set_name(param_name);

                    let alloca = self.builder.build_alloca(param_type, param_name).unwrap();
                    self.builder.build_store(alloca, param_value).unwrap();

                    self.variables.insert(param_name.clone(), alloca);
                    self.variable_types
                        .insert(param_name.clone(), "f64".to_string());
                    println!(
                        "[CodeGen] Closure Param '{}' type: {:?} stored as 'f64'",
                        param_name, param_type
                    );
                }

                let mut last_val: Option<BasicValueEnum<'ctx>> = None;
                for stmt in body {
                    last_val = self.gen_statement(stmt)?;
                    if self
                        .builder
                        .get_insert_block()
                        .and_then(|bb| bb.get_terminator())
                        .is_some()
                    {
                        break;
                    }
                }

                if !self
                    .builder
                    .get_insert_block()
                    .and_then(|bb| bb.get_terminator())
                    .is_some()
                {
                    match (last_val, fn_type.get_return_type()) {
                        (Some(val), Some(expected_ret_type)) => {
                            if val.get_type() == expected_ret_type {
                                self.builder.build_return(Some(&val)).unwrap();
                            } else {
                                eprintln!("[CodeGen Warning] Closure return type mismatch. Expected {:?}, got {:?}. Attempting bitcast.", expected_ret_type, val.get_type());
                                let basic_expected_type =
                                    BasicTypeEnum::try_from(expected_ret_type).unwrap();
                                let casted_val = self
                                    .builder
                                    .build_bit_cast(
                                        val,
                                        basic_expected_type,
                                        "unsafe_closure_ret_cast",
                                    )
                                    .unwrap();
                                self.builder.build_return(Some(&casted_val)).unwrap();
                            }
                        }
                        (Some(_), None) => {
                            self.builder.build_return(None).unwrap();
                        }
                        (None, Some(_)) => {
                            return Err(format!(
                                "Closure '{}' must return a value.",
                                closure_name_str
                            ));
                        }
                        (None, None) => {
                            self.builder.build_return(None).unwrap();
                        }
                    }
                }

                self.variables = old_vars;
                self.variable_types = old_var_types;
                if let Some(bb) = old_builder_pos {
                    self.builder.position_at_end(bb);
                } else {
                }

                Ok(function.as_global_value().as_pointer_value().into())
            }
            Expr::Array(elements) => {
                if elements.is_empty() {
                    let f64_ptr_type = self.context.ptr_type(AddressSpace::default());
                    return Ok(f64_ptr_type.const_null().into());
                }

                let first_val = self.gen_expr(&elements[0])?;
                let element_type = first_val.get_type();
                let array_type = element_type.array_type(elements.len() as u32);

                let array_alloca = self
                    .builder
                    .build_alloca(array_type, "array_literal")
                    .unwrap();

                for (i, elem_expr) in elements.iter().enumerate() {
                    let elem_val = self.gen_expr(elem_expr)?;
                    if elem_val.get_type() != element_type {
                        eprintln!("[CodeGen Warning] Array element type mismatch. Expected {:?}, got {:?}. Attempting bitcast.", element_type, elem_val.get_type());
                        let casted_val = self
                            .builder
                            .build_bit_cast(elem_val, element_type, "unsafe_arr_elem_cast")
                            .unwrap();
                        let index_val = self.context.i32_type().const_int(i as u64, false);
                        let elem_ptr = unsafe {
                            self.builder
                                .build_gep(
                                    array_type,
                                    array_alloca,
                                    &[self.context.i32_type().const_zero(), index_val],
                                    "elem_ptr",
                                )
                                .unwrap()
                        };
                        self.builder.build_store(elem_ptr, casted_val).unwrap();
                    } else {
                        let index_val = self.context.i32_type().const_int(i as u64, false);
                        let elem_ptr = unsafe {
                            self.builder
                                .build_gep(
                                    array_type,
                                    array_alloca,
                                    &[self.context.i32_type().const_zero(), index_val],
                                    "elem_ptr",
                                )
                                .unwrap()
                        };
                        self.builder.build_store(elem_ptr, elem_val).unwrap();
                    }
                }

                let first_elem_ptr = unsafe {
                    self.builder
                        .build_gep(
                            array_type,
                            array_alloca,
                            &[
                                self.context.i32_type().const_zero(),
                                self.context.i32_type().const_zero(),
                            ],
                            "array_decay",
                        )
                        .unwrap()
                };
                Ok(first_elem_ptr.into())
            }
            Expr::ArrayAccess { array, index } => self.gen_array_access(array, index),
            Expr::Assignment { target, value } => self.gen_assignment(target, value),
            Expr::StructInstantiate(struct_init) => self.gen_struct_instantiate(struct_init),
            Expr::FieldAccess { object, field } => self.gen_field_access(object, field),
            Expr::UnaryOp { op, expr } => match op {
                UnaryOperator::Negate => {
                    let val = self.gen_expr(expr)?.into_float_value();
                    Ok(self
                        .builder
                        .build_float_neg(val, "negtmp")
                        .map_err(|e| e.to_string())?
                        .into())
                }
                UnaryOperator::LogicalNot => {
                    let val = self.gen_expr(expr)?.into_float_value();
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
                        .build_unsigned_int_to_float(
                            is_zero,
                            self.context.f64_type(),
                            "bool_to_float",
                        )
                        .map_err(|e| e.to_string())?
                        .into())
                }
                UnaryOperator::BitwiseNot => {
                    let val = self.gen_expr(expr)?.into_float_value();
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
            },
        }
    }

    fn gen_comparison(
        &self,
        predicate: FloatPredicate,
        lhs: FloatValue<'ctx>,
        rhs: FloatValue<'ctx>,
    ) -> Result<FloatValue<'ctx>, String> {
        let cmp = self
            .builder
            .build_float_compare(predicate, lhs, rhs, "cmptmp")
            .map_err(|e| e.to_string())?;

        Ok(self
            .builder
            .build_unsigned_int_to_float(cmp, self.context.f64_type(), "booltmp")
            .map_err(|e| e.to_string())?)
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
                    Expr::Number(n) => {
                        fmt_str.push_str("%g");
                        args.push(self.context.f64_type().const_float(*n).into());
                    }
                    Expr::Identifier(name) => {
                        if let Some(ptr) = self.variables.get(name) {
                            if self
                                .variable_types
                                .get(name)
                                .map_or(false, |t| t == "string")
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
                .map_or(false, |t| t == "string")),
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

    pub(crate) fn gen_array_access(
        &mut self,
        array: &Box<Expr>,
        index: &Box<Expr>,
    ) -> Result<BasicValueEnum<'ctx>, String> {
        let gep = self.get_array_element_ptr(array, index)?;

        self.builder
            .build_load(self.context.f64_type(), gep, "array_element")
            .map_err(|e| e.to_string())
    }

    fn gen_assignment(
        &mut self,
        target: &Expr,
        value: &Expr,
    ) -> Result<BasicValueEnum<'ctx>, String> {
        if let Expr::Identifier(name) = target {
            let val = self.gen_expr(value)?;

            if let Some(ptr) = self.variables.get(name) {
                self.builder
                    .build_store(*ptr, val)
                    .map_err(|e| e.to_string())?;

                return Ok(val);
            } else {
                return Err(format!("Cannot assign to undefined variable: {}", name));
            }
        }

        if let Expr::ArrayAccess { array, index } = target {
            let gep = self.get_array_element_ptr(array, index)?;
            let value_val = self.gen_expr(value)?;

            self.builder
                .build_store(gep, value_val)
                .map_err(|e| e.to_string())?;

            return Ok(value_val);
        }

        Err("Invalid assignment target".to_string())
    }

    fn get_array_element_ptr(
        &mut self,
        array: &Box<Expr>,
        index: &Box<Expr>,
    ) -> Result<inkwell::values::PointerValue<'ctx>, String> {
        fn get_base_array(expr: &Expr) -> Option<String> {
            match expr {
                Expr::Identifier(name) => Some(name.clone()),
                Expr::ArrayAccess { array, index: _ } => get_base_array(array),
                _ => None,
            }
        }

        let array_name = if let Some(name) = get_base_array(array) {
            name
        } else {
            return Err("Array access only supports identifier arrays for now".to_string());
        };

        let array_ptr = if let Some(&ptr) = self.variables.get(&array_name) {
            ptr
        } else {
            return Err(format!("Unknown array variable: {}", array_name));
        };

        let index_val = self.gen_expr(index)?;

        let float_val = index_val.into_float_value();

        let index_int = self
            .builder
            .build_float_to_unsigned_int(float_val, self.context.i32_type(), "array_idx")
            .map_err(|e| e.to_string())?;

        let array_ptr_val = self
            .builder
            .build_load(
                self.context.ptr_type(AddressSpace::default()),
                array_ptr,
                "array_ptr",
            )
            .map_err(|e| e.to_string())?;

        let array_ptr_val = array_ptr_val.into_pointer_value();

        let result = unsafe {
            self.builder
                .build_in_bounds_gep(
                    self.context.f64_type(),
                    array_ptr_val,
                    &[index_int],
                    "array_access",
                )
                .map_err(|e| e.to_string())
        };

        result
    }

    pub(crate) fn gen_field_access(
        &mut self,
        object: &Box<x_ast::Expr>,
        field: &str,
    ) -> Result<BasicValueEnum<'ctx>, String> {
        match &**object {
            x_ast::Expr::Identifier(name) => {
                if let Some(&ptr) = self.variables.get(name) {
                    let struct_name = match self.variable_types.get(name) {
                        Some(t) => t.clone(),
                        None => return Err(format!("Variable {} is not a struct", name)),
                    };

                    let (struct_type, field_names) = match self.struct_types.get(&struct_name) {
                        Some(t) => t,
                        None => return Err(format!("Unknown struct type: {}", struct_name)),
                    };

                    let field_index = match field_names.iter().position(|f| f == field) {
                        Some(idx) => idx as u32,
                        None => {
                            return Err(format!(
                                "Unknown field '{}' in struct '{}'",
                                field, struct_name
                            ))
                        }
                    };

                    let struct_ptr_val = self
                        .builder
                        .build_load(
                            self.context.ptr_type(AddressSpace::default()),
                            ptr,
                            "struct_ptr",
                        )
                        .map_err(|e| e.to_string())?
                        .into_pointer_value();

                    let field_ptr = {
                        self.builder
                            .build_struct_gep(
                                *struct_type,
                                struct_ptr_val,
                                field_index,
                                &format!("{}.{}", name, field),
                            )
                            .map_err(|e| e.to_string())?
                    };

                    self.builder
                        .build_load(self.context.f64_type(), field_ptr, field)
                        .map_err(|e| e.to_string())
                } else {
                    Err(format!("Unknown variable: {}", name))
                }
            }
            x_ast::Expr::FieldAccess {
                object: inner_obj,
                field: _inner_field,
            } => self.gen_field_access(inner_obj, field),
            _ => Err(
                "Field access is only supported on identifiers or other field accesses".to_string(),
            ),
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
}
