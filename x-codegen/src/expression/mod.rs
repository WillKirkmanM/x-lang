use crate::CodeGen;
use inkwell::{
    module::Linkage,
    values::{BasicMetadataValueEnum, BasicValueEnum, FloatValue, FunctionValue},
    AddressSpace, FloatPredicate,
};
use x_ast::{Expr, Operator, StringLiteral, StringPart};

impl<'ctx> CodeGen<'ctx> {
    pub(crate) fn gen_expr(&mut self, expr: &Expr) -> Result<BasicValueEnum<'ctx>, String> {
        match expr {
            Expr::Number(n) => Ok(self.context.f64_type().const_float(*n as f64).into()),
            Expr::Identifier(name) => {
                if let Some(ptr) = self.variables.get(name) {
                    Ok(self
                        .builder
                        .build_load(self.context.f64_type(), *ptr, name)
                        .map_err(|e| e.to_string())?
                        .into())
                } else {
                    Err(format!("Undefined variable: {}", name))
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
                            let array_ptr = self.gen_expr(array)?;
                            let index_val = self.gen_expr(index)?.into_float_value();
                            let value_val = self.gen_expr(right)?;

                            let index_int = self
                                .builder
                                .build_float_to_unsigned_int(
                                    index_val,
                                    self.context.i32_type(),
                                    "array_idx",
                                )
                                .map_err(|e| e.to_string())?;

                            let gep = unsafe {
                                self.builder
                                    .build_in_bounds_gep(
                                        self.context.f64_type().array_type(100),
                                        array_ptr.into_pointer_value(),
                                        &[self.context.i32_type().const_zero(), index_int],
                                        "array_access",
                                    )
                                    .map_err(|e| e.to_string())?
                            };

                            self.builder
                                .build_store(gep, value_val)
                                .map_err(|e| e.to_string())?;

                            return Ok(value_val);
                        }
                        Expr::FieldAccess { object: _, field: _ } => {
                            let rhs_val = self.gen_expr(right)?.into_float_value();

                            fn get_base_object_and_field(expr: &Expr) -> Option<(String, String)> {
                                match expr {
                                    Expr::FieldAccess { object, field } => {
                                        match object.as_ref() {
                                            Expr::Identifier(name) => {
                                                Some((name.clone(), field.clone()))
                                            }
                                            Expr::FieldAccess {
                                                object: inner_obj,
                                                field: _,
                                            } => {
                                                match inner_obj.as_ref() {
                                                    Expr::Identifier(name) => {
                                                        Some((name.clone(), field.clone()))
                                                    }
                                                    _ => None,
                                                }
                                            }
                                            _ => None,
                                        }
                                    }
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
                    Operator::Assign => unreachable!("Assignment should have been handled already"),
                }?;

                Ok(result.into())
            }
            Expr::FunctionCall { name, args } => match name.as_str() {
                "print" => {
                    if let Some(Expr::String(string_literal)) = args.first() {
                        self.gen_print_str(string_literal).map(|v| v.into())
                    } else {
                        self.gen_print(args).map(|v| v.into())
                    }
                }
                _ => self.gen_function_call(name, args).map(|v| v.into()),
            },
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
                let lambda_name = format!("lambda_{}", self.get_unique_id());

                let function = self.compile_anonymous_function(params, body, &lambda_name)?;

                if let Some(binding_name) = self.current_binding_name.as_ref() {
                    self.register_function(binding_name.clone(), function);
                } else {
                    self.register_function(lambda_name.clone(), function);
                }

                let fn_ptr = self
                    .builder
                    .build_alloca(
                        self.context.ptr_type(AddressSpace::default()),
                        "anonymous_fn",
                    )
                    .map_err(|e| e.to_string())?;

                self.builder
                    .build_store(fn_ptr, function.as_global_value().as_pointer_value())
                    .map_err(|e| e.to_string())?;

                Ok(fn_ptr.into())
            }
            Expr::Array(elements) => self.gen_array(elements),
            Expr::ArrayAccess { array, index } => self.gen_array_access(array, index),
            Expr::Assignment { target, value } => self.gen_assignment(target, value),
            Expr::StructInstantiate(struct_init) => self.gen_struct_instantiate(struct_init),
            Expr::FieldAccess { object, field } => self.gen_field_access(object, field),
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
                        let str_val = self
                            .builder
                            .build_global_string_ptr(&str_lit.parts[0].to_string(), "str_const")
                            .map_err(|e| e.to_string())?;
                        args.push(str_val.as_pointer_value().into());
                    }
                    Expr::Identifier(name) => {
                        if let Some(ptr) = self.variables.get(name) {
                            if name == "name" {
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
                                fmt_str.push_str("%f");
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
                        fmt_str.push_str("%f");
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
        printf_args.extend(
            args.iter()
                .map(|&arg| -> BasicMetadataValueEnum<'ctx> { arg.into() }),
        );

        self.builder
            .build_call(printf, &printf_args, "printf_call")
            .map_err(|e| e.to_string())?;

        Ok(self.context.f64_type().const_float(0.0))
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

    fn gen_array(&mut self, elements: &[Expr]) -> Result<BasicValueEnum<'ctx>, String> {
        let f64_type = self.context.f64_type();
        let array_type = f64_type.array_type(elements.len() as u32);

        let alloca = self
            .builder
            .build_alloca(array_type, "array_alloca")
            .map_err(|e| e.to_string())?;

        for (i, element) in elements.iter().enumerate() {
            let value = self.gen_expr(element)?.into_float_value();

            let element_ptr = unsafe {
                self.builder
                    .build_in_bounds_gep(
                        array_type,
                        alloca,
                        &[
                            self.context.i32_type().const_zero(),
                            self.context.i32_type().const_int(i as u64, false),
                        ],
                        &format!("array_elem_ptr_{}", i),
                    )
                    .map_err(|e| e.to_string())?
            };

            self.builder
                .build_store(element_ptr, value)
                .map_err(|e| e.to_string())?;
        }

        Ok(alloca.into())
    }

    pub(crate) fn gen_array_access(
        &mut self,
        array: &Box<Expr>,
        index: &Box<Expr>,
    ) -> Result<BasicValueEnum<'ctx>, String> {
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

        let index_val = self.gen_expr(index)?.into_float_value();

        let index_int = self
            .builder
            .build_float_to_unsigned_int(index_val, self.context.i32_type(), "array_idx")
            .map_err(|e| e.to_string())?;

        let array_ptr_val = self
            .builder
            .build_load(
                self.context.ptr_type(AddressSpace::default()),
                array_ptr,
                "array_ptr",
            )
            .map_err(|e| e.to_string())?
            .into_pointer_value();

        let gep = unsafe {
            self.builder
                .build_in_bounds_gep(
                    self.context.f64_type(),
                    array_ptr_val,
                    &[index_int],
                    "array_access",
                )
                .map_err(|e| e.to_string())?
        };

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
            let array_ptr = self.gen_expr(array)?;
            let index_val = self.gen_expr(index)?.into_float_value();
            let value_val = self.gen_expr(value)?;

            let index_int = self
                .builder
                .build_float_to_unsigned_int(index_val, self.context.i32_type(), "array_idx")
                .map_err(|e| e.to_string())?;

            let gep = unsafe {
                self.builder
                    .build_in_bounds_gep(
                        self.context.f64_type().array_type(100),
                        array_ptr.into_pointer_value(),
                        &[self.context.i32_type().const_zero(), index_int],
                        "array_access",
                    )
                    .map_err(|e| e.to_string())?
            };

            self.builder
                .build_store(gep, value_val)
                .map_err(|e| e.to_string())?;

            return Ok(value_val);
        }

        Err("Invalid assignment target".to_string())
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
            } => {
                self.gen_field_access(inner_obj, field)
            }
            _ => Err(
                "Field access is only supported on identifiers or other field accesses".to_string(),
            ),
        }
    }
}
