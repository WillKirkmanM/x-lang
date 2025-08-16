use crate::CodeGen;
use inkwell::types::{AnyType, BasicTypeEnum};
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum, FunctionValue, PointerValue};
use x_ast::{Expr, Statement, StringLiteral, StringPart};

impl<'ctx> CodeGen<'ctx> {
    pub fn get_unique_id(&mut self) -> usize {
        use std::sync::atomic::{AtomicUsize, Ordering};
        static COUNTER: AtomicUsize = AtomicUsize::new(0);
        COUNTER.fetch_add(1, Ordering::Relaxed)
    }

    pub fn compile_anonymous_function(
        &mut self,
        params: &[String],
        body: &[Statement],
        original_name: &str,
    ) -> Result<FunctionValue<'ctx>, String> {
        let saved_vars = self.variables.clone();
        let saved_block = self.builder.get_insert_block();

        let fn_name = format!("anonymous_{}", self.get_unique_id());

        let param_types = vec![self.context.f64_type().into(); params.len()];
        let fn_type = self.context.f64_type().fn_type(&param_types, false);
        let function = self.module.add_function(&fn_name, fn_type, None);
        self.register_function(original_name.to_string(), function);

        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);

        self.variables.clear();
        for (i, param_name) in params.iter().enumerate() {
            let param = function.get_nth_param(i as u32).unwrap();
            let alloca = self
                .builder
                .build_alloca(self.context.f64_type(), param_name)
                .map_err(|e| e.to_string())?;
            self.builder
                .build_store(alloca, param)
                .map_err(|e| e.to_string())?;
            self.variables.insert(param_name.clone(), alloca);
        }

        let mut last_val = self.context.f64_type().const_float(0.0);
        for stmt in body {
            if let Some(val) = self.gen_statement(stmt)? {
                last_val = val.into_float_value();
            }
        }

        self.builder
            .build_return(Some(&last_val))
            .map_err(|e| e.to_string())?;

        self.variables = saved_vars;
        if let Some(block) = saved_block {
            self.builder.position_at_end(block);
        }

        Ok(function)
    }

    pub fn gen_function_call(
        &mut self,
        name: &str,
        args: &[Expr],
    ) -> Result<BasicValueEnum<'ctx>, String> {
        if name == "print" && args.len() == 1 {
            let arg_expr = &args[0];

            if let Expr::String(StringLiteral { parts }) = arg_expr {
                if parts.len() > 1
                    || parts
                        .iter()
                        .any(|p| matches!(p, StringPart::Interpolation(_)))
                {
                    let printf_fn = self
                        .module
                        .get_function("printf")
                        .ok_or("printf function not declared")?;

                    for part in parts {
                        match part {
                            StringPart::Text(text) => {
                                let global_str = self.get_or_create_global_string(text, "str_part");
                                let format_str = self.get_or_create_global_string("%s", "fmt_s");
                                self.builder
                                    .build_call(
                                        printf_fn,
                                        &[format_str.into(), global_str.into()],
                                        "printf_text",
                                    )
                                    .unwrap();
                            }
                            StringPart::Interpolation(expr) => {
                                let val = self.gen_expr(expr)?;
                                match val {
                                    BasicValueEnum::PointerValue(pv) => {
                                        let format_str =
                                            self.get_or_create_global_string("%s", "fmt_s");
                                        self.builder
                                            .build_call(
                                                printf_fn,
                                                &[format_str.into(), pv.into()],
                                                "printf_interp_s",
                                            )
                                            .unwrap();
                                    }
                                    BasicValueEnum::FloatValue(fv) => {
                                        let format_str =
                                            self.get_or_create_global_string("%.0f", "fmt_f");
                                        self.builder
                                            .build_call(
                                                printf_fn,
                                                &[format_str.into(), fv.into()],
                                                "printf_interp_f",
                                            )
                                            .unwrap();
                                    }
                                    BasicValueEnum::IntValue(iv) => {
                                        let format_str =
                                            self.get_or_create_global_string("%lld", "fmt_i");
                                        self.builder
                                            .build_call(
                                                printf_fn,
                                                &[format_str.into(), iv.into()],
                                                "printf_interp_i",
                                            )
                                            .unwrap();
                                    }
                                    _ => {
                                        return Err(format!(
                                            "Unsupported type for string interpolation: {:?}",
                                            val.get_type()
                                        ))
                                    }
                                }
                            }
                        }
                    }

                    let newline_str = self.get_or_create_global_string("\n", "fmt_nl");
                    self.builder
                        .build_call(printf_fn, &[newline_str.into()], "printf_nl")
                        .unwrap();

                    return Ok(self.context.f64_type().const_float(0.0).into());
                }
            }

            let val = self.gen_expr(arg_expr)?;
            let (target_fn_name, meta_arg): (&str, BasicMetadataValueEnum) = match val {
                BasicValueEnum::PointerValue(pv) => ("print_str", pv.into()),
                BasicValueEnum::FloatValue(fv) => ("print", fv.into()),
                BasicValueEnum::IntValue(iv) => {
                    let f64_type = self.context.f64_type();
                    let float_val = self
                        .builder
                        .build_signed_int_to_float(iv, f64_type, "print_i_to_f")
                        .unwrap();
                    ("print", float_val.into())
                }
                _ => {
                    return Err(format!(
                        "std::print called with unsupported argument type {:?}",
                        val.get_type()
                    ))
                }
            };
            let func = self
                .imported_functions
                .get(target_fn_name)
                .copied()
                .or_else(|| self.module.get_function(target_fn_name))
                .ok_or_else(|| {
                    format!(
                        "Standard library function '{}' not imported or found.",
                        target_fn_name
                    )
                })?;
            self.builder
                .build_call(func, &[meta_arg], &format!("calltmp_{}", target_fn_name))
                .unwrap();
            return Ok(self.context.f64_type().const_float(0.0).into());
        }

        let function_to_call: FunctionValue<'ctx>;
        let expected_param_types: Vec<BasicTypeEnum<'ctx>>;

        if let Some(var_ptr) = self.variables.get(name) {
            let f64_type = &"f64".to_string();
            let var_type_name = self.variable_types.get(name).unwrap_or(f64_type);
            let var_llvm_type = self.map_type(var_type_name)?;

            let loaded_val = self
                .builder
                .build_load(var_llvm_type, *var_ptr, &format!("load_{}", name))
                .unwrap();

            if loaded_val.is_pointer_value() {
                let loaded_ptr = loaded_val.into_pointer_value();
                if loaded_ptr.get_type().as_any_type_enum().is_function_type() {
                    let function_name = loaded_ptr.get_name().to_string_lossy().to_string();
                    function_to_call =
                        self.module.get_function(&function_name).ok_or_else(|| {
                            format!(
                                "Function pointer '{}' does not point to a known function.",
                                name
                            )
                        })?;

                    let params_metadata = function_to_call.get_type().get_param_types();

                    expected_param_types = params_metadata
                        .iter()
                        .map(|param| {
                            BasicTypeEnum::try_from(*param).map_err(|_| {
                                format!("Function '{}' has unsupported metadata parameters.", name)
                            })
                        })
                        .collect::<Result<Vec<_>, _>>()?;
                } else {
                    return Err(format!(
                        "Variable '{}' is a pointer but does not point to a function.",
                        name
                    ));
                }
            } else {
                return Err(format!(
                    "Variable '{}' is not a function or function pointer.",
                    name
                ));
            }
        } else if let Some(f) = self.functions.get(name) {
            function_to_call = *f;
            expected_param_types = function_to_call
                .get_type()
                .get_param_types()
                .into_iter()
                .map(|meta| {
                    BasicTypeEnum::try_from(meta).map_err(|_| {
                        format!("Function '{}' has unsupported metadata parameters.", name)
                    })
                })
                .collect::<Result<Vec<_>, _>>()?;
        } else if let Some(f) = self.imported_functions.get(name) {
            function_to_call = *f;
            expected_param_types = function_to_call
                .get_type()
                .get_param_types()
                .into_iter()
                .map(|meta| {
                    BasicTypeEnum::try_from(meta).map_err(|_| {
                        format!("Function '{}' has unsupported metadata parameters.", name)
                    })
                })
                .collect::<Result<Vec<_>, _>>()?;
        } else if let Some(f) = self.external_functions.get(name) {
            function_to_call = *f;
            expected_param_types = function_to_call
                .get_type()
                .get_param_types()
                .into_iter()
                .map(|meta| {
                    BasicTypeEnum::try_from(meta).map_err(|_| {
                        format!("Function '{}' has unsupported metadata parameters.", name)
                    })
                })
                .collect::<Result<Vec<_>, _>>()?;
        } else if let Some(f) = self.module.get_function(name) {
            function_to_call = f;
            expected_param_types = function_to_call
                .get_type()
                .get_param_types()
                .into_iter()
                .map(|meta| {
                    BasicTypeEnum::try_from(meta).map_err(|_| {
                        format!("Function '{}' has unsupported metadata parameters.", name)
                    })
                })
                .collect::<Result<Vec<_>, _>>()?;
        } else {
            return Err(format!(
                 "Unknown function or callable variable: '{}'. Available internal: {:?}, imported: {:?}, external: {:?}, module: {:?}, variables: {:?}",
                 name,
                 self.functions.keys().collect::<Vec<_>>(),
                 self.imported_functions.keys().collect::<Vec<_>>(),
                 self.external_functions.keys().collect::<Vec<_>>(),
                 self.module.get_functions().map(|f| f.get_name().to_str().unwrap_or("").to_string()).collect::<Vec<_>>(),
                 self.variables.keys().collect::<Vec<_>>()
             ));
        }

        if args.len() != expected_param_types.len() {
            return Err(format!(
                "Function/Callable '{}' called with {} arguments, but expected {}.",
                name,
                args.len(),
                expected_param_types.len()
            ));
        }

        let mut compiled_args: Vec<BasicMetadataValueEnum<'ctx>> = Vec::new();
        for (i, arg) in args.iter().enumerate() {
            let expected_type = expected_param_types[i];
            let arg_val = self.gen_expr(arg)?;

            let coerced_arg: BasicMetadataValueEnum<'ctx> = match (expected_type, arg_val) {
                (BasicTypeEnum::FloatType(et), BasicValueEnum::FloatValue(av))
                    if et == av.get_type() =>
                {
                    av.into()
                }
                (BasicTypeEnum::IntType(et), BasicValueEnum::IntValue(av))
                    if et == av.get_type() =>
                {
                    av.into()
                }
                (BasicTypeEnum::PointerType(et), BasicValueEnum::PointerValue(av))
                    if et == av.get_type() =>
                {
                    av.into()
                }
                (BasicTypeEnum::StructType(et), BasicValueEnum::StructValue(av))
                    if et == av.get_type() =>
                {
                    av.into()
                }

                (BasicTypeEnum::FloatType(et), BasicValueEnum::IntValue(av)) => self
                    .builder
                    .build_signed_int_to_float(av, et, "arg_i_to_f")
                    .map_err(|e| e.to_string())?
                    .into(),
                (BasicTypeEnum::IntType(et), BasicValueEnum::FloatValue(av)) => self
                    .builder
                    .build_float_to_signed_int(av, et, "arg_f_to_i")
                    .map_err(|e| e.to_string())?
                    .into(),
                (BasicTypeEnum::PointerType(et), BasicValueEnum::PointerValue(av)) => {
                    if et != av.get_type() {
                        self.builder
                            .build_pointer_cast(av, et, "arg_ptr_cast")
                            .map_err(|e| e.to_string())?
                            .into()
                    } else {
                        av.into()
                    }
                }
                (BasicTypeEnum::StructType(st), BasicValueEnum::PointerValue(pv))
                    if expected_type.is_struct_type() =>
                {
                    self.builder
                        .build_load(st, pv, "load_struct_arg")
                        .map_err(|e| e.to_string())?
                        .into()
                }

                _ if expected_type != arg_val.get_type() => {
                    eprintln!(
                        "[CodeGen Warning] Unsafe argument type coercion: Attempting bitcast for argument {} in call to '{}'. Expected {:?}, got {:?}.",
                        i, name, expected_type, arg_val.get_type()
                    );
                    self.builder
                        .build_bit_cast(arg_val, expected_type, "unsafe_arg_cast")
                        .map_err(|e| format!("Failed unsafe bitcast for argument {}: {}", i, e))?
                        .into()
                }
                _ => arg_val.into(),
            };
            compiled_args.push(coerced_arg);
        }

        let call_result = self
            .builder
            .build_call(function_to_call, &compiled_args, "calltmp")
            .map_err(|e| e.to_string())?;

        Ok(call_result
            .try_as_basic_value()
            .left()
            .unwrap_or_else(|| self.context.f64_type().const_float(0.0).into()))
    }

    fn get_or_create_global_string(&mut self, text: &str, name_prefix: &str) -> PointerValue<'ctx> {
        let unique_name = format!("{}_{}", name_prefix, self.get_unique_id());
        self.builder
            .build_global_string_ptr(text, &unique_name)
            .unwrap()
            .as_pointer_value()
    }

    pub fn compile_function(
        &mut self,
        name: &str,
        params: &[String],
        body: &[Statement],
        is_pure: bool,
        is_memoised: bool,
    ) -> Result<FunctionValue<'ctx>, String> {
        let function = self
            .module
            .get_function(name)
            .ok_or_else(|| format!("Function {} not declared before compilation.", name))?;

        if is_pure || !is_memoised {
            use inkwell::attributes::{Attribute, AttributeLoc};
            let readnone_attr_kind_id = Attribute::get_named_enum_kind_id("readnone");
            assert_ne!(readnone_attr_kind_id, 0);
            let readnone_attr = self.context.create_enum_attribute(readnone_attr_kind_id, 0);
            function.add_attribute(AttributeLoc::Function, readnone_attr);
        }

        let entry_block = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry_block);

        let old_vars = self.variables.clone();
        let old_var_types = self.variable_types.clone();
        let old_current_fn = self.current_function;
        self.variables.clear();
        self.variable_types.clear();
        self.current_function = Some(function);

        for (i, param_name) in params.iter().enumerate() {
            let param_value = function
                .get_nth_param(i as u32)
                .ok_or_else(|| format!("Missing parameter {} for function {}", i, name))?;
            let param_type = param_value.get_type();

            let alloca = self
                .builder
                .build_alloca(param_type, param_name)
                .map_err(|e| format!("Failed to alloca param {}: {}", param_name, e))?;

            self.builder
                .build_store(alloca, param_value)
                .map_err(|e| format!("Failed to store param {}: {}", param_name, e))?;

            self.variables.insert(param_name.clone(), alloca);

            let type_name_str = match param_type {
                BasicTypeEnum::FloatType(_) => "f64",
                BasicTypeEnum::PointerType(_) => "str",
                BasicTypeEnum::IntType(_) => "i32",
                BasicTypeEnum::StructType(st) => self
                    .struct_types
                    .iter()
                    .find(|(_, (llvm_st, _))| llvm_st == &st)
                    .map(|(name, _)| name.as_str())
                    .unwrap_or("unknown_struct"),
                _ => "unknown",
            };
            self.variable_types
                .insert(param_name.clone(), type_name_str.to_string());
            println!(
                "[CodeGen] Param '{}' type: {:?} stored as '{}'",
                param_name, param_type, type_name_str
            );
        }

        let mut last_val: Option<BasicValueEnum<'ctx>> = None;
        for stmt in body {
            if self
                .builder
                .get_insert_block()
                .and_then(|bb| bb.get_terminator())
                .is_some()
            {
                eprintln!("[CodeGen Warning] Unreachable code detected in function '{}' after a terminator.", name);
                break;
            }
            match self.gen_statement(stmt) {
                Ok(val_opt) => {
                    last_val = val_opt;
                }
                Err(e) => {
                    self.variables = old_vars;
                    self.variable_types = old_var_types;
                    self.current_function = old_current_fn;
                    return Err(format!("Error in body of function {}: {}", name, e));
                }
            }
        }

        println!("[CodeGen Debug] Building return for '{}'. Current block terminated: {:?}. Last value produced: {:?}",
                 name,
                 self.builder.get_insert_block().and_then(|bb| bb.get_terminator()).is_some(),
                 last_val.map(|v| v.get_type()));

        if self
            .builder
            .get_insert_block()
            .and_then(|bb| bb.get_terminator())
            .is_none()
        {
            let expected_return_type = function.get_type().get_return_type();

            println!(
                "[CodeGen Debug] Expected return type for '{}': {:?}",
                name, expected_return_type
            );

            match expected_return_type {
                Some(ret_type) => match last_val {
                    Some(val) => {
                        if val.get_type() == ret_type {
                            println!("[CodeGen Debug] Building return with value: {:?}", val);
                            self.builder.build_return(Some(&val)).map_err(|e| {
                                format!("Failed to build return for {}: {}", name, e)
                            })?;
                        } else {
                            if ret_type.is_float_type() && val.is_int_value() {
                                eprintln!("[CodeGen Warning] Coercing return value (int -> float) in function '{}'.", name);
                                let coerced = self
                                    .builder
                                    .build_signed_int_to_float(
                                        val.into_int_value(),
                                        ret_type.into_float_type(),
                                        "ret_coerce_i2f",
                                    )
                                    .unwrap();
                                println!(
                                    "[CodeGen Debug] Building return with coerced value: {:?}",
                                    coerced
                                );
                                self.builder.build_return(Some(&coerced)).unwrap();
                            } else {
                                self.variables = old_vars;
                                self.variable_types = old_var_types;
                                self.current_function = old_current_fn;
                                return Err(format!(
                                        "Implicit return type mismatch in function '{}'. Expected {:?}, got {:?}. Cannot safely coerce.",
                                        name, ret_type, val.get_type()
                                    ));
                            }
                        }
                    }
                    None => {
                        self.variables = old_vars;
                        self.variable_types = old_var_types;
                        self.current_function = old_current_fn;
                        return Err(format!("Function '{}' expects a return value of type {:?}, but none was provided by the last statement.", name, ret_type));
                    }
                },
                None => {
                    println!("[CodeGen Debug] Building void return for '{}'", name);
                    self.builder
                        .build_return(None)
                        .map_err(|e| format!("Failed to build void return for {}: {}", name, e))?;
                }
            }
        } else {
            println!(
                "[CodeGen Debug] Block for '{}' already terminated. Skipping implicit return.",
                name
            );
        }

        self.variables = old_vars;
        self.variable_types = old_var_types;
        self.current_function = old_current_fn;

        Ok(function)
    }

    pub fn register_function(&mut self, original_name: String, function: FunctionValue<'ctx>) {
        let generated_name = function.get_name().to_string_lossy().into_owned();
        self.functions.insert(original_name.clone(), function);
        self.original_to_generated
            .insert(original_name.clone(), generated_name.clone());
        self.generated_to_original
            .insert(generated_name, original_name);
    }
}
