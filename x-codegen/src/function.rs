use std::collections::{HashMap, HashSet};

use crate::{CodeGen, INSTANTIATION_CACHE};
use inkwell::attributes::{Attribute, AttributeLoc};
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum};
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum, FunctionValue};
use inkwell::AddressSpace;
use x_ast::{Expr, ExternParam, Param, Statement, StringLiteral, StringPart, StructDef, Type};

use x_logging::{debug, error, info, trace, warn};
use x_typechecker::FunctionSignature;

impl<'ctx> CodeGen<'ctx> {
    pub fn get_unique_id(&mut self) -> usize {
        use std::sync::atomic::{AtomicUsize, Ordering};
        static COUNTER: AtomicUsize = AtomicUsize::new(0);
        COUNTER.fetch_add(1, Ordering::Relaxed)
    }

    fn apply_function_parameter_attributes(
        &self,
        func_value: FunctionValue<'ctx>,
        ast_params: &[(String, x_ast::Type)],
    ) {
        let noalias_kind_id = Attribute::get_named_enum_kind_id("noalias");
        if noalias_kind_id == 0 {
            warn!("Could not find LLVM attribute kind for 'noalias'");
            return;
        }
        let noalias_attribute = self.context.create_enum_attribute(noalias_kind_id, 0);

        for (i, (_name, ast_type)) in ast_params.iter().enumerate() {
            // Check for the `is_unique` flag on reference types.
            if let x_ast::Type::Ref {
                is_unique: true, ..
            } = ast_type
            {
                // If the AST parameter is a `&unique` reference, add the `noalias`
                // attribute to the corresponding LLVM function parameter.
                func_value.add_attribute(AttributeLoc::Param(i as u32), noalias_attribute);
                trace!(
                    "Applied 'noalias' attribute to param {} of function {:?}",
                    i,
                    func_value.get_name()
                );
            }
        }
    }

    fn add_fn_attr(&self, func: FunctionValue<'ctx>, attr_name: &str) {
        let attr = self.context.create_string_attribute(attr_name, "");
        func.add_attribute(AttributeLoc::Function, attr);
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
            if let Some(val) = self.gen_statement(stmt, None)? {
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

    pub(crate) fn gen_function_call(
        &mut self,
        name: &str,
        args: &[Expr],
        self_type: Option<&x_ast::Type>,
    ) -> Result<BasicValueEnum<'ctx>, String> {
        // Check if this is a function we can specialise
        if let Some(generic_fn_stmt_ref) = self.specialisable_functions.get(name) {
            let generic_fn_stmt = generic_fn_stmt_ref.clone();
            // Expect the stored value to be a Function statement; if it's not, skip.
            if let Statement::Function { params, .. } = &generic_fn_stmt {
                let mut static_args = HashMap::new();
                let mut is_specialisable = true;

                // Collect static arguments and check if they are compile-time constants
                for (i, param) in params.iter().enumerate() {
                    if param.is_static {
                        match args.get(i) {
                            Some(a) => match a {
                                Expr::Int(_)
                                | Expr::Float(_)
                                | Expr::Boolean(_)
                                | Expr::String(_) => {
                                    static_args.insert(param.name.clone(), a.clone());
                                }
                                _ => {
                                    // If any static arg is not a constant, we can't specialise this call.
                                    is_specialisable = false;
                                    break;
                                }
                            },
                            None => {
                                is_specialisable = false;
                                break;
                            }
                        }
                    }
                }

                if is_specialisable {
                    // Mangle the name to include the constant values
                    let mangled_name = self.mangle_specialised_name(name, params, args);

                    // If this specialisation hasn't been compiled yet, create it now.
                    if self.module.get_function(&mangled_name).is_none() {
                        self.specialise_and_compile_function(
                            &mangled_name,
                            &generic_fn_stmt,
                            &static_args,
                        )?;
                    }

                    // Rewrite the call to use the new, specialised function
                    return self.gen_function_call(&mangled_name, args, self_type);
                }
            }
        }

        if name == "print" && args.len() == 1 {
            return self.gen_poly_print(&args[0], self_type);
        }

        let mut arg_vals: Vec<BasicValueEnum<'ctx>> = Vec::with_capacity(args.len());
        for a in args {
            arg_vals.push(self.gen_expr(a, self_type)?);
        }

        let function_opt: Option<FunctionValue<'ctx>> = if let Some(f) = self.functions.get(name) {
            Some(*f)
        } else {
            self.module.get_function(name)
        };

        if let Some(function) = function_opt {
            let expected_param_types = function.get_type().get_param_types();
            if arg_vals.len() != expected_param_types.len() {
                return Err(format!(
                    "Incorrect number of arguments for function '{}': expected {}, got {}",
                    name,
                    expected_param_types.len(),
                    arg_vals.len()
                ));
            }

            let mut compiled_args: Vec<BasicMetadataValueEnum<'ctx>> =
                Vec::with_capacity(arg_vals.len());
            for (i, mut arg_val) in arg_vals.into_iter().enumerate() {
                let expected_type = expected_param_types[i];

                // If arg is an alloca pointer but expected is a value type, load it.
                if arg_val.is_pointer_value() {
                    let ptr_val = arg_val.into_pointer_value();
                    match expected_type {
                        inkwell::types::BasicMetadataTypeEnum::PointerType(_) => {
                            // expected pointer -> pass as-is
                            arg_val = ptr_val.into();
                        }
                        inkwell::types::BasicMetadataTypeEnum::FloatType(ft) => {
                            arg_val = self
                                .builder
                                .build_load(
                                    ft.as_basic_type_enum(),
                                    ptr_val,
                                    &format!("load_arg_{}", i),
                                )
                                .unwrap();
                        }
                        inkwell::types::BasicMetadataTypeEnum::IntType(it) => {
                            arg_val = self
                                .builder
                                .build_load(
                                    it.as_basic_type_enum(),
                                    ptr_val,
                                    &format!("load_arg_{}", i),
                                )
                                .unwrap();
                        }
                        inkwell::types::BasicMetadataTypeEnum::ArrayType(at) => {
                            arg_val = self
                                .builder
                                .build_load(
                                    at.as_basic_type_enum(),
                                    ptr_val,
                                    &format!("load_arg_{}", i),
                                )
                                .unwrap();
                        }
                        inkwell::types::BasicMetadataTypeEnum::StructType(st) => {
                            arg_val = self
                                .builder
                                .build_load(
                                    st.as_basic_type_enum(),
                                    ptr_val,
                                    &format!("load_arg_{}", i),
                                )
                                .unwrap();
                        }
                        inkwell::types::BasicMetadataTypeEnum::VectorType(vt) => {
                            arg_val = self
                                .builder
                                .build_load(
                                    vt.as_basic_type_enum(),
                                    ptr_val,
                                    &format!("load_arg_{}", i),
                                )
                                .unwrap();
                        }
                        _ => {
                            // leave as-is for other metadata kinds
                            arg_val = ptr_val.into();
                        }
                    }
                }

                let coerced = self.build_coercion(arg_val, expected_type, &format!("arg_{}", i))?;
                compiled_args.push(coerced.into());
            }

            let call = self
                .builder
                .build_call(function, &compiled_args, "calltmp")
                .unwrap();

            return Ok(call
                .try_as_basic_value()
                .left()
                .unwrap_or_else(|| self.context.f64_type().const_float(0.0).into()));
        }

        // Try monomorphised candidate: mangle from argument AST types (e.g. get_first_Pair_i32_bool)
        // This helps when generic blueprints weren't declared but concrete variants exist.
        // Best-effort, local inference for argument AST types so we can try to
        // call existing monomorphised concrete functions without depending on the full semantic state
        let arg_types_strs_res: Result<Vec<String>, String> = (|| {
            let mut ts = Vec::with_capacity(args.len());
            for a in args {
                use x_ast::Expr::*;
                let inferred = match a {
                    Identifier(n) => {
                        // Prefer already-known variable types, otherwise if there's a
                        // registered struct with that name treat it as that custom type.
                        if let Some(ty) = self.variable_types.get(n) {
                            ty.clone()
                        } else if self.struct_types.contains_key(n) {
                            Type::Custom(n.clone())
                        } else {
                            return Err(format!("Unknown identifier in type inference: {}", n));
                        }
                    }
                    StructInstantiate(init) => Type::Custom(init.name.clone()),
                    Int(_) => Type::Int,
                    Float(_) => Type::Float,
                    Boolean(_) => Type::Bool,
                    String(_) => Type::String,
                    _ => {
                        return Err(
                            "Unsupported expression form for monomorphised candidate".to_string()
                        )
                    }
                };
                ts.push(inferred.to_string());
            }
            Ok(ts)
        })();

        if let Ok(arg_types_strs) = arg_types_strs_res {
            if !arg_types_strs.is_empty() {
                let cand = format!("{}_{}", name, arg_types_strs.join("_"));
                trace!(candidate = %cand, "trying monomorphised function candidate");
                if let Some(fv) = self.module.get_function(&cand) {
                    // found concrete function, call it (reuse same call path as above)
                    let function = fv;
                    let expected_param_types = function.get_type().get_param_types();
                    if arg_vals.len() != expected_param_types.len() {
                        return Err(format!(
                            "Incorrect number of arguments for function '{}': expected {}, got {}",
                            cand,
                            expected_param_types.len(),
                            arg_vals.len()
                        ));
                    }
                    let mut compiled_args: Vec<BasicMetadataValueEnum<'ctx>> =
                        Vec::with_capacity(arg_vals.len());
                    for (i, mut arg_val) in arg_vals.into_iter().enumerate() {
                        let expected_type = expected_param_types[i];
                        if arg_val.is_pointer_value() {
                            let ptr_val = arg_val.into_pointer_value();
                            match expected_type {
                                inkwell::types::BasicMetadataTypeEnum::PointerType(_) => {
                                    arg_val = ptr_val.into();
                                }
                                inkwell::types::BasicMetadataTypeEnum::FloatType(ft) => {
                                    arg_val = self
                                        .builder
                                        .build_load(
                                            ft.as_basic_type_enum(),
                                            ptr_val,
                                            &format!("load_arg_{}", i),
                                        )
                                        .unwrap();
                                }
                                inkwell::types::BasicMetadataTypeEnum::IntType(it) => {
                                    arg_val = self
                                        .builder
                                        .build_load(
                                            it.as_basic_type_enum(),
                                            ptr_val,
                                            &format!("load_arg_{}", i),
                                        )
                                        .unwrap();
                                }
                                inkwell::types::BasicMetadataTypeEnum::ArrayType(at) => {
                                    arg_val = self
                                        .builder
                                        .build_load(
                                            at.as_basic_type_enum(),
                                            ptr_val,
                                            &format!("load_arg_{}", i),
                                        )
                                        .unwrap();
                                }
                                inkwell::types::BasicMetadataTypeEnum::StructType(st) => {
                                    arg_val = self
                                        .builder
                                        .build_load(
                                            st.as_basic_type_enum(),
                                            ptr_val,
                                            &format!("load_arg_{}", i),
                                        )
                                        .unwrap();
                                }
                                inkwell::types::BasicMetadataTypeEnum::VectorType(vt) => {
                                    arg_val = self
                                        .builder
                                        .build_load(
                                            vt.as_basic_type_enum(),
                                            ptr_val,
                                            &format!("load_arg_{}", i),
                                        )
                                        .unwrap();
                                }
                                _ => {
                                    arg_val = ptr_val.into();
                                }
                            }
                        }

                        let coerced =
                            self.build_coercion(arg_val, expected_type, &format!("arg_{}", i))?;
                        compiled_args.push(coerced.into());
                    }

                    let call = self
                        .builder
                        .build_call(function, &compiled_args, "calltmp")
                        .unwrap();

                    return Ok(call
                        .try_as_basic_value()
                        .left()
                        .unwrap_or_else(|| self.context.f64_type().const_float(0.0).into()));
                }
            }
        }

        // 2) Fallback: maybe `name` is a variable storing a function pointer (closure).
        if let Some(var_alloca) = self.variables.get(name) {
            let ptr = *var_alloca;
            let elem_basic = ptr.get_type().as_basic_type_enum();
            let loaded_val = self
                .builder
                .build_load(elem_basic, ptr, &format!("load_{}", name))
                .map_err(|e| e.to_string())?;

            let param_metadata_types: Vec<inkwell::types::BasicMetadataTypeEnum<'ctx>> = args
                .iter()
                .map(|_| self.context.f64_type().into())
                .collect();
            let fn_ty = self
                .context
                .f64_type()
                .fn_type(&param_metadata_types, false);
            let fn_ptr_type = self.context.ptr_type(AddressSpace::default());

            let mut coerced_args: Vec<BasicMetadataValueEnum<'ctx>> =
                Vec::with_capacity(args.len());
            for (i, v) in arg_vals.into_iter().enumerate() {
                let meta = match v {
                    BasicValueEnum::FloatValue(fv) => fv.into(),
                    BasicValueEnum::IntValue(iv) => self
                        .builder
                        .build_signed_int_to_float(
                            iv,
                            self.context.f64_type(),
                            &format!("arg{}_i2f", i),
                        )
                        .unwrap()
                        .into(),
                    other => {
                        return Err(format!(
                            "Closure '{}' expects f64 arg {}, got {:?}",
                            name,
                            i,
                            other.get_type()
                        ));
                    }
                };
                coerced_args.push(meta);
            }

            match loaded_val {
                BasicValueEnum::PointerValue(pv) => {
                    let ind_call = self
                        .builder
                        .build_indirect_call(fn_ty, pv.into(), &coerced_args, "indirect_call")
                        .unwrap();
                    return Ok(ind_call.try_as_basic_value().left().unwrap());
                }
                BasicValueEnum::IntValue(iv) => {
                    // Convert integer back to function pointer and call.
                    let func_ptr = self
                        .builder
                        .build_int_to_ptr(iv, fn_ptr_type, "int_to_fnptr")
                        .unwrap();
                    let ind_call = self
                        .builder
                        .build_indirect_call(fn_ty, func_ptr, &coerced_args, "indirect_call")
                        .unwrap();
                    return Ok(ind_call.try_as_basic_value().left().unwrap());
                }
                other => {
                    return Err(format!(
                        "Variable '{}' does not contain a callable function pointer (found {:?})",
                        name,
                        other.get_type()
                    ));
                }
            }
        }

        Err(format!("Unknown function called: {}", name))
    }

    /// Generates a call to the correct underlying print function (`print_str` or `print`) based on the argument's type.
    fn gen_poly_print(
        &mut self,
        arg_expr: &Expr,
        self_type: Option<&x_ast::Type>,
    ) -> Result<BasicValueEnum<'ctx>, String> {
        if let Expr::String(literal) = arg_expr {
            if literal.parts.len() > 1
                || literal
                    .parts
                    .iter()
                    .any(|p| matches!(p, StringPart::Interpolation(_)))
            {
                return self.gen_interpolated_string_print(literal, self_type);
            }
        }

        let arg_val = self.gen_expr(arg_expr, self_type)?;

        let (fn_name, arg_to_print): (&str, BasicMetadataValueEnum) = match arg_val {
            BasicValueEnum::PointerValue(pv) => ("print_str", pv.into()),
            BasicValueEnum::FloatValue(fv) => ("print", fv.into()),
            BasicValueEnum::IntValue(iv) => {
                let float_val = self
                    .builder
                    .build_signed_int_to_float(iv, self.context.f64_type(), "print_i2f")
                    .unwrap();
                ("print", float_val.into())
            }
            _ => {
                return Err(format!(
                    "Unsupported type for print: {:?}",
                    arg_val.get_type()
                ))
            }
        };

        let func = self.module.get_function(fn_name).unwrap();
        self.builder
            .build_call(func, &[arg_to_print], "calltmp_print")
            .unwrap();

        Ok(self.context.f64_type().const_float(0.0).into())
    }

    fn gen_interpolated_string_print(
        &mut self,
        literal: &StringLiteral,
        self_type: Option<&x_ast::Type>,
    ) -> Result<BasicValueEnum<'ctx>, String> {
        let printf = self.module.get_function("printf").unwrap();
        let mut format_string = String::new();
        let mut printf_args = Vec::<BasicMetadataValueEnum<'ctx>>::new();

        for part in &literal.parts {
            match part {
                StringPart::Text(text) => format_string.push_str(&text.replace("%", "%%")),
                StringPart::Interpolation(expr) => {
                    let val = self.gen_expr(expr, self_type)?;
                    match val {
                        BasicValueEnum::IntValue(_) => format_string.push_str("%d"),
                        BasicValueEnum::FloatValue(_) => format_string.push_str("%.0f"),
                        BasicValueEnum::PointerValue(_) => format_string.push_str("%s"),
                        _ => return Err("Unsupported interpolation type.".to_string()),
                    }
                    printf_args.push(val.into());
                }
            }
        }
        format_string.push('\n');

        let format_ptr = self
            .builder
            .build_global_string_ptr(&format_string, "fmt")
            .unwrap();
        printf_args.insert(0, format_ptr.as_pointer_value().into());

        self.builder
            .build_call(printf, &printf_args, "printf_interp")
            .unwrap();
        Ok(self.context.f64_type().const_float(0.0).into())
    }

    pub fn compile_function(
        &mut self,
        name: &str,
        params: Vec<(String, x_ast::Type)>,
        body: &[Statement],
        is_pure: bool,
        is_memoised: bool,
    ) -> Result<FunctionValue<'ctx>, String> {
        let function = self
            .module
            .get_function(name)
            .ok_or_else(|| format!("Function {} not declared before compilation.", name))?;

        if is_pure || is_memoised {
            self.add_fn_attr(function, "readnone");
        }
        if !self.throws_functions.contains(name) {
            self.add_fn_attr(function, "nounwind");
        }

        println!("{}", function);

        let entry_block = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry_block);

        let old_vars = self.variables.clone();
        let old_var_types = self.variable_types.clone();
        let old_fn_params = self.fn_param_names.clone();
        self.current_function = Some(function);

        self.variables.clear();
        self.variable_types.clear();

        self.fn_param_names.insert(
            name.to_string(),
            params.iter().map(|(s, _)| s.clone()).collect(),
        );

        for (i, (param_name, param_ast_type)) in params.iter().enumerate() {
            let param_llvm_type = self.map_ast_type_to_llvm(param_ast_type);
            let param_value = function.get_nth_param(i as u32).unwrap();
            param_value.set_name(param_name);

            let alloca = self
                .builder
                .build_alloca(param_llvm_type, param_name)
                .unwrap();

            self.builder.build_store(alloca, param_value).unwrap();

            self.variables.insert(param_name.to_string(), alloca);
            self.variable_types
                .insert(param_name.to_string(), param_ast_type.clone());
        }

        let mut last_val: Option<BasicValueEnum<'ctx>> = None;
        for stmt in body {
            if self
                .builder
                .get_insert_block()
                .and_then(|b| b.get_terminator())
                .is_some()
            {
                warn!(function = %name, "Unreachable code detected in function");
                break;
            }
            last_val = self.gen_statement(stmt, None)?;
        }

        if self
            .builder
            .get_insert_block()
            .and_then(|b| b.get_terminator())
            .is_none()
        {
            let return_type = function.get_type().get_return_type();
            match (last_val, return_type) {
                (Some(val), Some(expected_type)) => {
                    let ret_val = match (val, expected_type) {
                        (BasicValueEnum::IntValue(iv), t) if t.is_float_type() => self
                            .builder
                            .build_signed_int_to_float(iv, self.context.f64_type(), "ret_cast")
                            .unwrap()
                            .into(),
                        (BasicValueEnum::FloatValue(fv), t) if t.is_int_type() => self
                            .builder
                            .build_float_to_signed_int(fv, self.context.i32_type(), "ret_cast")
                            .unwrap()
                            .into(),
                        _ => val,
                    };
                    self.builder.build_return(Some(&ret_val)).unwrap();
                }
                (_, None) => {
                    self.builder.build_return(None).unwrap();
                }
                (None, Some(exp_bt)) => {
                    // No explicit value produced in this path, but function expects a return.
                    // Synthesise a sensible default zero/null value for the expected return type
                    // so codegen remains well-formed. This avoids "must return a value" errors
                    // when all real-return paths have already emitted terminators.
                    let default_val: BasicValueEnum<'ctx> = match exp_bt {
                        BasicTypeEnum::FloatType(ft) => ft.const_float(0.0).into(),
                        BasicTypeEnum::IntType(it) => it.const_int(0, false).into(),
                        BasicTypeEnum::PointerType(pt) => pt.const_null().into(),
                        BasicTypeEnum::ArrayType(at) => at.const_zero().into(),
                        BasicTypeEnum::StructType(st) => st.const_zero().into(),
                        BasicTypeEnum::VectorType(vt) => vt.const_zero().into(),
                        _ => {
                            return Err(format!(
                                    "Function '{}' must return a value but none was produced (unsupported default for type {:?})",
                                    name, exp_bt
                                ));
                        }
                    };
                    self.builder.build_return(Some(&default_val)).unwrap();
                }
            }
        }

        self.variables = old_vars;
        self.variable_types = old_var_types;
        self.fn_param_names = old_fn_params;
        self.current_function = None;

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

    // Helper: detect if a type contains a type-parameter (i.e. still-generic).
    fn type_contains_type_parameter(ty: &Type) -> bool {
        match ty {
            Type::TypeParameter(_) => true,
            Type::Array(inner) => Self::type_contains_type_parameter(inner),
            Type::Ref { inner, .. } => Self::type_contains_type_parameter(inner),
            Type::GenericInstance { type_args, .. } => type_args
                .iter()
                .any(|t| Self::type_contains_type_parameter(t)),

            // Heuristic: a Custom type that is a simple uppercase identifier (e.g. "T", "U")
            // is likely a type parameter left in the AST after monomorphisation pass.
            // Treat it as containing a type-parameter so declaration/compilation is deferred.
            Type::Custom(name) => {
                if !name.is_empty() && name.chars().all(|c| c.is_ascii_uppercase()) {
                    return true;
                }
                false
            }
            _ => false,
        }
    }

    fn build_coercion(
        &self,
        value: BasicValueEnum<'ctx>,
        expected_type: inkwell::types::BasicMetadataTypeEnum<'ctx>,
        name: &str,
    ) -> Result<BasicValueEnum<'ctx>, String> {
        let expected_basic = match expected_type {
            BasicMetadataTypeEnum::FloatType(t) => t.as_basic_type_enum(),
            BasicMetadataTypeEnum::IntType(t) => t.as_basic_type_enum(),
            BasicMetadataTypeEnum::PointerType(t) => t.as_basic_type_enum(),
            BasicMetadataTypeEnum::ArrayType(t) => t.as_basic_type_enum(),
            BasicMetadataTypeEnum::StructType(t) => t.as_basic_type_enum(),
            BasicMetadataTypeEnum::VectorType(t) => t.as_basic_type_enum(),
            _ => {
                return Err(format!(
                    "Unsupported expected parameter type for coercion: {:?}",
                    expected_type
                ));
            }
        };

        if value.get_type() == expected_basic {
            return Ok(value);
        }

        match (expected_basic, value) {
            (BasicTypeEnum::FloatType(et), BasicValueEnum::IntValue(av)) => Ok(self
                .builder
                .build_signed_int_to_float(av, et, name)
                .unwrap()
                .into()),
            (BasicTypeEnum::IntType(et), BasicValueEnum::FloatValue(av)) => Ok(self
                .builder
                .build_float_to_signed_int(av, et, name)
                .unwrap()
                .into()),
            _ => Err(format!(
                "Cannot coerce value of type {:?} to {:?}",
                value.get_type(),
                expected_basic
            )),
        }
    }

    pub fn declare_all_functions(&mut self, all_statements: &[Statement]) -> Result<(), String> {
        for stmt in all_statements {
            match stmt {
                Statement::Function {
                    name,
                    params,
                    return_type,
                    generic_params,
                    is_multi,
                    is_throws,
                    is_memoised,
                    is_pure,
                    ..
                } => {
                    debug!(name = %name, "processing function declaration");

                    if generic_params.is_some() {
                        debug!(name = %name, "skipping declaration of generic function blueprint");
                        continue;
                    }

                    // If the signature still contains type-parameters (e.g. return_type = "T"
                    // or a parameter is GenericInstance/TypeParameter), skip declaring it.
                    let sig_has_type_param = Self::type_contains_type_parameter(return_type)
                        || params
                            .iter()
                            .any(|p| Self::type_contains_type_parameter(&p.ty));
                    if sig_has_type_param {
                        debug!(name = %name, "skipping declaration: signature contains unresolved type-parameters");
                        continue;
                    }

                    if *is_multi {
                        if params.len() < 2 {
                            return Err(format!(
                                "multi requires at least 2 parameters for '{}'",
                                name
                            ));
                        }
                        let mangled = format!(
                            "{}${}",
                            name,
                            params
                                .iter()
                                .map(|p| p.ty.to_string())
                                .collect::<Vec<_>>()
                                .join("$")
                        );

                        if *is_throws {
                            self.throws_functions.insert(mangled.clone());
                        }
                        if self.module.get_function(&mangled).is_none() {
                            let mut param_types: Vec<BasicMetadataTypeEnum<'ctx>> = Vec::new();
                            for p in params.iter() {
                                let llvm_ty = self.map_ast_type_to_llvm(&p.ty);
                                param_types.push(llvm_ty.into());
                            }
                            let ret_llvm = if *return_type == Type::Void {
                                None
                            } else {
                                Some(self.map_ast_type_to_llvm(return_type))
                            };
                            let fn_type = match ret_llvm {
                                None => self.context.void_type().fn_type(&param_types, false),
                                Some(rt) => rt.fn_type(&param_types, false),
                            };
                            let func = self.module.add_function(&mangled, fn_type, None);
                            self.functions.insert(mangled.clone(), func);

                            let param_pairs: Vec<(String, x_ast::Type)> = params
                                .iter()
                                .map(|p| (p.name.clone(), p.ty.clone()))
                                .collect();
                            self.apply_function_parameter_attributes(func, &param_pairs);

                            if *is_pure {
                                self.add_fn_attr(func, "readnone");
                            }
                            if *is_memoised {
                                self.add_fn_attr(func, "readonly");
                            }
                            if !self.throws_functions.contains(&mangled) {
                                self.add_fn_attr(func, "nounwind");
                            }
                            self.multi_variants.entry(name.clone()).or_default().push((
                                params.iter().map(|p| p.ty.clone()).collect(),
                                mangled.clone(),
                            ));
                            debug!(mangled = %mangled, return_type = ?return_type, "declared multi variant");
                        }

                        if !self.multi_resolvers.contains(name)
                            && self.module.get_function(name).is_none()
                        {
                            let mut param_types: Vec<BasicMetadataTypeEnum<'ctx>> = Vec::new();
                            for p in params.iter() {
                                param_types.push(self.map_ast_type_to_llvm(&p.ty).into());
                            }
                            let fn_type = if *return_type == Type::Void {
                                self.context.void_type().fn_type(&param_types, false)
                            } else {
                                self.map_ast_type_to_llvm(return_type)
                                    .fn_type(&param_types, false)
                            };
                            let resolver = self.module.add_function(name, fn_type, None);
                            self.functions.insert(name.clone(), resolver);
                            let param_pairs: Vec<(String, x_ast::Type)> = params
                                .iter()
                                .map(|p| (p.name.clone(), p.ty.clone()))
                                .collect();
                            self.apply_function_parameter_attributes(resolver, &param_pairs);
                            if *is_pure {
                                self.add_fn_attr(resolver, "readnone");
                            }
                            if *is_memoised {
                                self.add_fn_attr(resolver, "readonly");
                            }
                            if *is_throws {
                                self.throws_functions.insert(name.clone());
                            } else {
                                self.add_fn_attr(resolver, "nounwind");
                            }
                            self.multi_resolvers.insert(name.clone());
                            debug!(name = %name, "declared multi resolver");
                        }
                        continue;
                    }

                    if self.module.get_function(name).is_some() {
                        debug!(name = %name, "function already declared in module, skipping");
                        continue;
                    }

                    let mut param_types: Vec<BasicMetadataTypeEnum<'ctx>> = Vec::new();
                    for (idx, p) in params.iter().enumerate() {
                        let pname = &p.name;
                        let pty = &p.ty;
                        debug!(func = %name, idx = idx, param_name = %pname, ast_type = ?pty, "mapping param");
                        let llvm_ty = self.map_ast_type_to_llvm(pty);
                        debug!(func = %name, idx = idx, param_name = %pname, llvm_type = ?llvm_ty, "mapped param");
                        param_types.push(llvm_ty.into());
                    }

                    debug!(func = %name, ast_return_type = ?return_type, "mapping return type");
                    let fn_type = if return_type == &x_ast::Type::Void {
                        debug!(func = %name, "function has void return type");
                        self.context.void_type().fn_type(&param_types, false)
                    } else {
                        let llvm_ret = self.map_ast_type_to_llvm(return_type);
                        debug!(func = %name, llvm_return_type = ?llvm_ret, "mapped return type");
                        llvm_ret.fn_type(&param_types, false)
                    };

                    info!(name = %name, fn_type = ?fn_type, "adding function to module");
                    let func = self.module.add_function(name, fn_type, None);
                    self.functions.insert(name.clone(), func);
                    let param_pairs: Vec<(String, x_ast::Type)> = params
                        .iter()
                        .map(|p| (p.name.clone(), p.ty.clone()))
                        .collect();
                    self.apply_function_parameter_attributes(func, &param_pairs);

                    if *is_pure {
                        self.add_fn_attr(func, "readnone");
                    }
                    if *is_memoised {
                        self.add_fn_attr(func, "readonly");
                    }
                    if !self.throws_functions.contains(name) {
                        self.add_fn_attr(func, "nounwind");
                    }
                    debug!(name = %name, params = ?params, return_type = ?return_type, "declared internal function");
                }

                Statement::ImplDecl(impl_def) => {
                    if let x_ast::Type::Custom(struct_name) = &impl_def.type_name {
                        let implemented_methods: std::collections::HashSet<String> = impl_def
                            .methods
                            .iter()
                            .filter_map(|m| {
                                if let Statement::Function { name, .. } = m {
                                    Some(name.clone())
                                } else {
                                    None
                                }
                            })
                            .collect();

                        for method in &impl_def.methods {
                            if let Statement::Function {
                                name,
                                params,
                                return_type,
                                ..
                            } = method
                            {
                                let mangled_name = format!("{}_{}", struct_name, name);
                                let param_pairs: Vec<(String, x_ast::Type)> = params
                                    .iter()
                                    .map(|p| (p.name.clone(), p.ty.clone()))
                                    .collect();
                                self.declare_function_signature(
                                    &mangled_name,
                                    &param_pairs,
                                    return_type,
                                    Some(&impl_def.type_name),
                                )?;
                                debug!(struct = %struct_name, method = %mangled_name, "declared impl method signature");
                            }
                        }

                        let mut methods_to_declare = Vec::new();
                        if let Some(trait_def) = self.traits.get(&impl_def.trait_name) {
                            for (method_name, default_body_opt) in &trait_def.methods {
                                if !implemented_methods.contains(method_name) {
                                    if let Some(Statement::Function {
                                        params,
                                        return_type,
                                        ..
                                    }) = default_body_opt
                                    {
                                        methods_to_declare.push((
                                            method_name.clone(),
                                            params.clone(),
                                            return_type.clone(),
                                        ));
                                    }
                                }
                            }
                        }

                        for (method_name, params, return_type) in methods_to_declare {
                            let mangled_name = format!("{}_{}", struct_name, &method_name);
                            info!(mangled = %mangled_name, "declaring synthesised default method");
                            let param_pairs: Vec<(String, x_ast::Type)> = params
                                .iter()
                                .map(|p| (p.name.clone(), p.ty.clone()))
                                .collect();
                            self.declare_function_signature(
                                &mangled_name,
                                &param_pairs,
                                &return_type,
                                Some(&impl_def.type_name),
                            )?;
                        }
                    }
                }

                Statement::ExternFunctionDecl {
                    name,
                    params,
                    return_type: ast_return_type,
                } => {
                    if self.module.get_function(name).is_none()
                        && !self.external_functions.contains_key(name)
                    {
                        let mut param_llvm_types = Vec::new();
                        let mut param_type_names = Vec::new();
                        for ExternParam { name: _, type_name } in params {
                            let llvm_type = self.map_type(type_name)?;
                            param_llvm_types.push(llvm_type.into());
                            param_type_names.push(type_name.clone());
                        }

                        let return_llvm_type: Option<BasicTypeEnum<'ctx>> = match ast_return_type {
                            Some(type_name) => Some(self.map_type(type_name)?),
                            None => None,
                        };

                        let fn_ty = match return_llvm_type {
                            Some(rt) => rt.fn_type(&param_llvm_types[..], false),
                            None => self
                                .context
                                .void_type()
                                .fn_type(&param_llvm_types[..], false),
                        };

                        let func = self.module.add_function(name, fn_ty, None);
                        self.external_functions.insert(name.clone(), func);

                        let ret_type_str =
                            return_llvm_type.map_or("void".to_string(), |rt| format!("{:?}", rt));
                        debug!(name = %name, params = ?param_type_names, return = %ret_type_str, "declared external function");
                    }
                }

                _ => {}
            }
        }

        Ok(())
    }

    pub fn instantiate_function(
        &mut self,
        name: &str,
        type_args: &[Type],
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
        gen_structs: &HashMap<String, StructDef>,
        concrete_statements: &mut Vec<Statement>,
    ) -> Result<String, String> {
        let mangled_name = format!(
            "{}_{}",
            name,
            type_args
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join("_")
        );

        if INSTANTIATION_CACHE.with(|cache| cache.borrow().contains_key(&mangled_name)) {
            return Ok(mangled_name);
        }

        let (params, ret_type, body, is_pure, is_memoised, generic_params) =
            gen_funcs.get(name).unwrap();
        let type_map: HashMap<_, _> = generic_params
            .iter()
            .cloned()
            .zip(type_args.iter().cloned())
            .collect();

        let mut concrete_body = body.clone();
        for stmt in concrete_body.iter_mut() {
            self.substitute_in_statement(stmt, &type_map, gen_structs, concrete_statements)?;
        }

        let concrete_params: Vec<Param> = params
            .iter()
            .map(|(param_name, param_type)| {
                let concrete_param_type = self.substitute_type(param_type, &type_map);
                Param {
                    name: param_name.clone(),
                    ty: concrete_param_type,
                    is_static: false,
                }
            })
            .collect();

        let concrete_return_type = self.substitute_type(ret_type, &type_map);

        let concrete_func = Statement::Function {
            name: mangled_name.clone(),
            generic_params: None,
            params: concrete_params,
            return_type: concrete_return_type,
            body: Some(concrete_body),
            is_pure: *is_pure,
            is_memoised: *is_memoised,
            // These are NOT multi or throws; keep them as normal concrete functions.
            is_multi: false,
            is_throws: false,
        };

        info!(mangled = %mangled_name, "Instantiated new function");
        concrete_statements.push(concrete_func);
        INSTANTIATION_CACHE.with(|cache| {
            cache
                .borrow_mut()
                .insert(mangled_name.clone(), mangled_name.clone())
        });

        Ok(mangled_name)
    }

    pub fn register_methods(&mut self, ast: &x_ast::Program) {
        for stmt in &ast.statements {
            if let x_ast::Statement::ImplDecl(impl_def) = stmt {
                if let x_ast::Type::Custom(struct_name) = &impl_def.type_name {
                    let method_names_map =
                        self.struct_methods.entry(struct_name.clone()).or_default();
                    let mut implemented_method_names = std::collections::HashSet::new();
                    for method_stmt in &impl_def.methods {
                        if let x_ast::Statement::Function {
                            name,
                            params,
                            return_type,
                            ..
                        } = method_stmt
                        {
                            let sig = FunctionSignature {
                                param_types: params.iter().map(|p| p.ty.clone()).collect(),
                                return_type: return_type.clone(),
                            };
                            method_names_map.insert(name.clone(), sig);
                            implemented_method_names.insert(name.clone());
                        }
                    }

                    if let Some(trait_def) = self.traits.get(&impl_def.trait_name) {
                        for (method_name, default_body_opt) in &trait_def.methods {
                            if !implemented_method_names.contains(method_name)
                                && default_body_opt.is_some()
                            {
                                if let Some(Statement::Function {
                                    params,
                                    return_type,
                                    ..
                                }) = default_body_opt
                                {
                                    let sig = FunctionSignature {
                                        param_types: params.iter().map(|p| p.ty.clone()).collect(),
                                        return_type: return_type.clone(),
                                    };
                                    method_names_map.insert(method_name.clone(), sig);
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    pub fn compile_all_function_bodies(
        &mut self,
        all_statements: &[Statement],
    ) -> Result<(), String> {
        if self
            .functions
            .values()
            .any(|f| f.get_first_basic_block().is_some())
        {
            info!("Skipping compile_all_function_bodies: function bodies already generated");
            return Ok(());
        }

        (for stmt in all_statements {
            match stmt {
                Statement::Function {
                    name,
                    params,
                    return_type,
                    body,
                    is_pure: _is_pure,
                    is_multi,
                    generic_params,
                    ..
                } => {
                    // If the function is a generic blueprint (either has generic params
                    // or its signature contains type-parameters), skip compiling it here.
                    // Concrete variants must be instantiated first (monomorphisation).
                    if generic_params.is_some() {
                        trace!(name = %name, "Skipping compilation of generic function blueprint");
                        continue;
                    }
                    let sig_has_type_param = Self::type_contains_type_parameter(return_type)
                        || params
                            .iter()
                            .any(|p| Self::type_contains_type_parameter(&p.ty));
                    if sig_has_type_param {
                        trace!(name = %name, "Skipping compilation of function with unresolved type-parameters");
                        continue;
                    }
                    info!(name = %name, "Compiling function");
                    let compile_name = if *is_multi {
                        format!(
                            "{}${}",
                            name,
                            params
                                .iter()
                                .map(|p| p.ty.to_string())
                                .collect::<Vec<_>>()
                                .join("$")
                        )
                    } else {
                        name.clone()
                    };
                    self.gen_function_def(&compile_name, params, return_type, body, None)?;
                }
                Statement::ImplDecl(impl_def) => {
                    let implemented_methods: HashSet<String> = impl_def
                        .methods
                        .iter()
                        .filter_map(|m| {
                            if let Statement::Function { name, .. } = m {
                                Some(name.clone())
                            } else {
                                None
                            }
                        })
                        .collect();

                    if let Type::Custom(struct_name) = &impl_def.type_name {
                        for method in &impl_def.methods {
                            if let Statement::Function {
                                name,
                                params,
                                return_type,
                                body,
                                is_pure: _is_pure,
                                ..
                            } = method
                            {
                                let mangled_name = format!("{}_{}", struct_name, name);
                                info!(mangled = %mangled_name, "Compiling implemented method");
                                self.gen_function_def(
                                    &mangled_name,
                                    params,
                                    return_type,
                                    body,
                                    Some(&impl_def.type_name),
                                )?;
                            }
                        }

                        let mut methods_to_synthesise = Vec::new();

                        if let Some(trait_def) = self.traits.get(&impl_def.trait_name) {
                            for (method_name, default_body_opt) in &trait_def.methods {
                                if !implemented_methods.contains(method_name) {
                                    if let Some(Statement::Function {
                                        params,
                                        return_type,
                                        body,
                                        ..
                                    }) = default_body_opt
                                    {
                                        let mangled_name =
                                            format!("{}_{}", struct_name, method_name);
                                        methods_to_synthesise.push((
                                            mangled_name,
                                            params.clone(),
                                            return_type.clone(),
                                            body.clone(),
                                            impl_def.type_name.clone(),
                                        ));
                                    }
                                }
                            }
                        }

                        for (mangled_name, params, return_type, body, self_type) in
                            methods_to_synthesise
                        {
                            info!(mangled = %mangled_name, "Synthesising default method");
                            self.gen_function_def(
                                &mangled_name,
                                &params,
                                &return_type,
                                &body,
                                Some(&self_type),
                            )?;
                        }
                    }
                }
                _ => {}
            }
        });

        match self.module.verify() {
            Ok(_) => {
                info!("LLVM module verification passed");
                return Ok(());
            }
            Err(msg) => {
                let ir = self.module.print_to_string();
                error!("LLVM module verification failed: {}", msg);
                error!("LLVM IR (pre-emit):\n{}", ir.to_string());
                return Err(format!("LLVM module verification failed: {}", msg));
            }
        }
    }
    pub fn declare_function_signature(
        &mut self,
        name: &str,
        params: &[(String, x_ast::Type)],
        return_type: &x_ast::Type,
        self_type: Option<&x_ast::Type>,
    ) -> Result<(), String> {
        let param_types: Vec<BasicTypeEnum<'ctx>> = params
            .iter()
            .map(|(_, ast_type)| {
                if let x_ast::Type::Ref { inner, .. } = ast_type {
                    if let x_ast::Type::TypeParameter(p_name) = &**inner {
                        if p_name == "Self" && self_type.is_some() {
                            return self.context.ptr_type(AddressSpace::default()).into();
                        }
                    }
                }
                self.map_ast_type_to_llvm(ast_type)
            })
            .collect();

        let param_metadata: Vec<inkwell::types::BasicMetadataTypeEnum<'ctx>> =
            param_types.iter().map(|&t| t.into()).collect();

        let fn_type = if *return_type == x_ast::Type::Void {
            self.context.void_type().fn_type(&param_metadata, false)
        } else {
            self.map_ast_type_to_llvm(return_type)
                .fn_type(&param_metadata, false)
        };

        let func = self.module.add_function(name, fn_type, None);
        self.apply_function_parameter_attributes(func, params);

        Ok(())
    }

    pub fn gen_function_def(
        &mut self,
        name: &str,
        params: &[x_ast::Param],
        _return_type: &x_ast::Type,
        body: &Option<Box<Vec<Statement>>>,
        self_type: Option<&x_ast::Type>,
    ) -> Result<(), String> {
        let function = self
            .module
            .get_function(name)
            .ok_or_else(|| format!("Function '{}' not declared", name))?;

        if function.get_first_basic_block().is_some() {
            return Ok(());
        }

        if let Some(body_stmts) = body {
            let entry_block = self.context.append_basic_block(function, "entry");
            self.builder.position_at_end(entry_block);

            let old_vars = self.variables.clone();
            let old_var_types = self.variable_types.clone();
            let old_fn_params = self.fn_param_names.clone();
            let old_current_fn = self.current_function;
            self.variables.clear();
            self.variable_types.clear();
            self.current_function = Some(function);
            self.fn_param_names.insert(
                name.to_string(),
                params.iter().map(|p| p.name.clone()).collect(),
            );

            for (i, param) in params.iter().enumerate() {
                let param_name = &param.name;
                let param_ast_type = &param.ty;

                let mut final_ast_type = param_ast_type.clone();
                if let x_ast::Type::Ref {
                    is_mut,
                    is_unique,
                    inner,
                } = &param.ty
                {
                    if let x_ast::Type::TypeParameter(p_name) = &**inner {
                        if p_name == "Self" {
                            if let Some(st) = self_type {
                                final_ast_type = x_ast::Type::Ref {
                                    is_mut: *is_mut,
                                    is_unique: *is_unique,
                                    inner: Box::new(st.clone()),
                                };
                            }
                        }
                    }
                }

                let llvm_type = self.map_ast_type_to_llvm(&final_ast_type);
                let alloca = self
                    .builder
                    .build_alloca(llvm_type, param_name)
                    .map_err(|e| e.to_string())?;
                let nth = function
                    .get_nth_param(i as u32)
                    .ok_or_else(|| format!("Missing parameter {} for function {}", i, name))?;
                self.builder.build_store(alloca, nth).unwrap();
                self.variables.insert(param_name.clone(), alloca);
                self.variable_types
                    .insert(param_name.clone(), final_ast_type);
            }

            let mut last_val: Option<BasicValueEnum<'ctx>> = None;
            for stmt in body_stmts.iter() {
                last_val = self.gen_statement(stmt, self_type)?;

                if self
                    .builder
                    .get_insert_block()
                    .and_then(|bb| bb.get_terminator())
                    .is_some()
                {
                    break;
                }
            }

            if self
                .builder
                .get_insert_block()
                .and_then(|bb| bb.get_terminator())
                .is_none()
            {
                let ret_ty_opt = function.get_type().get_return_type();
                match (last_val, ret_ty_opt) {
                    (Some(val), Some(exp_bt)) => {
                        if val.get_type() == exp_bt {
                            self.builder.build_return(Some(&val)).unwrap();
                        } else {
                            let coerced: BasicValueEnum<'ctx> = match (val, exp_bt) {
                                // int -> float
                                (BasicValueEnum::IntValue(iv), bt) if bt.is_float_type() => {
                                    let fty = bt.into_float_type();
                                    self.builder
                                        .build_signed_int_to_float(iv, fty, "ret_i2f")
                                        .map_err(|e| e.to_string())?
                                        .into()
                                }
                                // float -> int
                                (BasicValueEnum::FloatValue(fv), bt) if bt.is_int_type() => {
                                    let ity = bt.into_int_type();
                                    self.builder
                                        .build_float_to_signed_int(fv, ity, "ret_f2i")
                                        .map_err(|e| e.to_string())?
                                        .into()
                                }
                                // int -> int (width adjust, incl. i32 -> i1)
                                (BasicValueEnum::IntValue(iv), bt) if bt.is_int_type() => {
                                    let ity = bt.into_int_type();
                                    let src_w = iv.get_type().get_bit_width();
                                    let dst_w = ity.get_bit_width();
                                    if src_w == dst_w {
                                        iv.into()
                                    } else if src_w > dst_w {
                                        self.builder
                                            .build_int_truncate(iv, ity, "ret_trunc")
                                            .map_err(|e| e.to_string())?
                                            .into()
                                    } else {
                                        self.builder
                                            .build_int_s_extend(iv, ity, "ret_sext")
                                            .map_err(|e| e.to_string())?
                                            .into()
                                    }
                                }
                                // float -> float
                                (BasicValueEnum::FloatValue(fv), bt) if bt.is_float_type() => {
                                    let fty = bt.into_float_type();
                                    if fv.get_type() == fty {
                                        fv.into()
                                    } else {
                                        return Err(format!(
                                            "Return type mismatch in function '{}'",
                                            name
                                        ));
                                    }
                                }
                                // ptr -> ptr (bitcast)
                                (BasicValueEnum::PointerValue(pv), bt) if bt.is_pointer_type() => {
                                    let pty = bt.into_pointer_type();
                                    self.builder
                                        .build_bit_cast(pv, pty, "ret_ptr_cast")
                                        .map_err(|e| e.to_string())?
                                }
                                (got, want) => {
                                    return Err(format!(
                                        "Unsupported return coercion for function '{}': got {:?}, want {:?}",
                                        name, got.get_type(), want
                                    ));
                                }
                            };
                            self.builder.build_return(Some(&coerced)).unwrap();
                        }
                    }
                    (_, None) => {
                        self.builder.build_return(None).unwrap();
                    }
                    (None, Some(exp_bt)) => {
                        // No explicit value produced in this path, but function expects a return.
                        // Synthesise a sensible default zero/null value for the expected return type
                        // so codegen remains well-formed. This avoids "must return a value" errors
                        // when all real-return paths have already emitted terminators.
                        let default_val: BasicValueEnum<'ctx> = match exp_bt {
                            BasicTypeEnum::FloatType(ft) => ft.const_float(0.0).into(),
                            BasicTypeEnum::IntType(it) => it.const_int(0, false).into(),
                            BasicTypeEnum::PointerType(pt) => pt.const_null().into(),
                            BasicTypeEnum::ArrayType(at) => at.const_zero().into(),
                            BasicTypeEnum::StructType(st) => st.const_zero().into(),
                            BasicTypeEnum::VectorType(vt) => vt.const_zero().into(),
                            _ => {
                                return Err(format!(
                                    "Function '{}' must return a value but none was produced (unsupported default for type {:?})",
                                    name, exp_bt
                                ));
                            }
                        };
                        self.builder.build_return(Some(&default_val)).unwrap();
                    }
                }
            }

            self.variables = old_vars;
            self.variable_types = old_var_types;
            self.fn_param_names = old_fn_params;
            self.current_function = old_current_fn;

            if !function.verify(true) {
                unsafe { function.delete() };
                return Err(format!("Invalid function generated: {}", name));
            }
        }

        Ok(())
    }

    pub fn instantiate_called_generics(
        &mut self,
        all_statements: &[Statement],
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
        gen_structs: &HashMap<String, StructDef>,
    ) -> Result<Vec<Statement>, String> {
        use std::collections::HashSet;

        let mut concrete_stmts: Vec<Statement> = Vec::new();
        let mut seen: HashSet<String> = HashSet::new();

        // Walk expressions and collect function-call Expr references.
        fn walk_expr<'a>(expr: &'a Expr, out_calls: &mut Vec<&'a Expr>) {
            use x_ast::Expr::*;
            match expr {
                FunctionCall { args, .. } => {
                    out_calls.push(expr);
                    for a in args {
                        walk_expr(a, out_calls);
                    }
                }
                BinaryOp { left, right, .. } => {
                    walk_expr(left, out_calls);
                    walk_expr(right, out_calls);
                }
                AnonymousFunction { body, .. } => {
                    for stmt in body {
                        walk_statement(stmt, out_calls);
                    }
                }
                Array(items) => {
                    for it in items {
                        walk_expr(it, out_calls);
                    }
                }
                ArrayAccess { array, index } => {
                    walk_expr(array, out_calls);
                    walk_expr(index, out_calls);
                }
                Assignment { target, value } => {
                    walk_expr(target, out_calls);
                    walk_expr(value, out_calls);
                }
                StructInstantiate(init) => {
                    for (_n, v) in &init.fields {
                        walk_expr(v, out_calls);
                    }
                }
                FieldAccess { object, .. } => walk_expr(object, out_calls),
                UnaryOp { expr, .. } => walk_expr(expr, out_calls),
                AddressOf { expr, .. } => walk_expr(expr, out_calls),
                Deref { expr } => walk_expr(expr, out_calls),
                _ => {}
            }
        }

        // Walk statements and extract expressions.
        fn walk_statement<'a>(stmt: &'a Statement, out_calls: &mut Vec<&'a Expr>) {
            use x_ast::Statement::*;
            match stmt {
                Expression { expr } => walk_expr(expr, out_calls),
                VariableDecl { value, .. } => walk_expr(value, out_calls),
                Return { value } => {
                    if let Some(v) = value {
                        walk_expr(v, out_calls);
                    }
                }
                Statement::Function { body, .. } => {
                    if let Some(b) = body {
                        for s in b.iter() {
                            walk_statement(s, out_calls);
                        }
                    }
                }
                If {
                    condition,
                    then_block,
                    else_block,
                } => {
                    walk_expr(condition, out_calls);
                    for s in then_block {
                        walk_statement(s, out_calls);
                    }
                    if let Some(eb) = else_block {
                        for s in eb {
                            walk_statement(s, out_calls);
                        }
                    }
                }
                WhileLoop { condition, body } => {
                    walk_expr(condition, out_calls);
                    for s in body {
                        walk_statement(s, out_calls);
                    }
                }
                ForEachLoop { iterator, body, .. } => {
                    walk_expr(iterator, out_calls);
                    for s in body {
                        walk_statement(s, out_calls);
                    }
                }
                // ImplDecl contains methods which will be walked by top-level scanning if needed.
                StructDecl(_)
                | Import { .. }
                | FileImport { .. }
                | ExternFunctionDecl { .. }
                | ImplDecl(_) => {}
                _ => {}
            }
        }

        // Find a variable's annotated type by name from AST.
        fn find_var_type(name: &str, stmts: &[Statement]) -> Option<Type> {
            use x_ast::Statement::*;
            for s in stmts {
                match s {
                    VariableDecl {
                        name: vname,
                        type_ann: Some(ty),
                        ..
                    } if vname == name => {
                        return Some(ty.clone());
                    }
                    Statement::Function { body: Some(b), .. } => {
                        if let Some(found) = find_var_type(name, b) {
                            return Some(found);
                        }
                    }
                    If {
                        then_block,
                        else_block,
                        ..
                    } => {
                        if let Some(found) = find_var_type(name, then_block) {
                            return Some(found);
                        }
                        if let Some(eb) = else_block {
                            if let Some(found) = find_var_type(name, eb) {
                                return Some(found);
                            }
                        }
                    }
                    WhileLoop { body, .. } | ForEachLoop { body, .. } => {
                        if let Some(found) = find_var_type(name, body) {
                            return Some(found);
                        }
                    }
                    ImplDecl(impld) => {
                        // search methods
                        for m in &impld.methods {
                            if let Statement::Function { body: Some(b), .. } = m {
                                if let Some(found) = find_var_type(name, b) {
                                    return Some(found);
                                }
                            }
                        }
                    }
                    _ => {}
                }
            }
            None
        }

        // Lightweight, context-free type inference for call-site args.
        fn infer_type_no_ctx(expr: &Expr, stmts: &[Statement]) -> Option<Type> {
            match expr {
                Expr::Identifier(n) => find_var_type(n, stmts),
                Expr::StructInstantiate(init) => Some(Type::Custom(init.name.clone())),
                Expr::Int(_) => Some(Type::Int),
                Expr::Float(_) => Some(Type::Float),
                Expr::Boolean(_) => Some(Type::Bool),
                Expr::String(_) => Some(Type::String),
                // Arrays: infer element if homogeneous, else skip
                Expr::Array(items) if !items.is_empty() => {
                    let first = infer_type_no_ctx(&items[0], stmts)?;
                    if items
                        .iter()
                        .all(|e| infer_type_no_ctx(e, stmts) == Some(first.clone()))
                    {
                        Some(Type::Array(Box::new(first)))
                    } else {
                        None
                    }
                }
                // AddressOf/Deref pass through inner type best-effort
                Expr::AddressOf { expr, .. } | Expr::Deref { expr } => {
                    infer_type_no_ctx(expr, stmts)
                }
                // FieldAccess: leave to monomorphised functions
                _ => None,
            }
        }

        // Collect all function-call expressions
        let mut calls: Vec<&Expr> = Vec::new();
        for stmt in all_statements {
            walk_statement(stmt, &mut calls);
        }

        // For each call expression, attempt to instantiate matching generic template
        for call_expr in calls {
            if let Expr::FunctionCall { name, args } = call_expr {
                // Only consider names that correspond to generic blueprints
                if !gen_funcs.contains_key(name.as_str()) {
                    continue;
                }

                // Build concrete type args from call-site, without relying on get_expr_type.
                let mut type_args: Vec<Type> = Vec::with_capacity(args.len());
                let mut ok = true;
                for a in args {
                    if let Some(t) = infer_type_no_ctx(a, all_statements) {
                        type_args.push(t);
                    } else {
                        ok = false;
                        break;
                    }
                }
                if !ok {
                    continue;
                }

                let key = format!(
                    "{}_{}",
                    name,
                    type_args
                        .iter()
                        .map(|t| t.to_string())
                        .collect::<Vec<_>>()
                        .join("_")
                );
                if seen.contains(&key) {
                    continue;
                }

                match self.instantiate_function(
                    name,
                    &type_args,
                    gen_funcs,
                    gen_structs,
                    &mut concrete_stmts,
                ) {
                    Ok(_) => {
                        seen.insert(key);
                    }
                    Err(e) => {
                        return Err(format!("Failed to instantiate generic '{}': {}", name, e));
                    }
                }
            }
        }

        Ok(concrete_stmts)
    }
}
