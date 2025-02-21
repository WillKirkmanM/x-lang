use crate::CodeGen;
use inkwell::values::{BasicMetadataValueEnum, FloatValue, FunctionValue};
use inkwell::AddressSpace;
use x_ast::{Expr, Statement, StringPart};

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
                last_val = val;
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
    ) -> Result<FloatValue<'ctx>, String> {
        match (name, args.first()) {
            ("print", Some(Expr::String(string_literal))) => {
                let printf = self.get_printf_fn()?;
                let mut fmt_str = String::new();
                let mut printf_args: Vec<BasicMetadataValueEnum<'ctx>> = Vec::new();

                for part in &string_literal.parts {
                    match part {
                        StringPart::Text(text) => {
                            fmt_str.push_str(text);
                        }
                        StringPart::Interpolation(expr) => {
                            fmt_str.push_str("%s");
                            let val = self.gen_expr(expr)?;
                            printf_args.push(val.into());
                        }
                    }
                }

                fmt_str.push_str("\n\0");
                let fmt_ptr = self
                    .builder
                    .build_global_string_ptr(&fmt_str, "fmt_str")
                    .map_err(|e| e.to_string())?;

                let mut call_args = vec![fmt_ptr.as_pointer_value().into()];
                call_args.extend(printf_args);

                self.builder
                    .build_call(printf, &call_args, "printf_call")
                    .map_err(|e| e.to_string())?;

                Ok(self.context.f64_type().const_float(0.0))
            }
            ("print", _) => self.gen_print(args),
            _ => {
                let f = if let Some(f) = self.functions.get(name) {
                    *f
                } else if let Some(f) = self.imported_functions.get(name) {
                    *f
                } else if let Some(f) = self.module.get_function(name) {
                    f
                } else {
                    return Err(format!(
                        "Unknown function {}, Available Functions: {}",
                        name,
                        self.functions
                            .keys()
                            .map(|k| k.to_string())
                            .collect::<Vec<_>>()
                            .join(", ")
                    ));
                };

                let expected_args = f.count_params() as usize;
                if expected_args != args.len() {
                    return Err(format!(
                        "Function '{}' expects {} arguments but got {}",
                        name,
                        expected_args,
                        args.len()
                    ));
                }

                let compiled_args: Vec<BasicMetadataValueEnum<'ctx>> = args
                    .iter()
                    .map(|arg| self.gen_expr(arg))
                    .collect::<Result<Vec<_>, _>>()?
                    .into_iter()
                    .map(|val| val.into())
                    .collect();

                let call_result = self
                    .builder
                    .build_call(f, &compiled_args, "calltmp")
                    .map_err(|e| e.to_string())?;

                match call_result.try_as_basic_value().left() {
                    Some(value) => Ok(value.into_float_value()),
                    None => Ok(self.context.f64_type().const_float(0.0)),
                }
            }
        }
    }

    pub fn compile_function(
        &mut self,
        name: &str,
        params: &[String],
        body: &[Statement],
    ) -> Result<FunctionValue<'ctx>, String> {
        let param_types: Vec<_> = params
            .iter()
            .map(|p| {
                if p == "name" {
                    self.context.ptr_type(AddressSpace::default()).into()
                } else {
                    self.context.f64_type().into()
                }
            })
            .collect();
        let fn_type = self.context.f64_type().fn_type(&param_types, false);
        let function = self.module.add_function(name, fn_type, None);

        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);

        self.variables.clear();
        for (i, param_name) in params.iter().enumerate() {
            let param = function.get_nth_param(i as u32).unwrap();
            let ptr = if param_name == "name" {
                self.builder
                    .build_alloca(self.context.ptr_type(AddressSpace::default()), param_name)
                    .map_err(|e| e.to_string())?
            } else {
                self.builder
                    .build_alloca(self.context.f64_type(), param_name)
                    .map_err(|e| e.to_string())?
            };
            self.builder
                .build_store(ptr, param)
                .map_err(|e| e.to_string())?;
            self.variables.insert(param_name.clone(), ptr);
        }

        let mut last_value = self.context.f64_type().const_float(0.0);
        for stmt in body {
            if let Some(val) = self.gen_statement(stmt)? {
                last_value = val;
            }
        }

        self.builder
            .build_return(Some(&last_value))
            .map_err(|e| e.to_string())?;

        Ok(function)
    }

    pub fn register_function(&mut self, original_name: String, function: FunctionValue<'ctx>) {
        let generated_name = function.get_name().to_string_lossy().into_owned();
        self.functions.insert(original_name.clone(), function);
        self.original_to_generated.insert(original_name.clone(), generated_name.clone());
        self.generated_to_original.insert(generated_name, original_name);
    }

}
