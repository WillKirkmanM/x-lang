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
        if name == "print" && args.first().map_or(false, |arg| matches!(arg, Expr::String(_))) {
            if let Some(Expr::String(string_literal)) = args.first() {
                return self.gen_print_str(string_literal);
            }
        }

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

        let mut compiled_args: Vec<BasicMetadataValueEnum<'ctx>> = Vec::new();
        
        for (i, arg) in args.iter().enumerate() {
            match arg {
                Expr::String(string_literal) => {
                    let s = string_literal.parts.iter()
                        .map(|p| p.to_string())
                        .collect::<String>();
                    
                    let global_str = self.builder
                        .build_global_string_ptr(&s, &format!("str_arg_{}", i))
                        .map_err(|e| e.to_string())?;
                    
                    compiled_args.push(global_str.as_pointer_value().into());
                    
                    if let Some(param_name) = f.get_nth_param(i as u32)
                        .map(|p| p.get_name().to_string_lossy().into_owned())
                    {
                        self.variable_types.insert(param_name, "string".to_string());
                    }
                },
                Expr::Identifier(var_name) => {
                    if let Some(&ptr) = self.variables.get(var_name) {
                        if self.variable_types.get(var_name).map_or(false, |t| t == "string") {
                            let val = self.builder
                                .build_load(self.context.ptr_type(AddressSpace::default()), ptr, var_name)
                                .map_err(|e| e.to_string())?;
                            compiled_args.push(val.into());
                            
                            if let Some(param_name) = f.get_nth_param(i as u32)
                                .map(|p| p.get_name().to_string_lossy().into_owned())
                            {
                                self.variable_types.insert(param_name, "string".to_string());
                            }
                        } else {
                            let val = self.builder
                                .build_load(self.context.f64_type(), ptr, var_name)
                                .map_err(|e| e.to_string())?;
                            compiled_args.push(val.into());
                        }
                    } else {
                        return Err(format!("Unknown variable: {}", var_name));
                    }
                },
                _ => {
                    compiled_args.push(self.gen_expr(arg)?.into());
                }
            }
        }

        let call_result = self.builder
            .build_call(f, &compiled_args, "calltmp")
            .map_err(|e| e.to_string())?;

        match call_result.try_as_basic_value().left() {
            Some(value) => Ok(value.into_float_value()),
            None => Ok(self.context.f64_type().const_float(0.0)),
        }
    }

    pub fn compile_function(
        &mut self,
        name: &str,
        params: &[String],
        body: &[Statement],
    ) -> Result<FunctionValue<'ctx>, String> {
        let mut param_is_string = Vec::with_capacity(params.len());
        
        for param in params {
            let is_string = param == "source" || param == "target" || param == "auxiliary" ||
                            param.contains("str") || param.contains("name") || 
                            param.contains("path");
            param_is_string.push(is_string);
        }
        
        let param_types: Vec<_> = param_is_string
            .iter()
            .map(|&is_string| {
                if is_string {
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
            let is_string = param_is_string[i];
            
            if is_string {
                self.variable_types.insert(param_name.clone(), "string".to_string());
            }
            
            let ptr = if is_string {
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
