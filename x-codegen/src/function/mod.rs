use inkwell::values::{FloatValue, FunctionValue};
use x_ast::{Expr, Statement};

use crate::CodeGen;

impl<'ctx> CodeGen<'ctx> {
    pub fn gen_function_call(&self, name: &str, args: &[Expr]) -> Result<FloatValue<'ctx>, String> {
        match (name, args.first()) {
            ("print", Some(Expr::String(_))) => self.gen_print_str(args),
            ("print", _) => self.gen_print(args),
            _ => {
                let f = if let Some(f) = self.functions.get(name) {
                    *f
                } else if let Some(f) = self.imported_functions.get(name) {
                    *f
                } else if let Some(f) = self.module.get_function(name) {
                    f
                } else {
                    return Err(format!("Unknown function {}", name));
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

                let compiled_args: Vec<_> = args.iter()
                    .map(|arg| self.gen_expr(arg))
                    .collect::<Result<Vec<_>, _>>()?
                    .into_iter()
                    .map(|val| val.into())
                    .collect();

                let call_site = self.builder
                    .build_call(f, &compiled_args, "calltmp")
                    .map_err(|e| e.to_string())?;

                match call_site.try_as_basic_value().left() {
                    Some(value) => Ok(value.into_float_value()),
                    None => {
                        Ok(self.context.f64_type().const_float(0.0))
                    }
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
        let f64_type = self.context.f64_type();
        let fn_type = f64_type.fn_type(&vec![f64_type.into(); params.len()], false);
        let function = self.module.add_function(name, fn_type, None);
        
        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);
        
        let old_vars = self.variables.clone();
        
        for (i, param) in params.iter().enumerate() {
            let alloca = self.builder.build_alloca(f64_type, param)
                .map_err(|e| e.to_string())?;
            
            if let Some(param_value) = function.get_nth_param(i as u32) {
                self.builder.build_store(alloca, param_value)
                    .map_err(|e| e.to_string())?;
            }
            
            self.variables.insert(param.clone(), alloca);
        }
        
        let mut last_value = self.context.f64_type().const_float(0.0);
        for stmt in body {
            if let Some(val) = self.gen_statement(stmt)? {
                last_value = val;
            }
        }
        
        self.builder.build_return(Some(&last_value))
            .map_err(|e| e.to_string())?;
        
        self.variables = old_vars;
        
        Ok(function)
    }
}