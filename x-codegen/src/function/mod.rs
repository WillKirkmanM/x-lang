use inkwell::values::{FloatValue, FunctionValue};
use x_ast::{Expr, Statement};
use inkwell::values::BasicValueEnum;

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
        args: &[String],
        body: &Statement,
    ) -> Result<FunctionValue<'ctx>, String> {
        let f64_type = self.context.f64_type();
        let arg_types = vec![f64_type.into(); args.len()];
        let fn_type = f64_type.fn_type(&arg_types, false);
    
        let function = self.module.add_function(name, fn_type, None);
        let basic_block = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(basic_block);
    
        let old_vars = self.variables.clone();
        for (i, arg) in args.iter().enumerate() {
            let arg_value = function.get_nth_param(i as u32).unwrap().into_float_value();
            let arg_alloca = self.create_entry_block_alloca(function, arg);
            self.builder.build_store(arg_alloca, arg_value).unwrap();
            self.variables.insert(arg.clone(), arg_value);
        }
    
        // Handle both expressions and blocks
        let return_value = match body {
            Statement::Expression { expr } => self.gen_expr(expr)?,
            Statement::Block { statements } => {
                let mut last_value = self.context.f64_type().const_float(0.0);
                for stmt in statements {
                    if let Some(val) = self.gen_statement(stmt)? {
                        last_value = val;
                    }
                }
                last_value
            }
            _ => return Err("Function body must be an expression or block".to_string()),
        };
    
        self.builder.build_return(Some(&return_value)).unwrap();
        self.variables = old_vars;
    
        if function.verify(true) {
            self.register_function(name.to_string(), function);
            Ok(function)
        } else {
            Err("Invalid generated function.".to_string())
        }
    }

}