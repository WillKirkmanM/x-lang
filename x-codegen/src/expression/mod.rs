use crate::CodeGen;
use inkwell::values::FloatValue;
use x_ast::{Expr, Operator};

impl<'ctx> CodeGen<'ctx> {
    pub(crate) fn gen_expr(&self, expr: &Expr) -> Result<FloatValue<'ctx>, String> {
        match expr {
            Expr::Number(n) => {
                Ok(self.context.f64_type().const_float(*n as f64))
            },
            Expr::Identifier(name) => {
                self.variables.get(name)
                    .cloned()
                    .ok_or_else(|| format!("Undefined variable: {}", name))
            },
            Expr::BinaryOp { left, op, right } => {
                let l = self.gen_expr(left)?;
                let r = self.gen_expr(right)?;
                
                match op {
                    Operator::Add => self.builder.build_float_add(l, r, "addtmp"),
                    Operator::Subtract => self.builder.build_float_sub(l, r, "subtmp"),
                    Operator::Multiply => self.builder.build_float_mul(l, r, "multmp"),
                    Operator::Divide => self.builder.build_float_div(l, r, "divtmp"),
                }.map_err(|e| e.to_string())
            },
            Expr::FunctionCall { name, args } => self.gen_function_call(name, args),
            Expr::String(_) => {
                Err("String literals can only be used in print function calls".to_string())
            },
        }
    }

    fn gen_function_call(&self, name: &str, args: &[Expr]) -> Result<FloatValue<'ctx>, String> {
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
    
                let compiled_args: Vec<_> = args.iter()
                    .map(|arg| self.gen_expr(arg))
                    .collect::<Result<Vec<_>, _>>()?
                    .into_iter()
                    .map(|val| val.into())
                    .collect();
    
                let argslen = f.count_params() as usize;
                if argslen != args.len() {
                    return Err(format!("Expected {} arguments, got {}", argslen, args.len()));
                }
    
                match self.builder.build_call(f, &compiled_args, "calltmp")
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left() {
                    Some(value) => Ok(value.into_float_value()),
                    None => Err("Invalid call produced void value".to_string())
                }
            }
        }
    }
    fn gen_print_str(&self, args: &[Expr]) -> Result<FloatValue<'ctx>, String> {
        if let Some(print_str_fn) = self.imported_functions.get("print_str") {
            let string_ptr = if let Expr::String(s) = &args[0] {
                self.builder
                    .build_global_string_ptr(s, "str")
                    .map_err(|e| e.to_string())?
                    .as_pointer_value()
            } else {
                return Err("Expected string argument".to_string());
            };

            self.builder.build_call(
                *print_str_fn,
                &[string_ptr.into()],
                "calltmp"
            ).map_err(|e| e.to_string())?;

            Ok(self.context.f64_type().const_float(0.0))
        } else {
            Err("print_str function not imported".to_string())
        }
    }

    fn gen_print(&self, args: &[Expr]) -> Result<FloatValue<'ctx>, String> {
        if let Some(print_fn) = self.imported_functions.get("print") {
            let compiled_args: Vec<_> = args.iter()
                .map(|arg| self.gen_expr(arg))
                .collect::<Result<Vec<_>, _>>()?
                .into_iter()
                .map(|val| val.into())
                .collect();

            Ok(self.builder.build_call(
                *print_fn,
                &compiled_args,
                "calltmp"
            ).map_err(|e| e.to_string())?
             .try_as_basic_value()
             .left()
             .unwrap()
             .into_float_value())
        } else {
            Err("print function not imported".to_string())
        }
    }
}