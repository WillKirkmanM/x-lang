use inkwell::values::FloatValue;
use x_ast::Expr;
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

}