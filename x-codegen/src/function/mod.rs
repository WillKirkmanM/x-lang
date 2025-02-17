use inkwell::values::FloatValue;
use x_ast::Expr;

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
}