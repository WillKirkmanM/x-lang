use inkwell::values::{BasicMetadataValueEnum, FunctionValue};
use x_ast::Expr;

use crate::CodeGen;

impl<'ctx> CodeGen<'ctx> {
    pub(crate) fn gen_become(&mut self, expr: &Expr) -> Result<(), String> {
        let current_function: FunctionValue<'ctx> = self.current_function.ok_or_else(|| {
            "Internal Error: 'become' used outside of a function context.".to_string()
        })?;

        let (call_name, call_args) = match expr {
            Expr::FunctionCall { name, args } => (name.clone(), args.clone()),
            _ => return Err("'become' must be followed by a direct function call.".to_string()),
        };

        let current_name = current_function
            .get_name()
            .to_str()
            .map_err(|_| "Internal Error: failed to read current function name.".to_string())?;
        if call_name != current_name {
            return Err(format!(
                "'become' can only be used for recursive calls to the current function ('{}').",
                current_name
            ));
        }

        let new_arg_values: Vec<BasicMetadataValueEnum<'ctx>> = call_args
            .iter()
            .map(|arg_expr| {
                self.gen_expr(arg_expr)
                    .map(|v| BasicMetadataValueEnum::from(v))
            })
            .collect::<Result<Vec<BasicMetadataValueEnum<'ctx>>, String>>()?;

        let call_site = self
            .builder
            .build_call(current_function, &new_arg_values, "become_call")
            .unwrap();
        call_site.set_tail_call(true);

        if let Some(bv) = call_site.try_as_basic_value().left() {
            let _ = self.builder.build_return(Some(&bv));
        } else {
            let _ = self.builder.build_return(None);
        }

        Ok(())
    }
}
