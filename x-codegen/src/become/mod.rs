use x_ast::Expr;
use inkwell::values::{BasicValue, BasicMetadataValueEnum};

use crate::CodeGen;

impl<'ctx> CodeGen<'ctx> {
    pub(crate) fn gen_become(&mut self, call: &Expr) -> Result<(), String> {
        let current_function = self.current_function.ok_or_else(|| {
            "Internal Error: 'become' used outside of a function context.".to_string()
        })?;

        let (name, args) = match call {
            Expr::FunctionCall { name, args } => (name, args),
            _ => return Err("'become' must be followed by a direct function call.".to_string()),
        };

        if name != current_function.get_name().to_str().unwrap() {
            return Err(format!(
                "'become' can only be used for recursive calls to the current function ('{}').",
                current_function.get_name().to_str().unwrap()
            ));
        }

        let new_arg_values: Vec<BasicMetadataValueEnum<'ctx>> = args
            .iter()
            .map(|arg_expr| self.gen_expr(arg_expr).map(|val| val.into()))
            .collect::<Result<_, _>>()?;

        let call_site = self.builder.build_call(
            current_function,
            &new_arg_values,
            "become_call"
        ).unwrap();

        call_site.set_tail_call(true);

        let return_value = call_site.try_as_basic_value().left();
        self.builder.build_return(return_value.as_ref().map(|v| v as &dyn BasicValue)).unwrap();

        Ok(())
    }
}