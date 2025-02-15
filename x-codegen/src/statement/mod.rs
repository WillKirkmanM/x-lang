use crate::CodeGen;
use inkwell::values::FloatValue;
use x_ast::Statement;

impl<'ctx> CodeGen<'ctx> {
    pub(crate) fn gen_statement(&mut self, stmt: &Statement) -> Result<Option<FloatValue<'ctx>>, String> {
        match stmt {
            Statement::Expression { expr } => {
                Ok(Some(self.gen_expr(expr)?))
            },
            Statement::VariableDecl { name, value } => {
                let val = self.gen_expr(value)?;
                self.variables.insert(name.clone(), val);
                Ok(None)
            },
            Statement::Import { module, item } => {
                self.process_import(module, item)?;
                Ok(None)
            },
        }
    }
}