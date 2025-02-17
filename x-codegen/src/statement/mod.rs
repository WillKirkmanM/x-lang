use crate::CodeGen;
use inkwell::values::FloatValue;
use x_ast::Statement;

impl<'ctx> CodeGen<'ctx> {
    pub(crate) fn gen_statement(
        &mut self,
        stmt: &Statement,
    ) -> Result<Option<FloatValue<'ctx>>, String> {
        match stmt {
            Statement::Expression { expr } => Ok(Some(self.gen_expr(expr)?.into_float_value())),
            Statement::VariableDecl { name, value } => {
                let alloca = self
                    .builder
                    .build_alloca(self.context.f64_type(), name)
                    .map_err(|e| e.to_string())?;

                let val = self.gen_expr(value)?;
                self.builder
                    .build_store(alloca, val)
                    .map_err(|e| e.to_string())?;

                self.variables.insert(name.clone(), alloca);

                Ok(None)
            }
            Statement::Import { module, item } => {
                self.process_import(module, item)?;
                Ok(None)
            }
            Statement::Function { name, params, body } => {
                self.compile_function(name, params, body)?;
                Ok(None)
            }
            Statement::Block { statements } => {
                let mut last_value = None;
                for statement in statements {
                    if let Some(val) = self.gen_statement(statement)? {
                        last_value = Some(val);
                    }
                }
                Ok(last_value)
            }
            Statement::Comment(_) => Ok(None),
            Statement::ForLoop { var, start, end, body } => {
                self.gen_for_loop(var, start, end, body)
            },
            Statement::If { condition, then_block, else_block } => {
                self.gen_if(condition, then_block, else_block)
            },
        }
    }
}
