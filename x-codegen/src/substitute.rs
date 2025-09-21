use std::collections::HashMap;

use x_ast::{Expr, Statement, StructDef, Type};

use crate::CodeGen;

impl<'ctx> CodeGen<'ctx> {
    pub fn substitute_in_statement(
        &self,
        stmt: &mut Statement,
        type_map: &HashMap<String, Type>,
        _gen_structs: &HashMap<String, StructDef>,
        _concrete_statements: &mut Vec<Statement>,
    ) -> Result<(), String> {
        match stmt {
            Statement::Return { value } => {
                if let Some(expr) = value {
                    self.substitute_in_expr(expr, type_map)?;
                }
            }
            _ => {}
        }
        Ok(())
    }

    fn substitute_in_expr(
        &self,
        _expr: &mut Expr,
        _type_map: &HashMap<String, Type>,
    ) -> Result<(), String> {
        Ok(())
    }
}
