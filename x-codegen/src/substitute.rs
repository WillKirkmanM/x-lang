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
                    self.substitute_in_expr(expr, type_map);
                }
            }
            _ => {}
        }
        Ok(())
    }

    pub fn substitute_in_expr(&self, expr: &mut Expr, type_map: &HashMap<String, Type>) {
        match expr {
            Expr::BinaryOp { left, right, .. } => {
                self.substitute_in_expr(left, type_map);
                self.substitute_in_expr(right, type_map);
            }
            Expr::UnaryOp { expr, .. } => {
                self.substitute_in_expr(expr, type_map);
            }
            Expr::FunctionCall { args, .. } => {
                for arg in args {
                    self.substitute_in_expr(arg, type_map);
                }
            }
            Expr::StructInstantiate(init) => {
                init.name = self
                    .substitute_type(&Type::Custom(init.name.clone()), type_map)
                    .to_string();
                for (_, field_expr) in &mut init.fields {
                    self.substitute_in_expr(field_expr, type_map);
                }
            }
            Expr::FieldAccess { object, .. } => {
                self.substitute_in_expr(object, type_map);
            }
            Expr::ArrayAccess { array, index } => {
                self.substitute_in_expr(array, type_map);
                self.substitute_in_expr(index, type_map);
            }
            _ => {}
        }
    }
}
