use std::collections::HashMap;

use x_ast::{Expr, Statement, Type};

use crate::TypeChecker;

impl TypeChecker {
    /// Recursively walks a statement and substitutes all type parameters.
    pub fn substitute_types_in_statement(
        &mut self,
        stmt: &mut Statement,
        type_map: &HashMap<String, Type>,
    ) {
        match stmt {
            Statement::VariableDecl {
                type_ann, value, ..
            } => {
                if let Some(ann) = type_ann {
                    *type_ann = Some(self.substitute_type(ann, type_map));
                }
                self.substitute_types_in_expr(value, type_map);
            }
            Statement::Function {
                body,
                return_type,
                params,
                ..
            } => {
                // For nested functions
                // params: &mut Vec<(String, Type)>, return_type: &mut Type
                for (_, p_type) in params.iter_mut() {
                    *p_type = self.substitute_type(&*p_type, type_map);
                }
                *return_type = self.substitute_type(&*return_type, type_map);
                for s in body.as_mut().unwrap().iter_mut() {
                    self.substitute_types_in_statement(s, type_map);
                }
            }
            Statement::Expression { expr } => self.substitute_types_in_expr(expr, type_map),
            Statement::Block { statements } => {
                for s in statements {
                    self.substitute_types_in_statement(s, type_map);
                }
            }
            Statement::Return { value } => {
                if let Some(v) = value {
                    self.substitute_types_in_expr(v, type_map);
                }
            }
            Statement::If {
                condition,
                then_block,
                else_block,
            } => {
                self.substitute_types_in_expr(condition, type_map);
                for s in then_block {
                    self.substitute_types_in_statement(s, type_map);
                }
                if let Some(else_b) = else_block {
                    for s in else_b {
                        self.substitute_types_in_statement(s, type_map);
                    }
                }
            }
            Statement::WhileLoop { condition, body } => {
                self.substitute_types_in_expr(condition, type_map);
                for s in body {
                    self.substitute_types_in_statement(s, type_map);
                }
            }
            _ => {}
        }
    }

    /// Recursively walks an expression and substitutes all type parameters.
    fn substitute_types_in_expr(&mut self, expr: &mut Expr, type_map: &HashMap<String, Type>) {
        match expr {
            Expr::BinaryOp { left, right, .. } => {
                self.substitute_types_in_expr(left, type_map);
                self.substitute_types_in_expr(right, type_map);
            }
            Expr::UnaryOp { expr, .. } => {
                self.substitute_types_in_expr(expr, type_map);
            }
            Expr::FunctionCall { args, .. } => {
                for arg in args {
                    self.substitute_types_in_expr(arg, type_map);
                }
            }
            Expr::StructInstantiate(init) => {
                for (_, field_expr) in &mut init.fields {
                    self.substitute_types_in_expr(field_expr, type_map);
                }
            }
            Expr::Array(elements) => {
                for el in elements {
                    self.substitute_types_in_expr(el, type_map);
                }
            }
            Expr::ArrayAccess { array, index } => {
                self.substitute_types_in_expr(array, type_map);
                self.substitute_types_in_expr(index, type_map);
            }
            Expr::FieldAccess { object, .. } => {
                self.substitute_types_in_expr(object, type_map);
            }
            // Literals and identifiers are base cases with nothing to substitute inside.
            Expr::Int(_)
            | Expr::Float(_)
            | Expr::String(_)
            | Expr::Boolean(_)
            | Expr::Identifier(_) => {}
            _ => {}
        }
    }

    /// Recursively substitutes type parameters (like T) with concrete types (like i32).
    pub fn substitute_type(&mut self, ty: &Type, map: &HashMap<String, Type>) -> Type {
        match ty {
            // Map generic identifiers represented as Custom("T")
            Type::Custom(name) => {
                if let Some(concrete) = map.get(name) {
                    return concrete.clone();
                }
                Type::Custom(name.clone())
            }
            // Map explicit type parameters
            Type::TypeParameter(name) => map.get(name).cloned().unwrap_or_else(|| ty.clone()),
            Type::GenericInstance { name, type_args } => {
                let concrete_args: Vec<Type> = type_args
                    .iter()
                    .map(|t| self.substitute_type(t, map))
                    .collect();
                if self.symbols.generic_structs.contains_key(name) {
                    if let Ok(mangled) = self.instantiate_struct(name, &concrete_args) {
                        return Type::Custom(mangled);
                    }
                }
                Type::GenericInstance {
                    name: name.clone(),
                    type_args: concrete_args,
                }
            }
            Type::Array(inner) => Type::Array(Box::new(self.substitute_type(inner, map))),
            _ => ty.clone(),
        }
    }
}
