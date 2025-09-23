use std::collections::HashMap;

use x_ast::{Expr, Statement, StringPart, Type};

use crate::CodeGen;

impl<'ctx> CodeGen<'ctx> {
    /// This function is called during an initial analysis pass to find functions with `static` params.
    pub fn collect_specialisable_functions(&mut self, statements: &[Statement]) {
        for stmt in statements {
            if let Statement::Function { name, params, .. } = stmt {
                if params.iter().any(|p| p.is_static) {
                    self.specialisable_functions
                        .insert(name.clone(), stmt.clone());
                }
            }
        }
    }

    pub fn mangle_specialised_name(
        &self,
        name: &str,
        params: &[x_ast::Param],
        args: &[Expr],
    ) -> String {
        // Helper to sanitise pieces so the mangled name contains only safe characters.
        fn sanitize(s: &str) -> String {
            s.chars()
                .map(|c| if c.is_ascii_alphanumeric() { c } else { '_' })
                .collect()
        }

        let mut parts: Vec<String> = Vec::new();
        parts.push(name.to_string());

        for (i, arg) in args.iter().enumerate() {
            // If parameter info exists, only include those marked static; otherwise include by default.
            let include = params.get(i).map(|p| p.is_static).unwrap_or(true);
            if !include {
                continue;
            }

            // Convert argument expression into a compact string representation.
            let piece = match arg {
                Expr::Int(v) => v.to_string(),
                Expr::Float(v) => v.to_string(),
                Expr::Boolean(b) => b.to_string(),
                Expr::String(lit) => {
                    // Join text parts; interpolations are represented by a placeholder.
                    lit.parts
                        .iter()
                        .map(|p| match p {
                            StringPart::Text(t) => t.clone(),
                            StringPart::Interpolation(_) => "{interp}".to_string(),
                        })
                        .collect::<String>()
                }
                other => format!("{:?}", other),
            };

            parts.push(sanitize(&piece));
        }

        parts.join("_")
    }

    /// Creates a new, specialised version of a function by replacing its static
    /// parameters with constant values at the AST level.
    pub fn specialise_and_compile_function(
        &mut self,
        mangled_name: &str,
        generic_fn_stmt: &Statement,
        static_args: &HashMap<String, Expr>,
    ) -> Result<(), String> {
        let mut specialised_fn_stmt = generic_fn_stmt.clone();

        if let Statement::Function {
            name,
            body,
            params,
            return_type,
            is_pure,
            is_memoised,
            ..
        } = &mut specialised_fn_stmt
        {
            *name = mangled_name.to_string();

            for stmt in body.as_mut().unwrap().iter_mut() {
                self.substitute_static_params_in_statement(stmt, static_args);
            }

            let param_sigs: Vec<(String, Type)> = params
                .iter()
                .map(|p| (p.name.clone(), p.ty.clone()))
                .collect();
            self.declare_function_signature(name, &param_sigs, return_type, None)?;

            self.compile_function(
                name,
                param_sigs,
                body.as_ref().unwrap(),
                *is_pure,
                *is_memoised,
            )?;
        } else {
            return Err("Expected a Function statement for specialisation".to_string());
        }

        Ok(())
    }

    /// Recursively walks a statement and substitutes all static parameter uses with constant expressions.
    fn substitute_static_params_in_statement(
        &self,
        stmt: &mut Statement,
        constants: &HashMap<String, Expr>,
    ) {
        match stmt {
            Statement::Expression { expr } => {
                self.substitute_static_params_in_expr(expr, constants)
            }
            Statement::VariableDecl { value, .. } => {
                self.substitute_static_params_in_expr(value, constants)
            }
            Statement::Return { value: Some(expr) } => {
                self.substitute_static_params_in_expr(expr, constants)
            }
            Statement::Block { statements } => {
                for s in statements {
                    self.substitute_static_params_in_statement(s, constants);
                }
            }
            Statement::If {
                condition,
                then_block,
                else_block,
            } => {
                self.substitute_static_params_in_expr(condition, constants);
                for s in then_block {
                    self.substitute_static_params_in_statement(s, constants);
                }
                if let Some(eb) = else_block {
                    for s in eb {
                        self.substitute_static_params_in_statement(s, constants);
                    }
                }
            }
            Statement::WhileLoop { condition, body } => {
                self.substitute_static_params_in_expr(condition, constants);
                for s in body {
                    self.substitute_static_params_in_statement(s, constants);
                }
            }
            // Other statements that contain expressions or statements would be handled here.
            _ => {}
        }
    }

    /// Recursively replaces identifiers that match static parameters with constant expressions.
    fn substitute_static_params_in_expr(&self, expr: &mut Expr, constants: &HashMap<String, Expr>) {
        // If the entire expression is an identifier that's a static param, replace it.
        if let Expr::Identifier(name) = expr {
            if let Some(const_expr) = constants.get(name) {
                *expr = const_expr.clone();
                return; // Stop recursion after replacement
            }
        }

        // Otherwise, recurse through the expression tree.
        match expr {
            Expr::BinaryOp { left, right, .. } => {
                self.substitute_static_params_in_expr(left, constants);
                self.substitute_static_params_in_expr(right, constants);
            }
            Expr::UnaryOp { expr, .. } => self.substitute_static_params_in_expr(expr, constants),
            Expr::FunctionCall { args, .. } => {
                for arg in args {
                    self.substitute_static_params_in_expr(arg, constants);
                }
            }
            Expr::StructInstantiate(init) => {
                for (_, field_expr) in &mut init.fields {
                    self.substitute_static_params_in_expr(field_expr, constants);
                }
            }
            Expr::Array(elements) => {
                for el in elements {
                    self.substitute_static_params_in_expr(el, constants);
                }
            }
            Expr::ArrayAccess { array, index } => {
                self.substitute_static_params_in_expr(array, constants);
                self.substitute_static_params_in_expr(index, constants);
            }
            Expr::FieldAccess { object, .. } => {
                self.substitute_static_params_in_expr(object, constants)
            }
            Expr::AddressOf { expr, .. } => self.substitute_static_params_in_expr(expr, constants),
            Expr::Deref { expr } => self.substitute_static_params_in_expr(expr, constants),
            // Base cases like literals and already-replaced identifiers have no sub-expressions.
            _ => {}
        }
    }
}
