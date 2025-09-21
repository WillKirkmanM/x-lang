use std::{fs, path::Path};

use x_ast::{Expr, Statement, Type};

use crate::TypeChecker;

impl TypeChecker {
    pub fn collect_all_statements(
        &mut self,
        statements: Vec<Statement>,
        current_dir: &Path,
    ) -> Result<Vec<Statement>, String> {
        let mut collected = Vec::new();

        for stmt in statements {
            if let Statement::FileImport { path } = stmt {
                let import_path = current_dir.join(&path);
                let canonical_path = match import_path.canonicalize() {
                    Ok(p) => p.to_string_lossy().into_owned(),
                    Err(e) => return Err(format!("Failed to find import file '{}': {}", path, e)),
                };

                if self.processed_files.insert(canonical_path.clone()) {
                    println!("[TypeChecker] Processing import: {}", canonical_path);

                    let source = fs::read_to_string(&canonical_path)
                        .map_err(|e| format!("Failed to read import file '{}': {}", path, e))?;

                    let imported_program = x_parser::parse(&source)
                        .map_err(|e| format!("Failed to parse import file '{}': {:?}", path, e))?;

                    let parent_dir = Path::new(&canonical_path)
                        .parent()
                        .unwrap_or_else(|| Path::new("."));
                    let mut nested_statements =
                        self.collect_all_statements(imported_program.statements, parent_dir)?;
                    collected.append(&mut nested_statements);
                }
            } else {
                collected.push(stmt);
            }
        }
        Ok(collected)
    }

    pub fn check_statement(&mut self, stmt: &mut Statement) -> Result<(), String> {
        match stmt {
            Statement::VariableDecl {
                name,
                type_ann,
                value,
            } => {
                if let Some(ann) = type_ann {
                    *type_ann = Some(self.resolve_type(ann)?);
                }
                if let Some(Type::Custom(mangled_name)) = type_ann {
                    if let Expr::StructInstantiate(init) = &mut *value {
                        if self.symbols.generic_structs.contains_key(&init.name) {
                            init.name = mangled_name.clone();
                        }
                    }
                }

                if let Expr::TypeLiteral(t) = value {
                    let resolved = self.resolve_type(t)?;
                    *value = Expr::TypeLiteral(resolved);
                }

                let value_type = self.check_expression(value)?;
                let final_type = type_ann.clone().unwrap_or(value_type);
                if type_ann.is_none() {
                    *type_ann = Some(final_type.clone());
                }
                self.symbols.add_variable(name.clone(), final_type);
            }
            Statement::Function {
                body,
                params,
                return_type,
                ..
            } => {
                self.symbols.enter_scope();
                for (pname, ptype) in params.iter() {
                    self.symbols.add_variable(pname.clone(), ptype.clone());
                }
                for st in body.as_mut().unwrap().iter_mut() {
                    self.check_statement(st)?;
                }
                self.symbols.exit_scope();

                *return_type = self.resolve_type(return_type)?;
            }
            Statement::Expression { expr } => {
                self.check_expression(expr)?;
            }
            _ => {}
        }
        Ok(())
    }
}
