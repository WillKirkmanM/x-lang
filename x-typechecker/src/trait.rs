use std::collections::HashMap;

use x_ast::Statement;

use crate::{FunctionSignature, TraitDef, TypeChecker};

impl TypeChecker {
    pub fn register_traits_and_impls(&mut self) -> Result<(), String> {
        for stmt in &self.program.statements {
            match stmt {
                Statement::TraitDecl(trait_def) => {
                    self.symbols
                        .trait_ast_defs
                        .insert(trait_def.name.clone(), trait_def.clone());

                    let mut trait_methods = HashMap::new();

                    for (method_name, method_opt_stmt) in &trait_def.methods {
                        if let Some(Statement::Function {
                            params,
                            return_type,
                            ..
                        }) = method_opt_stmt
                        {
                            let sig = FunctionSignature {
                                param_types: params.iter().map(|(_, t)| t.clone()).collect(),
                                return_type: return_type.clone(),
                            };
                            trait_methods.insert(method_name.clone(), sig);
                        }
                    }

                    let new_trait_def = TraitDef {
                        name: trait_def.name.clone(),
                        methods: trait_methods,
                    };
                    self.symbols
                        .traits
                        .insert(trait_def.name.clone(), new_trait_def);
                }
                Statement::ImplDecl(impl_def) => {
                    self.symbols
                        .impls_for_type
                        .entry(impl_def.type_name.clone())
                        .or_default()
                        .push(impl_def.trait_name.clone());
                }
                _ => {}
            }
        }
        Ok(())
    }
}
