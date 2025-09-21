use std::collections::HashMap;

use x_ast::{Program, Statement, TraitDef};

use crate::CodeGen;

impl<'ctx> CodeGen<'ctx> {
    /// Scans the AST for TraitDecl statements to populate the traits map.
    pub fn register_traits<'a>(&mut self, ast: &Program) {
        for stmt in &ast.statements {
            if let Statement::TraitDecl(trait_def) = stmt {
                let methods: HashMap<String, Option<Statement>> = trait_def
                    .methods
                    .iter()
                    .map(|(name, stmt_opt)| (name.clone(), stmt_opt.clone()))
                    .collect();

                let td = TraitDef {
                    name: trait_def.name.clone(),
                    methods,
                };

                self.traits.insert(trait_def.name.clone(), td);
            }
        }
    }
}
