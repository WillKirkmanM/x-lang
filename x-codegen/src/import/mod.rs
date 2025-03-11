mod file_import;

use crate::CodeGen;

impl<'ctx> CodeGen<'ctx> {
    pub(crate) fn process_import(&mut self, module: &str, item: &str) -> Result<(), String> {
        match (module, item) {
            ("std", "print") => {
                self.imported_functions.insert(
                    "print".to_string(),
                    self.stdlib.get_print()
                );
                self.imported_functions.insert(
                    "print_str".to_string(),
                    self.stdlib.get_print_str()
                );
                Ok(())
            },
            _ => Err(format!("Unknown import: {}::{}", module, item))
        }
    }
    
    pub(crate) fn process_imports(&mut self, program: &x_ast::Program) -> Result<(), String> {
        for stmt in &program.statements {
            match stmt {
                x_ast::Statement::Import { module, item } => {
                    self.process_import(module, item)?;
                },
                x_ast::Statement::FileImport { path } => {
                    self.process_file_import(path)?;
                },
                _ => {}
            }
        }
        Ok(())
    }
}