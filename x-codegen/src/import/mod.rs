mod file_import;

use x_ast::Statement;
use inkwell::module::Linkage;

use crate::CodeGen;

impl<'ctx> CodeGen<'ctx> {
    pub(crate) fn process_import(&mut self, module: &str, item: &str) -> Result<(), String> {
        match (module, item) {
            ("std", "print") => {
                let std_print = self.stdlib.get_print();
                let imported = if let Some(f) = self.module.get_function("print") {
                    f
                } else {
                    let ty = std_print.get_type();
                    self.module.add_function("print", ty, Some(Linkage::External))
                };
                self.imported_functions.insert("print".to_string(), imported);

                let std_print_str = self.stdlib.get_print_str();
                let imported_str = if let Some(f) = self.module.get_function("print_str") {
                    f
                } else {
                    let ty2 = std_print_str.get_type();
                    self.module.add_function("print_str", ty2, Some(Linkage::External))
                };
                self.imported_functions.insert("print_str".to_string(), imported_str);

                Ok(())
            }
            _ => Err(format!("Unknown import: {}::{}", module, item)),
        }
    }
    
    pub(crate) fn process_import_statements(&mut self, statements: &[Statement]) -> Result<(), String> {
        for stmt in statements {
            if let Statement::Import { module, item } = stmt {
                self.process_import(module, item)?;
            }
        }
        Ok(())
    }
}