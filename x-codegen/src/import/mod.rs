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
}