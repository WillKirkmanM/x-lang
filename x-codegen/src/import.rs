use inkwell::module::Linkage;
use std::collections::HashSet;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::{Mutex, OnceLock};
use x_ast::Statement;
use x_parser::parse;

use crate::CodeGen;

impl<'ctx> CodeGen<'ctx> {
    pub(crate) fn process_file_import(&mut self, path: &str) -> Result<(), String> {
        static IMPORTED_FILES: OnceLock<Mutex<HashSet<PathBuf>>> = OnceLock::new();

        let imported_files_lock = IMPORTED_FILES.get_or_init(|| Mutex::new(HashSet::new()));
        let mut imported_files = imported_files_lock
            .lock()
            .map_err(|_| "Failed to lock imported files mutex".to_string())?;

        let import_path = Path::new(path);
        let canonical_path = match import_path.canonicalize() {
            Ok(p) => p,
            Err(_) => {
                let path_with_ext = format!("{}.x", path);
                let import_path_with_ext = Path::new(&path_with_ext);
                match import_path_with_ext.canonicalize() {
                    Ok(p) => p,
                    Err(e) => {
                        return Err(format!("Failed to resolve import path: {}: {}", path, e))
                    }
                }
            }
        };

        if imported_files.contains(&canonical_path) {
            return Ok(());
        }

        imported_files.insert(canonical_path.clone());

        let source = match fs::read_to_string(&canonical_path) {
            Ok(content) => content,
            Err(e) => return Err(format!("Failed to read imported file: {}: {}", path, e)),
        };

        let program = match parse(&source) {
            Ok(p) => p,
            Err(e) => return Err(format!("Failed to parse imported file: {}: {}", path, e)),
        };

        for stmt in &program.statements {
            match stmt {
                Statement::Import { module, item } => {
                    self.process_import(module, item)?;
                }
                Statement::FileImport { path: import_path } => {
                    self.process_file_import(import_path)?;
                }
                _ => {}
            }
        }

        for stmt in &program.statements {
            match stmt {
                Statement::Function {
                    name,
                    params,
                    body,
                    is_pure,
                    is_memoised,
                    return_type: _return_type,
                    generic_params: _generic_params,
                    is_multi: _is_multi,
                    is_throws: _is_throws,
                } => {
                    self.compile_function(
                        name,
                        params
                            .iter()
                            .map(|p| (p.name.clone(), p.ty.clone()))
                            .collect(),
                        &body.clone().unwrap(),
                        *is_pure,
                        *is_memoised,
                    )?;
                }
                _ => {}
            }
        }

        Ok(())
    }

    pub(crate) fn process_import(&mut self, module: &str, item: &str) -> Result<(), String> {
        match (module, item) {
            ("std", "print") => {
                let std_print = self.stdlib.get_print();
                let imported = if let Some(f) = self.module.get_function("print") {
                    f
                } else {
                    let ty = std_print.get_type();
                    self.module
                        .add_function("print", ty, Some(Linkage::External))
                };
                self.imported_functions
                    .insert("print".to_string(), imported);

                let std_print_str = self.stdlib.get_print_str();
                let imported_str = if let Some(f) = self.module.get_function("print_str") {
                    f
                } else {
                    let ty2 = std_print_str.get_type();
                    self.module
                        .add_function("print_str", ty2, Some(Linkage::External))
                };
                self.imported_functions
                    .insert("print_str".to_string(), imported_str);

                Ok(())
            }
            _ => Err(format!("Unknown import: {}::{}", module, item)),
        }
    }

    pub(crate) fn process_import_statements(
        &mut self,
        statements: &[Statement],
    ) -> Result<(), String> {
        for stmt in statements {
            if let Statement::Import { module, item } = stmt {
                self.process_import(module, item)?;
            }
        }
        Ok(())
    }
}
