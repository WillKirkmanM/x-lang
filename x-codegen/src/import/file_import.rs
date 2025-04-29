use std::collections::HashSet;
use std::fs;
use std::path::{Path, PathBuf};
use x_parser::parse;

use crate::CodeGen;

impl<'ctx> CodeGen<'ctx> {
    pub(crate) fn process_file_import(&mut self, path: &str) -> Result<(), String> {
        static mut IMPORTED_FILES: Option<HashSet<PathBuf>> = None;
        
        let imported_files = unsafe {
            if IMPORTED_FILES.is_none() {
                IMPORTED_FILES = Some(HashSet::new());
            }
            IMPORTED_FILES.as_mut().unwrap()
        };
        
        let import_path = Path::new(path);
        let canonical_path = match import_path.canonicalize() {
            Ok(p) => p,
            Err(_) => {
                let path_with_ext = format!("{}.x", path);
                let import_path_with_ext = Path::new(&path_with_ext);
                match import_path_with_ext.canonicalize() {
                    Ok(p) => p,
                    Err(e) => return Err(format!("Failed to resolve import path: {}: {}", path, e))
                }
            }
        };
        
        if imported_files.contains(&canonical_path) {
            return Ok(());
        }
        
        imported_files.insert(canonical_path.clone());
        
        let source = match fs::read_to_string(&canonical_path) {
            Ok(content) => content,
            Err(e) => return Err(format!("Failed to read imported file: {}: {}", path, e))
        };
        
        let program = match parse(&source) {
            Ok(p) => p,
            Err(e) => return Err(format!("Failed to parse imported file: {}: {}", path, e))
        };
        
        for stmt in &program.statements {
            match stmt {
                x_ast::Statement::Import { module, item } => {
                    self.process_import(module, item)?;
                },
                x_ast::Statement::FileImport { path: import_path } => {
                    self.process_file_import(import_path)?;
                },
                _ => {}
            }
        }
        
        for stmt in &program.statements {
            match stmt {
                x_ast::Statement::Function { name, params, body } => {
                    self.compile_function(name, params, body)?;
                },
                _ => {}
            }
        }
        
        Ok(())
    }
}