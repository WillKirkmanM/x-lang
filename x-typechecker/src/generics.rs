use x_ast::Statement;

use crate::{FunctionDef, TypeChecker};

impl TypeChecker {
    pub fn register_generics(&mut self) -> Result<(), String> {
        let mut non_generic = Vec::new();
        for stmt in self.program.statements.drain(..) {
            match stmt {
                Statement::Function {
                    name,
                    generic_params,
                    params,
                    return_type,
                    body,
                    is_pure,
                    is_memoised,
                    is_multi,
                    is_throws,
                } if generic_params.is_some() => {
                    let func = FunctionDef {
                        name: name.clone(),
                        generic_params,
                        params,
                        return_type,
                        body: body.unwrap(),
                        is_pure,
                        is_memoised,
                        is_multi,
                        is_throws,
                    };
                    self.symbols.generic_functions.insert(name, func);
                }
                Statement::StructDecl(s) if s.generic_params.is_some() => {
                    self.symbols.generic_structs.insert(s.name.clone(), s);
                }
                other => non_generic.push(other),
            }
        }
        self.program.statements = non_generic;
        Ok(())
    }
}
