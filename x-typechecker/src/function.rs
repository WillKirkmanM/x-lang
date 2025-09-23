use std::collections::HashMap;

use x_ast::{Param, Statement, Type};

use crate::{FunctionSignature, TypeChecker};

impl TypeChecker {
    pub fn instantiate_function(
        &mut self,
        name: &str,
        type_args: &[Type],
    ) -> Result<String, String> {
        let key = (
            name.to_string(),
            type_args.iter().map(|t| t.to_string()).collect::<Vec<_>>(),
        );
        if let Some(mangled_name) = self.symbols.instantiations.get(&key) {
            return Ok(mangled_name.clone());
        }

        let generic_fn = self
            .symbols
            .generic_functions
            .get(name)
            .ok_or_else(|| format!("Unknown generic function {}", name))?
            .clone();
        let gen_params = generic_fn.generic_params.as_ref().unwrap();
        if gen_params.len() != type_args.len() {
            return Err(format!(
                "Incorrect number of generic arguments for function '{}'",
                name
            ));
        }

        let type_map: HashMap<String, Type> = gen_params
            .iter()
            .cloned()
            .zip(type_args.iter().cloned())
            .collect();
        let mangled_name = format!(
            "{}_{}",
            name,
            type_args
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<_>>()
                .join("_")
        );

        let mut specialised_func = generic_fn;
        specialised_func.name = mangled_name.clone();
        specialised_func.generic_params = None;

        specialised_func.params = specialised_func
            .params
            .into_iter()
            .map(|param| Param {
                ty: self.substitute_type(&param.ty, &type_map),
                ..param
            })
            .collect();
        specialised_func.return_type =
            self.substitute_type(&specialised_func.return_type, &type_map);

        for stmt in specialised_func.body.iter_mut() {
            self.substitute_types_in_statement(stmt, &type_map);
        }

        self.program.statements.push(Statement::Function {
            name: specialised_func.name.clone(),
            generic_params: None,
            params: specialised_func.params.clone(),
            return_type: specialised_func.return_type.clone(),
            body: Some(specialised_func.body.clone()),
            is_pure: specialised_func.is_pure,
            is_memoised: specialised_func.is_memoised,
            is_multi: specialised_func.is_multi,
            is_throws: specialised_func.is_throws,
        });

        self.symbols.add_function(
            mangled_name.clone(),
            FunctionSignature {
                param_types: specialised_func
                    .params
                    .into_iter()
                    .map(|param| param.ty)
                    .collect(),
                return_type: specialised_func.return_type,
            },
        );

        self.symbols
            .instantiations
            .insert(key, mangled_name.clone());
        Ok(mangled_name)
    }
}
