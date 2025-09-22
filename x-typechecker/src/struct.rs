use std::collections::HashMap;

use x_ast::{Statement, StructDef, Type};

use crate::TypeChecker;

impl TypeChecker {
    pub fn instantiate_struct(&mut self, name: &str, type_args: &[Type]) -> Result<String, String> {
        let key = (
            name.to_string(),
            type_args.iter().map(|t| t.to_string()).collect::<Vec<_>>(),
        );
        if let Some(m) = self.symbols.instantiations.get(&key) {
            return Ok(m.clone());
        }

        let generic_struct = self
            .symbols
            .generic_structs
            .get(name)
            .ok_or_else(|| format!("Unknown generic struct {}", name))?
            .clone();
        let generic_params = generic_struct
            .generic_params
            .as_ref()
            .ok_or_else(|| format!("Struct {} has no generic params", name))?;
        if generic_params.len() != type_args.len() {
            return Err(format!(
                "Incorrect number of generic arguments for struct '{}'",
                name
            ));
        }

        let type_map: HashMap<String, Type> = generic_params
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

        let specialised_fields = generic_struct
            .fields
            .into_iter()
            .map(|(fname, ftype)| (fname, self.substitute_type(&ftype, &type_map)))
            .collect::<Vec<_>>();

        let new_struct_def = StructDef {
            name: mangled_name.clone(),
            generic_params: None,
            fields: specialised_fields.clone(),
            invariant: None,
            layout: generic_struct.layout,
        };

        println!("[TypeChecker] Monomorphised new struct: {}", mangled_name);
        self.program
            .statements
            .push(Statement::StructDecl(new_struct_def.clone()));
        self.symbols
            .add_struct(mangled_name.clone(), specialised_fields);
        self.symbols
            .instantiations
            .insert(key, mangled_name.clone());

        self.symbols
            .struct_inst_args
            .insert(mangled_name.clone(), type_args.to_vec());

        Ok(mangled_name)
    }
}
