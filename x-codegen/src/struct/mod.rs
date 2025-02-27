use crate::CodeGen;
use inkwell::values::BasicValueEnum;

impl<'ctx> CodeGen<'ctx> {
    pub(crate) fn gen_struct_decl(&mut self, struct_def: &x_ast::StructDef) -> Result<(), String> {
        let field_types: Vec<_> = vec![self.context.f64_type(); struct_def.fields.len()]
            .into_iter()
            .map(|t| t.into())
            .collect();

        let struct_type = self.context.struct_type(&field_types, false);

        self.struct_types.insert(
            struct_def.name.clone(),
            (struct_type, struct_def.fields.clone()),
        );

        Ok(())
    }

    pub(crate) fn gen_struct_instantiate(
        &mut self,
        struct_init: &x_ast::StructInit,
    ) -> Result<BasicValueEnum<'ctx>, String> {
        let struct_type;
        let field_names;
        {
            let struct_info = match self.struct_types.get(&struct_init.name) {
                Some(t) => t,
                None => return Err(format!("Unknown struct type: {}", struct_init.name)),
            };
            struct_type = struct_info.0;
            field_names = struct_info.1.clone();
        }

        let struct_ptr = self
            .builder
            .build_alloca(struct_type, &format!("{}_instance", struct_init.name))
            .map_err(|e| e.to_string())?;

        for (field_name, value_expr) in &struct_init.fields {
            if let Some(index) = field_names.iter().position(|name| name == field_name) {
                let value = self.gen_expr(value_expr)?.into_float_value();

                let field_ptr = {
                    self.builder
                        .build_struct_gep(
                            struct_type,
                            struct_ptr,
                            index as u32,
                            &format!("{}.{}", struct_init.name, field_name),
                        )
                        .map_err(|e| e.to_string())?
                };

                self.builder
                    .build_store(field_ptr, value)
                    .map_err(|e| e.to_string())?;
            } else {
                return Err(format!(
                    "Unknown field '{}' in struct '{}'",
                    field_name, struct_init.name
                ));
            }
        }

        if let Some(name) = &self.current_binding_name {
            self.variable_types
                .insert(name.clone(), struct_init.name.clone());
        }

        Ok(struct_ptr.into())
    }
}