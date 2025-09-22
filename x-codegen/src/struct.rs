use std::collections::HashMap;

use crate::{CodeGen, INSTANTIATION_CACHE};
use inkwell::{types::BasicTypeEnum, values::BasicValueEnum};
use x_ast::{Statement, StructDef, Type};

use x_logging::info;

impl<'ctx> CodeGen<'ctx> {
    pub(crate) fn gen_struct_decl(&mut self, struct_def: &x_ast::StructDef) -> Result<(), String> {
        let field_types: Vec<BasicTypeEnum<'ctx>> = struct_def
            .fields
            .iter()
            .map(|(_, ft)| {
                let resolved = match ft {
                    x_ast::Type::TypeParameter(name) => match name.as_str() {
                        "i32" => x_ast::Type::Int,
                        "f64" => x_ast::Type::Float,
                        "i64" => x_ast::Type::Int,
                        "i8" => x_ast::Type::Int,
                        "bool" => x_ast::Type::Bool,
                        "str" => x_ast::Type::String,
                        other => x_ast::Type::Custom(other.to_string()),
                    },
                    other => other.clone(),
                };
                self.map_ast_type_to_llvm(&resolved)
            })
            .collect();

        let struct_type = if struct_def.layout == x_ast::Layout::SoA {
            let ptr_field_types: Vec<BasicTypeEnum<'ctx>> = field_types
                .iter()
                .map(|_t| {
                    self.context
                        .ptr_type(inkwell::AddressSpace::default())
                        .into()
                })
                .collect();
            self.context.struct_type(&ptr_field_types, false)
        } else {
            self.context.struct_type(&field_types, false)
        };

        self.struct_types.insert(
            struct_def.name.clone(),
            (
                struct_type,
                struct_def
                    .fields
                    .iter()
                    .map(|(name, _)| name.clone())
                    .collect(),
                struct_def.layout,
            ),
        );

        Ok(())
    }

    pub(crate) fn gen_struct_instantiate(
        &mut self,
        struct_init: &x_ast::StructInit,
        self_type: Option<&Type>,
    ) -> Result<BasicValueEnum<'ctx>, String> {
        let (struct_type, field_names, layout) =
            self.struct_types.get(&struct_init.name).ok_or_else(|| {
                format!(
                    "Unknown struct type when generating struct instantiation: {}",
                    struct_init.name
                )
            })?;
        let struct_type = *struct_type;
        let field_names = field_names.clone();

        let layout = *layout;

        if layout == x_ast::Layout::SoA {
            let struct_alloca = self
                .builder
                .build_alloca(struct_type, &struct_init.name)
                .unwrap();

            for (field_name, value_expr) in &struct_init.fields {
                let index = field_names.iter().position(|n| n == field_name).unwrap() as u32;
                let field_ptr_ptr = self
                    .builder
                    .build_struct_gep(struct_type, struct_alloca, index, "field_ptr_ptr")
                    .unwrap();

                let field_val_ptr = self.gen_expr(value_expr, self_type)?.into_pointer_value();
                let _ = self.builder.build_store(field_ptr_ptr, field_val_ptr);
            }

            return Ok(struct_alloca.into());
        }

        // An aggregate value (like a struct) is built up field by field.
        // We start with an `undef` value of the struct's type.
        let mut aggregate = struct_type.get_undef();

        for (field_name, value_expr) in &struct_init.fields {
            let index = field_names
                .iter()
                .position(|name| name == field_name)
                .ok_or_else(|| {
                    format!(
                        "Unknown field '{}' in struct '{}'",
                        field_name, struct_init.name
                    )
                })?;
            let index = index as u32;

            // Generate the value for the field's initialiser expression.
            let value = self.gen_expr(value_expr, self_type)?;
            let expected_type = struct_type.get_field_type_at_index(index).unwrap();

            // Coerce the value to the field's expected type (e.g., int to float).
            let final_value =
                self.coerce_value_to_type(value, expected_type, "field_init_coerce")?;

            // Insert the computed value into our aggregate struct.
            aggregate = self
                .builder
                .build_insert_value(aggregate, final_value, index, "insert_field")
                .unwrap()
                .into_struct_value();
        }

        Ok(aggregate.into())
    }

    pub fn declare_struct(&mut self, struct_def: &StructDef) -> Result<(), String> {
        if self.struct_types.contains_key(&struct_def.name) {
            // This is a valid case if the same monomorphised struct is needed by multiple modules
            return Ok(());
        }

        // Resolve TypeParameters that are actually primitive names (produced by the
        // monomorphiser) before mapping to LLVM types. This prevents creating named
        // opaque struct types like "%i32" for primitive fields.
        let field_types: Vec<BasicTypeEnum<'ctx>> = struct_def
            .fields
            .iter()
            .map(|(_fname, ft)| {
                let resolved = match ft {
                    Type::TypeParameter(name) => match name.as_str() {
                        "i32" => Type::Int,
                        "f64" => Type::Float,
                        "i64" => Type::Int,
                        "i8" => Type::Int,
                        "bool" => Type::Bool,
                        "str" => Type::String,
                        other => Type::Custom(other.to_string()),
                    },
                    other => other.clone(),
                };
                self.map_ast_type_to_llvm(&resolved)
            })
            .collect();

        // Create a named, opaque struct first to allow for recursive types.
        let struct_type = self.context.opaque_struct_type(&struct_def.name);

        // Now, set the body with the concrete field types according to the layout.
        if struct_def.layout == x_ast::Layout::SoA {
            let ptr_field_types: Vec<BasicTypeEnum<'ctx>> = field_types
                .iter()
                .map(|_t| {
                    self.context
                        .ptr_type(inkwell::AddressSpace::default())
                        .into()
                })
                .collect();
            struct_type.set_body(&ptr_field_types, false);
        } else {
            struct_type.set_body(&field_types, false);
        }

        let field_names: Vec<String> = struct_def
            .fields
            .iter()
            .map(|(name, _)| name.clone())
            .collect();

        self.struct_types.insert(
            struct_def.name.clone(),
            (struct_type, field_names, struct_def.layout),
        );

        info!(
            declared_struct = %struct_def.name,
            fields = ?struct_def.fields,
            layout = ?struct_def.layout,
            "Declared struct"
        );
        Ok(())
    }

    pub fn process_struct_declarations(
        &mut self,
        all_statements: &[Statement],
    ) -> Result<(), String> {
        for stmt in all_statements {
            if let Statement::StructDecl(struct_def) = stmt {
                // If the struct definition has generic parameters, it's a blueprint.
                // The CodeGen should only process the concrete versions
                // that the TypeChecker will create.
                if struct_def.generic_params.is_some() {
                    info!(
                        struct_name = %struct_def.name,
                        "Skipping declaration of generic struct blueprint"
                    );
                    continue;
                }

                self.declare_struct(struct_def)?;
                self.ast_structs
                    .insert(struct_def.name.clone(), struct_def.clone());
            }
        }
        Ok(())
    }

    pub fn instantiate_struct(
        &mut self,
        name: &str,
        type_args: &[Type],
        gen_structs: &HashMap<String, StructDef>,
        concrete_statements: &mut Vec<Statement>,
    ) -> Result<String, String> {
        let mangled_name = format!(
            "{}_{}",
            name,
            type_args
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join("_")
        );

        if INSTANTIATION_CACHE.with(|cache| cache.borrow().contains_key(&mangled_name)) {
            return Ok(mangled_name);
        }

        let blueprint = gen_structs.get(name).unwrap();

        let generic_params = blueprint.generic_params.as_ref().unwrap();

        if generic_params.len() != type_args.len() {
            return Err(format!(
                "Generic argument count mismatch for struct '{}'. Expected {}, but got {}.",
                name,
                generic_params.len(),
                type_args.len()
            ));
        }

        let type_map: HashMap<_, _> = blueprint
            .generic_params
            .as_ref()
            .unwrap()
            .iter()
            .cloned()
            .zip(type_args.iter().cloned())
            .collect();

        let concrete_fields = blueprint
            .fields
            .iter()
            .map(|(fname, ftype)| (fname.clone(), self.substitute_type(ftype, &type_map)))
            .collect();

        let concrete_invariant = if let Some(invariant_expr) = &blueprint.invariant {
            let mut concrete_expr = *invariant_expr.clone();

            // Recursively substitute types within the invariant expression.
            self.substitute_in_expr(&mut concrete_expr, &type_map);
            Some(Box::new(concrete_expr))
        } else {
            None
        };

        let concrete_struct = Statement::StructDecl(StructDef {
            name: mangled_name.clone(),
            generic_params: None,
            fields: concrete_fields,
            layout: blueprint.layout,
            invariant: concrete_invariant,
        });

        info!(instantiated_struct = %mangled_name, "Instantiated new struct");
        concrete_statements.push(concrete_struct);
        INSTANTIATION_CACHE.with(|cache| {
            cache
                .borrow_mut()
                .insert(mangled_name.clone(), mangled_name.clone())
        });

        Ok(mangled_name)
    }

    pub(crate) fn gen_field_access(
        &mut self,
        object: &x_ast::Expr,
        field: &str,
        self_type: Option<&Type>,
    ) -> Result<BasicValueEnum<'ctx>, String> {
        let obj_val = self.gen_expr(object, self_type)?;
        // Normalise to a PointerValue: if we have a StructValue, stack-allocate and store it so we can take a pointer.
        let object_ptr_val = match obj_val {
            BasicValueEnum::PointerValue(p) => p,
            BasicValueEnum::StructValue(sv) => {
                let alloca = self
                    .builder
                    .build_alloca(sv.get_type(), "tmp_struct_alloca")
                    .map_err(|e| e.to_string())?;
                let _ = self.builder.build_store(alloca, sv);
                alloca
            }
            other => {
                return Err(format!(
                    "Expected pointer/struct for field access, got {:?}",
                    other
                ));
            }
        };

        let object_ast_type = if let x_ast::Expr::Identifier(name) = object {
            if let Some(t) = self.variable_types.get(name) {
                t.clone()
            } else {
                self.infer_ast_type_from_expr(object)?
            }
        } else {
            self.infer_ast_type_from_expr(object)?
        };

        let base_ast_type = match object_ast_type {
            x_ast::Type::Ref { inner, .. } => *inner,
            _ => object_ast_type,
        };

        // Convert GenericInstance -> mangled name if necessary
        fn type_to_mangled_name(t: &x_ast::Type) -> String {
            match t {
                x_ast::Type::Int => "i32".to_string(),
                x_ast::Type::Float => "f64".to_string(),
                x_ast::Type::Bool => "bool".to_string(),
                x_ast::Type::String => "str".to_string(),
                x_ast::Type::Custom(s) => s.clone(),
                x_ast::Type::TypeParameter(p) => p.clone(),
                x_ast::Type::GenericInstance { name, type_args } => {
                    let parts: Vec<String> = type_args
                        .iter()
                        .map(|ta| type_to_mangled_name(ta))
                        .collect();
                    format!("{}_{}", name, parts.join("_"))
                }
                _ => format!("{:?}", t),
            }
        }

        let struct_name = match base_ast_type {
            x_ast::Type::Custom(s) => s,
            x_ast::Type::GenericInstance { .. } => type_to_mangled_name(&base_ast_type),
            other => {
                return Err(format!(
                    "Cannot access member on non-struct type: {:?}",
                    other
                ))
            }
        };

        // Use the recorded struct entry to GEP/load the field value (reliable path).
        if let Some((struct_llvm_type, field_names, layout)) = self.struct_types.get(&struct_name) {
            if let Some(index) = field_names.iter().position(|f_name| f_name == field) {
                return match layout {
                    x_ast::Layout::AoS => {
                        // AoS: GEP to find field address, then load value.
                        let field_ptr = self
                            .builder
                            .build_struct_gep(
                                *struct_llvm_type,
                                object_ptr_val,
                                index as u32,
                                "fieldptr",
                            )
                            .unwrap();
                        let field_llvm_type = struct_llvm_type.get_field_types()[index];
                        self.builder
                            .build_load(field_llvm_type, field_ptr, "loadfield")
                            .map_err(|e| e.to_string())
                    }
                    x_ast::Layout::SoA => {
                        // SoA: GEP to find field *pointer*, load it, then the caller will use it as an array base.
                        let field_ptr_ptr = self
                            .builder
                            .build_struct_gep(
                                *struct_llvm_type,
                                object_ptr_val,
                                index as u32,
                                "field_ptr_ptr",
                            )
                            .unwrap();
                        let field_array_base_ptr = self
                            .builder
                            .build_load(field_ptr_ptr.get_type(), field_ptr_ptr, "field_array_base")
                            .unwrap();
                        Ok(field_array_base_ptr)
                    }
                };
            }
        }

        if let Some(methods) = self.struct_methods.get(&struct_name) {
            if methods.contains_key(field) {
                let mangled_name = format!("{}_{}", struct_name, field);
                let function = self.module.get_function(&mangled_name).ok_or_else(|| {
                    format!(
                        "CodeGen Error: Method '{}' was declared but not generated.",
                        mangled_name
                    )
                })?;
                let args = &[object_ptr_val.into()];
                let call = self
                    .builder
                    .build_call(function, args, "methodcall")
                    .unwrap();
                return Ok(call
                    .try_as_basic_value()
                    .left()
                    .unwrap_or(self.context.i32_type().const_int(0, false).into()));
            }
        }

        Err(format!(
            "Struct '{}' has no field or method named '{}'",
            struct_name, field
        ))
    }
}
