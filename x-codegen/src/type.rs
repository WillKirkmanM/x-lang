use std::collections::HashMap;

use inkwell::{
    types::{BasicType, BasicTypeEnum},
    AddressSpace,
};
use x_ast::{Expr, Statement, StructDef, Type};

use x_logging::warn;

use crate::CodeGen;

impl<'ctx> CodeGen<'ctx> {
    pub fn map_type(&self, type_name: &str) -> Result<BasicTypeEnum<'ctx>, String> {
        match type_name {
            "f64" => Ok(self.context.f64_type().into()),
            "i32" => Ok(self.context.i32_type().into()),
            "i64" => Ok(self.context.i64_type().into()),
            "i8" => Ok(self.context.i8_type().into()),
            "str" => Ok(self.context.ptr_type(AddressSpace::default()).into()),
            "array_ptr" => Ok(self.context.ptr_type(AddressSpace::default()).into()),
            "fn_ptr" => {
                let _void_fn_type = self.context.void_type().fn_type(&[], false);
                Ok(self.context.ptr_type(AddressSpace::default()).into())
            }
            struct_name if self.struct_types.contains_key(struct_name) => {
                Ok(self.struct_types[struct_name].0.into())
            }
            struct_ptr_name if struct_ptr_name.ends_with("_ptr") => {
                let base_name = &struct_ptr_name[..struct_ptr_name.len() - 4];
                if let Some((_struct_type, _)) = self.struct_types.get(base_name) {
                    Ok(self.context.ptr_type(AddressSpace::default()).into())
                } else {
                    Err(format!(
                        "Unknown base struct type for pointer: {}",
                        base_name
                    ))
                }
            }
            _ => Err(format!(
                "Unknown type name during map_type: '{}'",
                type_name
            )),
        }
    }

    pub fn map_ast_type_to_llvm(&mut self, ty: &x_ast::Type) -> BasicTypeEnum<'ctx> {
        match ty {
            x_ast::Type::Int => self.context.i32_type().into(),
            x_ast::Type::Float => self.context.f64_type().into(),
            x_ast::Type::Bool => self.context.bool_type().into(),
            x_ast::Type::String => self.context.ptr_type(AddressSpace::default()).into(),
            x_ast::Type::Void => self.context.i32_type().into(),

            x_ast::Type::Array(_element_type) => {
                // An array is a struct { data_ptr, length }
                let i64_type = self.context.i64_type();
                let ptr_type = self.context.ptr_type(AddressSpace::default());

                self.context
                    .struct_type(&[ptr_type.into(), i64_type.into()], false)
                    .into()
            }
            x_ast::Type::Custom(name) => {
                // A struct value is the struct type itself.
                // If the named struct hasn't been registered yet (e.g. a generic template
                // like `Pair` that wasn't monomorphised), create and register an opaque
                // LLVM struct with that name so the codegen can continue rather than panic.
                if let Some((st, _)) = self.struct_types.get(name) {
                    st.as_basic_type_enum()
                } else {
                    // Create an opaque struct with the requested name and register it.
                    let opaque = self.context.opaque_struct_type(name.as_str());
                    // Insert with empty metadata/fields; later passes may replace/populate if needed.
                    self.struct_types.insert(name.clone(), (opaque, Vec::new()));
                    opaque.as_basic_type_enum()
                }
            }

            x_ast::Type::Ref { inner: _inner, .. } => {
                // A reference (&T) is a pointer to the LLVM type of T.
                self.context.ptr_type(AddressSpace::default()).into()
            }
            x_ast::Type::Unknown => {
                warn!("Encountered unknown AST type, defaulting to i32");
                self.context.i32_type().into()
            }
            x_ast::Type::TypeParameter(name) | x_ast::Type::GenericInstance { name, .. } => {
                // Resolve common primitive names first.
                match name.as_str() {
                    "f64" => return self.context.f64_type().into(),
                    "i32" => return self.context.i32_type().into(),
                    "i64" => return self.context.i64_type().into(),
                    "i8" => return self.context.i8_type().into(),
                    "str" => return self.context.ptr_type(AddressSpace::default()).into(),
                    _ => {}
                }

                // If a concrete struct with this exact name has already been declared/instantiated, use it.
                if let Some((st, _)) = self.struct_types.get(name) {
                    return st.as_basic_type_enum();
                }

                // Heuristic: if the provided name is itself a mangled generic (e.g. "Pair_T_U")
                // strip down to the base and prefer an existing monomorphised variant such as
                // "Pair_i32_bool" by matching the "Pair_" prefix.
                let base_name = name.splitn(2, '_').next().unwrap_or(name.as_str());
                let prefix = format!("{}_", base_name);
                let mut found: Option<String> = None;
                for key in self.struct_types.keys() {
                    if key.starts_with(&prefix) {
                        // pick first match
                        found = Some(key.clone());
                        break;
                    }
                }
                if let Some(k) = found {
                    return self.struct_types.get(&k).unwrap().0.as_basic_type_enum();
                }

                // As a last resort, create and register an opaque struct
                let opaque = self.context.opaque_struct_type(name.as_str());
                self.struct_types.insert(name.clone(), (opaque, Vec::new()));
                self.struct_types.get(name).unwrap().0.as_basic_type_enum()
            }
        }
    }

    pub fn resolve_type(
        &mut self,
        ty: &Type,
        gen_structs: &HashMap<String, StructDef>,
        concrete_statements: &mut Vec<Statement>,
    ) -> Result<Type, String> {
        if let Type::GenericInstance { name, type_args } = ty {
            if gen_structs.contains_key(name) {
                let mangled_name =
                    self.instantiate_struct(name, type_args, gen_structs, concrete_statements)?;
                return Ok(Type::Custom(mangled_name));
            }
        }
        Ok(ty.clone())
    }

    pub fn substitute_type(&self, ty: &Type, type_map: &HashMap<String, Type>) -> Type {
        match ty {
            // Replace generic names that were modeled as Custom("T") / Custom("U")
            Type::Custom(name) => {
                if let Some(repl) = type_map.get(name) {
                    repl.clone()
                } else {
                    Type::Custom(name.clone())
                }
            }

            Type::TypeParameter(name) => type_map.get(name).cloned().unwrap_or_else(|| ty.clone()),
            Type::Array(inner) => Type::Array(Box::new(self.substitute_type(inner, type_map))),

            Type::GenericInstance { name, type_args } => {
                let new_args = type_args
                    .iter()
                    .map(|t| self.substitute_type(t, type_map))
                    .collect();
                Type::GenericInstance {
                    name: name.clone(),
                    type_args: new_args,
                }
            }

            _ => ty.clone(),
        }
    }

    pub fn get_expr_type(&self, expr: &Expr) -> Result<Type, String> {
        match expr {
            Expr::Identifier(name) => {
                // 1. Check the monomorphization pre-pass scope first.
                if let Some(ty) = self.monomorph_scope.get(name) {
                    return Ok(ty.clone());
                }

                // 2. Fallback to the main variable types map for the codegen pass.
                if let Some(ty) = self.variable_types.get(name) {
                    return Ok(ty.clone());
                }

                // If a struct with this name exists, treat it as that custom type
                if self.struct_types.contains_key(name) {
                    return Ok(Type::Custom(name.clone()));
                }
                Err(format!("Unknown identifier in get_expr_type: {}", name))
            }
            Expr::Int(_) => Ok(Type::Int),
            Expr::Float(_) => Ok(Type::Float),
            Expr::Boolean(_) => Ok(Type::Bool),
            Expr::StructInstantiate(init) => Ok(Type::Custom(init.name.clone())),
            _ => Err("Unsupported expression type for inference".to_string()),
        }
    }
}
