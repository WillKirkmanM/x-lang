use std::collections::HashMap;

use x_ast::Type;

use crate::TypeChecker;

impl TypeChecker {
    /// Recursively matches a generic parameter type against a concrete argument type to infer type mappings.
    pub fn infer_types_from_match(
        &self,
        param_type: &Type,
        arg_type: &Type,
        map: &mut HashMap<String, Type>,
    ) -> Result<(), String> {
        match (param_type, arg_type) {
            (Type::TypeParameter(name), concrete_type) => {
                if let Some(existing) = map.get(name) {
                    if existing != concrete_type {
                        return Err(format!(
                            "Type inference conflict for generic parameter '{}'. Found both {:?} and {:?}.",
                            name, existing, concrete_type
                        ));
                    }
                } else {
                    map.insert(name.clone(), concrete_type.clone());
                }
            }
            (Type::Array(p_inner), Type::Array(a_inner)) => {
                self.infer_types_from_match(p_inner, a_inner, map)?;
            }
            // Allow inferring when parameter expects an array but argument is a
            // concrete/custom type that was produced by monomorphisation or a runtime
            // "array-like" wrapper (e.g. mangled names).
            (Type::Array(p_inner), Type::Custom(c_mangled)) => {
                // First, prefer recorded instantiation metadata.
                if let Some(concrete_args) = self.symbols.struct_inst_args.get(c_mangled) {
                    if let Some(first) = concrete_args.get(0) {
                        self.infer_types_from_match(p_inner, first, map)?;
                    }
                } else {
                    // Try to recognise common mangled/canonical array names such
                    // as "Array_i32" or "Slice_f64" and map the suffix back to a primitive Type.
                    if let Some(idx) = c_mangled.find('_') {
                        let suffix = &c_mangled[idx + 1..];
                        let elem_ty = match suffix {
                            "i32" => Type::Int,
                            "i64" => Type::Int,
                            "f64" => Type::Float,
                            "f32" => Type::Float,
                            "bool" => Type::Bool,
                            "str" => Type::String,
                            other => Type::Custom(other.to_string()),
                        };
                        self.infer_types_from_match(p_inner, &elem_ty, map)?;
                    }
                }
            }
            // If param is a generic instance (e.g., Pair<T,U>) and arg is a concrete custom
            // type (e.g., Pair_i32_bool), look up the recorded concrete type args and recurse.
            (
                Type::GenericInstance {
                    name: p_name,
                    type_args: p_args,
                },
                Type::Custom(c_mangled),
            ) => {
                if let Some(concrete_args) = self.symbols.struct_inst_args.get(c_mangled) {
                    // Pair<T,U> vs Pair<i32,bool> -> infer T=i32, U=bool
                    if p_args.len() != concrete_args.len() {
                        return Err(format!(
                            "Generic arity mismatch for '{}': param has {}, arg has {}",
                            p_name,
                            p_args.len(),
                            concrete_args.len()
                        ));
                    }
                    for (p_arg, c_arg) in p_args.iter().zip(concrete_args.iter()) {
                        self.infer_types_from_match(p_arg, c_arg, map)?;
                    }
                }
            }
            _ => {}
        }
        Ok(())
    }
}
