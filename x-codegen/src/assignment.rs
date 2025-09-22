use inkwell::values::{BasicValueEnum, PointerValue};
use x_ast::{Expr, Type};

use crate::CodeGen;

impl<'ctx> CodeGen<'ctx> {
    /// Generates code for an assignment expression.
    ///
    /// This function is the main entry point. It uses a helper to find the
    /// memory address of the assignment target and then performs the store.
    pub fn gen_assignment(
        &mut self,
        target: &Expr,
        value: &Expr,
        self_type: Option<&Type>,
    ) -> Result<BasicValueEnum<'ctx>, String> {
        // 1. Get the memory address (pointer) of the target we are assigning to.
        let target_ptr = self.gen_lvalue_address(target, self_type)?;

        // 2. Generate the code for the value being assigned.
        let value_to_store = self.gen_expr(value, self_type)?;

        // 3. Store the value at the target's address.
        self.builder
            .build_store(target_ptr, value_to_store)
            .map_err(|e| e.to_string())?;

        // An assignment expression evaluates to the assigned value.
        Ok(value_to_store)
    }

    /// Recursively generates code to get the memory address (a PointerValue) of an l-value expression.
    fn gen_lvalue_address(
        &mut self,
        target: &Expr,
        self_type: Option<&Type>,
    ) -> Result<PointerValue<'ctx>, String> {
        match target {
            Expr::Identifier(name) => self
                .variables
                .get(name)
                .copied()
                .ok_or_else(|| format!("Cannot assign to undefined variable: {}", name)),

            Expr::FieldAccess { object, field } => {
                // First, recursively get the pointer to the struct that contains the field.
                let object_ptr = self.gen_lvalue_address(object, self_type)?;

                // Then, get the AST type of the struct to find the field's index and layout.
                let object_ast_type = self.infer_ast_type_from_expr(object)?;

                let base_ast_ty = match &object_ast_type {
                    x_ast::Type::Ref { inner, .. } => &**inner,
                    other => other,
                };

                if let x_ast::Type::Custom(struct_name) = base_ast_ty {
                    let (struct_llvm_type, field_names, _layout) = self
                        .struct_types
                        .get(struct_name)
                        .ok_or_else(|| format!("Unknown struct '{}'", struct_name))?;

                    let field_index = field_names
                        .iter()
                        .position(|fname| fname == field)
                        .ok_or_else(|| {
                            format!("Struct '{}' has no field named '{}'", struct_name, field)
                        })?;

                    // If the object is a reference (e.g., &Point), we must first load the pointer it holds.
                    // If it's a value, the l-value address is already the direct pointer to the struct.
                    let struct_ptr = if matches!(object_ast_type, x_ast::Type::Ref { .. }) {
                        let element_type = object_ptr.get_type();
                        self.builder
                            .build_load(element_type, object_ptr, "load_ref")
                            .unwrap()
                            .into_pointer_value()
                    } else {
                        object_ptr
                    };

                    self.builder
                        .build_struct_gep(
                            *struct_llvm_type,
                            struct_ptr,
                            field_index as u32,
                            "field_ptr",
                        )
                        .map_err(|e| e.to_string())
                } else {
                    Err(format!(
                        "Field access on non-struct value: {:?}",
                        object_ast_type
                    ))
                }
            }

            Expr::ArrayAccess { array, index } => {
                // We can reuse the existing helper for array access.
                self.get_array_element_ptr(array, index, self_type)
            }

            _ => Err(format!("Invalid assignment target {:?}", target)),
        }
    }
}
