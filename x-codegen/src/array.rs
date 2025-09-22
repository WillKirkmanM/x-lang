use inkwell::{
    values::BasicValueEnum::{self, *},
    AddressSpace,
};
use x_ast::{Expr, Type};

use crate::CodeGen;

impl<'ctx> CodeGen<'ctx> {
    pub(crate) fn gen_array_access(
        &mut self,
        array: &Box<Expr>,
        index: &Box<Expr>,
        self_type: Option<&Type>,
    ) -> Result<BasicValueEnum<'ctx>, String> {
        let gep = self.get_array_element_ptr(array, index, self_type)?;

        self.builder
            .build_load(self.context.f64_type(), gep, "array_element")
            .map_err(|e| e.to_string())
    }

    pub fn get_array_element_ptr(
        &mut self,
        array: &Box<Expr>,
        index: &Box<Expr>,
        self_type: Option<&Type>,
    ) -> Result<inkwell::values::PointerValue<'ctx>, String> {
        fn get_base_array(expr: &Expr) -> Option<String> {
            match expr {
                Expr::Identifier(name) => Some(name.clone()),
                Expr::ArrayAccess { array, index: _ } => get_base_array(array),
                _ => None,
            }
        }

        let array_name = get_base_array(array)
            .ok_or_else(|| "Array access only supports identifier arrays for now".to_string())?;

        let array_var_ptr = *self
            .variables
            .get(&array_name)
            .ok_or_else(|| format!("Unknown array variable: {}", array_name))?;

        let index_val = self.gen_expr(index, self_type)?;

        // Accept either a float or an int expression as the index.
        // If it's a float, convert to unsigned int; if it's an int, cast/resize to i32.
        let index_int = match index_val {
            FloatValue(fv) => self
                .builder
                .build_float_to_unsigned_int(fv, self.context.i32_type(), "array_idx")
                .map_err(|e| e.to_string())?,
            IntValue(iv) => {
                // If already i32, use as-is. Otherwise truncate or zero-extend to i32.
                let iv_ty = iv.get_type();
                let target_ty = self.context.i32_type();
                if iv_ty == target_ty {
                    iv
                } else {
                    let bit_width = iv_ty.get_bit_width();
                    if bit_width > 32 {
                        self.builder
                            .build_int_truncate(iv, target_ty, "idx_trunc")
                            .map_err(|e| e.to_string())?
                    } else {
                        self.builder
                            .build_int_z_extend(iv, target_ty, "idx_zext")
                            .map_err(|e| e.to_string())?
                    }
                }
            }
            other => {
                return Err(format!(
                    "Array index must be an integer or float value, got: {:?}",
                    other
                ));
            }
        };

        let array_ast_type = self.variable_types.get(&array_name).unwrap().clone();

        let data_ptr = if matches!(&array_ast_type, x_ast::Type::Ref { .. }) {
            // --- CASE 1: The array is a reference (e.g., from bubble_sort) ---

            // 1. Load the pointer to the slice struct that was passed as an argument.
            let llvm_ref_type = self.map_ast_type_to_llvm(&array_ast_type);
            let slice_struct_ptr = self
                .builder
                .build_load(llvm_ref_type, array_var_ptr, "slice_struct_ptr")
                .map_err(|e| e.to_string())?
                .into_pointer_value();

            // 2. Get the type of the underlying slice struct: { ptr, i64 }
            let array_inner_type =
                self.map_ast_type_to_llvm(&x_ast::Type::Array(Box::new(x_ast::Type::Float)));

            // 3. Get a pointer to the first field (index 0) of the struct.
            let data_ptr_ptr = self
                .builder
                .build_struct_gep(
                    array_inner_type.into_struct_type(),
                    slice_struct_ptr,
                    0,
                    "data_ptr_ptr",
                )
                .map_err(|e| e.to_string())?;

            // 4. Load the actual data pointer (the `double*`) from that field.
            self.builder
                .build_load(
                    self.context.ptr_type(AddressSpace::default()),
                    data_ptr_ptr,
                    "data_ptr",
                )
                .map_err(|e| e.to_string())?
                .into_pointer_value()
        } else {
            // --- CASE 2: The array is a value (e.g., from binary_search) ---

            // 1. Load the slice struct value from the local variable.
            let array_inner_type = self.map_ast_type_to_llvm(&array_ast_type);
            let slice_struct_val = self
                .builder
                .build_load(array_inner_type, array_var_ptr, "slice_struct_val")
                .map_err(|e| e.to_string())?
                .into_struct_value();

            // 2. Extract the data pointer (field 0) directly from the struct value.
            self.builder
                .build_extract_value(slice_struct_val, 0, "data_ptr")
                .map_err(|e| e.to_string())?
                .into_pointer_value()
        };

        // Now, use the correct data pointer to get the element
        let element_ptr = unsafe {
            self.builder
                .build_in_bounds_gep(
                    self.context.f64_type(),
                    data_ptr,
                    &[index_int],
                    "element_ptr",
                )
                .map_err(|e| e.to_string())
        };

        element_ptr
    }
}
