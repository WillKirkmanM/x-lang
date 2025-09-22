use inkwell::{
    types::BasicTypeEnum,
    values::{BasicValue, BasicValueEnum},
    AddressSpace,
};
use x_ast::{Expr, Type};

use crate::CodeGen;

impl<'ctx> CodeGen<'ctx> {
    pub fn handle_array_literal(
        &mut self,
        elements: &Vec<Expr>,
        self_type: Option<&Type>,
    ) -> Result<BasicValueEnum<'ctx>, String> {
        if elements.is_empty() {
            // Handle empty array case
            let array_type =
                self.map_ast_type_to_llvm(&x_ast::Type::Array(Box::new(x_ast::Type::Float))); // Default to [f64]
            return Ok(array_type.const_zero().into());
        }

        // Generate the values for the elements
        let mut element_values = Vec::new();
        for elem_expr in elements {
            element_values.push(self.gen_expr(elem_expr, self_type)?);
        }

        let first_element_type = element_values[0].get_type();
        let array_len = elements.len() as u32;

        // Decide how to construct the constant array based on element type.
        let const_array_val =
            match first_element_type {
                BasicTypeEnum::FloatType(_) => {
                    // Collect float values, ensure all elements are floats
                    let mut float_values = Vec::with_capacity(array_len as usize);
                    for val in element_values.into_iter() {
                        match val {
                        BasicValueEnum::FloatValue(fv) => float_values.push(fv),
                        _ => return Err(
                            "Array literal contains non-float value while first element is float"
                                .into(),
                        ),
                    }
                    }
                    // Create a constant array of f64 values
                    self.context.f64_type().const_array(&float_values)
                }
                BasicTypeEnum::IntType(int_ty) => {
                    // Collect int values, ensure all elements are ints
                    let mut int_values = Vec::with_capacity(array_len as usize);
                    for val in element_values.into_iter() {
                        match val {
                            BasicValueEnum::IntValue(iv) => int_values.push(iv),
                            _ => return Err(
                                "Array literal contains non-int value while first element is int"
                                    .into(),
                            ),
                        }
                    }
                    // Create a constant array of the detected integer type
                    int_ty.const_array(&int_values)
                }
                other => {
                    return Err(format!("Unsupported array element LLVM type: {:?}", other));
                }
            };

        // Create the global array variable and set the correct initialiser.
        let global_array = self.module.add_global(
            const_array_val.get_type(), // Use the type from the constant value
            Some(AddressSpace::default()),
            "array_literal_data",
        );

        global_array.set_initializer(&const_array_val.as_basic_value_enum());

        // Create the "fat pointer" struct value.
        let i64_type = self.context.i64_type();

        // global_array is an array value (e.g. [N x T]). Produce a pointer to the first element (T*)
        // via a constant GEP: getelementptr inbounds <array type>, ptr @g, i32 0, i32 0
        let idx0 = self.context.i32_type().const_zero();
        let idx1 = self.context.i32_type().const_zero();

        // Compose the index slice for the GEP (i32 0, i32 0).
        let indices = [idx0, idx1];

        // Use the const_in_bounds_gep overload that takes the pointee type first, then indices.
        let array_type = const_array_val.get_type();
        let data_ptr = unsafe {
            global_array
                .as_pointer_value()
                .const_in_bounds_gep(array_type, indices.as_slice())
        };
        let length_val = i64_type.const_int(array_len as u64, false);

        let struct_val = self
            .context
            .const_struct(&[data_ptr.into(), length_val.into()], false);

        Ok(struct_val.into())
    }
}
