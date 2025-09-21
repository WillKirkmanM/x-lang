use crate::CodeGen;
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::BasicValueEnum;
use x_logging::warn;

impl<'ctx> CodeGen<'ctx> {
    pub(crate) fn coerce_value_to_type(
        &self,
        value: BasicValueEnum<'ctx>,
        expected_type: BasicTypeEnum<'ctx>,
        name: &str,
    ) -> Result<BasicValueEnum<'ctx>, String> {
        let current_type = value.get_type();

        if current_type == expected_type {
            return Ok(value);
        }

        match (current_type, expected_type) {
            // Integer -> Float
            (BasicTypeEnum::IntType(_int_type), BasicTypeEnum::FloatType(float_type)) => {
                warn!(target: "codegen", "Coercing value from Int to Float: {}", name);
                let coerced = self
                    .builder
                    .build_signed_int_to_float(value.into_int_value(), float_type, name)
                    .map_err(|e| format!("Builder error while converting Int to Float: {:?}", e))?;
                Ok(coerced.into())
            }

            // Float -> Integer
            (BasicTypeEnum::FloatType(_float_type), BasicTypeEnum::IntType(int_type)) => {
                warn!(target: "codegen", "Coercing value from Float to Int: {}", name);
                let coerced = self
                    .builder
                    .build_float_to_signed_int(value.into_float_value(), int_type, name)
                    .map_err(|e| format!("Builder error while converting Float to Int: {:?}", e))?;
                Ok(coerced.into())
            }

            // Pointer -> Int (ptr -> integer)
            (BasicTypeEnum::PointerType(_ptr_ty), BasicTypeEnum::IntType(int_ty)) => {
                warn!(target: "codegen", "Coercing value from Pointer to Int (ptr->int): {}", name);
                let int_val = self
                    .builder
                    .build_ptr_to_int(value.into_pointer_value(), int_ty, name)
                    .map_err(|e| format!("Builder error while converting Ptr to Int: {:?}", e))?;
                Ok(int_val.into())
            }

            // Int -> Pointer (int -> ptr)
            (BasicTypeEnum::IntType(_int_ty), BasicTypeEnum::PointerType(ptr_ty)) => {
                warn!(target: "codegen", "Coercing value from Int to Pointer (int->ptr): {}", name);
                let ptr_val = self
                    .builder
                    .build_int_to_ptr(value.into_int_value(), ptr_ty, name)
                    .map_err(|e| format!("Builder error while converting Int to Ptr: {:?}", e))?;
                Ok(ptr_val.into())
            }

            // Pointer -> Pointer (bitcast between pointer types)
            (BasicTypeEnum::PointerType(_src_ptr), BasicTypeEnum::PointerType(target_ptr)) => {
                warn!(target: "codegen", "Bitcasting pointer to expected pointer type: {}", name);
                let casted = self
                    .builder
                    .build_bit_cast(
                        value.into_pointer_value(),
                        target_ptr.as_basic_type_enum(),
                        name,
                    )
                    .map_err(|e| format!("Builder error while bitcasting pointer: {:?}", e))?;
                Ok(casted.into())
            }

            _ => Err(format!(
                "Type Mismatch: Cannot return type {:?} from a function expecting {:?}.",
                current_type, expected_type
            )),
        }
    }
}
