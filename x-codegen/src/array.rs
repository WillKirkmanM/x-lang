use inkwell::{types::BasicType, values::BasicValueEnum};
use x_ast::{Expr, Type};

use crate::CodeGen;

impl<'ctx> CodeGen<'ctx> {
    pub(crate) fn gen_array_access(
        &mut self,
        array: &Box<Expr>,
        index: &Box<Expr>,
        self_type: Option<&Type>,
    ) -> Result<BasicValueEnum<'ctx>, String> {
        let ptr = self.get_array_element_ptr(array, index, self_type)?;
        let elem_ty = if let Expr::Identifier(name) = array.as_ref() {
            if let Some(Type::Array(inner)) = self.variable_types.get(name) {
                match **inner {
                    Type::Int => {
                        return Ok(self
                            .builder
                            .build_load(self.context.i32_type(), ptr, "ld_i32")
                            .map_err(|e| e.to_string())?
                            .into())
                    }
                    Type::Float => self.context.f64_type().as_basic_type_enum(),
                    _ => return Err(format!("Unsupported array element type: {:?}", inner)),
                }
            } else {
                self.context.f64_type().as_basic_type_enum()
            }
        } else {
            self.context.f64_type().as_basic_type_enum()
        };

        let loaded = self
            .builder
            .build_load(elem_ty, ptr, "ld_elem")
            .map_err(|e| e.to_string())?;
        Ok(loaded)
    }

    pub fn get_array_element_ptr(
        &mut self,
        array: &Box<Expr>,
        index: &Box<Expr>,
        self_type: Option<&Type>,
    ) -> Result<inkwell::values::PointerValue<'ctx>, String> {
        let arr_val = self.gen_expr(array, self_type)?;
        let arr_struct = arr_val.into_struct_value();

        let data_ptr = self
            .builder
            .build_extract_value(arr_struct, 0, "arr_data_ptr")
            .map_err(|_| "Failed to extract data ptr".to_string())?
            .into_pointer_value();

        let index_val_any = self.gen_expr(index, self_type)?;
        let i64_ty = self.context.i64_type();
        let idx_i64 = match index_val_any {
            BasicValueEnum::IntValue(iv) => {
                if iv.get_type().get_bit_width() != 64 {
                    self.builder
                        .build_int_s_extend(iv, i64_ty, "idx_ext")
                        .map_err(|e| e.to_string())?
                } else {
                    iv
                }
            }
            BasicValueEnum::FloatValue(fv) => self
                .builder
                .build_float_to_signed_int(fv, i64_ty, "idx_f2i")
                .map_err(|e| e.to_string())?,
            _ => return Err("Array index must be int/float".into()),
        };

        let elem_bt = if let Expr::Identifier(name) = array.as_ref() {
            if let Some(Type::Array(inner)) = self.variable_types.get(name) {
                match **inner {
                    Type::Int => self.context.i32_type().as_basic_type_enum(),
                    Type::Float => self.context.f64_type().as_basic_type_enum(),
                    _ => {
                        return Err(format!(
                            "Unsupported array element type for '{}': {:?}",
                            name, inner
                        ))
                    }
                }
            } else {
                self.context.f64_type().as_basic_type_enum()
            }
        } else {
            self.context.f64_type().as_basic_type_enum()
        };

        let gep = unsafe {
            self.builder
                .build_gep(elem_bt, data_ptr, &[idx_i64], "elem_ptr")
                .map_err(|e| e.to_string())?
        };
        Ok(gep)
    }
}
