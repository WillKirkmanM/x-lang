use inkwell::values::BasicValueEnum;
use x_ast::Expr;

use crate::CodeGen;

impl<'ctx> CodeGen<'ctx> {
    pub fn gen_assignment(
        &mut self,
        target: &Expr,
        value: &Expr,
    ) -> Result<BasicValueEnum<'ctx>, String> {
        if let Expr::Identifier(name) = target {
            let val = self.gen_expr(value)?;

            if let Some(ptr) = self.variables.get(name) {
                self.builder
                    .build_store(*ptr, val)
                    .map_err(|e| e.to_string())?;

                return Ok(val);
            } else {
                return Err(format!("Cannot assign to undefined variable: {}", name));
            }
        }

        if let Expr::FieldAccess { object, field } = target {
            if let Expr::Identifier(obj_name) = object.as_ref() {
                let val = self.gen_expr(value)?;

                if let Some(var_ptr) = self.variables.get(obj_name) {
                    let ast_ty = self
                        .variable_types
                        .get(obj_name)
                        .ok_or_else(|| format!("Unknown type for variable '{}'", obj_name))?;

                    if let x_ast::Type::Custom(struct_name) = ast_ty {
                        let (_, field_names) = self
                            .struct_types
                            .get(struct_name)
                            .ok_or_else(|| format!("Unknown struct '{}'", struct_name))?;

                        let field_index = field_names
                            .iter()
                            .position(|fname| fname == field)
                            .ok_or_else(|| {
                                format!("Struct '{}' has no field named '{}'", struct_name, field)
                            })?;

                        let (struct_ty, _) = self
                            .struct_types
                            .get(struct_name)
                            .ok_or_else(|| format!("Unknown struct '{}'", struct_name))?;

                        let field_ptr = self
                            .builder
                            .build_struct_gep(*struct_ty, *var_ptr, field_index as u32, "field_ptr")
                            .map_err(|e| e.to_string())?;

                        self.builder
                            .build_store(field_ptr, val)
                            .map_err(|e| e.to_string())?;

                        return Ok(val);
                    } else {
                        return Err(format!("Field access on non-struct value: {:?}", ast_ty));
                    }
                } else {
                    return Err(format!("Cannot assign to undefined variable: {}", obj_name));
                }
            } else {
                return Err(format!("Invalid assignment target {:?}", target));
            }
        }

        if let Expr::ArrayAccess { array, index } = target {
            let gep = self.get_array_element_ptr(array, index)?;
            let value_val = self.gen_expr(value)?;

            self.builder
                .build_store(gep, value_val)
                .map_err(|e| e.to_string())?;

            return Ok(value_val);
        }

        Err(format!("Invalid assignment target {:?}", target).to_string())
    }
}
