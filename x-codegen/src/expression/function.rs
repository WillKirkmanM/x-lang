use inkwell::{types::BasicTypeEnum, values::BasicValueEnum};
use x_ast::{Statement, Type};

use x_logging::warn;

use crate::CodeGen;

impl<'ctx> CodeGen<'ctx> {
    pub fn handle_anonymous_function(
        &mut self,
        params: &Vec<String>,
        body: &Vec<Statement>,
        self_type: Option<&Type>,
    ) -> Result<BasicValueEnum<'ctx>, String> {
        let closure_name = self.get_unique_id();

        let param_types: Vec<BasicTypeEnum<'ctx>> = params
            .iter()
            .map(|_| self.context.f64_type().into())
            .collect();

        let param_metadata_types: Vec<inkwell::types::BasicMetadataTypeEnum<'ctx>> =
            param_types.iter().map(|&ty| ty.into()).collect();

        let fn_type = self
            .context
            .f64_type()
            .fn_type(&param_metadata_types, false);

        let closure_name_str = format!("closure_{}", closure_name);
        let function = self.module.add_function(&closure_name_str, fn_type, None);
        let entry_block = self.context.append_basic_block(function, "entry");

        let old_builder_pos = self.builder.get_insert_block();
        let old_vars = self.variables.clone();
        let old_var_types = self.variable_types.clone();

        self.builder.position_at_end(entry_block);
        self.variables.clear();
        self.variable_types.clear();

        // Create local backing storage for each closure parameter and
        // register it so body codegen can lookup the parameter name.
        for (i, param_name) in params.iter().enumerate() {
            // Param strings coming from the parser look like "x: f64".
            // Extract the identifier portion (before ':') and optional type suffix.
            let (param_ident, opt_ty_str) = if let Some(colon_idx) = param_name.find(':') {
                (
                    param_name[..colon_idx].trim().to_string(),
                    Some(param_name[colon_idx + 1..].trim().to_string()),
                )
            } else {
                (param_name.trim().to_string(), None)
            };

            // Use the precomputed LLVM basic type for parameters
            let llvm_param_ty = param_types[i];

            // get the i-th incoming LLVM parameter
            let llvm_param_val = function
                .get_nth_param(i as u32)
                .ok_or_else(|| format!("Closure parameter missing LLVM value: {}", param_name))?;

            // Give the incoming param a readable name in IR (use cleaned identifier)
            llvm_param_val.set_name(&param_ident);

            // Allocate a stack slot for the parameter and store the incoming value
            let alloca = self
                .builder
                .build_alloca(llvm_param_ty, &format!("{}_slot", param_name))
                .map_err(|e| e.to_string())?;
            let _ = self.builder.build_store(alloca, llvm_param_val);

            // Register the alloca pointer in CodeGen variable map so subsequent
            // gen_expr lookups (for identifiers) find the parameter.
            self.variables.insert(param_ident.clone(), alloca);

            // If the parser attached a textual type (e.g. "f64"), record a best-effort AST type
            // so later codegen that inspects variable_types can behave correctly.
            if let Some(ty_str) = opt_ty_str {
                match ty_str.as_str() {
                    "f64" => {
                        self.variable_types.insert(param_ident.clone(), Type::Float);
                    }
                    "i32" => {
                        self.variable_types.insert(param_ident.clone(), Type::Int);
                    }
                    "str" => {
                        self.variable_types
                            .insert(param_ident.clone(), Type::String);
                    }
                    _ => {
                        // Don't insert unknown textual types
                    }
                }
            }
        }

        let mut last_val: Option<BasicValueEnum<'ctx>> = None;
        for stmt in body {
            last_val = self.gen_statement(stmt, self_type)?;
            if self
                .builder
                .get_insert_block()
                .and_then(|bb| bb.get_terminator())
                .is_some()
            {
                break;
            }
        }

        if !self
            .builder
            .get_insert_block()
            .and_then(|bb| bb.get_terminator())
            .is_some()
        {
            match (last_val, fn_type.get_return_type()) {
                (Some(val), Some(expected_ret_type)) => {
                    if val.get_type() == expected_ret_type {
                        self.builder.build_return(Some(&val)).unwrap();
                    } else {
                        warn!(
                            closure = %closure_name_str,
                            expected = ?expected_ret_type,
                            got = ?val.get_type(),
                            "Closure return type mismatch. Attempting bitcast."
                        );

                        let basic_expected_type =
                            BasicTypeEnum::try_from(expected_ret_type).unwrap();
                        let coerced_val = self.coerce_value_to_type(
                            val,
                            basic_expected_type,
                            "closure_ret_coerce",
                        )?;

                        self.builder.build_return(Some(&coerced_val)).unwrap();
                    }
                }
                (Some(_), None) => {
                    self.builder.build_return(None).unwrap();
                }
                (None, Some(_)) => {
                    return Err(format!(
                        "Closure '{}' must return a value.",
                        closure_name_str
                    ));
                }
                (None, None) => {
                    self.builder.build_return(None).unwrap();
                }
            }
        }

        self.variables = old_vars;
        self.variable_types = old_var_types;
        if let Some(bb) = old_builder_pos {
            self.builder.position_at_end(bb);
        } else {
        }

        Ok(function.as_global_value().as_pointer_value().into())
    }
}
