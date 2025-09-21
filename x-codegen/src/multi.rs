use inkwell::AddressSpace;
use x_ast::{Statement, Type};

use crate::CodeGen;

mod fxhash {
    pub fn hash64(s: &str) -> u64 {
        let mut h: u64 = 0xcbf29ce484222325;
        for b in s.as_bytes() {
            h ^= *b as u64;
            h = h.wrapping_mul(0x100000001b3);
        }
        h
    }
}

impl<'ctx> CodeGen<'ctx> {
    /// Build resolver bodies for each multi function name (2-arg only).
    /// Uses a switch over a combined (tid0<<32 | tid1) i64 key.
    pub fn build_multi_resolvers(&mut self, all_statements: &[Statement]) -> Result<(), String> {
        use inkwell::values::{BasicValueEnum, IntValue};

        let i32_ty = self.context.i32_type();
        let i64_ty = self.context.i64_type();
        let i8_ptr = self.context.ptr_type(AddressSpace::default());

        // Declare extern for runtime type ID: i32 __xlang_type_id(ptr)
        if self.module.get_function("__xlang_type_id").is_none() {
            let tid_fn_ty = i32_ty.fn_type(&[i8_ptr.into()], false);
            self.module.add_function("__xlang_type_id", tid_fn_ty, None);
        }
        let tid_fn = self.module.get_function("__xlang_type_id").unwrap();

        for stmt in all_statements {
            if let Statement::Function {
                name,
                params,
                return_type,
                is_multi,
                ..
            } = stmt
            {
                if !*is_multi {
                    continue;
                }
                let Some(resolver) = self.module.get_function(name) else {
                    continue;
                };

                if resolver.get_first_basic_block().is_some() {
                    continue;
                }

                let entry = self.context.append_basic_block(resolver, "entry");
                self.builder.position_at_end(entry);

                if params.len() < 2 {
                    return Err(format!("multi '{}' requires at least 2 parameters", name));
                }

                let a0 = resolver
                    .get_nth_param(0)
                    .ok_or_else(|| "resolver missing arg0".to_string())?;
                let a1 = resolver
                    .get_nth_param(1)
                    .ok_or_else(|| "resolver missing arg1".to_string())?;
                let p0 = match a0 {
                    BasicValueEnum::PointerValue(p) => p,
                    _ => {
                        return Err(format!(
                            "multi '{}' requires first param to be a reference/pointer type",
                            name
                        ))
                    }
                };
                let p1 = match a1 {
                    BasicValueEnum::PointerValue(p) => p,
                    _ => {
                        return Err(format!(
                            "multi '{}' requires second param to be a reference/pointer type",
                            name
                        ))
                    }
                };

                let a0_cast = self
                    .builder
                    .build_pointer_cast(p0, i8_ptr, "a0_cast")
                    .map_err(|e| e.to_string())?;
                let a1_cast = self
                    .builder
                    .build_pointer_cast(p1, i8_ptr, "a1_cast")
                    .map_err(|e| e.to_string())?;
                let tid0 = self
                    .builder
                    .build_call(tid_fn, &[a0_cast.into()], "tid0")
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .unwrap()
                    .into_int_value();
                let tid1 = self
                    .builder
                    .build_call(tid_fn, &[a1_cast.into()], "tid1")
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value()
                    .left()
                    .unwrap()
                    .into_int_value();

                // key = (u64)tid0 << 32 | (u64)tid1
                let t0ext = self
                    .builder
                    .build_int_z_extend(tid0, i64_ty, "t0z")
                    .map_err(|e| e.to_string())?;
                let t1ext = self
                    .builder
                    .build_int_z_extend(tid1, i64_ty, "t1z")
                    .map_err(|e| e.to_string())?;
                let t0sh = self
                    .builder
                    .build_left_shift(t0ext, i64_ty.const_int(32, false), "t0sh")
                    .map_err(|e| e.to_string())?;
                let key = self
                    .builder
                    .build_or(t0sh, t1ext, "key")
                    .map_err(|e| e.to_string())?;

                // Prepare default block and case vector
                let default_bb = self.context.append_basic_block(resolver, "multi_default");

                let mut cases: Vec<(IntValue<'ctx>, inkwell::basic_block::BasicBlock<'ctx>)> =
                    Vec::new();
                let mut case_blocks: Vec<(inkwell::basic_block::BasicBlock<'ctx>, String)> =
                    Vec::new();

                if let Some(variants) = self.multi_variants.get(name) {
                    for (ptys, mangled) in variants {
                        if ptys.len() < 2 {
                            continue;
                        }
                        // Compile-time 32-bit IDs derived from type names
                        let t0s = ptys[0].to_string();
                        let t1s = ptys[1].to_string();
                        let id0 = (fxhash::hash64(&t0s) & 0xffff_ffff) as u64;
                        let id1 = (fxhash::hash64(&t1s) & 0xffff_ffff) as u64;
                        let case_key = i64_ty.const_int((id0 << 32) | id1, false);
                        let bb = self
                            .context
                            .append_basic_block(resolver, &format!("case_{}_{}", id0, id1));
                        cases.push((case_key, bb));
                        case_blocks.push((bb, mangled.clone()));
                    }
                }

                // Build the switch with all cases at once (no add_case on Result)
                self.builder
                    .build_switch(key, default_bb, &cases)
                    .map_err(|e| e.to_string())?;

                // Emit each case block
                for (bb, mangled) in case_blocks {
                    self.builder.position_at_end(bb);
                    let target = self
                        .module
                        .get_function(&mangled)
                        .ok_or_else(|| format!("missing multi variant '{}'", mangled))?;
                    let args: Vec<_> = resolver.get_param_iter().map(|a| a.into()).collect();
                    let call = self
                        .builder
                        .build_call(target, &args, "mdispatch")
                        .map_err(|e| e.to_string())?;
                    if *return_type == Type::Void {
                        self.builder.build_return(None).map_err(|e| e.to_string())?;
                    } else {
                        let rv = call.try_as_basic_value().left().ok_or_else(|| {
                            format!("multi variant '{}' did not produce a value", mangled)
                        })?;
                        self.builder
                            .build_return(Some(&rv))
                            .map_err(|e| e.to_string())?;
                    }
                }

                self.builder.position_at_end(default_bb);
                if *return_type == Type::Void {
                    self.builder.build_return(None).map_err(|e| e.to_string())?;
                } else {
                    let ret_any = resolver.get_type().get_return_type().unwrap();
                    let zero: BasicValueEnum<'ctx> = if ret_any.is_float_type() {
                        ret_any.into_float_type().const_float(0.0).into()
                    } else if ret_any.is_int_type() {
                        ret_any.into_int_type().const_zero().into()
                    } else if ret_any.is_pointer_type() {
                        ret_any.into_pointer_type().const_null().into()
                    } else {
                        return Err("Unsupported return type in multi resolver default".to_string());
                    };
                    self.builder
                        .build_return(Some(&zero))
                        .map_err(|e| e.to_string())?;
                }

                if !resolver.verify(true) {
                    return Err(format!("Invalid multi resolver generated: {}", name));
                }
            }
        }

        Ok(())
    }
}
