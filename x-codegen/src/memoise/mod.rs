use inkwell::types::BasicType;
use inkwell::values::FunctionValue;
use inkwell::AddressSpace;
use x_ast::Statement;

use crate::CodeGen;

impl<'ctx> CodeGen<'ctx> {
    pub fn compile_memoised_function(
        &mut self,
        name: &str,
        params: &[String],
        body: &[Statement],
    ) -> Result<FunctionValue<'ctx>, String> {
        let wrapper_fn = self.module.get_function(name).unwrap();
        let fn_type = wrapper_fn.get_type();
        let return_type = fn_type
            .get_return_type()
            .ok_or("Memoised function must have a return type")?;
        let impl_name = format!("{}.impl", name);

        let i8_ptr_type = self.context.ptr_type(AddressSpace::default());
        let i64_type = self.context.i64_type();
        let cache_global = self
            .module
            .add_global(i8_ptr_type, None, &format!("{}_cache", name));
        cache_global.set_initializer(&i8_ptr_type.const_null());

        let get_fn_type = i8_ptr_type.fn_type(&[i8_ptr_type.into(), i64_type.into()], false);

        let get_fn = self
            .module
            .get_function("cache_get")
            .unwrap_or_else(|| self.module.add_function("cache_get", get_fn_type, None));

        let set_fn_type = self.context.void_type().fn_type(
            &[
                i8_ptr_type.into(),
                i8_ptr_type.into(),
                i64_type.into(),
                i8_ptr_type.into(),
            ],
            false,
        );

        let set_fn = self
            .module
            .get_function("cache_set")
            .unwrap_or_else(|| self.module.add_function("cache_set", set_fn_type, None));

        self.memoisation_caches
            .insert(name.to_string(), (cache_global, return_type.into()));

        self.module.add_function(&impl_name, fn_type, None);
        self.compile_function(&impl_name, params, body, true, false)?;

        let entry = self.context.append_basic_block(wrapper_fn, "entry");
        let cache_hit = self.context.append_basic_block(wrapper_fn, "cache.hit");
        let cache_miss = self.context.append_basic_block(wrapper_fn, "cache.miss");
        let end_block = self.context.append_basic_block(wrapper_fn, "end");

        self.builder.position_at_end(entry);

        let mut key_size = 0u32;

        let param_types: Vec<_> = wrapper_fn
            .get_type()
            .get_param_types()
            .into_iter()
            .map(|t| inkwell::types::BasicTypeEnum::try_from(t).unwrap())
            .collect();

        for p_type in &param_types {
            key_size += p_type
                .size_of()
                .and_then(|v| v.get_zero_extended_constant())
                .unwrap_or(0) as u32;
        }

        let key_size_val = self.context.i64_type().const_int(key_size as u64, false);
        let key_buffer = self
            .builder
            .build_alloca(self.context.i8_type().array_type(key_size), "key_buffer")
            .unwrap();

        let mut offset = 0;
        for (i, param) in wrapper_fn.get_param_iter().enumerate() {
            let param_ptr = self
                .builder
                .build_pointer_cast(
                    key_buffer,
                    self.context.ptr_type(AddressSpace::default()),
                    "",
                )
                .unwrap();
            let gep = unsafe {
                self.builder
                    .build_gep(
                        self.context.i8_type(),
                        param_ptr,
                        &[self.context.i64_type().const_int(offset as u64, false)],
                        "gep",
                    )
                    .unwrap()
            };

            let casted_gep = self
                .builder
                .build_pointer_cast(
                    gep,
                    self.context.ptr_type(AddressSpace::default()),
                    "casted_gep",
                )
                .unwrap();

            self.builder.build_store(casted_gep, param).unwrap();

            offset += param_types[i]
                .size_of()
                .and_then(|v| v.get_zero_extended_constant())
                .unwrap_or(0) as u64;
        }

        let loaded_cache_ptr = self
            .builder
            .build_load(i8_ptr_type, cache_global.as_pointer_value(), "load_cache")
            .unwrap();
        let key_buffer_ptr = self
            .builder
            .build_pointer_cast(key_buffer, i8_ptr_type, "key_ptr")
            .unwrap();
        let cached_result_ptr = self
            .builder
            .build_call(
                get_fn,
                &[
                    loaded_cache_ptr.into(),
                    key_buffer_ptr.into(),
                    key_size_val.into(),
                ],
                "call_get",
            )
            .unwrap()
            .try_as_basic_value()
            .left()
            .unwrap()
            .into_pointer_value();

        let is_miss = self
            .builder
            .build_is_null(cached_result_ptr, "is_miss")
            .unwrap();
        self.builder
            .build_conditional_branch(is_miss, cache_miss, cache_hit)
            .unwrap();

        self.builder.position_at_end(cache_miss);
        let impl_fn = self.module.get_function(&impl_name).unwrap();
        let args: Vec<_> = wrapper_fn.get_param_iter().map(|a| a.into()).collect();
        let computed_val = self
            .builder
            .build_call(impl_fn, &args, "call_impl")
            .unwrap()
            .try_as_basic_value()
            .left()
            .unwrap();

        let malloc_size = return_type.size_of().unwrap();
        let malloc_fn = self.get_malloc_fn();
        let result_mem = self
            .builder
            .build_call(malloc_fn, &[malloc_size.into()], "malloc_result")
            .unwrap()
            .try_as_basic_value()
            .left()
            .unwrap()
            .into_pointer_value();
        let typed_result_mem = self
            .builder
            .build_pointer_cast(
                result_mem,
                self.context.ptr_type(AddressSpace::default()),
                "",
            )
            .unwrap();
        self.builder
            .build_store(typed_result_mem, computed_val)
            .unwrap();

        self.builder
            .build_call(
                set_fn,
                &[
                    loaded_cache_ptr.into(),
                    key_buffer_ptr.into(),
                    key_size_val.into(),
                    result_mem.into(),
                ],
                "",
            )
            .unwrap();
        self.builder.build_unconditional_branch(end_block).unwrap();

        self.builder.position_at_end(cache_hit);
        let typed_cached_ptr = self
            .builder
            .build_pointer_cast(
                cached_result_ptr,
                self.context.ptr_type(AddressSpace::default()),
                "",
            )
            .unwrap();
        let loaded_val = self
            .builder
            .build_load(return_type, typed_cached_ptr, "load_cached")
            .unwrap();
        self.builder.build_unconditional_branch(end_block).unwrap();

        self.builder.position_at_end(end_block);
        let phi = self.builder.build_phi(return_type, "retval").unwrap();
        phi.add_incoming(&[(&computed_val, cache_miss), (&loaded_val, cache_hit)]);
        self.builder
            .build_return(Some(&phi.as_basic_value()))
            .unwrap();

        Ok(wrapper_fn)
    }

    fn get_malloc_fn(&self) -> FunctionValue<'ctx> {
        if let Some(func) = self.module.get_function("malloc") {
            return func;
        }
        let i8_ptr_type = self.context.ptr_type(AddressSpace::default());
        let i64_type = self.context.i64_type();
        let malloc_type = i8_ptr_type.fn_type(&[i64_type.into()], false);
        self.module.add_function("malloc", malloc_type, None)
    }
}
