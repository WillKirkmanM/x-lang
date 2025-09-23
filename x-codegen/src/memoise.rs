use inkwell::types::BasicTypeEnum;
use inkwell::values::FunctionValue;
use inkwell::AddressSpace;
use x_ast::{Param, Statement};

use crate::CodeGen;

impl<'ctx> CodeGen<'ctx> {
    /// Compiles a memoised function.
    /// This creates a wrapper function that handles caching logic and calls an inner ".impl" function
    /// for the actual computation on a cache miss.
    pub fn compile_memoised_function(
        &mut self,
        name: &str,
        params: Vec<Param>,
        body: &[Statement],
    ) -> Result<FunctionValue<'ctx>, String> {
        let wrapper_fn = self.module.get_function(name).unwrap();
        let fn_type = wrapper_fn.get_type();
        let return_type = fn_type
            .get_return_type()
            .ok_or("Memoised function must have a return type")?;
        let impl_name = format!("{}.impl", name);

        // Declare C Runtime Components & Global Cache
        let i8_ptr_type = self.context.ptr_type(AddressSpace::default());
        let i32_type = self.context.i32_type();
        let i64_type = self.context.i64_type();

        let cache_global = self
            .module
            .add_global(i8_ptr_type, None, &format!("{}_cache", name));
        cache_global.set_initializer(&i8_ptr_type.const_null());

        // Use helper functions to get or declare C runtime functions with correct signatures
        let create_fn = self.get_cache_create_fn();
        let get_fn = self.get_cache_get_fn();
        let set_fn = self.get_cache_set_fn();
        let memset_fn = self.get_memset_fn();
        let malloc_fn = self.get_malloc_fn();

        self.memoisation_caches
            .insert(name.to_string(), (cache_global, return_type.into()));

        // Create and compile the core implementation function
        self.module.add_function(&impl_name, fn_type, None);
        self.compile_function(
            &impl_name,
            params
                .iter()
                .map(|p| (p.name.clone(), p.ty.clone()))
                .collect::<Vec<_>>(),
            body,
            true,
            false,
        )?;

        // Wrapper Function Logic
        let entry = self.context.append_basic_block(wrapper_fn, "entry");
        let init_cache_block = self.context.append_basic_block(wrapper_fn, "init_cache");
        let cache_ready_block = self.context.append_basic_block(wrapper_fn, "cache_ready");
        let cache_hit = self.context.append_basic_block(wrapper_fn, "cache.hit");
        let cache_miss = self.context.append_basic_block(wrapper_fn, "cache.miss");
        let end_block = self.context.append_basic_block(wrapper_fn, "end");

        self.builder.position_at_end(entry);

        // Check if cache is initialised
        let loaded_cache_ptr = self
            .builder
            .build_load(i8_ptr_type, cache_global.as_pointer_value(), "load_cache")
            .unwrap()
            .into_pointer_value();

        let cache_is_null = self
            .builder
            .build_is_null(loaded_cache_ptr, "cache_is_null")
            .unwrap();

        self.builder
            .build_conditional_branch(cache_is_null, init_cache_block, cache_ready_block)
            .unwrap();

        // Initialise cache if it's null
        self.builder.position_at_end(init_cache_block);
        let cache_size = i32_type.const_int(1024, false);
        let new_cache = self
            .builder
            .build_call(create_fn, &[cache_size.into()], "create_cache")
            .unwrap()
            .try_as_basic_value()
            .left()
            .unwrap()
            .into_pointer_value();

        self.builder
            .build_store(cache_global.as_pointer_value(), new_cache)
            .unwrap();
        self.builder
            .build_unconditional_branch(cache_ready_block)
            .unwrap();

        // Cache is ready, proceed to build the key
        self.builder.position_at_end(cache_ready_block);
        let final_cache_ptr = self
            .builder
            .build_load(i8_ptr_type, cache_global.as_pointer_value(), "final_cache")
            .unwrap()
            .into_pointer_value();

        // Key Generation
        let target_data = self.execution_engine.get_target_data();
        let mut total_key_size: u64 = 0;

        for p_meta_type in fn_type.get_param_types() {
            let p_type = BasicTypeEnum::try_from(p_meta_type)
                .map_err(|_| "Memoisation key contains a non-basic type.".to_string())?;

            let type_size = target_data.get_store_size(&p_type);
            let type_align = target_data.get_abi_alignment(&p_type) as u64;

            // Align the current offset to the required alignment of the current type
            total_key_size = (total_key_size + type_align - 1) & !(type_align - 1);
            total_key_size += type_size;
        }

        let key_size_val = i64_type.const_int(total_key_size, false);
        let key_buffer = self
            .builder
            .build_alloca(
                self.context.i8_type().array_type(total_key_size as u32),
                "key_buffer",
            )
            .unwrap();

        // Zero-initialise the key buffer to avoid garbage data in padding bytes.
        self.builder
            .build_call(
                memset_fn,
                &[
                    key_buffer.into(),
                    i32_type.const_zero().into(),
                    key_size_val.into(),
                ],
                "",
            )
            .unwrap();

        // Store each parameter into the key buffer at its correctly aligned offset.
        let mut current_offset: u64 = 0;
        for param in wrapper_fn.get_param_iter() {
            let p_type = param.get_type();
            let type_size = target_data.get_store_size(&p_type);
            let type_align = target_data.get_abi_alignment(&p_type) as u64;

            // Align the current offset for the store operation
            current_offset = (current_offset + type_align - 1) & !(type_align - 1);

            let offset_ptr = unsafe {
                self.builder
                    .build_gep(
                        self.context.i8_type(),
                        key_buffer,
                        &[i64_type.const_int(current_offset, false)],
                        "offset_ptr",
                    )
                    .unwrap()
            };

            let typed_ptr = self
                .builder
                .build_pointer_cast(
                    offset_ptr,
                    self.context.ptr_type(AddressSpace::default()),
                    "typed_key_ptr",
                )
                .unwrap();

            self.builder.build_store(typed_ptr, param).unwrap();
            current_offset += type_size;
        }

        // Cache Lookup
        let cached_result_ptr = self
            .builder
            .build_call(
                get_fn,
                &[
                    final_cache_ptr.into(),
                    key_buffer.into(),
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

        // Cache Miss: Compute, store, and return value
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

        let result_size = target_data.get_store_size(&return_type);
        let result_mem = self
            .builder
            .build_call(
                malloc_fn,
                &[i64_type.const_int(result_size, false).into()],
                "malloc_result",
            )
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
                "typed_result_ptr",
            )
            .unwrap();
        self.builder
            .build_store(typed_result_mem, computed_val)
            .unwrap();

        self.builder
            .build_call(
                set_fn,
                &[
                    final_cache_ptr.into(),
                    key_buffer.into(),
                    key_size_val.into(),
                    result_mem.into(),
                ],
                "",
            )
            .unwrap();
        self.builder.build_unconditional_branch(end_block).unwrap();

        // Cache Hit: Load and return cached value
        self.builder.position_at_end(cache_hit);
        let typed_cached_ptr = self
            .builder
            .build_pointer_cast(
                cached_result_ptr,
                self.context.ptr_type(AddressSpace::default()),
                "typed_cached_ptr",
            )
            .unwrap();
        let loaded_val = self
            .builder
            .build_load(return_type, typed_cached_ptr, "load_cached")
            .unwrap();
        self.builder.build_unconditional_branch(end_block).unwrap();

        // End: Return the final result
        self.builder.position_at_end(end_block);
        let phi = self.builder.build_phi(return_type, "retval").unwrap();
        phi.add_incoming(&[(&computed_val, cache_miss), (&loaded_val, cache_hit)]);
        self.builder
            .build_return(Some(&phi.as_basic_value()))
            .unwrap();

        Ok(wrapper_fn)
    }

    // Helper functions to get or declare C runtime functions

    fn get_cache_create_fn(&self) -> FunctionValue<'ctx> {
        self.module.get_function("cache_create").unwrap_or_else(|| {
            let i8_ptr_type = self.context.ptr_type(AddressSpace::default());
            let i32_type = self.context.i32_type();
            let fn_type = i8_ptr_type.fn_type(&[i32_type.into()], false);
            self.module.add_function("cache_create", fn_type, None)
        })
    }

    fn get_cache_get_fn(&self) -> FunctionValue<'ctx> {
        self.module.get_function("cache_get").unwrap_or_else(|| {
            let i8_ptr_type = self.context.ptr_type(AddressSpace::default());
            let i64_type = self.context.i64_type();
            let fn_type = i8_ptr_type.fn_type(
                &[i8_ptr_type.into(), i8_ptr_type.into(), i64_type.into()],
                false,
            );
            self.module.add_function("cache_get", fn_type, None)
        })
    }

    fn get_cache_set_fn(&self) -> FunctionValue<'ctx> {
        self.module.get_function("cache_set").unwrap_or_else(|| {
            let i8_ptr_type = self.context.ptr_type(AddressSpace::default());
            let i64_type = self.context.i64_type();
            let fn_type = self.context.void_type().fn_type(
                &[
                    i8_ptr_type.into(),
                    i8_ptr_type.into(),
                    i64_type.into(),
                    i8_ptr_type.into(),
                ],
                false,
            );
            self.module.add_function("cache_set", fn_type, None)
        })
    }

    fn get_memset_fn(&self) -> FunctionValue<'ctx> {
        self.module.get_function("memset").unwrap_or_else(|| {
            let i8_ptr_type = self.context.ptr_type(AddressSpace::default());
            let i32_type = self.context.i32_type();
            let i64_type = self.context.i64_type();
            let fn_type = i8_ptr_type.fn_type(
                &[i8_ptr_type.into(), i32_type.into(), i64_type.into()],
                false,
            );
            self.module.add_function("memset", fn_type, None)
        })
    }

    fn get_malloc_fn(&self) -> FunctionValue<'ctx> {
        self.module.get_function("malloc").unwrap_or_else(|| {
            let i8_ptr_type = self.context.ptr_type(AddressSpace::default());
            let i64_type = self.context.i64_type();
            let fn_type = i8_ptr_type.fn_type(&[i64_type.into()], false);
            self.module.add_function("malloc", fn_type, None)
        })
    }

    pub fn initialise_all_caches(&self) {
        if !self.memoisation_caches.is_empty() {
            let create_fn = self.get_cache_create_fn();
            let capacity = self.context.i32_type().const_int(1024, false);

            for name in self.memoisation_caches.keys() {
                let cache_global_name = format!("{}_cache", name);
                let cache_global = self.module.get_global(&cache_global_name).unwrap();

                let new_cache_ptr = self
                    .builder
                    .build_call(create_fn, &[capacity.into()], "create_cache")
                    .unwrap()
                    .try_as_basic_value()
                    .left()
                    .unwrap();

                self.builder
                    .build_store(cache_global.as_pointer_value(), new_cache_ptr)
                    .unwrap();
            }
        }
    }

    pub fn initialise_memoisation_caches(&mut self) -> Result<(), String> {
        if self.memoisation_caches.is_empty() {
            return Ok(());
        }

        let create_fn = self.module.get_function("cache_create").unwrap_or_else(|| {
            let cache_ptr_type = self.context.ptr_type(AddressSpace::default());
            let i32_type = self.context.i32_type();
            let create_fn_type = cache_ptr_type.fn_type(&[i32_type.into()], false);
            self.module
                .add_function("cache_create", create_fn_type, None)
        });

        for name in self.memoisation_caches.keys().cloned().collect::<Vec<_>>() {
            let cache_global_name = format!("{}_cache", name);
            let cache_global = self.module.get_global(&cache_global_name).ok_or_else(|| {
                format!("Memoisation cache global '{}' not found", cache_global_name)
            })?;

            let capacity = self.context.i32_type().const_int(1024, false);
            let new_cache_ptr = self
                .builder
                .build_call(create_fn, &[capacity.into()], "create_cache")
                .map_err(|e| e.to_string())?
                .try_as_basic_value()
                .left()
                .ok_or("cache_create call did not return a value")?;

            self.builder
                .build_store(cache_global.as_pointer_value(), new_cache_ptr)
                .map_err(|e| e.to_string())?;
        }
        Ok(())
    }
}
