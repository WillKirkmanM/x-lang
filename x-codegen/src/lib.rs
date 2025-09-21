use std::collections::{HashMap, HashSet};

use inkwell::values::GlobalValue;
use inkwell::{
    builder::Builder,
    context::Context,
    execution_engine::ExecutionEngine,
    module::Module,
    types::{BasicTypeEnum, StructType},
    values::{FunctionValue, PointerValue},
    OptimizationLevel,
};
use x_ast::{Program, Statement, TraitDef, Type};
use x_std::StdLib;

use x_logging::{debug, error, info, trace};

pub mod expression;
pub mod extern_fn;
pub mod for_loop;
pub mod function;
pub mod r#if;
pub mod import;
pub mod statement;
pub mod r#struct;
pub mod r#while;
pub use extern_fn::*;
pub mod array;
pub mod assignment;
pub mod r#become;
pub mod coercion;
pub mod r#comparison;
pub mod memoise;
pub mod multi;
pub mod substitute;
pub mod r#trait;
pub mod r#type;

thread_local! {
    static INSTANTIATION_CACHE: std::cell::RefCell<HashMap<String, String>> = std::cell::RefCell::new(HashMap::new());
}

pub struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    execution_engine: ExecutionEngine<'ctx>,
    variables: HashMap<String, PointerValue<'ctx>>,
    stdlib: StdLib<'ctx>,
    pub functions: HashMap<String, FunctionValue<'ctx>>,
    pub external_functions: HashMap<String, FunctionValue<'ctx>>,
    original_to_generated: HashMap<String, String>,
    generated_to_original: HashMap<String, String>,
    imported_functions: HashMap<String, FunctionValue<'ctx>>,
    struct_types: HashMap<String, (StructType<'ctx>, Vec<String>)>,
    traits: HashMap<String, TraitDef>,
    struct_methods: HashMap<String, HashSet<String>>,
    variable_types: HashMap<String, x_ast::Type>,
    current_function: Option<FunctionValue<'ctx>>,
    memoisation_caches: HashMap<String, (GlobalValue<'ctx>, BasicTypeEnum<'ctx>)>,
    monomorph_scope: HashMap<String, x_ast::Type>,
    fn_param_names: HashMap<String, Vec<String>>,
    multi_variants: HashMap<String, Vec<(Vec<Type>, String)>>,
    multi_resolvers: HashSet<String>,
    throws_functions: HashSet<String>,
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new(context: &'ctx Context, module_name: &str) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();

        let stdlib = StdLib::new(context);
        stdlib.link_to_module(&module);

        let execution_engine = module
            .create_jit_execution_engine(OptimizationLevel::None)
            .expect("Failed to create execution engine");

        info!("CodeGen::new: module={}", module_name);
        debug!("stdlib linked into module: module={}", module_name);

        CodeGen {
            context,
            module,
            builder,
            execution_engine,
            variables: HashMap::new(),
            stdlib,
            functions: HashMap::new(),
            external_functions: HashMap::new(),
            original_to_generated: HashMap::new(),
            generated_to_original: HashMap::new(),
            traits: HashMap::new(),
            imported_functions: HashMap::new(),
            struct_methods: HashMap::new(),
            struct_types: HashMap::new(),
            variable_types: HashMap::new(),
            current_function: None,
            memoisation_caches: HashMap::new(),
            monomorph_scope: HashMap::new(), // Initialize the new field
            fn_param_names: HashMap::new(),
            multi_variants: HashMap::new(),
            multi_resolvers: HashSet::new(),
            throws_functions: HashSet::new(),
        }
    }

    pub fn generate(&mut self, program: Program) -> Result<(), String> {
        info!("Starting Program Generation");
        trace!(
            "generate: entering monomorphisation pre-pass; statements={}",
            program.statements.len()
        );

        info!("CodeGen: phase={}", "monomorphisation_prepass");

        let mut generic_structs = HashMap::new();
        let mut generic_functions: HashMap<
            String,
            (
                Vec<(String, Type)>,
                Type,
                Box<Vec<Statement>>,
                bool,
                bool,
                Vec<String>,
            ),
        > = HashMap::new();
        let mut concrete_statements = Vec::new();

        fn sig_contains_type_params(ty: &Type) -> bool {
            match ty {
                Type::TypeParameter(_) => true,
                Type::Ref { inner, .. } => sig_contains_type_params(inner),
                Type::Array(inner) => sig_contains_type_params(inner),
                Type::GenericInstance { type_args, .. } => {
                    type_args.iter().any(sig_contains_type_params)
                }
                // Uppercase custom names like "T", "U"
                Type::Custom(name) => {
                    !name.is_empty() && name.chars().all(|c| c.is_ascii_uppercase())
                }
                _ => false,
            }
        }

        // Collect generic param names occurring in a type (T, U, Self, etc.)
        fn collect_param_names(ty: &Type, out: &mut std::collections::HashSet<String>) {
            match ty {
                Type::TypeParameter(n) => {
                    out.insert(n.clone());
                }
                Type::Ref { inner, .. } => collect_param_names(inner, out),
                Type::Array(inner) => collect_param_names(inner, out),
                Type::GenericInstance { type_args, .. } => {
                    for a in type_args {
                        collect_param_names(a, out);
                    }
                }
                Type::Custom(name)
                    if !name.is_empty() && name.chars().all(|c| c.is_ascii_uppercase()) =>
                {
                    out.insert(name.clone());
                }
                _ => {}
            }
        }

        for stmt in program.statements {
            match stmt {
                Statement::StructDecl(sd) if sd.generic_params.is_some() => {
                    trace!("found generic struct blueprint: name={}", sd.name);
                    generic_structs.insert(sd.name.clone(), sd);
                }
                Statement::Function {
                    generic_params,
                    name,
                    params,
                    return_type,
                    body,
                    is_pure,
                    is_memoised,
                    is_multi: _,
                    is_throws: _,
                } => {
                    // Treat as generic if explicit generics OR signature still contains type params.
                    let sig_has_tps = sig_contains_type_params(&return_type)
                        || params.iter().any(|(_, p)| sig_contains_type_params(p));

                    if generic_params.is_some() || sig_has_tps {
                        let mut gp_set = std::collections::HashSet::new();
                        for (_, p) in &params {
                            collect_param_names(p, &mut gp_set);
                        }
                        collect_param_names(&return_type, &mut gp_set);
                        let gp_list = gp_set.into_iter().collect::<Vec<_>>();
                        trace!(func = %name, generics = ?gp_list, "registering generic function blueprint");
                        generic_functions.insert(
                            name.clone(),
                            (
                                params.clone(),
                                return_type.clone(),
                                body.clone().unwrap_or_default(),
                                is_pure,
                                is_memoised,
                                gp_list,
                            ),
                        );
                    } else {
                        concrete_statements.push(Statement::Function {
                            generic_params: None,
                            name,
                            params,
                            return_type,
                            body,
                            is_pure,
                            is_memoised,
                            is_multi: false,
                            is_throws: false,
                        });
                    }
                }
                other => concrete_statements.push(other),
            }
        }

        let mut i = 0;
        while i < concrete_statements.len() {
            let mut stmt = concrete_statements[i].clone();
            trace!("monomorphisation: checking statement: index={}", i);
            self.check_and_rewrite_statement(
                &mut stmt,
                &generic_structs,
                &generic_functions,
                &mut concrete_statements,
            )?;
            concrete_statements[i] = stmt;
            i += 1;
        }

        info!(
            "Finished monomorphisation pre-pass; concrete_statements={}",
            concrete_statements.len()
        );

        // Instantiate generic functions referenced at call sites (e.g., get_first_Pair_i32_bool)
        if !generic_functions.is_empty() {
            trace!(
                "Instantiating generics referenced by call sites: count={}",
                generic_functions.len()
            );
            let instantiated = self.instantiate_called_generics(
                &concrete_statements,
                &generic_functions,
                &generic_structs,
            )?;
            if !instantiated.is_empty() {
                trace!(
                    "instantiate_called_generics produced {} concrete statements",
                    instantiated.len()
                );
                concrete_statements.extend(instantiated);
            }
        }

        let all_statements = concrete_statements.clone();
        info!(
            "Finished statement collection; total={}",
            all_statements.len()
        );

        info!("Processing struct declarations...");
        self.process_struct_declarations(&all_statements)?;

        info!("Processing standard library imports...");
        self.process_import_statements(&all_statements)?;

        info!("Declaring all function signatures...");
        self.declare_all_functions(&all_statements)?;

        info!("Building multi dispatch resolvers (if any)...");

        self.build_multi_resolvers(&all_statements)?;
        trace!("multi_resolvers: count={}", self.multi_resolvers.len());

        for stmt in &all_statements {
            if let Statement::Function {
                name,
                params,
                body,
                is_multi: true,
                is_pure,
                is_memoised,
                ..
            } = stmt
            {
                if let Some(body_vec) = body.as_ref() {
                    let mut mangled = name.clone();
                    for (_param_name, param_type) in params.iter() {
                        mangled.push_str(&format!("${}", param_type));
                    }

                    if let Some(func) = self.module.get_function(&mangled) {
                        if func.count_basic_blocks() == 0 {
                            trace!("Compiling multi variant impl: {}", mangled);
                            self.compile_function(
                                &mangled,
                                params,
                                body_vec,
                                *is_pure,
                                *is_memoised,
                            )?;
                        }
                    }
                }
            }
        }

        info!("Starting function body compilation pass...");

        self.compile_all_function_bodies(&all_statements)?;
        info!(
            "Completed function body compilation pass; compiled_functions={}",
            self.functions.len()
        );

        info!("Generating main wrapper/entry point...");

        let main_fn_defined = all_statements
            .iter()
            .any(|s| matches!(s, Statement::Function { name, .. } if name == "main"));

        let has_top_level_code = all_statements.iter().any(|stmt| {
            matches!(
                stmt,
                Statement::Expression { .. } | Statement::VariableDecl { .. }
            )
        });

        // If there's no `main` function and no top-level code (e.g., it's a library), skip entry point generation.
        if !main_fn_defined && !has_top_level_code {
            info!("No 'main' function or top-level code found. Skipping entry point generation.");
            return Ok(());
        }

        // Get the `main` function if it was defined, or create a new one if we need a wrapper.
        // This `get_or_insert` pattern prevents panics. We default to a standard C `main` signature.
        let main_fn = self.module.get_function("main").unwrap_or_else(|| {
            let i32_type = self.context.i32_type();
            let fn_type = i32_type.fn_type(&[], false);
            self.module.add_function("main", fn_type, None)
        });

        // If the user defined `fn main`, its body is already compiled. We do not need to do anything else.
        if main_fn_defined {
            info!("'main' function was defined in source, skipping wrapper population.");

            // Ensure the declared `main` actually has a body. In some cases the signature
            // was declared but the body wasn't emitted (resulting in a `declare` in IR).
            // If so, find the AST `main` and compile it now.
            if main_fn.count_basic_blocks() == 0 {
                trace!("Detected declared-only 'main' function; compiling its body now.");
                if let Some(Statement::Function {
                    name: _,
                    params,
                    return_type: _,
                    body,
                    is_pure,
                    is_memoised,
                    ..
                }) = all_statements
                    .iter()
                    .find(|s| matches!(s, Statement::Function { name, .. } if name == "main"))
                    .cloned()
                {
                    if let Some(body_vec) = body {
                        // Compile the function body into the previously-declared function
                        self.compile_function("main", &params, &body_vec, is_pure, is_memoised)?;
                    } else {
                        error!("AST 'main' found but has no body");
                        return Err("AST 'main' has no body".to_string());
                    }
                } else {
                    error!("Declared 'main' not found in AST statements");
                    return Err("Declared 'main' not found in AST".to_string());
                }
            }
        } else {
            // Otherwise, we create a body for `main` that runs the top-level code.
            let entry_block = self.context.append_basic_block(main_fn, "entry");
            self.builder.position_at_end(entry_block);

            self.initialise_memoisation_caches()?;

            // Compile all top-level statements.
            for stmt in all_statements.iter() {
                if !matches!(
                    stmt,
                    Statement::Function { .. }
                        | Statement::StructDecl(_)
                        | Statement::Import { .. }
                        | Statement::FileImport { .. }
                        | Statement::ExternFunctionDecl { .. }
                ) {
                    trace!("generating top-level stmt in wrapper: stmt={:?}", stmt);
                    self.gen_statement(stmt)?;
                }
            }

            // Add an implicit `return` at the end of the wrapper, matching the function's signature.
            // Does not check the entry block (it may already be terminated, e.g. by a while jump).
            // Instead, emit the return in the current block of `main`, if it isn't terminated yet.
            let target_bb = self
                .builder
                .get_insert_block()
                .filter(|bb| bb.get_parent().map(|f| f == main_fn).unwrap_or(false))
                .unwrap_or(entry_block);

            if target_bb.get_terminator().is_none() {
                self.builder.position_at_end(target_bb);
                match main_fn.get_type().get_return_type() {
                    Some(ret_type) => {
                        if ret_type.is_int_type() {
                            let i32_type = self.context.i32_type();
                            self.builder
                                .build_return(Some(&i32_type.const_int(0, false)))
                                .unwrap();
                        } else if ret_type.is_float_type() {
                            let f64_type = self.context.f64_type();
                            self.builder
                                .build_return(Some(&f64_type.const_float(0.0)))
                                .unwrap();
                        } else {
                            error!(
                                "Unsupported return type for implicit main: ret_type={:?}",
                                ret_type
                            );
                            return Err(format!(
                                "Unsupported return type for implicit main: {:?}",
                                ret_type
                            ));
                        }
                    }
                    None => {
                        // This is a void function
                        self.builder.build_return(None).unwrap();
                    }
                }
            }
        }

        info!("Finished generating main.");

        // Re-run function-body compilation to catch functions that were registered
        // while generating the main wrapper (e.g. anonymous functions / closures).
        // This ensures closures compiled during top-level codegen get their bodies emitted.
        self.compile_all_function_bodies(&all_statements)?;
        info!(
            "Completed second function body compilation pass; compiled_functions={}",
            self.functions.len()
        );

        Ok(())
    }

    pub fn jit_execute(&self) -> Result<f64, String> {
        unsafe {
            // Prefer a function literally named "main" if present, otherwise pick the first defined function.
            let target_name = if self.module.get_function("main").is_some() {
                Some("main".to_string())
            } else {
                // Find first non-declaration function (has at least one basic block)
                self.module
                    .get_functions()
                    .into_iter()
                    .find(|f| f.count_basic_blocks() > 0)
                    .and_then(|f| f.get_name().to_str().ok().map(|s| s.to_string()))
            };

            let name = match target_name {
                Some(n) => n,
                None => return Err("No executable function found in module".to_string()),
            };

            info!("jit_execute: selected target: name={}", name);

            // Try a few common return signatures, converting to f64 as needed.
            if let Ok(func) = self
                .execution_engine
                .get_function::<unsafe extern "C" fn() -> f64>(&name)
            {
                info!("jit_execute: calling -> f64");
                return Ok(func.call());
            }
            if let Ok(func) = self
                .execution_engine
                .get_function::<unsafe extern "C" fn() -> f32>(&name)
            {
                info!("jit_execute: calling -> f32");
                return Ok(func.call() as f64);
            }
            if let Ok(func) = self
                .execution_engine
                .get_function::<unsafe extern "C" fn() -> i64>(&name)
            {
                info!("jit_execute: calling -> i64");
                return Ok(func.call() as f64);
            }
            if let Ok(func) = self
                .execution_engine
                .get_function::<unsafe extern "C" fn() -> i32>(&name)
            {
                info!("jit_execute: calling -> i32");
                return Ok(func.call() as f64);
            }
            if let Ok(func) = self
                .execution_engine
                .get_function::<unsafe extern "C" fn() -> ()>(&name)
            {
                info!("jit_execute: calling -> void()");
                func.call();
                return Ok(0.0);
            }

            error!(
                "jit_execute: no matching callable signature: target={}",
                name
            );
            Err(format!(
                "Found function '{}' but could not match a callable signature",
                name
            ))
        }
    }

    pub fn create_entry_block_alloca(
        &self,
        function: FunctionValue<'ctx>,
        name: &str,
    ) -> PointerValue<'ctx> {
        let builder = self.context.create_builder();
        let entry = function.get_first_basic_block().unwrap();

        match entry.get_first_instruction() {
            Some(first_instr) => builder.position_before(&first_instr),
            None => builder.position_at_end(entry),
        }

        let alloca = builder.build_alloca(self.context.f64_type(), name).unwrap();
        debug!("created entry alloca: name={}", name);
        alloca
    }

    pub fn get_ir(&self) -> String {
        let ir = self.module.print_to_string().to_string();
        debug!("get_ir: len={}", ir.len());
        ir
    }
}
