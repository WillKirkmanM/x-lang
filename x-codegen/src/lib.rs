use std::collections::{HashMap, HashSet};
use std::path::Path;
use std::fs;

use inkwell::{
    builder::Builder,
    context::Context,
    execution_engine::ExecutionEngine,
    module::Module,
    types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, StructType},
    values::{FunctionValue, PointerValue},
    OptimizationLevel, AddressSpace,
};
use x_parser::parse;
use x_ast::{Expr, ExternParam, Program, Statement, StructDef};
use x_std::StdLib;

pub mod expression;
pub mod function;
pub mod import;
pub mod statement;
pub mod for_loop;
pub mod r#if;
pub mod r#while;
pub mod r#struct;
pub mod extern_fn;

pub struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    execution_engine: ExecutionEngine<'ctx>,
    variables: HashMap<String, PointerValue<'ctx>>,
    stdlib: StdLib<'ctx>,
    functions: HashMap<String, FunctionValue<'ctx>>,
    external_functions: HashMap<String, FunctionValue<'ctx>>,
    original_to_generated: HashMap<String, String>,
    generated_to_original: HashMap<String, String>,
    imported_functions: HashMap<String, FunctionValue<'ctx>>,
    current_binding_name: Option<String>,
    struct_types: HashMap<String, (StructType<'ctx>, Vec<String>)>,
    variable_types: HashMap<String, String>,
    processed_files: HashSet<String>,
    current_function: Option<FunctionValue<'ctx>>,
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
            imported_functions: HashMap::new(),
            current_binding_name: None,
            struct_types: HashMap::new(),
            variable_types: HashMap::new(),
            processed_files: HashSet::new(),
            current_function: None,
        }
    }

    fn map_type(&self, type_name: &str) -> Result<BasicTypeEnum<'ctx>, String> {
        match type_name {
            "f64" => Ok(self.context.f64_type().into()),
            "i32" => Ok(self.context.i32_type().into()),
            "i64" => Ok(self.context.i64_type().into()),
            "i8" => Ok(self.context.i8_type().into()),
            "str" => Ok(self.context.ptr_type(AddressSpace::default()).into()),
            "array_ptr" => Ok(self.context.ptr_type(AddressSpace::default()).into()),
            "fn_ptr" => {
                let _void_fn_type = self.context.void_type().fn_type(&[], false);
                Ok(self.context.ptr_type(AddressSpace::default()).into())
            }
            struct_name if self.struct_types.contains_key(struct_name) => {
                Ok(self.struct_types[struct_name].0.into())
            }
            struct_ptr_name if struct_ptr_name.ends_with("_ptr") => {
                let base_name = &struct_ptr_name[..struct_ptr_name.len() - 4];
                if let Some((_struct_type, _)) = self.struct_types.get(base_name) {
                    Ok(self.context.ptr_type(AddressSpace::default()).into())
                } else {
                    Err(format!("Unknown base struct type for pointer: {}", base_name))
                }
            }
            _ => Err(format!("Unknown type name during map_type: '{}'", type_name)),
        }
    }

    fn declare_struct(&mut self, struct_def: &StructDef) -> Result<(), String> {
        if self.struct_types.contains_key(&struct_def.name) {
            println!("[CodeGen Warning] Struct '{}' already declared. Skipping.", struct_def.name);
            return Ok(());
        }

        let field_types: Vec<BasicTypeEnum<'ctx>> = struct_def
            .fields
            .iter()
            .map(|_field_name| self.context.f64_type().into())
            .collect();

        let struct_type = self.context.opaque_struct_type(&struct_def.name);
        struct_type.set_body(&field_types, false);

        let field_names: Vec<String> = struct_def.fields.iter().cloned().collect();
        self.struct_types.insert(struct_def.name.clone(), (struct_type, field_names));

        println!("[CodeGen] Declared struct '{}' with fields: {:?}", struct_def.name, struct_def.fields);
        Ok(())
    }

    fn collect_statements_recursive(
        &mut self,
        statements: Vec<Statement>,
        collected: &mut Vec<Statement>,
        current_dir: &str,
    ) -> Result<(), String> {
        for stmt in statements {
            match stmt {
                Statement::FileImport { ref path } => {
                    let import_path = Path::new(current_dir).join(path);
                    let canonical_path = match import_path.canonicalize() {
                        Ok(p) => p.to_string_lossy().into_owned(),
                        Err(e) => return Err(format!("Failed to find import file '{}' (resolved to '{}'): {}", path, import_path.display(), e)),
                    };

                    if self.processed_files.insert(canonical_path.clone()) {
                        println!("[CodeGen] Processing import: {}", canonical_path);
                        let source = match fs::read_to_string(&canonical_path) {
                             Ok(s) => s,
                             Err(e) => return Err(format!("Failed to read import file '{}': {}", path, e)),
                        };

                        let imported_program = match parse(&source) {
                             Ok(p) => p,
                             Err(e) => return Err(format!("Failed to parse import file '{}': {}", path, e)),
                        };

                        let parent_dir = Path::new(&canonical_path).parent().unwrap_or_else(|| Path::new(".")).to_str().unwrap_or(".");

                        self.collect_statements_recursive(imported_program.statements, collected, parent_dir)?;
                    } else {
                        println!("[CodeGen] Skipping already processed import: {}", canonical_path);
                    }
                }
                Statement::Import { .. } => {
                    collected.push(stmt);
                }
                 Statement::Function { ref name, .. } => {
                    println!("[CodeGen] Collected function statement: {}", name);
                    collected.push(stmt);
                }
                _ => {
                    collected.push(stmt);
                }
            }
        }
        Ok(())
    }

    pub fn generate(&mut self, program: Program) -> Result<(), String> {
        let mut all_statements = Vec::new();
        let initial_statements = program.statements.clone();
        println!("[CodeGen] Starting statement collection...");
        self.collect_statements_recursive(initial_statements, &mut all_statements, ".")?;
        println!("[CodeGen] Finished statement collection. Total statements: {}", all_statements.len());

        println!("[CodeGen] Processing struct declarations...");
        for stmt in &all_statements {
            if let Statement::StructDecl(struct_def) = stmt {
                self.declare_struct(struct_def)?;
            }
        }
        println!("[CodeGen] Finished struct declarations.");

        println!("[CodeGen] Processing imports...");
        self.process_import_statements(&all_statements)?;

        
        use std::string::String;
        let mut sigs: HashMap<String, Vec<BasicTypeEnum<'ctx>>> = HashMap::new();
        for _stmt in &all_statements {
            fn scan_expr<'a, 'ctx>(
                name_map: &mut HashMap<String,Vec<BasicTypeEnum<'ctx>>>,
                ctx: &'ctx Context,
                stmt_list: &[x_ast::Statement],
            ) {
                for st in stmt_list {
                    if let x_ast::Statement::Expression{expr} = st {
                        collect_calls(expr, name_map, ctx);
                    }
                    if let x_ast::Statement::Function{body,..} = st {
                        scan_expr(name_map, ctx, body);
                    }
                }
            }
            fn collect_calls<'a, 'ctx>(
                e: &x_ast::Expr,
                name_map: &mut HashMap<String,Vec<BasicTypeEnum<'ctx>>>,
                ctx: &'ctx Context,
            ) {
                if let x_ast::Expr::FunctionCall{name, args} = e {
                    if !name_map.contains_key(name) {
                        let mut tys = vec![];
                        for arg in args {
                            match arg {
                                Expr::String(_) => tys.push(ctx.ptr_type(AddressSpace::default()).into()),
                                Expr::Number(_) => tys.push(ctx.f64_type().into()),
                                Expr::Array(_) => {
                                    tys.push(ctx.ptr_type(AddressSpace::default()).into());
                                }
                                Expr::Identifier(_id_name) => {
                                     tys.push(ctx.f64_type().into());
                                }
                                _ => tys.push(ctx.f64_type().into()),
                            }
                        }
                        name_map.insert(name.clone(), tys);
                    }
                    for sub in args { collect_calls(sub, name_map, ctx) }
                }
                match e {
                    Expr::BinaryOp { left, right, .. } => {
                        collect_calls(left, name_map, ctx);
                        collect_calls(right, name_map, ctx);
                    }
                    Expr::UnaryOp { expr, .. } => {
                        collect_calls(expr, name_map, ctx);
                    }
                    Expr::ArrayAccess { array, index } => {
                         collect_calls(array, name_map, ctx);
                         collect_calls(index, name_map, ctx);
                    }
                    _ => {}
                }
            }
            scan_expr(&mut sigs, &self.context, &all_statements);
        }

        println!("[CodeGen] Starting function declaration pass...");
        for stmt in &all_statements {
            match stmt {
                Statement::Function { name, params, body } => {
                    if self.module.get_function(name).is_none() {
                        let ptypes = if let Some(pt) = sigs.get(name) {
                            println!("[CodeGen] Using inferred signature for {}: {:?}", name, pt);
                            pt.clone()
                        } else {
                            println!("[CodeGen] Warning: No call found for '{}', defaulting signature to all f64.", name);
                            vec![self.context.f64_type().into(); params.len()]
                        };
                        let metadata_types: Vec<BasicMetadataTypeEnum<'ctx>> =
                            ptypes.iter().map(|&ty| ty.into()).collect();

                        let return_type: Option<BasicTypeEnum<'ctx>> = match body.last() {
                            Some(Statement::Expression { .. }) => {
                                println!("[CodeGen] Function '{}' inferred return type: f64 (from last expr)", name);
                                Some(self.context.f64_type().as_basic_type_enum())
                            }
                            Some(Statement::Return { value: Some(_) }) => {
                                 println!("[CodeGen] Function '{}' inferred return type: f64 (from return stmt)", name);
                                 Some(self.context.f64_type().as_basic_type_enum())
                            }
                            _ => {
                                 println!("[CodeGen] Function '{}' inferred return type: void", name);
                                 None
                            }
                        };

                        let fn_ty = match return_type {
                            Some(rt) => rt.fn_type(&metadata_types[..], false),
                            None => self.context.void_type().fn_type(&metadata_types[..], false),
                        };

                        let func = self.module.add_function(name, fn_ty, None);
                        self.functions.insert(name.clone(), func);

                        let ret_type_str = return_type.map_or("void".to_string(), |rt| format!("{:?}", rt));
                        println!("[CodeGen] declared internal {}({:?}) -> {}", name, ptypes, ret_type_str);
                    }
                }
                Statement::ExternFunctionDecl { name, params, return_type: ast_return_type } => {
                    if self.module.get_function(name).is_none() && !self.external_functions.contains_key(name) {
                        let mut param_llvm_types = Vec::new();
                        let mut param_type_names = Vec::new();
                        for ExternParam { name: _, type_name } in params {
                            let llvm_type = self.map_type(type_name)?;
                            param_llvm_types.push(llvm_type.into());
                            param_type_names.push(type_name.clone());
                        }

                        let return_llvm_type: Option<BasicTypeEnum<'ctx>> = match ast_return_type {
                            Some(type_name) => Some(self.map_type(type_name)?),
                            None => None,
                        };

                        let fn_ty = match return_llvm_type {
                            Some(rt) => rt.fn_type(&param_llvm_types[..], false),
                            None => self.context.void_type().fn_type(&param_llvm_types[..], false),
                        };

                        let func = self.module.add_function(name, fn_ty, None);
                        self.external_functions.insert(name.clone(), func);

                        let ret_type_str = return_llvm_type.map_or("void".to_string(), |rt| format!("{:?}", rt));
                        println!("[CodeGen] declared external {}({:?}) -> {}", name, param_type_names, ret_type_str);
                    }
                }
                _ => {}
            }
        }
        println!("[CodeGen] Finished function declaration pass.");

        println!("[CodeGen] Starting function compilation pass...");
        for stmt in &all_statements {
            if let Statement::Function { name, params, body } = stmt {
                 println!("[CodeGen] Compiling function body: {}", name);
                 match self.compile_function(name, params, body) {
                     Ok(func) => self.register_function(name.clone(), func),
                     Err(e) => return Err(format!("Failed to compile function {}: {}", name, e)),
                 }
            }
        }
        println!("[CodeGen] Finished function compilation pass.");

        println!("[CodeGen] Generating main wrapper/entry point...");
        let f64_type = self.context.f64_type();
        let fn_type = f64_type.fn_type(&[], false);
        let main_fn = if let Some(existing_main) = self.module.get_function("main") {
             println!("[CodeGen] Using existing 'main' function.");
             existing_main
        } else {
             println!("[CodeGen] Creating 'main' function wrapper.");
             self.module.add_function("main", fn_type, None)
        };

        if main_fn.count_basic_blocks() == 0 {
            println!("[CodeGen] Populating 'main' function body.");
            let entry_block = self.context.append_basic_block(main_fn, "entry");
            self.builder.position_at_end(entry_block);
            let mut last_val = None;

            for stmt in program.statements.iter() {
                match stmt {
                    Statement::Function { .. } | Statement::ExternFunctionDecl { .. } | Statement::FileImport { .. } | Statement::Import { .. } | Statement::StructDecl(_) => continue,
                    _ => {
                        if let Some(val) = self.gen_statement(stmt)? {
                            last_val = Some(val);
                        }
                    }
                }
            }

            let ret_val = last_val.unwrap_or_else(|| inkwell::values::BasicValueEnum::FloatValue(f64_type.const_float(0.0)));
            self.builder.build_return(Some(&ret_val)).map_err(|e| e.to_string())?;
        } else {
             println!("[CodeGen] 'main' function already has body, skipping wrapper population.");
        }

        println!("[CodeGen] Finished generating main.");

        Ok(())
    }

    pub fn jit_execute(&self) -> Result<f64, String> {
        unsafe {
            let main = self
                .execution_engine
                .get_function::<unsafe extern "C" fn() -> f64>("main")
                .map_err(|e| format!("Failed to get main function: {}", e))?;

            let result = main.call();
            Ok(result)
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

        builder.build_alloca(self.context.f64_type(), name).unwrap()
    }

    pub fn get_ir(&self) -> String {
        self.module.print_to_string().to_string()
    }
}

pub fn compile(program: Program) -> Result<String, String> {
    let context = Context::create();
    let mut codegen = CodeGen::new(&context, "main");
    codegen.generate(program)?;
    Ok(codegen.get_ir())
}
