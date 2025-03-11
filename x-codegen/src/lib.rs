use std::collections::HashMap;

use inkwell::{
    builder::Builder,
    context::Context,
    execution_engine::ExecutionEngine,
    module::Module,
    values::{FunctionValue, PointerValue},
    OptimizationLevel,
};
use x_ast::{Program, Statement};
use x_std::StdLib;

pub mod expression;
pub mod function;
pub mod import;
pub mod statement;
pub mod for_loop;
pub mod r#if;
pub mod r#while;
pub mod r#struct;

pub struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    execution_engine: ExecutionEngine<'ctx>,
    variables: HashMap<String, PointerValue<'ctx>>,
    stdlib: StdLib<'ctx>,
    functions: HashMap<String, FunctionValue<'ctx>>,
    original_to_generated: HashMap<String, String>,
    generated_to_original: HashMap<String, String>,
    imported_functions: HashMap<String, FunctionValue<'ctx>>,
    current_binding_name: Option<String>,
    struct_types: HashMap<String, (inkwell::types::StructType<'ctx>, Vec<String>)>,
    variable_types: HashMap<String, String>,
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
            variables: std::collections::HashMap::new(),
            stdlib,
            functions: HashMap::new(),
            original_to_generated: HashMap::new(),
            generated_to_original: HashMap::new(),
            imported_functions: std::collections::HashMap::new(),
            current_binding_name: None,
            struct_types: HashMap::new(),
            variable_types: HashMap::new(),
        }
    }

    pub fn generate(&mut self, program: Program) -> Result<(), String> {
        self.process_imports(&program)?;

        for stmt in &program.statements {
            if let Statement::Function { name, params, body } = stmt {
                self.compile_function(name, params, body)?;
            }
        }

        let f64_type = self.context.f64_type();
        let fn_type = f64_type.fn_type(&[], false);
        let main_fn = self.module.add_function("main", fn_type, None);
        let entry_block = self.context.append_basic_block(main_fn, "entry");
        self.builder.position_at_end(entry_block);
        let mut last_val = None;

        for stmt in program.statements.iter() {
            // Skip function definitions since they're already registered.
            if let Statement::Function { .. } = stmt {
                continue;
            }
            if let Some(val) = self.gen_statement(stmt)? {
                last_val = Some(val);
            }
        }

        let ret_val = last_val.unwrap_or_else(|| f64_type.const_float(0.0));
        self.builder.build_return(Some(&ret_val)).unwrap();
        Ok(())
    }

    pub fn get_ir(&self) -> String {
        self.module.print_to_string().to_string()
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
}

pub fn compile(program: Program) -> Result<String, String> {
    let context = Context::create();
    let mut codegen = CodeGen::new(&context, "main");
    codegen.generate(program)?;
    Ok(codegen.get_ir())
}
