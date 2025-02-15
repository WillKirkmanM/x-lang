use inkwell::{
    context::Context,
    values::{FloatValue, FunctionValue},
    builder::Builder, 
    module::Module,
    execution_engine::ExecutionEngine,
    OptimizationLevel,
};
use x_ast::{Expr, Operator, Program, Statement};
use x_std::StdLib;

pub mod statement;
pub mod expression;
pub mod import;

pub struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    execution_engine: ExecutionEngine<'ctx>,
    variables: std::collections::HashMap<String, FloatValue<'ctx>>,
    stdlib: StdLib<'ctx>,
    imported_functions: std::collections::HashMap<String, FunctionValue<'ctx>>,
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new(context: &'ctx Context, module_name: &str) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();
        
        let stdlib = StdLib::new(context);
        stdlib.link_to_module(&module);
        
        let execution_engine = module.create_jit_execution_engine(OptimizationLevel::None)
            .expect("Failed to create execution engine");
            
        CodeGen {
            context,
            module,
            builder,
            execution_engine,
            variables: std::collections::HashMap::new(),
            stdlib,
            imported_functions: std::collections::HashMap::new(),
        }
    }

    pub fn generate(&mut self, program: Program) -> Result<(), String> {
        for stmt in &program.statements {
            if let Statement::Import { module, item } = stmt {
                self.process_import(module, item)?;
            }
        }

        let f64_type = self.context.f64_type();
        let fn_type = f64_type.fn_type(&[], false);
        let function = self.module.add_function("main", fn_type, None);
        let basic_block = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(basic_block);

        let mut last_value = None;
        
        for stmt in program.statements {
            match stmt {
                Statement::Import { .. } => continue,
                _ => last_value = self.gen_statement(&stmt)?,
            }
        }

        let return_value = last_value.unwrap_or_else(|| {
            self.context.f64_type().const_float(0.0)
        });
        self.builder.build_return(Some(&return_value))
            .map_err(|e| e.to_string())?;

        Ok(())
    }

    pub fn get_ir(&self) -> String {
        self.module.print_to_string().to_string()
    }

    pub fn jit_execute(&self) -> Result<f64, String> {
        unsafe {
            let main = self.execution_engine
                .get_function::<unsafe extern "C" fn() -> f64>("main")
                .map_err(|e| format!("Failed to get main function: {}", e))?;

            let result = main.call();
            Ok(result)
        }
    }
}

pub fn compile(program: Program) -> Result<String, String> {
    let context = Context::create();
    let mut codegen = CodeGen::new(&context, "main");
    codegen.generate(program)?;
    Ok(codegen.get_ir())
}