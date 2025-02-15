use inkwell::{
    context::Context,
    values::FloatValue,
    builder::Builder, 
    module::Module,
    execution_engine::ExecutionEngine,
    OptimizationLevel,
};
use x_ast::{Expr, Operator, Program, Statement};

pub struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    execution_engine: ExecutionEngine<'ctx>,
    variables: std::collections::HashMap<String, FloatValue<'ctx>>,
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new(context: &'ctx Context, module_name: &str) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();
        let execution_engine = module.create_jit_execution_engine(OptimizationLevel::None)
            .expect("Failed to create execution engine");
            
        CodeGen {
            context,
            module,
            builder,
            execution_engine,
            variables: std::collections::HashMap::new(),
        }
    }

    pub fn generate(&mut self, program: Program) -> Result<(), String> {
        let f64_type = self.context.f64_type();
        let fn_type = f64_type.fn_type(&[], false);
        let function = self.module.add_function("main", fn_type, None);
        let basic_block = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(basic_block);

        let mut result = None;
        for stmt in program.statements {
            result = self.gen_statement(&stmt)?;
        }

        if let Some(ret_val) = result {
            self.builder.build_return(Some(&ret_val)).map_err(|e| e.to_string())?;
        }

        Ok(())
    }

    fn gen_statement(&mut self, stmt: &Statement) -> Result<Option<FloatValue<'ctx>>, String> {
        match stmt {
            Statement::Expression { expr } => {
                Ok(Some(self.gen_expr(expr)?))
            },
            Statement::VariableDecl { name, value } => {
                let val = self.gen_expr(value)?;
                self.variables.insert(name.clone(), val);
                Ok(None)
            }
        }
    }

    fn gen_expr(&self, expr: &Expr) -> Result<FloatValue<'ctx>, String> {
        match expr {
            Expr::Number(n) => {
                Ok(self.context.f64_type().const_float(*n as f64))
            }
            Expr::Identifier(name) => {
                self.variables.get(name)
                    .cloned()
                    .ok_or_else(|| format!("Undefined variable: {}", name))
            }
            Expr::Add(left, right) => {
                let l = self.gen_expr(left)?;
                let r = self.gen_expr(right)?;
                self.builder.build_float_add(l, r, "addtmp").map_err(|e| e.to_string())
            }
            Expr::Multiply(left, right) => {
                let l = self.gen_expr(left)?;
                let r = self.gen_expr(right)?;
                self.builder.build_float_mul(l, r, "multmp").map_err(|e| e.to_string())
            }
            Expr::BinaryOp { left, op, right } => {
                let l = self.gen_expr(left)?;
                let r = self.gen_expr(right)?;
                
                match op {
                    Operator::Add => self.builder.build_float_add(l, r, "addtmp").map_err(|e| e.to_string()),
                    Operator::Subtract => self.builder.build_float_sub(l, r, "subtmp").map_err(|e| e.to_string()),
                    Operator::Multiply => self.builder.build_float_mul(l, r, "multmp").map_err(|e| e.to_string()),
                    Operator::Divide => self.builder.build_float_div(l, r, "divtmp").map_err(|e| e.to_string()),
                }
            },
        }
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