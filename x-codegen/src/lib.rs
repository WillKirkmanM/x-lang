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

    fn process_import(&mut self, module: &str, item: &str) -> Result<(), String> {
        match (module, item) {
            ("std", "print") => {
                self.imported_functions.insert(
                    "print".to_string(),
                    self.stdlib.get_print()
                );
                self.imported_functions.insert(
                    "print_str".to_string(),
                    self.stdlib.get_print_str()
                );
                Ok(())
            },
            _ => Err(format!("Unknown import: {}::{}", module, item))
        }
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
            },
            Statement::Import { module, item } => {
                if module == "std" && item == "print" {
                    self.imported_functions.insert(
                        "print".to_string(),
                        self.stdlib.get_print()
                    );
                }
                Ok(None)
            },
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
            // Expr::Add(left, right) => {
            //     let l = self.gen_expr(left)?;
            //     let r = self.gen_expr(right)?;
            //     self.builder.build_float_add(l, r, "addtmp").map_err(|e| e.to_string())
            // }
            // Expr::Multiply(left, right) => {
            //     let l = self.gen_expr(left)?;
            //     let r = self.gen_expr(right)?;
            //     self.builder.build_float_mul(l, r, "multmp").map_err(|e| e.to_string())
            // }
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
            Expr::FunctionCall { name, args } => {
                match (name.as_str(), args.first()) {
                    ("print", Some(Expr::String(_))) => {
                        if let Some(print_str_fn) = self.imported_functions.get("print_str") {
                            let string_ptr = if let Expr::String(s) = &args[0] {
                                self.builder
                                    .build_global_string_ptr(s, "str")
                                    .map_err(|e| e.to_string())?
                                    .as_pointer_value()
                            } else {
                                return Err("Expected string argument".to_string());
                            };

                            let call = self.builder.build_call(
                                *print_str_fn,
                                &[string_ptr.into()],
                                "calltmp"
                            ).map_err(|e| e.to_string())?;

                            Ok(self.context.f64_type().const_float(0.0))
                        } else {
                            Err("print_str function not imported".to_string())
                        }
                    },
                    ("print", _) => {
                        if let Some(print_fn) = self.imported_functions.get("print") {
                            let compiled_args: Vec<_> = args.iter()
                                .map(|arg| self.gen_expr(arg))
                                .collect::<Result<Vec<_>, _>>()?
                                .into_iter()
                                .map(|val| val.into())
                                .collect();

                            Ok(self.builder.build_call(
                                *print_fn,
                                &compiled_args,
                                "calltmp"
                            ).map_err(|e| e.to_string())?
                             .try_as_basic_value()
                             .left()
                             .unwrap()
                             .into_float_value())
                        } else {
                            Err("print function not imported".to_string())
                        }
                    },
                    _ => Err(format!("Unknown function: {}", name))
                }
            },

            Expr::String(_) => {
                Err("String literals can only be used in print function calls".to_string())
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