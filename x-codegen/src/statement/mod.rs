use crate::CodeGen;
use inkwell::values::FloatValue;
use x_ast::{Expr, Statement};

impl<'ctx> CodeGen<'ctx> {
    pub(crate) fn gen_statement(
        &mut self,
        stmt: &Statement,
    ) -> Result<Option<FloatValue<'ctx>>, String> {
        match stmt {
            Statement::Expression { expr } => Ok(Some(self.gen_expr(expr)?.into_float_value())),
            Statement::VariableDecl { name, value } => {
                                self.current_binding_name = Some(name.clone());
                
                                let alloca = self
                                    .builder
                                    .build_alloca(self.context.f64_type(), name)
                                    .map_err(|e| e.to_string())?;
            
                                let val = self.gen_expr(value)?;
                
                                if let Expr::StructInstantiate(struct_init) = value {
                                    self.variable_types.insert(name.clone(), struct_init.name.clone());
                                }
                
                                self.current_binding_name = None;
                
                                self.builder
                                    .build_store(alloca, val)
                                    .map_err(|e| e.to_string())?;
            
                                self.variables.insert(name.clone(), alloca);
            
                                Ok(None)
                            }
            Statement::Import { module, item } => {
                                self.process_import(module, item)?;
                                Ok(None)
                            }
            Statement::Function { name, params, body } => {
                                self.compile_function(name, params, body)?;
                                Ok(None)
                            }
            Statement::Block { statements } => {
                                let mut last_value = None;
                                for statement in statements {
                                    if let Some(val) = self.gen_statement(statement)? {
                                        last_value = Some(val);
                                    }
                                }
                                Ok(last_value)
                            }
            Statement::Comment(_) => Ok(None),
            Statement::ForLoop { var, start, end, body } => {
                                self.gen_for_loop(var, start, end, body)
                            },
            Statement::If { condition, then_block, else_block } => {
                                self.gen_if(condition, then_block, else_block)
                            },
            Statement::WhileLoop { condition, body } => {
                                self.gen_while_loop(condition, body)?;
                                Ok(None)
                            },
            Statement::StructDecl(struct_def) => {
                self.gen_struct_decl(struct_def)?;
                Ok(None)
            },
            Statement::Return { value } => {
                let return_val = if let Some(expr) = value {
                    self.gen_expr(expr)?.into_float_value()
                } else {
                    self.context.f64_type().const_float(0.0)
                };
                
                self.builder
                    .build_return(Some(&return_val))
                    .map_err(|e| e.to_string())?;
                    
                let function = self.builder.get_insert_block().unwrap().get_parent().unwrap();
                let unreachable_block = self.context.append_basic_block(function, "after_return");
                self.builder.position_at_end(unreachable_block);
                
                Ok(None)
            },
        }
    }
}
