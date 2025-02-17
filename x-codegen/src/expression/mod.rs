use crate::CodeGen;
use inkwell::{values::{FloatValue, PointerValue}, FloatPredicate};
use x_ast::{Expr, Operator};

impl<'ctx> CodeGen<'ctx> {
    pub(crate) fn gen_expr(&self, expr: &Expr) -> Result<FloatValue<'ctx>, String> {
        match expr {
            Expr::Number(n) => Ok(self.context.f64_type().const_float(*n as f64)),
            Expr::Identifier(name) => {
                if let Some(ptr) = self.variables.get(name) {
                    Ok(self.builder
                        .build_load(self.context.f64_type(), *ptr, name)
                        .map_err(|e| e.to_string())?
                        .into_float_value())
                } else {
                    Err(format!("Undefined variable: {}", name))
                }
            },
            Expr::BinaryOp { left, op, right } => {
                let lhs = self.gen_expr(left)?;
                let rhs = self.gen_expr(right)?;
                
                match op {
                    Operator::Add => Ok(self.builder
                        .build_float_add(lhs, rhs, "addtmp")
                        .map_err(|e| e.to_string())?),
                    Operator::Subtract => Ok(self.builder.build_float_sub(lhs, rhs, "subtmp") 
                        .map_err(|e| e.to_string())?),
                    Operator::Multiply => Ok(self.builder
                        .build_float_mul(lhs, rhs, "multmp")
                        .map_err(|e| e.to_string())?),
                    Operator::Divide => Ok(self.builder.build_float_div(lhs, rhs, "divtmp")
                        .map_err(|e| e.to_string())?),
                    Operator::LessThan => self.gen_comparison(FloatPredicate::OLT, lhs, rhs),
                    Operator::GreaterThan => self.gen_comparison(FloatPredicate::OGT, lhs, rhs),
                    Operator::LessThanOrEqual => self.gen_comparison(FloatPredicate::OLE, lhs, rhs),
                    Operator::GreaterThanOrEqual => self.gen_comparison(FloatPredicate::OGE, lhs, rhs),
                    Operator::Equal => self.gen_comparison(FloatPredicate::OEQ, lhs, rhs),
                    Operator::NotEqual => self.gen_comparison(FloatPredicate::ONE, lhs, rhs),
                }
            },
            Expr::FunctionCall { name, args } => self.gen_function_call(name, args),
            Expr::String(_) => {
                Err("String literals can only be used in print function calls".to_string())
            },
        }
    }

    fn gen_comparison(
        &self,
        predicate: FloatPredicate,
        lhs: FloatValue<'ctx>,
        rhs: FloatValue<'ctx>
    ) -> Result<FloatValue<'ctx>, String> {
        let cmp = self.builder
            .build_float_compare(predicate, lhs, rhs, "cmptmp")
            .map_err(|e| e.to_string())?;
        
        Ok(self.builder
            .build_unsigned_int_to_float(
                cmp,
                self.context.f64_type(),
                "booltmp"
            )
            .map_err(|e| e.to_string())?)
    }

    pub fn gen_print_str(&self, args: &[Expr]) -> Result<FloatValue<'ctx>, String> {
        if let Some(Expr::String(s)) = args.first() {
            let print_str = self.module.get_function("print_str")
                .ok_or_else(|| "print_str function not found".to_string())?;
    
            let str_ptr = self.builder.build_global_string_ptr(s, "str")
                .map_err(|e| e.to_string())?;
    
            self.builder
                .build_call(print_str, &[str_ptr.as_pointer_value().into()], "print_call")
                .map_err(|e| e.to_string())?;
    
            Ok(self.context.f64_type().const_float(0.0))
        } else {
            Err("Expected string argument for print".to_string())
        }
    }

    pub fn gen_print(&self, args: &[Expr]) -> Result<FloatValue<'ctx>, String> {
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
    }
}