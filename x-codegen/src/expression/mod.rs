use crate::CodeGen;
use inkwell::{module::Linkage, values::{FloatValue, FunctionValue, BasicMetadataValueEnum, BasicValueEnum}, AddressSpace, FloatPredicate};
use x_ast::{Expr, Operator, StringLiteral, StringPart};

impl<'ctx> CodeGen<'ctx> {
    pub(crate) fn gen_expr(&self, expr: &Expr) -> Result<BasicValueEnum<'ctx>, String> {
        match expr {
            Expr::Number(n) => Ok(self.context.f64_type().const_float(*n as f64).into()),
            Expr::Identifier(name) => {
                if let Some(ptr) = self.variables.get(name) {
                    Ok(self.builder
                        .build_load(self.context.f64_type(), *ptr, name)
                        .map_err(|e| e.to_string())?
                        .into())
                } else {
                    Err(format!("Undefined variable: {}", name))
                }
            },
            Expr::BinaryOp { left, op, right } => {
                let lhs = self.gen_expr(left)?.into_float_value();
                let rhs = self.gen_expr(right)?.into_float_value();
                
                match op {
                    Operator::Add => Ok(self.builder
                        .build_float_add(lhs, rhs, "addtmp")
                        .map_err(|e| e.to_string())?
                        .into()),
                    Operator::Subtract => Ok(self.builder.build_float_sub(lhs, rhs, "subtmp") 
                        .map_err(|e| e.to_string())?
                        .into()),
                    Operator::Multiply => Ok(self.builder
                        .build_float_mul(lhs, rhs, "multmp")
                        .map_err(|e| e.to_string())?
                        .into()),
                    Operator::Divide => Ok(self.builder.build_float_div(lhs, rhs, "divtmp")
                        .map_err(|e| e.to_string())?
                        .into()),
                    Operator::LessThan => self.gen_comparison(FloatPredicate::OLT, lhs, rhs).map(|v| v.into()),
                    Operator::GreaterThan => self.gen_comparison(FloatPredicate::OGT, lhs, rhs).map(|v| v.into()),
                    Operator::LessThanOrEqual => self.gen_comparison(FloatPredicate::OLE, lhs, rhs).map(|v| v.into()),
                    Operator::GreaterThanOrEqual => self.gen_comparison(FloatPredicate::OGE, lhs, rhs).map(|v| v.into()),
                    Operator::Equal => self.gen_comparison(FloatPredicate::OEQ, lhs, rhs).map(|v| v.into()),
                    Operator::NotEqual => self.gen_comparison(FloatPredicate::ONE, lhs, rhs).map(|v| v.into()),
                }
            },
            Expr::FunctionCall { name, args } => {
                match name.as_str() {
                    "print" => {
                        if let Some(Expr::String(string_literal)) = args.first() {
                            self.gen_print_str(string_literal).map(|v| v.into())
                        } else {
                            self.gen_print(args).map(|v| v.into())
                        }
                    },
                    _ => self.gen_function_call(name, args).map(|v| v.into()),
                }
            },
            Expr::String(str_lit) => {
                let s = str_lit.parts.iter().map(|p| p.to_string()).collect::<String>();
                let global_str = self.builder
                    .build_global_string_ptr(&s, "str_const")
                    .map_err(|e| e.to_string())?;
                Ok(global_str.as_pointer_value().into())
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

    pub fn get_printf_fn(&self) -> Result<FunctionValue<'ctx>, String> {
        if let Some(func) = self.module.get_function("printf") {
            Ok(func)
        } else {
            let printf_type = self.context.i32_type().fn_type(
                &[self.context.ptr_type(AddressSpace::default()).into()],
                true
            );
            
            let printf = self.module.add_function("printf", printf_type, Some(Linkage::External));
            Ok(printf)
        }
    }

    pub fn gen_print_str(&self, string: &StringLiteral) -> Result<FloatValue<'ctx>, String> {
        let printf = self.get_printf_fn()?;
        let mut fmt_str = String::new();
        let mut args: Vec<BasicValueEnum<'ctx>> = Vec::new();
        
        for part in &string.parts {
            match part {
                StringPart::Text(text) => {
                    let escaped = text.replace("%", "%%");
                    fmt_str.push_str(&escaped);
                },
                StringPart::Interpolation(expr) => match expr.as_ref() {
                    Expr::String(str_lit) => {
                        fmt_str.push_str("%s");
                        let str_val = self.builder
                            .build_global_string_ptr(&str_lit.parts[0].to_string(), "str_const")
                            .map_err(|e| e.to_string())?;
                        args.push(str_val.as_pointer_value().into());
                    },
                    Expr::Identifier(name) => {
                        if let Some(ptr) = self.variables.get(name) {
                            if name == "name" {
                                fmt_str.push_str("%s");
                                let val = self.builder
                                    .build_load(
                                        self.context.ptr_type(AddressSpace::default()),
                                        *ptr,
                                        name
                                    )
                                    .map_err(|e| e.to_string())?;
                                args.push(val);
                            } else {
                                fmt_str.push_str("%f");
                                let val = self.builder
                                    .build_load(self.context.f64_type(), *ptr, name)
                                    .map_err(|e| e.to_string())?;
                                args.push(val);
                            }
                        } else {
                            return Err(format!("Undefined variable in interpolation: {}", name));
                        }
                    },
                    _ => {
                        fmt_str.push_str("%f");
                        let val = self.gen_expr(expr)?;
                        args.push(val);
                    }
                }
            }
        }
        
        fmt_str.push_str("\n\0");
        
        let fmt_ptr = self.builder
            .build_global_string_ptr(&fmt_str, "fmt_str")
            .map_err(|e| e.to_string())?;
        
        let mut printf_args: Vec<BasicMetadataValueEnum<'ctx>> =
            vec![fmt_ptr.as_pointer_value().into()];
        printf_args.extend(args.iter().map(|&arg| -> BasicMetadataValueEnum<'ctx> {
            arg.into()
        }));
        
        self.builder
            .build_call(printf, &printf_args, "printf_call")
            .map_err(|e| e.to_string())?;
        
        Ok(self.context.f64_type().const_float(0.0))
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