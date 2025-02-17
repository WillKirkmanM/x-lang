use inkwell::values::FloatValue;
use inkwell::FloatPredicate;
use x_ast::{Expr, Statement};

use crate::CodeGen;

impl<'ctx> CodeGen<'ctx> {
    pub fn gen_for_loop(
        &mut self,
        var: &str,
        start: &Expr,
        end: &Expr,
        body: &[Statement],
    ) -> Result<Option<FloatValue<'ctx>>, String> {
        let f64_type = self.context.f64_type();
        
        let var_alloca = self.builder
            .build_alloca(f64_type, var)
            .map_err(|e| e.to_string())?;
        
        let start_val = self.gen_expr(start)?;
        self.builder
            .build_store(var_alloca, start_val)
            .map_err(|e| e.to_string())?;
        
        let parent = self.builder.get_insert_block().unwrap();
        let function = parent.get_parent().unwrap();
        let loop_bb = self.context.append_basic_block(function, "loop");
        let body_bb = self.context.append_basic_block(function, "body");
        let end_bb = self.context.append_basic_block(function, "endloop");
        
        self.builder.build_unconditional_branch(loop_bb)
            .map_err(|e| e.to_string())?;
        
        self.builder.position_at_end(loop_bb);
        let current = self.builder
            .build_load(f64_type, var_alloca, "current")
            .map_err(|e| e.to_string())?
            .into_float_value();
        
        let end_val = self.gen_expr(end)?.into_float_value();
        let condition = self.builder
            .build_float_compare(
                FloatPredicate::OLT,
                current,
                end_val,
                "condition"
            )
            .map_err(|e| e.to_string())?;
        
        self.builder
            .build_conditional_branch(condition, body_bb, end_bb)
            .map_err(|e| e.to_string())?;
        
        self.builder.position_at_end(body_bb);
        self.variables.insert(var.to_string(), var_alloca);
        
        for stmt in body {
            self.gen_statement(stmt)?;
        }
        
        let next = self.builder
            .build_float_add(
                current,
                f64_type.const_float(1.0),
                "next"
            )
            .map_err(|e| e.to_string())?;
        
        self.builder
            .build_store(var_alloca, next)
            .map_err(|e| e.to_string())?;
        
        self.builder
            .build_unconditional_branch(loop_bb)
            .map_err(|e| e.to_string())?;
        
        self.builder.position_at_end(end_bb);
        
        Ok(None)
    }
}