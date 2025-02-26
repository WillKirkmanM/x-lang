use inkwell::values::FloatValue;
use inkwell::FloatPredicate;
use x_ast::{Expr, Statement};

use crate::CodeGen;

impl<'ctx> CodeGen<'ctx> {
    pub fn gen_while_loop(
        &mut self,
        condition: &Expr,
        body: &[Statement],
    ) -> Result<Option<FloatValue<'ctx>>, String> {
        let parent = self.builder.get_insert_block().unwrap();
        let function = parent.get_parent().unwrap();
        
        let cond_bb = self.context.append_basic_block(function, "while.cond");
        let body_bb = self.context.append_basic_block(function, "while.body");
        let end_bb = self.context.append_basic_block(function, "while.end");
        
        self.builder.build_unconditional_branch(cond_bb)
            .map_err(|e| e.to_string())?;
        
        self.builder.position_at_end(cond_bb);
        let cond_val = self.gen_expr(condition)?.into_float_value();
        let zero = self.context.f64_type().const_float(0.0);
        let comparison = self.builder
            .build_float_compare(
                FloatPredicate::ONE,
                cond_val,
                zero,
                "while.cmp"
            )
            .map_err(|e| e.to_string())?;
        
        self.builder
            .build_conditional_branch(comparison, body_bb, end_bb)
            .map_err(|e| e.to_string())?;
        
        self.builder.position_at_end(body_bb);
        for stmt in body {
            self.gen_statement(stmt)?;
        }
        
        self.builder
            .build_unconditional_branch(cond_bb)
            .map_err(|e| e.to_string())?;
        
        self.builder.position_at_end(end_bb);
        
        Ok(None)
    }
}