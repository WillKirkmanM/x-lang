use inkwell::values::FloatValue;
use inkwell::FloatPredicate;
use x_ast::{Expr, Statement};

use crate::CodeGen;

impl<'ctx> CodeGen<'ctx> {
    pub fn gen_if(
        &mut self,
        condition: &Expr,
        then_block: &[Statement],
        else_block: &Option<Vec<Statement>>,
    ) -> Result<Option<FloatValue<'ctx>>, String> {
        let cond_val = self.gen_expr(condition)?.into_float_value();
        
        let parent = self.builder.get_insert_block().unwrap();
        let function = parent.get_parent().unwrap();
        let then_bb = self.context.append_basic_block(function, "then");
        let else_bb = else_block.as_ref().map(|_| 
            self.context.append_basic_block(function, "else"));
        let merge_bb = self.context.append_basic_block(function, "ifcont");
        
        let zero = self.context.f64_type().const_float(0.0);
        let condition = self.builder
            .build_float_compare(
                FloatPredicate::ONE,
                cond_val,
                zero,
                "ifcond"
            )
            .map_err(|e| e.to_string())?;
        
        self.builder
            .build_conditional_branch(
                condition,
                then_bb,
                else_bb.unwrap_or(merge_bb)
            )
            .map_err(|e| e.to_string())?;
        
        self.builder.position_at_end(then_bb);
        let mut then_val = None;
        for stmt in then_block {
            if let Some(val) = self.gen_statement(stmt)? {
                then_val = Some(val);
            }
        }
        self.builder.build_unconditional_branch(merge_bb)
            .map_err(|e| e.to_string())?;
        
        let mut else_val = None;
        if let Some(else_stmts) = else_block {
            self.builder.position_at_end(else_bb.unwrap());
            for stmt in else_stmts {
                if let Some(val) = self.gen_statement(stmt)? {
                    else_val = Some(val);
                }
            }
            self.builder.build_unconditional_branch(merge_bb)
                .map_err(|e| e.to_string())?;
        }
        
        self.builder.position_at_end(merge_bb);
        
        Ok(then_val.or(else_val))
    }
}