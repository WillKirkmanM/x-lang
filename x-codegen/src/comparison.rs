use inkwell::{values::FloatValue, FloatPredicate};

use crate::CodeGen;

impl<'ctx> CodeGen<'ctx> {
    pub fn gen_comparison(
        &self,
        predicate: FloatPredicate,
        lhs: FloatValue<'ctx>,
        rhs: FloatValue<'ctx>,
    ) -> Result<FloatValue<'ctx>, String> {
        let cmp = self
            .builder
            .build_float_compare(predicate, lhs, rhs, "cmptmp")
            .map_err(|e| e.to_string())?;

        Ok(self
            .builder
            .build_unsigned_int_to_float(cmp, self.context.f64_type(), "booltmp")
            .map_err(|e| e.to_string())?)
    }
}
