use crate::ast::{BinaryOp, Expr};
use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    execution_engine::{ExecutionEngine, JitFunction},
    module::Module,
    values::{FunctionValue, IntValue, PointerValue},
    AddressSpace, IntPredicate, OptimizationLevel,
};
use std::collections::HashMap;
use std::error::Error;
pub struct ScopeStack<'ctx> {
    scopes: Vec<HashMap<String, PointerValue<'ctx>>>,
}
impl<'ctx> ScopeStack<'ctx> {
    pub fn new() -> Self {
        let mut scopes = Vec::new();
        scopes.push(HashMap::new()); // Global scope
        Self { scopes }
    }
    pub fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }
    pub fn pop_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }
    pub fn get(&self, name: &str) -> Option<&PointerValue<'ctx>> {
        for scope in self.scopes.iter().rev() {
            if let Some(val) = scope.get(name) {
                return Some(val);
            }
        }
        None
    }
    pub fn insert(&mut self, name: String, value: PointerValue<'ctx>) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, value);
        }
    }
    pub fn get_or_create(
        &mut self,
        name: &str,
        builder: &Builder<'ctx>,
        i32_type: inkwell::types::IntType<'ctx>,
    ) -> PointerValue<'ctx> {
        if let Some(ptr) = self.get(name) {
            *ptr
        } else {
            let alloca = builder.build_alloca(i32_type, name).unwrap();
            if self.scopes.is_empty() {
                self.push_scope();
            }
            self.scopes
                .last_mut()
                .unwrap()
                .insert(name.to_string(), alloca);
            alloca
        }
    }
    pub fn update(&mut self, name: &str, value: PointerValue<'ctx>) -> bool {
        for scope in self.scopes.iter_mut().rev() {
            if scope.contains_key(name) {
                scope.insert(name.to_string(), value);
                return true;
            }
        }
        false
    }
    pub fn get_mut(&mut self, name: &str) -> Option<&mut PointerValue<'ctx>> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(val) = scope.get_mut(name) {
                return Some(val);
            }
        }
        None
    }
}
pub struct CodeGen<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub execution_engine: ExecutionEngine<'ctx>,
    pub variables: ScopeStack<'ctx>,
}
impl<'ctx> CodeGen<'ctx> {
    pub fn new(context: &'ctx Context) -> Result<Self, Box<dyn Error>> {
        let module = context.create_module("hello");
        let execution_engine = module.create_jit_execution_engine(OptimizationLevel::None)?;
        Ok(CodeGen {
            context,
            module,
            builder: context.create_builder(),
            execution_engine,
            variables: ScopeStack::new(),
        })
    }
    fn setup_printf(&self) -> inkwell::values::FunctionValue<'ctx> {
        let i32_type = self.context.i32_type();
        let ptr_type = self.context.ptr_type(AddressSpace::from(0));
        let printf_type = i32_type.fn_type(&[ptr_type.into()], true);
        self.module.add_function("printf", printf_type, None)
    }
    fn generate_for_loop(
        &mut self,
        iter: &str,
        start: &Expr,
        end: &Expr,
        body: &[Expr],
        function: FunctionValue<'ctx>,
        printf: FunctionValue<'ctx>,
        debug: bool, // Add debug parameter
    ) {
        let i32_type = self.context.i32_type();
        let iter_val = self.builder.build_alloca(i32_type, iter).unwrap();
        self.variables.insert(iter.to_string(), iter_val);
        let start_val = match start {
            Expr::Int(i) => i32_type.const_int(*i as u64, false),
            _ => i32_type.const_int(0, false),
        };
        self.builder.build_store(iter_val, start_val).unwrap();
        let loop_bb = self.context.append_basic_block(function, "loop");
        let loop_body_bb = self.context.append_basic_block(function, "loop_body");
        let after_bb = self.context.append_basic_block(function, "after_loop");
        self.builder.build_unconditional_branch(loop_bb).unwrap();
        self.builder.position_at_end(loop_bb);
        let current = self
            .builder
            .build_load(i32_type, iter_val, "current")
            .unwrap();
        let end_val = match end {
            Expr::Int(i) => i32_type.const_int(*i as u64, false),
            _ => i32_type.const_int(0, false),
        };
        let compare = self
            .builder
            .build_int_compare(
                IntPredicate::SLT,
                current.into_int_value(),
                end_val,
                "compare",
            )
            .unwrap();
        self.builder
            .build_conditional_branch(compare, loop_body_bb, after_bb)
            .unwrap();
        self.builder.position_at_end(loop_body_bb);
        self.variables.push_scope();
        for expr in body {
            self.generate_expr(expr, function, printf, debug, false);
        }
        self.variables.pop_scope();
        let next = self
            .builder
            .build_int_add(
                current.into_int_value(),
                i32_type.const_int(1, false),
                "next",
            )
            .unwrap();
        self.builder.build_store(iter_val, next).unwrap();
        self.builder.build_unconditional_branch(loop_bb).unwrap();
        self.builder.position_at_end(after_bb);
        self.variables.pop_scope();
    }
    fn generate_expr(
        &mut self,
        expr: &Expr,
        function: FunctionValue<'ctx>,
        printf: FunctionValue<'ctx>,
        debug: bool,
        in_print: bool, // Add this parameter
    ) -> Option<IntValue<'ctx>> {
        match expr {
            Expr::Print(inner) => {
                match &**inner {
                    Expr::String(s) => {
                        let fmt = self
                            .builder
                            .build_global_string_ptr(&format!("{}\n", s), "str_fmt")
                            .unwrap();
                        self.builder
                            .build_call(printf, &[fmt.as_pointer_value().into()], "printf")
                            .unwrap();
                    }
                    Expr::Var(name) => {
                        if let Some(&ptr) = self.variables.get(name) {
                            let val = self
                                .builder
                                .build_load(self.context.i32_type(), ptr, "load")
                                .unwrap();
                            let fmt = self
                                .builder
                                .build_global_string_ptr("%d\n", "int_fmt")
                                .unwrap();
                            self.builder
                                .build_call(
                                    printf,
                                    &[fmt.as_pointer_value().into(), val.into()],
                                    "printf",
                                )
                                .unwrap();
                        }
                    }
                    _ => {
                        if let Some(val) = self.generate_expr(inner, function, printf, debug, true)
                        {
                            let fmt = self
                                .builder
                                .build_global_string_ptr("%d\n", "int_fmt")
                                .unwrap();
                            self.builder
                                .build_call(
                                    printf,
                                    &[fmt.as_pointer_value().into(), val.into()],
                                    "printf",
                                )
                                .unwrap();
                        }
                    }
                }
                None // Important: Return None to prevent duplicate prints
            }
            Expr::Binary(op, left, right) => match op {
                BinaryOp::And => {
                    let lhs = self.generate_expr(left, function, printf, debug, in_print)?;
                    let right_bb = self.context.append_basic_block(function, "and_right");
                    let merge_bb = self.context.append_basic_block(function, "merge");
                    let left_bool = self
                        .builder
                        .build_int_truncate_or_bit_cast(lhs, self.context.bool_type(), "to_bool")
                        .unwrap();
                    self.builder
                        .build_conditional_branch(left_bool, right_bb, merge_bb)
                        .unwrap();
                    self.builder.position_at_end(right_bb);
                    let rhs = self.generate_expr(right, function, printf, debug, in_print)?;
                    let right_bool = self
                        .builder
                        .build_int_truncate_or_bit_cast(rhs, self.context.bool_type(), "to_bool")
                        .unwrap();
                    self.builder.build_unconditional_branch(merge_bb).unwrap();
                    self.builder.position_at_end(merge_bb);
                    let phi = self
                        .builder
                        .build_phi(self.context.bool_type(), "and_result")
                        .unwrap();
                    phi.add_incoming(&[
                        (&self.context.bool_type().const_int(0, false), merge_bb),
                        (&right_bool, right_bb),
                    ]);
                    Some(
                        self.builder
                            .build_int_z_extend(
                                phi.as_basic_value().into_int_value(),
                                self.context.i32_type(),
                                "to_i32",
                            )
                            .unwrap(),
                    )
                }
                BinaryOp::Mod => {
                    let lhs = self.generate_expr(left, function, printf, debug, in_print)?;
                    let rhs = self.generate_expr(right, function, printf, debug, in_print)?;
                    let zero = self.context.i32_type().const_int(0, false);
                    let is_zero = self
                        .builder
                        .build_int_compare(IntPredicate::EQ, rhs, zero, "is_zero")
                        .unwrap();
                    let safe_bb = self.context.append_basic_block(function, "safe_mod");
                    let error_bb = self.context.append_basic_block(function, "mod_error");
                    let merge_bb = self.context.append_basic_block(function, "merge");
                    self.builder
                        .build_conditional_branch(is_zero, error_bb, safe_bb)
                        .unwrap();
                    self.builder.position_at_end(safe_bb);
                    let mod_result = self.builder.build_int_signed_rem(lhs, rhs, "mod").unwrap();
                    self.builder.build_unconditional_branch(merge_bb).unwrap();
                    self.builder.position_at_end(error_bb);
                    let fmt = self
                        .builder
                        .build_global_string_ptr("Error: Division by zero\n", "error_fmt")
                        .unwrap();
                    self.builder
                        .build_call(printf, &[fmt.as_pointer_value().into()], "printf")
                        .unwrap();
                    self.builder.build_unconditional_branch(merge_bb).unwrap();
                    self.builder.position_at_end(merge_bb);
                    Some(mod_result)
                }
                BinaryOp::Eq => {
                    let lhs = self.generate_expr(left, function, printf, debug, in_print)?;
                    let rhs = self.generate_expr(right, function, printf, debug, in_print)?;
                    Some(
                        self.builder
                            .build_int_compare(IntPredicate::EQ, lhs, rhs, "eq")
                            .unwrap(),
                    )
                }
                BinaryOp::Add => {
                    let lhs = self.generate_expr(left, function, printf, debug, in_print)?;
                    let rhs = self.generate_expr(right, function, printf, debug, in_print)?;
                    Some(self.builder.build_int_add(lhs, rhs, "add").unwrap())
                }
                BinaryOp::Sub => {
                    let lhs = self.generate_expr(left, function, printf, debug, in_print)?;
                    let rhs = self.generate_expr(right, function, printf, debug, in_print)?;
                    Some(self.builder.build_int_sub(lhs, rhs, "sub").unwrap())
                }
                BinaryOp::Mul => {
                    let lhs = self.generate_expr(left, function, printf, debug, in_print)?;
                    let rhs = self.generate_expr(right, function, printf, debug, in_print)?;
                    Some(self.builder.build_int_mul(lhs, rhs, "mul").unwrap())
                }
                BinaryOp::Div => {
                    let lhs = self.generate_expr(left, function, printf, debug, in_print)?;
                    let rhs = self.generate_expr(right, function, printf, debug, in_print)?;
                    Some(self.builder.build_int_signed_div(lhs, rhs, "div").unwrap())
                }
                BinaryOp::Gt => {
                    let lhs = self.generate_expr(left, function, printf, debug, in_print)?;
                    let rhs = self.generate_expr(right, function, printf, debug, in_print)?;
                    Some(
                        self.builder
                            .build_int_compare(IntPredicate::SGT, lhs, rhs, "gt")
                            .unwrap(),
                    )
                }
                BinaryOp::Lt => {
                    let lhs = self.generate_expr(left, function, printf, debug, in_print)?;
                    let rhs = self.generate_expr(right, function, printf, debug, in_print)?;
                    Some(
                        self.builder
                            .build_int_compare(IntPredicate::SLT, lhs, rhs, "lt")
                            .unwrap(),
                    )
                }
                BinaryOp::Ge => {
                    let lhs = self.generate_expr(left, function, printf, debug, in_print)?;
                    let rhs = self.generate_expr(right, function, printf, debug, in_print)?;
                    Some(
                        self.builder
                            .build_int_compare(IntPredicate::SGE, lhs, rhs, "ge")
                            .unwrap(),
                    )
                }
                BinaryOp::Le => {
                    let lhs = self.generate_expr(left, function, printf, debug, in_print)?;
                    let rhs = self.generate_expr(right, function, printf, debug, in_print)?;
                    Some(
                        self.builder
                            .build_int_compare(IntPredicate::SLE, lhs, rhs, "le")
                            .unwrap(),
                    )
                }
                BinaryOp::Ne => {
                    let lhs = self.generate_expr(left, function, printf, debug, in_print)?;
                    let rhs = self.generate_expr(right, function, printf, debug, in_print)?;
                    Some(
                        self.builder
                            .build_int_compare(IntPredicate::NE, lhs, rhs, "ne")
                            .unwrap(),
                    )
                }
                BinaryOp::AddAssign => {
                    let lhs = self.generate_expr(left, function, printf, debug, in_print)?;
                    let rhs = self.generate_expr(right, function, printf, debug, in_print)?;
                    let new_val = self.builder.build_int_add(lhs, rhs, "addassign").unwrap();
                    if let Expr::String(name) = &**left {
                        if let Some(&ptr) = self.variables.get(name) {
                            self.builder.build_store(ptr, new_val).unwrap();
                        }
                    }
                    Some(new_val)
                }
                BinaryOp::SubAssign => {
                    let lhs = self.generate_expr(left, function, printf, debug, in_print)?;
                    let rhs = self.generate_expr(right, function, printf, debug, in_print)?;
                    let new_val = self.builder.build_int_sub(lhs, rhs, "subassign").unwrap();
                    if let Expr::String(name) = &**left {
                        if let Some(&ptr) = self.variables.get(name) {
                            self.builder.build_store(ptr, new_val).unwrap();
                        }
                    }
                    Some(new_val)
                }
                BinaryOp::MulAssign => {
                    let lhs = self.generate_expr(left, function, printf, debug, in_print)?;
                    let rhs = self.generate_expr(right, function, printf, debug, in_print)?;
                    let new_val = self.builder.build_int_mul(lhs, rhs, "mulassign").unwrap();
                    if let Expr::String(name) = &**left {
                        if let Some(&ptr) = self.variables.get(name) {
                            self.builder.build_store(ptr, new_val).unwrap();
                        }
                    }
                    Some(new_val)
                }
                BinaryOp::DivAssign => {
                    let lhs = self.generate_expr(left, function, printf, debug, in_print)?;
                    let rhs = self.generate_expr(right, function, printf, debug, in_print)?;
                    let new_val = self
                        .builder
                        .build_int_signed_div(lhs, rhs, "divassign")
                        .unwrap();
                    if let Expr::String(name) = &**left {
                        if let Some(&ptr) = self.variables.get(name) {
                            self.builder.build_store(ptr, new_val).unwrap();
                        }
                    }
                    Some(new_val)
                }
            },
            Expr::Assign(name, value) => {
                let _i32_type = self.context.i32_type();
                if let Some(&ptr) = self.variables.get(name) {
                    if let Some(val) = self.generate_expr(value, function, printf, debug, false) {
                        self.builder.build_store(ptr, val).unwrap();
                    }
                }
                None
            }
            Expr::String(name) => {
                if let Some(ptr) = self.variables.get(name) {
                    Some(
                        self.builder
                            .build_load(self.context.i32_type(), *ptr, "load")
                            .unwrap()
                            .into_int_value(),
                    )
                } else {
                    None
                }
            }
            Expr::Var(name) => {
                if let Some(ptr) = self.variables.get(name) {
                    Some(
                        self.builder
                            .build_load(self.context.i32_type(), *ptr, "load")
                            .unwrap()
                            .into_int_value(),
                    )
                } else {
                    None
                }
            }
            Expr::Int(i) => Some(self.context.i32_type().const_int(*i as u64, false)),
            Expr::For(iter, start, end, body) => {
                self.generate_for_loop(iter, start, end, body, function, printf, debug);
                None
            }
            Expr::Let(name, value) => {
                if let Expr::Int(val) = **value {
                    let alloca = self
                        .builder
                        .build_alloca(self.context.i32_type(), name)
                        .unwrap();
                    let val = self.context.i32_type().const_int(val as u64, false); // Add false for sign_extend
                    self.builder.build_store(alloca, val).unwrap();
                    self.variables.insert(name.clone(), alloca);
                }
                None
            }
            Expr::If(cond, then_body, else_if_blocks, else_body) => {
                let then_block = self.context.append_basic_block(function, "then");
                let merge_block = self.context.append_basic_block(function, "merge");
                let else_if_data: Vec<(BasicBlock<'ctx>, BasicBlock<'ctx>)> = else_if_blocks
                    .iter()
                    .enumerate()
                    .map(|(i, _)| {
                        (
                            self.context
                                .append_basic_block(function, &format!("else_if_{}", i)),
                            self.context
                                .append_basic_block(function, &format!("else_if_body_{}", i)),
                        )
                    })
                    .collect();
                let else_block = if else_body.is_some() {
                    Some(self.context.append_basic_block(function, "else"))
                } else {
                    None
                };
                let cond_val = self
                    .generate_expr(cond, function, printf, debug, false)
                    .unwrap_or_else(|| self.context.bool_type().const_int(0, false));
                if debug {
                    let fmt = self
                        .builder
                        .build_global_string_ptr("Main if condition = %d\n", "debug_fmt")
                        .unwrap();
                    self.builder
                        .build_call(
                            printf,
                            &[fmt.as_pointer_value().into(), cond_val.into()],
                            "debug_print",
                        )
                        .unwrap();
                }
                let next_block = if !else_if_data.is_empty() {
                    else_if_data[0].0
                } else if let Some(eb) = else_block {
                    eb
                } else {
                    merge_block
                };
                self.builder
                    .build_conditional_branch(cond_val, then_block, next_block)
                    .unwrap();
                self.builder.position_at_end(then_block);
                self.variables.push_scope();
                for expr in then_body {
                    self.generate_expr(expr, function, printf, debug, false);
                }
                self.variables.pop_scope();
                self.builder
                    .build_unconditional_branch(merge_block)
                    .unwrap();
                for (i, ((else_if_block, body_block), (cond_expr, body))) in
                    else_if_data.iter().zip(else_if_blocks).enumerate()
                {
                    self.builder.position_at_end(*else_if_block);
                    let elif_cond = self
                        .generate_expr(cond_expr, function, printf, debug, false)
                        .unwrap_or_else(|| self.context.bool_type().const_int(0, false));
                    if debug {
                        let fmt = self
                            .builder
                            .build_global_string_ptr(&format!("else-if #{} = %d\n", i), "debug_fmt")
                            .unwrap();
                        self.builder
                            .build_call(
                                printf,
                                &[fmt.as_pointer_value().into(), elif_cond.into()],
                                "debug_print",
                            )
                            .unwrap();
                    }
                    let next_else = if i < else_if_data.len() - 1 {
                        else_if_data[i + 1].0
                    } else if let Some(eb) = else_block {
                        eb
                    } else {
                        merge_block
                    };
                    self.builder
                        .build_conditional_branch(elif_cond, *body_block, next_else)
                        .unwrap();
                    self.builder.position_at_end(*body_block);
                    self.variables.push_scope();
                    for expr in body {
                        self.generate_expr(expr, function, printf, debug, false);
                    }
                    self.variables.pop_scope();
                    self.builder
                        .build_unconditional_branch(merge_block)
                        .unwrap();
                }
                if let (Some(else_block), Some(else_body)) = (else_block, else_body) {
                    self.builder.position_at_end(else_block);
                    self.variables.push_scope();
                    for expr in else_body {
                        self.generate_expr(expr, function, printf, debug, false);
                    }
                    self.variables.pop_scope();
                    self.builder
                        .build_unconditional_branch(merge_block)
                        .unwrap();
                }
                self.builder.position_at_end(merge_block);
                None
            }
            Expr::Continue => {
                if let Some(loop_bb) = function
                    .get_basic_blocks()
                    .iter()
                    .find(|bb| bb.get_name().to_str().unwrap() == "loop")
                {
                    self.builder.build_unconditional_branch(*loop_bb).unwrap();
                }
                None
            }
        }
    }
    pub fn generate_code(
        &mut self,
        exprs: &[Expr],
        debug: bool,
    ) -> Option<JitFunction<unsafe extern "C" fn() -> i32>> {
        let printf = self.setup_printf();
        let i32_type = self.context.i32_type();
        let fn_type = i32_type.fn_type(&[], false);
        let function = self.module.add_function("main", fn_type, None);
        let basic_block = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(basic_block);
        for expr in exprs {
            self.generate_expr(expr, function, printf, debug, false);
        }
        self.builder
            .build_return(Some(&i32_type.const_int(0, false)))
            .unwrap();
        unsafe { self.execution_engine.get_function("main").ok() }
    }
}
