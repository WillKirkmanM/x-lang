use crate::{ast::{BinaryOp, Expr}, binary_ops, scope::ScopeStack};
use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    execution_engine::{ExecutionEngine, JitFunction},
    module::Module,
    values::{BasicMetadataValueEnum, BasicValue, FunctionValue, IntValue},
    AddressSpace, IntPredicate, OptimizationLevel,
};
use std::error::Error;

pub struct CodeGen<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub execution_engine: ExecutionEngine<'ctx>,
    pub variables: ScopeStack<'ctx>,
    current_loop_after: Option<BasicBlock<'ctx>>,
    lambda_count: usize,
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
            current_loop_after: None,
            lambda_count: 0,
        })
    }
    fn setup_printf(&self) -> inkwell::values::FunctionValue<'ctx> {
        let i32_type = self.context.i32_type();
        let ptr_type = self.context.i8_type().ptr_type(AddressSpace::default());
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
        debug: bool,
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
    pub fn generate_expr(
        &mut self,
        expr: &Expr,
        function: FunctionValue<'ctx>,
        printf: FunctionValue<'ctx>,
        debug: bool,
        in_print: bool,
    ) -> Option<IntValue<'ctx>> {
        match expr {
            Expr::Print(inner) => {
                if debug {
                    println!("DEBUG: Entering Print expression");
                }
                match &**inner {
                    Expr::String(s) => {
                        if debug {
                            println!("DEBUG: Printing string: {}", s);
                        }
                        let s_with_null = format!("{}\n\0", s);
                        let fmt = self
                            .builder
                            .build_global_string_ptr(&s_with_null, "str_fmt")
                            .unwrap();
                        if debug {
                            println!("DEBUG: Built string pointer");
                        }
                        self.builder
                            .build_call(printf, &[fmt.as_pointer_value().into()], "printf")
                            .unwrap();
                        if debug {
                            println!("DEBUG: Completed string print");
                        }
                    }
                    Expr::Var(name) => {
                        if debug {
                            println!("DEBUG: Printing variable: {}", name);
                        }
                        if let Some(&ptr) = self.variables.get(name) {
                            if debug {
                                println!("DEBUG: Found variable pointer");
                            }
                            let val = self
                                .builder
                                .build_load(self.context.i32_type(), ptr, "load")
                                .unwrap();
                            if debug {
                                println!("DEBUG: Loaded variable value");
                            }
                            let fmt = self
                                .builder
                                .build_global_string_ptr("%d\n\0", "int_fmt")
                                .unwrap();
                            self.builder
                                .build_call(
                                    printf,
                                    &[fmt.as_pointer_value().into(), val.into()],
                                    "printf",
                                )
                                .unwrap();
                            if debug {
                                println!("DEBUG: Completed variable print");
                            }
                        } else {
                            if debug {
                                println!("DEBUG: Variable not found: {}", name);
                            }
                        }
                    }
                    Expr::Bool(b) => {
                        if debug {
                            println!("DEBUG: Printing boolean: {}", b);
                        }
                        let val = self.context.bool_type().const_int(*b as u64, false);
                        let fmt = self
                            .builder
                            .build_global_string_ptr("%d\n\0", "bool_fmt")
                            .unwrap();
                        self.builder
                            .build_call(
                                printf,
                                &[fmt.as_pointer_value().into(), val.into()],
                                "printf",
                            )
                            .unwrap();
                        if debug {
                            println!("DEBUG: Completed boolean print");
                        }
                    }
                    _ => {
                        if debug {
                            println!("DEBUG: Printing other expression");
                        }
                        if let Some(val) = self.generate_expr(inner, function, printf, debug, true)
                        {
                            if debug {
                                println!("DEBUG: Generated expression value");
                            }
                            let fmt = self
                                .builder
                                .build_global_string_ptr("%d\n\0", "int_fmt")
                                .unwrap();
                            self.builder
                                .build_call(
                                    printf,
                                    &[fmt.as_pointer_value().into(), val.into()],
                                    "printf",
                                )
                                .unwrap();
                            if debug {
                                println!("DEBUG: Completed expression print");
                            }
                        } else {
                            if debug {
                                println!("DEBUG: Expression generated no value");
                            }
                        }
                    }
                }
                if debug {
                    println!("DEBUG: Exiting Print expression");
                }
                None
            }
            Expr::Binary(op, left, right) => match op {
                BinaryOp::And => binary_ops::and::build_and(self, left, right, function, printf, debug, in_print),
                BinaryOp::Mod => binary_ops::bitmod::build_bitmod(self, left, right, function, printf, debug, in_print),
                BinaryOp::Eq => { binary_ops::eq::build_eq(self, left, right, function, printf, debug, in_print) }
                BinaryOp::Add => binary_ops::add::build_add(self, left, right, function, printf, debug, in_print),
                BinaryOp::Sub => binary_ops::sub::build_sub(self, left, right, function, printf, debug, in_print),
                BinaryOp::Mul => binary_ops::mul::build_mul(self, left, right, function, printf, debug, in_print),
                BinaryOp::Div => binary_ops::div::build_div(self, left, right, function, printf, debug, in_print),
                BinaryOp::Gt => binary_ops::gt::build_gt(self, left, right, function, printf, debug, in_print),
                BinaryOp::Lt => binary_ops::lt::build_lt(self, left, right, function, printf, debug, in_print),
                BinaryOp::Ge => binary_ops::ge::build_ge(self, left, right, function, printf, debug, in_print),
                BinaryOp::Le => binary_ops::le::build_le(self, left, right, function, printf, debug, in_print),
                BinaryOp::Ne => binary_ops::ne::build_ne(self, left, right, function, printf, debug, in_print),
                BinaryOp::AddAssign => binary_ops::addassign::build_addassign(self, left, right, function, printf, debug, in_print),
                BinaryOp::SubAssign => binary_ops::subassign::build_subassign(self, left, right, function, printf, debug, in_print),
                BinaryOp::MulAssign => binary_ops::mulassign::build_mulassign(self, left, right, function, printf, debug, in_print),
                BinaryOp::DivAssign => binary_ops::divassign::build_divassign(self, left, right, function, printf, debug, in_print),
                BinaryOp::Or => binary_ops::or::build_or(self, left, right, function, printf, debug, in_print),
                BinaryOp::BitAnd => binary_ops::bitand::build_bitand(self, left, right, function, printf, debug, in_print),
                BinaryOp::BitOr => binary_ops::bitor::build_bitor(self, left, right, function, printf, debug, in_print),
                BinaryOp::BitXor => binary_ops::bitxor::build_bitxor(self, left, right, function, printf, debug, in_print),
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
                if debug {
                    println!("DEBUG: Let {}", name);
                }

                let alloca = self
                    .builder
                    .build_alloca(self.context.i32_type(), name)
                    .unwrap();
                self.variables.insert(name.clone(), alloca);

                match &**value {
                    Expr::Int(val) => {
                        let const_val = self.context.i32_type().const_int(*val as u64, false);
                        self.builder.build_store(alloca, const_val).unwrap();
                    }
                    Expr::Bool(b) => {
                        let const_val = self.context.bool_type().const_int(*b as u64, false);
                        self.builder.build_store(alloca, const_val).unwrap();
                    }
                    Expr::Binary(op, lhs, rhs) => {
                        if let (Expr::Bool(l), Expr::Bool(r)) = (&**lhs, &**rhs) {
                            let result = match op {
                                BinaryOp::And => *l && *r,
                                BinaryOp::Or => *l || *r,
                                _ => return None,
                            };
                            let const_val =
                                self.context.bool_type().const_int(result as u64, false);
                            self.builder.build_store(alloca, const_val).unwrap();
                        }
                    }
                    Expr::Var(var_name) => {
                        if let Some(bool_result) = self.eval_boolean_expr(var_name) {
                            let const_val = self
                                .context
                                .bool_type()
                                .const_int(bool_result as u64, false);
                            self.builder.build_store(alloca, const_val).unwrap();
                            return None;
                        }

                        let result = if var_name.contains('&') {
                            self.parse_binary_op(var_name, '&')
                        } else if var_name.contains('|') {
                            self.parse_binary_op(var_name, '|')
                        } else if var_name.contains('^') {
                            self.parse_binary_op(var_name, '^')
                        } else if let Some(&ptr) = self
                            .variables
                            .get(var_name.split(';').next().unwrap_or(var_name).trim())
                        {
                            let val = self
                                .builder
                                .build_load(self.context.i32_type(), ptr, "load")
                                .unwrap();
                            self.builder.build_store(alloca, val).unwrap();
                            return None;
                        } else {
                            return None;
                        };

                        if let Some(val) = result {
                            let const_val = self.context.i32_type().const_int(val as u64, false);
                            self.builder.build_store(alloca, const_val).unwrap();
                        }
                    }
                    _ => {
                        if let Some(val) = self.generate_expr(value, function, printf, debug, true)
                        {
                            self.builder.build_store(alloca, val).unwrap();
                        }
                    }
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
            Expr::Bool(b) => Some(self.context.bool_type().const_int(*b as u64, false)),
            Expr::Float(f) => {
                let float_val = self.context.f64_type().const_float(*f);
                Some(
                    self.builder
                        .build_float_to_signed_int(
                            float_val,
                            self.context.i32_type(),
                            "float_to_int",
                        )
                        .unwrap(),
                )
            }
            Expr::Array(elements) => {
                let i32_type = self.context.i32_type();
                let len = elements.len();
                let array_type = i32_type.array_type(len as u32);
                let alloca = self
                    .builder
                    .build_array_alloca(i32_type, i32_type.const_int(len as u64, false), "array")
                    .unwrap();

                for (i, elem) in elements.iter().enumerate() {
                    if let Some(val) = self.generate_expr(elem, function, printf, debug, false) {
                        let gep = unsafe {
                            self.builder
                                .build_gep(
                                    i32_type,
                                    alloca,
                                    &[i32_type.const_int(i as u64, false)],
                                    &format!("array_{}", i),
                                )
                                .unwrap()
                        };
                        self.builder.build_store(gep, val).unwrap();
                    }
                }
                None
            }
            Expr::Null => Some(self.context.i32_type().const_zero()),
            Expr::Function(name, params, body) => {
                let i32_type = self.context.i32_type();
                let param_types = vec![i32_type.into(); params.len()];
                let fn_type = i32_type.fn_type(&param_types, false);

                let function = self.module.add_function(name, fn_type, None);
                let entry = self.context.append_basic_block(function, "entry");

                self.builder.position_at_end(entry);
                self.variables.push_scope();

                for (i, param) in params.iter().enumerate() {
                    let arg = function.get_nth_param(i as u32).unwrap();
                    let alloca = self.builder.build_alloca(i32_type, param).unwrap();
                    self.builder.build_store(alloca, arg).unwrap();
                    self.variables.insert(param.clone(), alloca);
                }

                for expr in body {
                    self.generate_expr(expr, function, printf, debug, false);
                }

                if !body.iter().any(|expr| matches!(expr, Expr::Return(_))) {
                    self.builder
                        .build_return(Some(&i32_type.const_int(0, false)))
                        .unwrap();
                }

                self.variables.pop_scope();
                None
            }
            Expr::Call(name, args) => {
                if let Some(func) = self.module.get_function(name) {
                    let mut compiled_args = Vec::new();
                    for arg in args {
                        if let Some(val) = self.generate_expr(arg, function, printf, debug, false) {
                            compiled_args.push(BasicMetadataValueEnum::from(val));
                        }
                    }

                    let result = self
                        .builder
                        .build_call(func, &compiled_args, "call")
                        .unwrap();
                    Some(result.try_as_basic_value().left().unwrap().into_int_value())
                } else {
                    None
                }
            }
            Expr::Return(value) => {
                if let Some(val) = value {
                    if let Some(ret_val) = self.generate_expr(val, function, printf, debug, false) {
                        self.builder.build_return(Some(&ret_val)).unwrap();
                    }
                } else {
                    self.builder
                        .build_return(Some(&self.context.i32_type().const_int(0, false)))
                        .unwrap();
                }
                None
            }
            Expr::While(condition, body) => {
                let cond_block = self.context.append_basic_block(function, "while.cond");
                let body_block = self.context.append_basic_block(function, "while.body");
                let after_block = self.context.append_basic_block(function, "while.after");

                let prev_after = self.current_loop_after;
                self.current_loop_after = Some(after_block);

                self.builder.build_unconditional_branch(cond_block).unwrap();

                self.builder.position_at_end(cond_block);
                let cond_val = self.generate_expr(condition, function, printf, debug, false)?;

                if debug {
                    let fmt = self
                        .builder
                        .build_global_string_ptr("While condition = %d\n", "debug_fmt")
                        .unwrap();
                    self.builder
                        .build_call(
                            printf,
                            &[fmt.as_pointer_value().into(), cond_val.into()],
                            "debug_print",
                        )
                        .unwrap();
                }

                self.builder
                    .build_conditional_branch(cond_val, body_block, after_block)
                    .unwrap();

                self.builder.position_at_end(body_block);
                for expr in body {
                    self.generate_expr(expr, function, printf, debug, false);
                }
                self.builder.build_unconditional_branch(cond_block).unwrap();

                self.builder.position_at_end(after_block);
                self.current_loop_after = prev_after;
                None
            }

            Expr::Break => {
                if let Some(after_block) = self.current_loop_after {
                    self.builder
                        .build_unconditional_branch(after_block)
                        .unwrap();
                }
                None
            }

            Expr::Global(name, init_expr) => {
                let i32_type = self.context.i32_type();
                let global = self.module.add_global(i32_type, None, name);

                if let Some(init_val) =
                    self.generate_expr(init_expr, function, printf, debug, false)
                {
                    global.set_initializer(&init_val);
                }
                None
            }

            Expr::Const(name, value) => {
                let i32_type = self.context.i32_type();
                if let Some(val) = self.generate_expr(value, function, printf, debug, false) {
                    let global =
                        self.module
                            .add_global(i32_type, Some(AddressSpace::from(0)), name);
                    global.set_constant(true);
                    global.set_initializer(&val);
                }
                None
            }

            Expr::Lambda(params, _, body) => {
                let i32_type = self.context.i32_type();
                let param_types = vec![i32_type.into(); params.len()];
                let fn_type = i32_type.fn_type(&param_types, false);

                let function = self.module.add_function(
                    &format!("lambda_{}", self.lambda_count),
                    fn_type,
                    None,
                );
                self.lambda_count += 1;

                let entry = self.context.append_basic_block(function, "entry");
                self.builder.position_at_end(entry);

                self.variables.push_scope();

                for (i, (param_name, _)) in params.iter().enumerate() {
                    let arg = function.get_nth_param(i as u32).unwrap();
                    let alloca = self.builder.build_alloca(i32_type, param_name).unwrap();
                    self.builder.build_store(alloca, arg).unwrap();
                    self.variables.insert(param_name.clone(), alloca);
                }

                for expr in body {
                    self.generate_expr(expr, function, printf, debug, false);
                }

                self.variables.pop_scope();

                Some(
                    function
                        .as_global_value()
                        .as_pointer_value()
                        .as_basic_value_enum()
                        .into_int_value(),
                )
            }

            Expr::FnRef(name) => {
                if let Some(func) = self.module.get_function(name) {
                    Some(
                        func.as_global_value()
                            .as_pointer_value()
                            .as_basic_value_enum()
                            .into_int_value(),
                    )
                } else {
                    None
                }
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

    fn eval_boolean_expr(&self, expr: &str) -> Option<bool> {
        let clean = expr.split(';').next()?.trim();
        if clean == "true" {
            return Some(true);
        }
        if clean == "false" {
            return Some(false);
        }
        if clean.contains("||") {
            let parts: Vec<&str> = clean.split("||").map(|s| s.trim()).collect();
            return Some(parts[0] == "true" || parts[1] == "true");
        }
        None
    }

    fn parse_binary_op(&self, expr: &str, op: char) -> Option<i32> {
        let clean = expr.split(';').next()?.trim();
        let parts: Vec<&str> = clean.split(op).map(|s| s.trim()).collect();
        if parts.len() != 2 {
            return None;
        }

        let lhs = parts[0].parse::<i32>().ok()?;
        let rhs = parts[1].parse::<i32>().ok()?;

        Some(match op {
            '&' => lhs & rhs,
            '|' => lhs | rhs,
            '^' => lhs ^ rhs,
            _ => return None,
        })
    }
}
