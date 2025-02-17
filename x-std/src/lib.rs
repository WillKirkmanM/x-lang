use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::values::FunctionValue;
use inkwell::AddressSpace;

pub struct StdLib<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    print_fn: FunctionValue<'ctx>,
    print_str_fn: FunctionValue<'ctx>,
}

impl<'ctx> StdLib<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
        let module = context.create_module("stdlib");
        
        let i32_type = context.i32_type();
        let i8_ptr_type = context.ptr_type(AddressSpace::default());
        let f64_type = context.f64_type();
        
        let printf_type = i32_type.fn_type(&[i8_ptr_type.into()], true);
        let printf = module.add_function("printf", printf_type, Some(inkwell::module::Linkage::External));
        
        let print_type = f64_type.fn_type(&[f64_type.into()], false);
        let print_fn = module.add_function("print", print_type, None);
        let basic_block = context.append_basic_block(print_fn, "entry");
        let builder = context.create_builder();
        builder.position_at_end(basic_block);
        
        let fmt_str = builder.build_global_string_ptr("%f\n", "fmt_str")
            .unwrap()
            .as_pointer_value();
        
        let param = print_fn.get_nth_param(0).unwrap();
        builder.build_call(printf, &[fmt_str.into(), param.into_float_value().into()], "printf_call")
            .expect("Failed to build printf call");
        builder.build_return(Some(&param))
            .expect("Failed to build return");
        
        let print_str_type = f64_type.fn_type(&[i8_ptr_type.into()], false);
        let print_str_fn = module.add_function("print_str", print_str_type, None);
        let basic_block = context.append_basic_block(print_str_fn, "entry");
        builder.position_at_end(basic_block);
        
        let fmt_str = builder.build_global_string_ptr("%s\n", "str_fmt")
            .unwrap()
            .as_pointer_value();

        let param = print_str_fn.get_nth_param(0).unwrap();
        builder.build_call(printf, &[fmt_str.into(), param.into()], "printf_call")
            .expect("Failed to build printf call");
        builder.build_return(Some(&context.f64_type().const_float(0.0)))
            .expect("Failed to build return");

        StdLib {
            context,
            module,
            print_fn,
            print_str_fn,
        }
    }

    pub fn link_to_module(&self, target_module: &Module<'ctx>) {
        target_module.link_in_module(self.module.clone())
            .expect("Failed to link stdlib module");
    }

    pub fn get_print(&self) -> FunctionValue<'ctx> {
        self.print_fn
    }

    pub fn get_print_str(&self) -> FunctionValue<'ctx> {
        self.print_str_fn
    }
}