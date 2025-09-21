use inkwell::{context::Context, values::AnyValue};
use x_parser::parse;
use x_typechecker::TypeChecker;
use x_borrow_checker::BorrowChecker;
use x_codegen::CodeGen;

/// Compile source through parse -> typecheck -> borrow check -> codegen and return the IR.
/// On error this will panic with detailed diagnostic info (typed program + partial IR + declared functions + per-function IR)
pub fn compile_and_get_ir(src: &str, module_name: &str) -> String {
    let program = parse(src).expect("parse failed");
    let typed = TypeChecker::new().check(program).expect("typecheck failed");
    BorrowChecker::new().check(&typed).expect("borrow check failed");

    let ctx = Context::create();
    let mut cg = CodeGen::new(&ctx, module_name);

    // Mirror x-cli pre-codegen steps if your CLI does them
    let _ = cg.register_traits(&typed);
    let _ = cg.register_methods(&typed);

    if let Err(e) = cg.generate(typed.clone()) {
        // Attempt to include partial IR and declared functions to aid debugging
        let partial_ir = cg.get_ir();
        let declared: Vec<String> = cg
            .functions
            .keys()
            .cloned()
            .chain(cg.external_functions.keys().cloned())
            .collect();

        // Per-function IR snippets (if available)
        let mut func_snippets = Vec::new();
        for (name, func) in &cg.functions {
            // print_to_string() works for function values and shows if it is only a declaration
            let snippet = func.print_to_string().to_string();
            func_snippets.push((name.clone(), snippet));
        }

        panic!(
            "codegen generate failed: {:?}\n\nTyped program: {:#?}\n\nDeclared functions: {:#?}\n\nPer-function IR snippets:\n{:#?}\n\nPartial IR:\n{}",
            e, typed, declared, func_snippets, partial_ir
        );
    }

    cg.get_ir()
}