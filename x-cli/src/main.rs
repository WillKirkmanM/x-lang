use inkwell::context::Context;
use x_parser::parse;
use x_codegen::CodeGen;

fn main() {
    let input = r#"
import std::print;
let a = 3;
let b = 4;
print("The result of 3 + 4 is:");
print(a + b);
"#;
    
    let program = match parse(input) {
        Ok(p) => p,
        Err(e) => {
            eprintln!("Parse error: {}", e);
            return;
        }
    };

    println!("AST: {:?}", program);

    let context = Context::create();
    let mut codegen = CodeGen::new(&context, "main");
    
    match codegen.generate(program) {
        Ok(_) => {
            println!("Generated LLVM IR:");
            println!("{}", codegen.get_ir());
            
            match codegen.jit_execute() {
                Ok(result) => println!("Result: {}", result),
                Err(e) => eprintln!("Execution error: {}", e)
            }
        },
        Err(e) => eprintln!("Codegen error: {}", e)
    }
}