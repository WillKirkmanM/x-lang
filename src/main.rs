use clap::Parser;
use codegen::CodeGen;
use inkwell::context::Context;
use parser::parse_file;
use std::error::Error;
use std::fs;

pub mod ast;
pub mod codegen;
pub mod parser;
pub mod scope;
pub mod binary_ops;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    file: String,
    #[arg(long)]
    debug: bool,
    #[arg(long)]
    emit_llvm: bool,
}
fn main() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();
    let content = fs::read_to_string(&args.file)?;
    let exprs = parse_file(&content);
    let context = Context::create();
    let mut codegen = CodeGen::new(&context)?;
    if args.emit_llvm {
        println!("LLVM IR:");
        codegen.module.print_to_stderr();
    }
    let main_fn = codegen
        .generate_code(&exprs, args.debug)
        .ok_or("Failed to compile")?;
    unsafe {
        main_fn.call();
    }
    Ok(())
}

