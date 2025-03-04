use std::process::Command;
use std::fs;
use clap::{Parser, Subcommand};
use inkwell::context::Context;
use x_codegen::CodeGen;
use x_parser::parse;

#[derive(Parser)]
#[command(name = "x")]
#[command(about = "X Programming Language CLI", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Run the source file using JIT execution
    Run {
        /// Source file to run
        file: String,
        
        /// Emit LLVM IR
        #[arg(long)]
        emit_llvm: bool,
        
        /// Show parsing output
        #[arg(long)]
        show_parse: bool,
    },
    /// Build the source file into an executable
    Build {
        /// Source file to compile
        file: String,
        
        /// Emit LLVM IR
        #[arg(long)]
        emit_llvm: bool,
        
        /// Show parsing output
        #[arg(long)]
        show_parse: bool,
    },
    /// Build and run the executable
    BuildAndRun {
        /// Source file to compile and run
        file: String,
        
        /// Emit LLVM IR
        #[arg(long)]
        emit_llvm: bool,
        
        /// Show parsing output
        #[arg(long)]
        show_parse: bool,
    },
}

fn run_jit(source: &str, emit_llvm: bool, show_parse: bool) -> Result<(), String> {
    let program = parse(source).map_err(|e| format!("Parse error: {}", e))?;

    if show_parse {
        println!("Parse result:");
        println!("{:#?}", program);
    }
    
    let context = Context::create();
    let mut codegen = CodeGen::new(&context, "main");
    
    codegen.generate(program)?;
    
    if emit_llvm {
        println!("Generated LLVM IR:");
        println!("{}", codegen.get_ir());
    }
    
    codegen.jit_execute()?;
    Ok(())
}

fn build(source: &str, output: &str, emit_llvm: bool, show_parse: bool) -> Result<(), String> {
    let program = parse(source).map_err(|e| format!("Parse error: {}", e))?;
    
    if show_parse {
        println!("Parse result:");
        println!("{:#?}", program);
    }
    
    let context = Context::create();
    let mut codegen = CodeGen::new(&context, "main");
    
    codegen.generate(program)?;
    
    let ir = codegen.get_ir();
    if emit_llvm {
        println!("Generated LLVM IR:");
        println!("{}", ir);
    }
    
    fs::write("output.ll", ir)
        .map_err(|e| format!("Failed to write IR: {}", e))?;

    let status = Command::new("llc")
        .args([
            "-opaque-pointers",
            "-filetype=obj",
            "-relocation-model=pic",
            "output.ll",
            "-o",
            "output.o"
        ])
        .status()
        .map_err(|e| format!("Failed to run llc: {}", e))?;
    
    if !status.success() {
        return Err("llc failed".to_string());
    }

    let status = Command::new("clang")
        .args([
            "output.o",
            "-o",
            output,
            "-fPIE",
            "-pie",
            "-lm"
        ])
        .status()
        .map_err(|e| format!("Failed to run clang: {}", e))?;
    
    if !status.success() {
        return Err("linking failed".to_string());
    }

    fs::remove_file("output.ll").ok();
    fs::remove_file("output.o").ok();
    
    Ok(())
}

fn main() {
    let cli = Cli::parse();

    match &cli.command {
        Commands::Run { file, emit_llvm, show_parse } => {
            match fs::read_to_string(file) {
                Ok(source) => {
                    if let Err(e) = run_jit(&source, *emit_llvm, *show_parse) {
                        eprintln!("Error: {}", e);
                    }
                }
                Err(e) => eprintln!("Error reading file: {}", e),
            }
        }
        Commands::Build { file, emit_llvm, show_parse } => {
            match fs::read_to_string(file) {
                Ok(source) => {
                    let output = file.trim_end_matches(".x");
                    if let Err(e) = build(&source, output, *emit_llvm, *show_parse) {
                        eprintln!("Error: {}", e);
                    }
                }
                Err(e) => eprintln!("Error reading file: {}", e),
            }
        }
        Commands::BuildAndRun { file, emit_llvm, show_parse } => {
            match fs::read_to_string(file) {
                Ok(source) => {
                    let output = file.trim_end_matches(".x");
                    if let Err(e) = build(&source, output, *emit_llvm, *show_parse) {
                        eprintln!("Error: {}", e);
                        return;
                    }
                    
                    let status = Command::new(format!("./{}", output))
                        .status()
                        .unwrap_or_else(|e| {
                            eprintln!("Failed to run program: {}", e);
                            std::process::exit(1);
                        });
                    
                    fs::remove_file(output).ok();
                    
                    if !status.success() {
                        eprintln!("Program exited with error");
                    }
                }
                Err(e) => eprintln!("Error reading file: {}", e),
            }
        }
    }
}
