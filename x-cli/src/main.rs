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
    },
    /// Build the source file into an executable
    Build {
        /// Source file to compile
        file: String,
    },
    /// Build and run the executable
    BuildAndRun {
        /// Source file to compile and run
        file: String,
    },
}

fn run_jit(source: &str) -> Result<(), String> {
    let program = parse(source).map_err(|e| format!("Parse error: {}", e))?;
    
    let context = Context::create();
    let mut codegen = CodeGen::new(&context, "main");
    
    codegen.generate(program)?;
    println!("Generated LLVM IR:");
    println!("{}", codegen.get_ir());
    
    codegen.jit_execute()?;
    Ok(())
}

fn build(source: &str, output: &str) -> Result<(), String> {
    let program = parse(source).map_err(|e| format!("Parse error: {}", e))?;
    
    let context = Context::create();
    let mut codegen = CodeGen::new(&context, "main");
    
    codegen.generate(program)?;
    
    fs::write("output.ll", codegen.get_ir())
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
        Commands::Run { file } => {
            match fs::read_to_string(file) {
                Ok(source) => {
                    if let Err(e) = run_jit(&source) {
                        eprintln!("Error: {}", e);
                    }
                }
                Err(e) => eprintln!("Error reading file: {}", e),
            }
        }
        Commands::Build { file } => {
            match fs::read_to_string(file) {
                Ok(source) => {
                    let output = file.trim_end_matches(".x");
                    if let Err(e) = build(&source, output) {
                        eprintln!("Error: {}", e);
                    }
                }
                Err(e) => eprintln!("Error reading file: {}", e),
            }
        }
        Commands::BuildAndRun { file } => {
            match fs::read_to_string(file) {
                Ok(source) => {
                    let output = file.trim_end_matches(".x");
                    if let Err(e) = build(&source, output) {
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
