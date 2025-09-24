use clap::{Parser, Subcommand};
use inkwell::context::Context;
use std::fs;
use std::path::PathBuf;
use std::process::Command;
use x_borrow_checker::BorrowChecker;
use x_codegen::CodeGen;
use x_parser::parse;
use x_typechecker::TypeChecker;

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

        /// Libraries to link (-l flag)
        #[arg(long = "link", short = 'l')]
        libs: Vec<String>,

        /// Library search paths (-L flag)
        #[arg(long = "link-path", short = 'L')]
        lib_paths: Vec<String>,

        /// Include paths (-I flag)
        #[arg(long = "include", short = 'I')]
        include_paths: Vec<String>,

        /// LLVM optimisation level (0-3)
        #[arg(short = 'O', long, default_value_t = 0, value_parser = clap::value_parser!(u8).range(0..=3))]
        opt_level: u8,
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

        /// Libraries to link (-l flag)
        #[arg(long = "link", short = 'l')]
        libs: Vec<String>,

        /// Library search paths (-L flag)
        #[arg(long = "link-path", short = 'L')]
        lib_paths: Vec<String>,

        /// Include paths (-I flag)
        #[arg(long = "include", short = 'I')]
        include_paths: Vec<String>,

        /// LLVM optimisation level (0-3)
        #[arg(short = 'O', long, default_value_t = 0, value_parser = clap::value_parser!(u8).range(0..=3))]
        opt_level: u8,
    },
}

fn run_jit(source: &str, emit_llvm: bool, show_parse: bool) -> Result<(), String> {
    let program = parse(source).map_err(|e| format!("Parse error: {}", e))?;

    if show_parse {
        println!("Parse result:");
        println!("{:?}", program);
    }

    let context = Context::create();
    let mut codegen = CodeGen::new(&context, "main");

    let mut checker = TypeChecker::new();
    let typed_program = checker
        .check(program)
        .map_err(|e| format!("Type error: {e}"))?;

    let mut borrow_checker = BorrowChecker::new();
    borrow_checker
        .check(&typed_program)
        .map_err(|e| format!("Borrow error: {e}"))?;

    codegen.generate(typed_program)?;

    if emit_llvm {
        println!("Generated LLVM IR:");
        println!("{:?}", codegen.get_ir());
    }

    codegen.jit_execute()?;
    Ok(())
}

fn build(
    source: &str,
    output: &str,
    emit_llvm: bool,
    show_parse: bool,
    libs: &[String],
    lib_paths: &[String],
    include_paths: &[String],
    opt_level: u8,
) -> Result<(), String> {
    let program = parse(source).map_err(|e| format!("Parse error: {}", e))?;

    if show_parse {
        println!("Parse result:");
        println!("{:?}", program);
    }

    let context = Context::create();

    let mut codegen = CodeGen::new(&context, "main");

    let mut checker = TypeChecker::new();
    let typed_program = checker
        .check(program)
        .map_err(|e| format!("Type error: {e}"))?;

    let mut borrow_checker = BorrowChecker::new();
    borrow_checker
        .check(&typed_program)
        .map_err(|e| format!("Borrow error: {e}"))?;

    codegen.register_traits(&typed_program);
    codegen.register_methods(&typed_program);

    codegen.generate(typed_program)?;

    let ir = codegen.get_ir();
    if emit_llvm {
        println!("Generated LLVM IR:");
        println!("{:#?}", ir);
    }

    fs::write("output.ll", ir).map_err(|e| format!("Failed to write IR: {}", e))?;

    let opt_level_arg = format!("-O{}", opt_level); // Create the -O<level> string

    let llc_status = Command::new("llc")
        .args([
            opt_level_arg.as_str(),
            "-filetype=obj",
            "-relocation-model=pic",
            "output.ll",
            "-o",
            "output.o",
        ])
        .status()
        .map_err(|e| format!("Failed to run llc: {}", e))?;

    if !llc_status.success() {
        fs::remove_file("output.ll").ok();
        return Err("llc compilation failed".to_string());
    }

    let mut clang_args = vec![
        "output.o".to_string(),
        // Embed cache runtime at compile time and write it out for the linker
        {
            let runtime_c = format!(
                "{}\n\nint __xlang_type_id(void* p) {{ (void)p; return 0; }}\n",
                include_str!("../../runtime/cache_runtime.c")
            );
            let runtime_h = format!(
                "{}\n\nint __xlang_type_id(void* p);\n",
                include_str!("../../runtime/cache_runtime.h")
            );
            let runtime_c_path = "cache_runtime.c";
            let runtime_h_path = "cache_runtime.h";
            fs::write(runtime_h_path, runtime_h)
                .map_err(|e| format!("Failed to write runtime header: {}", e))?;

            let dispatch_c = include_str!("../../runtime/parallel.c");
            let runtime_c = format!("{}\n\n{}\n", runtime_c, dispatch_c);
            fs::write(runtime_c_path, runtime_c)
                .map_err(|e| format!("Failed to write runtime C: {}", e))?;
            runtime_c_path.to_string()
        },
        "-o".to_string(),
        output.to_string(),
    ];

    #[cfg(not(target_os = "windows"))]
    {
        clang_args.push("-fPIE".to_string());
        clang_args.push("-pie".to_string());
        clang_args.push("-lm".to_string());
        clang_args.push("-lm".to_string());
    }

    clang_args.push("-fopenmp".to_string());

    for path in lib_paths {
        clang_args.push("-L".to_string());
        clang_args.push(path.clone());
    }

    for path in include_paths {
        clang_args.push("-I".to_string());
        clang_args.push(path.clone());
    }

    for lib in libs {
        clang_args.push("-l".to_string());
        clang_args.push(lib.clone());
    }

    let clang_status = Command::new("clang")
        .args(&clang_args)
        .status()
        .map_err(|e| format!("Failed to run clang linker: {}", e))?;

    fs::remove_file("output.ll").ok();
    fs::remove_file("output.o").ok();

    if !clang_status.success() {
        return Err("Linking failed".to_string());
    }

    println!("Build successful: {}", output);
    Ok(())
}

fn main() {
    x_logging::init();

    let cli = Cli::parse();

    match &cli.command {
        Commands::Run {
            file,
            emit_llvm,
            show_parse,
        } => match fs::read_to_string(file) {
            Ok(source) => {
                if let Err(e) = run_jit(&source, *emit_llvm, *show_parse) {
                    eprintln!("Error: {}", e);
                }
            }
            Err(e) => eprintln!("Error reading file: {}", e),
        },
        Commands::Build {
            file,
            emit_llvm,
            show_parse,
            libs,
            lib_paths,
            include_paths,
            opt_level,
        } => match fs::read_to_string(file) {
            Ok(source) => {
                let output = file.trim_end_matches(".x");
                if let Err(e) = build(
                    &source,
                    output,
                    *emit_llvm,
                    *show_parse,
                    libs,
                    lib_paths,
                    include_paths,
                    *opt_level,
                ) {
                    eprintln!("Error: {}", e);
                }
            }
            Err(e) => eprintln!("Error reading file: {}", e),
        },
        Commands::BuildAndRun {
            file,
            emit_llvm,
            show_parse,
            libs,
            lib_paths,
            include_paths,
            opt_level,
        } => {
            match fs::read_to_string(file) {
                Ok(source) => {
                    let output = file.trim_end_matches(".x");
                    if let Err(e) = build(
                        &source,
                        output,
                        *emit_llvm,
                        *show_parse,
                        libs,
                        lib_paths,
                        include_paths,
                        *opt_level,
                    ) {
                        eprintln!("Error: {}", e);
                        return;
                    }

                    let mut run_path = PathBuf::from(output);
                    if !run_path.exists() {
                        let try_exe = run_path.with_extension("exe");
                        if try_exe.exists() {
                            run_path = try_exe;
                        } else {
                            if let Some(parent) = run_path.parent() {
                                if let Some(stem) = run_path.file_stem().and_then(|s| s.to_str()) {
                                    if let Ok(entries) = fs::read_dir(parent) {
                                        for ent in entries.flatten() {
                                            let p = ent.path();
                                            if let Some(pstem) =
                                                p.file_stem().and_then(|s| s.to_str())
                                            {
                                                if pstem == stem {
                                                    run_path = p;
                                                    break;
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }

                    if !run_path.exists() {
                        eprintln!("Failed to find built artifact at '{}'", output);
                        std::process::exit(1);
                    }

                    let status = Command::new(&run_path).status().unwrap_or_else(|e| {
                        eprintln!("Failed to run program: {}", e);
                        std::process::exit(1);
                    });

                    fs::remove_file(&run_path).ok();

                    if !status.success() {
                        // eprintln!("Program exited with error");
                    }
                }
                Err(e) => eprintln!("Error reading file: {}", e),
            }
        }
    }
}
