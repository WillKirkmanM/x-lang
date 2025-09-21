use clap::Parser;
use console::{style, Style};
use indicatif::{ProgressBar, ProgressStyle};
use similar::{ChangeTag, TextDiff};
use std::collections::BTreeMap;
use std::fs;
use std::path::PathBuf;
use std::process::Command;

#[derive(Parser, Debug)]
#[command(name = "x-debugger")]
#[command(about = "Generates and diffs LLVM IR for different optimisation levels.")]
struct Cli {
    /// Path to the x-cli compiler executable.
    #[arg(long, default_value = "./target/debug/x-cli.exe")]
    cli_path: PathBuf,

    /// Path to the input .x source file.
    #[arg()]
    file: PathBuf,

    /// Directory to save the generated LLVM IR files.
    #[arg(long, default_value = "debug_out")]
    out_dir: PathBuf,
}

/// Invokes the compiler to generate LLVM IR for a specific optimisation level.
fn generate_ir(cli_path: &PathBuf, source_file: &PathBuf, opt_level: u8) -> Result<String, String> {
    let output = Command::new(cli_path)
        .args([
            "build",
            source_file.to_str().unwrap(),
            &format!("-O{}", opt_level),
            "--emit-llvm",
        ])
        .output()
        .map_err(|e| format!("Failed to execute compiler: {}", e))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(format!(
            "Compiler failed at -O{} with error:\n{}",
            opt_level, stderr
        ));
    }

    Ok(String::from_utf8_lossy(&output.stdout).to_string())
}

/// Prints a colored, line-by-line diff between two versions of the IR.
fn print_diff(old_label: &str, new_label: &str, old_ir: &str, new_ir: &str) {
    println!(
        "{}",
        style(format!("\n--- Diff: {} vs {} ---", old_label, new_label))
            .bold()
            .yellow()
    );

    let diff = TextDiff::from_lines(old_ir, new_ir);
    let mut no_changes = true;

    for change in diff.iter_all_changes() {
        let (sign, style) = match change.tag() {
            ChangeTag::Delete => {
                no_changes = false;
                ("-", Style::new().red())
            }
            ChangeTag::Insert => {
                no_changes = false;
                ("+", Style::new().green())
            }
            ChangeTag::Equal => (" ", Style::new().dim()),
        };
        print!("{}{}", style.apply_to(sign).bold(), style.apply_to(change));
    }

    if no_changes {
        println!("{}", style("No functional changes detected.").cyan());
    }
}

fn main() -> Result<(), String> {
    let args = Cli::parse();

    if !args.cli_path.exists() {
        return Err(format!(
            "Compiler not found at: {}",
            args.cli_path.display()
        ));
    }
    if !args.file.exists() {
        return Err(format!("Input file not found at: {}", args.file.display()));
    }

    fs::create_dir_all(&args.out_dir)
        .map_err(|e| format!("Failed to create output directory: {}", e))?;

    let opt_levels = [0u8, 1, 2, 3];
    let mut ir_results = BTreeMap::new();

    let bar = ProgressBar::new(opt_levels.len() as u64);
    bar.set_style(
        ProgressStyle::with_template("{spinner:.green} [{bar:25.cyan/blue}] {msg}")
            .unwrap()
            .progress_chars("=> "),
    );

    println!("🔬 Generating LLVM IR for all optimisation levels...");

    for &level in &opt_levels {
        bar.set_message(format!("Compiling with -O{}", level));
        let ir = generate_ir(&args.cli_path, &args.file, level)?;
        let file_name = args.file.file_stem().unwrap().to_str().unwrap().to_string();
        let out_path = args.out_dir.join(format!("{}.o{}.ll", file_name, level));

        fs::write(&out_path, &ir)
            .map_err(|e| format!("Failed to write IR to {}: {}", out_path.display(), e))?;

        ir_results.insert(level, ir);
        bar.inc(1);
    }
    bar.finish_with_message("Done");

    println!(
        "\n✅ Successfully generated IR files in `{}/` directory.",
        args.out_dir.display()
    );

    print_diff(
        "O0",
        "O1",
        ir_results.get(&0).unwrap(),
        ir_results.get(&1).unwrap(),
    );
    print_diff(
        "O1",
        "O2",
        ir_results.get(&1).unwrap(),
        ir_results.get(&2).unwrap(),
    );
    print_diff(
        "O2",
        "O3",
        ir_results.get(&2).unwrap(),
        ir_results.get(&3).unwrap(),
    );

    Ok(())
}
