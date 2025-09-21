use clap::Parser;
use indicatif::{ProgressBar, ProgressStyle};
use plotters::prelude::*;
use std::collections::{BTreeMap, HashMap};
use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::{Duration, Instant};

#[derive(Parser, Debug)]
#[command(name = "x-bench")]
#[command(about = "Benchmark runner for the X compiler.")]
struct Cli {
    /// Path to the x-cli compiler executable.
    #[arg(long, default_value = "../target/debug/x-cli.exe")]
    cli_path: String,

    /// Path to the C cache runtime file.
    #[arg(long, default_value = "../runtime/cache_runtime.c")]
    cache_runtime_path: String,

    /// Directory containing the .x benchmark files.
    #[arg(long, default_value = "./benchmarks")]
    bench_dir: String,

    /// Number of iterations to run for each benchmark scenario.
    #[arg(long, short, default_value_t = 3)]
    iterations: u32,
}

#[derive(Debug, Clone)]
struct BenchResult {
    name: String,
    times: HashMap<String, f64>, // Maps "O0", "O1", etc. to avg time (ms)
}

/// Struct to group normal & memoised benchmark results.
struct BenchPair<'a> {
    base_name: String,
    normal: &'a BenchResult,
    memoised: &'a BenchResult,
}

/// Compiles and runs a single benchmark scenario.
fn run_test(
    source_path: &Path,
    opt_level: u8,
    cli_path: &str,
    _cache_runtime_path: &str,
) -> Result<Duration, String> {
    // Create a unique name for the executable for this run
    let exe_path = source_path.with_extension(format!("exe.o{}", opt_level));

    let mut build_cmd = Command::new(cli_path);
    build_cmd.args([
        "build",
        source_path.to_str().unwrap(),
        &format!("-O{}", opt_level),
    ]);

    let build_output = build_cmd.output().map_err(|e| e.to_string())?;
    if !build_output.status.success() {
        let stdout = String::from_utf8_lossy(&build_output.stdout);
        let stderr = String::from_utf8_lossy(&build_output.stderr);
        eprintln!("Build failed for {}:", source_path.display());
        if !stdout.trim().is_empty() {
            eprintln!("--- stdout ---\n{}\n", stdout);
        }
        if !stderr.trim().is_empty() {
            eprintln!("--- stderr ---\n{}\n", stderr);
        }
        return Err(format!("Build failed: {}", stderr.trim()));
    }

    let stem = source_path
        .file_stem()
        .and_then(|s| s.to_str())
        .ok_or_else(|| "Invalid source filename".to_string())?;
    let parent = source_path
        .parent()
        .unwrap_or_else(|| std::path::Path::new("."));

    let produced_candidates = vec![
        parent.join(stem),
        parent.join(format!("{}.exe", stem)),
        PathBuf::from(stem),
        PathBuf::from(format!("{}.exe", stem)),
    ];

    let produced_exe = produced_candidates
        .iter()
        .find(|p| p.exists())
        .ok_or_else(|| {
            format!(
                "Built executable not found. Candidates tried: {:?}",
                produced_candidates
            )
        })?
        .to_path_buf();

    fs::rename(&produced_exe, &exe_path).map_err(|e| {
        format!(
            "Failed to rename produced executable {:?} -> {:?}: {}",
            produced_exe, exe_path, e
        )
    })?;

    let start = Instant::now();
    Command::new(&exe_path)
        .output()
        .map_err(|e| e.to_string())?;
    let duration = start.elapsed();

    fs::remove_file(&exe_path).ok();
    Ok(duration)
}

/// Normalises a benchmark name to find its counterpart (e.g., "fib_memoised" -> "fib").
fn normalise_base_name(name: &str) -> String {
    let mut s = name.to_lowercase();
    // More robustly handle variations
    for pat in &[
        "_memoised",
        "-memoised",
        " memoised",
        "memoised",
        "_memoize",
        "-memoize",
        " memoize",
        "memoize",
    ] {
        s = s.replace(pat, "");
    }
    s.replace(['_', '-', '.'], "").trim().to_string()
}

/// Finds pairs of (normal, memoised) benchmarks from the full result list.
fn find_benchmark_pairs(results: &'_ [BenchResult]) -> Vec<BenchPair<'_>> {
    let mut map: BTreeMap<String, (Option<&BenchResult>, Option<&BenchResult>)> = BTreeMap::new();

    for r in results {
        let base_name = normalise_base_name(&r.name);
        let entry = map.entry(base_name).or_insert((None, None));
        if r.name.to_lowercase().contains("memo") {
            entry.1 = Some(r);
        } else {
            entry.0 = Some(r);
        }
    }

    map.into_iter()
        .filter_map(|(base_name, (normal_opt, memoised_opt))| {
            if let (Some(normal), Some(memoised)) = (normal_opt, memoised_opt) {
                Some(BenchPair {
                    base_name,
                    normal,
                    memoised,
                })
            } else {
                None
            }
        })
        .collect()
}

/// Generates a clean markdown table with all data.
fn generate_markdown_table(results: &[BenchResult]) -> Result<(), std::io::Error> {
    fs::create_dir_all("results")?;
    let file_path = "results/summary.md";
    let mut file = fs::File::create(file_path)?;
    writeln!(file, "# Benchmark Results")?;
    writeln!(
        file,
        "| Test Case | O0 (ms) | O1 (ms) | O2 (ms) | O3 (ms) |"
    )?;
    writeln!(
        file,
        "|-----------|---------|---------|---------|---------|"
    )?;
    for r in results {
        writeln!(
            file,
            "| {} | {:.2} | {:.2} | {:.2} | {:.2} |",
            r.name,
            r.times.get("O0").unwrap_or(&0.0),
            r.times.get("O1").unwrap_or(&0.0),
            r.times.get("O2").unwrap_or(&0.0),
            r.times.get("O3").unwrap_or(&0.0)
        )?;
    }
    println!("📊 Benchmark data table saved to {}", file_path);
    Ok(())
}

/// Generates a grouped bar chart comparing all results.
fn generate_summary_chart(results: &[BenchResult]) -> Result<(), Box<dyn std::error::Error>> {
    fs::create_dir_all("results")?;
    let file_path = "results/summary_chart.png";
    let root = BitMapBackend::new(file_path, (1600, 900)).into_drawing_area();
    root.fill(&WHITE)?;

    let max_time = results
        .iter()
        .flat_map(|r| r.times.values())
        .fold(0.0f64, |acc, &t| acc.max(t))
        .max(1.0f64);
    let test_names: Vec<String> = results.iter().map(|r| r.name.clone()).collect();
    let n = test_names.len();

    // Use a numeric (f64) x-axis so we can compute bar positions from indices directly.
    let mut chart = ChartBuilder::on(&root)
        .x_label_area_size(50)
        .y_label_area_size(80)
        .margin(20)
        .caption("Overall Performance Comparison", ("sans-serif", 40))
        .build_cartesian_2d(0f64..(n as f64), 0.0..(max_time * 1.1))?;

    chart
        .configure_mesh()
        .y_desc("Time (ms)")
        .x_desc("Benchmark")
        .disable_x_mesh()
        .axis_desc_style(("sans-serif", 15))
        .draw()?;

    let colors = [BLUE, GREEN, RED, BLACK];
    let opt_levels = ["O0", "O1", "O2", "O3"];
    let bar_width = 0.2;
    let bar_space = 0.05;

    for (i, level) in opt_levels.iter().enumerate() {
        let offset = (i as f64 - (opt_levels.len() as f64 - 1.0) / 2.0) * (bar_width + bar_space);
        chart
            .draw_series(results.iter().enumerate().map(|(j, res)| {
                let time = *res.times.get(*level).unwrap_or(&0.0);
                let center = j as f64;
                Rectangle::new(
                    [
                        (center + offset - bar_width / 2.0, 0.0),
                        (center + offset + bar_width / 2.0, time),
                    ],
                    colors[i].filled(),
                )
            }))?
            .label(*level)
            .legend(move |(x, y)| {
                Rectangle::new([(x, y - 5), (x + 15, y + 5)], colors[i].filled())
            });
    }
    chart
        .configure_series_labels()
        .background_style(WHITE.mix(0.8))
        .border_style(BLACK)
        .draw()?;

    println!("📊 Summary chart saved to {}", file_path);
    root.present()?;
    Ok(())
}

/// Generates line charts showing optimisation impact for each algorithm.
fn generate_line_charts(results: &[BenchResult]) -> Result<(), Box<dyn std::error::Error>> {
    fs::create_dir_all("results")?;
    for result in results {
        let file_path = format!("results/{}_optimisations.png", result.name);
        let root = BitMapBackend::new(&file_path, (1024, 768)).into_drawing_area();
        root.fill(&WHITE)?;

        let opt_levels = ["O0", "O1", "O2", "O3"];
        let times: Vec<f64> = opt_levels
            .iter()
            .map(|&level| *result.times.get(level).unwrap_or(&0.0))
            .collect();
        let max_time = times
            .iter()
            .fold(0.0_f64, |acc, &t| acc.max(t))
            .max(1.0_f64);

        let mut chart = ChartBuilder::on(&root)
            .margin(20)
            .x_label_area_size(40)
            .y_label_area_size(80)
            .caption(
                format!("Impact of Optimisation on '{}'", result.name),
                ("sans-serif", 30),
            )
            .build_cartesian_2d(0u32..3u32, 0.0..(max_time * 1.1))?;

        chart
            .configure_mesh()
            .x_labels(4)
            .x_label_formatter(&|x| format!("O{}", x))
            .y_desc("Time (ms)")
            .draw()?;

        chart.draw_series(LineSeries::new(
            times.iter().enumerate().map(|(i, &t)| (i as u32, t)),
            BLUE.stroke_width(2),
        ))?;
        chart.draw_series(PointSeries::of_element(
            times.iter().enumerate().map(|(i, &t)| (i as u32, t)),
            5,
            BLUE.filled(),
            &|c, s, st| Circle::new(c, s, st),
        ))?;

        println!("📊 Optimisation line chart saved to {}", file_path);
        root.present()?;
    }
    Ok(())
}

/// For each (normal, memoised) pair, generates an overlay line chart.
fn generate_pair_line_charts(pairs: &[BenchPair]) -> Result<(), Box<dyn std::error::Error>> {
    fs::create_dir_all("results")?;
    let opt_levels = ["O0", "O1", "O2", "O3"];

    for pair in pairs {
        let times_normal: Vec<f64> = opt_levels
            .iter()
            .map(|&l| *pair.normal.times.get(l).unwrap_or(&0.0))
            .collect();
        let times_memo: Vec<f64> = opt_levels
            .iter()
            .map(|&l| *pair.memoised.times.get(l).unwrap_or(&0.0))
            .collect();

        let max_time = times_normal
            .iter()
            .chain(times_memo.iter())
            .cloned()
            .fold(0.0_f64, f64::max)
            .max(1.0);

        let file_path = format!("results/{}_memo_compare.png", pair.base_name);
        let root = BitMapBackend::new(&file_path, (1024, 768)).into_drawing_area();
        root.fill(&WHITE)?;
        let mut chart = ChartBuilder::on(&root)
            .margin(20)
            .caption(
                format!("{} — Normal vs Memoised", pair.base_name),
                ("sans-serif", 24),
            )
            .x_label_area_size(40)
            .y_label_area_size(60)
            .build_cartesian_2d(0u32..3u32, 0.0..(max_time * 1.1))?;

        chart
            .configure_mesh()
            .x_labels(4)
            .x_label_formatter(&|x| format!("O{}", x))
            .y_desc("Time (ms)")
            .draw()?;

        // Normal Series
        chart
            .draw_series(LineSeries::new(
                times_normal.iter().enumerate().map(|(i, &v)| (i as u32, v)),
                BLUE.stroke_width(2),
            ))?
            .label("Normal")
            .legend(|(x, y)| PathElement::new(vec![(x, y), (x + 20, y)], BLUE));

        // Memoised Series
        chart
            .draw_series(LineSeries::new(
                times_memo.iter().enumerate().map(|(i, &v)| (i as u32, v)),
                RED.stroke_width(2),
            ))?
            .label("Memoised")
            .legend(|(x, y)| PathElement::new(vec![(x, y), (x + 20, y)], RED));

        chart
            .configure_series_labels()
            .background_style(WHITE.mix(0.8))
            .border_style(BLACK)
            .draw()?;
        root.present()?;
        println!("📊 Saved comparison chart: {}", file_path);
    }
    Ok(())
}

/// Summary bar chart for average speedup across all pairs.
fn generate_speedup_summary_chart(pairs: &[BenchPair]) -> Result<(), Box<dyn std::error::Error>> {
    if pairs.is_empty() {
        return Ok(());
    }
    fs::create_dir_all("results")?;
    let opt_levels = ["O0", "O1", "O2", "O3"];
    let mut avg_speedups = Vec::new();

    for &lvl in &opt_levels {
        let speedups: Vec<f64> = pairs
            .iter()
            .filter_map(|pair| {
                let normal_time = *pair.normal.times.get(lvl).unwrap_or(&0.0);
                let memo_time = *pair.memoised.times.get(lvl).unwrap_or(&0.0);
                if memo_time > 1e-9 && normal_time > 1e-9 {
                    Some(normal_time / memo_time)
                } else {
                    None
                }
            })
            .collect();
        let mean = if speedups.is_empty() {
            0.0
        } else {
            speedups.iter().sum::<f64>() / speedups.len() as f64
        };
        avg_speedups.push(mean);
    }

    let max_speedup = avg_speedups
        .iter()
        .cloned()
        .fold(0.0_f64, f64::max)
        .max(1.0);
    let file_path = "results/speedup_summary.png";
    let root = BitMapBackend::new(file_path, (1024, 768)).into_drawing_area();
    root.fill(&WHITE)?;

    let mut chart = ChartBuilder::on(&root)
        .margin(20)
        .caption("Average Memoisation Speedup", ("sans-serif", 24))
        .x_label_area_size(40)
        .y_label_area_size(60)
        .build_cartesian_2d(opt_levels.into_segmented(), 0.0..(max_speedup * 1.1))?;

    chart
        .configure_mesh()
        .y_desc("Average Speedup (X times faster)")
        .draw()?;

    chart.draw_series(
        Histogram::vertical(&chart).style(GREEN.filled()).data(
            opt_levels
                .iter()
                .zip(avg_speedups.iter())
                .map(|(l, s)| (l, *s)),
        ),
    )?;

    root.present()?;
    println!("📊 Saved speedup summary: {}", file_path);
    Ok(())
}

/// Generates a comprehensive HTML report.
fn generate_html_report(
    results: &[BenchResult],
    pairs: &[BenchPair],
) -> Result<(), std::io::Error> {
    fs::create_dir_all("results")?;
    let file_path = "results/index.html";
    let mut file = fs::File::create(file_path)?;

    writeln!(
        file,
        r#"<!DOCTYPE html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1.0"><title>X Language Benchmark Results</title><style>body{{font-family:-apple-system,BlinkMacSystemFont,"Segoe UI",Roboto,Helvetica,Arial,sans-serif;background-color:#f8f9fa;color:#212529;margin:0;padding:2rem;}} .container{{max-width:1200px;margin:0 auto;background-color:#fff;padding:2rem;border-radius:8px;box-shadow:0 4px 6px rgba(0,0,0,0.1);}} h1,h2,h3{{color:#343a40;border-bottom:2px solid #dee2e6;padding-bottom:0.5rem;margin-top:2rem;}} h1{{text-align:center;border:none;}} table{{width:100%;border-collapse:collapse;margin-top:1rem;}} th,td{{padding:0.75rem;border:1px solid #dee2e6;text-align:left;}} thead th{{background-color:#e9ecef;}} tbody tr:nth-child(odd){{background-color:#f8f9fa;}} .charts{{display:grid;grid-template-columns:repeat(auto-fit,minmax(500px,1fr));gap:2rem;margin-top:2rem;}} figure{{margin:0;}} img{{display:block;width:100%;height:auto;border-radius:4px;box-shadow:0 2px 4px rgba(0,0,0,0.075);}} figcaption{{text-align:center;font-style:italic;color:#6c757d;margin-top:0.5rem;}} footer{{text-align:center;margin-top:2rem;color:#6c757d;}}</style></head><body><div class="container"><h1>X Language Benchmark Results</h1>"#
    )?;

    // Summary Table
    writeln!(
        file,
        r#"<section><h2>Summary Data</h2><p><a href="summary.md">Download as Markdown</a></p><table><thead><tr><th>Test Case</th><th>O0 (ms)</th><th>O1 (ms)</th><th>O2 (ms)</th><th>O3 (ms)</th></tr></thead><tbody>"#
    )?;
    for r in results {
        writeln!(
            file,
            "<tr><td>{}</td><td>{:.2}</td><td>{:.2}</td><td>{:.2}</td><td>{:.2}</td></tr>",
            html_escape::encode_text(&r.name),
            r.times.get("O0").unwrap_or(&0.0),
            r.times.get("O1").unwrap_or(&0.0),
            r.times.get("O2").unwrap_or(&0.0),
            r.times.get("O3").unwrap_or(&0.0)
        )?;
    }
    writeln!(file, "</tbody></table></section>")?;

    // --- Charts ---

    // Memoisation Comparisons
    if !pairs.is_empty() {
        writeln!(
            file,
            "<section><h2>Memoisation vs Normal Comparison</h2><div class='charts'>"
        )?;
        writeln!(
            file,
            r#"<figure><img src="speedup_summary.png" alt="Average Speedup Chart"><figcaption>Average Speedup Factor from Memoisation</figcaption></figure>"#
        )?;
        for pair in pairs {
            writeln!(
                file,
                r#"<figure><img src="{}_memo_compare.png" alt="Comparison chart for {}"><figcaption>Performance: {} vs {}</figcaption></figure>"#,
                pair.base_name, pair.base_name, pair.normal.name, pair.memoised.name
            )?;
        }
        writeln!(file, "</div></section>")?;
    }

    // Overall & Individual Charts
    writeln!(
        file,
        "<section><h2>Overall and Individual Charts</h2><div class='charts'>"
    )?;
    writeln!(
        file,
        r#"<figure><img src="summary_chart.png" alt="Summary Chart"><figcaption>Overall Performance Comparison</figcaption></figure>"#
    )?;

    for r in results {
        let img_name = format!("{}_optimisations.png", r.name);
        writeln!(
            file,
            r#"<figure><img src="{}" alt="Optimisation chart for {}"><figcaption>Optimisation Impact: {}</figcaption></figure>"#,
            img_name,
            html_escape::encode_text(&r.name),
            html_escape::encode_text(&r.name)
        )?;
    }
    writeln!(file, "</div></section>")?;

    // Footer
    writeln!(
        file,
        r#"<footer>Generated by x-bench</footer></div></body></html>"#
    )?;
    println!("📄 HTML report saved to {}", file_path);
    Ok(())
}

fn main() -> Result<(), String> {
    let args = Cli::parse();
    let opt_levels = [0u8, 1u8, 2u8, 3u8];

    let benchmark_files: Vec<PathBuf> = fs::read_dir(&args.bench_dir)
        .map_err(|e| format!("Failed to read benchmark directory: {}", e))?
        .filter_map(|entry| entry.ok())
        .map(|entry| entry.path())
        .filter(|path| path.extension().map_or(false, |ext| ext == "x"))
        .collect();

    if benchmark_files.is_empty() {
        return Err("No benchmark files (.x) found in the specified directory.".to_string());
    }

    let total_runs = benchmark_files.len() * opt_levels.len();
    let bar = ProgressBar::new(total_runs as u64);
    bar.set_style(
        ProgressStyle::with_template(
            "{spinner:.green} [{elapsed_precise}] [{bar:40.cyan/blue}] {pos}/{len} ({eta}) - {msg}",
        )
        .unwrap()
        .progress_chars("#>-"),
    );

    let mut results_map: HashMap<String, BenchResult> = HashMap::new();

    for file_path in &benchmark_files {
        let name = file_path.file_stem().unwrap().to_str().unwrap().to_string();
        let mut result = BenchResult {
            name: name.clone(),
            times: HashMap::new(),
        };

        for &opt_level in &opt_levels {
            bar.set_message(format!("{} -O{}", name, opt_level));
            let mut total_duration = Duration::new(0, 0);
            for i in 0..args.iterations {
                bar.set_message(format!(
                    "{} -O{} (iter {}/{})",
                    name,
                    opt_level,
                    i + 1,
                    args.iterations
                ));
                total_duration += run_test(
                    file_path,
                    opt_level,
                    &args.cli_path,
                    &args.cache_runtime_path,
                )?;
            }
            let avg_time_ms = total_duration.as_millis() as f64 / args.iterations as f64;
            result.times.insert(format!("O{}", opt_level), avg_time_ms);
            bar.inc(1);
        }
        results_map.insert(name, result);
    }
    bar.finish_with_message("All benchmarks complete.");

    println!("\n--- Processing Results ---");

    let mut final_results: Vec<BenchResult> = results_map.into_values().collect();
    final_results.sort_by(|a, b| a.name.cmp(&b.name));

    // Find memoised/normal pairs for special visualisations
    let benchmark_pairs = find_benchmark_pairs(&final_results);
    if !benchmark_pairs.is_empty() {
        println!(
            "Found {} benchmark pairs for comparison.",
            benchmark_pairs.len()
        );
    }

    fs::create_dir_all("results").map_err(|e| e.to_string())?;

    // Generate all visualisations
    generate_markdown_table(&final_results).map_err(|e| e.to_string())?;
    generate_summary_chart(&final_results).map_err(|e| e.to_string())?;
    generate_line_charts(&final_results).map_err(|e| e.to_string())?;

    // Generate new comparison charts
    generate_pair_line_charts(&benchmark_pairs).map_err(|e| e.to_string())?;
    generate_speedup_summary_chart(&benchmark_pairs).map_err(|e| e.to_string())?;

    // Generate the final HTML report including all charts
    generate_html_report(&final_results, &benchmark_pairs).map_err(|e| e.to_string())?;

    println!("\n✅ All done! Open results/index.html to see the full report.");

    Ok(())
}
