use crate::{Diagnostic, FileDatabase, LabelStyle, Severity, SpanEdit, Suggestion};
use std::cmp::{max, min};
use std::collections::{BTreeMap, BTreeSet};
use std::io::{self, Write};
use unicode_width::UnicodeWidthStr;

pub struct Emitter {
    pub use_colour: bool,
    pub tty_width: usize,
}

impl Default for Emitter {
    fn default() -> Self {
        Self {
            use_colour: atty::is(atty::Stream::Stderr),
            tty_width: 100,
        }
    }
}

fn colour(enable: bool, code: &str) -> &'static str {
    if enable {
        match code {
            "red" => "\x1b[31m",
            "yellow" => "\x1b[33m",
            "blue" => "\x1b[34m",
            "cyan" => "\x1b[36m",
            "bold" => "\x1b[1m",
            "reset" => "\x1b[0m",
            "grey" => "\x1b[1m\x1b[90m",

            "light_blue" => "\x1b[94m",
            "white" => "\x1b[37m",
            "green" => "\x1b[32m",

            "blue_dark" => "\x1b[38;5;17m",
            "blue_dark_bold" => "\x1b[1m\x1b[38;5;17m",
            "blue_mid" => "\x1b[38;5;19m",
            "blue_light2" => "\x1b[38;5;25m",
            "blue_cyanish" => "\x1b[38;5;27m",

            _ => "",
        }
    } else {
        ""
    }
}

impl Emitter {
    pub fn emit(&self, db: &FileDatabase, d: &Diagnostic, mut w: impl Write) -> io::Result<()> {
        let (sev_str, sev_colour) = match d.severity {
            Severity::Error => ("error", "red"),
            Severity::Warning => ("warning", "yellow"),
            Severity::Note => ("note", "blue"),
            Severity::Help => ("help", "cyan"),
        };
        let code_str = d
            .code
            .as_ref()
            .map(|c| format!("[{}]", c))
            .unwrap_or_default();

        let is_type_mismatch = d.message.to_lowercase().contains("type mismatch");
        let message_colour = if is_type_mismatch {
            "blue_mid"
        } else if d
            .labels
            .iter()
            .any(|l| matches!(l.style, LabelStyle::Primary))
        {
            "red"
        } else if d
            .labels
            .iter()
            .any(|l| matches!(l.style, LabelStyle::Secondary))
        {
            "blue"
        } else {
            "white"
        };
        let colon_colour = if is_type_mismatch { "blue_mid" } else { "grey" };

        writeln!(
            w,
            "{}{}{}{}{}{}{}{}{}",
            colour(self.use_colour, sev_colour),
            format!("{}{}", sev_str, code_str),
            colour(self.use_colour, "reset"),
            colour(self.use_colour, colon_colour),
            ": ",
            colour(self.use_colour, "reset"),
            colour(self.use_colour, message_colour),
            d.message,
            colour(self.use_colour, "reset"),
        )?;

        let mut by_file = BTreeMap::new();
        for lbl in &d.labels {
            by_file
                .entry(lbl.span.source)
                .or_insert_with(Vec::new)
                .push(lbl);
        }
        for (fid, labels) in by_file {
            let file = db.get(fid);
            let mut labels = labels.clone();
            labels.sort_by_key(|l| l.span.start);
            let mut line_done = BTreeSet::new();
            for lbl in labels {
                let (line_no, col_start) = file.line_col(lbl.span.start);
                let (_, col_end) =
                    file.line_col(lbl.span.end.saturating_sub(1).max(lbl.span.start));
                if !line_done.insert((line_no, lbl.span.start)) {
                    continue;
                }
                let line_src = file.line_str(line_no);
                let line_clean = line_src.strip_suffix('\n').unwrap_or(line_src);
                writeln!(
                    w,
                    "{}  -->{} {}:{}:{}",
                    colour(self.use_colour, "cyan"),
                    colour(self.use_colour, "reset"),
                    file.path.display(),
                    line_no,
                    col_start
                )?;

                writeln!(
                    w,
                    "   {}{:>4}{} {}{}{} {}",
                    colour(self.use_colour, "cyan"),
                    "",
                    colour(self.use_colour, "reset"),
                    colour(self.use_colour, "cyan"),
                    "|",
                    colour(self.use_colour, "reset"),
                    ""
                )?;

                writeln!(
                    w,
                    "   {}{:>4}{} {}{}{} {}",
                    colour(self.use_colour, "cyan"),
                    line_no,
                    colour(self.use_colour, "reset"),
                    colour(self.use_colour, "cyan"),
                    "|",
                    colour(self.use_colour, "reset"),
                    line_clean
                )?;

                let prefix = line_clean.chars().take(col_start - 1).collect::<String>();
                let width = UnicodeWidthStr::width(prefix.as_str());
                let span_w = (col_end - col_start).max(1);
                let caret_colour = match lbl.style {
                    LabelStyle::Primary => "red",
                    LabelStyle::Secondary => "blue",
                };
                let mut marker = String::new();
                marker.push_str(&" ".repeat(width));
                marker.push_str(colour(self.use_colour, caret_colour));
                marker.push_str(&"^".repeat(span_w));
                marker.push_str(colour(self.use_colour, "reset"));
                if let Some(m) = &lbl.message {
                    marker.push(' ');
                    marker.push_str(colour(self.use_colour, caret_colour));
                    marker.push_str(m);
                    marker.push_str(colour(self.use_colour, "reset"));
                }
                writeln!(
                    w,
                    "        {}|{} {}",
                    colour(self.use_colour, "cyan"),
                    colour(self.use_colour, "reset"),
                    marker
                )?;

                writeln!(
                    w,
                    "   {}{:>4}{} {}{}{} {}",
                    colour(self.use_colour, "cyan"),
                    "",
                    colour(self.use_colour, "reset"),
                    colour(self.use_colour, "cyan"),
                    "|",
                    colour(self.use_colour, "reset"),
                    ""
                )?;
            }
        }

        for n in &d.notes {
            writeln!(
                w,
                "{}note{}: {}",
                colour(self.use_colour, "blue"),
                colour(self.use_colour, "reset"),
                n
            )?;
        }
        if let Some(h) = &d.help {
            writeln!(
                w,
                "{}help{}: {}",
                colour(self.use_colour, "cyan"),
                colour(self.use_colour, "reset"),
                h
            )?;
        }

        for sug in &d.suggestions {
            self.emit_suggestion(db, sug, &mut w)?;
        }

        if let Some(code) = d.code.clone() {
            writeln!(
                w,
                "{}For more information about this error, try `x --explain {}`{}",
                colour(
                    self.use_colour,
                    if d.message.to_lowercase().contains("type mismatch") {
                        "blue_mid"
                    } else {
                        "grey"
                    }
                ),
                code,
                colour(self.use_colour, "reset")
            )?;
        }

        Ok(())
    }

    pub fn emit_all(
        &self,
        db: &FileDatabase,
        diags: &[Diagnostic],
        mut w: impl Write,
    ) -> io::Result<()> {
        let mut warnings = 0usize;
        let mut errors = 0usize;
        for d in diags {
            if d.severity == Severity::Warning {
                warnings += 1;
            }
            if d.is_error() {
                errors += 1;
            }
            self.emit(db, d, &mut w)?;
        }
        if errors + warnings > 0 {
            writeln!(
                w,
                "{}{}{}",
                colour(self.use_colour, if errors > 0 { "red" } else { "yellow" }),
                if errors > 0 {
                    format!(
                        "error: {} previous error{}{}",
                        errors,
                        if errors == 1 { "" } else { "s" },
                        if warnings > 0 {
                            format!(
                                "; {warnings} warning{}",
                                if warnings == 1 { "" } else { "s" }
                            )
                        } else {
                            String::new()
                        }
                    )
                } else {
                    format!(
                        "warning: {warnings} warning{}",
                        if warnings == 1 { "" } else { "s" }
                    )
                },
                colour(self.use_colour, "reset")
            )?;
        }
        Ok(())
    }

    fn emit_suggestion(
        &self,
        db: &FileDatabase,
        sug: &Suggestion,
        mut w: impl Write,
    ) -> io::Result<()> {
        writeln!(
            w,
            "{}help{}: {}",
            colour(self.use_colour, "cyan"),
            colour(self.use_colour, "reset"),
            sug.message
        )?;

        if sug.edits.is_empty() {
            return Ok(());
        }

        let mut by_file: BTreeMap<_, Vec<&SpanEdit>> =
            BTreeMap::new();
        for e in &sug.edits {
            by_file.entry(e.span.source).or_default().push(e);
        }

        for (fid, edits) in by_file {
            let file = db.get(fid);
            let mut edits = edits.clone();
            edits.sort_by_key(|e| e.span.start);

            let mut min_line = usize::MAX;
            let mut max_line = 0usize;
            for e in &edits {
                let (ls, _) = file.line_col(e.span.start);
                let (le, _) = file.line_col(e.span.end.saturating_sub(1).max(e.span.start));
                min_line = min(min_line, ls);
                max_line = max(max_line, le);
            }
            if min_line == usize::MAX {
                continue;
            }

            for line_no in min_line..=max_line {
                let orig_line = file.line_str(line_no);
                let line_clean = orig_line.strip_suffix('\n').unwrap_or(orig_line);

                let line_start_byte;
                {
                    let (ln, _) = file.line_col(file.line_offsets[line_no - 1]);
                    debug_assert_eq!(ln, line_no);
                }
                line_start_byte = file.line_offsets[line_no - 1];
                let line_end_byte = if line_no < file.line_offsets.len() {
                    file.line_offsets[line_no] - 1
                } else {
                    file.src.len()
                };

                let mut line_edits: Vec<&SpanEdit> = edits
                    .iter()
                    .filter(|e| e.span.start >= line_start_byte && e.span.start <= line_end_byte)
                    .copied()
                    .collect();

                if line_edits.is_empty() {
                    writeln!(
                        w,
                        "   {}{:>4}{} {}{}{} {}",
                        colour(self.use_colour, "cyan"),
                        "",
                        colour(self.use_colour, "reset"),
                        colour(self.use_colour, "cyan"),
                        "|",
                        colour(self.use_colour, "reset"),
                        ""
                    )?;

                    writeln!(
                        w,
                        "   {}{:>4}{} {}{}{} {}",
                        colour(self.use_colour, "cyan"),
                        line_no,
                        colour(self.use_colour, "reset"),
                        colour(self.use_colour, "cyan"),
                        "|",
                        colour(self.use_colour, "reset"),
                        line_clean
                    )?;

                    writeln!(
                        w,
                        "   {}{:>4}{} {}{}{} {}",
                        colour(self.use_colour, "cyan"),
                        "",
                        colour(self.use_colour, "reset"),
                        colour(self.use_colour, "cyan"),
                        "|",
                        colour(self.use_colour, "reset"),
                        ""
                    )?;

                    continue;
                }

                writeln!(
                    w,
                    "   {}{:>4}{} {}{}{} {}",
                    colour(self.use_colour, "cyan"),
                    "",
                    colour(self.use_colour, "reset"),
                    colour(self.use_colour, "cyan"),
                    "|",
                    colour(self.use_colour, "reset"),
                    ""
                )?;

                writeln!(
                    w,
                    "   {}{:>4}{} {}{}{} {}",
                    colour(self.use_colour, "cyan"),
                    line_no,
                    colour(self.use_colour, "reset"),
                    colour(self.use_colour, "cyan"),
                    "|",
                    colour(self.use_colour, "reset"),
                    line_clean
                )?;

                line_edits.sort_by_key(|e| e.span.start);
                let mut rebuilt = String::new();
                let mut caret_segments: Vec<(usize, usize)> = vec![];
                let mut cursor = line_start_byte;

                for e in &line_edits {
                    if e.span.start > cursor {
                        rebuilt.push_str(&file.src[cursor..e.span.start]);
                    }
                    rebuilt.push_str(colour(self.use_colour, "green"));
                    rebuilt.push_str(&e.replacement);
                    rebuilt.push_str(colour(self.use_colour, "reset"));

                    let repl_col_start = self.byte_to_col(file, line_no, e.span.start);
                    let repl_col_end =
                        repl_col_start + UnicodeWidthStr::width(e.replacement.as_str());
                    caret_segments.push((repl_col_start, max(repl_col_end, repl_col_start + 1)));
                    cursor = e.span.end;
                }
                if cursor < line_start_byte + line_clean.len() {
                    rebuilt.push_str(&file.src[cursor..(line_start_byte + line_clean.len())]);
                }

                writeln!(
                    w,
                    "   {}{:>4}{} {}{}{} {}",
                    colour(self.use_colour, "cyan"),
                    line_no,
                    colour(self.use_colour, "reset"),
                    colour(self.use_colour, "cyan"),
                    "|",
                    colour(self.use_colour, "reset"),
                    rebuilt
                )?;

                let mut caret_line = String::new();
                let max_col = caret_segments.iter().map(|(_, e)| *e).max().unwrap_or(0);
                caret_line.push_str(&" ".repeat(max_col));
                let mut chars: Vec<char> = vec![' '; max_col];
                for (s, e) in caret_segments {
                    for i in s - 1..e - 1 {
                        if i < chars.len() {
                            chars[i] = '^';
                        }
                    }
                }
                let rendered: String = chars.into_iter().collect();

                writeln!(
                    w,
                    "        {}|{} {}{}{}",
                    colour(self.use_colour, "cyan"),
                    colour(self.use_colour, "reset"),
                    colour(self.use_colour, "green"),
                    rendered.trim_end(),
                    colour(self.use_colour, "reset")
                )?;

                writeln!(
                    w,
                    "   {}{:>4}{} {}{}{} {}",
                    colour(self.use_colour, "cyan"),
                    "",
                    colour(self.use_colour, "reset"),
                    colour(self.use_colour, "cyan"),
                    "|",
                    colour(self.use_colour, "reset"),
                    ""
                )?;
            }
        }

        Ok(())
    }

    fn byte_to_col(&self, file: &crate::FileRecord, line: usize, byte: usize) -> usize {
        let line_start = file.line_offsets[line - 1];
        let slice = &file.src[line_start..byte];
        UnicodeWidthStr::width(slice) + 1
    }
}
