pub mod applicator;
pub mod diagnostic;
pub mod emitter;
pub mod macros;
pub mod span;

pub use applicator::*;
pub use diagnostic::*;
pub use emitter::*;
pub use span::*;

fn main() {
    let mut db = FileDatabase::default();
    let src = "fn main() { let x: f64 = 1; }\n";
    let sid = db.add_file("sample.x", src.to_string());

    let lit_byte = src.find("= 1;").unwrap() + 2;
    let lit_span = Span {
        source: sid,
        start: lit_byte,
        end: lit_byte + 1,
    };

    let brace = src.find('{').unwrap();
    let format_insert = Span {
        source: sid,
        start: brace + 1,
        end: brace + 1,
    };

    // Error with two edits: insert newline + indent, replace literal
    let err_sug = Suggestion::new("apply formatting and fix literal")
        .applicability(Applicability::MachineApplicable)
        .edit(format_insert, "\n    ")
        .edit(lit_span, "1.0");

    let error_diag = Diagnostic::error("type mismatch")
        .with_code("E0001")
        .with_label(Label::primary(lit_span, "expected f64, found Int"))
        .with_help("change the literal to 1.0")
        .with_suggestion(err_sug);

    // Simple warning (no footer)
    let warn_diag = Diagnostic::warning("unused variable")
        .with_code("W0001")
        .with_help("consider prefixing with '_' to silence this warning");

    // Note
    let note_diag = Diagnostic::note("unused variable context")
        .with_code("N0001")
        .with_help("variable is never read");

    // Help
    let help_diag = Diagnostic::help_diag("rename suggestion")
        .with_code("H0001")
        .with_help("rename variable to reflect intent");

    let diags = vec![error_diag, warn_diag, note_diag, help_diag];
    let emitter = Emitter::default();
    emitter
        .emit_all(&db, &diags, &mut std::io::stderr())
        .unwrap();
}
