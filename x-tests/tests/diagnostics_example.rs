use x_diagnostics::*;

#[test]
fn render_sample_diagnostic() {
    let mut db = FileDatabase::default();
    let sid = db.add_file("sample.x", "fn main() { let x: f64 = 1; }\n".to_string());
    let span_expr = Span { source: sid, start: 24, end: 25 }; // '1'
    let diag = Diagnostic::error("type mismatch")
        .with_code("E0001")
        .with_label(Label::primary(span_expr, "expected f64, found Int"))
        .with_help("change the literal to 1.0");
    let emitter = Emitter::default();
    emitter.emit(&db, &diag, &mut std::io::stderr()).unwrap();
}