use x_diagnostics::*;

pub fn type_mismatch(
    src: SourceId,
    got_span: Span,
    expected_span: Span,
    expected: &str,
    got: &str,
) -> Diagnostic {
    Diagnostic::error("type mismatch")
        .with_code("E0001")
        .with_label(Label::primary(got_span, format!("expected {}", expected)))
        .with_label(Label::secondary(expected_span, format!("found {}", got)))
        .with_help("adjust the expression or add an explicit cast")
}
