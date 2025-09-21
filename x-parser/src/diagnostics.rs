use crate::Rule;
use pest::iterators::Pair;
use x_diagnostics::{Diagnostic, FileDatabase, Label, Span};

pub fn span_from_pair(source: x_diagnostics::SourceId, p: &Pair<Rule>) -> Span {
    let span = p.as_span();
    let start = span.start();
    let end = span.end();
    Span { source, start, end }
}
