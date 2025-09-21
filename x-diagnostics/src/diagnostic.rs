use crate::Span;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
    Note,
    Help,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LabelStyle {
    Primary,
    Secondary,
}

#[derive(Debug, Clone)]
pub struct Label {
    pub span: Span,
    pub style: LabelStyle,
    pub message: Option<String>,
}
impl Label {
    pub fn primary(span: Span, msg: impl Into<String>) -> Self {
        Self {
            span,
            style: LabelStyle::Primary,
            message: Some(msg.into()),
        }
    }
    pub fn secondary(span: Span, msg: impl Into<String>) -> Self {
        Self {
            span,
            style: LabelStyle::Secondary,
            message: Some(msg.into()),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Applicability {
    MachineApplicable,
    MaybeIncorrect,
    HasPlaceholders,
    Unspecified,
}

#[derive(Debug, Clone)]
pub struct SpanEdit {
    pub span: Span,
    pub replacement: String,
}

#[derive(Debug, Clone)]
pub struct Suggestion {
    pub message: String,
    pub applicability: Applicability,
    pub edits: Vec<SpanEdit>,
}

impl Suggestion {
    pub fn new(msg: impl Into<String>) -> Self {
        Self {
            message: msg.into(),
            applicability: Applicability::Unspecified,
            edits: vec![],
        }
    }
    pub fn applicability(mut self, a: Applicability) -> Self {
        self.applicability = a;
        self
    }
    pub fn edit(mut self, span: Span, replacement: impl Into<String>) -> Self {
        self.edits.push(SpanEdit {
            span,
            replacement: replacement.into(),
        });
        self
    }
}

#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub severity: Severity,
    pub code: Option<String>,
    pub message: String,
    pub labels: Vec<Label>,
    pub notes: Vec<String>,
    pub help: Option<String>,
    pub suggestions: Vec<Suggestion>,
}

impl Diagnostic {
    pub fn error(msg: impl Into<String>) -> Self {
        Self {
            severity: Severity::Error,
            code: None,
            message: msg.into(),
            labels: vec![],
            notes: vec![],
            help: None,
            suggestions: vec![],
        }
    }
    pub fn warning(msg: impl Into<String>) -> Self {
        Self {
            severity: Severity::Warning,
            code: None,
            message: msg.into(),
            labels: vec![],
            notes: vec![],
            help: None,
            suggestions: vec![],
        }
    }
    pub fn note(msg: impl Into<String>) -> Self {
        Self {
            severity: Severity::Note,
            code: None,
            message: msg.into(),
            labels: vec![],
            notes: vec![],
            help: None,
            suggestions: vec![],
        }
    }
    pub fn help_diag(msg: impl Into<String>) -> Self {
        Self {
            severity: Severity::Help,
            code: None,
            message: msg.into(),
            labels: vec![],
            notes: vec![],
            help: None,
            suggestions: vec![],
        }
    }
    pub fn with_code(mut self, c: impl Into<String>) -> Self {
        self.code = Some(c.into());
        self
    }
    pub fn with_label(mut self, l: Label) -> Self {
        self.labels.push(l);
        self
    }
    pub fn with_help(mut self, h: impl Into<String>) -> Self {
        self.help = Some(h.into());
        self
    }
    pub fn with_note(mut self, n: impl Into<String>) -> Self {
        self.notes.push(n.into());
        self
    }
    pub fn with_suggestion(mut self, s: Suggestion) -> Self {
        self.suggestions.push(s);
        self
    }
    pub fn is_error(&self) -> bool {
        matches!(self.severity, Severity::Error)
    }
}
