use once_cell::sync::OnceCell;
use tracing_subscriber::{fmt, EnvFilter};
use x_diagnostics::{Diagnostic, Severity, Suggestion};

static INIT: OnceCell<()> = OnceCell::new();

pub use tracing::{debug, error, info, span, trace, warn, Level};

pub fn init() {
    INIT.get_or_init(|| {
        let filter = EnvFilter::try_from_default_env()
            .or_else(|_| EnvFilter::try_new("info"))
            .unwrap();
        #[cfg(feature = "json")]
        fmt()
            .with_env_filter(filter)
            .json()
            .flatten_event(true)
            .init();
        #[cfg(not(feature = "json"))]
        fmt()
            .with_env_filter(filter)
            .with_target(true)
            .with_level(true)
            .init();
    });
}

pub fn log_diagnostic(diag: &Diagnostic) {
    let sev = match diag.severity {
        Severity::Error => "error",
        Severity::Warning => "warning",
        Severity::Note => "note",
        Severity::Help => "help",
    };
    let suggestions: Vec<_> = diag
        .suggestions
        .iter()
        .map(|s: &Suggestion| format!("{} edits={}", s.message, s.edits.len()))
        .collect();
    tracing::event!(
        target: "x.diagnostic",
        tracing::Level::INFO,
        severity = sev,
        code = diag.code.as_deref().unwrap_or(""),
        message = diag.message,
        labels = diag.labels.len(),
        notes = diag.notes.len(),
        help = diag.help.as_deref().unwrap_or(""),
        suggestions = ?suggestions
    );
}
