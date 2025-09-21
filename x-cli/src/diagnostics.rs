use x_diagnostics::{Emitter, FileDatabase, Diagnostic};

pub fn emit_all(db: &FileDatabase, diags: &[Diagnostic]) -> i32 {
    let emitter = Emitter::default();
    let mut errs = 0;
    for d in diags {
        if matches!(d.severity, x_diagnostics::Severity::Error) {
            errs += 1;
        }
        let _ = emitter.emit(db, d, &mut std::io::stderr());
    }
    errs
}