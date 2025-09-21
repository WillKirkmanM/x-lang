use x_diagnostics::{apply_machine_applicable, Diagnostic, FileDatabase};

pub fn maybe_apply(db: &FileDatabase, diags: &[Diagnostic], write: bool) {
    let applied = apply_machine_applicable(db, diags);
    for file in applied {
        if file.edits_applied == 0 { continue; }
        eprintln!(
            "info: applied {} edit(s) ({} skipped) to {}",
            file.edits_applied,
            file.edits_skipped,
            file.source.0
        );
        if write {
            // std::fs::write(&path, file.modified).unwrap();
        }
    }
}