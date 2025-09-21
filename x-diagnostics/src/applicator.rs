use crate::{Applicability, Diagnostic, FileDatabase, SourceId, SpanEdit};
use std::collections::BTreeMap;

#[derive(Debug)]
pub struct AppliedFile {
    pub source: SourceId,
    pub original: String,
    pub modified: String,
    pub edits_applied: usize,
    pub edits_skipped: usize,
}

pub fn apply_machine_applicable(db: &FileDatabase, diags: &[Diagnostic]) -> Vec<AppliedFile> {
    let mut per_file: BTreeMap<SourceId, Vec<&SpanEdit>> = BTreeMap::new();

    for d in diags {
        for s in &d.suggestions {
            if !matches!(s.applicability, Applicability::MachineApplicable) {
                continue;
            }
            for e in &s.edits {
                per_file.entry(e.span.source).or_default().push(e);
            }
        }
    }

    let mut results = Vec::new();
    for (sid, mut edits) in per_file {
        edits.sort_by_key(|e| e.span.start);

        let mut ok = true;
        for w in edits.windows(2) {
            if w[0].span.end > w[1].span.start {
                ok = false;
                break;
            }
        }
        let file = db.get(sid);
        let original = file.src.clone();
        if !ok {
            results.push(AppliedFile {
                source: sid,
                original,
                modified: file.src.clone(),
                edits_applied: 0,
                edits_skipped: edits.len(),
            });
            continue;
        }
        let mut text = original.clone();
        let mut applied = 0;
        for e in edits.iter().rev() {
            if e.span.end <= text.len() && e.span.start <= e.span.end {
                text.replace_range(e.span.start..e.span.end, &e.replacement);
                applied += 1;
            }
        }
        results.push(AppliedFile {
            source: sid,
            original,
            modified: text,
            edits_applied: applied,
            edits_skipped: 0,
        });
    }
    results
}
