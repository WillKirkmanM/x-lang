use std::path::PathBuf;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct SourceId(pub usize);

#[derive(Debug)]
pub struct FileRecord {
    pub id: SourceId,
    pub path: PathBuf,
    pub src: String,
    pub line_offsets: Vec<usize>,
}

impl FileRecord {
    fn new(id: SourceId, path: PathBuf, src: String) -> Self {
        let mut line_offsets = vec![0];
        for (i, b) in src.bytes().enumerate() {
            if b == b'\n' {
                line_offsets.push(i + 1);
            }
        }
        Self {
            id,
            path,
            src,
            line_offsets,
        }
    }
    pub fn line_col(&self, byte: usize) -> (usize, usize) {
        let idx = match self.line_offsets.binary_search(&byte) {
            Ok(i) => i,
            Err(i) => i - 1,
        };
        let line_start = self.line_offsets[idx];
        (idx + 1, (byte - line_start) + 1)
    }
    pub fn line_str(&self, line: usize) -> &str {
        let l0 = line - 1;
        let start = self.line_offsets[l0];
        let end = if l0 + 1 < self.line_offsets.len() {
            self.line_offsets[l0 + 1]
        } else {
            self.src.len()
        };
        &self.src[start..end]
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub source: SourceId,
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn single(source: SourceId, pos: usize) -> Self {
        Self {
            source,
            start: pos,
            end: pos + 1,
        }
    }
    pub fn join(a: Span, b: Span) -> Self {
        assert_eq!(a.source, b.source);
        Self {
            source: a.source,
            start: a.start.min(b.start),
            end: a.end.max(b.end),
        }
    }
    pub fn is_empty(&self) -> bool {
        self.end <= self.start
    }
}

#[derive(Default)]
pub struct FileDatabase {
    files: Vec<FileRecord>,
}

impl FileDatabase {
    pub fn add_file(&mut self, path: impl Into<PathBuf>, src: String) -> SourceId {
        let id = SourceId(self.files.len());
        self.files.push(FileRecord::new(id, path.into(), src));
        id
    }
    pub fn get(&self, id: SourceId) -> &FileRecord {
        &self.files[id.0]
    }
}
