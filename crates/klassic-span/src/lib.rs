use std::fmt;
use std::sync::Arc;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub const fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub fn merge(self, other: Self) -> Self {
        Self {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SourceFile {
    name: String,
    text: String,
    line_starts: Vec<usize>,
}

impl SourceFile {
    pub fn new(name: impl Into<String>, text: impl Into<String>) -> Self {
        let name = name.into();
        let text = text.into();
        let mut line_starts = vec![0];
        for (index, ch) in text.char_indices() {
            if ch == '\n' {
                line_starts.push(index + 1);
            }
        }
        Self {
            name,
            text,
            line_starts,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn text(&self) -> &str {
        &self.text
    }

    pub fn line_col(&self, offset: usize) -> (usize, usize) {
        let clamped = offset.min(self.text.len());
        let line_index = self
            .line_starts
            .partition_point(|line_start| *line_start <= clamped)
            .saturating_sub(1);
        let line_start = self.line_starts[line_index];
        (line_index + 1, clamped - line_start + 1)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DiagnosticKind {
    Parse,
    Type,
    Compile,
    Runtime,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Diagnostic {
    pub kind: DiagnosticKind,
    pub severity: Severity,
    pub span: Option<Span>,
    pub message: String,
    pub incomplete_input: bool,
    /// The source file `span` actually indexes into, when known. A
    /// runtime error raised inside a stdlib/imported module's
    /// function body carries a span in that module's own byte space;
    /// tagging it here lets rendering pick the right `SourceFile`
    /// instead of misrendering against whichever file happens to be
    /// evaluating when the error surfaces (issue #450).
    pub source: Option<Arc<SourceFile>>,
    /// The name the user actually called to reach the def whose body
    /// raised this error, when known — e.g. `last` for a `head`
    /// failure raised deep inside `def last(xs) = ... head(xs) ...`.
    /// Rendered as a `{name}: ` prefix on the message so the error
    /// names what the user called instead of only the innermost
    /// builtin (issue #450, second half).
    pub call_name: Option<String>,
}

impl Diagnostic {
    pub fn parse(span: Span, message: impl Into<String>) -> Self {
        Self {
            kind: DiagnosticKind::Parse,
            severity: Severity::Error,
            span: Some(span),
            message: message.into(),
            incomplete_input: false,
            source: None,
            call_name: None,
        }
    }

    pub fn runtime(span: Span, message: impl Into<String>) -> Self {
        Self {
            kind: DiagnosticKind::Runtime,
            severity: Severity::Error,
            span: Some(span),
            message: message.into(),
            incomplete_input: false,
            source: None,
            call_name: None,
        }
    }

    pub fn compile(span: Span, message: impl Into<String>) -> Self {
        Self {
            kind: DiagnosticKind::Compile,
            severity: Severity::Error,
            span: Some(span),
            message: message.into(),
            incomplete_input: false,
            source: None,
            call_name: None,
        }
    }

    pub fn without_span(kind: DiagnosticKind, message: impl Into<String>) -> Self {
        Self {
            kind,
            severity: Severity::Error,
            span: None,
            message: message.into(),
            incomplete_input: false,
            source: None,
            call_name: None,
        }
    }

    pub fn with_incomplete_input(mut self) -> Self {
        self.incomplete_input = true;
        self
    }

    pub fn is_incomplete(&self) -> bool {
        self.incomplete_input
    }

    fn formatted_message(&self) -> String {
        match &self.call_name {
            Some(name) => format!("{name}: {}", self.message),
            None => self.message.clone(),
        }
    }

    pub fn render(&self, source: &SourceFile) -> String {
        let message = self.formatted_message();
        match self.span {
            Some(span) => {
                let (line, column) = source.line_col(span.start);
                format!("{}:{}:{}: {}", source.name(), line, column, message)
            }
            None => format!("{}: {}", source.name(), message),
        }
    }

    /// Render against `self.source` when tagged (a runtime error that
    /// crossed into a different module's function body), falling back
    /// to `fallback` (the file actually being evaluated) otherwise.
    pub fn render_with_fallback(&self, fallback: &SourceFile) -> String {
        match &self.source {
            Some(source) => self.render(source),
            None => self.render(fallback),
        }
    }
}

impl fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.span {
            Some(span) => write!(
                f,
                "{:?} {}..{}: {}",
                self.kind, span.start, span.end, self.message
            ),
            None => write!(f, "{:?}: {}", self.kind, self.message),
        }
    }
}

impl std::error::Error for Diagnostic {}

#[cfg(test)]
mod tests {
    use super::{Diagnostic, SourceFile, Span};

    #[test]
    fn line_col_is_one_based() {
        let file = SourceFile::new("test.kl", "1 + 2\n3 + 4\n");
        assert_eq!(file.line_col(0), (1, 1));
        assert_eq!(file.line_col(5), (1, 6));
        assert_eq!(file.line_col(6), (2, 1));
    }

    #[test]
    fn diagnostic_render_uses_source_positions() {
        let file = SourceFile::new("test.kl", "1 + 2");
        let diagnostic = Diagnostic::parse(Span::new(4, 5), "unexpected token");
        assert_eq!(diagnostic.render(&file), "test.kl:1:5: unexpected token");
    }
}
