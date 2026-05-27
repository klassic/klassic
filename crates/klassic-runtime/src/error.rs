//! Runtime error type shared between the evaluator and native runtime
//! call paths.
//!
//! The evaluator currently surfaces runtime failures through
//! `klassic_span::Diagnostic` directly. As the runtime crate grows, both
//! the evaluator's builtins and the future portable-backend C-ABI shims
//! will report through this type so the rendered messages stay aligned.

use klassic_span::{Diagnostic, Span};

/// A runtime failure with optional source-span fidelity. `RuntimeError`
/// is intentionally a thin wrapper around the kind / message / span
/// triple so it can be converted into a [`Diagnostic`] by either the
/// evaluator (which already uses `Diagnostic` everywhere) or by a future
/// native runtime call path that wants to write directly to stderr.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RuntimeError {
    pub kind: RuntimeErrorKind,
    pub message: String,
    pub span: Option<Span>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RuntimeErrorKind {
    /// Generic runtime failure (assert, index out of range, divide by zero, ...).
    Runtime,
    /// I/O failure (file read/write, stdin, ...).
    Io,
    /// Environment / process failure (env var missing, exit, ...).
    Environment,
}

impl RuntimeError {
    pub fn runtime(message: impl Into<String>) -> Self {
        Self {
            kind: RuntimeErrorKind::Runtime,
            message: message.into(),
            span: None,
        }
    }

    pub fn io(message: impl Into<String>) -> Self {
        Self {
            kind: RuntimeErrorKind::Io,
            message: message.into(),
            span: None,
        }
    }

    pub fn environment(message: impl Into<String>) -> Self {
        Self {
            kind: RuntimeErrorKind::Environment,
            message: message.into(),
            span: None,
        }
    }

    pub fn with_span(mut self, span: Span) -> Self {
        self.span = Some(span);
        self
    }

    pub fn into_diagnostic(self) -> Diagnostic {
        match self.span {
            Some(span) => Diagnostic::runtime(span, self.message),
            None => Diagnostic::without_span(klassic_span::DiagnosticKind::Runtime, self.message),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn runtime_constructor_records_kind_and_message() {
        let error = RuntimeError::runtime("boom");
        assert_eq!(error.kind, RuntimeErrorKind::Runtime);
        assert_eq!(error.message, "boom");
        assert_eq!(error.span, None);
    }

    #[test]
    fn with_span_attaches_span() {
        let error = RuntimeError::io("read failed").with_span(Span::new(3, 7));
        assert_eq!(error.kind, RuntimeErrorKind::Io);
        assert_eq!(error.span, Some(Span::new(3, 7)));
    }

    #[test]
    fn into_diagnostic_uses_runtime_kind_with_or_without_span() {
        let with_span = RuntimeError::environment("missing PATH")
            .with_span(Span::new(0, 4))
            .into_diagnostic();
        assert_eq!(with_span.kind, klassic_span::DiagnosticKind::Runtime);
        assert_eq!(with_span.span, Some(Span::new(0, 4)));

        let without_span = RuntimeError::runtime("no span").into_diagnostic();
        assert_eq!(without_span.kind, klassic_span::DiagnosticKind::Runtime);
        assert_eq!(without_span.span, None);
    }
}
