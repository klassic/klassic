use klassic_span::{Diagnostic, Span};

use crate::Value;

pub(crate) fn ensure_arity(
    name: &str,
    arguments: &[Value],
    expected: usize,
    span: Span,
) -> Result<(), Diagnostic> {
    if arguments.len() == expected {
        Ok(())
    } else {
        Err(Diagnostic::runtime(
            span,
            format!(
                "{name} expects {expected} arguments but got {}",
                arguments.len()
            ),
        ))
    }
}

pub(crate) fn expect_string<'a>(
    value: &'a Value,
    name: &str,
    span: Span,
) -> Result<&'a str, Diagnostic> {
    match value {
        Value::String(text) => Ok(text.as_str()),
        _ => Err(Diagnostic::runtime(
            span,
            format!("{name} expects a string"),
        )),
    }
}

/// An index argument for the clamping string slicers (`substring` /
/// `at`): negatives clamp to 0, matching `klassic_rt` and both native
/// backends (issue #434 — the evaluator used to reject them).
pub(crate) fn expect_clamped_index(
    value: &Value,
    name: &str,
    span: Span,
) -> Result<usize, Diagnostic> {
    match value {
        Value::Int(number) => Ok((*number).max(0) as usize),
        Value::Long(number) => Ok((*number).max(0) as usize),
        _ => Err(Diagnostic::runtime(
            span,
            format!("{name} expects an integer"),
        )),
    }
}

pub(crate) fn expect_non_negative_int(
    value: &Value,
    name: &str,
    span: Span,
) -> Result<usize, Diagnostic> {
    match value {
        Value::Int(number) if *number >= 0 => Ok(*number as usize),
        Value::Long(number) if *number >= 0 => Ok(*number as usize),
        Value::Int(_) | Value::Long(_) => Err(Diagnostic::runtime(
            span,
            format!("{name} expects a non-negative integer index"),
        )),
        _ => Err(Diagnostic::runtime(
            span,
            format!("{name} expects an integer"),
        )),
    }
}

pub(crate) fn expect_list<'a>(
    value: &'a Value,
    name: &str,
    span: Span,
) -> Result<&'a [Value], Diagnostic> {
    match value {
        Value::List(values) => Ok(values.as_slice()),
        _ => Err(Diagnostic::runtime(span, format!("{name} expects a list"))),
    }
}

pub(crate) fn expect_map<'a>(
    value: &'a Value,
    name: &str,
    span: Span,
) -> Result<&'a [(Value, Value)], Diagnostic> {
    match value {
        Value::Map(entries) => Ok(entries.as_slice()),
        _ => Err(Diagnostic::runtime(span, format!("{name} expects a map"))),
    }
}

pub(crate) fn expect_set<'a>(
    value: &'a Value,
    name: &str,
    span: Span,
) -> Result<&'a [Value], Diagnostic> {
    match value {
        Value::Set(values) => Ok(values.as_slice()),
        _ => Err(Diagnostic::runtime(span, format!("{name} expects a set"))),
    }
}

pub(crate) fn clamp_index(index: usize, len: usize) -> usize {
    index.min(len)
}

pub(crate) fn simple_regex_is_match(input: &str, pattern: &str) -> bool {
    match pattern {
        ".*" => true,
        "[0-9]+" => !input.is_empty() && input.chars().all(|ch| ch.is_ascii_digit()),
        "[0-9]" => input.chars().count() == 1 && input.chars().all(|ch| ch.is_ascii_digit()),
        _ => input == pattern,
    }
}

pub(crate) fn simple_regex_replace_all(input: &str, pattern: &str, replacement: &str) -> String {
    match pattern {
        "[0-9]" => input
            .chars()
            .map(|ch| {
                if ch.is_ascii_digit() {
                    replacement.to_string()
                } else {
                    ch.to_string()
                }
            })
            .collect(),
        _ => input.replace(pattern, replacement),
    }
}
