use std::collections::HashMap;

use klassic_syntax::{TypeAnnotation, TypeClassConstraint};
use klassic_types::KnownType;

use crate::Value;

use super::{RecordSchema, resolve_record};

pub(crate) fn dynamic_type_name(value: &Value) -> Option<&str> {
    match value {
        Value::Int(_) => Some("Int"),
        Value::Long(_) => Some("Long"),
        Value::Float(_) => Some("Float"),
        Value::Double(_) => Some("Double"),
        Value::Bool(_) => Some("Boolean"),
        Value::String(_) => Some("String"),
        Value::List(_) => Some("List"),
        Value::Map(_) => Some("Map"),
        Value::Set(_) => Some("Set"),
        Value::Record { name, .. } => Some(name.as_str()),
        Value::Unit => Some("Unit"),
        Value::Enum { enum_name, .. } => Some(enum_name.as_str()),
        Value::Null
        | Value::BuiltinFunction(_)
        | Value::Function(_)
        | Value::TypeClassMethod(_)
        | Value::BoundTypeClassMethod { .. }
        | Value::EnumConstructor { .. } => None,
    }
}

pub(crate) fn infer_constraint_substitutions(
    param_annotations: &[Option<TypeAnnotation>],
    argument_values: &[Value],
) -> HashMap<String, KnownType> {
    let mut substitutions = HashMap::new();
    for (annotation, value) in param_annotations.iter().zip(argument_values.iter()) {
        if let Some(annotation) = annotation {
            infer_known_type_bindings(
                &annotation.text,
                known_type_from_value(value),
                &mut substitutions,
            );
        }
    }
    substitutions
}

pub(crate) fn constraint_runtime_type_name(
    constraint: &TypeClassConstraint,
    substitutions: &HashMap<String, KnownType>,
) -> Option<String> {
    constraint
        .arguments
        .first()
        .and_then(|argument| resolve_runtime_annotation_name(&argument.text, substitutions))
}

fn resolve_runtime_annotation_name(
    annotation: &str,
    substitutions: &HashMap<String, KnownType>,
) -> Option<String> {
    let annotation = annotation.trim();
    if annotation.is_empty() || annotation == "*" {
        return None;
    }
    if annotation.starts_with('\'') && !annotation.contains('<') {
        return substitutions.get(annotation).and_then(known_type_name);
    }
    if annotation.starts_with("List<") {
        return Some("List".to_string());
    }
    if annotation.starts_with("Set<") {
        return Some("Set".to_string());
    }
    if annotation.starts_with("Map<") {
        return Some("Map".to_string());
    }
    if let Some(record) = annotation.strip_prefix('#') {
        if let Some((name, _)) = parse_named_generic_args(record) {
            return Some(name);
        }
        return Some(record.trim().to_string());
    }
    if let Some((head, _)) = parse_runtime_type_application(annotation) {
        if head.starts_with('\'') {
            return substitutions.get(&head).and_then(known_type_name);
        }
        return Some(head);
    }
    Some(annotation.to_string())
}

fn known_type_name(known: &KnownType) -> Option<String> {
    match known {
        KnownType::Byte => Some("Byte".to_string()),
        KnownType::Short => Some("Short".to_string()),
        KnownType::Int => Some("Int".to_string()),
        KnownType::Long => Some("Long".to_string()),
        KnownType::Float => Some("Float".to_string()),
        KnownType::Double => Some("Double".to_string()),
        KnownType::Bool => Some("Boolean".to_string()),
        KnownType::String => Some("String".to_string()),
        KnownType::Unit => Some("Unit".to_string()),
        KnownType::Dynamic => None,
        KnownType::Null => Some("null".to_string()),
        KnownType::List(_) => Some("List".to_string()),
        KnownType::Map(_, _) => Some("Map".to_string()),
        KnownType::Set(_) => Some("Set".to_string()),
        KnownType::Function => None,
        KnownType::Record(name, _) => Some(name.clone()),
        KnownType::StructuralRecord(_) => None,
        KnownType::Named(name) => Some(name.clone()),
    }
}

pub(crate) fn known_type_from_value(value: &Value) -> KnownType {
    match value {
        Value::Int(_) => KnownType::Int,
        Value::Long(_) => KnownType::Long,
        Value::Float(_) => KnownType::Float,
        Value::Double(_) => KnownType::Double,
        Value::Bool(_) => KnownType::Bool,
        Value::String(_) => KnownType::String,
        Value::Null => KnownType::Null,
        Value::Unit => KnownType::Unit,
        Value::List(values) => {
            let inner = values
                .iter()
                .map(known_type_from_value)
                .reduce(merge_known_types)
                .unwrap_or(KnownType::Dynamic);
            KnownType::List(Box::new(inner))
        }
        Value::Map(entries) => {
            let key = entries
                .iter()
                .map(|(key, _)| known_type_from_value(key))
                .reduce(merge_known_types)
                .unwrap_or(KnownType::Dynamic);
            let value = entries
                .iter()
                .map(|(_, value)| known_type_from_value(value))
                .reduce(merge_known_types)
                .unwrap_or(KnownType::Dynamic);
            KnownType::Map(Box::new(key), Box::new(value))
        }
        Value::Set(values) => {
            let inner = values
                .iter()
                .map(known_type_from_value)
                .reduce(merge_known_types)
                .unwrap_or(KnownType::Dynamic);
            KnownType::Set(Box::new(inner))
        }
        Value::Record { name, fields } => {
            if name.is_empty() {
                KnownType::StructuralRecord(
                    fields
                        .iter()
                        .map(|(name, value)| (name.clone(), known_type_from_value(value)))
                        .collect(),
                )
            } else {
                let type_args = resolve_record(name)
                    .map(|schema| infer_record_type_args(&schema, fields))
                    .unwrap_or_default();
                KnownType::Record(name.clone(), type_args)
            }
        }
        Value::Enum { .. } => KnownType::Dynamic,
        Value::BuiltinFunction(_)
        | Value::Function(_)
        | Value::TypeClassMethod(_)
        | Value::BoundTypeClassMethod { .. }
        | Value::EnumConstructor { .. } => KnownType::Function,
    }
}

fn merge_known_types(lhs: KnownType, rhs: KnownType) -> KnownType {
    match (lhs, rhs) {
        (KnownType::StructuralRecord(lhs_fields), KnownType::StructuralRecord(rhs_fields))
            if lhs_fields.len() == rhs_fields.len()
                && lhs_fields
                    .iter()
                    .zip(rhs_fields.iter())
                    .all(|((lhs_name, _), (rhs_name, _))| lhs_name == rhs_name) =>
        {
            KnownType::StructuralRecord(
                lhs_fields
                    .into_iter()
                    .zip(rhs_fields)
                    .map(|((name, lhs_ty), (_, rhs_ty))| (name, merge_known_types(lhs_ty, rhs_ty)))
                    .collect(),
            )
        }
        (lhs, rhs) if lhs == rhs => lhs,
        _ => KnownType::Dynamic,
    }
}

fn infer_record_type_args(schema: &RecordSchema, fields: &[(String, Value)]) -> Vec<KnownType> {
    let mut substitutions = HashMap::new();
    for ((_, value), field) in fields.iter().zip(schema.fields.iter()) {
        if let Some(annotation) = &field.annotation {
            infer_known_type_bindings(annotation, known_type_from_value(value), &mut substitutions);
        }
    }
    schema
        .type_params
        .iter()
        .map(|param| {
            substitutions
                .get(param)
                .cloned()
                .unwrap_or(KnownType::Dynamic)
        })
        .collect()
}

fn infer_known_type_bindings(
    annotation: &str,
    actual: KnownType,
    substitutions: &mut HashMap<String, KnownType>,
) {
    let annotation = annotation.trim();
    if annotation.is_empty() || annotation == "*" {
        return;
    }
    if let Some((head, args)) = parse_runtime_type_application(annotation)
        && head.starts_with('\'')
    {
        if let Some(actual_head) = known_type_head(&actual) {
            substitutions
                .entry(head.clone())
                .and_modify(|known| *known = merge_known_types(known.clone(), actual_head.clone()))
                .or_insert(actual_head);
        }
        for (expected, actual_arg) in args.into_iter().zip(known_type_args(actual)) {
            infer_known_type_bindings(&expected, actual_arg, substitutions);
        }
        return;
    }
    if annotation.starts_with('\'') {
        substitutions
            .entry(annotation.to_string())
            .and_modify(|known| *known = merge_known_types(known.clone(), actual.clone()))
            .or_insert(actual);
        return;
    }
    if let Some(inner) = annotation
        .strip_prefix("List<")
        .and_then(|rest| rest.strip_suffix('>'))
    {
        if let KnownType::List(inner_actual) = actual {
            infer_known_type_bindings(inner, *inner_actual, substitutions);
        }
        return;
    }
    if let Some(inner) = annotation
        .strip_prefix("Set<")
        .and_then(|rest| rest.strip_suffix('>'))
    {
        if let KnownType::Set(inner_actual) = actual {
            infer_known_type_bindings(inner, *inner_actual, substitutions);
        }
        return;
    }
    if let Some(inner) = annotation
        .strip_prefix("Map<")
        .and_then(|rest| rest.strip_suffix('>'))
    {
        let parts = split_top_level(inner, ',');
        if let [key, value] = parts.as_slice()
            && let KnownType::Map(actual_key, actual_value) = actual
        {
            infer_known_type_bindings(key, *actual_key, substitutions);
            infer_known_type_bindings(value, *actual_value, substitutions);
        }
        return;
    }
    if let Some(record) = annotation.strip_prefix('#')
        && let Some((name, expected_args)) = parse_named_generic_args(record)
        && let KnownType::Record(actual_name, actual_args) = actual
        && actual_name == name
    {
        for (expected, actual) in expected_args.into_iter().zip(actual_args) {
            infer_known_type_bindings(&expected, actual, substitutions);
        }
    }
}

fn known_type_head(known: &KnownType) -> Option<KnownType> {
    match known {
        KnownType::List(_) => Some(KnownType::Named("List".to_string())),
        KnownType::Set(_) => Some(KnownType::Named("Set".to_string())),
        KnownType::Map(_, _) => Some(KnownType::Named("Map".to_string())),
        KnownType::Record(name, _) => Some(KnownType::Named(name.clone())),
        KnownType::Named(name) => Some(KnownType::Named(name.clone())),
        _ => None,
    }
}

fn known_type_args(known: KnownType) -> Vec<KnownType> {
    match known {
        KnownType::List(inner) => vec![*inner],
        KnownType::Set(inner) => vec![*inner],
        KnownType::Map(key, value) => vec![*key, *value],
        KnownType::Record(_, args) => args,
        _ => Vec::new(),
    }
}

fn parse_named_generic_args(text: &str) -> Option<(String, Vec<String>)> {
    let name_end = text.find('<')?;
    let name = text[..name_end].trim().to_string();
    let inner = text[name_end + 1..].strip_suffix('>')?;
    Some((name, split_top_level(inner, ',')))
}

fn parse_runtime_type_application(text: &str) -> Option<(String, Vec<String>)> {
    let name_end = text.find('<')?;
    let head = text[..name_end].trim().to_string();
    let inner = text[name_end + 1..].strip_suffix('>')?;
    Some((head, split_top_level(inner, ',')))
}

fn split_top_level(text: &str, separator: char) -> Vec<String> {
    let mut parts = Vec::new();
    let mut start = 0usize;
    let mut paren = 0usize;
    let mut angle = 0usize;
    for (index, ch) in text.char_indices() {
        match ch {
            '(' => paren += 1,
            ')' => paren = paren.saturating_sub(1),
            '<' => angle += 1,
            '>' => angle = angle.saturating_sub(1),
            _ if ch == separator && paren == 0 && angle == 0 => {
                parts.push(text[start..index].trim().to_string());
                start = index + ch.len_utf8();
            }
            _ => {}
        }
    }
    if start < text.len() {
        parts.push(text[start..].trim().to_string());
    }
    parts.into_iter().filter(|part| !part.is_empty()).collect()
}
