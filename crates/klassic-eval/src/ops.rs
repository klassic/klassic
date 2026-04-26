use klassic_span::{Diagnostic, Span};
use klassic_syntax::{BinaryOp, UnaryOp};

use crate::Value;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum NumericKind {
    Int,
    Long,
    Float,
    Double,
}

#[derive(Clone, Copy, Debug)]
enum NumericValue {
    Int(i64),
    Long(i64),
    Float(f32),
    Double(f64),
}

impl NumericValue {
    fn kind(self) -> NumericKind {
        match self {
            Self::Int(_) => NumericKind::Int,
            Self::Long(_) => NumericKind::Long,
            Self::Float(_) => NumericKind::Float,
            Self::Double(_) => NumericKind::Double,
        }
    }

    fn promote(self, kind: NumericKind) -> Self {
        match kind {
            NumericKind::Int => match self {
                Self::Int(value) => Self::Int(value),
                _ => unreachable!("promotion to Int should only happen for Int"),
            },
            NumericKind::Long => match self {
                Self::Int(value) => Self::Long(value),
                Self::Long(value) => Self::Long(value),
                _ => unreachable!("promotion to Long should only happen for integral types"),
            },
            NumericKind::Float => match self {
                Self::Int(value) => Self::Float(value as f32),
                Self::Long(value) => Self::Float(value as f32),
                Self::Float(value) => Self::Float(value),
                _ => unreachable!("promotion to Float should only happen for numeric types"),
            },
            NumericKind::Double => match self {
                Self::Int(value) => Self::Double(value as f64),
                Self::Long(value) => Self::Double(value as f64),
                Self::Float(value) => Self::Double(value as f64),
                Self::Double(value) => Self::Double(value),
            },
        }
    }

    fn as_f64(self) -> f64 {
        match self {
            Self::Int(value) => value as f64,
            Self::Long(value) => value as f64,
            Self::Float(value) => value as f64,
            Self::Double(value) => value,
        }
    }

    fn into_value(self) -> Value {
        match self {
            Self::Int(value) => Value::Int(value),
            Self::Long(value) => Value::Long(value),
            Self::Float(value) => Value::Float(value),
            Self::Double(value) => Value::Double(value),
        }
    }
}

fn numeric_value(value: &Value) -> Option<NumericValue> {
    match value {
        Value::Int(value) => Some(NumericValue::Int(*value)),
        Value::Long(value) => Some(NumericValue::Long(*value)),
        Value::Float(value) => Some(NumericValue::Float(*value)),
        Value::Double(value) => Some(NumericValue::Double(*value)),
        _ => None,
    }
}

fn promote_numeric_pair(
    lhs: NumericValue,
    rhs: NumericValue,
) -> (NumericValue, NumericValue, NumericKind) {
    use NumericKind::*;
    let rank = |kind| match kind {
        Int => 0,
        Long => 1,
        Float => 2,
        Double => 3,
    };
    let kind = if rank(lhs.kind()) >= rank(rhs.kind()) {
        lhs.kind()
    } else {
        rhs.kind()
    };
    (lhs.promote(kind), rhs.promote(kind), kind)
}

pub(crate) fn eval_unary(op: UnaryOp, value: Value, span: Span) -> Result<Value, Diagnostic> {
    match (op, value) {
        (UnaryOp::Plus, Value::Int(value)) => Ok(Value::Int(value)),
        (UnaryOp::Plus, Value::Long(value)) => Ok(Value::Long(value)),
        (UnaryOp::Plus, Value::Float(value)) => Ok(Value::Float(value)),
        (UnaryOp::Plus, Value::Double(value)) => Ok(Value::Double(value)),
        (UnaryOp::Minus, Value::Int(value)) => value
            .checked_neg()
            .map(Value::Int)
            .ok_or_else(|| Diagnostic::runtime(span, "integer overflow in unary negation")),
        (UnaryOp::Minus, Value::Long(value)) => value
            .checked_neg()
            .map(Value::Long)
            .ok_or_else(|| Diagnostic::runtime(span, "integer overflow in unary negation")),
        (UnaryOp::Minus, Value::Float(value)) => Ok(Value::Float(-value)),
        (UnaryOp::Minus, Value::Double(value)) => Ok(Value::Double(-value)),
        (UnaryOp::Not, Value::Bool(value)) => Ok(Value::Bool(!value)),
        (UnaryOp::Plus, _) | (UnaryOp::Minus, _) => Err(Diagnostic::runtime(
            span,
            "numeric unary operator expected a number",
        )),
        (UnaryOp::Not, _) => Err(Diagnostic::runtime(span, "`!` expected a boolean")),
    }
}

pub(crate) fn eval_binary(
    op: BinaryOp,
    lhs: Value,
    rhs: Value,
    span: Span,
) -> Result<Value, Diagnostic> {
    match op {
        BinaryOp::Add => eval_add(lhs, rhs, span),
        BinaryOp::Subtract => eval_subtract(lhs, rhs, span),
        BinaryOp::Multiply => eval_multiply(lhs, rhs, span),
        BinaryOp::Divide => eval_divide(lhs, rhs, span),
        BinaryOp::Less | BinaryOp::LessEqual | BinaryOp::Greater | BinaryOp::GreaterEqual => {
            eval_comparison(op, lhs, rhs, span)
        }
        BinaryOp::Equal | BinaryOp::NotEqual => {
            let equal = lhs == rhs;
            if matches!(op, BinaryOp::Equal) {
                Ok(Value::Bool(equal))
            } else {
                Ok(Value::Bool(!equal))
            }
        }
        BinaryOp::BitAnd | BinaryOp::BitOr | BinaryOp::BitXor => eval_bitwise(op, lhs, rhs, span),
        BinaryOp::LogicalAnd | BinaryOp::LogicalOr => unreachable!("handled separately"),
    }
}

fn eval_add(lhs: Value, rhs: Value, span: Span) -> Result<Value, Diagnostic> {
    match (lhs, rhs) {
        (Value::String(lhs), Value::String(rhs)) => Ok(Value::String(lhs + &rhs)),
        (Value::String(lhs), rhs) => Ok(Value::String(lhs + &rhs.to_string())),
        (lhs, Value::String(rhs)) => Ok(Value::String(lhs.to_string() + &rhs)),
        (lhs, rhs) => match (numeric_value(&lhs), numeric_value(&rhs)) {
            (Some(lhs), Some(rhs)) => {
                let (lhs, rhs, kind) = promote_numeric_pair(lhs, rhs);
                let value = match (lhs, rhs, kind) {
                    (NumericValue::Int(lhs), NumericValue::Int(rhs), NumericKind::Int) => lhs
                        .checked_add(rhs)
                        .map(NumericValue::Int)
                        .ok_or_else(|| Diagnostic::runtime(span, "integer overflow"))?,
                    (NumericValue::Long(lhs), NumericValue::Long(rhs), NumericKind::Long) => lhs
                        .checked_add(rhs)
                        .map(NumericValue::Long)
                        .ok_or_else(|| Diagnostic::runtime(span, "integer overflow"))?,
                    (NumericValue::Float(lhs), NumericValue::Float(rhs), NumericKind::Float) => {
                        NumericValue::Float(lhs + rhs)
                    }
                    (NumericValue::Double(lhs), NumericValue::Double(rhs), NumericKind::Double) => {
                        NumericValue::Double(lhs + rhs)
                    }
                    _ => unreachable!("promoted numeric add should align kinds"),
                };
                Ok(value.into_value())
            }
            _ => Err(Diagnostic::runtime(
                span,
                "`+` expects numbers or strings of the same shape",
            )),
        },
    }
}

fn eval_subtract(lhs: Value, rhs: Value, span: Span) -> Result<Value, Diagnostic> {
    match (numeric_value(&lhs), numeric_value(&rhs)) {
        (Some(lhs), Some(rhs)) => {
            let (lhs, rhs, kind) = promote_numeric_pair(lhs, rhs);
            let value = match (lhs, rhs, kind) {
                (NumericValue::Int(lhs), NumericValue::Int(rhs), NumericKind::Int) => lhs
                    .checked_sub(rhs)
                    .map(NumericValue::Int)
                    .ok_or_else(|| Diagnostic::runtime(span, "integer overflow"))?,
                (NumericValue::Long(lhs), NumericValue::Long(rhs), NumericKind::Long) => lhs
                    .checked_sub(rhs)
                    .map(NumericValue::Long)
                    .ok_or_else(|| Diagnostic::runtime(span, "integer overflow"))?,
                (NumericValue::Float(lhs), NumericValue::Float(rhs), NumericKind::Float) => {
                    NumericValue::Float(lhs - rhs)
                }
                (NumericValue::Double(lhs), NumericValue::Double(rhs), NumericKind::Double) => {
                    NumericValue::Double(lhs - rhs)
                }
                _ => unreachable!("promoted numeric subtract should align kinds"),
            };
            Ok(value.into_value())
        }
        _ => Err(Diagnostic::runtime(span, "`-` expects numbers")),
    }
}

fn eval_multiply(lhs: Value, rhs: Value, span: Span) -> Result<Value, Diagnostic> {
    match (numeric_value(&lhs), numeric_value(&rhs)) {
        (Some(lhs), Some(rhs)) => {
            let (lhs, rhs, kind) = promote_numeric_pair(lhs, rhs);
            let value = match (lhs, rhs, kind) {
                (NumericValue::Int(lhs), NumericValue::Int(rhs), NumericKind::Int) => lhs
                    .checked_mul(rhs)
                    .map(NumericValue::Int)
                    .ok_or_else(|| Diagnostic::runtime(span, "integer overflow"))?,
                (NumericValue::Long(lhs), NumericValue::Long(rhs), NumericKind::Long) => lhs
                    .checked_mul(rhs)
                    .map(NumericValue::Long)
                    .ok_or_else(|| Diagnostic::runtime(span, "integer overflow"))?,
                (NumericValue::Float(lhs), NumericValue::Float(rhs), NumericKind::Float) => {
                    NumericValue::Float(lhs * rhs)
                }
                (NumericValue::Double(lhs), NumericValue::Double(rhs), NumericKind::Double) => {
                    NumericValue::Double(lhs * rhs)
                }
                _ => unreachable!("promoted numeric multiply should align kinds"),
            };
            Ok(value.into_value())
        }
        _ => Err(Diagnostic::runtime(span, "`*` expects numbers")),
    }
}

fn eval_divide(lhs: Value, rhs: Value, span: Span) -> Result<Value, Diagnostic> {
    match (numeric_value(&lhs), numeric_value(&rhs)) {
        (Some(lhs), Some(rhs)) => {
            let (lhs, rhs, kind) = promote_numeric_pair(lhs, rhs);
            let value = match (lhs, rhs, kind) {
                (_, NumericValue::Int(0), NumericKind::Int)
                | (_, NumericValue::Long(0), NumericKind::Long)
                | (_, NumericValue::Float(0.0), NumericKind::Float)
                | (_, NumericValue::Double(0.0), NumericKind::Double) => {
                    return Err(Diagnostic::runtime(span, "division by zero"));
                }
                (NumericValue::Int(lhs), NumericValue::Int(rhs), NumericKind::Int) => lhs
                    .checked_div(rhs)
                    .map(NumericValue::Int)
                    .ok_or_else(|| Diagnostic::runtime(span, "integer overflow"))?,
                (NumericValue::Long(lhs), NumericValue::Long(rhs), NumericKind::Long) => lhs
                    .checked_div(rhs)
                    .map(NumericValue::Long)
                    .ok_or_else(|| Diagnostic::runtime(span, "integer overflow"))?,
                (NumericValue::Float(lhs), NumericValue::Float(rhs), NumericKind::Float) => {
                    NumericValue::Float(lhs / rhs)
                }
                (NumericValue::Double(lhs), NumericValue::Double(rhs), NumericKind::Double) => {
                    NumericValue::Double(lhs / rhs)
                }
                _ => unreachable!("promoted numeric divide should align kinds"),
            };
            Ok(value.into_value())
        }
        _ => Err(Diagnostic::runtime(span, "`/` expects numbers")),
    }
}

fn eval_comparison(op: BinaryOp, lhs: Value, rhs: Value, span: Span) -> Result<Value, Diagnostic> {
    let (lhs, rhs) = match (numeric_value(&lhs), numeric_value(&rhs)) {
        (Some(lhs), Some(rhs)) => {
            let (lhs, rhs, _) = promote_numeric_pair(lhs, rhs);
            (lhs.as_f64(), rhs.as_f64())
        }
        _ => return Err(Diagnostic::runtime(span, "comparison expects numbers")),
    };
    let result = match op {
        BinaryOp::Less => lhs < rhs,
        BinaryOp::LessEqual => lhs <= rhs,
        BinaryOp::Greater => lhs > rhs,
        BinaryOp::GreaterEqual => lhs >= rhs,
        _ => unreachable!("comparison operator expected"),
    };
    Ok(Value::Bool(result))
}

fn eval_bitwise(op: BinaryOp, lhs: Value, rhs: Value, span: Span) -> Result<Value, Diagnostic> {
    let (lhs, rhs) = match (lhs, rhs) {
        (Value::Int(lhs), Value::Int(rhs)) => (lhs, rhs),
        (Value::Long(lhs), Value::Long(rhs)) => {
            let value = match op {
                BinaryOp::BitAnd => lhs & rhs,
                BinaryOp::BitOr => lhs | rhs,
                BinaryOp::BitXor => lhs ^ rhs,
                _ => unreachable!("bitwise operator expected"),
            };
            return Ok(Value::Long(value));
        }
        _ => {
            return Err(Diagnostic::runtime(
                span,
                "bitwise operators expect integers",
            ));
        }
    };
    let value = match op {
        BinaryOp::BitAnd => lhs & rhs,
        BinaryOp::BitOr => lhs | rhs,
        BinaryOp::BitXor => lhs ^ rhs,
        _ => unreachable!("bitwise operator expected"),
    };
    Ok(Value::Int(value))
}
