use std::fmt;
use std::rc::Rc;

use klassic_syntax::{Expr, TypeAnnotation, TypeClassConstraint};

use super::Environment;

#[derive(Clone, Debug)]
pub enum Value {
    Int(i64),
    Long(i64),
    Float(f32),
    Double(f64),
    Bool(bool),
    String(String),
    Null,
    Unit,
    List(Vec<Value>),
    Map(Vec<(Value, Value)>),
    Set(Vec<Value>),
    Record {
        name: String,
        fields: Vec<(String, Value)>,
    },
    TypeClassMethod(String),
    BoundTypeClassMethod {
        name: String,
        fallback: Box<Value>,
    },
    BuiltinFunction(Rc<BuiltinFunctionValue>),
    Function(Rc<FunctionValue>),
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Int(lhs), Self::Int(rhs)) => lhs == rhs,
            (Self::Long(lhs), Self::Long(rhs)) => lhs == rhs,
            (Self::Float(lhs), Self::Float(rhs)) => lhs == rhs,
            (Self::Double(lhs), Self::Double(rhs)) => lhs == rhs,
            (Self::Bool(lhs), Self::Bool(rhs)) => lhs == rhs,
            (Self::String(lhs), Self::String(rhs)) => lhs == rhs,
            (Self::Null, Self::Null) => true,
            (Self::Unit, Self::Unit) => true,
            (Self::List(lhs), Self::List(rhs)) => lhs == rhs,
            (Self::Map(lhs), Self::Map(rhs)) => lhs == rhs,
            (Self::Set(lhs), Self::Set(rhs)) => lhs == rhs,
            (
                Self::Record {
                    name: lhs_name,
                    fields: lhs_fields,
                },
                Self::Record {
                    name: rhs_name,
                    fields: rhs_fields,
                },
            ) => lhs_name == rhs_name && lhs_fields == rhs_fields,
            (Self::Int(lhs), Self::Double(rhs)) => (*lhs as f64) == *rhs,
            (Self::Double(lhs), Self::Int(rhs)) => *lhs == (*rhs as f64),
            (Self::Int(lhs), Self::Long(rhs)) => *lhs == *rhs,
            (Self::Long(lhs), Self::Int(rhs)) => *lhs == *rhs,
            (Self::Int(lhs), Self::Float(rhs)) => (*lhs as f32) == *rhs,
            (Self::Float(lhs), Self::Int(rhs)) => *lhs == (*rhs as f32),
            (Self::Long(lhs), Self::Double(rhs)) => (*lhs as f64) == *rhs,
            (Self::Double(lhs), Self::Long(rhs)) => *lhs == (*rhs as f64),
            (Self::Long(lhs), Self::Float(rhs)) => (*lhs as f32) == *rhs,
            (Self::Float(lhs), Self::Long(rhs)) => *lhs == (*rhs as f32),
            (Self::Float(lhs), Self::Double(rhs)) => (*lhs as f64) == *rhs,
            (Self::Double(lhs), Self::Float(rhs)) => *lhs == (*rhs as f64),
            _ => false,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(value) => write!(f, "{value}"),
            Self::Long(value) => write!(f, "{value}"),
            Self::Float(value) => {
                if value.fract() == 0.0 {
                    write!(f, "{value:.1}")
                } else {
                    write!(f, "{value}")
                }
            }
            Self::Double(value) => {
                if value.fract() == 0.0 {
                    write!(f, "{value:.1}")
                } else {
                    write!(f, "{value}")
                }
            }
            Self::Bool(value) => write!(f, "{value}"),
            Self::String(value) => write!(f, "{value}"),
            Self::Null => write!(f, "null"),
            Self::Unit => write!(f, "()"),
            Self::List(values) => {
                write!(f, "[")?;
                for (index, value) in values.iter().enumerate() {
                    if index > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{value}")?;
                }
                write!(f, "]")
            }
            Self::Map(entries) => {
                write!(f, "%[")?;
                for (index, (key, value)) in entries.iter().enumerate() {
                    if index > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{key}: {value}")?;
                }
                write!(f, "]")
            }
            Self::Set(values) => {
                write!(f, "%(")?;
                for (index, value) in values.iter().enumerate() {
                    if index > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{value}")?;
                }
                write!(f, ")")
            }
            Self::Record { name, fields } => {
                write!(f, "#{name}(")?;
                for (index, (_, value)) in fields.iter().enumerate() {
                    if index > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{value}")?;
                }
                write!(f, ")")
            }
            Self::TypeClassMethod(name) => write!(f, "<typeclass-method:{name}>"),
            Self::BoundTypeClassMethod { name, .. } => {
                write!(f, "<bound-typeclass-method:{name}>")
            }
            Self::BuiltinFunction(function) => write!(f, "<builtin:{}>", function.name),
            Self::Function(_) => write!(f, "<function>"),
        }
    }
}

#[derive(Clone, Debug)]
pub struct BuiltinFunctionValue {
    pub(crate) name: &'static str,
    pub(crate) bound_args: Vec<Value>,
}

#[derive(Clone, Debug)]
pub struct FunctionValue {
    pub(crate) params: Vec<String>,
    pub(crate) param_annotations: Vec<Option<TypeAnnotation>>,
    pub(crate) constraints: Vec<TypeClassConstraint>,
    pub(crate) body: Expr,
    pub(crate) env: Environment,
}
