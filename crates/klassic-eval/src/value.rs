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
    /// Algebraic data type instance produced by an `enum` variant
    /// constructor. The enum name comes from the declaration, the
    /// variant name from the constructor that built this value, and
    /// fields hold each named parameter in declaration order.
    ///
    /// `fields` is `Rc`-shared (mirroring `Function`/`BuiltinFunction`
    /// below) rather than plain `Vec` so that cloning a `Value::Enum`
    /// is O(1): a recursive enum like a cons-list stores its tail as
    /// another `Value::Enum` nested inside `fields`, and a naive
    /// per-field clone would recurse one native Rust stack frame per
    /// list element, overflowing the host stack a few thousand
    /// elements deep on any code path that clones the value (pattern
    /// binding a tail variable in `match`, reading a variable out of
    /// the environment, passing it as an argument, ...). Bumping a
    /// refcount instead makes clone depth-independent; see
    /// `EnumFields`'s `Drop` impl below for the matching fix on the
    /// teardown side.
    Enum {
        enum_name: String,
        variant: String,
        fields: Rc<EnumFields>,
    },
    /// Reference to an `enum` variant constructor — produced when the
    /// user writes the variant name without arguments. Calling the
    /// constructor with the right number of arguments yields
    /// `Value::Enum`.
    EnumConstructor {
        enum_name: String,
        variant: String,
        param_names: Vec<String>,
    },
    TypeClassMethod(String),
    BoundTypeClassMethod {
        name: String,
        fallback: Box<Value>,
    },
    BuiltinFunction(Rc<BuiltinFunctionValue>),
    Function(Rc<FunctionValue>),
}

/// The field list backing a `Value::Enum`, always accessed behind an
/// `Rc` (see the doc comment on `Value::Enum`). This is a thin
/// newtype around `Vec<(String, Value)>` — rather than using the
/// `Vec` directly — purely so it can carry its own `Drop` impl below.
/// `Value` itself deliberately does *not* implement `Drop`: countless
/// call sites throughout the evaluator match an owned `Value` by
/// value and move a variant's payload out (`Value::Function(f) => ...`
/// and friends), which the language forbids for a type that
/// implements `Drop` (E0509). Hanging the custom teardown off this
/// separate, rarely-pattern-matched type sidesteps that restriction
/// entirely.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct EnumFields(pub(crate) Vec<(String, Value)>);

impl EnumFields {
    pub(crate) fn new(fields: Vec<(String, Value)>) -> Self {
        Self(fields)
    }
}

impl std::ops::Deref for EnumFields {
    type Target = Vec<(String, Value)>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Drop for EnumFields {
    fn drop(&mut self) {
        // A recursive enum (e.g. a cons-style list built by repeated
        // `Cons(head, tail)` calls) nests one `Value::Enum` inside
        // the next via `Rc<EnumFields>`. Left to the default,
        // compiler-generated drop glue, tearing down the outermost
        // `EnumFields` would drop its `Value`s, and if one of those
        // turns out to be the last owner of *another* `Rc<EnumFields>`
        // that would recurse straight back into this same
        // `Drop::drop` — one native Rust stack frame per level of
        // nesting, which overflows the host stack a few hundred
        // thousand elements deep. Flatten the whole reachable chain
        // onto an explicit, heap-allocated worklist first instead, so
        // the actual teardown is an iterative loop.
        let mut pending = std::mem::take(&mut self.0);
        let mut index = 0;
        while index < pending.len() {
            let reclaimed = if let Value::Enum { fields, .. } = &mut pending[index].1
                && let Some(owned) = Rc::get_mut(fields)
            {
                Some(std::mem::take(&mut owned.0))
            } else {
                None
            };
            if let Some(mut reclaimed) = reclaimed {
                pending.append(&mut reclaimed);
            }
            index += 1;
        }
        // `pending` drops here. Every `Value::Enum` we could uniquely
        // reclaim above (`Rc::get_mut` returned `Some`) now has empty
        // `fields`, so the ordinary, per-item drop glue that runs as
        // this `Vec` is torn down does O(1) work per entry instead of
        // recursing again. Entries whose `fields` were still shared
        // elsewhere (`Rc::get_mut` returned `None`) merely have their
        // refcount decremented here, which is also O(1); whoever
        // holds the other reference is responsible for the eventual
        // real teardown, using this same iterative path.
    }
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
            (
                Self::Enum {
                    enum_name: l_en,
                    variant: l_v,
                    fields: l_f,
                },
                Self::Enum {
                    enum_name: r_en,
                    variant: r_v,
                    fields: r_f,
                },
            ) => l_en == r_en && l_v == r_v && l_f == r_f,
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
            Self::Enum {
                enum_name: _,
                variant,
                fields,
            } => {
                if fields.is_empty() {
                    write!(f, "{variant}")
                } else {
                    write!(f, "{variant}(")?;
                    for (index, (_, value)) in fields.iter().enumerate() {
                        if index > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{value}")?;
                    }
                    write!(f, ")")
                }
            }
            Self::EnumConstructor {
                enum_name: _,
                variant,
                ..
            } => write!(f, "<enum-constructor:{variant}>"),
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
