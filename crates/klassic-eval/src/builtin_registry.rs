use crate::Value;

pub(crate) fn builtin_name(name: &str) -> Option<&'static str> {
    klassic_runtime::builtins::canonical_name(name)
}

pub(crate) fn builtin_arity(name: &str) -> Option<usize> {
    klassic_runtime::builtins::arity(name)
}

pub(crate) fn value_method_builtin_name(value: &Value, field: &str) -> Option<&'static str> {
    match value {
        Value::Int(_)
        | Value::Long(_)
        | Value::Float(_)
        | Value::Double(_)
        | Value::Bool(_)
        | Value::Unit
        | Value::Enum { .. }
        | Value::Record { .. } => match field {
            "toString" => Some("toString"),
            _ => None,
        },
        Value::String(_) => match field {
            "toString" => Some("toString"),
            "substring" => Some("substring"),
            "at" => Some("at"),
            "matches" => Some("matches"),
            "split" => Some("split"),
            "trim" => Some("trim"),
            "trimLeft" => Some("trimLeft"),
            "trimRight" => Some("trimRight"),
            "replace" => Some("replace"),
            "replaceAll" => Some("replaceAll"),
            "toLowerCase" => Some("toLowerCase"),
            "toUpperCase" => Some("toUpperCase"),
            "startsWith" => Some("startsWith"),
            "endsWith" => Some("endsWith"),
            "contains" => Some("contains"),
            "isEmpty" | "isEmptyString" => Some("isEmptyString"),
            "indexOf" => Some("indexOf"),
            "lastIndexOf" => Some("lastIndexOf"),
            "length" => Some("length"),
            "repeat" => Some("repeat"),
            "reverse" => Some("reverse"),
            "padStart" => Some("padStart"),
            "padEnd" => Some("padEnd"),
            "parseIntOr" => Some("String#parseIntOr"),
            "parseDoubleOr" => Some("String#parseDoubleOr"),
            "isInteger" => Some("String#isInt"),
            "isDouble" => Some("String#isDouble"),
            _ => None,
        },
        Value::List(_) => match field {
            "toString" => Some("toString"),
            "head" => Some("head"),
            "tail" => Some("tail"),
            "size" => Some("size"),
            "isEmpty" => Some("isEmpty"),
            "contains" => Some("contains"),
            "join" => Some("join"),
            "map" => Some("map"),
            "foldLeft" => Some("foldLeft"),
            _ => None,
        },
        Value::Map(_) => match field {
            "toString" => Some("toString"),
            "containsKey" => Some("Map#containsKey"),
            "containsValue" => Some("Map#containsValue"),
            "get" => Some("Map#get"),
            "getOrElse" => Some("Map#getOrElse"),
            "keys" => Some("Map#keys"),
            "values" => Some("Map#values"),
            "isEmpty" => Some("Map#isEmpty"),
            "size" => Some("Map#size"),
            "put" => Some("Map#put"),
            "remove" => Some("Map#remove"),
            _ => None,
        },
        Value::Set(_) => match field {
            "toString" => Some("toString"),
            "contains" => Some("Set#contains"),
            "isEmpty" => Some("Set#isEmpty"),
            "size" => Some("Set#size"),
            "add" => Some("Set#add"),
            "remove" => Some("Set#remove"),
            "toList" => Some("Set#toList"),
            "union" => Some("Set#union"),
            "intersect" => Some("Set#intersect"),
            "subtract" => Some("Set#subtract"),
            _ => None,
        },
        _ => None,
    }
}
