use crate::Value;

pub(crate) fn builtin_name(name: &str) -> Option<&'static str> {
    match name {
        "println" => Some("println"),
        "printlnError" => Some("printlnError"),
        "ToDo" => Some("ToDo"),
        "assert" => Some("assert"),
        "assertResult" => Some("assertResult"),
        "thread" => Some("thread"),
        "sleep" => Some("sleep"),
        "stopwatch" => Some("stopwatch"),
        "double" => Some("double"),
        "int" => Some("int"),
        "floor" => Some("floor"),
        "ceil" => Some("ceil"),
        "abs" => Some("abs"),
        "sqrt" => Some("sqrt"),
        "toString" => Some("toString"),
        "substring" => Some("substring"),
        "at" => Some("at"),
        "matches" => Some("matches"),
        "split" => Some("split"),
        "join" => Some("join"),
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
        "isEmptyString" => Some("isEmptyString"),
        "indexOf" => Some("indexOf"),
        "lastIndexOf" => Some("lastIndexOf"),
        "length" => Some("length"),
        "repeat" => Some("repeat"),
        "reverse" => Some("reverse"),
        "head" => Some("head"),
        "tail" => Some("tail"),
        "size" => Some("size"),
        "isEmpty" => Some("isEmpty"),
        "cons" => Some("cons"),
        "map" => Some("map"),
        "foldLeft" => Some("foldLeft"),
        "FileInput#open" => Some("FileInput#open"),
        "FileInput#readAll" => Some("FileInput#readAll"),
        "FileInput#readLines" => Some("FileInput#readLines"),
        "FileInput#all" => Some("FileInput#all"),
        "FileInput#lines" => Some("FileInput#lines"),
        "FileOutput#write" => Some("FileOutput#write"),
        "FileOutput#append" => Some("FileOutput#append"),
        "FileOutput#exists" => Some("FileOutput#exists"),
        "FileOutput#delete" => Some("FileOutput#delete"),
        "FileOutput#writeLines" => Some("FileOutput#writeLines"),
        "Dir#current" => Some("Dir#current"),
        "Dir#home" => Some("Dir#home"),
        "Dir#temp" => Some("Dir#temp"),
        "Dir#exists" => Some("Dir#exists"),
        "Dir#mkdir" => Some("Dir#mkdir"),
        "Dir#mkdirs" => Some("Dir#mkdirs"),
        "Dir#isDirectory" => Some("Dir#isDirectory"),
        "Dir#isFile" => Some("Dir#isFile"),
        "Dir#list" => Some("Dir#list"),
        "Dir#listFull" => Some("Dir#listFull"),
        "Dir#delete" => Some("Dir#delete"),
        "Dir#copy" => Some("Dir#copy"),
        "Dir#move" => Some("Dir#move"),
        "Map#containsKey" => Some("Map#containsKey"),
        "Map#containsValue" => Some("Map#containsValue"),
        "Map#get" => Some("Map#get"),
        "Map#isEmpty" => Some("Map#isEmpty"),
        "Map#size" => Some("Map#size"),
        "Set#contains" => Some("Set#contains"),
        "Set#isEmpty" => Some("Set#isEmpty"),
        "Set#size" => Some("Set#size"),
        _ => None,
    }
}

pub(crate) fn builtin_arity(name: &str) -> Option<usize> {
    match name {
        "println" | "printlnError" | "assert" | "thread" | "sleep" | "stopwatch" | "double"
        | "int" | "floor" | "ceil" | "abs" | "sqrt" | "toString" | "trim" | "trimLeft"
        | "trimRight" | "toLowerCase" | "toUpperCase" | "isEmptyString" | "length" | "reverse"
        | "head" | "tail" | "size" | "isEmpty" => Some(1),
        "assertResult" | "at" | "matches" | "split" | "join" | "startsWith" | "endsWith"
        | "contains" | "indexOf" | "lastIndexOf" | "repeat" | "cons" | "map" => Some(2),
        "substring" | "replace" | "replaceAll" | "foldLeft" => Some(3),
        "FileInput#readAll"
        | "FileInput#readLines"
        | "FileInput#all"
        | "FileInput#lines"
        | "FileOutput#exists"
        | "FileOutput#delete"
        | "Dir#exists"
        | "Dir#mkdir"
        | "Dir#mkdirs"
        | "Dir#isDirectory"
        | "Dir#isFile"
        | "Dir#list"
        | "Dir#listFull"
        | "Dir#delete"
        | "Map#isEmpty"
        | "Map#size"
        | "Set#isEmpty"
        | "Set#size" => Some(1),
        "FileInput#open"
        | "FileOutput#write"
        | "FileOutput#append"
        | "FileOutput#writeLines"
        | "Dir#copy"
        | "Dir#move"
        | "Map#containsKey"
        | "Map#containsValue"
        | "Map#get"
        | "Set#contains" => Some(2),
        "Dir#current" | "Dir#home" | "Dir#temp" | "ToDo" => Some(0),
        _ => None,
    }
}

pub(crate) fn value_method_builtin_name(value: &Value, field: &str) -> Option<&'static str> {
    match value {
        Value::Int(_)
        | Value::Long(_)
        | Value::Float(_)
        | Value::Double(_)
        | Value::Bool(_)
        | Value::Unit
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
            _ => None,
        },
        Value::List(_) => match field {
            "head" => Some("head"),
            "tail" => Some("tail"),
            "size" => Some("size"),
            "isEmpty" => Some("isEmpty"),
            "join" => Some("join"),
            "map" => Some("map"),
            "foldLeft" => Some("foldLeft"),
            _ => None,
        },
        Value::Map(_) => match field {
            "containsKey" => Some("Map#containsKey"),
            "containsValue" => Some("Map#containsValue"),
            "get" => Some("Map#get"),
            "isEmpty" => Some("Map#isEmpty"),
            "size" => Some("Map#size"),
            _ => None,
        },
        Value::Set(_) => match field {
            "contains" => Some("Set#contains"),
            "isEmpty" => Some("Set#isEmpty"),
            "size" => Some("Set#size"),
            _ => None,
        },
        _ => None,
    }
}
