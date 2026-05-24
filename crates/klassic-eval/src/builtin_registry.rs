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
        "stdin" | "StandardInput#all" => Some("StandardInput#all"),
        "stdinLines" | "StandardInput#lines" => Some("StandardInput#lines"),
        "getEnv" | "Environment#get" => Some("Environment#get"),
        "hasEnv" | "Environment#exists" => Some("Environment#exists"),
        "env" | "Environment#vars" => Some("Environment#vars"),
        "args" | "CommandLine#args" => Some("CommandLine#args"),
        "exit" | "Process#exit" => Some("Process#exit"),
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
        "Time#nowMillis" => Some("Time#nowMillis"),
        "Math#powInt" => Some("Math#powInt"),
        "Math#sqrtInt" => Some("Math#sqrtInt"),
        "Math#gcd" => Some("Math#gcd"),
        "String#parseInt" => Some("String#parseInt"),
        "Random#seed" => Some("Random#seed"),
        "Random#nextInt" => Some("Random#nextInt"),
        "__gc_alloc" => Some("__gc_alloc"),
        "__gc_record" => Some("__gc_record"),
        "__gc_array" => Some("__gc_array"),
        "__gc_string" => Some("__gc_string"),
        "__gc_string_concat" => Some("__gc_string_concat"),
        "__gc_string_println" => Some("__gc_string_println"),
        "__gc_string_len" => Some("__gc_string_len"),
        "__gc_string_alloc" => Some("__gc_string_alloc"),
        "__gc_string_get_byte" => Some("__gc_string_get_byte"),
        "__gc_string_set_byte" => Some("__gc_string_set_byte"),
        "__gc_string_eq" => Some("__gc_string_eq"),
        "__gc_string_substring" => Some("__gc_string_substring"),
        "__gc_string_repeat" => Some("__gc_string_repeat"),
        "__gc_string_index_of" => Some("__gc_string_index_of"),
        "__gc_string_to_int" => Some("__gc_string_to_int"),
        "__gc_int_to_string" => Some("__gc_int_to_string"),
        "__gc_string_starts_with" => Some("__gc_string_starts_with"),
        "__gc_string_ends_with" => Some("__gc_string_ends_with"),
        "__gc_string_contains" => Some("__gc_string_contains"),
        "__gc_pointer_count" => Some("__gc_pointer_count"),
        "__gc_segment_count" => Some("__gc_segment_count"),
        "__gc_collect_count" => Some("__gc_collect_count"),
        "__gc_list_int" => Some("__gc_list_int"),
        "__gc_list_int_set" => Some("__gc_list_int_set"),
        "__gc_list_int_get" => Some("__gc_list_int_get"),
        "__gc_list_int_println" => Some("__gc_list_int_println"),
        "__gc_list_int_push" => Some("__gc_list_int_push"),
        "__gc_list_int_pop" => Some("__gc_list_int_pop"),
        "__gc_list_int_reverse" => Some("__gc_list_int_reverse"),
        "__gc_list_concat" => Some("__gc_list_concat"),
        "__gc_list_ptr" => Some("__gc_list_ptr"),
        "__gc_list_ptr_len" => Some("__gc_list_ptr_len"),
        "__gc_list_ptr_set" => Some("__gc_list_ptr_set"),
        "__gc_list_ptr_get" => Some("__gc_list_ptr_get"),
        "__gc_list_ptr_push" => Some("__gc_list_ptr_push"),
        "__gc_list_ptr_pop" => Some("__gc_list_ptr_pop"),
        "__gc_list_ptr_concat" => Some("__gc_list_ptr_concat"),
        "__gc_list_ptr_reverse" => Some("__gc_list_ptr_reverse"),
        "__gc_list_ptr_join" => Some("__gc_list_ptr_join"),
        "__gc_list_int_sum" => Some("__gc_list_int_sum"),
        "__gc_list_int_min" => Some("__gc_list_int_min"),
        "__gc_list_int_max" => Some("__gc_list_int_max"),
        "__gc_string_split" => Some("__gc_string_split"),
        "__gc_string_lines" => Some("__gc_string_lines"),
        "__gc_string_replace" => Some("__gc_string_replace"),
        "__gc_string_trim" => Some("__gc_string_trim"),
        "__gc_string_to_lower" => Some("__gc_string_to_lower"),
        "__gc_string_to_upper" => Some("__gc_string_to_upper"),
        "__gc_list_int_to_string" => Some("__gc_list_int_to_string"),
        "__gc_smap_new" => Some("__gc_smap_new"),
        "__gc_smap_size" => Some("__gc_smap_size"),
        "__gc_smap_has" => Some("__gc_smap_has"),
        "__gc_smap_get" => Some("__gc_smap_get"),
        "__gc_smap_set" => Some("__gc_smap_set"),
        "__gc_smap_keys" => Some("__gc_smap_keys"),
        "__gc_smap_values" => Some("__gc_smap_values"),
        "__gc_collect" => Some("__gc_collect"),
        "__gc_pin" => Some("__gc_pin"),
        "__gc_unpin" => Some("__gc_unpin"),
        "__gc_read" => Some("__gc_read"),
        "__gc_read_ptr" => Some("__gc_read_ptr"),
        "__gc_write" => Some("__gc_write"),
        _ => None,
    }
}

pub(crate) fn builtin_arity(name: &str) -> Option<usize> {
    match name {
        "println"
        | "printlnError"
        | "assert"
        | "thread"
        | "sleep"
        | "stopwatch"
        | "double"
        | "int"
        | "floor"
        | "ceil"
        | "abs"
        | "sqrt"
        | "toString"
        | "trim"
        | "trimLeft"
        | "trimRight"
        | "toLowerCase"
        | "toUpperCase"
        | "isEmptyString"
        | "length"
        | "reverse"
        | "head"
        | "tail"
        | "size"
        | "isEmpty"
        | "__gc_alloc"
        | "__gc_record"
        | "__gc_array"
        | "__gc_string"
        | "__gc_string_println"
        | "__gc_string_len"
        | "__gc_string_alloc"
        | "__gc_string_to_int"
        | "__gc_string_lines"
        | "__gc_string_trim"
        | "__gc_string_to_lower"
        | "__gc_string_to_upper"
        | "__gc_int_to_string"
        | "__gc_pointer_count"
        | "__gc_list_int"
        | "__gc_list_int_pop"
        | "__gc_list_int_reverse"
        | "__gc_list_int_sum"
        | "__gc_list_int_min"
        | "__gc_list_int_max"
        | "__gc_list_int_println"
        | "__gc_list_ptr"
        | "__gc_list_ptr_len"
        | "__gc_list_ptr_pop"
        | "__gc_list_ptr_reverse"
        | "__gc_smap_size"
        | "__gc_smap_keys"
        | "__gc_smap_values"
        | "__gc_pin"
        | "__gc_unpin" => Some(1),
        "__gc_collect" | "__gc_segment_count" | "__gc_collect_count" | "__gc_smap_new" => Some(0),
        "__gc_read"
        | "__gc_read_ptr"
        | "__gc_string_concat"
        | "__gc_string_repeat"
        | "__gc_string_index_of"
        | "__gc_string_starts_with"
        | "__gc_string_ends_with"
        | "__gc_string_contains"
        | "__gc_string_split"
        | "__gc_list_int_get"
        | "__gc_list_int_push"
        | "__gc_list_int_to_string"
        | "__gc_list_concat"
        | "__gc_list_ptr_get"
        | "__gc_list_ptr_push"
        | "__gc_list_ptr_concat"
        | "__gc_list_ptr_join"
        | "__gc_smap_has"
        | "__gc_smap_get"
        | "__gc_string_get_byte"
        | "__gc_string_eq" => Some(2),
        "__gc_write"
        | "__gc_list_int_set"
        | "__gc_list_ptr_set"
        | "__gc_string_set_byte"
        | "__gc_string_substring"
        | "__gc_string_replace"
        | "__gc_smap_set" => Some(3),
        "assertResult" | "at" | "matches" | "split" | "join" | "startsWith" | "endsWith"
        | "contains" | "indexOf" | "lastIndexOf" | "repeat" | "cons" | "map" => Some(2),
        "substring" | "replace" | "replaceAll" | "foldLeft" => Some(3),
        "FileInput#readAll"
        | "FileInput#readLines"
        | "FileInput#all"
        | "FileInput#lines"
        | "FileOutput#exists"
        | "FileOutput#delete"
        | "Process#exit"
        | "Environment#get"
        | "Environment#exists"
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
        "Math#powInt" | "Math#gcd" => Some(2),
        "Math#sqrtInt" | "String#parseInt" | "Random#seed" | "Random#nextInt" => Some(1),
        "StandardInput#all"
        | "StandardInput#lines"
        | "Environment#vars"
        | "CommandLine#args"
        | "Dir#current"
        | "Dir#home"
        | "Dir#temp"
        | "Time#nowMillis"
        | "ToDo" => Some(0),
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
            "contains" => Some("contains"),
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
