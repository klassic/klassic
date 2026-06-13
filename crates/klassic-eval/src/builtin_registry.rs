use crate::Value;

pub(crate) fn builtin_name(name: &str) -> Option<&'static str> {
    if let Some(canonical) = klassic_runtime::builtins::canonical_name(name) {
        return Some(canonical);
    }
    match name {
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
        "__gc_string_index_of_from" => Some("__gc_string_index_of_from"),
        "__gc_string_last_index_of" => Some("__gc_string_last_index_of"),
        "__gc_string_to_int" => Some("__gc_string_to_int"),
        "__gc_int_to_string" => Some("__gc_int_to_string"),
        "__gc_string_starts_with" => Some("__gc_string_starts_with"),
        "__gc_string_ends_with" => Some("__gc_string_ends_with"),
        "__gc_string_contains" => Some("__gc_string_contains"),
        "__gc_pointer_count" => Some("__gc_pointer_count"),
        "__gc_segment_count" => Some("__gc_segment_count"),
        "__gc_collect_count" => Some("__gc_collect_count"),
        "__gc_list_int" => Some("__gc_list_int"),
        "__gc_list_int_len" => Some("__gc_list_int_len"),
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
        "__gc_list_ptr_get_string" => Some("__gc_list_ptr_get_string"),
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
        "__gc_smap_get_string" => Some("__gc_smap_get_string"),
        "__gc_smap_set" => Some("__gc_smap_set"),
        "__gc_smap_keys" => Some("__gc_smap_keys"),
        "__gc_smap_values" => Some("__gc_smap_values"),
        "__gc_collect" => Some("__gc_collect"),
        "__gc_pin" => Some("__gc_pin"),
        "__gc_unpin" => Some("__gc_unpin"),
        "__gc_read" => Some("__gc_read"),
        "__gc_read_ptr" => Some("__gc_read_ptr"),
        "__gc_read_string" => Some("__gc_read_string"),
        "__gc_write" => Some("__gc_write"),
        _ => None,
    }
}

pub(crate) fn builtin_arity(name: &str) -> Option<usize> {
    if let Some(arity) = klassic_runtime::builtins::arity(name) {
        return Some(arity);
    }
    match name {
        "__gc_alloc"
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
        | "__gc_list_int_len"
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
        | "__gc_read_string"
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
        | "__gc_list_ptr_get_string"
        | "__gc_list_ptr_push"
        | "__gc_list_ptr_concat"
        | "__gc_list_ptr_join"
        | "__gc_smap_has"
        | "__gc_smap_get"
        | "__gc_smap_get_string"
        | "__gc_string_get_byte"
        | "__gc_string_last_index_of"
        | "__gc_string_eq" => Some(2),
        "__gc_write"
        | "__gc_string_index_of_from"
        | "__gc_list_int_set"
        | "__gc_list_ptr_set"
        | "__gc_string_set_byte"
        | "__gc_string_substring"
        | "__gc_string_replace"
        | "__gc_smap_set" => Some(3),
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
            "padStart" => Some("padStart"),
            "padEnd" => Some("padEnd"),
            "parseIntOr" => Some("String#parseIntOr"),
            "parseDoubleOr" => Some("String#parseDoubleOr"),
            "isInteger" => Some("String#isInt"),
            "isDouble" => Some("String#isDouble"),
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
