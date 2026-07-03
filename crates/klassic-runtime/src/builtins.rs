//! Shared descriptor table for public Klassic builtins.
//!
//! Today the descriptor only carries the canonical name, its aliases,
//! and the call-site arity that the typechecker / native lowering /
//! evaluator dispatch each cross-check against. Later PRs will grow the
//! descriptor with a signature, purity / effect tags, native support
//! level, and short documentation — the shape is intentionally minimal
//! for the first MVP so the evaluator can replace its hand-rolled
//! `builtin_name` / `builtin_arity` match arms without changing
//! behaviour.
//!
//! The raw GC-heap debug builtins (`__gc_*`) that used to live outside
//! this manifest have been removed from the user-facing language
//! entirely — the typechecker no longer declares them, so a Klassic
//! program cannot reference them. A handful of `__gc_*` names remain
//! as compiler-internal dispatch tags inside `klassic-native`'s
//! enum-lowering desugar pass (never reachable from parsed source);
//! see `docs/architecture-rust.md` for the current list.

use std::collections::HashMap;
use std::sync::OnceLock;

/// A single public builtin known to evaluator / typechecker / native.
pub struct BuiltinDescriptor {
    /// Canonical name. The evaluator's dispatcher, the typechecker's
    /// signature install table, and the native compiler's lowering all
    /// agree on this name.
    pub canonical: &'static str,
    /// Names that resolve to the same builtin but are not the canonical
    /// form. Provided as an explicit list so the manifest stays the
    /// only place aliasing is described.
    pub aliases: &'static [&'static str],
    /// Number of arguments the dispatcher expects at the call site.
    pub arity: usize,
}

/// The full set of public builtins. Order is roughly grouped by area
/// (I/O, numerics, strings, lists, file, environment, math, random,
/// time) and does not affect dispatch.
pub const BUILTINS: &[BuiltinDescriptor] = &[
    // --- I/O, control, threading ---
    BuiltinDescriptor {
        canonical: "println",
        aliases: &[],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "printlnError",
        aliases: &[],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "ToDo",
        aliases: &[],
        arity: 0,
    },
    BuiltinDescriptor {
        canonical: "assert",
        aliases: &[],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "assertResult",
        aliases: &[],
        arity: 2,
    },
    BuiltinDescriptor {
        canonical: "thread",
        aliases: &[],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "sleep",
        aliases: &[],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "stopwatch",
        aliases: &[],
        arity: 1,
    },
    // --- Numeric conversions / arithmetic helpers ---
    BuiltinDescriptor {
        canonical: "double",
        aliases: &[],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "int",
        aliases: &[],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "floor",
        aliases: &[],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "ceil",
        aliases: &[],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "abs",
        aliases: &[],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "sqrt",
        aliases: &[],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "round",
        aliases: &[],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "sin",
        aliases: &[],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "cos",
        aliases: &[],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "tan",
        aliases: &[],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "asin",
        aliases: &[],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "acos",
        aliases: &[],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "atan",
        aliases: &[],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "exp",
        aliases: &[],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "log",
        aliases: &[],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "log10",
        aliases: &[],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "log2",
        aliases: &[],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "pow",
        aliases: &[],
        arity: 2,
    },
    BuiltinDescriptor {
        canonical: "atan2",
        aliases: &[],
        arity: 2,
    },
    // --- String helpers ---
    BuiltinDescriptor {
        canonical: "toString",
        aliases: &[],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "substring",
        aliases: &[],
        arity: 3,
    },
    BuiltinDescriptor {
        canonical: "at",
        aliases: &[],
        arity: 2,
    },
    BuiltinDescriptor {
        canonical: "matches",
        aliases: &[],
        arity: 2,
    },
    BuiltinDescriptor {
        canonical: "split",
        aliases: &[],
        arity: 2,
    },
    BuiltinDescriptor {
        canonical: "join",
        aliases: &[],
        arity: 2,
    },
    BuiltinDescriptor {
        canonical: "trim",
        aliases: &[],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "trimLeft",
        aliases: &[],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "trimRight",
        aliases: &[],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "replace",
        aliases: &[],
        arity: 3,
    },
    BuiltinDescriptor {
        canonical: "replaceAll",
        aliases: &[],
        arity: 3,
    },
    BuiltinDescriptor {
        canonical: "toLowerCase",
        aliases: &[],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "toUpperCase",
        aliases: &[],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "startsWith",
        aliases: &[],
        arity: 2,
    },
    BuiltinDescriptor {
        canonical: "endsWith",
        aliases: &[],
        arity: 2,
    },
    BuiltinDescriptor {
        canonical: "contains",
        aliases: &[],
        arity: 2,
    },
    BuiltinDescriptor {
        canonical: "isEmptyString",
        aliases: &[],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "indexOf",
        aliases: &[],
        arity: 2,
    },
    BuiltinDescriptor {
        canonical: "lastIndexOf",
        aliases: &[],
        arity: 2,
    },
    BuiltinDescriptor {
        canonical: "length",
        aliases: &[],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "repeat",
        aliases: &[],
        arity: 2,
    },
    BuiltinDescriptor {
        canonical: "reverse",
        aliases: &[],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "padStart",
        aliases: &[],
        arity: 3,
    },
    BuiltinDescriptor {
        canonical: "padEnd",
        aliases: &[],
        arity: 3,
    },
    BuiltinDescriptor {
        canonical: "format",
        aliases: &[],
        arity: 2,
    },
    // --- List helpers ---
    BuiltinDescriptor {
        canonical: "head",
        aliases: &[],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "tail",
        aliases: &[],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "size",
        aliases: &[],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "isEmpty",
        aliases: &[],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "cons",
        aliases: &[],
        arity: 2,
    },
    BuiltinDescriptor {
        canonical: "map",
        aliases: &[],
        arity: 2,
    },
    BuiltinDescriptor {
        canonical: "foldLeft",
        aliases: &[],
        arity: 3,
    },
    // --- File / directory / streams ---
    BuiltinDescriptor {
        canonical: "FileInput#open",
        aliases: &[],
        arity: 2,
    },
    BuiltinDescriptor {
        canonical: "FileInput#readAll",
        aliases: &[],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "FileInput#readLines",
        aliases: &[],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "FileInput#all",
        aliases: &[],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "FileInput#lines",
        aliases: &[],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "FileOutput#write",
        aliases: &[],
        arity: 2,
    },
    BuiltinDescriptor {
        canonical: "FileOutput#append",
        aliases: &[],
        arity: 2,
    },
    BuiltinDescriptor {
        canonical: "FileOutput#exists",
        aliases: &[],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "FileOutput#delete",
        aliases: &[],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "FileOutput#writeLines",
        aliases: &[],
        arity: 2,
    },
    BuiltinDescriptor {
        canonical: "StandardInput#all",
        aliases: &["stdin"],
        arity: 0,
    },
    BuiltinDescriptor {
        canonical: "StandardInput#lines",
        aliases: &["stdinLines"],
        arity: 0,
    },
    // --- Environment, process, CLI ---
    BuiltinDescriptor {
        canonical: "Environment#get",
        aliases: &["getEnv"],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "Environment#exists",
        aliases: &["hasEnv"],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "Environment#vars",
        aliases: &["env"],
        arity: 0,
    },
    BuiltinDescriptor {
        canonical: "CommandLine#args",
        aliases: &["args"],
        arity: 0,
    },
    BuiltinDescriptor {
        canonical: "Process#exit",
        aliases: &["exit"],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "Process#run",
        aliases: &[],
        arity: 2,
    },
    // --- Directory ---
    BuiltinDescriptor {
        canonical: "Dir#current",
        aliases: &[],
        arity: 0,
    },
    BuiltinDescriptor {
        canonical: "Dir#home",
        aliases: &[],
        arity: 0,
    },
    BuiltinDescriptor {
        canonical: "Dir#temp",
        aliases: &[],
        arity: 0,
    },
    BuiltinDescriptor {
        canonical: "Dir#exists",
        aliases: &[],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "Dir#mkdir",
        aliases: &[],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "Dir#mkdirs",
        aliases: &[],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "Dir#isDirectory",
        aliases: &[],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "Dir#isFile",
        aliases: &[],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "Dir#list",
        aliases: &[],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "Dir#listFull",
        aliases: &[],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "Dir#delete",
        aliases: &[],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "Dir#copy",
        aliases: &[],
        arity: 2,
    },
    BuiltinDescriptor {
        canonical: "Dir#move",
        aliases: &[],
        arity: 2,
    },
    // --- Map / Set ---
    BuiltinDescriptor {
        canonical: "Map#containsKey",
        aliases: &[],
        arity: 2,
    },
    BuiltinDescriptor {
        canonical: "Map#containsValue",
        aliases: &[],
        arity: 2,
    },
    BuiltinDescriptor {
        canonical: "Map#get",
        aliases: &[],
        arity: 2,
    },
    BuiltinDescriptor {
        canonical: "Map#getOrElse",
        aliases: &[],
        arity: 3,
    },
    BuiltinDescriptor {
        canonical: "Map#keys",
        aliases: &[],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "Map#values",
        aliases: &[],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "Map#isEmpty",
        aliases: &[],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "Map#size",
        aliases: &[],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "Map#put",
        aliases: &[],
        arity: 3,
    },
    BuiltinDescriptor {
        canonical: "Map#remove",
        aliases: &[],
        arity: 2,
    },
    BuiltinDescriptor {
        canonical: "Map#fromPairs",
        aliases: &[],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "Map#empty",
        aliases: &[],
        arity: 0,
    },
    BuiltinDescriptor {
        canonical: "Set#contains",
        aliases: &[],
        arity: 2,
    },
    BuiltinDescriptor {
        canonical: "Set#isEmpty",
        aliases: &[],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "Set#size",
        aliases: &[],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "Set#add",
        aliases: &[],
        arity: 2,
    },
    BuiltinDescriptor {
        canonical: "Set#remove",
        aliases: &[],
        arity: 2,
    },
    BuiltinDescriptor {
        canonical: "Set#fromList",
        aliases: &[],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "Set#toList",
        aliases: &[],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "Set#empty",
        aliases: &[],
        arity: 0,
    },
    BuiltinDescriptor {
        canonical: "Set#union",
        aliases: &[],
        arity: 2,
    },
    BuiltinDescriptor {
        canonical: "Set#intersect",
        aliases: &[],
        arity: 2,
    },
    BuiltinDescriptor {
        canonical: "Set#subtract",
        aliases: &[],
        arity: 2,
    },
    // --- Time / Math / Random ---
    BuiltinDescriptor {
        canonical: "Time#nowMillis",
        aliases: &[],
        arity: 0,
    },
    BuiltinDescriptor {
        canonical: "Math#powInt",
        aliases: &[],
        arity: 2,
    },
    BuiltinDescriptor {
        canonical: "Math#sqrtInt",
        aliases: &[],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "Math#gcd",
        aliases: &[],
        arity: 2,
    },
    BuiltinDescriptor {
        canonical: "String#parseInt",
        aliases: &[],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "String#parseDouble",
        aliases: &[],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "String#parseIntOr",
        aliases: &[],
        arity: 2,
    },
    BuiltinDescriptor {
        canonical: "String#parseDoubleOr",
        aliases: &[],
        arity: 2,
    },
    BuiltinDescriptor {
        canonical: "String#isInt",
        aliases: &[],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "String#isDouble",
        aliases: &[],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "Random#seed",
        aliases: &[],
        arity: 1,
    },
    BuiltinDescriptor {
        canonical: "Random#nextInt",
        aliases: &[],
        arity: 1,
    },
];

fn resolution_index() -> &'static HashMap<&'static str, &'static BuiltinDescriptor> {
    static INDEX: OnceLock<HashMap<&'static str, &'static BuiltinDescriptor>> = OnceLock::new();
    INDEX.get_or_init(|| {
        let mut map = HashMap::with_capacity(BUILTINS.len() * 2);
        for descriptor in BUILTINS {
            assert!(
                map.insert(descriptor.canonical, descriptor).is_none(),
                "duplicate builtin canonical name '{}'",
                descriptor.canonical,
            );
            for alias in descriptor.aliases {
                assert!(
                    map.insert(alias, descriptor).is_none(),
                    "alias '{}' collides with another builtin entry",
                    alias,
                );
            }
        }
        map
    })
}

/// Resolves a name (canonical or alias) to its descriptor.
pub fn lookup(name: &str) -> Option<&'static BuiltinDescriptor> {
    resolution_index().get(name).copied()
}

/// Returns the canonical name for `name` if it refers to a public builtin.
pub fn canonical_name(name: &str) -> Option<&'static str> {
    lookup(name).map(|descriptor| descriptor.canonical)
}

/// Returns the call-site arity for `name` if it refers to a public builtin.
pub fn arity(name: &str) -> Option<usize> {
    lookup(name).map(|descriptor| descriptor.arity)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn canonical_names_are_self_resolving() {
        for descriptor in BUILTINS {
            assert_eq!(
                canonical_name(descriptor.canonical),
                Some(descriptor.canonical),
                "canonical '{}' did not resolve to itself",
                descriptor.canonical,
            );
            assert_eq!(arity(descriptor.canonical), Some(descriptor.arity));
        }
    }

    #[test]
    fn aliases_resolve_to_canonical_form() {
        assert_eq!(canonical_name("stdin"), Some("StandardInput#all"));
        assert_eq!(canonical_name("getEnv"), Some("Environment#get"));
        assert_eq!(canonical_name("args"), Some("CommandLine#args"));
        assert_eq!(arity("exit"), Some(1));
    }

    #[test]
    fn unknown_name_returns_none() {
        assert!(canonical_name("definitely_not_a_builtin").is_none());
        assert!(arity("definitely_not_a_builtin").is_none());
    }

    #[test]
    fn manifest_index_is_collision_free() {
        // Constructing the index panics on collision; call it once so
        // the test fails loudly if the manifest ever introduces a dup.
        let index = resolution_index();
        let expected_entries = BUILTINS
            .iter()
            .map(|descriptor| 1 + descriptor.aliases.len())
            .sum::<usize>();
        assert_eq!(index.len(), expected_entries);
    }
}
