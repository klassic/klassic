//! Embedded sources for the stdlib module namespace.
//!
//! Today only `std.list` ships through this bundle; future PRs will
//! add `std.string`, `std.math`, etc. as they migrate from the v0.1
//! `stdlibFoo` prelude aliases into proper modules.
//!
//! The evaluator loads each module here as its own translation unit so
//! the `module std.X` header at the top of every file ends up
//! populating that module's exports cleanly. Native compilation is on
//! the roadmap (see `docs/roadmap-targets-stdlib.md`): until it lands,
//! the compiler will reject `import std.<X>` in native builds with a
//! source-located diagnostic.

/// One embedded stdlib module.
pub struct EmbeddedModule {
    /// Logical module path, e.g. `std.list`. The source's
    /// `module ...` header MUST match this string.
    pub path: &'static str,
    /// File name used in diagnostics. Conventionally `<stdlib path>`
    /// so user-facing line numbers stay 1-based relative to the
    /// module source.
    pub diagnostic_name: &'static str,
    /// Raw Klassic source for the module.
    pub source: &'static str,
}

const STD_LIST_SOURCE: &str = include_str!("../../../stdlib/std/list.kl");
const STD_STRING_SOURCE: &str = include_str!("../../../stdlib/std/string.kl");
const STD_MATH_SOURCE: &str = include_str!("../../../stdlib/std/math.kl");
const STD_PATH_SOURCE: &str = include_str!("../../../stdlib/std/path.kl");
const STD_OPTION_SOURCE: &str = include_str!("../../../stdlib/std/option.kl");
const STD_RESULT_SOURCE: &str = include_str!("../../../stdlib/std/result.kl");
const STD_TEST_SOURCE: &str = include_str!("../../../stdlib/std/test.kl");
const STD_CLI_SOURCE: &str = include_str!("../../../stdlib/std/cli.kl");
const STD_ENV_SOURCE: &str = include_str!("../../../stdlib/std/env.kl");
const STD_FILE_SOURCE: &str = include_str!("../../../stdlib/std/file.kl");
const STD_PROCESS_SOURCE: &str = include_str!("../../../stdlib/std/process.kl");
const STD_DIR_SOURCE: &str = include_str!("../../../stdlib/std/dir.kl");

/// Every embedded module the compiler ships with. The driver loads
/// these in order before user code so the user's source sees their
/// exports already registered.
pub const STDLIB_MODULES: &[EmbeddedModule] = &[
    EmbeddedModule {
        path: "std.list",
        diagnostic_name: "<stdlib std.list>",
        source: STD_LIST_SOURCE,
    },
    EmbeddedModule {
        path: "std.string",
        diagnostic_name: "<stdlib std.string>",
        source: STD_STRING_SOURCE,
    },
    EmbeddedModule {
        path: "std.math",
        diagnostic_name: "<stdlib std.math>",
        source: STD_MATH_SOURCE,
    },
    EmbeddedModule {
        path: "std.path",
        diagnostic_name: "<stdlib std.path>",
        source: STD_PATH_SOURCE,
    },
    EmbeddedModule {
        path: "std.option",
        diagnostic_name: "<stdlib std.option>",
        source: STD_OPTION_SOURCE,
    },
    EmbeddedModule {
        path: "std.result",
        diagnostic_name: "<stdlib std.result>",
        source: STD_RESULT_SOURCE,
    },
    EmbeddedModule {
        path: "std.test",
        diagnostic_name: "<stdlib std.test>",
        source: STD_TEST_SOURCE,
    },
    EmbeddedModule {
        path: "std.cli",
        diagnostic_name: "<stdlib std.cli>",
        source: STD_CLI_SOURCE,
    },
    EmbeddedModule {
        path: "std.env",
        diagnostic_name: "<stdlib std.env>",
        source: STD_ENV_SOURCE,
    },
    EmbeddedModule {
        path: "std.file",
        diagnostic_name: "<stdlib std.file>",
        source: STD_FILE_SOURCE,
    },
    EmbeddedModule {
        path: "std.process",
        diagnostic_name: "<stdlib std.process>",
        source: STD_PROCESS_SOURCE,
    },
    EmbeddedModule {
        path: "std.dir",
        diagnostic_name: "<stdlib std.dir>",
        source: STD_DIR_SOURCE,
    },
];

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn every_module_has_matching_header() {
        for module in STDLIB_MODULES {
            let expected_header = format!("module {}", module.path);
            assert!(
                module.source.trim_start().starts_with(&expected_header),
                "embedded module `{}` source must start with `module {}`",
                module.path,
                module.path,
            );
        }
    }

    #[test]
    fn module_paths_are_unique() {
        let mut seen = std::collections::HashSet::new();
        for module in STDLIB_MODULES {
            assert!(
                seen.insert(module.path),
                "duplicate stdlib module path `{}`",
                module.path,
            );
        }
    }
}
