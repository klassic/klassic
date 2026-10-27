#[path = "gc_duration.rs"]
mod gc_duration;
#[path = "gc_heap.rs"]
mod gc_heap;
use std::path::PathBuf;

use klassic_native::{NativeTarget, PlannedTarget, TargetRecognition};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ExecutionConfig {
    pub deny_trust: bool,
    pub warn_trust: bool,
    pub native_target: NativeTarget,
    /// `--backend c`: `build` emits a portable C translation unit at
    /// the output path instead of a direct ELF executable.
    pub c_backend: bool,
    /// `--backend llvm`: `build` emits LLVM IR and compiles it with
    /// clang (migration plan `docs/llvm-backend-plan.md`). An output
    /// ending in `.ll` writes the IR instead of an executable.
    pub llvm_backend: bool,
    /// True when `--target` was given on the command line. Hosts with
    /// no direct native backend (e.g. macOS) route a target-less
    /// `build` through the portable C backend instead of silently
    /// cross-building for the default Linux target.
    pub explicit_target: bool,
    /// `--gc-log`: emitted programs print GC statistics to stderr at
    /// exit.
    pub gc_log: bool,
    pub gc_log_format: Option<klassic_native::GcLogFormat>,
    /// `--gc-stress`: emitted `gc_alloc` collects before every
    /// allocation (deterministic detector for pointers left unrooted
    /// across an allocation).
    pub gc_stress: bool,
    /// `--gc-poison`: heap-stored pointers are colored BAD, so any
    /// unbarriered dereference faults on a non-canonical address and the
    /// load-barrier slow path runs on every load.
    pub gc_poison: bool,
    /// Select the generational direct native collector; legacy remains default.
    pub gc_generational: bool,
    pub gc_max_heap: Option<u64>,
    pub gc_soft_max_heap: Option<u64>,
    pub gc_max_metadata: Option<u64>,
    /// Initial accessible payload and lower limit for page uncommit.
    pub gc_min_heap: Option<u64>,
    pub gc_uncommit_delay_ns: Option<u64>,
    pub gc_pre_touch: bool,
    pub gc_large_pages: Option<klassic_native::GcLargePages>,
}

impl ExecutionConfig {
    pub const fn new(deny_trust: bool, warn_trust: bool, native_target: NativeTarget) -> Self {
        Self {
            deny_trust,
            warn_trust,
            native_target,
            c_backend: false,
            llvm_backend: false,
            explicit_target: false,
            gc_log: false,
            gc_log_format: None,
            gc_stress: false,
            gc_poison: false,
            gc_generational: false,
            gc_max_heap: None,
            gc_soft_max_heap: None,
            gc_max_metadata: None,
            gc_min_heap: None,
            gc_uncommit_delay_ns: None,
            gc_pre_touch: false,
            gc_large_pages: None,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum RunAction {
    BuildFile { input: PathBuf, output: PathBuf },
    EvaluateExpression(String),
    EvaluateFile(PathBuf),
    StartRepl,
    ListTargets,
    ShowVersion,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ParsedCommand {
    pub action: RunAction,
    pub config: ExecutionConfig,
    /// Arguments that should be visible to the user's program via
    /// `CommandLine#args()`. Populated from anything after the `--`
    /// separator on the command line (and intentionally empty for any
    /// invocation that lacks one).
    pub script_args: Vec<String>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CommandLineError {
    /// Arguments don't match any recognised invocation. Driver should
    /// print the standard usage block.
    Usage,
    /// `--target` was given without a value.
    MissingTargetArgument,
    /// `--backend` was given without a value or with an unknown one.
    UnknownBackend(String),
    /// `--target X` named a target the compiler knows about but doesn't
    /// have a backend for yet.
    PlannedTarget {
        argument: String,
        planned: &'static PlannedTarget,
    },
    /// `--target X` named something the compiler does not recognise at
    /// all (typo, future triple).
    UnknownTarget(String),
    /// `--gc=<mode>` named something other than `legacy` or
    /// `generational`.
    UnknownGcMode(String),
    InvalidGcHeap(String),
}

pub fn parse_command_line(args: &[String]) -> Result<ParsedCommand, CommandLineError> {
    let mut deny_trust = false;
    let mut c_backend = false;
    let mut llvm_backend = false;
    let mut warn_trust = false;
    let mut native_target = NativeTarget::default();
    let mut explicit_target = false;
    let mut gc_log = false;
    let mut gc_log_format = None;
    let mut gc_stress = false;
    let mut gc_poison = false;
    let mut gc_generational = false;
    let mut gc_max_heap = None;
    let mut gc_soft_max_heap = None;
    let mut gc_max_metadata = None;
    let mut gc_min_heap = None;
    let mut gc_uncommit_delay_ns = None;
    let mut gc_pre_touch = false;
    let mut gc_large_pages = None;
    let mut others = Vec::new();
    let mut script_args = Vec::new();
    let mut seen_separator = false;
    let mut index = 0usize;

    while index < args.len() {
        if seen_separator {
            script_args.push(args[index].clone());
            index += 1;
            continue;
        }
        match args[index].as_str() {
            "--" => {
                seen_separator = true;
                index += 1;
            }
            "--deny-trust" => {
                deny_trust = true;
                index += 1;
            }
            "--warn-trust" => {
                warn_trust = true;
                index += 1;
            }
            "--gc-log-format" => {
                let value = args.get(index + 1).ok_or_else(|| {
                    CommandLineError::InvalidGcHeap("missing --gc-log-format value".into())
                })?;
                gc_log_format = Some(
                    value
                        .parse()
                        .map_err(|e: &str| CommandLineError::InvalidGcHeap(e.into()))?,
                );
                index += 2;
            }
            value if value.starts_with("--gc-log-format=") => {
                gc_log_format = Some(
                    value["--gc-log-format=".len()..]
                        .parse()
                        .map_err(|e: &str| CommandLineError::InvalidGcHeap(e.into()))?,
                );
                index += 1;
            }
            "--gc-log" => {
                gc_log = true;
                index += 1;
            }
            "--gc-stress" => {
                gc_stress = true;
                index += 1;
            }
            "--gc-poison" => {
                gc_poison = true;
                index += 1;
            }
            mode if mode.starts_with("--gc=") => {
                match &mode["--gc=".len()..] {
                    "legacy" => gc_generational = false,
                    "generational" => gc_generational = true,
                    other => return Err(CommandLineError::UnknownGcMode(other.to_owned())),
                }
                index += 1;
            }
            "--gc-large-pages" => {
                let value = args.get(index + 1).ok_or_else(|| {
                    CommandLineError::InvalidGcHeap("missing --gc-large-pages value".into())
                })?;
                gc_large_pages = Some(
                    value
                        .parse()
                        .map_err(|e: &str| CommandLineError::InvalidGcHeap(e.into()))?,
                );
                index += 2;
            }
            value if value.starts_with("--gc-large-pages=") => {
                gc_large_pages = Some(
                    value["--gc-large-pages=".len()..]
                        .parse()
                        .map_err(|e: &str| CommandLineError::InvalidGcHeap(e.into()))?,
                );
                index += 1;
            }
            "--gc-pre-touch" => {
                gc_pre_touch = true;
                index += 1;
            }
            "--gc-uncommit-delay" => {
                let value = args.get(index + 1).ok_or_else(|| {
                    CommandLineError::InvalidGcHeap("missing --gc-uncommit-delay value".into())
                })?;
                gc_uncommit_delay_ns =
                    Some(gc_duration::parse(value).map_err(CommandLineError::InvalidGcHeap)?);
                index += 2;
            }
            value if value.starts_with("--gc-uncommit-delay=") => {
                gc_uncommit_delay_ns = Some(
                    gc_duration::parse(&value["--gc-uncommit-delay=".len()..])
                        .map_err(CommandLineError::InvalidGcHeap)?,
                );
                index += 1;
            }
            "--gc-min-heap" => {
                let value = args.get(index + 1).ok_or_else(|| {
                    CommandLineError::InvalidGcHeap("missing --gc-min-heap value".into())
                })?;
                gc_min_heap =
                    Some(gc_heap::parse_min(value).map_err(CommandLineError::InvalidGcHeap)?);
                index += 2;
            }
            value if value.starts_with("--gc-min-heap=") => {
                gc_min_heap = Some(
                    gc_heap::parse_min(&value["--gc-min-heap=".len()..])
                        .map_err(CommandLineError::InvalidGcHeap)?,
                );
                index += 1;
            }
            "--gc-soft-max-heap" => {
                let value = args.get(index + 1).ok_or_else(|| {
                    CommandLineError::InvalidGcHeap("missing --gc-soft-max-heap value".into())
                })?;
                gc_soft_max_heap =
                    Some(gc_heap::parse_soft(value).map_err(CommandLineError::InvalidGcHeap)?);
                index += 2;
            }
            value if value.starts_with("--gc-soft-max-heap=") => {
                gc_soft_max_heap = Some(
                    gc_heap::parse_soft(&value["--gc-soft-max-heap=".len()..])
                        .map_err(CommandLineError::InvalidGcHeap)?,
                );
                index += 1;
            }
            "--gc-max-metadata" => {
                let value = args.get(index + 1).ok_or_else(|| {
                    CommandLineError::InvalidGcHeap("missing --gc-max-metadata value".into())
                })?;
                gc_max_metadata =
                    Some(gc_heap::parse_metadata(value).map_err(CommandLineError::InvalidGcHeap)?);
                index += 2;
            }
            value if value.starts_with("--gc-max-metadata=") => {
                gc_max_metadata = Some(
                    gc_heap::parse_metadata(&value["--gc-max-metadata=".len()..])
                        .map_err(CommandLineError::InvalidGcHeap)?,
                );
                index += 1;
            }
            "--gc-max-heap" => {
                let value = args.get(index + 1).ok_or_else(|| {
                    CommandLineError::InvalidGcHeap("missing --gc-max-heap value".into())
                })?;
                gc_max_heap = Some(gc_heap::parse(value).map_err(CommandLineError::InvalidGcHeap)?);
                index += 2;
            }
            value if value.starts_with("--gc-max-heap=") => {
                gc_max_heap = Some(
                    gc_heap::parse(&value["--gc-max-heap=".len()..])
                        .map_err(CommandLineError::InvalidGcHeap)?,
                );
                index += 1;
            }
            "--backend" => {
                let Some(backend) = args.get(index + 1) else {
                    return Err(CommandLineError::UnknownBackend(String::new()));
                };
                match backend.as_str() {
                    "c" => c_backend = true,
                    "llvm" => llvm_backend = true,
                    _ => return Err(CommandLineError::UnknownBackend(backend.clone())),
                }
                index += 2;
            }
            "--target" => {
                let Some(target_name) = args.get(index + 1) else {
                    return Err(CommandLineError::MissingTargetArgument);
                };
                native_target = match NativeTarget::recognize(target_name) {
                    TargetRecognition::Supported(target) => target,
                    TargetRecognition::Planned(planned) => {
                        return Err(CommandLineError::PlannedTarget {
                            argument: target_name.clone(),
                            planned,
                        });
                    }
                    TargetRecognition::Unknown => {
                        return Err(CommandLineError::UnknownTarget(target_name.clone()));
                    }
                };
                explicit_target = true;
                index += 2;
            }
            other => {
                others.push(other.to_owned());
                index += 1;
            }
        }
    }

    let mut config = ExecutionConfig::new(deny_trust, warn_trust, native_target);
    config.c_backend = c_backend;
    config.llvm_backend = llvm_backend;
    config.explicit_target = explicit_target;
    config.gc_log = gc_log || gc_log_format.is_some();
    config.gc_log_format = gc_log_format;
    config.gc_stress = gc_stress;
    config.gc_poison = gc_poison;
    config.gc_generational = gc_generational;
    config.gc_max_heap = gc_max_heap;
    config.gc_soft_max_heap = gc_soft_max_heap;
    config.gc_max_metadata = gc_max_metadata;
    config.gc_min_heap = gc_min_heap;
    config.gc_uncommit_delay_ns = gc_uncommit_delay_ns;
    config.gc_pre_touch = gc_pre_touch;
    config.gc_large_pages = gc_large_pages;
    let action = match others.as_slice() {
        [command] if command == "targets" => RunAction::ListTargets,
        [flag] if flag == "--version" || flag == "-V" => RunAction::ShowVersion,
        [] => RunAction::StartRepl,
        [file_name] if file_name.ends_with(".kl") => {
            RunAction::EvaluateFile(PathBuf::from(file_name))
        }
        [flag, file_name] if flag == "-f" => RunAction::EvaluateFile(PathBuf::from(file_name)),
        [flag, expression] if flag == "-e" => RunAction::EvaluateExpression(expression.clone()),
        [command, file_name] if command == "run" => {
            RunAction::EvaluateFile(PathBuf::from(file_name))
        }
        [command, input, output_flag, output] if command == "build" && output_flag == "-o" => {
            RunAction::BuildFile {
                input: PathBuf::from(input),
                output: PathBuf::from(output),
            }
        }
        _ => return Err(CommandLineError::Usage),
    };

    if gc_log_format.is_some()
        && (!gc_generational
            || c_backend
            || llvm_backend
            || !matches!(action, RunAction::BuildFile { .. }))
    {
        return Err(CommandLineError::InvalidGcHeap(
            "--gc-log-format requires a --gc=generational native build".into(),
        ));
    }
    if (gc_soft_max_heap.is_some()
        || gc_max_heap.is_some()
        || gc_max_metadata.is_some()
        || gc_min_heap.is_some()
        || gc_uncommit_delay_ns.is_some()
        || gc_pre_touch
        || gc_large_pages.is_some())
        && (!gc_generational
            || !matches!(action, RunAction::BuildFile { .. })
            || ((c_backend || llvm_backend)
                && (gc_max_metadata.is_some() || gc_pre_touch || gc_large_pages.is_some())))
    {
        return Err(CommandLineError::InvalidGcHeap(
            if gc_max_metadata.is_some() {
                "heap/metadata/uncommit settings require a --gc=generational native build"
            } else {
                "heap/uncommit/pre-touch/large-page settings require a --gc=generational native build"
            }
            .into(),
        ));
    }
    if gc_soft_max_heap.is_some() || gc_max_heap.is_some() || gc_min_heap.is_some() {
        klassic_native::validate_gc_soft_heap_limits(
            gc_min_heap.unwrap_or(klassic_native::DEFAULT_GC_MIN_HEAP),
            gc_soft_max_heap
                .or(gc_max_heap)
                .unwrap_or(klassic_native::DEFAULT_GC_MAX_HEAP),
            gc_max_heap.unwrap_or(klassic_native::DEFAULT_GC_MAX_HEAP),
        )
        .map_err(|e| CommandLineError::InvalidGcHeap(e.into()))?;
    }
    if let Some(mode) = gc_large_pages {
        klassic_native::validate_gc_large_pages(
            mode,
            native_target,
            gc_min_heap.unwrap_or(klassic_native::DEFAULT_GC_MIN_HEAP),
            gc_max_heap.unwrap_or(klassic_native::DEFAULT_GC_MAX_HEAP),
        )
        .map_err(|e| CommandLineError::InvalidGcHeap(e.into()))?;
    }
    Ok(ParsedCommand {
        action,
        config,
        script_args,
    })
}

pub fn usage() -> String {
    format!(
        "Usage: klassic [--deny-trust] [--warn-trust] [--target <target>] \
         (-f <fileName> | -e <expression> | run <fileName> | <fileName> | \
         build <fileName> -o <output> | targets) [-- <script args>...]\n\
     Options:\n\
       --deny-trust       : reject programs that depend on trusted proofs\n\
       --warn-trust       : warn when trusted proofs are used\n\
       --target <target>  : native build target (supported: {})\n\
       --backend c        : C backend; a .c output writes source, otherwise compile and link\n\
       --backend llvm     : `build` emits LLVM IR and compiles it with clang (>= 15); an .ll output writes the IR\n\
       --gc=<mode>        : `build` collector: legacy (default) or generational \
(direct native; canonical C/LLVM on Linux x86-64)\n\
       --gc-large-pages=off|try|required: Linux THP policy (default off)\n\
       --gc-pre-touch: write each OS page when payload memory is committed\n\
       --gc-uncommit-delay <duration>: free-page delay (default 300s; ns/us/ms/s/m; plain seconds)\n\
       --gc-min-heap <size>: initial accessible payload and uncommit floor (default 16 MiB)\n\
       --gc-soft-max-heap <size>: advisory payload target (default hard maximum)\n\
       --gc-max-metadata <size>: direct metadata budget (default geometry bound; canonical C/LLVM fixes 256 MiB and rejects overrides)\n\
       --gc-max-heap <size>: generational payload limit (default 256 MiB; KiB/MiB/GiB)\n\
       <fileName>         : read a program from <fileName> and execute it\n\
       run <fileName>     : same as <fileName>; pairs naturally with `-- <args>`\n\
       -e <expression>    : evaluate <expression>\n\
       build <fileName> -o <output>: compile <fileName> to a native executable \
for the detected host (or for an explicit --target); the portable C backend \
covers hosts and constructs the direct backends do not handle yet\n\
       targets            : list known native targets and their support status\n\
       --                 : separates klassic flags from arguments visible to \
the user script via CommandLine#args()\n",
        NativeTarget::supported_names_csv()
    )
}

/// Render the error that the driver should print on stderr before
/// exiting with a non-zero status.
pub fn render_command_line_error(error: &CommandLineError) -> String {
    match error {
        CommandLineError::Usage => usage(),
        CommandLineError::MissingTargetArgument => {
            format!("error: --target requires an argument\n{}", usage())
        }
        CommandLineError::UnknownBackend(argument) if argument.is_empty() => {
            format!("error: --backend requires an argument\n{}", usage())
        }
        CommandLineError::UnknownBackend(argument) => format!(
            "error: unknown backend '{argument}'\n  help: supported backends: c (and the default direct ELF backend)\n"
        ),
        CommandLineError::PlannedTarget { argument, planned } => format!(
            "error: target {} is recognized but not implemented yet\n  \
             help: tier {} target via the {} backend producing a {}\n  \
             help: supported targets: {}\n",
            argument,
            planned.tier,
            planned.backend,
            planned.artifact,
            NativeTarget::supported_names_csv(),
        ),
        CommandLineError::UnknownTarget(argument) => format!(
            "error: unknown target '{}'\n  help: run `klassic targets` to list known targets\n",
            argument
        ),
        CommandLineError::InvalidGcHeap(message) => format!("error: {message}\n"),
        CommandLineError::UnknownGcMode(argument) => format!(
            "error: unknown --gc mode '{argument}'\n  help: supported modes: legacy, generational\n"
        ),
    }
}

#[cfg(test)]
mod tests {
    use klassic_native::NativeTarget;

    use super::{
        CommandLineError, RunAction, parse_command_line, render_command_line_error, usage,
    };

    #[test]
    fn parses_expression_invocation() {
        let args = vec!["-e".to_string(), "1 + 2".to_string()];
        let parsed = parse_command_line(&args).expect("command should parse");
        assert_eq!(
            parsed.action,
            RunAction::EvaluateExpression("1 + 2".to_string())
        );
    }

    #[test]
    fn parses_positional_file_invocation() {
        let args = vec!["sample.kl".to_string()];
        let parsed = parse_command_line(&args).expect("command should parse");
        match parsed.action {
            RunAction::EvaluateFile(path) => assert_eq!(path.to_string_lossy(), "sample.kl"),
            other => panic!("unexpected action: {other:?}"),
        }
    }

    #[test]
    fn parses_dash_f_file_invocation() {
        let args = vec!["-f".to_string(), "sample.kl".to_string()];
        let parsed = parse_command_line(&args).expect("command should parse");
        match parsed.action {
            RunAction::EvaluateFile(path) => assert_eq!(path.to_string_lossy(), "sample.kl"),
            other => panic!("unexpected action: {other:?}"),
        }
    }

    #[test]
    fn parses_no_args_as_repl() {
        let args = vec![];
        let parsed = parse_command_line(&args).expect("repl command should parse");
        assert_eq!(parsed.action, RunAction::StartRepl);
    }

    #[test]
    fn parses_native_build_invocation() {
        let args = vec![
            "build".to_string(),
            "sample.kl".to_string(),
            "-o".to_string(),
            "sample".to_string(),
        ];
        let parsed = parse_command_line(&args).expect("build command should parse");
        match parsed.action {
            RunAction::BuildFile { input, output } => {
                assert_eq!(input.to_string_lossy(), "sample.kl");
                assert_eq!(output.to_string_lossy(), "sample");
                assert_eq!(parsed.config.native_target, NativeTarget::LinuxX86_64);
                assert!(!parsed.config.explicit_target);
            }
            other => panic!("unexpected action: {other:?}"),
        }
    }

    #[test]
    fn parses_gc_log_and_gc_stress_flags() {
        let args = vec![
            "--gc-log".to_string(),
            "--gc-stress".to_string(),
            "build".to_string(),
            "sample.kl".to_string(),
            "-o".to_string(),
            "sample".to_string(),
        ];
        let parsed = parse_command_line(&args).expect("build command should parse");
        assert!(parsed.config.gc_log);
        assert!(parsed.config.gc_stress);
        assert!(matches!(parsed.action, RunAction::BuildFile { .. }));
    }

    #[test]
    fn parses_gc_poison_flag() {
        let args = vec![
            "--gc-poison".to_string(),
            "build".to_string(),
            "sample.kl".to_string(),
            "-o".to_string(),
            "sample".to_string(),
        ];
        let parsed = parse_command_line(&args).expect("build command should parse");
        assert!(parsed.config.gc_poison);
        assert!(matches!(parsed.action, RunAction::BuildFile { .. }));
    }

    #[test]
    fn parses_gc_mode_selector() {
        let build = |flag: &str| {
            vec![
                flag.to_string(),
                "build".to_string(),
                "sample.kl".to_string(),
                "-o".to_string(),
                "sample".to_string(),
            ]
        };
        let parsed = parse_command_line(&build("--gc=generational")).expect("should parse");
        assert!(parsed.config.gc_generational);
        let parsed = parse_command_line(&build("--gc=legacy")).expect("should parse");
        assert!(!parsed.config.gc_generational);
        match parse_command_line(&build("--gc=nope")) {
            Err(error @ CommandLineError::UnknownGcMode(_)) => {
                assert!(render_command_line_error(&error).contains("unknown --gc mode 'nope'"));
            }
            other => panic!("expected UnknownGcMode, got {other:?}"),
        }
    }

    #[test]
    fn gc_flags_default_off() {
        let args = vec![
            "build".to_string(),
            "sample.kl".to_string(),
            "-o".to_string(),
            "sample".to_string(),
        ];
        let parsed = parse_command_line(&args).expect("build command should parse");
        assert!(!parsed.config.gc_log);
        assert!(!parsed.config.gc_stress);
        assert!(!parsed.config.gc_poison);
        assert!(!parsed.config.gc_generational);
    }

    #[test]
    fn parses_native_build_invocation_with_explicit_target() {
        for target in ["linux-x86_64", "x86_64-unknown-linux-gnu"] {
            let args = vec![
                "build".to_string(),
                "--target".to_string(),
                target.to_string(),
                "sample.kl".to_string(),
                "-o".to_string(),
                "sample".to_string(),
            ];
            let parsed = parse_command_line(&args).expect("build command should parse");
            assert_eq!(parsed.config.native_target, NativeTarget::LinuxX86_64);
            assert!(parsed.config.explicit_target);
            match parsed.action {
                RunAction::BuildFile { input, output } => {
                    assert_eq!(input.to_string_lossy(), "sample.kl");
                    assert_eq!(output.to_string_lossy(), "sample");
                }
                other => panic!("unexpected action: {other:?}"),
            }
        }
    }

    #[cfg(all(target_os = "linux", target_arch = "x86_64"))]
    #[test]
    fn parses_native_build_invocation_with_native_target_alias() {
        let args = vec![
            "build".to_string(),
            "--target".to_string(),
            "native".to_string(),
            "sample.kl".to_string(),
            "-o".to_string(),
            "sample".to_string(),
        ];
        let parsed = parse_command_line(&args).expect("build command should parse");
        assert_eq!(parsed.config.native_target, NativeTarget::host().unwrap());
        match parsed.action {
            RunAction::BuildFile { input, output } => {
                assert_eq!(input.to_string_lossy(), "sample.kl");
                assert_eq!(output.to_string_lossy(), "sample");
            }
            other => panic!("unexpected action: {other:?}"),
        }
    }

    #[test]
    fn rejects_unknown_native_target() {
        let args = vec![
            "build".to_string(),
            "--target".to_string(),
            "nope-not-a-triple".to_string(),
            "sample.kl".to_string(),
            "-o".to_string(),
            "sample".to_string(),
        ];
        match parse_command_line(&args).unwrap_err() {
            CommandLineError::UnknownTarget(name) => assert_eq!(name, "nope-not-a-triple"),
            other => panic!("unexpected error: {other:?}"),
        }
    }

    #[test]
    fn flags_planned_native_target() {
        let args = vec![
            "build".to_string(),
            "--target".to_string(),
            "aarch64-unknown-linux-gnu".to_string(),
            "sample.kl".to_string(),
            "-o".to_string(),
            "sample".to_string(),
        ];
        match parse_command_line(&args).unwrap_err() {
            CommandLineError::PlannedTarget { argument, planned } => {
                assert_eq!(argument, "aarch64-unknown-linux-gnu");
                assert_eq!(planned.triple, "aarch64-unknown-linux-gnu");
                assert_eq!(planned.tier, 1);
            }
            other => panic!("unexpected error: {other:?}"),
        }
    }

    #[test]
    fn parses_targets_subcommand() {
        let args = vec!["targets".to_string()];
        let parsed = parse_command_line(&args).expect("targets command should parse");
        assert_eq!(parsed.action, RunAction::ListTargets);
    }

    #[test]
    fn parses_version_flag() {
        for flag in ["--version", "-V"] {
            let args = vec![flag.to_string()];
            let parsed = parse_command_line(&args).expect("version flag should parse");
            assert_eq!(parsed.action, RunAction::ShowVersion);
        }
    }

    #[test]
    fn renders_planned_target_error_with_help() {
        let args = vec![
            "build".to_string(),
            "--target".to_string(),
            "wasm32-wasi".to_string(),
            "sample.kl".to_string(),
            "-o".to_string(),
            "sample".to_string(),
        ];
        let rendered = render_command_line_error(&parse_command_line(&args).unwrap_err());
        assert!(rendered.contains("wasm32-wasi"));
        assert!(rendered.contains("recognized but not implemented yet"));
        assert!(rendered.contains("tier 2"));
        assert!(rendered.contains("wasm"));
    }

    #[test]
    fn usage_lists_supported_native_targets_from_registry() {
        assert!(usage().contains(&NativeTarget::supported_names_csv()));
    }

    #[test]
    fn parses_run_subcommand_with_script_args() {
        let args = vec![
            "run".to_string(),
            "scripts/wc.kl".to_string(),
            "--".to_string(),
            "README.md".to_string(),
            "--verbose".to_string(),
        ];
        let parsed = parse_command_line(&args).expect("run command should parse");
        match parsed.action {
            RunAction::EvaluateFile(path) => assert_eq!(path.to_string_lossy(), "scripts/wc.kl"),
            other => panic!("unexpected action: {other:?}"),
        }
        assert_eq!(
            parsed.script_args,
            vec!["README.md".to_string(), "--verbose".to_string()]
        );
    }

    #[test]
    fn positional_file_invocation_accepts_script_args_after_double_dash() {
        let args = vec![
            "sample.kl".to_string(),
            "--".to_string(),
            "alpha".to_string(),
            "beta".to_string(),
        ];
        let parsed = parse_command_line(&args).expect("positional invocation should parse");
        match parsed.action {
            RunAction::EvaluateFile(path) => assert_eq!(path.to_string_lossy(), "sample.kl"),
            other => panic!("unexpected action: {other:?}"),
        }
        assert_eq!(
            parsed.script_args,
            vec!["alpha".to_string(), "beta".to_string()]
        );
    }

    #[test]
    fn large_pages_parse_modes_and_reject_unsupported_scope_target_and_alignment() {
        for args in [
            vec!["--gc-large-pages", "try"],
            vec!["--gc-large-pages=try"],
        ] {
            let mut a = vec!["--gc=generational"];
            a.extend(args);
            a.extend(["build", "x.kl", "-o", "x"]);
            let c = parse_command_line(&a.into_iter().map(str::to_owned).collect::<Vec<_>>())
                .unwrap()
                .config;
            assert_eq!(c.gc_large_pages, Some(klassic_native::GcLargePages::Try));
        }
        for args in [
            vec!["--gc-large-pages=off", "build", "x.kl", "-o", "x"],
            vec!["--gc=generational", "--gc-large-pages=try", "-e", "1"],
            vec![
                "--gc=generational",
                "--gc-large-pages=try",
                "--backend",
                "c",
                "build",
                "x.kl",
                "-o",
                "x",
            ],
            vec![
                "--gc=generational",
                "--gc-large-pages=try",
                "--backend",
                "llvm",
                "build",
                "x.kl",
                "-o",
                "x",
            ],
            vec![
                "--gc=generational",
                "--gc-large-pages=required",
                "--gc-min-heap=128KiB",
                "build",
                "x.kl",
                "-o",
                "x",
            ],
            vec![
                "--gc=generational",
                "--gc-large-pages=required",
                "--target",
                "aarch64-apple-darwin",
                "build",
                "x.kl",
                "-o",
                "x",
            ],
            vec![
                "--gc=generational",
                "--gc-large-pages=try",
                "--target",
                "x86_64-pc-windows-msvc",
                "build",
                "x.kl",
                "-o",
                "x",
            ],
            vec![
                "--gc=generational",
                "--gc-large-pages=bad",
                "build",
                "x.kl",
                "-o",
                "x",
            ],
            vec!["--gc=generational", "--gc-large-pages"],
        ] {
            assert!(
                parse_command_line(&args.into_iter().map(str::to_owned).collect::<Vec<_>>())
                    .is_err()
            );
        }
        let a = [
            "--gc=generational",
            "--gc-large-pages=required",
            "--gc-min-heap=4MiB",
            "--gc-max-heap=64MiB",
            "build",
            "x.kl",
            "-o",
            "x",
        ]
        .map(str::to_owned);
        assert_eq!(
            parse_command_line(&a).unwrap().config.gc_large_pages,
            Some(klassic_native::GcLargePages::Required)
        );
    }

    #[test]
    fn pre_touch_requires_a_generational_direct_native_build_and_stays_opt_in() {
        let build = ["--gc=generational", "build", "x.kl", "-o", "x"];
        assert!(
            !parse_command_line(&build.map(str::to_owned))
                .unwrap()
                .config
                .gc_pre_touch
        );
        let mut enabled = build.to_vec();
        enabled.insert(0, "--gc-pre-touch");
        assert!(
            parse_command_line(&enabled.into_iter().map(str::to_owned).collect::<Vec<_>>())
                .unwrap()
                .config
                .gc_pre_touch
        );
        for args in [
            vec!["--gc-pre-touch", "build", "x.kl", "-o", "x"],
            vec!["--gc=generational", "--gc-pre-touch", "-e", "1"],
            vec![
                "--gc=generational",
                "--gc-pre-touch",
                "--backend",
                "c",
                "build",
                "x.kl",
                "-o",
                "x.c",
            ],
            vec![
                "--gc=generational",
                "--gc-pre-touch",
                "--backend",
                "llvm",
                "build",
                "x.kl",
                "-o",
                "x",
            ],
            vec![
                "--gc=generational",
                "--gc-pre-touch=false",
                "build",
                "x.kl",
                "-o",
                "x",
            ],
        ] {
            assert!(
                parse_command_line(&args.into_iter().map(str::to_owned).collect::<Vec<_>>())
                    .is_err()
            );
        }
    }

    #[test]
    fn soft_heap_target_accepts_both_spellings_and_validates_order_and_scope() {
        for option in [
            vec!["--gc-soft-max-heap", "32MiB"],
            vec!["--gc-soft-max-heap=32MiB"],
        ] {
            let mut args = vec![
                "--gc=generational",
                "--gc-min-heap=4MiB",
                "--gc-max-heap=64MiB",
            ];
            args.extend(option);
            args.extend(["build", "x.kl", "-o", "x"]);
            let args = args.into_iter().map(str::to_owned).collect::<Vec<_>>();
            let config = parse_command_line(&args).unwrap().config;
            assert_eq!(config.gc_soft_max_heap, Some(32 << 20));
            assert_eq!(config.gc_max_heap, Some(64 << 20));
        }
        for options in [
            vec!["--gc=generational", "--gc-soft-max-heap"],
            vec!["--gc=generational", "--gc-soft-max-heap=0"],
            vec!["--gc=generational", "--gc-soft-max-heap=16777217"],
            vec!["--gc=generational", "--gc-soft-max-heap=8MiB"],
            vec!["--gc=generational", "--gc-soft-max-heap=512MiB"],
            vec![
                "--gc=generational",
                "--gc-soft-max-heap=18446744073709551615GiB",
            ],
            vec!["--gc-soft-max-heap=32MiB"],
        ] {
            let mut args = options.into_iter().map(str::to_owned).collect::<Vec<_>>();
            // Missing value must remain last so it is diagnosed directly.
            if args.last().unwrap() != "--gc-soft-max-heap" {
                args.extend(["build", "x.kl", "-o", "x"].map(str::to_owned));
            }
            assert!(parse_command_line(&args).is_err(), "{args:?}");
        }
        for backend in ["c", "llvm"] {
            let args = [
                "--gc=generational",
                "--gc-soft-max-heap=32MiB",
                "--backend",
                backend,
                "build",
                "x.kl",
                "-o",
                "x",
            ]
            .map(str::to_owned);
            assert_eq!(
                parse_command_line(&args).unwrap().config.gc_soft_max_heap,
                Some(32 << 20)
            );
        }
        let args = ["--gc=generational", "--gc-soft-max-heap=32MiB", "-e", "1"].map(str::to_owned);
        assert!(parse_command_line(&args).is_err());
    }

    #[test]
    fn metadata_budget_accepts_both_spellings_and_is_independent_of_payload() {
        for option in [
            vec!["--gc-max-metadata", "4GiB"],
            vec!["--gc-max-metadata=4GiB"],
        ] {
            let args: Vec<String> = [
                vec!["--gc=generational", "--gc-max-heap=16MiB"],
                option,
                vec!["build", "x.kl", "-o", "x"],
            ]
            .concat()
            .into_iter()
            .map(str::to_owned)
            .collect();
            let c = parse_command_line(&args).unwrap().config;
            assert_eq!(c.gc_max_metadata, Some(4 << 30));
            assert_eq!(c.gc_max_heap, Some(16 << 20));
        }
        assert_eq!(
            super::ExecutionConfig::new(false, false, NativeTarget::LinuxX86_64).gc_max_metadata,
            None
        );
    }

    #[test]
    fn metadata_budget_rejects_invalid_values_and_unsupported_execution_modes() {
        for args in [
            vec!["--gc=generational", "--gc-max-metadata"],
            vec![
                "--gc=generational",
                "--gc-max-metadata=0",
                "build",
                "x.kl",
                "-o",
                "x",
            ],
            vec![
                "--gc=generational",
                "--gc-max-metadata=4097",
                "build",
                "x.kl",
                "-o",
                "x",
            ],
            vec![
                "--gc=generational",
                "--gc-max-metadata=9223372036854775808",
                "build",
                "x.kl",
                "-o",
                "x",
            ],
            vec!["--gc-max-metadata=4MiB", "build", "x.kl", "-o", "x"],
            vec!["--gc=generational", "--gc-max-metadata=4MiB", "-e", "1"],
            vec![
                "--gc=generational",
                "--gc-max-metadata=4MiB",
                "--backend",
                "c",
                "build",
                "x.kl",
                "-o",
                "x",
            ],
            vec![
                "--gc=generational",
                "--gc-max-metadata=4MiB",
                "--backend",
                "llvm",
                "build",
                "x.kl",
                "-o",
                "x",
            ],
        ] {
            let args: Vec<String> = args.into_iter().map(str::to_owned).collect();
            assert!(parse_command_line(&args).is_err(), "{args:?}");
        }
    }

    #[test]
    fn expression_invocation_without_separator_has_no_script_args() {
        let args = vec!["-e".to_string(), "1 + 2".to_string()];
        let parsed = parse_command_line(&args).expect("expression invocation should parse");
        assert!(parsed.script_args.is_empty());
    }
}

#[cfg(test)]
mod heap_tests {
    use super::*;
    fn args(values: &[&str]) -> Vec<String> {
        values.iter().map(|s| (*s).into()).collect()
    }
    #[test]
    fn minimum_heap_requires_native_generation_and_orders_effective_limits() {
        let a = args(&[
            "--gc=generational",
            "--gc-max-heap=16MiB",
            "--gc-min-heap=128KiB",
            "build",
            "input.kl",
            "-o",
            "output",
        ]);
        assert_eq!(
            parse_command_line(&a).unwrap().config.gc_min_heap,
            Some(128 << 10)
        );
        for flags in [
            vec!["--gc-min-heap=4MiB"],
            vec![
                "--gc=generational",
                "--gc-min-heap=32MiB",
                "--gc-max-heap=16MiB",
            ],
        ] {
            let mut a = args(&flags);
            a.extend(args(&["build", "input.kl", "-o", "output"]));
            assert!(matches!(
                parse_command_line(&a),
                Err(CommandLineError::InvalidGcHeap(_))
            ));
        }
    }
    #[test]
    fn maximum_heap_requires_a_generational_native_build_and_accepts_both_forms() {
        for flags in [
            ["--gc-max-heap", "16MiB"].as_slice(),
            ["--gc-max-heap=16MiB"].as_slice(),
        ] {
            let mut a = vec!["--gc=generational".to_owned()];
            a.extend(args(flags));
            a.extend(args(&["build", "input.kl", "-o", "output"]));
            assert_eq!(
                parse_command_line(&a).unwrap().config.gc_max_heap,
                Some(16 << 20)
            );
        }
        for values in [
            vec![
                "--gc-max-heap",
                "16MiB",
                "build",
                "input.kl",
                "-o",
                "output",
            ],
            vec!["--gc=generational", "--gc-max-heap=16MiB", "-e", "1"],
            vec!["--gc=generational", "--gc-max-heap"],
        ] {
            assert!(matches!(
                parse_command_line(&args(&values)),
                Err(CommandLineError::InvalidGcHeap(_))
            ));
        }
    }
    #[test]
    fn uncommit_delay_requires_generational_native_build_and_accepts_both_forms() {
        for flag in [
            vec!["--gc-uncommit-delay", "2ms"],
            vec!["--gc-uncommit-delay=2ms"],
        ] {
            let mut a = vec!["--gc=generational".to_owned()];
            a.extend(flag.into_iter().map(str::to_owned));
            a.extend(["build", "x.kl", "-o", "x"].map(str::to_owned));
            assert_eq!(
                parse_command_line(&a).unwrap().config.gc_uncommit_delay_ns,
                Some(2_000_000)
            );
        }
        for backend in ["c", "llvm"] {
            let args = [
                "--gc=generational",
                "--backend",
                backend,
                "--gc-uncommit-delay=1s",
                "build",
                "x.kl",
                "-o",
                "x",
            ]
            .map(str::to_owned);
            assert_eq!(
                parse_command_line(&args)
                    .unwrap()
                    .config
                    .gc_uncommit_delay_ns,
                Some(1000000000)
            );
        }
        for a in [
            vec!["--gc-uncommit-delay=1s", "build", "x.kl", "-o", "x"],
            vec!["--gc=generational", "--gc-uncommit-delay=1s", "-e", "1"],
            vec!["--gc=generational", "--gc-uncommit-delay"],
            vec![
                "--gc=generational",
                "--gc-uncommit-delay=18446744074s",
                "build",
                "x.kl",
                "-o",
                "x",
            ],
        ] {
            assert!(
                parse_command_line(&a.into_iter().map(str::to_owned).collect::<Vec<_>>()).is_err()
            );
        }
    }
    #[test]
    fn log_format_parses_and_enables_logging_but_rejects_unsupported_scope() {
        for mode in ["text", "json"] {
            for flag in [
                vec![format!("--gc-log-format={mode}")],
                vec!["--gc-log-format".into(), mode.into()],
            ] {
                let mut args = vec!["--gc=generational".to_owned()];
                args.extend(flag);
                args.extend(["build", "x.kl", "-o", "x"].map(str::to_owned));
                let c = parse_command_line(&args).unwrap().config;
                assert!(c.gc_log);
                assert_eq!(c.gc_log_format, Some(mode.parse().unwrap()));
            }
        }
        for args in [
            vec!["--gc-log-format"],
            vec![
                "--gc=generational",
                "--gc-log-format=yaml",
                "build",
                "x.kl",
                "-o",
                "x",
            ],
            vec!["--gc-log-format=text", "build", "x.kl", "-o", "x"],
            vec![
                "--gc=generational",
                "--gc-log-format=json",
                "--backend",
                "c",
                "build",
                "x.kl",
                "-o",
                "x",
            ],
            vec![
                "--gc=generational",
                "--gc-log-format=json",
                "--backend",
                "llvm",
                "build",
                "x.kl",
                "-o",
                "x",
            ],
            vec!["--gc=generational", "--gc-log-format=json", "-e", "1"],
            vec!["--gc=generational", "--gc-log-format=json"],
        ] {
            assert!(
                parse_command_line(&args.into_iter().map(str::to_owned).collect::<Vec<_>>())
                    .is_err()
            );
        }
    }
}
