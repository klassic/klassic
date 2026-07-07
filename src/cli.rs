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
    /// True when `--target` was given on the command line. Hosts with
    /// no direct native backend (e.g. macOS) route a target-less
    /// `build` through the portable C backend instead of silently
    /// cross-building for the default Linux target.
    pub explicit_target: bool,
    /// `--gc-log`: emitted programs print GC statistics to stderr at
    /// exit.
    pub gc_log: bool,
    /// `--gc-stress`: emitted `gc_alloc` collects before every
    /// allocation (deterministic detector for pointers left unrooted
    /// across an allocation).
    pub gc_stress: bool,
    /// `--gc-poison`: heap-stored pointers are colored BAD, so any
    /// unbarriered dereference faults on a non-canonical address and the
    /// load-barrier slow path runs on every load.
    pub gc_poison: bool,
}

impl ExecutionConfig {
    pub const fn new(deny_trust: bool, warn_trust: bool, native_target: NativeTarget) -> Self {
        Self {
            deny_trust,
            warn_trust,
            native_target,
            c_backend: false,
            explicit_target: false,
            gc_log: false,
            gc_stress: false,
            gc_poison: false,
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
}

pub fn parse_command_line(args: &[String]) -> Result<ParsedCommand, CommandLineError> {
    let mut deny_trust = false;
    let mut c_backend = false;
    let mut warn_trust = false;
    let mut native_target = NativeTarget::default();
    let mut explicit_target = false;
    let mut gc_log = false;
    let mut gc_stress = false;
    let mut gc_poison = false;
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
            "--backend" => {
                let Some(backend) = args.get(index + 1) else {
                    return Err(CommandLineError::UnknownBackend(String::new()));
                };
                if backend != "c" {
                    return Err(CommandLineError::UnknownBackend(backend.clone()));
                }
                c_backend = true;
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
    config.explicit_target = explicit_target;
    config.gc_log = gc_log;
    config.gc_stress = gc_stress;
    config.gc_poison = gc_poison;
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
       --backend c        : `build` emits a portable C translation unit instead of an ELF executable\n\
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
    fn expression_invocation_without_separator_has_no_script_args() {
        let args = vec!["-e".to_string(), "1 + 2".to_string()];
        let parsed = parse_command_line(&args).expect("expression invocation should parse");
        assert!(parsed.script_args.is_empty());
    }
}
