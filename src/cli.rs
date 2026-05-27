use std::path::PathBuf;

use klassic_native::{NativeTarget, PlannedTarget, TargetRecognition};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ExecutionConfig {
    pub deny_trust: bool,
    pub warn_trust: bool,
    pub native_target: NativeTarget,
}

impl ExecutionConfig {
    pub const fn new(deny_trust: bool, warn_trust: bool, native_target: NativeTarget) -> Self {
        Self {
            deny_trust,
            warn_trust,
            native_target,
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
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ParsedCommand {
    pub action: RunAction,
    pub config: ExecutionConfig,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CommandLineError {
    /// Arguments don't match any recognised invocation. Driver should
    /// print the standard usage block.
    Usage,
    /// `--target` was given without a value.
    MissingTargetArgument,
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
    let mut warn_trust = false;
    let mut native_target = NativeTarget::default();
    let mut others = Vec::new();
    let mut index = 0usize;

    while index < args.len() {
        match args[index].as_str() {
            "--deny-trust" => {
                deny_trust = true;
                index += 1;
            }
            "--warn-trust" => {
                warn_trust = true;
                index += 1;
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
                index += 2;
            }
            other => {
                others.push(other.to_owned());
                index += 1;
            }
        }
    }

    let config = ExecutionConfig::new(deny_trust, warn_trust, native_target);
    let action = match others.as_slice() {
        [command] if command == "targets" => RunAction::ListTargets,
        [] => RunAction::StartRepl,
        [file_name] if file_name.ends_with(".kl") => {
            RunAction::EvaluateFile(PathBuf::from(file_name))
        }
        [flag, file_name] if flag == "-f" => RunAction::EvaluateFile(PathBuf::from(file_name)),
        [flag, expression] if flag == "-e" => RunAction::EvaluateExpression(expression.clone()),
        [command, input, output_flag, output] if command == "build" && output_flag == "-o" => {
            RunAction::BuildFile {
                input: PathBuf::from(input),
                output: PathBuf::from(output),
            }
        }
        _ => return Err(CommandLineError::Usage),
    };

    Ok(ParsedCommand { action, config })
}

pub fn usage() -> String {
    format!(
        "Usage: klassic [--deny-trust] [--warn-trust] [--target <target>] (-f <fileName> | -e <expression> | build <fileName> -o <output> | targets)\n\
     Options:\n\
       --deny-trust   : reject programs that depend on trusted proofs\n\
       --warn-trust   : warn when trusted proofs are used\n\
       --target <target>: native build target (supported: {})\n\
       <fileName>     : read a program from <fileName> and execute it\n\
       -e <expression>: evaluate <expression>\n\
       build <fileName> -o <output>: compile <fileName> to a native executable\n\
       targets        : list known native targets and their support status\n",
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
            }
            other => panic!("unexpected action: {other:?}"),
        }
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
}
