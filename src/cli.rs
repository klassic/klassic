use std::path::PathBuf;

use klassic_native::NativeTarget;

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
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ParsedCommand {
    pub action: RunAction,
    pub config: ExecutionConfig,
}

pub fn parse_command_line(args: &[String]) -> Option<ParsedCommand> {
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
                let target_name = args.get(index + 1)?;
                native_target = NativeTarget::parse(target_name)?;
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
        _ => return None,
    };

    Some(ParsedCommand { action, config })
}

pub fn usage() -> String {
    format!(
        "Usage: klassic [--deny-trust] [--warn-trust] [--target <target>] (-f <fileName> | -e <expression> | build <fileName> -o <output>)\n\
     Options:\n\
       --deny-trust   : reject programs that depend on trusted proofs\n\
       --warn-trust   : warn when trusted proofs are used\n\
       --target <target>: native build target (supported: {})\n\
       <fileName>     : read a program from <fileName> and execute it\n\
       -e <expression>: evaluate <expression>\n\
       build <fileName> -o <output>: compile <fileName> to a native Linux x86_64 executable\n",
        NativeTarget::SUPPORTED_NAMES.join(", ")
    )
}

#[cfg(test)]
mod tests {
    use klassic_native::NativeTarget;

    use super::{RunAction, parse_command_line, usage};

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
            "linux-aarch64".to_string(),
            "sample.kl".to_string(),
            "-o".to_string(),
            "sample".to_string(),
        ];
        assert!(parse_command_line(&args).is_none());
    }

    #[test]
    fn usage_lists_supported_native_targets_from_registry() {
        assert!(usage().contains(&NativeTarget::SUPPORTED_NAMES.join(", ")));
    }
}
