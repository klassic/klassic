mod cli;

use std::fs;
use std::io::{self, Write};
#[cfg(unix)]
use std::os::unix::fs::PermissionsExt;
use std::process::ExitCode;

use cli::{
    ExecutionConfig, ParsedCommand, RunAction, parse_command_line, render_command_line_error,
};
use klassic_eval::{Evaluator, EvaluatorConfig};
use klassic_native::{NativeCompilerConfig, NativeTarget, compile_source_with_prelude_for_target};

/// The standard prelude is bundled into the compiler at build time. It is
/// loaded as a separate translation unit so user code is parsed in its own
/// SourceFile — line numbers in diagnostics and runtime error messages
/// stay 1-based for the user's view of their .kl file.
const STDLIB_PRELUDE: &str = include_str!("../stdlib/prelude.kl");
const STDLIB_PRELUDE_NAME: &str = "<stdlib>";

fn main() -> ExitCode {
    let args = std::env::args().skip(1).collect::<Vec<_>>();
    let command = match parse_command_line(&args) {
        Ok(command) => command,
        Err(error) => {
            eprint!("{}", render_command_line_error(&error));
            return ExitCode::from(1);
        }
    };

    match run(command) {
        Ok(()) => ExitCode::SUCCESS,
        Err(code) => ExitCode::from(code),
    }
}

fn run(command: ParsedCommand) -> Result<(), u8> {
    match command.action {
        RunAction::BuildFile { input, output } => {
            let text = match fs::read_to_string(&input) {
                Ok(text) => text,
                Err(error) => {
                    eprintln!("{}: {error}", input.display());
                    return Err(1);
                }
            };
            let config = NativeCompilerConfig {
                target: command.config.native_target,
                deny_trust: command.config.deny_trust,
                warn_trust: command.config.warn_trust,
            };
            let bytes = match compile_source_with_prelude_for_target(
                &input.display().to_string(),
                STDLIB_PRELUDE,
                &text,
                config,
            ) {
                Ok(bytes) => bytes,
                Err(error) => {
                    eprintln!("{error}");
                    return Err(1);
                }
            };
            if let Err(error) = fs::write(&output, bytes) {
                eprintln!("{}: {error}", output.display());
                return Err(1);
            }
            #[cfg(unix)]
            if let Err(error) = fs::set_permissions(&output, fs::Permissions::from_mode(0o755)) {
                eprintln!("{}: {error}", output.display());
                return Err(1);
            }
            Ok(())
        }
        RunAction::EvaluateExpression(expression) => {
            let mut evaluator = prepare_evaluator(EvaluatorConfig {
                deny_trust: command.config.deny_trust,
                warn_trust: command.config.warn_trust,
            })?;
            match evaluator.evaluate_text("<expression>", &expression) {
                Ok(value) => {
                    println!("{value}");
                    Ok(())
                }
                Err(error) => {
                    eprintln!("{error}");
                    Err(1)
                }
            }
        }
        RunAction::EvaluateFile(path) => {
            let text = match fs::read_to_string(&path) {
                Ok(text) => text,
                Err(error) => {
                    eprintln!("{}: {error}", path.display());
                    return Err(1);
                }
            };
            let mut evaluator = prepare_evaluator(EvaluatorConfig {
                deny_trust: command.config.deny_trust,
                warn_trust: command.config.warn_trust,
            })?;
            match evaluator.evaluate_text(&path.display().to_string(), &text) {
                Ok(_) => Ok(()),
                Err(error) => {
                    eprintln!("{error}");
                    Err(1)
                }
            }
        }
        RunAction::StartRepl => {
            start_repl(command.config);
            Ok(())
        }
        RunAction::ListTargets => {
            println!("{}", NativeTarget::target_matrix());
            Ok(())
        }
    }
}

fn prepare_evaluator(config: EvaluatorConfig) -> Result<Evaluator, u8> {
    let mut evaluator = Evaluator::with_config(config);
    if let Err(error) = evaluator.evaluate_text(STDLIB_PRELUDE_NAME, STDLIB_PRELUDE) {
        eprintln!("{error}");
        return Err(1);
    }
    for module in klassic_runtime::STDLIB_MODULES {
        if let Err(error) = evaluator.evaluate_text(module.diagnostic_name, module.source) {
            eprintln!("{error}");
            return Err(1);
        }
    }
    Ok(evaluator)
}

fn start_repl(config: ExecutionConfig) {
    let mut history = Vec::<String>::new();
    let mut buffer = String::new();
    let mut evaluator = match prepare_evaluator(EvaluatorConfig {
        deny_trust: config.deny_trust,
        warn_trust: config.warn_trust,
    }) {
        Ok(evaluator) => evaluator,
        Err(_) => return,
    };
    let _ = STDLIB_PRELUDE_NAME; // keep marker available for future REPL diagnostics

    loop {
        print!("{}", if buffer.is_empty() { "> " } else { "| " });
        let _ = io::stdout().flush();

        let mut line = String::new();
        match io::stdin().read_line(&mut line) {
            Ok(0) | Err(_) => break,
            Ok(_) => {}
        }

        let trimmed = line.trim_end_matches(['\r', '\n']);
        if buffer.is_empty() && trimmed == ":exit" {
            break;
        }
        if buffer.is_empty() && trimmed == ":history" {
            for (index, command) in history.iter().enumerate() {
                println!("{}: {}", index + 1, command);
            }
            continue;
        }

        buffer.push_str(trimmed);
        buffer.push('\n');

        match evaluator.evaluate_text("<repl>", &buffer) {
            Ok(value) => {
                println!("value = {value}");
                history.push(buffer.trim_end().to_string());
                buffer.clear();
            }
            Err(error) if error.is_incomplete() => {}
            Err(error) => {
                println!("Error: {error}");
                buffer.clear();
            }
        }
    }
}
