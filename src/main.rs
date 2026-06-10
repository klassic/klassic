mod cli;

use std::fs;
use std::io::{self, Write};
#[cfg(unix)]
use std::os::unix::fs::PermissionsExt;
use std::process::ExitCode;

use cli::{
    ExecutionConfig, ParsedCommand, RunAction, parse_command_line, render_command_line_error,
};
use klassic_eval::{Evaluator, EvaluatorConfig, set_script_args};
use klassic_native::{
    NativeCompilerConfig, NativeTarget, UserModuleSource, compile_source_to_c,
    compile_source_with_prelude_and_modules_for_target,
};
use std::path::Path;

/// The standard prelude is bundled into the compiler at build time. It is
/// loaded as a separate translation unit so user code is parsed in its own
/// SourceFile — line numbers in diagnostics and runtime error messages
/// stay 1-based for the user's view of their .kl file.
const STDLIB_PRELUDE: &str = include_str!("../stdlib/prelude.kl");
const STDLIB_PRELUDE_NAME: &str = "<stdlib>";

/// If the source begins with a `#!` shebang line, replace the leading
/// `#!` bytes with `//` so the rest of the line is read as a normal
/// Klassic comment. Byte offsets stay identical, so spans and line /
/// column numbers in diagnostics are unaffected.
fn strip_shebang(text: String) -> String {
    if !text.starts_with("#!") {
        return text;
    }
    let mut bytes = text.into_bytes();
    bytes[0] = b'/';
    bytes[1] = b'/';
    String::from_utf8(bytes).expect("replacing two ASCII bytes preserves UTF-8")
}

fn main() -> ExitCode {
    let args = std::env::args().skip(1).collect::<Vec<_>>();
    let command = match parse_command_line(&args) {
        Ok(command) => command,
        Err(error) => {
            eprint!("{}", render_command_line_error(&error));
            return ExitCode::from(1);
        }
    };

    set_script_args(command.script_args.clone());

    match run(command) {
        Ok(()) => ExitCode::SUCCESS,
        Err(code) => ExitCode::from(code),
    }
}

fn run(command: ParsedCommand) -> Result<(), u8> {
    match command.action {
        RunAction::BuildFile { input, output } => {
            let text = match fs::read_to_string(&input) {
                Ok(text) => strip_shebang(text),
                Err(error) => {
                    eprintln!("{}: {error}", input.display());
                    return Err(1);
                }
            };
            if command.config.c_backend {
                let c_source = match compile_source_to_c(&input.display().to_string(), &text) {
                    Ok(c_source) => c_source,
                    Err(error) => {
                        eprintln!("{error}");
                        return Err(1);
                    }
                };
                if let Err(error) = fs::write(&output, c_source) {
                    eprintln!("{}: {error}", output.display());
                    return Err(1);
                }
                return Ok(());
            }
            let config = NativeCompilerConfig {
                target: command.config.native_target,
                deny_trust: command.config.deny_trust,
                warn_trust: command.config.warn_trust,
            };
            let user_modules = user_modules_for(&input, &text)?;
            let bytes = match compile_source_with_prelude_and_modules_for_target(
                &input.display().to_string(),
                STDLIB_PRELUDE,
                &text,
                &user_modules,
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
                Ok(text) => strip_shebang(text),
                Err(error) => {
                    eprintln!("{}: {error}", path.display());
                    return Err(1);
                }
            };
            let mut evaluator = prepare_evaluator(EvaluatorConfig {
                deny_trust: command.config.deny_trust,
                warn_trust: command.config.warn_trust,
            })?;
            for module in user_modules_for(&path, &text)? {
                if let Err(error) = evaluator.evaluate_text(&module.path, &module.source) {
                    eprintln!("{error}");
                    return Err(1);
                }
            }
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
        RunAction::ShowVersion => {
            println!("klassic {}", env!("CARGO_PKG_VERSION"));
            Ok(())
        }
    }
}

/// Discover user module files imported (directly or transitively) by
/// `text`. A dotted import path resolves relative to `base_dir`:
/// `import util.text` tries `util/text.kl` then `util.text.kl`, and a
/// selective import (`import mylib.{f}` / `import mylib.f`) retries
/// with the final segment dropped. Bundled `std.*` modules and paths
/// with no matching file are skipped (the type checker keeps its
/// `unknown module` diagnostic for those). Modules are returned in
/// dependency order; an import cycle is reported as an error.
fn collect_user_modules(
    base_dir: &Path,
    text: &str,
    loading: &mut Vec<String>,
    out: &mut Vec<UserModuleSource>,
) -> Result<(), String> {
    for line in text.lines() {
        let trimmed = line.trim_start();
        let Some(rest) = trimmed.strip_prefix("import ") else {
            continue;
        };
        let token = rest.split_whitespace().next().unwrap_or("");
        let token = token.split(".{").next().unwrap_or(token);
        let token = token.strip_suffix("._").unwrap_or(token);
        let token = token.trim_end_matches(';');
        if token.is_empty() || token.starts_with("std.") || token == "std" {
            continue;
        }
        let mut candidates = vec![token.to_string()];
        if let Some((parent, _)) = token.rsplit_once('.') {
            candidates.push(parent.to_string());
        }
        let resolved = candidates.iter().find_map(|module_path| {
            let slashed = base_dir.join(format!("{}.kl", module_path.replace('.', "/")));
            let dotted = base_dir.join(format!("{module_path}.kl"));
            if slashed.is_file() {
                Some((module_path.clone(), slashed))
            } else if dotted.is_file() {
                Some((module_path.clone(), dotted))
            } else {
                None
            }
        });
        let Some((module_path, file)) = resolved else {
            continue;
        };
        if out.iter().any(|module| module.path == module_path) {
            continue;
        }
        if loading.contains(&module_path) {
            return Err(format!(
                "import cycle detected through module `{module_path}`"
            ));
        }
        let source = match fs::read_to_string(&file) {
            Ok(source) => strip_shebang(source),
            Err(error) => return Err(format!("{}: {error}", file.display())),
        };
        loading.push(module_path.clone());
        collect_user_modules(base_dir, &source, loading, out)?;
        loading.pop();
        out.push(UserModuleSource {
            path: module_path,
            source,
        });
    }
    Ok(())
}

fn user_modules_for(path: &Path, text: &str) -> Result<Vec<UserModuleSource>, u8> {
    let base_dir = path.parent().unwrap_or_else(|| Path::new("."));
    let mut modules = Vec::new();
    let mut loading = Vec::new();
    if let Err(error) = collect_user_modules(base_dir, text, &mut loading, &mut modules) {
        eprintln!("{error}");
        return Err(1);
    }
    Ok(modules)
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

const REPL_HELP: &str = ":help          show this help\n\
                        :quit | :exit  leave the REPL\n\
                        :history       print previously evaluated input\n\
                        :load <path>   evaluate <path> in the current session\n\
                        :reset         reload the stdlib prelude and modules\n";

fn start_repl(config: ExecutionConfig) {
    let mut history = Vec::<String>::new();
    let mut buffer = String::new();
    let evaluator_config = EvaluatorConfig {
        deny_trust: config.deny_trust,
        warn_trust: config.warn_trust,
    };
    let mut evaluator = match prepare_evaluator(evaluator_config) {
        Ok(evaluator) => evaluator,
        Err(_) => return,
    };

    loop {
        print!("{}", if buffer.is_empty() { "> " } else { "| " });
        let _ = io::stdout().flush();

        let mut line = String::new();
        match io::stdin().read_line(&mut line) {
            Ok(0) | Err(_) => break,
            Ok(_) => {}
        }

        let trimmed = line.trim_end_matches(['\r', '\n']);
        if buffer.is_empty() {
            if trimmed == ":exit" || trimmed == ":quit" {
                break;
            }
            if trimmed == ":help" {
                print!("{REPL_HELP}");
                continue;
            }
            if trimmed == ":history" {
                for (index, command) in history.iter().enumerate() {
                    println!("{}: {}", index + 1, command);
                }
                continue;
            }
            if trimmed == ":reset" {
                evaluator = match prepare_evaluator(evaluator_config) {
                    Ok(evaluator) => evaluator,
                    Err(_) => return,
                };
                println!("(stdlib reloaded)");
                continue;
            }
            if let Some(path) = trimmed.strip_prefix(":load ").map(str::trim)
                && !path.is_empty()
            {
                match fs::read_to_string(path) {
                    Ok(text) => {
                        let text = strip_shebang(text);
                        match evaluator.evaluate_text(path, &text) {
                            Ok(value) => println!("loaded {path} = {value}"),
                            Err(error) => println!("Error: {error}"),
                        }
                    }
                    Err(error) => println!("{path}: {error}"),
                }
                continue;
            }
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
