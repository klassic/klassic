//! Explicit native linking for canonical emitter output. This is independent
//! of executable CLI admission: callers must already have diagnosed unsupported
//! language, target and control settings before choosing this runtime.
use std::fmt;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};
use std::process::{Command, ExitStatus};
use std::sync::atomic::{AtomicU64, Ordering};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum GenerationalSourceKind {
    C,
    Llvm,
}
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum GenerationalInstrumentation {
    #[default]
    None,
    AddressUndefined,
    Thread,
}
impl GenerationalInstrumentation {
    fn apply(self, command: &mut Command, link: bool) {
        let sanitizer = match self {
            Self::None => return,
            Self::AddressUndefined => "address,undefined",
            Self::Thread => "thread",
        };
        command
            .arg(format!("-fsanitize={sanitizer}"))
            .arg("-fno-omit-frame-pointer");
        if link {
            command.arg("-no-pie");
        }
    }
}
#[derive(Clone, Copy, Debug)]
pub struct GenerationalLinkOptions<'a> {
    pub kind: GenerationalSourceKind,
    /// C11 compiler; LLVM input requires clang with opaque-pointer IR support.
    pub compiler: &'a Path,
    pub instrumentation: GenerationalInstrumentation,
    /// Matching canonical C sources and their shared headers.
    pub runtime_directory: &'a Path,
    /// Matching Rust scalar-printer static library.
    pub runtime_archive: &'a Path,
    pub output: &'a Path,
}
#[derive(Debug)]
pub enum GenerationalLinkError {
    UnsupportedHost,
    MissingInput {
        kind: &'static str,
        path: PathBuf,
    },
    Io {
        operation: &'static str,
        path: PathBuf,
        error: io::Error,
    },
    Compiler {
        stage: &'static str,
        compiler: PathBuf,
        status: ExitStatus,
        stderr: String,
    },
}
impl fmt::Display for GenerationalLinkError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnsupportedHost => write!(
                f,
                "canonical native linking requires a Linux x86-64 host and target"
            ),
            Self::MissingInput { kind, path } => write!(f, "{kind} not found: {}", path.display()),
            Self::Io {
                operation,
                path,
                error,
            } => write!(f, "{operation} {}: {error}", path.display()),
            Self::Compiler {
                stage,
                compiler,
                status,
                stderr,
            } => write!(
                f,
                "{} failed during {stage} ({status}):\n{stderr}",
                compiler.display()
            ),
        }
    }
}
impl std::error::Error for GenerationalLinkError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::Io { error, .. } => Some(error),
            _ => None,
        }
    }
}
const RUNTIME_SOURCES: [&str; 7] = [
    "klassic_gen_runtime.c",
    "klassic_gen_remembered.c",
    "gc_native_driver.c",
    "klassic_gen_policy.c",
    "klassic_gen_verify.c",
    "klassic_gen_string.c",
    "klassic_gen_scalar.c",
];
struct Scratch(PathBuf);
impl Drop for Scratch {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.0);
    }
}
fn io_error(operation: &'static str, path: &Path, error: io::Error) -> GenerationalLinkError {
    GenerationalLinkError::Io {
        operation,
        path: path.to_owned(),
        error,
    }
}
fn require_file(kind: &'static str, path: &Path) -> Result<(), GenerationalLinkError> {
    if path.is_file() {
        Ok(())
    } else {
        Err(GenerationalLinkError::MissingInput {
            kind,
            path: path.to_owned(),
        })
    }
}
fn scratch(output: &Path) -> Result<Scratch, GenerationalLinkError> {
    static NEXT: AtomicU64 = AtomicU64::new(0);
    let parent = output
        .parent()
        .filter(|p| !p.as_os_str().is_empty())
        .unwrap_or_else(|| Path::new("."));
    loop {
        let id = NEXT.fetch_add(1, Ordering::Relaxed);
        let time = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap_or_default()
            .as_nanos();
        let directory = parent.join(format!(
            ".klassic-canonical-{}-{time}-{id}",
            std::process::id()
        ));
        match fs::create_dir(&directory) {
            Ok(()) => return Ok(Scratch(directory)),
            Err(error) if error.kind() == io::ErrorKind::AlreadyExists => continue,
            Err(error) => return Err(io_error("create build directory", &directory, error)),
        }
    }
}
fn invoke(
    compiler: &Path,
    command: &mut Command,
    stage: &'static str,
) -> Result<(), GenerationalLinkError> {
    // output() drains both pipes while the compiler runs; diagnostics cannot
    // fill a pipe and masquerade as a stalled compiler.
    let result = command
        .output()
        .map_err(|e| io_error("run compiler", compiler, e))?;
    if result.status.success() {
        Ok(())
    } else {
        Err(GenerationalLinkError::Compiler {
            stage,
            compiler: compiler.to_owned(),
            status: result.status,
            stderr: String::from_utf8_lossy(&result.stderr).into_owned(),
        })
    }
}
fn compile_c(
    options: GenerationalLinkOptions<'_>,
    source: &Path,
    object: &Path,
) -> Result<(), GenerationalLinkError> {
    let mut command = Command::new(options.compiler);
    options.instrumentation.apply(&mut command, false);
    command
        .args(["-std=c11", "-D_DEFAULT_SOURCE", "-O2", "-pthread", "-c"])
        .arg("-I")
        .arg(options.runtime_directory)
        .arg(source)
        .arg("-o")
        .arg(object);
    invoke(options.compiler, &mut command, "canonical C compilation")
}
/// Link actual canonical runtime sources, never the legacy pointer collector.
/// Compilation and link finish in a private directory beside the output. The
/// existing output is replaced only after success; failures clean intermediates
/// and preserve a previous executable. Tool discovery and CLI feature admission
/// deliberately remain the caller's responsibility.
pub fn link_generational_program(
    source: &str,
    options: GenerationalLinkOptions<'_>,
) -> Result<(), GenerationalLinkError> {
    if !cfg!(all(target_os = "linux", target_arch = "x86_64")) {
        return Err(GenerationalLinkError::UnsupportedHost);
    }
    require_file("Rust runtime archive", options.runtime_archive)?;
    for name in RUNTIME_SOURCES {
        require_file(
            "canonical runtime source",
            &options.runtime_directory.join(name),
        )?;
    }
    for name in [
        "klassic_gen_runtime.h",
        "klassic_gen_string.h",
        "klassic_gen_scalar.h",
        "gc_abi.h",
    ] {
        require_file(
            "canonical runtime header",
            &options.runtime_directory.join(name),
        )?;
    }
    let build = scratch(options.output)?;
    let extension = match options.kind {
        GenerationalSourceKind::C => "c",
        GenerationalSourceKind::Llvm => "ll",
    };
    let input = build.0.join(format!("program.{extension}"));
    fs::write(&input, source).map_err(|e| io_error("write canonical source", &input, e))?;
    let main = build.0.join("program.o");
    match options.kind {
        GenerationalSourceKind::C => compile_c(options, &input, &main)?,
        GenerationalSourceKind::Llvm => {
            let mut command = Command::new(options.compiler);
            options.instrumentation.apply(&mut command, false);
            command
                .args(["-O2", "-Wno-override-module", "-c"])
                .arg(&input)
                .arg("-o")
                .arg(&main);
            invoke(options.compiler, &mut command, "canonical LLVM compilation")?;
        }
    }
    let mut objects = Vec::new();
    for name in RUNTIME_SOURCES {
        let input = options.runtime_directory.join(name);
        let object = build.0.join(format!("{name}.o"));
        compile_c(options, &input, &object)?;
        objects.push(object);
    }
    let executable = build.0.join("executable");
    let mut command = Command::new(options.compiler);
    options.instrumentation.apply(&mut command, true);
    command
        .arg(&main)
        .args(objects)
        .arg(options.runtime_archive)
        .args(["-pthread", "-ldl", "-lm", "-o"])
        .arg(&executable);
    invoke(
        options.compiler,
        &mut command,
        "canonical executable linking",
    )?;
    fs::rename(&executable, options.output)
        .map_err(|e| io_error("publish executable", options.output, e))?;
    Ok(())
}
