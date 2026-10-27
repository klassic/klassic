//! Dedicated canonical executable linking. Kept separate from legacy C/LLVM.
use super::cli::ExecutionConfig;
use klassic_native::{
    GenerationalInstrumentation, GenerationalLinkOptions, GenerationalSourceKind,
    link_generational_program,
};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

fn compiler() -> Result<PathBuf, String> {
    if let Some(explicit) = std::env::var_os("KLASSIC_GEN_CC") {
        // An explicit selection never silently falls back to a different tool.
        return Ok(explicit.into());
    }
    ["clang-18", "clang-17", "clang-16", "clang-15", "clang"]
        .into_iter()
        .find(|name| {
            Command::new(name).arg("--version")
                .stdout(Stdio::null()).stderr(Stdio::null())
                .status().is_ok_and(|status| status.success())
        })
        .map(PathBuf::from)
        .ok_or_else(|| "no canonical compiler found; install clang with opaque-pointer LLVM support or set KLASSIC_GEN_CC".into())
}
fn runtime_directory() -> Result<PathBuf, String> {
    if let Some(explicit) = std::env::var_os("KLASSIC_GEN_RUNTIME_DIR") {
        let path = PathBuf::from(explicit);
        return if path.is_dir() {
            Ok(path)
        } else {
            Err(format!(
                "canonical runtime directory not found: {}",
                path.display()
            ))
        };
    }
    let mut candidates = Vec::new();
    if let Ok(exe) = std::env::current_exe()
        && let Some(directory) = exe.parent()
    {
        candidates.push(directory.join("lib/klassic/gc"));
        candidates.push(directory.join("runtime/gc"));
    }
    // Source checkouts remain usable without imposing a toolchain on direct builds.
    candidates.push(Path::new(env!("CARGO_MANIFEST_DIR")).join("runtime/gc"));
    candidates.into_iter()
        .find(|path| path.is_dir())
        .ok_or_else(|| "canonical runtime sources not found; set KLASSIC_GEN_RUNTIME_DIR to the matching source/header directory".into())
}
fn runtime_archive() -> Result<PathBuf, String> {
    if let Some(explicit) = std::env::var_os("KLASSIC_RT_LIB") {
        let path = PathBuf::from(explicit);
        return if path.is_file() {
            Ok(path)
        } else {
            Err(format!(
                "Rust runtime archive not found: {}",
                path.display()
            ))
        };
    }
    super::find_runtime_staticlib().ok_or_else(||
        "libklassic_runtime.a not found; build klassic-runtime and set KLASSIC_RT_LIB to its matching archive".into())
}
fn build(source: &str, output: &Path, config: &ExecutionConfig) -> Result<(), String> {
    if !cfg!(all(target_os = "linux", target_arch = "x86_64")) {
        return Err("canonical executable linking currently requires a Linux x86-64 host".into());
    }
    let compiler = compiler()?;
    let runtime = runtime_directory()?;
    let archive = runtime_archive()?;
    link_generational_program(
        source,
        GenerationalLinkOptions {
            kind: if config.llvm_backend {
                GenerationalSourceKind::Llvm
            } else {
                GenerationalSourceKind::C
            },
            compiler: &compiler,
            instrumentation: GenerationalInstrumentation::None,
            runtime_directory: &runtime,
            runtime_archive: &archive,
            output,
        },
    )
    .map_err(|error| error.to_string())
}
pub(super) fn link(source: &str, output: &Path, config: &ExecutionConfig) -> Result<(), u8> {
    build(source, output, config).map_err(|error| {
        eprintln!("error: {error}");
        1
    })
}
