//! `--gc=generational` (M-G1): the opt-in generational GC integration on
//! the direct x86-64 ELF backend. M-G1 never collects; these tests pin the
//! selector, its rejections, and evaluator == native output under the flag.

use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::{SystemTime, UNIX_EPOCH};

fn klassic_bin() -> &'static str {
    env!("CARGO_BIN_EXE_klassic")
}

fn temp_path(tag: &str, extension: &str) -> PathBuf {
    static COUNTER: AtomicU64 = AtomicU64::new(0);
    let stamp = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("system time should be after epoch")
        .as_nanos();
    let serial = COUNTER.fetch_add(1, Ordering::Relaxed);
    std::env::temp_dir().join(format!(
        "klassic_gcgen_{tag}_{}_{stamp}_{serial}.{extension}",
        std::process::id()
    ))
}

fn write_source(tag: &str, source: &str) -> PathBuf {
    let path = temp_path(tag, "kl");
    fs::write(&path, source).expect("temp source file should write");
    path
}

fn build(flags: &[&str], source: &Path, output: &Path) -> Output {
    let mut args: Vec<String> = flags.iter().map(|flag| flag.to_string()).collect();
    args.push("build".to_string());
    args.push(source.display().to_string());
    args.push("-o".to_string());
    args.push(output.display().to_string());
    Command::new(klassic_bin())
        .args(&args)
        .output()
        .expect("klassic should run")
}

fn build_failure_stderr(tag: &str, flags: &[&str], source: &str) -> String {
    let source_path = write_source(tag, source);
    let output_path = temp_path(tag, "bin");
    let result = build(flags, &source_path, &output_path);
    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);
    assert!(
        !result.status.success(),
        "build with {flags:?} should fail\nstdout:\n{}",
        String::from_utf8_lossy(&result.stdout)
    );
    String::from_utf8_lossy(&result.stderr).into_owned()
}

#[test]
fn unknown_gc_mode_is_rejected() {
    let stderr = build_failure_stderr("unknown_mode", &["--gc=bogus"], "println(1)\n");
    assert!(
        stderr.contains("unknown --gc mode 'bogus'"),
        "stderr:\n{stderr}"
    );
}

#[test]
fn generational_rejects_llvm_and_c_backends() {
    for backend in ["llvm", "c"] {
        let stderr = build_failure_stderr(
            "backend",
            &["--gc=generational", "--backend", backend],
            "println(1)\n",
        );
        assert!(
            stderr.contains("--gc=generational is only supported by the direct x86_64"),
            "backend {backend} stderr:\n{stderr}"
        );
    }
}

#[test]
fn generational_rejects_non_linux_x86_targets() {
    for target in ["x86_64-pc-windows-msvc", "aarch64-apple-darwin"] {
        let stderr = build_failure_stderr(
            "target",
            &["--gc=generational", "--target", target],
            "println(1)\n",
        );
        assert!(
            stderr.contains("--gc=generational is only supported by the direct x86_64"),
            "target {target} stderr:\n{stderr}"
        );
    }
}

#[test]
fn generational_rejects_legacy_gc_debug_flags() {
    for flag in ["--gc-stress", "--gc-poison"] {
        let stderr = build_failure_stderr("debug", &["--gc=generational", flag], "println(1)\n");
        assert!(
            stderr.contains("not accepted with --gc=generational"),
            "flag {flag} stderr:\n{stderr}"
        );
    }
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
mod linux_x86_64 {
    use super::*;

    /// Build `source` with `flags`, run it, and return the executable's
    /// output. The build itself must succeed.
    pub(super) fn build_and_run(tag: &str, flags: &[&str], source: &str) -> Output {
        let source_path = write_source(tag, source);
        let output_path = temp_path(tag, "bin");
        let result = build(flags, &source_path, &output_path);
        let _ = fs::remove_file(&source_path);
        assert!(
            result.status.success(),
            "native build with {flags:?} should succeed\nstderr:\n{}",
            String::from_utf8_lossy(&result.stderr)
        );
        let run = Command::new(&output_path)
            .output()
            .expect("compiled binary should run");
        let _ = fs::remove_file(&output_path);
        run
    }

    #[test]
    fn generational_rejects_enums_with_a_diagnostic() {
        let source = "enum Color {\n  case Red\n  case Green\n}\nval c = Red\nprintln(c match { case Red => 1; case Green => 2 })\n";
        let stderr = build_failure_stderr("enum", &["--gc=generational"], source);
        assert!(
            stderr.contains("enums under --gc=generational (M-G1)"),
            "stderr:\n{stderr}"
        );
        // The same program still builds on the default (legacy) path.
        let run = build_and_run("enum_legacy", &[], source);
        assert!(run.status.success());
        assert_eq!(String::from_utf8_lossy(&run.stdout), "1\n");
    }

    #[test]
    fn generational_builds_an_int_program() {
        let run = build_and_run(
            "int",
            &["--gc=generational"],
            "def fib(n: Int): Int = if(n < 2) n else fib(n - 1) + fib(n - 2)\nprintln(fib(20))\n",
        );
        assert!(
            run.status.success(),
            "stderr:\n{}",
            String::from_utf8_lossy(&run.stderr)
        );
        assert_eq!(String::from_utf8_lossy(&run.stdout), "6765\n");
    }
}
