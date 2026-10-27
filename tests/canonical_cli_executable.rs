//! Isolated executable-route proof. Required clang/runtime failures are failures.
#![cfg(all(target_os = "linux", target_arch = "x86_64"))]
use std::{
    io::Read,
    path::{Path, PathBuf},
    process::{Command, Output, Stdio},
    time::{Duration, Instant},
};
struct Scratch(PathBuf);
impl Scratch {
    fn new() -> Self {
        let path = std::env::temp_dir().join(format!(
            "klassic canonical cli {} {}",
            std::process::id(),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ));
        std::fs::create_dir(&path).unwrap();
        Self(path)
    }
}
impl Drop for Scratch {
    fn drop(&mut self) {
        if !std::thread::panicking() {
            let _ = std::fs::remove_dir_all(&self.0);
        }
    }
}
fn runtime_archive() -> PathBuf {
    let profile = Path::new(env!("CARGO_BIN_EXE_klassic")).parent().unwrap();
    std::env::var_os("KLASSIC_RT_LIB")
        .map(PathBuf::from)
        .unwrap_or_else(|| {
            let archive = profile.join("libklassic_runtime.a");
            if archive.is_file() {
                return archive;
            }
            std::fs::read_dir(profile.join("deps"))
                .into_iter()
                .flatten()
                .filter_map(Result::ok)
                .map(|entry| entry.path())
                .find(|path| {
                    path.file_name().is_some_and(|name| {
                        let name = name.to_string_lossy();
                        name.starts_with("libklassic_runtime-") && name.ends_with(".a")
                    })
                })
                .unwrap_or(archive)
        })
}
fn command(backend: &str, input: &Path, output: &Path) -> Command {
    let mut command = Command::new(env!("CARGO_BIN_EXE_klassic"));
    command
        .args(["--gc=generational", "--backend", backend, "build"])
        .arg(input)
        .arg("-o")
        .arg(output);
    command.env("KLASSIC_RT_LIB", runtime_archive());
    command.env(
        "KLASSIC_GEN_RUNTIME_DIR",
        Path::new(env!("CARGO_MANIFEST_DIR")).join("runtime/gc"),
    );
    command
}
fn checked(command: &mut Command) -> Output {
    let output = command.output().unwrap();
    assert!(
        output.status.success(),
        "{command:?}: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    output
}
fn execute(path: &Path) -> Output {
    let mut command = Command::new(path);
    let mut child = command
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();
    let collect = |mut pipe: Box<dyn Read + Send>| {
        std::thread::spawn(move || {
            let mut bytes = Vec::new();
            pipe.read_to_end(&mut bytes).unwrap();
            bytes
        })
    };
    let stdout = collect(Box::new(child.stdout.take().unwrap()));
    let stderr = collect(Box::new(child.stderr.take().unwrap()));
    let start = Instant::now();
    loop {
        if let Some(status) = child.try_wait().unwrap() {
            return Output {
                status,
                stdout: stdout.join().unwrap(),
                stderr: stderr.join().unwrap(),
            };
        }
        if start.elapsed() > Duration::from_secs(45) {
            let _ = child.kill();
            let _ = child.wait();
            let _ = stdout.join();
            panic!(
                "canonical CLI executable timeout: {}",
                String::from_utf8_lossy(&stderr.join().unwrap())
            );
        }
        std::thread::sleep(Duration::from_millis(10));
    }
}
#[test]
fn canonical_cli_links_modules_strings_and_preserves_output_on_failure() {
    let runtime = std::env::var_os("KLASSIC_GEN_RUNTIME_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|| Path::new(env!("CARGO_MANIFEST_DIR")).join("runtime/gc"));
    let archive = runtime_archive();
    assert!(
        runtime.is_dir() && archive.is_file(),
        "canonical runtime and Rust archive are required"
    );
    let scratch = Scratch::new();
    std::fs::write(scratch.0.join("values.kl"), "module values\nprintln(\"init\")\nval label: String = \"灯😀\"\ndef repeat(n: Int): String = if (n <= 0) label else repeat(n - 1) + \"λ\"\ndef fail(n: Int): Int = 1 / n\n").unwrap();
    let input = scratch.0.join("program.kl");
    std::fs::write(&input, "import values as A\nimport values as Again\nval keep = record { text: A#repeat(12) }\nmutable i = 0\nwhile (i < 30) { val garbage = record { text: \"discard\" + \"😀\" }; i = i + 1 }\nprintln(keep.text)\nprintln(Again.label)\n").unwrap();
    let expected = checked(Command::new(env!("CARGO_BIN_EXE_klassic")).arg(&input));
    for backend in ["c", "llvm"] {
        let binary = scratch.0.join(format!("{backend} executable"));
        checked(command(backend, &input, &binary).arg("--gc-stress"));
        let actual = execute(&binary);
        assert!(actual.status.success(), "{actual:?}");
        assert_eq!(actual.stdout, expected.stdout);
        let previous = std::fs::read(&binary).unwrap();
        for (key, missing) in [
            ("KLASSIC_GEN_CC", scratch.0.join("missing compiler")),
            ("KLASSIC_GEN_RUNTIME_DIR", scratch.0.join("missing runtime")),
            ("KLASSIC_RT_LIB", scratch.0.join("missing archive")),
        ] {
            let result = command(backend, &input, &binary)
                .env(key, missing)
                .output()
                .unwrap();
            assert!(
                !result.status.success(),
                "{key} must not silently fall back"
            );
            assert_eq!(std::fs::read(&binary).unwrap(), previous, "{key}");
        }
        let invalid = scratch.0.join("invalid archive.a");
        std::fs::write(&invalid, "not an archive").unwrap();
        assert!(
            !command(backend, &input, &binary)
                .env("KLASSIC_RT_LIB", invalid)
                .output()
                .unwrap()
                .status
                .success()
        );
        assert_eq!(std::fs::read(&binary).unwrap(), previous);
        for flag in ["--gc-poison", "--gc-log"] {
            let result = command(backend, &input, &binary)
                .arg(flag)
                .output()
                .unwrap();
            assert!(!result.status.success(), "{flag}");
            assert_eq!(std::fs::read(&binary).unwrap(), previous);
        }
        let result = command(backend, &input, &binary)
            .args(["--target", "x86_64-pc-windows-msvc"])
            .output()
            .unwrap();
        assert!(!result.status.success());
        assert_eq!(std::fs::read(&binary).unwrap(), previous);
        let bad_input = scratch.0.join("error.kl");
        std::fs::write(&bad_input, "import values as A\nprintln(A#fail(0))\n").unwrap();
        let expected_error = Command::new(env!("CARGO_BIN_EXE_klassic"))
            .arg(&bad_input)
            .output()
            .unwrap();
        let error_binary = scratch.0.join(format!("{backend} error"));
        checked(&mut command(backend, &bad_input, &error_binary));
        let actual_error = execute(&error_binary);
        assert_eq!(actual_error.status.code(), expected_error.status.code());
        assert_eq!(actual_error.stdout, expected_error.stdout);
        // The evaluator uses a logical module name; canonical CLI diagnostics
        // intentionally retain the resolved physical source path.
        let logical_error = String::from_utf8(expected_error.stderr).unwrap();
        let suffix = logical_error.strip_prefix("values:").unwrap();
        let physical_error = format!("{}:{suffix}", scratch.0.join("values.kl").display());
        assert_eq!(
            String::from_utf8(actual_error.stderr).unwrap(),
            physical_error
        );
    }
    // An installed binary finds its adjacent canonical runtime before any checkout.
    let install = scratch.0.join("install");
    std::fs::create_dir(&install).unwrap();
    let installed = install.join("klassic");
    std::fs::copy(env!("CARGO_BIN_EXE_klassic"), &installed).unwrap();
    std::fs::copy(archive, install.join("libklassic_runtime.a")).unwrap();
    let runtime_out = install.join("runtime/gc");
    std::fs::create_dir_all(&runtime_out).unwrap();
    for entry in std::fs::read_dir(runtime).unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();
        let name = entry.file_name();
        if path.extension().is_some_and(|ext| ext == "h")
            || [
                "klassic_gen_runtime.c",
                "klassic_gen_remembered.c",
                "gc_native_driver.c",
                "klassic_gen_policy.c",
                "klassic_gen_verify.c",
                "klassic_gen_string.c",
                "klassic_gen_scalar.c",
            ]
            .iter()
            .any(|candidate| name == *candidate)
        {
            std::fs::copy(entry.path(), runtime_out.join(entry.file_name())).unwrap();
        }
    }
    let output = install.join("program");
    for backend in ["c", "llvm"] {
        checked(
            Command::new(&installed)
                .env_remove("KLASSIC_RT_LIB")
                .env_remove("KLASSIC_GEN_RUNTIME_DIR")
                .env_remove("KLASSIC_GEN_CC")
                .args(["--gc=generational", "--backend", backend, "build"])
                .arg(&input)
                .arg("-o")
                .arg(&output),
        );
        let actual = execute(&output);
        assert!(actual.status.success(), "{actual:?}");
        assert_eq!(actual.stdout, expected.stdout);
    }
    let previous = std::fs::read(&output).unwrap();
    for missing in ["gc_abi.h", "klassic_gen_runtime.c"] {
        let path = runtime_out.join(missing);
        let bytes = std::fs::read(&path).unwrap();
        std::fs::remove_file(&path).unwrap();
        let broken = Command::new(&installed)
            .env_remove("KLASSIC_RT_LIB")
            .env_remove("KLASSIC_GEN_RUNTIME_DIR")
            .env_remove("KLASSIC_GEN_CC")
            .args(["--gc=generational", "--backend", "c", "build"])
            .arg(&input)
            .arg("-o")
            .arg(&output)
            .output()
            .unwrap();
        assert!(
            !broken.status.success(),
            "installed runtime must not fall back to a checkout"
        );
        assert!(String::from_utf8_lossy(&broken.stderr).contains(missing));
        assert_eq!(std::fs::read(&output).unwrap(), previous);
        std::fs::write(path, bytes).unwrap();
    }
    assert!(
        !std::fs::read_dir(&scratch.0)
            .unwrap()
            .filter_map(Result::ok)
            .any(|entry| entry
                .file_name()
                .to_string_lossy()
                .starts_with(".klassic-canonical-"))
    );
}
