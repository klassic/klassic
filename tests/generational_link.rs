#![cfg(all(target_os = "linux", target_arch = "x86_64"))]

use klassic_native::{
    GenerationalBackendConfig, GenerationalInstrumentation, GenerationalLinkOptions,
    GenerationalSourceKind, compile_source_to_c_generational, compile_source_to_llvm_generational,
    link_generational_program,
};
use std::io::Read;
use std::path::Path;
use std::process::{Command, Stdio};
use std::time::{Duration, Instant};

fn run_bounded(executable: &Path) -> (std::process::ExitStatus, Vec<u8>, Vec<u8>) {
    let mut child = Command::new(executable)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();
    let mut stdout = child.stdout.take().unwrap();
    let mut stderr = child.stderr.take().unwrap();
    let out = std::thread::spawn(move || {
        let mut b = Vec::new();
        stdout.read_to_end(&mut b).unwrap();
        b
    });
    let err = std::thread::spawn(move || {
        let mut b = Vec::new();
        stderr.read_to_end(&mut b).unwrap();
        b
    });
    let deadline = Instant::now() + Duration::from_secs(60);
    let status = loop {
        if let Some(status) = child.try_wait().unwrap() {
            break status;
        }
        if Instant::now() >= deadline {
            child.kill().unwrap();
            child.wait().unwrap();
            panic!("canonical linked program timed out");
        }
        std::thread::sleep(Duration::from_millis(10));
    };
    (status, out.join().unwrap(), err.join().unwrap())
}

#[test]
fn canonical_linker_runs_both_backends_and_preserves_output_on_failure() {
    let compiler = ["clang-18", "clang-17", "clang-16", "clang-15", "clang"]
        .into_iter()
        .find(|name| {
            Command::new(name)
                .arg("--version")
                .output()
                .is_ok_and(|o| o.status.success())
        })
        .expect("Clang is required for canonical linker tests");
    let instrumentation = match std::env::var("KLASSIC_GEN_SANITIZE").ok().as_deref() {
        None => GenerationalInstrumentation::None,
        Some("address") => GenerationalInstrumentation::AddressUndefined,
        Some("thread") => GenerationalInstrumentation::Thread,
        _ => panic!("unknown sanitizer mode"),
    };
    let dir = std::env::temp_dir().join(format!(
        "klassic-link-{}-{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ));
    std::fs::create_dir(&dir).unwrap();
    let gc = Path::new(env!("CARGO_MANIFEST_DIR")).join("runtime/gc");
    let archive = Path::new(env!("CARGO_BIN_EXE_klassic"))
        .parent()
        .unwrap()
        .join("libklassic_runtime.a");
    let output = dir.join("program");
    let program = "val r = record { x: 42 }\nmutable i = 0\nwhile (i < 20) { val t = record { x: i }; i += 1 }\nprintln(r.x)\n";
    let config = GenerationalBackendConfig {
        flags: 1,
        ..Default::default()
    };
    for (kind, source) in [
        (
            GenerationalSourceKind::C,
            compile_source_to_c_generational("c.kl", program, config).unwrap(),
        ),
        (
            GenerationalSourceKind::Llvm,
            compile_source_to_llvm_generational("llvm.kl", program, config).unwrap(),
        ),
    ] {
        let options = GenerationalLinkOptions {
            kind,
            compiler: Path::new(compiler),
            instrumentation,
            runtime_directory: &gc,
            runtime_archive: &archive,
            output: &output,
        };
        link_generational_program(&source, options).unwrap();
        let (status, stdout, stderr) = run_bounded(&output);
        assert!(status.success(), "{}", String::from_utf8_lossy(&stderr));
        assert_eq!(stdout, b"42\n");
        let before = std::fs::read(&output).unwrap();
        let bad = GenerationalLinkOptions {
            compiler: Path::new("klassic-no-such-compiler"),
            ..options
        };
        assert!(link_generational_program(&source, bad).is_err());
        assert_eq!(std::fs::read(&output).unwrap(), before);
    }
    std::fs::remove_dir_all(dir).unwrap();
}
