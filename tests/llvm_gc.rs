//! Runs the C garbage collector's own unit tests under
//! AddressSanitizer + UBSan (`runtime/gc/run_tests.sh`) as part of
//! `cargo test`. Part of the LLVM backend migration
//! (`docs/llvm-backend-plan.md`, M6): the GC is C now, so it gets a
//! memory sanitizer the hand-emitted collector never could. Gated on a
//! C compiler being present so CI without one stays green.
#![cfg(all(target_os = "linux", target_arch = "x86_64"))]

use std::process::Command;

fn find_cc() -> Option<String> {
    if let Ok(explicit) = std::env::var("KLASSIC_CLANG") {
        return Some(explicit);
    }
    [
        "clang-18", "clang-17", "clang-16", "clang-15", "clang", "cc",
    ]
    .into_iter()
    .find(|name| {
        Command::new(name)
            .arg("--version")
            .stdout(std::process::Stdio::null())
            .stderr(std::process::Stdio::null())
            .status()
            .is_ok_and(|status| status.success())
    })
    .map(str::to_owned)
}

#[test]
fn c_gc_passes_under_sanitizers() {
    let Some(cc) = find_cc() else {
        eprintln!("skipping: no C compiler found");
        return;
    };
    let script = concat!(env!("CARGO_MANIFEST_DIR"), "/runtime/gc/run_tests.sh");
    let output = Command::new("bash")
        .arg(script)
        .env("KLASSIC_CLANG", &cc)
        .output()
        .expect("run_tests.sh should run");
    assert!(
        output.status.success(),
        "C GC sanitizer tests failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(
        String::from_utf8_lossy(&output.stdout).contains("ALL GC TESTS PASSED"),
        "expected the GC tests to report success"
    );
}
