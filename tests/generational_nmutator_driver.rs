//! Real C owner registrations and collector-driver retirement/transport races.
#![cfg(all(target_os = "linux", target_arch = "x86_64"))]

use std::io::Read;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::time::{Duration, Instant};

fn clang() -> &'static str {
    ["clang-18", "clang-17", "clang-16", "clang-15", "clang"]
        .into_iter()
        .find(|name| {
            Command::new(name)
                .arg("--version")
                .output()
                .is_ok_and(|output| output.status.success())
        })
        .expect("a C compiler is required for the actual N-mutator driver regression")
}
struct Fixture(PathBuf);
impl Fixture {
    fn compile() -> Self {
        Self::compile_source("klassic_gen_nmutator_driver_test.c", false)
    }
    fn compile_source(source: &str, separate_runtime: bool) -> Self {
        let path = std::env::temp_dir().join(format!(
            "klassic-nmutator-driver-{}-{}",
            std::process::id(),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ));
        std::fs::create_dir(&path).unwrap();
        let gc = std::env::var_os("KLASSIC_GEN_RUNTIME_DIR")
            .map(PathBuf::from)
            .unwrap_or_else(|| Path::new(env!("CARGO_MANIFEST_DIR")).join("runtime/gc"));
        assert!(
            gc.is_absolute(),
            "runtime source directory must be absolute"
        );
        let mut command = Command::new(clang());
        command.arg("-I").arg(&gc);
        command.args([
            "-std=c11",
            "-D_DEFAULT_SOURCE",
            "-O1",
            "-g",
            "-Wall",
            "-Wextra",
            "-Werror",
            "-pthread",
        ]);
        if let Some(value) = std::env::var_os("KLASSIC_GEN_SANITIZE") {
            let sanitizer = if value == "thread" {
                "thread"
            } else {
                "address,undefined"
            };
            command
                .arg(format!("-fsanitize={sanitizer}"))
                .args(["-fno-omit-frame-pointer", "-no-pie"]);
        }
        if separate_runtime {
            for runtime in [
                "klassic_gen_runtime.c",
                "gc_native_driver.c",
                "klassic_gen_remembered.c",
                "klassic_gen_policy.c",
                "klassic_gen_verify.c",
            ] {
                command.arg(gc.join(runtime));
            }
        }
        let output = command
            .arg(gc.join(source))
            .args(["-ldl", "-lm", "-o"])
            .arg(path.join("driver"))
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "C driver fixture compilation failed; artifacts {}: {}",
            path.display(),
            String::from_utf8_lossy(&output.stderr)
        );
        Self(path)
    }
    fn run(&self, arguments: &[&str], marker: &str) {
        self.run_case(arguments, marker, true);
    }
    fn rejected(&self, arguments: &[&str], diagnostic: &str) {
        self.run_case(arguments, diagnostic, false);
    }
    fn run_case(&self, arguments: &[&str], marker: &str, succeeds: bool) {
        let started = Instant::now();
        let no_aslr = std::env::var("KLASSIC_GEN_SANITIZE").as_deref() == Ok("thread")
            && Command::new("setarch")
                .args(["x86_64", "-R", "true"])
                .stdout(Stdio::null())
                .stderr(Stdio::null())
                .status()
                .is_ok_and(|status| status.success());
        let mut command = if no_aslr {
            let mut command = Command::new("setarch");
            command.args(["x86_64", "-R"]).arg(self.0.join("driver"));
            command
        } else {
            Command::new(self.0.join("driver"))
        };
        let mut child = command
            .args(arguments)
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .unwrap();
        let mut stdout = child.stdout.take().unwrap();
        let mut stderr = child.stderr.take().unwrap();
        let stdout = std::thread::spawn(move || {
            let mut bytes = Vec::new();
            stdout.read_to_end(&mut bytes).unwrap();
            bytes
        });
        let stderr = std::thread::spawn(move || {
            let mut bytes = Vec::new();
            stderr.read_to_end(&mut bytes).unwrap();
            bytes
        });
        let status = loop {
            if let Some(status) = child.try_wait().unwrap() {
                break status;
            }
            if started.elapsed() > Duration::from_secs(120) {
                child.kill().unwrap();
                let status = child.wait().unwrap();
                let out = stdout.join().unwrap();
                let error = stderr.join().unwrap();
                std::fs::write(self.0.join("timeout.stdout"), out).unwrap();
                std::fs::write(self.0.join("timeout.stderr"), &error).unwrap();
                panic!(
                    "N-mutator driver {arguments:?} timed out ({status}); artifacts {}: {}",
                    self.0.display(),
                    String::from_utf8_lossy(&error)
                );
            }
            std::thread::sleep(Duration::from_millis(10));
        };
        let out = stdout.join().unwrap();
        let error = stderr.join().unwrap();
        let name = arguments.join("-");
        std::fs::write(self.0.join(format!("{name}.stdout")), &out).unwrap();
        std::fs::write(self.0.join(format!("{name}.stderr")), &error).unwrap();
        let error_text = String::from_utf8_lossy(&error);
        if succeeds {
            assert!(
                status.success(),
                "N-mutator driver {arguments:?} failed ({status}); artifacts {}: {}",
                self.0.display(),
                error_text
            );
            assert!(String::from_utf8_lossy(&out).contains(marker));
        } else {
            use std::os::unix::process::ExitStatusExt;
            assert_eq!(
                status.signal(),
                Some(6),
                "unexpected rejection: {error_text}"
            );
            assert!(
                error_text.contains(marker),
                "wrong diagnostic: {error_text}"
            );
            for sanitizer in [
                "ERROR: AddressSanitizer",
                "SUMMARY: AddressSanitizer",
                "WARNING: ThreadSanitizer",
                "FATAL: ThreadSanitizer",
                "UndefinedBehaviorSanitizer",
                "runtime error:",
            ] {
                assert!(
                    !error_text.contains(sanitizer),
                    "sanitizer failure: {error_text}"
                );
            }
        }
    }
}
impl Drop for Fixture {
    fn drop(&mut self) {
        if !std::thread::panicking() {
            let _ = std::fs::remove_dir_all(&self.0);
        }
    }
}

#[test]
fn actual_c_driver_handles_two_and_eight_owners_and_pinned_detach() {
    let fixture = Fixture::compile();
    for count in ["2", "8"] {
        fixture.run(&[count], "owner buffers, moving aliases, pressure");
    }
    fixture.run(
        &["1", "pin"],
        "retains registration and rings through detach PASS",
    );
    fixture.run(&["1", "prelock"], "rejects late mutex access PASS");
}

#[test]
fn actual_c_rooted_publication_cannot_be_overtaken_by_source_retirement() {
    let fixture = Fixture::compile();
    fixture.run(&["1", "publication"], "restarts incoming proof PASS");
    fixture.run(
        &["1", "lease-worker"],
        "retain work through lease growth PASS",
    );
    fixture.run(&["1", "rendezvous"], "permits metadata assistance PASS");
    fixture.run(
        &["1", "peer-publication"],
        "primary revision unchanged PASS",
    );
}

#[test]
fn actual_c_public_owners_import_and_mutate_one_shared_moving_cycle() {
    let fixture = Fixture::compile_source("klassic_gen_nmutator_shared_test.c", true);
    for count in ["2", "8"] {
        fixture.run(
            &[count],
            "foreign-root import, cycles, moving aliases and detach PASS",
        );
    }
}

#[test]
fn actual_c_lease_growth_preserves_tokens_budget_and_concurrent_owners() {
    let fixture = Fixture::compile_source("klassic_gen_lease_growth_test.c", false);
    for mode in [
        "default",
        "faults",
        "2",
        "8",
        "prepare",
        "limit-race",
        "limits",
    ] {
        fixture.run(&[mode], "PASS");
    }
}

#[test]
fn actual_c_lease_bootstrap_checks_exact_budget_large_quantum_and_failures() {
    let fixture = Fixture::compile_source("klassic_gen_lease_bootstrap_test.c", false);
    fixture.run(
        &["budget", "0", "baseline"],
        "actual constructor budget and OS cleanup PASS",
    );
    fixture.rejected(&["budget", "0"], "initial lease prefix budget");
    fixture.rejected(&["budget", "4095"], "initial lease prefix budget");
    fixture.run(
        &["budget", "4096"],
        "actual constructor budget and OS cleanup PASS",
    );
    for mode in ["reserve-retry", "commit-retry", "quantum64"] {
        fixture.run(&[mode], "complete OS cleanup PASS");
    }
    fixture.rejected(&["reserve-permanent"], "lease reservation OS retry limit");
    fixture.rejected(&["generation-reject"], "initial lease slots already used");
}

#[test]
fn actual_c_byte_pins_hold_real_addresses_and_exclude_foreign_lifetimes() {
    let fixture = Fixture::compile_source("klassic_gen_pin_test.c", false);
    for mode in [
        "basic",
        "large",
        "zero",
        "null",
        "2",
        "8",
        "late",
        "detach-rendezvous",
        "receipts",
        "full",
        "faults",
        "identity",
    ] {
        fixture.run(&[mode], "PASS");
    }
    for (mode, diagnostic) in [
        ("record", "pin requires published RAW_BYTES"),
        ("borrow-running", "pinned bytes require Native borrow"),
        ("shutdown-live", "live byte pins at owner detach"),
        ("released", "unknown or released byte pin"),
        ("invalid-exit", "collector source guard exit"),
        ("reuse", "unknown or released byte pin"),
    ] {
        fixture.rejected(&[mode], diagnostic);
    }
}

#[test]
fn actual_c_reference_pins_require_rooted_access_and_live_owner() {
    let fixture = Fixture::compile_source("klassic_gen_reference_pin_test.c", false);
    for mode in ["basic", "2", "8"] {
        fixture.run(&[mode], "PASS");
    }
    for (mode, diagnostic) in [
        ("address", "reference payload requires rooted pin access"),
        ("raw", "pinned reference access requires nonnull REF_RECORD"),
        (
            "null",
            "pinned reference access requires nonnull REF_RECORD",
        ),
        ("offset", "reference load failed"),
        ("foreign", "byte pin owner"),
    ] {
        fixture.rejected(&[mode], diagnostic);
    }
}

#[test]
fn actual_c_tiny_queues_keep_old_snapshot_through_young_copy_and_retirement() {
    let fixture = Fixture::compile_source("klassic_gen_queue_overflow_test.c", false);
    for mode in [
        "basic",
        "partial",
        "partial-young",
        "full",
        "spanning",
        "pin",
        "reuse",
        "fair",
    ] {
        fixture.run(&[mode], "PASS");
    }
}

#[test]
fn actual_c_queue_core_retains_claimed_found_and_native_retirement_contracts() {
    let fixture = Fixture::compile_source("klassic_gen_queue_core_test.c", false);
    fixture.run(&[], "C queue core:");
}

#[test]
fn actual_c_weak_handles_clear_keep_alive_grow_and_race_real_gc() {
    let fixture = Fixture::compile_source("klassic_gen_weak_test.c", false);
    for mode in [
        "basic",
        "growth",
        "2",
        "8",
        "shutdown-scan",
        "faults",
        "gate",
        "transport",
    ] {
        fixture.run(&[mode], "PASS");
    }
}
