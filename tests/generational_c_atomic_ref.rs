//! Actual logical reference CAS/exchange, identities and concurrent OS owners.
#![cfg(all(target_os = "linux", target_arch = "x86_64"))]
use std::io::Read;
use std::os::unix::process::ExitStatusExt;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::time::{Duration, Instant};

struct Fixture(PathBuf);
impl Fixture {
    fn compile(public_only: bool) -> Self {
        let gc = std::env::var_os("KLASSIC_GEN_RUNTIME_DIR")
            .map(PathBuf::from)
            .unwrap_or_else(|| Path::new(env!("CARGO_MANIFEST_DIR")).join("runtime/gc"));
        let compiler = ["clang-18", "clang-17", "clang-16", "clang-15", "clang"]
            .into_iter()
            .find(|compiler| {
                Command::new(compiler)
                    .arg("--version")
                    .output()
                    .is_ok_and(|output| output.status.success())
            })
            .expect("actual C atomic reference tests require clang");
        let dir = std::env::temp_dir().join(format!(
            "klassic-c-atomic-ref-{}-{}",
            std::process::id(),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ));
        std::fs::create_dir(&dir).unwrap();
        let mut command = Command::new(compiler);
        command.args([
            "-std=c11", "-O1", "-g", "-Wall", "-Wextra", "-Werror", "-pthread",
        ]);
        if let Some(mode) = std::env::var_os("KLASSIC_GEN_SANITIZE") {
            command
                .arg(if mode == "thread" {
                    "-fsanitize=thread"
                } else {
                    "-fsanitize=address,undefined"
                })
                .args(["-fno-omit-frame-pointer", "-no-pie"]);
        }
        if public_only {
            for source in [
                "klassic_gen_runtime.c",
                "gc_native_driver.c",
                "klassic_gen_remembered.c",
                "klassic_gen_policy.c",
                "klassic_gen_verify.c",
            ] {
                command.arg(gc.join(source));
            }
        }
        let output = command
            .arg(gc.join(if public_only {
                "klassic_gen_atomic_ref_public_test.c"
            } else {
                "klassic_gen_atomic_ref_test.c"
            }))
            .arg("-o")
            .arg(dir.join("bulk"))
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "atomic reference C compile failed, artifacts {}: {}",
            dir.display(),
            String::from_utf8_lossy(&output.stderr)
        );
        Self(dir)
    }
    fn run(&self, mode: Option<&str>, rejected: Option<&str>) {
        let mut command = if std::env::var("KLASSIC_GEN_SANITIZE").as_deref() == Ok("thread") {
            let mut command = Command::new("setarch");
            command.args(["x86_64", "-R"]).arg(self.0.join("bulk"));
            command
        } else {
            Command::new(self.0.join("bulk"))
        };
        if let Some(mode) = mode {
            command.arg(mode);
        }
        let mut child = command
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .unwrap();
        let streams: [Box<dyn Read + Send>; 2] = [
            Box::new(child.stdout.take().unwrap()),
            Box::new(child.stderr.take().unwrap()),
        ];
        let readers: Vec<_> = streams
            .into_iter()
            .map(|mut stream| {
                std::thread::spawn(move || {
                    let mut bytes = Vec::new();
                    stream.read_to_end(&mut bytes).unwrap();
                    bytes
                })
            })
            .collect();
        let deadline = Instant::now() + Duration::from_secs(45);
        let status = loop {
            if let Some(status) = child.try_wait().unwrap() {
                break status;
            }
            if Instant::now() > deadline {
                child.kill().unwrap();
                let _ = child.wait();
                for (index, reader) in readers.into_iter().enumerate() {
                    std::fs::write(
                        self.0.join(format!("timeout-{index}")),
                        reader.join().unwrap(),
                    )
                    .unwrap();
                }
                panic!(
                    "atomic reference {mode:?} timed out, artifacts {}",
                    self.0.display()
                );
            }
            std::thread::sleep(Duration::from_millis(10));
        };
        let mut output = readers.into_iter().map(|reader| reader.join().unwrap());
        let stdout = output.next().unwrap();
        let stderr = output.next().unwrap();
        let name = mode.unwrap_or("public");
        std::fs::write(self.0.join(format!("{name}.stdout")), &stdout).unwrap();
        std::fs::write(self.0.join(format!("{name}.stderr")), &stderr).unwrap();
        let error = String::from_utf8_lossy(&stderr);
        for unexpected in [
            "ERROR: AddressSanitizer",
            "WARNING: ThreadSanitizer",
            "FATAL: ThreadSanitizer",
            "runtime error:",
        ] {
            assert!(
                !error.contains(unexpected),
                "{error}, artifacts {}",
                self.0.display()
            );
        }
        if let Some(diagnostic) = rejected {
            assert!(
                status.signal() == Some(6) && error.contains(diagnostic),
                "unexpected {status} / {error}, artifacts {}",
                self.0.display()
            );
        } else {
            assert!(
                status.success() && String::from_utf8_lossy(&stdout).contains("PASS"),
                "{status} / {error}, artifacts {}",
                self.0.display()
            );
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
fn c_atomic_references_keep_identity_barriers_and_linearizable_results() {
    let fixture = Fixture::compile(false);
    for mode in [
        "basic",
        "colors",
        "moves",
        "mutation",
        "heal",
        "full",
        "2",
        "8",
        "exchange2",
        "exchange8",
    ] {
        fixture.run(Some(mode), None);
    }
    for (mode, diagnostic) in [
        ("native", "atomic requires Running mutator"),
        ("alignment", "atomic reference operation"),
        ("range", "atomic reference operation"),
        ("type", "atomic reference operation"),
        ("null", "atomic reference operation"),
    ] {
        fixture.run(Some(mode), Some(diagnostic));
    }
}
#[test]
fn c_atomic_references_link_separate_runtime_translation_units() {
    Fixture::compile(true).run(None, None);
}
