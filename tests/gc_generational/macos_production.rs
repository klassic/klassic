//! Production Mach-O worker tests through separate emulated CPU contexts.
use super::*;
use std::collections::{BTreeMap, HashMap};
const SOURCE: &str = r#"
enum Tree { case Leaf(v: Int); case Node(l: Tree, r: Tree) }
enum Box { case Box(v: Int) }
def build(d: Int): Tree = if (d == 0) Leaf(1) else Node(build(d - 1), build(d - 1))
def sum(t: Tree): Int = t match { case Leaf(v) => v; case Node(l, r) => sum(l) + sum(r) }
val keep = build(3)
val opaque = Box(8595182864)
mutable i = 0
mutable total = 0
while (i < 240) { val t = build(5); total += sum(t); i += 1 }
println(total)
println(sum(keep))
println(opaque match { case Box(v) => v + 1 })
"#;
struct Fixture {
    image: PathBuf,
    python: PathBuf,
}
impl Drop for Fixture {
    fn drop(&mut self) {
        let _ = fs::remove_file(&self.image);
    }
}
impl Fixture {
    fn new(stress: bool) -> Option<Self> {
        Self::with_delay(stress, "0")
    }
    fn with_delay(stress: bool, delay: &str) -> Option<Self> {
        let program = if stress {
            SOURCE
                .replace("i < 240", "i < 8")
                .replace("build(5)", "build(3)")
        } else {
            SOURCE.into()
        };
        Self::with_program(stress, delay, &program)
    }
    fn with_program(stress: bool, delay: &str, program: &str) -> Option<Self> {
        let Some(python) = super::aarch64_macho::emulator_python() else {
            eprintln!("no unicorn interpreter; production worker model was not executed");
            return None;
        };
        let source = write_source("macos_worker", program);
        let image = temp_path("macos_worker", "macho");
        let mut flags = vec![
            "--target",
            "aarch64-apple-darwin",
            "--gc=generational",
            "--gc-log",
            "--gc-max-heap=16MiB",
            "--gc-min-heap=128KiB",
            "--gc-uncommit-delay",
            delay,
        ];
        if stress {
            flags.push("--gc-stress");
        }
        let output = build(&flags, &source, &image);
        let _ = fs::remove_file(source);
        assert!(output.status.success(), "{output:?}");
        Some(Self { image, python })
    }
    fn run(&self, fault: &str) -> Output {
        self.run_with_idle(fault, false)
    }
    fn run_with_idle(&self, fault: &str, idle: bool) -> Output {
        let script = Path::new(env!("CARGO_MANIFEST_DIR")).join("tools/run-macho-aarch64.py");
        Command::new(&self.python)
            .arg(script)
            .arg(&self.image)
            .env("KLASSIC_MACHO_WORKER_FAULT", fault)
            .env(
                "KLASSIC_MACHO_WORKER_IDLE_WRITE",
                if idle { "1" } else { "" },
            )
            .env("KLASSIC_MACHO_WORKER_QUANTUM", "50000")
            .env_remove("KLASSIC_MACHO_WORKER_TRACE")
            .env_remove("KLASSIC_MACHO_WORKER_PROGRESS")
            .output()
            .unwrap()
    }
}
fn counters(stderr: &str) -> BTreeMap<&str, u64> {
    stderr
        .lines()
        .find(|s| s.starts_with("macos-worker-model:"))
        .expect("worker API counters")
        .split_whitespace()
        .skip(1)
        .map(|s| {
            let (k, v) = s.split_once('=').unwrap();
            (k, v.parse().unwrap())
        })
        .collect()
}
fn values(output: &Output, expected: &[u8]) -> HashMap<String, u64> {
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(output.status.success(), "{output:?}");
    assert_eq!(output.stdout, expected, "{stderr}");
    let c = counters(&stderr);
    for name in ["returns", "join_calls", "joins", "stack_releases"] {
        assert_eq!(c[name], c["threads"], "{stderr}");
    }
    let stats = gc_stats(&stderr);
    assert_eq!(stats["objects"], stats["published"], "{stderr}");
    assert_eq!(stats["unmarked_copied"], 0, "{stderr}");
    assert_eq!(stats["young_unmarked_copied"], 0, "{stderr}");
    assert_eq!(
        stats["heap_committed_bytes"],
        (stats["heap_commit_pages"] - stats["heap_uncommit_pages"]) * 131072,
        "{stderr}"
    );
    stats
}
#[test]
fn macos_worker_marks_shadow_roots_and_copies_without_unmarked_objects() {
    let Some(f) = Fixture::new(false) else {
        return;
    };
    let output = f.run("");
    let stats = values(&output, b"7680\n8\n8595182865\n");
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stats["collections"] > 0 && stats["worker_scanned"] > 0 && stats["young_worker_units"] > 0,
        "{stderr}"
    );
    assert!(
        stats["heap_uncommit_pages"] > 0 && stats["heap_recommit_pages"] > 0,
        "{stderr}"
    );
    let c = counters(&stderr);
    assert_eq!(c["threads"], 1);
    assert!(c["waits"] > 0 && c["signals"] > 0 && c["clocks"] > 0);
}
#[test]
fn macos_worker_startup_failures_release_resources_and_keep_opaque_values() {
    let Some(f) = Fixture::new(true) else {
        return;
    };
    for fault in ["", "mutex-init", "cond-init", "thread-create"] {
        let output = f.run(fault);
        let stats = values(&output, b"64\n8\n8595182865\n");
        assert!(stats["collections"] > 100);
        let stderr = String::from_utf8_lossy(&output.stderr);
        let c = counters(&stderr);
        if fault.is_empty() {
            assert_eq!(c["threads"], 1);
            assert_eq!(c["mutex_destroy"], 1, "{stderr}");
            assert_eq!(c["cond_destroy"], 1, "{stderr}");
            assert!(c["waits"] > 0 && c["signals"] > 0, "{stderr}");
        } else {
            assert_eq!(c["threads"], 0);
            assert_eq!(stats["worker_scanned"], 0);
            assert_eq!(c["mutex_destroy"], u64::from(fault != "mutex-init"));
            assert_eq!(c["cond_destroy"], u64::from(fault == "thread-create"));
        }
    }
}
#[test]
fn macos_worker_reports_native_wait_signal_clock_and_cleanup_failures() {
    let Some(f) = Fixture::new(true) else {
        return;
    };
    for fault in ["wait", "signal", "cleanup", "clock"] {
        let output = f.run(fault);
        assert_eq!(output.status.code(), Some(1), "{fault}: {output:?}");
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            stderr.contains(if fault == "clock" {
                "GC monotonic clock failed"
            } else {
                "GC worker notification failed"
            }),
            "{fault}: {stderr}"
        );
    }
}

#[test]
fn macos_worker_timed_idle_wakes_uncommit_without_an_owner_signal() {
    let Some(f) = Fixture::with_delay(false, "1500ms") else {
        return;
    };
    let output = f.run_with_idle("", true);
    let stats = values(&output, b"7680\n8\n8595182865\n");
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert_eq!(stats["heap_uncommit_delay_ns"], 1_500_000_000);
    assert!(stats["heap_uncommit_pages"] > 0, "{stderr}");
    assert!(
        stderr.contains("macos-worker-idle:") && stderr.contains("no-owner-signal=1"),
        "{stderr}"
    );
    assert!(counters(&stderr)["timeouts"] > 0, "{stderr}");
}

#[test]
fn macos_worker_explicit_exit_joins_after_churn_and_preserves_computed_status() {
    for (exit, status) in [
        ("exit(7)", 7),
        ("mutable status = 17\nexit(status + 6)", 23),
    ] {
        let program = format!("{SOURCE}\n{exit}\nprintln(99)\n");
        let Some(f) = Fixture::with_program(false, "0", &program) else {
            return;
        };
        let output = f.run("");
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert_eq!(output.status.code(), Some(status), "{output:?}");
        assert_eq!(output.stdout, b"7680\n8\n8595182865\n", "{stderr}");
        let c = counters(&stderr);
        for name in [
            "threads",
            "returns",
            "join_calls",
            "joins",
            "stack_releases",
            "cond_destroy",
            "mutex_destroy",
        ] {
            assert_eq!(c[name], 1, "{stderr}");
        }
    }
}

#[test]
fn macos_worker_reports_join_result_and_post_join_destroy_failures() {
    let Some(f) = Fixture::new(true) else { return };
    for fault in ["join", "join-result", "cond-destroy", "mutex-destroy"] {
        let output = f.run(fault);
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert_eq!(output.status.code(), Some(1), "{fault}: {output:?}");
        assert!(
            stderr.contains("GC worker lifecycle failed"),
            "{fault}: {stderr}"
        );
        let c = counters(&stderr);
        if fault == "join" {
            // A failed join provides no guarantee that the child has returned.
            assert!(c["returns"] <= 1, "{stderr}");
        } else {
            assert_eq!(c["returns"], 1, "{stderr}");
        }
        assert_eq!(c["join_calls"], 1, "{stderr}");
        assert_eq!(c["joins"], u64::from(fault != "join"), "{stderr}");
        assert_eq!(
            c["cond_destroy"],
            u64::from(fault.ends_with("destroy")),
            "{stderr}"
        );
        assert_eq!(
            c["mutex_destroy"],
            u64::from(fault == "mutex-destroy"),
            "{stderr}"
        );
    }
}
