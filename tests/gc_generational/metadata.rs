//! Writable collector mappings are charged independently on all direct targets.
use super::*;

pub(super) const CHURN: &str = r#"
enum Tree { case Leaf(v: Int); case Node(l: Tree, r: Tree) }
def build(d: Int): Tree = if (d == 0) Leaf(1) else Node(build(d - 1), build(d - 1))
def sum(t: Tree): Int = t match { case Leaf(v) => v; case Node(l, r) => sum(l) + sum(r) }
val keep = build(5)
mutable i = 0
mutable total = 0
while (i < 160) { val t = build(5); total += sum(t); i += 1 }
println(total)
println(sum(keep))
"#;

// Test runners append their own line after the emitted program's stderr.
// Remove exactly that expected suffix; unknown output still fails JSON parsing.
fn strip_driver_log(output: &mut Output, marker: &str) {
    let text = String::from_utf8(std::mem::take(&mut output.stderr)).unwrap();
    let (snapshot, driver) = text.rsplit_once(marker).expect("runner diagnostics");
    assert_eq!(driver.lines().count(), 1);
    output.stderr = format!("{snapshot}\n").into_bytes();
}

pub(super) struct Fixture {
    directory: PathBuf,
    target: &'static str,
    driver: Option<PathBuf>,
    python: Option<PathBuf>,
}
impl Drop for Fixture {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.directory);
    }
}
impl Fixture {
    pub(super) fn new(target: &'static str) -> Option<Self> {
        let python = if target == "aarch64-apple-darwin" {
            let Some(python) = super::aarch64_macho::emulator_python() else {
                eprintln!("no unicorn interpreter; Darwin metadata cases not executed");
                return None;
            };
            Some(python)
        } else {
            None
        };
        let directory = temp_path("metadata_budget", "dir");
        fs::create_dir(&directory).unwrap();
        let driver = (target == "x86_64-pc-windows-msvc")
            .then(|| super::windows_production::driver(&directory));
        Some(Self {
            directory,
            target,
            driver,
            python,
        })
    }
    fn run(&self, program: &str, limit: Option<u64>) -> Output {
        self.run_limits(program, limit, None, 16 << 20, false)
    }
    pub(super) fn run_soft(&self, program: &str, soft: Option<u64>) -> Output {
        self.run_limits(program, None, soft, 64 << 20, false)
    }
    fn run_retirement(&self, program: &str, failure: bool) -> Output {
        self.run_limits(program, None, None, 16 << 20, failure)
    }
    fn run_limits(
        &self,
        program: &str,
        limit: Option<u64>,
        soft: Option<u64>,
        hard: u64,
        release_failure: bool,
    ) -> Output {
        self.run_config(program, (limit, soft, hard), release_failure, false, None)
    }
    pub(super) fn run_pre_touch(&self, program: &str, enabled: bool) -> Output {
        self.run_config(
            program,
            (None, Some(128 << 10), 64 << 20),
            false,
            enabled,
            None,
        )
    }
    pub(super) fn run_log_budget(&self, program: &str, format: &'static str, limit: u64) -> Output {
        self.run_config(
            program,
            (Some(limit), Some(128 << 10), 64 << 20),
            false,
            true,
            Some(format),
        )
    }
    pub(super) fn run_log_format(&self, program: &str, format: &'static str) -> Output {
        self.run_config(
            program,
            (None, Some(128 << 10), 64 << 20),
            false,
            true,
            Some(format),
        )
    }
    fn run_config(
        &self,
        program: &str,
        (limit, soft, hard): (Option<u64>, Option<u64>, u64),
        release_failure: bool,
        pre_touch: bool,
        log_format: Option<&str>,
    ) -> Output {
        let source = self.directory.join("source.kl");
        let image = self.directory.join("image.bin");
        fs::write(&source, program).unwrap();
        let minimum_flag = format!("--gc-min-heap={}", soft.unwrap_or(16 << 20).min(16 << 20));
        let hard_flag = format!("--gc-max-heap={hard}");
        let soft_flag = soft.map(|value| format!("--gc-soft-max-heap={value}"));
        let cap = limit.map(|value| format!("--gc-max-metadata={value}"));
        let mut flags = vec![
            "--target",
            self.target,
            "--gc=generational",
            "--gc-log",
            &hard_flag,
            &minimum_flag,
        ];
        if let Some(format) = log_format {
            flags.retain(|flag| *flag != "--gc-log");
            flags.extend([
                "--gc-log-format",
                format,
                "--gc-uncommit-delay=18446744073709551615ns",
            ]);
        }
        if let Some(cap) = &cap {
            flags.push(cap);
        }
        if let Some(soft) = &soft_flag {
            flags.push(soft);
        }
        if pre_touch {
            flags.push("--gc-pre-touch");
        }
        let output = build(&flags, &source, &image);
        assert!(output.status.success(), "{}: {output:?}", self.target);
        if let Some(driver) = &self.driver {
            let mut model = super::windows_production::run(
                driver,
                &image,
                if release_failure { 20 } else { 0 },
            );
            if release_failure {
                return model;
            }
            if log_format == Some("json") {
                strip_driver_log(&mut model, "\nwin-production:");
            }
            if let Some(mut native) =
                super::windows_production::run_native_image(&self.directory, &image)
            {
                if log_format == Some("json") {
                    strip_driver_log(&mut native, "\nwin-native:");
                }
                eprintln!(
                    "windows-native-metadata: exit={:?} limit={limit:?} soft={soft:?}",
                    native.status.code()
                );
                assert_eq!(
                    native.status.code(),
                    model.status.code(),
                    "native/model exit: {native:?} {model:?}"
                );
                assert_eq!(
                    native.stdout, model.stdout,
                    "native/model output: {native:?} {model:?}"
                );
                if model.status.success() {
                    let parse = |output: &Output| {
                        let text = String::from_utf8_lossy(&output.stderr);
                        if log_format == Some("json") {
                            super::log_format::json_stats(&text)
                        } else {
                            gc_stats(&text)
                        }
                    };
                    let m = parse(&model);
                    let n = parse(&native);
                    for field in [
                        "metadata_initial_bytes",
                        "metadata_limit_bytes",
                        "heap_soft_max_bytes",
                        "heap_capacity_bytes",
                    ] {
                        assert_eq!(m[field], n[field]);
                    }
                    assert_eq!(m["metadata_pending_bytes"], 0);
                    assert_eq!(m["metadata_charged_bytes"], m["metadata_committed_bytes"]);
                    assert!(m["metadata_charged_bytes"] <= m["metadata_limit_bytes"]);
                }
                native
            } else {
                model
            }
        } else if let Some(python) = &self.python {
            let mut output = Command::new(python)
                .arg(Path::new(env!("CARGO_MANIFEST_DIR")).join("tools/run-macho-aarch64.py"))
                .arg(image)
                .env(
                    "KLASSIC_MACHO_METADATA_RELEASE_FAULT",
                    if release_failure { "1" } else { "" },
                )
                .env("KLASSIC_MACHO_WORKER_FAULT", "")
                .env("KLASSIC_MACHO_WORKER_IDLE_WRITE", "")
                // Preserve the full retirement workload with precise frame
                // root barriers; this model budget is not a latency limit.
                .env("KLASSIC_MACHO_WORKER_QUANTUM", "1000000")
                .env("KLASSIC_MACHO_WORKER_SECONDS", "600")
                .output()
                .unwrap();
            if log_format == Some("json") {
                strip_driver_log(&mut output, "\nmacos-worker-model:");
            }
            output
        } else {
            Command::new(image).output().unwrap()
        }
    }
}
const TARGETS: [&str; 3] = [
    "x86_64-unknown-linux-gnu",
    "x86_64-pc-windows-msvc",
    "aarch64-apple-darwin",
];

#[test]
fn metadata_stats_include_initial_roots_and_lazy_copy_backings_on_each_direct_target() {
    for target in TARGETS {
        let Some(f) = Fixture::new(target) else {
            continue;
        };
        let output = f.run(CHURN, None);
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(output.status.success(), "{target}: {output:?}");
        assert_eq!(output.stdout, b"5120\n32\n", "{target}: {stderr}");
        let v = gc_stats(&stderr);
        let alignment = if target == "aarch64-apple-darwin" {
            16384
        } else {
            4096
        };
        assert!(v["collections"] > 0, "{stderr}");
        assert!(
            v["metadata_committed_bytes"] > v["metadata_initial_bytes"],
            "{stderr}"
        );
        assert_eq!(v["metadata_pending_bytes"], 0, "{stderr}");
        assert_eq!(
            v["metadata_charged_bytes"], v["metadata_committed_bytes"],
            "{stderr}"
        );
        assert!(
            v["metadata_peak_committed_bytes"] >= v["metadata_committed_bytes"],
            "{stderr}"
        );
        assert_eq!(v["metadata_unmap_failures"], 0, "{stderr}");
        assert!(
            v["metadata_committed_bytes"] <= v["metadata_limit_bytes"],
            "{stderr}"
        );
        assert!(
            v["metadata_maps"] > if alignment == 16384 { 2 } else { 1 },
            "{stderr}"
        );
        assert_eq!(v["metadata_map_failures"], 0, "{stderr}");
        assert_eq!(v["metadata_initial_bytes"] % alignment, 0);
        assert_eq!(v["metadata_committed_bytes"] % alignment, 0);
        assert_eq!(v["heap_capacity_bytes"], 16 << 20);
    }
}

#[test]
fn metadata_initial_exact_fit_and_copy_budget_exhaustion_are_independent_of_payload() {
    for target in TARGETS {
        let Some(f) = Fixture::new(target) else {
            continue;
        };
        let initial = f.run("println(42)", None);
        assert!(initial.status.success(), "{target}: {initial:?}");
        let v = gc_stats(&String::from_utf8_lossy(&initial.stderr));
        let bytes = v["metadata_initial_bytes"];
        assert_eq!(v["metadata_committed_bytes"], bytes);
        // All targets use the shared initial RootCell backing; Darwin no
        // longer maps a separate raw shadow-stack table.
        assert_eq!(v["metadata_maps"], 1);
        let exact = f.run("println(42)", Some(bytes));
        assert!(exact.status.success(), "{target}: {exact:?}");
        assert_eq!(exact.stdout, b"42\n");
        let stats = gc_stats(&String::from_utf8_lossy(&exact.stderr));
        assert_eq!(stats["metadata_limit_bytes"], bytes);
        assert_eq!(stats["metadata_committed_bytes"], bytes);
        for (program, cap) in [("println(42)", bytes - 4096), (CHURN, bytes)] {
            let output = f.run(program, Some(cap));
            assert_eq!(output.status.code(), Some(1), "{target}: {output:?}");
            assert!(
                String::from_utf8_lossy(&output.stderr).contains("metadata budget exhausted"),
                "{target}: {output:?}"
            );
            assert!(output.stdout.is_empty(), "{target}: {output:?}");
        }
    }
}

#[test]
fn metadata_source_retirement_unmaps_owned_backings_and_preserves_values_on_each_target() {
    let program = CHURN.replace("i < 160", "i < 1600");
    for target in TARGETS {
        let Some(f) = Fixture::new(target) else {
            continue;
        };
        let output = f.run_retirement(&program, false);
        assert!(output.status.success(), "{target}: {output:?}");
        assert_eq!(output.stdout, b"51200\n32\n");
        let stderr = String::from_utf8_lossy(&output.stderr);
        let v = gc_stats(&stderr);
        assert!(v["collections"] > 1, "{target}: {stderr}");
        assert!(v["metadata_unmaps"] > 0, "{target}: {stderr}");
        assert_eq!(v["metadata_unmap_failures"], 0, "{target}: {stderr}");
        assert!(v["metadata_peak_committed_bytes"] >= v["metadata_committed_bytes"]);
        assert_eq!(v["metadata_charged_bytes"], v["metadata_committed_bytes"]);
        assert_eq!(v["metadata_pending_bytes"], 0);
        assert_eq!(v["metadata_retire_pending"], 0);
    }
}
#[test]
fn metadata_release_api_failures_report_controlled_errors_on_windows_and_darwin() {
    let program = CHURN.replace("i < 160", "i < 1600");
    for target in ["x86_64-pc-windows-msvc", "aarch64-apple-darwin"] {
        let Some(f) = Fixture::new(target) else {
            continue;
        };
        let output = f.run_retirement(&program, true);
        assert_eq!(output.status.code(), Some(1), "{target}: {output:?}");
        assert!(
            String::from_utf8_lossy(&output.stderr).contains("native metadata release failed"),
            "{target}: {output:?}"
        );
    }
}

#[test]
fn metadata_scaled_fresh_record_banks_fit_a_smaller_budget_and_keep_the_hard_capacity_distinct() {
    for target in TARGETS {
        let Some(f) = Fixture::new(target) else {
            continue;
        };
        let small = f.run(CHURN, Some(160 << 20));
        assert!(small.status.success(), "{target}: {small:?}");
        assert_eq!(small.stdout, b"5120\n32\n");
        let s = gc_stats(&String::from_utf8_lossy(&small.stderr));
        assert_eq!(s["metadata_limit_bytes"], 160 << 20);
        assert!(s["metadata_initial_bytes"] < 160 << 20);
        assert!(s["metadata_charged_bytes"] <= 160 << 20);
        let large = f.run_limits("println(42)", None, None, 192 << 20, false);
        assert!(large.status.success(), "{target}: {large:?}");
        assert_eq!(large.stdout, b"42\n");
        let l = gc_stats(&String::from_utf8_lossy(&large.stderr));
        assert!(l["metadata_initial_bytes"] > s["metadata_initial_bytes"] + (128 << 20));
        eprintln!(
            "scaled-arena: target={target} small_initial={} large_initial={}",
            s["metadata_initial_bytes"], l["metadata_initial_bytes"]
        );
    }
}
