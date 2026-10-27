//! `--gc=generational` on the direct AArch64 Mach-O backend, executed
//! through `tools/run-macho-aarch64.py` (a unicorn emulator) so the Linux
//! development host can diff the binary against the evaluator. When no
//! Python with `unicorn` is found the tests only check that the build
//! succeeds and say so on stderr; the arm64 CI runner is the authority.
use super::*;

/// A Python interpreter that can import `unicorn`: `$KLASSIC_UNICORN_PYTHON`,
/// then `python3`, then any pyenv-installed `python3`.
pub(super) fn emulator_python() -> Option<PathBuf> {
    let mut candidates: Vec<PathBuf> = std::env::var_os("KLASSIC_UNICORN_PYTHON")
        .map(PathBuf::from)
        .into_iter()
        .collect();
    candidates.push(PathBuf::from("python3"));
    if let Some(home) = std::env::var_os("HOME") {
        let versions = PathBuf::from(home).join(".pyenv/versions");
        if let Ok(entries) = fs::read_dir(versions) {
            let mut found: Vec<PathBuf> = entries
                .filter_map(|entry| entry.ok())
                .map(|entry| entry.path().join("bin/python3"))
                .collect();
            found.sort();
            candidates.extend(found);
        }
    }
    candidates.into_iter().find(|python| {
        Command::new(python)
            .args(["-c", "import unicorn"])
            .output()
            .is_ok_and(|output| output.status.success())
    })
}

/// Build `source` for aarch64-apple-darwin with `flags` and run it in the
/// emulator; `None` when no emulator is available.
pub(super) fn build_and_emulate(tag: &str, flags: &[&str], source: &str) -> Option<Output> {
    let source_path = write_source(tag, source);
    let output_path = temp_path(tag, "macho");
    let mut all = vec!["--target", "aarch64-apple-darwin"];
    all.extend_from_slice(flags);
    let result = build(&all, &source_path, &output_path);
    let _ = fs::remove_file(&source_path);
    assert!(
        result.status.success(),
        "aarch64 build with {flags:?} should succeed\nstderr:\n{}",
        String::from_utf8_lossy(&result.stderr)
    );
    let Some(python) = emulator_python() else {
        let _ = fs::remove_file(&output_path);
        eprintln!("note: no Python with unicorn; {tag} was built but not executed");
        return None;
    };
    let script = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tools/run-macho-aarch64.py");
    let mut command = Command::new(python);
    command.arg(script).arg(&output_path);
    if tag == "a64s" {
        // Preserve the complete 3,000-iteration forced-GC workload with
        // deterministic instruction quanta and explicit YIELD scheduling.
        command
            .env("KLASSIC_MACHO_WORKER_QUANTUM", "1000000")
            .env("KLASSIC_MACHO_WORKER_SECONDS", "3600");
    }
    let run = command.output().expect("emulator should run");
    let _ = fs::remove_file(&output_path);
    Some(run)
}

/// Heap strings, enums (incl. a nested generic), a long-lived tree, deep
/// recursion and a garbage-heavy loop.
const PROGRAM: &str = "enum Option<a> { case Some(value: a); case None }\n\
enum Tree { case Leaf(v: Int); case Branch(l: Tree, r: Tree) }\n\
def total(t: Tree): Int = t match { case Leaf(v) => v; case Branch(l, r) => total(l) + total(r) }\n\
def build(n: Int): Tree = if (n <= 0) Leaf(1) else Branch(build(n - 1), Leaf(n))\n\
def describe(s: String, n: Int): String = if (n <= 0) s else describe(s + \"x\", n - 1)\n\
def depth(n: Int): Int = if (n <= 0) 0 else 1 + depth(n - 1)\n\
val keep = build(200)\n\
val o = Some(Some(\"hi\"))\n\
mutable i = 0\n\
mutable acc = 0\n\
while(i < 3000) {\n\
  acc += length(describe(\"abcdefghij\", 20))\n\
  i += 1\n\
}\n\
println(acc)\n\
println(total(keep))\n\
println(o match { case Some(Some(v)) => v; case _ => \"x\" })\n\
println(describe(\"k\", 3))\n\
println(depth(5000))\n";

#[test]
fn aarch64_generational_matches_the_evaluator() {
    let expected = evaluate("a64_eval", PROGRAM);
    let Some(run) = build_and_emulate("a64", &["--gc=generational", "--gc-log"], PROGRAM) else {
        return;
    };
    let stderr = String::from_utf8_lossy(&run.stderr);
    assert!(run.status.success(), "stderr:\n{stderr}");
    assert_eq!(String::from_utf8_lossy(&run.stdout), expected);
    let stats = gc_stats(&stderr);
    assert_eq!(
        stats["tenuring_full_samples"], stats["full_collections"],
        "stats: {stats:?}"
    );
    assert_eq!(
        stats["tenuring_full_samples"] + stats["tenuring_young_samples"],
        stats["collections"],
        "stats: {stats:?}"
    );
    assert!(
        (2..=15).contains(&stats["tenuring_threshold"]),
        "stats: {stats:?}"
    );
    assert!(
        (2..=15).contains(&stats["tenuring_frozen"]),
        "stats: {stats:?}"
    );
    assert!(stats["fresh_allocated_bytes"] > 0, "stats: {stats:?}");
    assert_eq!(
        stats["old_full_samples"], stats["full_collections"],
        "stats: {stats:?}"
    );
    assert_eq!(
        stats["old_full_samples"] + stats["old_young_samples"],
        stats["collections"],
        "stats: {stats:?}"
    );
    assert!(
        (stats["old_limit_bytes"] / 4
            ..=stats["old_limit_bytes"] - stats["young_limit_pages"] * 131072)
            .contains(&stats["old_target_bytes"]),
        "stats: {stats:?}"
    );
    assert!(stats["objects"] > 0, "stats: {stats:?}");
    assert_eq!(stats["objects"], stats["published"], "stats: {stats:?}");
}

#[test]
fn aarch64_generational_survives_stress_collections() {
    // Keep the retained depth-200 graph and >1000 forced collections, while
    // the normal (non-stress) case retains the full 3000-iteration workload.
    // 64 iterations allocate at least 1280 strings after graph construction.
    let program = PROGRAM.replace("while(i < 3000)", "while(i < 64)");
    let expected = evaluate("a64s_eval", &program);
    let flags = ["--gc=generational", "--gc-log", "--gc-stress"];
    let Some(run) = build_and_emulate("a64s", &flags, &program) else {
        return;
    };
    let stderr = String::from_utf8_lossy(&run.stderr);
    assert!(run.status.success(), "stderr:\n{stderr}");
    assert_eq!(String::from_utf8_lossy(&run.stdout), expected);
    // A collection before every allocation, and the long-lived tree is
    // promoted to Old.
    let stats = gc_stats(&stderr);
    assert_eq!(
        stats["tenuring_full_samples"], stats["full_collections"],
        "stats: {stats:?}"
    );
    assert_eq!(
        stats["tenuring_full_samples"] + stats["tenuring_young_samples"],
        stats["collections"],
        "stats: {stats:?}"
    );
    assert!(
        (2..=15).contains(&stats["tenuring_threshold"]),
        "stats: {stats:?}"
    );
    assert!(
        (2..=15).contains(&stats["tenuring_frozen"]),
        "stats: {stats:?}"
    );
    assert!(stats["fresh_allocated_bytes"] > 0, "stats: {stats:?}");
    assert_eq!(
        stats["old_full_samples"], stats["full_collections"],
        "stats: {stats:?}"
    );
    assert_eq!(
        stats["old_full_samples"] + stats["old_young_samples"],
        stats["collections"],
        "stats: {stats:?}"
    );
    assert!(
        (stats["old_limit_bytes"] / 4
            ..=stats["old_limit_bytes"] - stats["young_limit_pages"] * 131072)
            .contains(&stats["old_target_bytes"]),
        "stats: {stats:?}"
    );
    assert!(stats["collections"] > 1000, "stats: {stats:?}");
    assert!(stats["promoted_bytes"] > 0, "stats: {stats:?}");
}

#[test]
fn aarch64_generational_stress_roots_print_cursors_and_paths() {
    // Printing a nested list allocates each inner list's text while the
    // outer cursor waits (it must be a root under a moving collection), and
    // FileInput#all reads through a native mapping rather than a 1 MiB heap
    // buffer, which would exceed a generational page.
    let source = "val grid = [[1 2 3] [4 5 6]]\n\
                  println(grid)\n\
                  val text = FileInput#all(\"Cargo.toml\")\n\
                  println(length(text) > 0)\n";
    let expected = evaluate("a64r_eval", source);
    let flags = ["--gc=generational", "--gc-stress"];
    let Some(run) = build_and_emulate("a64r", &flags, source) else {
        return;
    };
    assert!(
        run.status.success(),
        "stderr:\n{}",
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&run.stdout), expected);
}

#[test]
fn aarch64_generational_boxed_reference_like_integer_stays_scalar() {
    let expected = evaluate("a64_scalar_eval", OPAQUE_SCALAR_PROGRAM);
    for flags in [
        &["--gc=generational"][..],
        &["--gc=generational", "--gc-stress"],
    ] {
        let Some(run) = build_and_emulate("a64_scalar", flags, OPAQUE_SCALAR_PROGRAM) else {
            return;
        };
        assert!(
            run.status.success(),
            "{}",
            String::from_utf8_lossy(&run.stderr)
        );
        assert_eq!(String::from_utf8_lossy(&run.stdout), expected, "{flags:?}");
    }
}

#[test]
fn aarch64_configured_heap_limits_drive_the_portable_runtime() {
    let expected = evaluate("a64_heap_eval", OPAQUE_SCALAR_PROGRAM);
    for size in [16, 512] {
        let flag = format!("--gc-max-heap={size}MiB");
        let flags = ["--gc=generational", "--gc-log", "--gc-stress", &flag];
        let Some(run) = build_and_emulate("a64_heap", &flags, OPAQUE_SCALAR_PROGRAM) else {
            return;
        };
        let stderr = String::from_utf8_lossy(&run.stderr);
        assert!(run.status.success(), "{stderr}");
        assert_eq!(String::from_utf8_lossy(&run.stdout), expected);
        let stats = gc_stats(&stderr);
        assert_eq!(stats["heap_capacity_bytes"], size << 20);
        assert_eq!(stats["old_limit_bytes"], (size << 20) / 131072 / 3 * 131072);
    }
}

#[test]
fn aarch64_minimum_heap_is_protected_then_committed_on_demand() {
    let flags = [
        "--gc=generational",
        "--gc-log",
        "--gc-stress",
        "--gc-max-heap=16MiB",
        "--gc-min-heap=128KiB",
    ];
    let Some(run) = build_and_emulate("a64_min_heap", &flags, OPAQUE_SCALAR_PROGRAM) else {
        return;
    };
    let stderr = String::from_utf8_lossy(&run.stderr);
    assert!(run.status.success(), "{stderr}");
    assert_eq!(String::from_utf8_lossy(&run.stdout), "true\n8595182865\n");
    let stats = gc_stats(&stderr);
    assert_eq!(stats["heap_initial_committed_bytes"], 128 << 10);
    assert!(stats["heap_commit_pages"] > 1);
    assert_eq!(
        stats["heap_commit_pages"] * 131072,
        stats["heap_committed_bytes"]
    );
    assert!(stats["heap_peak_committed_bytes"] <= 16 << 20);
}

#[test]
fn aarch64_zero_uncommit_delay_runs_owner_maintenance_and_preserves_live_values() {
    let flags = [
        "--gc=generational",
        "--gc-log",
        "--gc-stress",
        "--gc-max-heap=16MiB",
        "--gc-min-heap=128KiB",
        "--gc-uncommit-delay=0ns",
    ];
    // More than two complete owner scan rounds; the short scalar fixture
    // alone performs fewer than two 64-allocation maintenance samples.
    let source = OPAQUE_SCALAR_PROGRAM.replace("i < 40", "i < 1024");
    let Some(run) = build_and_emulate("a64_uncommit", &flags, &source) else {
        return;
    };
    let stderr = String::from_utf8_lossy(&run.stderr);
    assert!(run.status.success(), "{stderr}");
    assert_eq!(String::from_utf8_lossy(&run.stdout), "true\n8595182865\n");
    let v = gc_stats(&stderr);
    assert_eq!(v["heap_uncommit_delay_ns"], 0);
    assert!(v["heap_uncommit_pages"] > 0, "{v:?}");
    assert!(v["heap_recommit_pages"] > 0, "{v:?}");
    assert_eq!(
        v["heap_committed_bytes"],
        (v["heap_commit_pages"] - v["heap_uncommit_pages"]) * 131072
    );
    assert!(v["heap_committed_bytes"] >= 128 << 10, "{v:?}");
    assert!(v["heap_peak_committed_bytes"] <= 16 << 20, "{v:?}");
}

#[test]
fn aarch64_full_unsigned_uncommit_delay_is_logged_exactly() {
    let flags = [
        "--gc=generational",
        "--gc-log",
        "--gc-min-heap=128KiB",
        "--gc-max-heap=16MiB",
        "--gc-uncommit-delay=18446744073709551615ns",
    ];
    let Some(run) = build_and_emulate("a64_delay_u64", &flags, "println(1)\n") else {
        return;
    };
    let stderr = String::from_utf8_lossy(&run.stderr);
    assert!(run.status.success(), "{stderr}");
    assert_eq!(run.stdout, b"1\n");
    assert!(
        stderr.contains("heap_uncommit_delay_ns=18446744073709551615"),
        "{stderr}"
    );
    assert_eq!(gc_stats(&stderr)["heap_uncommit_delay_ns"], u64::MAX);
}
