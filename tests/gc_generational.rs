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

#[allow(dead_code)]
fn gc_stats(stderr: &str) -> std::collections::HashMap<String, u64> {
    let line = stderr
        .lines()
        .find(|line| line.starts_with("gc[generational]:"))
        .unwrap_or_else(|| panic!("no generational --gc-log line in:\n{stderr}"));
    line["gc[generational]:".len()..]
        .split_whitespace()
        .map(|field| {
            let (name, value) = field.split_once('=').expect("name=value field");
            (name.to_string(), value.parse().expect("numeric field"))
        })
        .collect()
}

#[allow(dead_code)]
fn evaluate(tag: &str, source: &str) -> String {
    let source_path = write_source(tag, source);
    let result = Command::new(klassic_bin())
        .arg(&source_path)
        .output()
        .expect("klassic should run");
    let _ = fs::remove_file(&source_path);
    assert!(
        result.status.success(),
        "evaluator should accept the program\nstderr:\n{}",
        String::from_utf8_lossy(&result.stderr)
    );
    String::from_utf8_lossy(&result.stdout).into_owned()
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
fn generational_rejects_unavailable_c_and_llvm_executables() {
    for backend in ["llvm", "c"] {
        let stderr = build_failure_stderr(
            "backend",
            &[
                "--gc=generational",
                "--target",
                "x86_64-unknown-linux-gnu",
                "--backend",
                backend,
            ],
            "println(1)\n",
        );
        assert!(
            stderr.contains("canonical C/LLVM executable linking is not yet enabled"),
            "backend {backend} stderr:\n{stderr}"
        );
    }
}

#[test]
fn generational_builds_for_every_direct_target() {
    // Every direct backend carries the generational runtime; only
    // --backend c/llvm are declined.
    for target in [
        "x86_64-unknown-linux-gnu",
        "x86_64-pc-windows-msvc",
        "aarch64-apple-darwin",
    ] {
        let source_path = write_source("target", "println(\"hi\")\n");
        let output_path = temp_path("target", "bin");
        let result = build(
            &["--gc=generational", "--target", target],
            &source_path,
            &output_path,
        );
        let _ = fs::remove_file(&source_path);
        let _ = fs::remove_file(&output_path);
        assert!(
            result.status.success(),
            "{target}: stderr:\n{}",
            String::from_utf8_lossy(&result.stderr)
        );
    }
}

#[test]
fn generational_rejects_legacy_gc_debug_flags() {
    let stderr = build_failure_stderr(
        "debug",
        &["--gc=generational", "--gc-poison"],
        "println(1)\n",
    );
    assert!(
        stderr.contains("not accepted with --gc=generational"),
        "stderr:\n{stderr}"
    );
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
    fn generational_promotes_long_lived_data_and_collects_old() {
        // A long-lived tree is promoted to Old and stays reachable while
        // Young churns; under stress every survivor is promoted, so Old
        // fills and full collections compact it.
        let source = "enum Tree { case Leaf(v: Int); case Branch(l: Tree, r: Tree) }\n\
                      def total(t: Tree): Int = t match { case Leaf(v) => v; case Branch(l, r) => total(l) + total(r) }\n\
                      def build(n: Int): Tree = if (n <= 0) Leaf(1) else Branch(build(n - 1), Leaf(n))\n\
                      def describe(s: String, n: Int): String = if (n <= 0) s else describe(s + \"x\", n - 1)\n\
                      val keep = build(200)\n\
                      mutable i = 0\n\
                      mutable acc = 0\n\
                      while(i < 40000) {\n\
                        acc += length(describe(\"abcdefghij\", i / 60000 + 20))\n\
                        i += 1\n\
                      }\n\
                      println(acc)\n\
                      println(total(keep))\n";
        let expected = evaluate("old_eval", source);
        let run = build_and_run("old", &["--gc=generational", "--gc-log"], source);
        let stderr = String::from_utf8_lossy(&run.stderr);
        assert!(run.status.success(), "stderr:\n{stderr}");
        assert_eq!(String::from_utf8_lossy(&run.stdout), expected);
        let stats = gc_stats(&stderr);
        assert!(stats["collections"] >= 2, "stats: {stats:?}");
        assert!(stats["promoted_bytes"] > 0, "stats: {stats:?}");
        let run = build_and_run(
            "old_stress",
            &["--gc=generational", "--gc-log", "--gc-stress"],
            source,
        );
        let stderr = String::from_utf8_lossy(&run.stderr);
        assert!(run.status.success(), "stderr:\n{stderr}");
        assert_eq!(String::from_utf8_lossy(&run.stdout), expected);
        let stats = gc_stats(&stderr);
        assert!(stats["full_collections"] >= 1, "stats: {stats:?}");
    }

    #[test]
    fn generational_windows_build_matches_the_evaluator_when_runnable() {
        // PE shares the x86-64 emission; only the mapping leaf (VirtualAlloc)
        // and the write path (the Win64 shim) differ. The .exe runs where WSL
        // interop is available; elsewhere the build itself is the check.
        let source = "enum Option<a> { case Some(value: a); case None }\n\
                      def describe(s: String, n: Int): String = if (n <= 0) s else describe(s + \"x\", n - 1)\n\
                      val o = Some(Some(\"hi\"))\n\
                      mutable i = 0\n\
                      mutable acc = 0\n\
                      while(i < 2000) {\n\
                        acc += length(describe(\"ab\", i / 60000 + 5))\n\
                        i += 1\n\
                      }\n\
                      println(acc)\n\
                      println(o match { case Some(Some(v)) => v; case _ => \"x\" })\n";
        let expected = evaluate("win_eval", source);
        let source_path = write_source("win", source);
        let output_path = temp_path("win", "exe");
        let result = build(
            &[
                "--target",
                "x86_64-pc-windows-msvc",
                "--gc=generational",
                "--gc-stress",
            ],
            &source_path,
            &output_path,
        );
        let _ = fs::remove_file(&source_path);
        assert!(
            result.status.success(),
            "stderr:\n{}",
            String::from_utf8_lossy(&result.stderr)
        );
        let bytes = fs::read(&output_path).expect("exe written");
        assert_eq!(&bytes[..2], b"MZ");
        if let Ok(run) = Command::new(&output_path).output()
            && run.status.success()
        {
            let stdout = String::from_utf8_lossy(&run.stdout).replace('\r', "");
            assert_eq!(stdout, expected);
        }
        let _ = fs::remove_file(&output_path);
    }

    #[test]
    fn generational_enums_survive_evacuation() {
        // Records hold references (payload addresses) to boxes, strings and
        // nested records; stress evacuates before every allocation.
        let source = "enum Option<a> { case Some(value: a); case None }\n\
                      enum Tree { case Leaf(v: Int); case Branch(l: Tree, r: Tree) }\n\
                      def total(t: Tree): Int = t match { case Leaf(v) => v; case Branch(l, r) => total(l) + total(r) }\n\
                      def build(n: Int): Tree = if (n <= 0) Leaf(1) else Branch(build(n - 1), Leaf(n))\n\
                      val o = Some(Some(\"hi\"))\n\
                      println(o match { case Some(Some(v)) => v; case _ => \"x\" })\n\
                      println(total(build(50)))\n\
                      println(Some(Some(\"hi\")) == o)\n\
                      println(Some(Some(\"ho\")) == o)\n";
        let expected = evaluate("enum_eval", source);
        for flags in [
            &["--gc=generational"][..],
            &["--gc=generational", "--gc-stress"],
        ] {
            let run = build_and_run("enum", flags, source);
            assert!(
                run.status.success(),
                "{flags:?} stderr:\n{}",
                String::from_utf8_lossy(&run.stderr)
            );
            assert_eq!(String::from_utf8_lossy(&run.stdout), expected, "{flags:?}");
        }
    }

    /// The `--gc-log` line of a generational run, parsed into its fields.
    #[test]
    fn generational_startup_runs_and_reports_an_empty_heap() {
        let run = build_and_run(
            "startup",
            &["--gc=generational", "--gc-log"],
            "println(1 + 2)\n",
        );
        let stderr = String::from_utf8_lossy(&run.stderr);
        assert!(run.status.success(), "stderr:\n{stderr}");
        assert_eq!(String::from_utf8_lossy(&run.stdout), "3\n");
        let stats = gc_stats(&stderr);
        assert_eq!(stats["collections"], 0);
        assert_eq!(stats["objects"], stats["published"]);
    }

    /// Evaluator stdout for `source`, the oracle for native output.
    /// Heap strings cross function calls through the pointer ABI, so these
    /// programs allocate on the generational heap (asserted via --gc-log).
    const HEAP_STRING_PROGRAM: &str = "def describe(s: String, n: Int): String = if (n <= 0) s else describe(s + \"x\", n - 1)\n\
         def ident(s: String): String = s\n\
         def churn(s: String, n: Int): Int = if (n <= 0) length(s) else churn(s + \"ab\", n - 1)\n\
         def lastFour(s: String): String = substring(s, length(s) - 4, length(s))\n\
         def pick(s: String, i: Int): String = s.at(i)\n\
         println(describe(\"r\", 4))\n\
         println(length(describe(\"\u{3042}\u{3044}\u{3046}\", 2)))\n\
         println(lastFour(ident(\"abcdefTAIL\")))\n\
         println(pick(ident(\"xyz\"), 1))\n\
         println(churn(\"\", 2000))\n\
         println(ident(\"a\") == ident(\"a\"))\n\
         println(\"#{describe(\"i\", 2)}-#{length(ident(\"four\"))}\")\n";

    #[test]
    fn generational_heap_strings_match_the_evaluator_across_pages() {
        let expected = evaluate("heap_eval", HEAP_STRING_PROGRAM);
        let run = build_and_run(
            "heap",
            &["--gc=generational", "--gc-log"],
            HEAP_STRING_PROGRAM,
        );
        let stderr = String::from_utf8_lossy(&run.stderr);
        assert!(run.status.success(), "stderr:\n{stderr}");
        assert_eq!(String::from_utf8_lossy(&run.stdout), expected);
        let stats = gc_stats(&stderr);
        assert_eq!(stats["collections"], 0);
        // churn(2000) allocates ~4 MB, so allocation refilled across pages.
        assert!(stats["pages_used"] > 1, "stats: {stats:?}");
        assert!(stats["objects"] > 2000, "stats: {stats:?}");
        // Every object on the used pages carries the raw-bytes descriptor
        // and was published through the guarded initialization protocol.
        assert_eq!(stats["objects"], stats["published"], "stats: {stats:?}");
        assert!(stats["allocated_bytes"] > 4_000_000, "stats: {stats:?}");
    }

    #[test]
    fn generational_reuses_frame_roots_in_long_loops() {
        // Each iteration roots a fresh heap string in a loop-body slot and
        // drops it at the end of the body; RESERVE never recycles a cell,
        // so without LIFO reuse this would exhaust the 65536-cell segment.
        // It also drives the root journal through many FLUSH drains. The
        // depth depends on `i` so the call cannot be folded at compile time.
        let source = "def describe(s: String, n: Int): String = if (n <= 0) s else describe(s + \"x\", n - 1)\n\
                      mutable i = 0\n\
                      mutable total = 0\n\
                      while(i < 100000) {\n\
                        val t = describe(\"ab\", i / 50000 + 1)\n\
                        total += length(t)\n\
                        i += 1\n\
                      }\n\
                      println(total)\n";
        assert_eq!(evaluate("loop_eval", source), "350000\n");
        let run = build_and_run("loop", &["--gc=generational", "--gc-log"], source);
        let stderr = String::from_utf8_lossy(&run.stderr);
        assert!(run.status.success(), "stderr:\n{stderr}");
        assert_eq!(String::from_utf8_lossy(&run.stdout), "350000\n");
        let stats = gc_stats(&stderr);
        assert!(stats["objects"] >= 100_000, "stats: {stats:?}");
        assert_eq!(stats["objects"], stats["published"], "stats: {stats:?}");
    }

    #[test]
    fn generational_collects_garbage_across_semispace_flips() {
        // Every iteration drops ~1.3 KB of heap strings, ~80 MB in total:
        // more than the 64 MiB heap, so this only finishes by evacuating
        // the live strings and recycling the dead pages.
        let source = "def describe(s: String, n: Int): String = if (n <= 0) s else describe(s + \"x\", n - 1)\n\
                      mutable i = 0\n\
                      mutable total = 0\n\
                      while(i < 60000) {\n\
                        total += length(describe(\"abcdefghij\", i / 60000 + 20))\n\
                        i += 1\n\
                      }\n\
                      println(total)\n";
        let run = build_and_run("collect", &["--gc=generational", "--gc-log"], source);
        let stderr = String::from_utf8_lossy(&run.stderr);
        assert!(run.status.success(), "stderr:\n{stderr}");
        assert_eq!(String::from_utf8_lossy(&run.stdout), "1800000\n");
        let stats = gc_stats(&stderr);
        assert!(stats["collections"] >= 2, "stats: {stats:?}");
        assert_eq!(stats["objects"], stats["published"], "stats: {stats:?}");
    }

    #[test]
    fn generational_live_data_beyond_a_semispace_is_an_honest_oom() {
        // Recursion keeps every intermediate string live (~36 MB), which
        // cannot fit one 32 MiB semispace; the legacy heap holds it.
        let source = "def churn(s: String, n: Int): Int = if (n <= 0) length(s) else churn(s + \"ab\", n - 1)\n\
                      mutable k = 0\n\
                      while(k < 1) {\n\
                        println(churn(\"\", k + 6000))\n\
                        k += 1\n\
                      }\n";
        let legacy = build_and_run("oom_legacy", &[], source);
        assert_eq!(String::from_utf8_lossy(&legacy.stdout), "12000\n");
        let run = build_and_run("oom", &["--gc=generational"], source);
        let stderr = String::from_utf8_lossy(&run.stderr);
        assert_eq!(run.status.code(), Some(1), "stderr:\n{stderr}");
        assert!(
            stderr
                .contains("out of memory (--gc=generational live data exceeds a young semispace)"),
            "stderr:\n{stderr}"
        );
    }

    #[test]
    fn generational_stress_collects_before_every_allocation() {
        let expected = evaluate("stress_eval", HEAP_STRING_PROGRAM);
        let run = build_and_run(
            "stress",
            &["--gc=generational", "--gc-stress", "--gc-log"],
            HEAP_STRING_PROGRAM,
        );
        let stderr = String::from_utf8_lossy(&run.stderr);
        assert!(run.status.success(), "stderr:\n{stderr}");
        assert_eq!(String::from_utf8_lossy(&run.stdout), expected);
        let stats = gc_stats(&stderr);
        assert!(stats["collections"] > 2000, "stats: {stats:?}");
        assert_eq!(stats["objects"], stats["published"], "stats: {stats:?}");
    }

    #[test]
    fn generational_rejects_objects_larger_than_a_page_at_run_time() {
        // 8 * 2^15 = 262144 bytes, larger than one 128 KiB Young page.
        let source = "def grow(s: String, n: Int): String = if (n <= 0) s else grow(s + s, n - 1)\n\
                      mutable k = 0\n\
                      while(k < 1) {\n\
                        println(length(grow(\"abcdefgh\", k + 15)))\n\
                        k += 1\n\
                      }\n";
        let legacy = build_and_run("large_legacy", &[], source);
        assert_eq!(String::from_utf8_lossy(&legacy.stdout), "262144\n");
        let run = build_and_run("large", &["--gc=generational"], source);
        let stderr = String::from_utf8_lossy(&run.stderr);
        assert_eq!(run.status.code(), Some(1), "stderr:\n{stderr}");
        assert!(
            stderr.contains("object larger than one --gc=generational page (M-G1 limit)"),
            "stderr:\n{stderr}"
        );
    }

    #[test]
    fn generational_prints_multi_chunk_heap_strings() {
        // Printing copies heap bytes out through a 4 KiB native buffer; an
        // 8 KiB+ string exercises several chunks per write, on both fds.
        let source = "def grow(s: String, n: Int): String = if (n <= 0) s else grow(s + s, n - 1)\n\
                      mutable k = 0\n\
                      while(k < 3) {\n\
                        println(grow(\"abcdefgh\", k + 9))\n\
                        printlnError(grow(\"E\", k))\n\
                        k += 1\n\
                      }\n";
        let expected = evaluate("print_eval", source);
        let run = build_and_run("print", &["--gc=generational"], source);
        assert!(run.status.success());
        assert_eq!(String::from_utf8_lossy(&run.stdout), expected);
        assert_eq!(String::from_utf8_lossy(&run.stderr), "E\nEE\nEEEE\n");
    }

    /// Timing lines are the only nondeterministic output in the corpus.
    fn normalize(output: &str) -> String {
        output
            .lines()
            .map(|line| {
                if line.starts_with("it took ") {
                    "it took <n> milli seconds"
                } else {
                    line
                }
            })
            .collect::<Vec<_>>()
            .join("\n")
    }

    /// Differential test over `test-programs/`: every program the
    /// generational build accepts prints exactly what the evaluator prints.
    /// Programs with enums are declined with the M-G1 diagnostic (counted,
    /// not skipped silently); the file-I/O pair is left to
    /// `sample_programs.rs`, which serializes its side effects.
    #[test]
    fn generational_corpus_matches_the_evaluator() {
        let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("test-programs");
        let mut programs = fs::read_dir(&root)
            .expect("test-programs should exist")
            .map(|entry| entry.expect("dir entry").path())
            .filter(|path| path.extension().is_some_and(|ext| ext == "kl"))
            .filter(|path| {
                let name = path.file_name().unwrap().to_string_lossy();
                !name.starts_with("file-")
            })
            .collect::<Vec<_>>();
        programs.sort();
        let (mut compared, mut declined) = (0, 0);
        for program in &programs {
            let evaluated = Command::new(klassic_bin())
                .arg(program)
                .current_dir(&root)
                .output()
                .expect("evaluator should run");
            let output_path = temp_path("corpus", "bin");
            let built = build(&["--gc=generational"], program, &output_path);
            if !built.status.success() {
                let stderr = String::from_utf8_lossy(&built.stderr);
                assert!(
                    stderr.contains("enums under --gc=generational")
                        || stderr.contains("not supported by the native compiler"),
                    "{} failed to build for an unexpected reason:\n{stderr}",
                    program.display()
                );
                declined += 1;
                continue;
            }
            let run = Command::new(&output_path)
                .current_dir(&root)
                .output()
                .expect("compiled binary should run");
            let _ = fs::remove_file(&output_path);
            assert_eq!(
                run.status.success(),
                evaluated.status.success(),
                "{}: exit status differs\nstderr:\n{}",
                program.display(),
                String::from_utf8_lossy(&run.stderr)
            );
            assert_eq!(
                normalize(&String::from_utf8_lossy(&run.stdout)),
                normalize(&String::from_utf8_lossy(&evaluated.stdout)),
                "{}: stdout differs",
                program.display()
            );
            compared += 1;
        }
        assert!(compared >= 50, "compared {compared}, declined {declined}");
    }

    /// A long-lived tree stays reachable (and is read) while each iteration
    /// builds and drops a fresh one, so a collection opens a relocation
    /// phase with a large to-space left to scan.
    const WORKER_PROGRAM: &str = "enum Tree { case Leaf(v: Int); case Node(l: Tree, r: Tree) }\n\
        def build(d: Int): Tree = if (d == 0) Leaf(1) else Node(build(d - 1), build(d - 1))\n\
        def sum(t: Tree): Int = t match { case Leaf(v) => v; case Node(l, r) => sum(l) + sum(r) }\n\
        val keep = build(14)\n\
        mutable i = 0\n\
        mutable total = 0\n\
        while (i < 200) {\n\
          val t = build(10)\n\
          total += sum(t)\n\
          if (i / 50 * 50 == i) { total += sum(keep) }\n\
          i += 1\n\
        }\n\
        println(total)\n\
        println(sum(keep))\n";

    #[test]
    fn generational_worker_thread_relocates_concurrently() {
        // The worker reports the objects it scanned; the mutator keeps
        // allocating and reading the evacuated tree meanwhile. Each build
        // runs several times to shake out races between the two threads.
        let expected = evaluate("worker_eval", WORKER_PROGRAM);
        for flags in [
            &["--gc=generational", "--gc-log"][..],
            &["--gc=generational", "--gc-log", "--gc-stress"],
        ] {
            let source_path = write_source("worker", WORKER_PROGRAM);
            let output_path = temp_path("worker", "bin");
            let result = build(flags, &source_path, &output_path);
            let _ = fs::remove_file(&source_path);
            assert!(
                result.status.success(),
                "stderr:\n{}",
                String::from_utf8_lossy(&result.stderr)
            );
            for _ in 0..5 {
                let run = Command::new(&output_path).output().expect("binary runs");
                let stderr = String::from_utf8_lossy(&run.stderr);
                assert!(run.status.success(), "{flags:?} stderr:\n{stderr}");
                assert_eq!(String::from_utf8_lossy(&run.stdout), expected, "{flags:?}");
                let stats = gc_stats(&stderr);
                assert!(stats["collections"] >= 1, "{flags:?} stats: {stats:?}");
                assert!(stats["worker_scanned"] > 0, "{flags:?} stats: {stats:?}");
                assert_eq!(stats["objects"], stats["published"], "stats: {stats:?}");
            }
            let _ = fs::remove_file(&output_path);
        }
    }

    #[test]
    fn generational_worker_marks_concurrently_and_claims_copies_by_cas() {
        // Without stress the semispace fills far enough to start a mark
        // cycle (the worker traces while the mutator allocates), and the
        // relocation that follows copies through the CAS-claimed path: the
        // worker's copies land in its private to-space pages.
        let expected = evaluate("mark_eval", WORKER_PROGRAM);
        let source_path = write_source("mark", WORKER_PROGRAM);
        let output_path = temp_path("mark", "bin");
        let result = build(
            &["--gc=generational", "--gc-log"],
            &source_path,
            &output_path,
        );
        let _ = fs::remove_file(&source_path);
        assert!(
            result.status.success(),
            "stderr:\n{}",
            String::from_utf8_lossy(&result.stderr)
        );
        let mut lost = 0;
        for _ in 0..5 {
            let run = Command::new(&output_path).output().expect("binary runs");
            let stderr = String::from_utf8_lossy(&run.stderr);
            assert!(run.status.success(), "stderr:\n{stderr}");
            assert_eq!(String::from_utf8_lossy(&run.stdout), expected);
            let stats = gc_stats(&stderr);
            assert!(stats["mark_cycles"] >= 1, "stats: {stats:?}");
            assert!(stats["marked"] > 0, "stats: {stats:?}");
            // Relocation after a complete mark never meets an unmarked object.
            assert_eq!(stats["unmarked_copied"], 0, "stats: {stats:?}");
            assert!(stats["worker_copies"] > 0, "stats: {stats:?}");
            assert_eq!(stats["objects"], stats["published"], "stats: {stats:?}");
            lost += stats["worker_lost"] + stats["mutator_lost"];
        }
        // Lost claim races depend on scheduling; they are reported, not required.
        eprintln!("lost copy races over 5 runs: {lost}");
        let _ = fs::remove_file(&output_path);
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

#[path = "gc_generational/aarch64_macho.rs"]
mod aarch64_macho;

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[path = "gc_generational/remset_log.rs"]
mod remset_log;
