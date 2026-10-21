//! `--gc=generational` on the direct AArch64 Mach-O backend, executed
//! through `tools/run-macho-aarch64.py` (a unicorn emulator) so the Linux
//! development host can diff the binary against the evaluator. When no
//! Python with `unicorn` is found the tests only check that the build
//! succeeds and say so on stderr; the arm64 CI runner is the authority.
use super::*;

/// A Python interpreter that can import `unicorn`: `$KLASSIC_UNICORN_PYTHON`,
/// then `python3`, then any pyenv-installed `python3`.
fn emulator_python() -> Option<PathBuf> {
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
fn build_and_emulate(tag: &str, flags: &[&str], source: &str) -> Option<Output> {
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
    let run = Command::new(python)
        .arg(script)
        .arg(&output_path)
        .output()
        .expect("emulator should run");
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
    assert!(stats["objects"] > 0, "stats: {stats:?}");
    assert_eq!(stats["objects"], stats["published"], "stats: {stats:?}");
}

#[test]
fn aarch64_generational_survives_stress_collections() {
    let expected = evaluate("a64s_eval", PROGRAM);
    let flags = ["--gc=generational", "--gc-log", "--gc-stress"];
    let Some(run) = build_and_emulate("a64s", &flags, PROGRAM) else {
        return;
    };
    let stderr = String::from_utf8_lossy(&run.stderr);
    assert!(run.status.success(), "stderr:\n{stderr}");
    assert_eq!(String::from_utf8_lossy(&run.stdout), expected);
    // A collection before every allocation, and the long-lived tree is
    // promoted to Old.
    let stats = gc_stats(&stderr);
    assert!(stats["collections"] > 1000, "stats: {stats:?}");
    assert!(stats["promoted_bytes"] > 0, "stats: {stats:?}");
}
