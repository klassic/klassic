//! G07 through ordinary programs: the double-buffered remembered sets are
//! flipped once per Young collection, entries are consumed and re-registered,
//! and the set does not grow. The directed cases (registration, dedupe, drops
//! by referent age / liveness, full-collection discard, racing stores) are
//! the machine-code harness tests in `klassic-native`
//! (`gc_barrier_harness_tests::remset`).
use super::linux_x86_64::build_and_run;
use super::*;

const PROGRAM: &str = "enum Tree { case Leaf(v: Int); case Branch(l: Tree, r: Tree) }\n\
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

#[test]
fn remembered_sets_flip_once_per_young_collection() {
    let expected = evaluate("remset_eval", PROGRAM);
    let run = build_and_run("remset", &["--gc=generational", "--gc-log"], PROGRAM);
    let stderr = String::from_utf8_lossy(&run.stderr);
    assert!(run.status.success(), "stderr:\n{stderr}");
    assert_eq!(String::from_utf8_lossy(&run.stdout), expected);
    let stats = gc_stats(&stderr);
    assert!(stats["collections"] >= 2, "stats: {stats:?}");
    assert_eq!(
        stats["remset_flips"],
        stats["collections"] - stats["full_collections"],
        "one flip per Young collection: {stats:?}"
    );
    assert!(stats["remset_kept"] <= stats["remset_flipped"], "{stats:?}");
    assert!(stats["remset_len"] < 64, "{stats:?}");
}

#[test]
fn remembered_sets_stay_bounded_under_stress_and_full_collections_discard_them() {
    let expected = evaluate("remset_stress_eval", PROGRAM);
    let run = build_and_run(
        "remset_stress",
        &["--gc=generational", "--gc-log", "--gc-stress"],
        PROGRAM,
    );
    let stderr = String::from_utf8_lossy(&run.stderr);
    assert!(run.status.success(), "stderr:\n{stderr}");
    assert_eq!(String::from_utf8_lossy(&run.stdout), expected);
    let stats = gc_stats(&stderr);
    assert!(stats["full_collections"] >= 1, "stats: {stats:?}");
    assert_eq!(
        stats["remset_flips"] + stats["full_collections"],
        stats["collections"],
        "{stats:?}"
    );
    // Entries were consumed and re-registered, never accumulated.
    assert!(stats["remset_flipped"] > 0, "{stats:?}");
    assert!(stats["remset_kept"] > 0, "{stats:?}");
    assert!(stats["remset_kept"] <= stats["remset_flipped"], "{stats:?}");
    assert!(stats["remset_len"] < 64, "{stats:?}");
}
