//! Configured hard limits drive actual heap mapping, moving and metadata.
use super::*;
const SOURCE: &str = r#"enum Tree { case Leaf(v: Int); case Node(l: Tree, r: Tree) }
def build(d: Int): Tree = if (d == 0) Leaf(1) else Node(build(d - 1), build(d - 1))
def sum(t: Tree): Int = t match { case Leaf(v) => v; case Node(l, r) => sum(l) + sum(r) }
val keep = build(8)
mutable i = 0
mutable total = 0
while (i < 1000) {
  val t = build(8)
  total += sum(t)
  i += 1
}
println(total)
println(sum(keep))
"#;
#[test]
fn configured_heap_limits_preserve_live_trees_across_real_collections() {
    for size in [16, 32, 256, 512] {
        let flag = format!("--gc-max-heap={size}MiB");
        let run = super::linux_x86_64::build_and_run(
            "heap_limit",
            &["--gc=generational", "--gc-log", &flag],
            SOURCE,
        );
        let stderr = String::from_utf8_lossy(&run.stderr);
        assert!(run.status.success(), "{size}: {stderr}");
        assert_eq!(run.stdout, b"256000\n256\n");
        let v = gc_stats(&stderr);
        assert_eq!(v["heap_capacity_bytes"], size << 20);
        assert_eq!(v["old_limit_bytes"], (size << 20) / 131072 / 3 * 131072);
        assert!(v["old_capacity_pages"] * 131072 <= v["old_limit_bytes"]);
        assert!(v["collections"] > 0);
        assert_eq!(v["unmarked_copied"], 0);
        assert_eq!(v["young_unmarked_copied"], 0);
    }
}

#[test]
fn larger_configured_heap_uses_more_than_the_previous_old_limit() {
    // Caller roots retain all intermediate strings until recursion unwinds:
    // over 121 MB of simultaneously reachable strings, each under 128 KiB.
    let source = "def churn(s: String, n: Int): Int = if (n <= 0) length(s) else churn(s + \"ab\", n - 1)\nprintln(churn(\"\", 11000))\n";
    let run = super::linux_x86_64::build_and_run(
        "heap_large_old",
        &["--gc=generational", "--gc-log", "--gc-max-heap=512MiB"],
        source,
    );
    let stderr = String::from_utf8_lossy(&run.stderr);
    assert!(run.status.success(), "{stderr}");
    assert_eq!(run.stdout, b"22000\n");
    let stats = gc_stats(&stderr);
    assert!(stats["old_charged_bytes"] > 64 << 20, "{stats:?}");
    assert!(
        stats["old_charged_bytes"] <= stats["old_limit_bytes"],
        "{stats:?}"
    );
}

#[test]
fn configured_minimum_commits_grow_on_demand_and_preserve_live_trees() {
    for minimum in ["128KiB", "4MiB", "16MiB"] {
        let flag = format!("--gc-min-heap={minimum}");
        let run = super::linux_x86_64::build_and_run(
            "heap_minimum",
            &[
                "--gc=generational",
                "--gc-log",
                "--gc-max-heap=16MiB",
                &flag,
            ],
            SOURCE,
        );
        let stderr = String::from_utf8_lossy(&run.stderr);
        assert!(run.status.success(), "{minimum}: {stderr}");
        assert_eq!(run.stdout, b"256000\n256\n");
        let stats = gc_stats(&stderr);
        let min = match minimum {
            "128KiB" => 128 << 10,
            "4MiB" => 4 << 20,
            _ => 16 << 20,
        };
        assert_eq!(stats["heap_min_committed_bytes"], min);
        assert_eq!(stats["heap_initial_committed_bytes"], min);
        assert!(stats["heap_committed_bytes"] >= min);
        assert!(stats["heap_peak_committed_bytes"] <= 16 << 20);
        assert_eq!(
            stats["heap_commit_pages"] * 131072,
            stats["heap_committed_bytes"]
        );
        assert_eq!(stats["heap_recommit_pages"], 0); // The default 300-second delay has not elapsed.
        assert_eq!(stats["heap_uncommit_pages"], 0);
    }
}

#[test]
fn configured_zero_uncommit_delay_releases_and_recommits_pages_without_losing_live_trees() {
    let run = super::linux_x86_64::build_and_run(
        "uncommit_zero",
        &[
            "--gc=generational",
            "--gc-log",
            "--gc-max-heap=16MiB",
            "--gc-min-heap=128KiB",
            "--gc-uncommit-delay=0",
        ],
        SOURCE,
    );
    let stderr = String::from_utf8_lossy(&run.stderr);
    assert!(run.status.success(), "{stderr}");
    assert_eq!(run.stdout, b"256000\n256\n");
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
    assert_eq!(v["unmarked_copied"], 0);
    assert_eq!(v["young_unmarked_copied"], 0);
}

#[test]
fn full_unsigned_uncommit_delay_is_logged_exactly() {
    let run = super::linux_x86_64::build_and_run(
        "delay_u64",
        &[
            "--gc=generational",
            "--gc-log",
            "--gc-min-heap=128KiB",
            "--gc-max-heap=16MiB",
            "--gc-uncommit-delay=18446744073709551615ns",
        ],
        "println(1)\n",
    );
    let stderr = String::from_utf8_lossy(&run.stderr);
    assert!(run.status.success(), "{stderr}");
    assert_eq!(run.stdout, b"1\n");
    assert!(
        stderr.contains("heap_uncommit_delay_ns=18446744073709551615"),
        "{stderr}"
    );
    assert_eq!(gc_stats(&stderr)["heap_uncommit_delay_ns"], u64::MAX);
}
