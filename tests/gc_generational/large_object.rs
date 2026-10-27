//! Large language strings on all direct backends, with live roots and dead churn.
use super::{gc_stats, metadata::Fixture};
const PROGRAM: &str = r#"
def grow(s: String,n: Int): String = if(n<=0) s else grow(s+s,n-1)
def exercise(depth: Int): Unit = {
val keep=grow("abcdefgh",depth)
mutable i=0
mutable total=0
while(i<24) {
    val s=grow("zyxwvuts",depth+1)
    total+=length(s)
    i+=1
}
println(total)
println(length(keep))
println(substring(keep,0,8))
println(substring(keep,length(keep)-8,length(keep)))

}
mutable k=0
while(k<1) { exercise(k+16); k+=1 }
"#;
#[test]
fn large_strings_survive_old_collections_and_reclaim_dead_spans_on_all_direct_targets() {
    for target in [
        "x86_64-unknown-linux-gnu",
        "x86_64-pc-windows-msvc",
        "aarch64-apple-darwin",
    ] {
        let Some(f) = Fixture::new(target) else {
            continue;
        };
        let r = f.run_soft(PROGRAM, Some(128 << 10));
        let text = String::from_utf8_lossy(&r.stderr);
        assert!(r.status.success(), "{target}: {r:?}");
        assert_eq!(
            r.stdout, b"25165824\n524288\nabcdefgh\nabcdefgh\n",
            "{target}: {text}"
        );
        let v = gc_stats(&text);
        assert!(v["large_allocations"] >= 99, "{target}: {text}");
        assert!(v["large_allocated_bytes"] > (24 << 20));
        assert!(v["full_collections"] > 0, "{target}: {text}");
        assert!(v["large_full_cycles"] > 0, "{target}: {text}");
        assert!(v["large_reclaimed_pages"] > 0, "{target}: {text}");
        assert!(v["large_live_pages"] > 0, "{target}: {text}");
        assert_eq!(
            v["large_scanned_fields"], 0,
            "raw strings: {target}: {text}"
        );
        assert_eq!(v["unmarked_copied"], 0);
        assert_eq!(v["young_unmarked_copied"], 0);
        assert_eq!(
            v["heap_committed_bytes"],
            (v["heap_commit_pages"] - v["heap_uncommit_pages"]) * 131072
        );
        eprintln!(
            "large-source: target={target} allocations={} full={} retries={} planned={:?}",
            v["large_allocations"],
            v["full_collections"],
            v["large_allocation_retries"],
            v.get("old_planned_cycles")
        );
    }
}
