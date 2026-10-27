//! Advisory sizing is emitted on every direct target; the hard pool stays fixed.
use super::{
    gc_stats,
    metadata::{CHURN, Fixture},
};

#[test]
fn soft_heap_target_changes_initial_policy_without_reducing_the_hard_pool_on_each_target() {
    for target in [
        "x86_64-unknown-linux-gnu",
        "x86_64-pc-windows-msvc",
        "aarch64-apple-darwin",
    ] {
        let Some(f) = Fixture::new(target) else {
            continue;
        };
        let mut hard_metadata = None;
        for soft in [None, Some(16 << 20), Some(128 << 10)] {
            let output = f.run_soft("println(42)", soft);
            assert!(output.status.success(), "{target}: {output:?}");
            assert_eq!(output.stdout, b"42\n");
            let text = String::from_utf8_lossy(&output.stderr);
            let v = gc_stats(&text);
            if let Some(initial) = hard_metadata {
                assert_eq!(
                    v["metadata_initial_bytes"], initial,
                    "soft target must not shrink the hard copy/arena reserve: {target}: {text}"
                );
            } else {
                hard_metadata = Some(v["metadata_initial_bytes"]);
            }

            assert_eq!(v["heap_capacity_bytes"], 64 << 20, "{target}: {text}");
            assert_eq!(
                v["heap_soft_max_bytes"],
                soft.unwrap_or(64 << 20),
                "{target}: {text}"
            );
            assert_eq!(
                v["heap_min_committed_bytes"],
                soft.unwrap_or(16 << 20).min(16 << 20)
            );
            assert_eq!(
                v["young_requested_pages"],
                match soft {
                    None => 85,
                    Some(16777216) => 21,
                    _ => 4,
                },
                "{target}: {text}"
            );
            assert_eq!(v["young_capacity_pages"], v["young_requested_pages"]);
        }
        let output = f.run_soft(CHURN, Some(16 << 20));
        assert!(output.status.success(), "{target}: {output:?}");
        assert_eq!(output.stdout, b"5120\n32\n");
        let text = String::from_utf8_lossy(&output.stderr);
        let v = gc_stats(&text);
        assert!(v["collections"] > 0, "{target}: {text}");
        assert_eq!(v["heap_capacity_bytes"], 64 << 20);
        assert_eq!(v["heap_soft_max_bytes"], 16 << 20);
        assert!(v["young_requested_pages"] <= 21, "{target}: {text}");
        assert!(v["heap_peak_committed_bytes"] <= 64 << 20);
    }
}
