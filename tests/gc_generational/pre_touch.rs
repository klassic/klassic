//! Shared payload touch policy is emitted on all direct native targets.
use super::{
    gc_stats,
    metadata::{CHURN, Fixture},
};

#[test]
fn pre_touch_is_opt_in_and_covers_committed_payload_on_each_target() {
    for target in [
        "x86_64-unknown-linux-gnu",
        "x86_64-pc-windows-msvc",
        "aarch64-apple-darwin",
    ] {
        let Some(f) = Fixture::new(target) else {
            continue;
        };
        for enabled in [false, true] {
            for (source, expected) in [("println(42)", "42\n"), (CHURN, "5120\n32\n")] {
                let output = f.run_pre_touch(source, enabled);
                assert!(output.status.success(), "{target}: {output:?}");
                assert_eq!(output.stdout, expected.as_bytes());
                let text = String::from_utf8_lossy(&output.stderr);
                let v = gc_stats(&text);
                assert_eq!(
                    v["heap_pretouched_bytes"],
                    if enabled {
                        v["heap_commit_pages"] * (128 << 10)
                    } else {
                        0
                    },
                    "{target}: {text}"
                );
                assert_eq!(v["heap_min_committed_bytes"], 128 << 10);
                assert_eq!(v["heap_capacity_bytes"], 64 << 20);
                if source == CHURN {
                    assert!(v["heap_commit_pages"] > 1, "{target}: {text}");
                    assert!(v["collections"] > 0, "{target}: {text}");
                }
                eprintln!(
                    "pre-touch: target={target} enabled={enabled} commits={} touched={}",
                    v["heap_commit_pages"], v["heap_pretouched_bytes"]
                );
            }
        }
    }
}
