//! The ordinary native build reaches Linux THP policy and target diagnostics.
use super::*;
#[test]
fn large_pages_preserve_linux_values_and_report_distinct_advice_and_collapse() {
    let churn = super::metadata::CHURN.replace("i < 160", "i < 1600");
    for (mode, number) in [("off", 0), ("try", 1), ("required", 2)] {
        for touch in [false, true] {
            let flag = format!("--gc-large-pages={mode}");
            let mut flags = vec![
                "--gc=generational",
                "--gc-log",
                "--gc-min-heap=4MiB",
                "--gc-soft-max-heap=4MiB",
                "--gc-max-heap=64MiB",
                "--gc-uncommit-delay=0",
                &flag,
            ];
            if touch {
                flags.push("--gc-pre-touch");
            }
            let output = super::linux_x86_64::build_and_run("large_pages", &flags, &churn);
            let text = String::from_utf8_lossy(&output.stderr);
            if mode == "required" && !output.status.success() {
                assert_eq!(output.status.code(), Some(1));
                assert!(text.contains("required large-page commit failed"), "{text}");
                eprintln!("large-pages source: THP unavailable, controlled required failure");
                continue;
            }
            assert!(output.status.success(), "{mode}/{touch}: {text}");
            assert_eq!(output.stdout, b"51200\n32\n");
            let v = gc_stats(&text);
            assert_eq!(v["heap_large_page_mode"], number);
            assert!(v["collections"] > 1, "{text}");
            assert_eq!(
                v["heap_pretouched_bytes"],
                if touch {
                    v["heap_commit_pages"] * 131072
                } else {
                    0
                }
            );
            assert!(v["heap_committed_bytes"] >= 4 << 20);
            if mode == "required" {
                assert_eq!(v["heap_committed_bytes"] % (2 << 20), 0);
                assert_eq!(
                    v["heap_large_page_collapsed_bytes"],
                    v["heap_commit_pages"] * 131072
                );
                assert_eq!(v["heap_uncommit_pages"] % 16, 0);
                assert_eq!(v["heap_large_page_collapse_failures"], 0);
            } else {
                assert_eq!(v["heap_large_page_collapsed_bytes"], 0);
            }
            if mode == "off" {
                assert_eq!(v["heap_large_page_advised_bytes"], 0);
                assert_eq!(v["heap_large_page_advice_failures"], 0);
                assert_eq!(v["heap_reservation_bytes"], (64 << 20) + 131072);
            } else {
                assert!(
                    v["heap_large_page_advised_bytes"] == 64 << 20
                        || v["heap_large_page_advice_failures"] > 0
                );
                assert_eq!(v["heap_reservation_bytes"], (64 << 20) + 131072 + (2 << 20));
            }
            eprintln!(
                "large-pages source: mode={mode} touch={touch} collections={} commits={} uncommits={} collapsed={}",
                v["collections"],
                v["heap_commit_pages"],
                v["heap_uncommit_pages"],
                v["heap_large_page_collapsed_bytes"]
            );
        }
    }
}
#[test]
fn large_pages_reject_unimplemented_targets_and_off_remains_supported() {
    let dir = temp_path("large_pages_targets", "dir");
    fs::create_dir(&dir).unwrap();
    let source = dir.join("source.kl");
    let image = dir.join("image.bin");
    fs::write(&source, "println(42)").unwrap();
    for target in ["x86_64-pc-windows-msvc", "aarch64-apple-darwin"] {
        for mode in ["--gc-large-pages=try", "--gc-large-pages=required"] {
            let output = build(
                &["--target", target, "--gc=generational", mode],
                &source,
                &image,
            );
            assert!(!output.status.success());
            assert!(
                String::from_utf8_lossy(&output.stderr)
                    .contains("not supported on this native target"),
                "{output:?}"
            );
        }
        let output = build(
            &[
                "--target",
                target,
                "--gc=generational",
                "--gc-large-pages=off",
            ],
            &source,
            &image,
        );
        assert!(output.status.success(), "{output:?}");
    }
    fs::remove_dir_all(dir).unwrap();
}
