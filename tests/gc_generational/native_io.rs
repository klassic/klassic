//! Production string printers cross the shared native copy-out boundary.
use super::*;

const SOURCE: &str = r#"
def grow(s: String, n: Int): String = if (n <= 0) s else grow(s + s, n - 1)
mutable round = 0
while(round < 2) {
val keep = grow("abcdefgh", 10 + round) + "!"
println(keep)
printlnError(keep)
println([["head", "tail"], ["nested"]])
println(grow("", 2))
mutable i = 0
while(i < 60) { val garbage = grow("q", 5); i += 1 }
println(length(keep))
println(keep + "after")
round += 1
}
"#;

fn check(run: Output, expected: &str, modeled_arm: bool) {
    let stderr = String::from_utf8_lossy(&run.stderr);
    assert!(run.status.success(), "status {:?}\n{stderr}", run.status);
    assert_eq!(String::from_utf8_lossy(&run.stdout), expected);
    assert!(stderr.starts_with(&format!("{}!\n", "abcdefgh".repeat(1024))));
    let stats = gc_stats(&stderr);
    let chunks = stats["native_copyout_chunks"];
    assert!(chunks >= 24, "{stats:?}");
    assert_eq!(
        stats["native_io_entries"],
        chunks + stats["native_buffer_writes"]
    );
    assert_eq!(
        stats["native_io_exits"],
        chunks + stats["native_buffer_writes"]
    );
    if modeled_arm {
        assert!(stderr.contains(&format!("native-copyout-model: writes={chunks}\n")));
    }
}

fn image(target: &str, stress: bool) -> (PathBuf, PathBuf) {
    let source = write_source("native_io", SOURCE);
    let image = temp_path("native_io", "bin");
    let mut flags = vec![
        "--target",
        target,
        "--gc=generational",
        "--gc-log",
        "--gc-max-heap=16MiB",
        "--gc-min-heap=128KiB",
    ];
    if stress {
        flags.push("--gc-stress");
    }
    let result = build(&flags, &source, &image);
    assert!(
        result.status.success(),
        "{}",
        String::from_utf8_lossy(&result.stderr)
    );
    (source, image)
}

#[test]
fn linux_production_native_io_preserves_heap_strings() {
    let expected = evaluate("native_io_eval", SOURCE);
    for stress in [false, true] {
        let run = linux_x86_64::build_and_run(
            "native_io",
            &[
                "--gc=generational",
                "--gc-log",
                "--gc-max-heap=16MiB",
                "--gc-min-heap=128KiB",
                if stress {
                    "--gc-stress"
                } else {
                    "--gc-uncommit-delay=0"
                },
            ],
            SOURCE,
        );
        check(run, &expected, false);
    }
}

#[test]
fn windows_production_native_io_preserves_heap_strings() {
    let expected = evaluate("native_io_eval", SOURCE);
    let directory = temp_path("native_io_win", "dir");
    fs::create_dir(&directory).unwrap();
    let driver = windows_production::driver(&directory);
    for stress in [false, true] {
        let (source, image) = image("x86_64-pc-windows-msvc", stress);
        check(
            windows_production::run(&driver, &image, 0),
            &expected,
            false,
        );
        if let Some(run) = windows_production::run_native_image(&directory, &image) {
            check(run, &expected, false);
        }
        fs::remove_file(source).unwrap();
        fs::remove_file(image).unwrap();
    }
    fs::remove_dir_all(directory).unwrap();
}

#[test]
fn aarch64_production_native_io_checks_storage_and_owner_at_syscall() {
    let expected = evaluate("native_io_eval", SOURCE);
    let python =
        aarch64_macho::emulator_python().expect("native boundary conformance requires unicorn");
    for stress in [false, true] {
        let (source, image) = image("aarch64-apple-darwin", stress);
        let run = Command::new(&python)
            .arg(Path::new(env!("CARGO_MANIFEST_DIR")).join("tools/run-macho-aarch64.py"))
            .arg(&image)
            .env("KLASSIC_MACHO_REQUIRE_NATIVE_COPYOUT", "1")
            .output()
            .unwrap();
        check(run, &expected, true);
        fs::remove_file(source).unwrap();
        fs::remove_file(image).unwrap();
    }
}
