//! Managed sleep publishes native state and preserves live managed roots.
use super::*;

const SOURCE: &str = r#"
enum Box { case Box(v: Int) }
def grow(s: String, n: Int): String = if(n <= 0) s else grow(s + s, n - 1)
val keep = Box(84)
sleep(0)
sleep(1)
mutable millis = 0
mutable total = 0
while(millis < 3) {
  sleep(millis)
  total += length(grow("q", millis + 6))
  millis += 1
}
println(keep match { case Box(v) => v })
println(millis)
println(total)
"#;

struct Fixture {
    directory: PathBuf,
    image: PathBuf,
}
impl Fixture {
    fn new(target: &str, stress: bool, program: &str) -> Self {
        Self::with_flags(target, stress, program, &[])
    }
    fn with_flags(target: &str, stress: bool, program: &str, extra: &[&str]) -> Self {
        let directory = temp_path("native_sleep", "dir");
        fs::create_dir(&directory).unwrap();
        let source = directory.join("source.kl");
        let image = directory.join("image.bin");
        fs::write(&source, program).unwrap();
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
        flags.extend_from_slice(extra);
        let built = build(&flags, &source, &image);
        assert!(
            built.status.success(),
            "{}",
            String::from_utf8_lossy(&built.stderr)
        );
        Self { directory, image }
    }
    fn arm(&self, fault: &str) -> Output {
        Command::new(aarch64_macho::emulator_python().expect("sleep conformance requires unicorn"))
            .arg(Path::new(env!("CARGO_MANIFEST_DIR")).join("tools/run-macho-aarch64.py"))
            .arg(&self.image)
            .env("KLASSIC_MACHO_WORKER_FAULT", fault)
            .output()
            .unwrap()
    }
}

#[test]
fn aarch64_production_native_sleep_keeps_worker_maintenance_running() {
    let source = r#"
enum Tree { case Leaf(v: Int); case Node(l: Tree, r: Tree) }
def build(d: Int): Tree = if(d == 0) Leaf(1) else Node(build(d - 1), build(d - 1))
def sum(t: Tree): Int = t match { case Leaf(v) => v; case Node(l,r) => sum(l) + sum(r) }
val keep = build(6)
mutable i = 0
mutable total = 0
while(i < 80) { total += sum(build(7)); i += 1 }
sleep(5000)
println(total)
println(sum(keep))
"#;
    let f = Fixture::with_flags(
        "aarch64-apple-darwin",
        false,
        source,
        &["--gc-uncommit-delay=1500ms"],
    );
    let run = Command::new(aarch64_macho::emulator_python().unwrap())
        .arg(Path::new(env!("CARGO_MANIFEST_DIR")).join("tools/run-macho-aarch64.py"))
        .arg(&f.image)
        .env("KLASSIC_MACHO_REQUIRE_SLEEP_PROGRESS", "1")
        .output()
        .unwrap();
    let stderr = String::from_utf8_lossy(&run.stderr);
    assert!(run.status.success(), "{:?}\n{stderr}", run.status);
    assert_eq!(run.stdout, b"10240\n64\n");
    let stats = gc_stats(&stderr);
    assert_eq!(
        (stats["native_io_entries"] - stats["native_buffer_writes"]),
        1
    );
    assert_eq!(
        (stats["native_io_exits"] - stats["native_buffer_writes"]),
        1
    );
    assert!(stats["heap_uncommit_pages"] > 0, "{stderr}");
    assert!(stderr.contains("native_sleep_progress=1"), "{stderr}");
}
impl Drop for Fixture {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.directory);
    }
}
fn check(run: Output, expected: &str, stress: bool) -> String {
    let stderr = String::from_utf8_lossy(&run.stderr).into_owned();
    assert!(run.status.success(), "{:?}\n{stderr}", run.status);
    assert_eq!(String::from_utf8_lossy(&run.stdout), expected);
    let stats = gc_stats(&stderr);
    assert_eq!(
        (stats["native_io_entries"] - stats["native_buffer_writes"]),
        5,
        "{stderr}"
    );
    assert_eq!(
        (stats["native_io_exits"] - stats["native_buffer_writes"]),
        5,
        "{stderr}"
    );
    assert_eq!(stats["native_copyout_chunks"], 0);
    if stress {
        assert!(stats["collections"] > 0, "{stderr}");
    }
    stderr
}
#[test]
fn linux_production_native_sleep_restores_roots_after_zero_literal_and_runtime_waits() {
    let expected = evaluate("native_sleep_eval", SOURCE);
    for stress in [false, true] {
        let f = Fixture::new("x86_64-unknown-linux-gnu", stress, SOURCE);
        check(Command::new(&f.image).output().unwrap(), &expected, stress);
    }
}
#[test]
fn windows_production_native_sleep_checks_blocked_state_and_restores_roots() {
    let expected = evaluate("native_sleep_eval", SOURCE);
    for stress in [false, true] {
        let f = Fixture::new("x86_64-pc-windows-msvc", stress, SOURCE);
        let driver = windows_production::driver(&f.directory);
        let stderr = check(
            windows_production::run(&driver, &f.image, 0),
            &expected,
            stress,
        );
        assert_eq!(stderr.matches("win-native-sleep:").count(), 3, "{stderr}");
        if let Some(run) = windows_production::run_native_image(&f.directory, &f.image) {
            check(run, &expected, stress);
        }
    }
}
#[test]
fn aarch64_production_native_sleep_blocks_and_resumes_interrupted_waits() {
    let expected = evaluate("native_sleep_eval", SOURCE);
    for stress in [false, true] {
        let f = Fixture::new("aarch64-apple-darwin", stress, SOURCE);
        for fault in ["", "sleep-interrupt", "sleep-error"] {
            let stderr = check(f.arm(fault), &expected, stress);
            assert!(
                stderr.contains(if fault == "sleep-interrupt" {
                    "native_sleeps=6"
                } else {
                    "native_sleeps=5"
                }),
                "{stderr}"
            );
            assert!(
                stderr.contains(if fault == "sleep-error" {
                    "native_sleep_ns=0"
                } else {
                    "native_sleep_ns=4000000"
                }),
                "{stderr}"
            );
        }
    }
}
#[test]
fn production_native_sleep_rejects_negative_duration_before_native_entry() {
    for target in [
        "x86_64-unknown-linux-gnu",
        "x86_64-pc-windows-msvc",
        "aarch64-apple-darwin",
    ] {
        for duration in ["-1", "{ mutable n = -1; n }"] {
            let f = Fixture::new(target, false, &format!("sleep({duration})\nprintln(99)\n"));
            let run = if target.starts_with("aarch64") {
                f.arm("")
            } else if target.contains("windows") {
                let driver = windows_production::driver(&f.directory);
                windows_production::run(&driver, &f.image, 0)
            } else {
                Command::new(&f.image).output().unwrap()
            };
            assert_eq!(run.status.code(), Some(1), "{run:?}");
            assert!(run.stdout.is_empty(), "{run:?}");
            assert!(
                String::from_utf8_lossy(&run.stderr).contains("sleep expects a non-negative"),
                "{run:?}"
            );
        }
    }
}
