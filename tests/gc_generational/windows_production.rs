//! Execute complete production PEs. POSIX uses explicit Win64 API models;
//! an x86-64 Windows host runs the same code against native kernel APIs.
use super::*;
const SOURCE: &str = r#"
enum Tree { case Leaf(v: Int); case Node(l: Tree, r: Tree) }
enum Box { case Box(v: Int) }
def build(d: Int): Tree = if (d == 0) Leaf(1) else Node(build(d - 1), build(d - 1))
def sum(t: Tree): Int = t match { case Leaf(v) => v; case Node(l, r) => sum(l) + sum(r) }
val keep = build(6)
val opaque = Box(8595182864)
mutable i = 0
mutable total = 0
while (i < 1200) { val t = build(7); total += sum(t); i += 1 }
println(total)
println(sum(keep))
println(opaque match { case Box(v) => v + 1 })
"#;
struct Scratch(PathBuf);
impl Drop for Scratch {
    fn drop(&mut self) {
        if std::thread::panicking() {
            eprintln!("production PE failure artifacts: {}", self.0.display());
            return;
        }
        let _ = fs::remove_dir_all(&self.0);
    }
}
pub(super) fn driver(d: &Path) -> PathBuf {
    let cc = std::env::var_os("KLASSIC_CLANG")
        .into_iter()
        .chain(["clang", "gcc", "cc"].map(std::ffi::OsString::from))
        .find(|p| {
            Command::new(p)
                .arg("--version")
                .output()
                .is_ok_and(|r| r.status.success())
        })
        .expect("production PE conformance requires a C compiler");
    let exe = d.join(if cfg!(windows) {
        "driver.exe"
    } else {
        "driver"
    });
    let source = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("crates/klassic-native/tests/windows_production_harness.c");
    let mut c = Command::new(cc);
    c.args(["-std=c11", "-O2", "-Wall", "-Wextra", "-Werror"]);
    if cfg!(windows) {
        c.arg("-D_CRT_SECURE_NO_WARNINGS");
    } else {
        c.arg("-pthread");
    }
    let r = c.arg(source).arg("-o").arg(&exe).output().unwrap();
    assert!(r.status.success(), "{}", String::from_utf8_lossy(&r.stderr));
    exe
}
pub(super) fn run(driver: &Path, image: &Path, mode: u32) -> Output {
    use std::process::Stdio;
    let child = Command::new(driver)
        .arg(image)
        .arg(mode.to_string())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();
    capture(
        child,
        std::time::Duration::from_secs(60),
        &format!("production PE mode {mode} timed out"),
    )
}

/// Drain both pipes while the child runs: waiting for exit first can deadlock
/// when either OS pipe fills, even though the image itself is making progress.
fn capture(mut child: std::process::Child, timeout: std::time::Duration, message: &str) -> Output {
    fn drain<T: std::io::Read + Send + 'static>(mut pipe: T) -> std::thread::JoinHandle<Vec<u8>> {
        std::thread::spawn(move || {
            let mut bytes = Vec::new();
            pipe.read_to_end(&mut bytes).expect("read child output");
            bytes
        })
    }
    let stdout = drain(child.stdout.take().unwrap());
    let stderr = drain(child.stderr.take().unwrap());
    let start = std::time::Instant::now();
    while child.try_wait().unwrap().is_none() {
        if start.elapsed() > timeout {
            let _ = child.kill();
            let _ = child.wait();
            panic!("{message}");
        }
        std::thread::sleep(std::time::Duration::from_millis(20));
    }
    Output {
        status: child.wait().unwrap(),
        stdout: stdout.join().expect("stdout reader"),
        stderr: stderr.join().expect("stderr reader"),
    }
}

#[cfg(unix)]
#[test]
fn production_pe_capture_drains_both_pipes_before_waiting_for_exit() {
    use std::os::unix::fs::PermissionsExt;
    let directory = Scratch(temp_path("pe_capture", "dir"));
    fs::create_dir(&directory.0).unwrap();
    let script = directory.0.join("large-output");
    fs::write(&script, b"#!/bin/sh\nn=0\nwhile [ \"$n\" -lt 2048 ]; do\n printf '%064d\\n' 0\n printf '%064d\\n' 1 >&2\n n=$((n+1))\ndone\n").unwrap();
    fs::set_permissions(&script, fs::Permissions::from_mode(0o755)).unwrap();
    let result = run(&script, Path::new("unused"), 0);
    assert!(result.status.success());
    assert_eq!(
        result.stdout,
        format!("{:064}\n", 0).repeat(2048).as_bytes()
    );
    assert_eq!(
        result.stderr,
        format!("{:064}\n", 1).repeat(2048).as_bytes()
    );
}
fn os_counter(stderr: &str, name: &str) -> u64 {
    let line = stderr
        .lines()
        .find(|s| s.starts_with("win-production:"))
        .expect("OS model counters");
    line.split_whitespace()
        .find_map(|s| {
            s.strip_prefix(&format!("{name}="))
                .map(|v| v.parse().unwrap())
        })
        .unwrap()
}
#[test]
fn windows_production_worker_runs_full_pe_and_falls_back_after_startup_failures() {
    let d = Scratch(temp_path("windows_production", "dir"));
    fs::create_dir(&d.0).unwrap();
    let c = driver(&d.0);
    let source = d.0.join("worker.kl");
    let image = d.0.join("worker.exe");
    fs::write(&source, SOURCE).unwrap();
    let r = build(
        &[
            "--target",
            "x86_64-pc-windows-msvc",
            "--gc=generational",
            "--gc-log",
            "--gc-max-heap=16MiB",
            "--gc-min-heap=128KiB",
            "--gc-uncommit-delay=0",
        ],
        &source,
        &image,
    );
    assert!(r.status.success(), "{}", String::from_utf8_lossy(&r.stderr));
    for mode in [0, 11, 1, 2] {
        let r = run(&c, &image, mode);
        let stderr = String::from_utf8_lossy(&r.stderr);
        assert!(r.status.success(), "mode {mode}: {:?}\n{stderr}", r.status);
        assert_eq!(r.stdout, b"153600\n64\n8595182865\n", "{stderr}");
        let v = gc_stats(&stderr);
        assert!(v["collections"] > 0, "{stderr}");
        assert_eq!(v["objects"], v["published"]);
        assert_eq!(v["unmarked_copied"], 0);
        assert_eq!(v["young_unmarked_copied"], 0);
        assert_eq!(
            v["heap_committed_bytes"],
            (v["heap_commit_pages"] - v["heap_uncommit_pages"]) * 131072
        );
        assert!(v["heap_uncommit_pages"] > 0, "{stderr}");
        assert!(v["heap_recommit_pages"] > 0, "{stderr}");
        assert!(os_counter(&stderr, "clocks") > 0, "{stderr}");
        if mode == 0 || mode == 11 {
            assert!(os_counter(&stderr, "notified") > 0, "{stderr}");
            assert!(os_counter(&stderr, "signals") > 0, "{stderr}");
            assert!(v["young_worker_units"] > 0, "{stderr}");
            assert_eq!(os_counter(&stderr, "joined"), 1, "{stderr}");
            assert_eq!(os_counter(&stderr, "thread_closes"), 1, "{stderr}");
            assert_eq!(os_counter(&stderr, "event_closes"), 1, "{stderr}");
        } else {
            assert_eq!(v["young_worker_units"], 0, "{stderr}");
        }
        assert!(stderr.contains("win-production:"), "{stderr}");
    }
    for mode in [3, 4, 5, 6, 13, 14, 15] {
        let r = run(&c, &image, mode);
        let stderr = String::from_utf8_lossy(&r.stderr);
        assert_eq!(
            r.status.code(),
            Some(1),
            "mode {mode}: {:?}\n{stderr}",
            r.status
        );
        assert!(
            stderr.contains(if mode == 6 {
                "GC monotonic clock failed"
            } else {
                "GC worker notification failed"
            }),
            "{stderr}"
        );
    }
}

#[test]
fn windows_production_worker_uncommits_while_owner_is_blocked_and_preserves_live_values() {
    let d = Scratch(temp_path("windows_idle", "dir"));
    fs::create_dir(&d.0).unwrap();
    let c = driver(&d.0);
    let source = d.0.join("idle.kl");
    let image = d.0.join("idle.exe");
    fs::write(
        &source,
        SOURCE.replace(
            "println(sum(keep))",
            // A native worker can finish tracing but cannot adopt its owner's
            // phase to retire sources. Establish completed full cycles before
            // blocking, as the native-read fixture does; a trailing concurrent
            // Young cycle alone does not guarantee any decommittable pages.
            "def grow(s: String, n: Int): String = if(n <= 0) s else grow(s+s,n-1)\nmutable large = 0\nmutable round = 0\nwhile(round < 3) { large += length(grow(\"x\", i - 1179)); round += 1 }\nprintln(large)\nprintln(getEnv(\"KLASSIC_GC_IDLE\"))\nprintln(sum(keep))",
        ),
    )
    .unwrap();
    let r = build(
        &[
            "--target",
            "x86_64-pc-windows-msvc",
            "--gc=generational",
            "--gc-log",
            "--gc-max-heap=16MiB",
            "--gc-min-heap=128KiB",
            "--gc-uncommit-delay=1500ms",
        ],
        &source,
        &image,
    );
    assert!(r.status.success(), "{}", String::from_utf8_lossy(&r.stderr));
    let r = run(&c, &image, 8);
    let stderr = String::from_utf8_lossy(&r.stderr);
    for line in stderr
        .lines()
        .filter(|l| l.starts_with("win-production-idle:"))
    {
        eprintln!("{line}");
    }
    assert!(r.status.success(), "{:?}\n{stderr}", r.status);
    assert_eq!(
        r.stdout, b"153600\n6291456\nresumed\n64\n8595182865\n",
        "{stderr}"
    );
    assert!(stderr.contains("win-production-idle:"), "{stderr}");
    let v = gc_stats(&stderr);
    assert_eq!(v["heap_uncommit_delay_ns"], 1_500_000_000);
    assert!(v["full_collections"] > 0, "{stderr}");
    assert!(v["large_full_cycles"] > 0, "{stderr}");
    assert!(v["heap_uncommit_pages"] > 0, "{stderr}");
    assert!(os_counter(&stderr, "timeouts") > 0, "{stderr}");
    let missing = run(&c, &image, 9);
    let error = String::from_utf8_lossy(&missing.stderr);
    assert_eq!(missing.status.code(), Some(1), "{error}");
    assert!(
        error.contains("Environment#get missing environment variable"),
        "{error}"
    );
    assert_eq!(
        v["heap_committed_bytes"],
        (v["heap_commit_pages"] - v["heap_uncommit_pages"]) * 131072
    );
}

#[test]
fn windows_production_worker_preserves_opaque_scalars_under_stress_and_fallback() {
    let d = Scratch(temp_path("windows_stress", "dir"));
    fs::create_dir(&d.0).unwrap();
    let c = driver(&d.0);
    let source = d.0.join("stress.kl");
    let image = d.0.join("stress.exe");
    fs::write(&source, OPAQUE_SCALAR_PROGRAM.replace("i < 40", "i < 512")).unwrap();
    let r = build(
        &[
            "--target",
            "x86_64-pc-windows-msvc",
            "--gc=generational",
            "--gc-log",
            "--gc-stress",
            "--gc-max-heap=16MiB",
            "--gc-min-heap=128KiB",
            "--gc-uncommit-delay=0",
        ],
        &source,
        &image,
    );
    assert!(r.status.success(), "{}", String::from_utf8_lossy(&r.stderr));
    for mode in [0, 1, 2] {
        let r = run(&c, &image, mode);
        let stderr = String::from_utf8_lossy(&r.stderr);
        assert!(r.status.success(), "mode {mode}: {:?}\n{stderr}", r.status);
        assert_eq!(r.stdout, b"true\n8595182865\n", "{stderr}");
        let v = gc_stats(&stderr);
        assert!(v["collections"] > 100, "{stderr}");
        assert_eq!(v["unmarked_copied"], 0);
        assert_eq!(v["young_unmarked_copied"], 0);
        assert_eq!(
            v["heap_committed_bytes"],
            (v["heap_commit_pages"] - v["heap_uncommit_pages"]) * 131072
        );
    }
}

#[path = "windows_native.rs"]
mod native;

#[test]
fn windows_production_explicit_exit_joins_worker_and_preserves_computed_status() {
    let d = Scratch(temp_path("windows_exit", "dir"));
    fs::create_dir(&d.0).unwrap();
    let c = driver(&d.0);
    for (mode, code, tail) in [
        (18, 7, "exit(7)"),
        (19, 23, "mutable status = 17\nexit(status + 6)"),
    ] {
        let source = d.0.join(format!("exit-{mode}.kl"));
        let image = d.0.join(format!("exit-{mode}.exe"));
        fs::write(&source, format!("{SOURCE}\n{tail}\nprintln(99)\n")).unwrap();
        let output = build(
            &["--target", "x86_64-pc-windows-msvc", "--gc=generational"],
            &source,
            &image,
        );
        assert!(output.status.success(), "{output:?}");
        let output = run(&c, &image, mode);
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert_eq!(output.status.code(), Some(code), "{stderr}");
        assert_eq!(output.stdout, b"153600\n64\n8595182865\n", "{stderr}");
        for name in ["joined", "thread_closes", "event_closes"] {
            assert_eq!(os_counter(&stderr, name), 1, "{stderr}");
        }
    }
}

/// Actual Windows loader/kernel when the host or WSL interop provides it.
pub(super) fn run_native_image(directory: &Path, image: &Path) -> Option<Output> {
    if native::native_host() {
        Some(native::run_image(directory, image, false, false))
    } else {
        eprintln!("native Windows unavailable; metadata case used the API model only");
        None
    }
}

pub(super) fn native_host() -> bool {
    native::native_host()
}
pub(super) fn run_native_image_with_input(directory: &Path, image: &Path, input: &Path) -> Output {
    native::run_image_with_input(directory, image, false, false, Some(input))
}
