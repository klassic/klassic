//! Emitted workers make progress while their sole owner is inside a read API.
//! Windows and Darwin API models; no hardware read-latency claim.
use super::*;
use std::process::Stdio;
use std::time::{Duration, Instant};
const PREFIX: &str = r#"
enum Tree { case Leaf(v: Int); case Node(l: Tree, r: Tree) }
def build(d: Int): Tree = if(d == 0) Leaf(1) else Node(build(d - 1), build(d - 1))
def sum(t: Tree): Int = t match { case Leaf(v) => v; case Node(l,r) => sum(l) + sum(r) }
def load(p: String): String = FileInput#all(p)
val keep = build(6)
mutable i = 0
mutable total = 0
while(i < 80) { total += sum(build(7)); i += 1 }
"#;
struct Fixture {
    directory: PathBuf,
    image: PathBuf,
    stdin: bool,
    full_preflight: bool,
}
impl Fixture {
    fn new(target: &str, stdin: bool) -> Self {
        let directory = temp_path("blocked_read", "dir");
        fs::create_dir(&directory).unwrap();
        fs::write(directory.join("input"), b"alpha\nbeta\n").unwrap();
        let read = if stdin {
            "StandardInput#all()".into()
        } else {
            format!("load({:?})", directory.join("input").display().to_string())
        };
        let source = directory.join("source.kl");
        let image = directory.join("image.bin");
        // Native workers can finish tracing/copying, but cannot adopt the
        // owner's phase to retire sources. Force a synchronous full cycle
        // before holding the real-thread backends inside their read API.
        // The dynamic depth prevents folding this allocation at compile time.
        let full_preflight = target.starts_with("x86_64");
        let (preflight, report) = if full_preflight {
            (
                "def grow(s: String, n: Int): String = if(n <= 0) s else grow(s+s,n-1)\nmutable large = 0\nmutable round = 0\nwhile(round < 3) { large += length(grow(\"x\", i - 59)); round += 1 }\n",
                "println(large)\n",
            )
        } else {
            ("", "")
        };
        fs::write(&source,format!("{PREFIX}\n{preflight}val text = {read}\n{report}println(length(text))\nprintln(total)\nprintln(sum(keep))\n")).unwrap();
        let r = build(
            &[
                "--target",
                target,
                "--gc=generational",
                "--gc-log",
                "--gc-max-heap=16MiB",
                "--gc-min-heap=128KiB",
                "--gc-uncommit-delay=1500ms",
            ],
            &source,
            &image,
        );
        assert!(r.status.success(), "{r:?}");
        Self {
            directory,
            image,
            stdin,
            full_preflight,
        }
    }
    fn arm(&self, fault: &str) -> Output {
        let mut c = Command::new(
            aarch64_macho::emulator_python().expect("blocked-read conformance requires unicorn"),
        );
        c.arg(Path::new(env!("CARGO_MANIFEST_DIR")).join("tools/run-macho-aarch64.py"))
            .arg(&self.image)
            .stdin(fs::File::open(self.directory.join("input")).unwrap())
            .env("KLASSIC_MACHO_REQUIRE_NATIVE_FILE_IO", "1")
            .env("KLASSIC_MACHO_REQUIRE_READ_PROGRESS", "1")
            .env("KLASSIC_MACHO_WORKER_FAULT", fault)
            .env("KLASSIC_MACHO_WORKER_SECONDS", "180");
        // The model bounds the entire emitted program, including allocation
        // preflight before the read. Let its 180-second diagnostic deadline
        // fire before the outer process supervisor's cleanup allowance.
        bounded_with_timeout(c, Duration::from_secs(190))
    }
    fn windows(&self, mode: u32) -> Output {
        let mut c = Command::new(windows_production::driver(&self.directory));
        c.arg(&self.image)
            .arg(mode.to_string())
            .stdin(fs::File::open(self.directory.join("input")).unwrap());
        bounded(c)
    }
    fn check(&self, r: &Output) -> String {
        let stderr = String::from_utf8_lossy(&r.stderr).into_owned();
        assert!(r.status.success(), "{:?}\n{stderr}", r.status);
        assert_eq!(r.stdout, self.expected(), "{stderr}");
        let stats = gc_stats(&stderr);
        if self.full_preflight {
            assert!(stats["full_collections"] > 0, "{stderr}");
            assert!(stats["large_full_cycles"] > 0, "{stderr}");
        }
        assert!(stats["heap_uncommit_pages"] > 0, "{stderr}");
        let io_entries = if self.stdin { 2 } else { 3 };
        // These counters cover the shared Native boundary, including actual
        // metadata map/retire work. Dynamic queue/lease growth may add paired
        // transitions between the observed read and shutdown. The read probe
        // separately proves its exact Native state, phase and worker progress.
        assert_eq!(
            stats["native_io_entries"], stats["native_io_exits"],
            "{stderr}"
        );
        assert!(
            stats["native_io_entries"] >= stats["native_buffer_writes"] + io_entries,
            "missing Native I/O transitions: {stderr}"
        );
        stderr
    }
    fn expected(&self) -> &'static [u8] {
        if self.full_preflight {
            b"6291456\n11\n10240\n64\n"
        } else {
            b"11\n10240\n64\n"
        }
    }
}
impl Drop for Fixture {
    fn drop(&mut self) {
        if std::thread::panicking() {
            eprintln!(
                "blocked-read failure artifacts: {}",
                self.directory.display()
            );
            return;
        }
        let _ = fs::remove_dir_all(&self.directory);
    }
}
fn bounded(c: Command) -> Output {
    bounded_with_timeout(c, Duration::from_secs(60))
}
fn bounded_with_timeout(mut c: Command, timeout: Duration) -> Output {
    use std::io::Read;
    let mut child = c
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();
    // Drain both pipes concurrently. Waiting for exit before reading can fill
    // either pipe and prevent an otherwise completed probe from exiting.
    let mut stdout = child.stdout.take().unwrap();
    let mut stderr = child.stderr.take().unwrap();
    let out = std::thread::spawn(move || {
        let mut bytes = Vec::new();
        stdout.read_to_end(&mut bytes).unwrap();
        bytes
    });
    let err = std::thread::spawn(move || {
        let mut bytes = Vec::new();
        stderr.read_to_end(&mut bytes).unwrap();
        bytes
    });
    let start = Instant::now();
    let status = loop {
        if let Some(status) = child.try_wait().unwrap() {
            break status;
        }
        if start.elapsed() > timeout {
            let _ = child.kill();
            let _ = child.wait();
            let stdout = out.join().unwrap();
            let stderr = err.join().unwrap();
            panic!(
                "blocked-read probe exceeded bounded API-model waits\nstdout:\n{}\nstderr:\n{}",
                String::from_utf8_lossy(&stdout),
                String::from_utf8_lossy(&stderr)
            );
        }
        std::thread::sleep(Duration::from_millis(20));
    };
    Output {
        status,
        stdout: out.join().unwrap(),
        stderr: err.join().unwrap(),
    }
}

#[cfg(unix)]
#[test]
fn blocked_read_supervisor_drains_both_pipes_before_child_exit() {
    let mut c = Command::new("sh");
    c.args([
        "-c",
        "head -c 131072 /dev/zero; head -c 131072 /dev/zero >&2",
    ]);
    let result = bounded(c);
    assert!(result.status.success(), "{:?}", result.status);
    assert_eq!(result.stdout.len(), 131072);
    assert_eq!(result.stderr.len(), 131072);
}
#[test]
fn windows_worker_uncommits_during_file_and_stdin_read_without_owner_notification() {
    for stdin in [false, true] {
        let f = Fixture::new("x86_64-pc-windows-msvc", stdin);
        let stderr = f.check(&f.windows(49));
        assert!(stderr.contains("win-native-read: progress=1"), "{stderr}");
        assert!(stderr.contains("no-owner-signal=1"), "{stderr}");
    }
}
#[test]
fn arm_worker_uncommits_during_file_and_stdin_read_without_owner_notification() {
    for stdin in [false, true] {
        let f = Fixture::new("aarch64-apple-darwin", stdin);
        let stderr = f.check(&f.arm(""));
        assert!(
            stderr.contains("native_read_blocks=1") && stderr.contains("native_read_progress=1"),
            "{stderr}"
        );
        assert!(
            stderr.contains("native-read-progress:") && stderr.contains("no-owner-signal=1"),
            "{stderr}"
        );
    }
}
#[test]
fn blocked_read_progress_probes_fail_when_worker_startup_is_disabled() {
    let f = Fixture::new("x86_64-pc-windows-msvc", false);
    let r = f.windows(50);
    let stderr = String::from_utf8_lossy(&r.stderr);
    assert!(!r.status.success(), "{r:?}");
    assert!(stderr.contains("win-native-read: progress=0"), "{stderr}");
    assert!(!r.stdout.windows(5).any(|w| w == b"10240"), "{stderr}");
    let f = Fixture::new("aarch64-apple-darwin", false);
    let r = f.arm("thread-create");
    let stderr = String::from_utf8_lossy(&r.stderr);
    assert!(!r.status.success(), "{r:?}");
    assert!(
        stderr.contains("native read made no worker progress"),
        "{stderr}"
    );
}

#[path = "blocked_read_linux.rs"]
mod linux;
