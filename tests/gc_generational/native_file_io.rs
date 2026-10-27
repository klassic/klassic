//! File operations publish native state and keep the kernel off managed data.
use super::*;

struct Fixture(PathBuf);
impl Fixture {
    fn new() -> Self {
        let p = temp_path("native_file_io", "dir");
        fs::create_dir(&p).unwrap();
        Self(p)
    }
    fn build(&self, target: &str, stress: bool, body: &str) -> PathBuf {
        let source = self.0.join("source.kl");
        let image = self.0.join("image.bin");
        fs::write(&source, body).unwrap();
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
        let r = build(&flags, &source, &image);
        assert!(r.status.success(), "{}", String::from_utf8_lossy(&r.stderr));
        image
    }
    fn program(&self) -> String {
        self.program_in(false)
    }
    fn program_in(&self, native_windows: bool) -> String {
        let path = |name| {
            let p = self.0.join(name);
            if native_windows && !cfg!(windows) {
                let r = Command::new("wslpath").arg("-w").arg(&p).output().unwrap();
                assert!(r.status.success());
                String::from_utf8(r.stdout).unwrap().trim().to_string()
            } else {
                p.display().to_string()
            }
        };
        let data = path("data.txt");
        let empty = path("empty.txt");
        format!(
            r#"
enum Box {{ case Box(v: Int) }}
def grow(s: String, n: Int): String = if(n <= 0) s else grow(s + s, n - 1)
def load(p: String): String = FileInput#all(p)
val keep = Box(84)
val text = grow("q", 13)
FileOutput#write({data:?}, text)
FileOutput#append({data:?}, "tail")
FileOutput#write({empty:?}, "")
val again = grow("z", 12)
mutable i = 0
mutable total = 0
while(i < 80) {{
  val fresh = Box(i)
  total += fresh match {{ case Box(v) => v }}
  i += 1
}}
val content = load({data:?})
println(length(content))
println(keep match {{ case Box(v) => v }})
println(length(load({empty:?})))
println(length(again))
println(total)
"#
        )
    }
    fn check(&self, r: &Output) {
        let stderr = String::from_utf8_lossy(&r.stderr);
        assert!(r.status.success(), "{:?}\n{stderr}", r.status);
        assert_eq!(r.stdout, b"8196\n84\n0\n4096\n3160\n", "{stderr}");
        assert_eq!(
            fs::read(self.0.join("data.txt")).unwrap(),
            [vec![b'q'; 8192], b"tail".to_vec()].concat()
        );
        assert!(fs::read(self.0.join("empty.txt")).unwrap().is_empty());
        let v = gc_stats(&stderr);
        assert_eq!(
            (v["native_io_entries"] - v["native_buffer_writes"]),
            15,
            "{stderr}"
        );
        assert_eq!(
            (v["native_io_exits"] - v["native_buffer_writes"]),
            15,
            "{stderr}"
        );
    }
}
impl Drop for Fixture {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.0);
    }
}

#[test]
fn linux_file_io_keeps_roots_and_balances_native_state() {
    for stress in [false, true] {
        let f = Fixture::new();
        let image = f.build("x86_64-unknown-linux-gnu", stress, &f.program());
        let run = Command::new(image).output().unwrap();
        f.check(&run);
        let oracle = Command::new(klassic_bin())
            .arg(f.0.join("source.kl"))
            .output()
            .unwrap();
        assert!(oracle.status.success(), "{oracle:?}");
        assert_eq!(run.stdout, oracle.stdout);
    }
}

#[test]
fn arm_file_io_copies_native_buffers_and_preserves_syscall_status() {
    for stress in [false, true] {
        let f = Fixture::new();
        let image = f.build("aarch64-apple-darwin", stress, &f.program());
        let r = Command::new(aarch64_macho::emulator_python().expect("file I/O requires unicorn"))
            .arg(Path::new(env!("CARGO_MANIFEST_DIR")).join("tools/run-macho-aarch64.py"))
            .arg(image)
            .env("KLASSIC_MACHO_REQUIRE_NATIVE_FILE_IO", "1")
            .output()
            .unwrap();
        f.check(&r);
    }
}

#[test]
fn windows_file_io_checks_native_state_and_handle_calls() {
    for stress in [false, true] {
        let f = Fixture::new();
        let driver = windows_production::driver(&f.0);
        let image = f.build("x86_64-pc-windows-msvc", stress, &f.program());
        let r = windows_production::run(&driver, &image, 0);
        f.check(&r);
        assert_eq!(
            String::from_utf8_lossy(&r.stderr)
                .matches("win-native-file: state=2")
                .count(),
            15
        );
    }
}

#[test]
fn windows_file_io_uses_actual_windows_kernel() {
    if !cfg!(windows)
        && !fs::read_to_string("/proc/sys/kernel/osrelease")
            .is_ok_and(|s| s.to_ascii_lowercase().contains("microsoft"))
    {
        return;
    }
    for stress in [false, true] {
        let f = Fixture::new();
        let image = f.build("x86_64-pc-windows-msvc", stress, &f.program_in(true));
        let r = windows_production::run_native_image(&f.0, &image).unwrap();
        f.check(&r);
    }
}

#[test]
fn arm_file_io_error_carry_survives_return_to_running() {
    for fault in ["open", "write", "close", "read"] {
        let f = Fixture::new();
        let body = if fault == "read" {
            fs::write(f.0.join("data.txt"), "before").unwrap();
            format!(
                "println(length(FileInput#all({:?})))\nprintln(99)",
                f.0.join("data.txt").display().to_string()
            )
        } else {
            format!(
                "FileOutput#write({:?}, \"bytes\")\nprintln(99)",
                f.0.join("data.txt").display().to_string()
            )
        };
        let image = f.build("aarch64-apple-darwin", false, &body);
        let r = Command::new(aarch64_macho::emulator_python().unwrap())
            .arg(Path::new(env!("CARGO_MANIFEST_DIR")).join("tools/run-macho-aarch64.py"))
            .arg(image)
            .env("KLASSIC_MACHO_REQUIRE_NATIVE_FILE_IO", "1")
            .env("KLASSIC_MACHO_FILE_FAULT", fault)
            .output()
            .unwrap();
        let stderr = String::from_utf8_lossy(&r.stderr);
        assert_eq!(r.status.code(), Some(1), "{fault}: {stderr}");
        assert!(
            stderr.contains(&format!("failed to {fault} file")),
            "{fault}: {stderr}"
        );
        assert!(!r.stdout.windows(2).any(|w| w == b"99"), "{stderr}");
    }
}

#[test]
fn arm_file_io_releases_large_native_copy_after_each_write() {
    let f = Fixture::new();
    let path = f.0.join("large.txt").display().to_string();
    let body = format!(
        r#"
def grow(s: String, n: Int): String = if(n <= 0) s else grow(s + s, n - 1)
val text = grow("q", 18)
mutable i = 0
while(i < 3) {{ FileOutput#write({path:?}, text); i += 1 }}
println(length(FileInput#all({path:?})))
println(length(text))
"#
    );
    let image = f.build("aarch64-apple-darwin", true, &body);
    let r = Command::new(aarch64_macho::emulator_python().unwrap())
        .arg(Path::new(env!("CARGO_MANIFEST_DIR")).join("tools/run-macho-aarch64.py"))
        .arg(image)
        .env("KLASSIC_MACHO_REQUIRE_NATIVE_FILE_IO", "1")
        .output()
        .unwrap();
    let stderr = String::from_utf8_lossy(&r.stderr);
    assert!(r.status.success(), "{stderr}");
    assert_eq!(r.stdout, b"262144\n262144\n");
    assert_eq!(fs::read(f.0.join("large.txt")).unwrap(), vec![b'q'; 262144]);
    let v = gc_stats(&stderr);
    assert_eq!((v["native_io_entries"] - v["native_buffer_writes"]), 12);
    assert_eq!((v["native_io_exits"] - v["native_buffer_writes"]), 12);
    assert!(v["large_allocations"] > 0);
}

#[test]
fn linux_and_windows_missing_file_errors_leave_native_state() {
    for target in ["x86_64-unknown-linux-gnu", "x86_64-pc-windows-msvc"] {
        let f = Fixture::new();
        let body = format!(
            "println(length(FileInput#all({:?})))\nprintln(99)",
            f.0.join("missing.txt").display().to_string()
        );
        let image = f.build(target, false, &body);
        let run = if target.contains("windows") {
            windows_production::run(&windows_production::driver(&f.0), &image, 0)
        } else {
            Command::new(image).output().unwrap()
        };
        let stderr = String::from_utf8_lossy(&run.stderr);
        assert_eq!(run.status.code(), Some(1), "{stderr}");
        assert!(stderr.contains("failed to open file"), "{stderr}");
        assert!(!stderr.contains("runtime invariant violated"), "{stderr}");
        assert!(run.stdout.is_empty());
    }
}
