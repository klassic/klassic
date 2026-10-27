//! Short reads, EOF reuse, copy boundaries, and Windows API failures.
use super::*;
struct Fixture(PathBuf);
impl Fixture {
    fn new() -> Self {
        let p = temp_path("native_stdio_copy", "dir");
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
    fn input(&self, bytes: &[u8]) -> fs::File {
        fs::write(self.0.join("stdin"), bytes).unwrap();
        fs::File::open(self.0.join("stdin")).unwrap()
    }
    fn path(&self, name: &str, windows: bool) -> String {
        let p = self.0.join(name);
        if windows {
            let r = Command::new("wslpath").arg("-w").arg(p).output().unwrap();
            assert!(r.status.success());
            String::from_utf8(r.stdout).unwrap().trim().into()
        } else {
            p.display().to_string()
        }
    }
    fn arm(&self, image: &Path, bytes: &[u8], short: bool) -> Output {
        let mut c = Command::new(aarch64_macho::emulator_python().expect("stdin requires unicorn"));
        c.arg(Path::new(env!("CARGO_MANIFEST_DIR")).join("tools/run-macho-aarch64.py"))
            .arg(image)
            .stdin(self.input(bytes))
            .env("KLASSIC_MACHO_REQUIRE_NATIVE_FILE_IO", "1");
        if short {
            c.env("KLASSIC_MACHO_STDIN_CHUNK", "3");
        }
        c.output().unwrap()
    }
    fn model(&self, image: &Path, bytes: &[u8], fault: u32) -> Output {
        Command::new(windows_production::driver(&self.0))
            .arg(image)
            .arg(fault.to_string())
            .stdin(self.input(bytes))
            .output()
            .unwrap()
    }
}
impl Drop for Fixture {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.0);
    }
}
const STDIN: &str = r#"
enum Box { case Box(v: Int) }
val keep = Box(84)
val text = StandardInput#all()
val empty = StandardInput#all()
mutable i = 0
mutable total = 0
while(i < 80) { val fresh = Box(i); total += fresh match { case Box(v) => v }; i += 1 }
println(length(text))
println(length(empty))
println(keep match { case Box(v) => v })
println(text == "alpha\nbeta\n")
println(total)
"#;
fn check(r: &Output, expected: &[u8], entries: u64) {
    let stderr = String::from_utf8_lossy(&r.stderr);
    assert!(r.status.success(), "{:?}\n{stderr}", r.status);
    assert_eq!(r.stdout, expected, "{stderr}");
    let v = gc_stats(&stderr);
    assert_eq!(
        (v["native_io_entries"] - v["native_buffer_writes"]),
        entries,
        "{stderr}"
    );
    assert_eq!(
        (v["native_io_exits"] - v["native_buffer_writes"]),
        entries,
        "{stderr}"
    );
}
#[test]
fn stdin_keeps_roots_and_remains_open_after_eof_on_all_backends() {
    for stress in [false, true] {
        for target in [
            "x86_64-unknown-linux-gnu",
            "aarch64-apple-darwin",
            "x86_64-pc-windows-msvc",
        ] {
            let f = Fixture::new();
            let image = f.build(target, stress, STDIN);
            let r = match target {
                "aarch64-apple-darwin" => f.arm(&image, b"alpha\nbeta\n", true),
                "x86_64-pc-windows-msvc" => f.model(&image, b"alpha\nbeta\n", 48),
                _ => Command::new(image)
                    .stdin(f.input(b"alpha\nbeta\n"))
                    .output()
                    .unwrap(),
            };
            check(
                &r,
                b"11\n0\n84\ntrue\n3160\n",
                if target.starts_with("x86_64-unknown") {
                    3
                } else {
                    6
                },
            );
        }
    }
}
#[test]
fn stdin_exact_capacity_and_overflow_are_distinguished() {
    let f = Fixture::new();
    let body = "println(length(StandardInput#all()))\nprintln(length(StandardInput#all()))";
    for target in ["x86_64-unknown-linux-gnu", "x86_64-pc-windows-msvc"] {
        let image = f.build(target, false, body);
        for n in [0, 65536, 65537] {
            let bytes = vec![b'q'; n];
            let r = if target.contains("windows") {
                f.model(&image, &bytes, 0)
            } else {
                Command::new(&image)
                    .stdin(f.input(&bytes))
                    .output()
                    .unwrap()
            };
            if n > 65536 {
                assert_eq!(r.status.code(), Some(1), "{r:?}");
                assert!(
                    String::from_utf8_lossy(&r.stderr).contains("exceeds 65536 bytes"),
                    "{r:?}"
                );
            } else {
                check(
                    &r,
                    format!("{n}\n0\n").as_bytes(),
                    if n == 0 { 2 } else { 3 },
                );
            }
        }
    }
}
#[test]
fn windows_stdin_handles_closed_pipe_and_reports_other_errors() {
    let f = Fixture::new();
    let image = f.build(
        "x86_64-pc-windows-msvc",
        false,
        "println(length(StandardInput#all()))\nprintln(99)",
    );
    for fault in [45, 46, 47] {
        let r = f.model(&image, b"", fault);
        if fault == 45 {
            assert_eq!(r.status.code(), Some(1), "{r:?}");
            assert!(
                String::from_utf8_lossy(&r.stderr).contains("failed to read stdin"),
                "{r:?}"
            );
        } else {
            check(&r, b"0\n99\n", 1);
        }
    }
}
#[test]
fn windows_file_api_failures_do_not_become_zero_byte_successes() {
    for (fault, op, body) in [
        (41, "read", "println(length(FileInput#all(P)))"),
        (42, "write", "FileOutput#write(P, \"data\")"),
        (43, "close", "FileOutput#write(P, \"data\")"),
        (44, "copy", "Dir#copy(P, Q)"),
    ] {
        let f = Fixture::new();
        fs::write(f.0.join("data"), "before").unwrap();
        let body = body
            .replace('P', &format!("{:?}", f.path("data", false)))
            .replace('Q', &format!("{:?}", f.path("copy", false)));
        let image = f.build(
            "x86_64-pc-windows-msvc",
            false,
            &format!("{body}\nprintln(99)"),
        );
        let r = f.model(&image, b"", fault);
        let stderr = String::from_utf8_lossy(&r.stderr);
        assert_eq!(r.status.code(), Some(1), "{op}: {stderr}");
        assert!(
            stderr.contains(&format!("failed to {op} file")),
            "{op}: {stderr}"
        );
        assert!(!r.stdout.windows(2).any(|w| w == b"99"), "{stderr}");
    }
}
#[test]
fn copy_and_stream_keep_native_state_through_multiple_chunks() {
    for stress in [false, true] {
        for target in ["x86_64-unknown-linux-gnu", "x86_64-pc-windows-msvc"] {
            for n in [0usize, 20_000, 131_072] {
                if target.contains("windows") && n > 65536 {
                    continue;
                }
                let f = Fixture::new();
                let bytes = vec![b'q'; n];
                fs::write(f.0.join("input"), &bytes).unwrap();
                let body = format!(
                    "Dir#copy({:?}, {:?})\nprintln(FileInput#all({:?}))",
                    f.path("input", false),
                    f.path("copy", false),
                    f.path("copy", false)
                );
                let image = f.build(target, stress, &body);
                let r = if target.contains("windows") {
                    f.model(&image, b"", 0)
                } else {
                    Command::new(&image).output().unwrap()
                };
                let expected = [bytes.clone(), b"\n".to_vec()].concat();
                let entries = if target.contains("windows") {
                    4
                } else {
                    3 + 2 * n.div_ceil(8192) as u64 + if n == 0 { 5 } else { 6 }
                };
                check(&r, &expected, entries);
                assert_eq!(fs::read(f.0.join("copy")).unwrap(), bytes);
            }
        }
    }
}
#[test]
fn linux_stdin_waits_for_eof_across_separate_pipe_writes() {
    use std::io::Write;
    use std::process::Stdio;
    let f = Fixture::new();
    let image = f.build("x86_64-unknown-linux-gnu", true, STDIN);
    let mut child = Command::new(image)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();
    let mut input = child.stdin.take().unwrap();
    let writer = std::thread::spawn(move || {
        for chunk in [b"alp".as_slice(), b"ha\n", b"bet", b"a\n"] {
            input.write_all(chunk).unwrap();
            std::thread::sleep(std::time::Duration::from_millis(20));
        }
    });
    let r = child.wait_with_output().unwrap();
    writer.join().unwrap();
    assert!(r.status.success(), "{r:?}");
    assert_eq!(r.stdout, b"11\n0\n84\ntrue\n3160\n", "{r:?}");
    let v = gc_stats(&String::from_utf8_lossy(&r.stderr));
    assert!(v["native_io_entries"] >= 3);
    assert_eq!(v["native_io_entries"], v["native_io_exits"]);
}
#[test]
fn actual_windows_stdin_pipe_and_copy_use_native_apis() {
    if !windows_production::native_host() {
        return;
    }
    for stress in [false, true] {
        let f = Fixture::new();
        let image = f.build("x86_64-pc-windows-msvc", stress, STDIN);
        f.input(b"alpha\nbeta\n");
        let r = windows_production::run_native_image_with_input(&f.0, &image, &f.0.join("stdin"));
        // Pipe reads may complete in any positive chunk size; both terminal reads return EOF.
        assert!(r.status.success(), "{r:?}");
        assert_eq!(r.stdout, b"11\n0\n84\ntrue\n3160\n", "{r:?}");
        let v = gc_stats(&String::from_utf8_lossy(&r.stderr));
        assert!(v["native_io_entries"] >= 3);
        assert_eq!(v["native_io_entries"], v["native_io_exits"]);
        let body = format!(
            "Dir#copy({:?}, {:?})\nprintln(length(FileInput#all({:?})))",
            f.path("stdin", true),
            f.path("copy", true),
            f.path("copy", true)
        );
        let image = f.build("x86_64-pc-windows-msvc", stress, &body);
        let r = windows_production::run_native_image(&f.0, &image).unwrap();
        check(&r, b"11\n", 4);
        assert_eq!(fs::read(f.0.join("copy")).unwrap(), b"alpha\nbeta\n");
    }
}
