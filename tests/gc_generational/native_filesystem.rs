//! Native filesystem operands, error results and roots across real OS calls.
use super::*;

struct Fixture(PathBuf);
impl Fixture {
    fn new() -> Self {
        let p = temp_path("native_filesystem", "dir");
        fs::create_dir(&p).unwrap();
        Self(p)
    }
    fn windows_drive() -> Self {
        let r = Command::new("powershell.exe")
            .args([
                "-NoProfile",
                "-NonInteractive",
                "-Command",
                "[IO.Path]::GetTempPath()",
            ])
            .output()
            .unwrap();
        assert!(r.status.success(), "{r:?}");
        let windows = String::from_utf8(r.stdout).unwrap();
        let r = Command::new("wslpath")
            .arg("-u")
            .arg(windows.trim())
            .output()
            .unwrap();
        assert!(r.status.success(), "{r:?}");
        let base = PathBuf::from(String::from_utf8(r.stdout).unwrap().trim());
        let unique = temp_path("native_filesystem_drive", "dir");
        let p = base.join(unique.file_name().unwrap());
        fs::create_dir(&p).unwrap();
        Self(p)
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
    fn run(&self, target: &str, image: &Path, fault: u32, arm_fault: &str) -> Output {
        if target == "x86_64-pc-windows-msvc" {
            windows_production::run(&windows_production::driver(&self.0), image, fault)
        } else if target == "aarch64-apple-darwin" {
            Command::new(aarch64_macho::emulator_python().expect("filesystem requires unicorn"))
                .arg(Path::new(env!("CARGO_MANIFEST_DIR")).join("tools/run-macho-aarch64.py"))
                .arg(image)
                .env("KLASSIC_MACHO_REQUIRE_NATIVE_FILESYSTEM", "1")
                .env("KLASSIC_MACHO_FS_FAULT", arm_fault)
                .output()
                .unwrap()
        } else {
            Command::new(image).output().unwrap()
        }
    }
    fn program(&self, windows: bool) -> String {
        let dir = self.path("new", windows);
        let moved = self.path("moved", windows);
        let file = self.path("file", windows);
        let nested = self.path("nested/a/b", windows);
        let a = self.path("nested/a", windows);
        let parent = self.path("nested", windows);
        format!(
            r#"
enum Box {{ case Box(v: Int) }}
def exists(p: String): Bool = Dir#exists(p)
def isdir(p: String): Bool = Dir#isDirectory(p)
def isfile(p: String): Bool = Dir#isFile(p)
def fileexists(p: String): Bool = FileOutput#exists(p)
def create(p: String): Unit = Dir#mkdir(p)
def remove(p: String): Unit = Dir#delete(p)
def erase(p: String): Unit = FileOutput#delete(p)
def move(s: String, t: String): Unit = Dir#move(s,t)
val keep = Box(84)
mutable i = 0
while(i < 80) {{ val garbage = Box(i); i += 1 }}
println(exists({dir:?}))
create({dir:?})
println(isdir({dir:?}))
println(isfile({file:?}))
println(fileexists({file:?}))
println(isfile({dir:?}))
println(isdir({file:?}))
erase({file:?})
erase({file:?})
println(exists({file:?}))
move({dir:?},{moved:?})
println(exists({dir:?}))
println(isdir({moved:?}))
remove({moved:?})
Dir#mkdirs({nested:?})
Dir#mkdirs({nested:?})
remove({nested:?})
remove({a:?})
remove({parent:?})
println(length(Dir#current()) > 0)
println(keep match {{ case Box(v) => v }})
"#
        )
    }
    fn check(&self, r: &Output, expected: &[u8]) {
        let stderr = String::from_utf8_lossy(&r.stderr);
        assert!(r.status.success(), "{:?}\n{stderr}", r.status);
        assert_eq!(r.stdout, expected, "{stderr}");
        let v = gc_stats(&stderr);
        assert!(v["native_io_entries"] > 0, "{stderr}");
        assert_eq!(v["native_io_entries"], v["native_io_exits"], "{stderr}");
    }
}
impl Drop for Fixture {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.0);
    }
}
const TARGETS: [&str; 3] = [
    "x86_64-unknown-linux-gnu",
    "x86_64-pc-windows-msvc",
    "aarch64-apple-darwin",
];
const EXPECTED: &[u8] = b"false\ntrue\ntrue\ntrue\nfalse\nfalse\nfalse\nfalse\ntrue\ntrue\n84\n";

#[test]
fn filesystem_calls_preserve_roots_and_native_state_on_each_target() {
    for target in TARGETS {
        for stress in [false, true] {
            let f = Fixture::new();
            fs::write(f.0.join("file"), b"contents").unwrap();
            let image = f.build(target, stress, &f.program(false));
            let r = f.run(target, &image, 0, "");
            f.check(&r, EXPECTED);
            let prefixes =
                f.0.join("nested/a/b")
                    .components()
                    .filter(|c| matches!(c, std::path::Component::Normal(_)))
                    .count();
            assert_eq!(
                native_nonbuffer_entries(&gc_stats(&String::from_utf8_lossy(&r.stderr))),
                18 + 2 * prefixes as u64
            );
            for name in ["file", "new", "moved", "nested"] {
                assert!(!f.0.join(name).exists());
            }
            if target == TARGETS[0] {
                fs::write(f.0.join("file"), b"contents").unwrap();
                let oracle = Command::new(klassic_bin())
                    .arg(f.0.join("source.kl"))
                    .output()
                    .unwrap();
                assert!(oracle.status.success(), "{oracle:?}");
                assert_eq!(r.stdout, oracle.stdout);
            }
        }
    }
}

#[test]
fn directory_enumeration_uses_native_buffers_and_detects_api_failures() {
    for target in &TARGETS[..2] {
        for stress in [false, true] {
            let f = Fixture::new();
            fs::create_dir(f.0.join("entries")).unwrap();
            for name in ["beta", "alpha"] {
                fs::write(f.0.join("entries").join(name), b"x").unwrap();
            }
            let body = format!(
                "def list(p: String): String = join(Dir#list(p), \",\")\ndef full(p: String): Int = size(Dir#listFull(p))\nprintln(list({:?}))\nprintln(full({:?}))",
                f.path("entries", false),
                f.path("entries", false)
            );
            let image = f.build(target, stress, &body);
            let r = f.run(target, &image, 0, "");
            f.check(&r, b"alpha,beta\n2\n");
            assert_eq!(
                native_nonbuffer_entries(&gc_stats(&String::from_utf8_lossy(&r.stderr))),
                // Enumeration plus one managed-string copy-out for the join.
                if *target == TARGETS[0] { 9 } else { 13 }
            );
            if *target == TARGETS[1] {
                for fault in [58, 59, 60] {
                    let r = f.run(target, &image, fault, "");
                    let err = String::from_utf8_lossy(&r.stderr);
                    assert!(!r.status.success(), "{fault}: {r:?}");
                    assert!(err.contains("failed to"), "{fault}: {err}");
                }
            }
        }
    }
}

#[test]
fn recursive_target_evaluation_retains_both_native_paths() {
    for target in TARGETS {
        for operation in if target == TARGETS[2] {
            &["move"][..]
        } else {
            &["move", "copy"][..]
        } {
            for stress in [false, true] {
                let f = Fixture::new();
                fs::write(f.0.join("outer"), b"outer").unwrap();
                fs::write(f.0.join("inner"), b"inner").unwrap();
                let body = format!(
                    r#"
enum Box {{ case Box(v: Int) }}
def move(s: String,t: String,n: Int): Unit = Dir#{operation}(s, if(n <= 0) t else {{
  move({inner:?},{inner_to:?},0)
  val fresh = Box(84)
  t
}})
move({outer:?},{outer_to:?},1)
println(84)
"#,
                    inner = f.path("inner", false),
                    inner_to = f.path("inner-to", false),
                    outer = f.path("outer", false),
                    outer_to = f.path("outer-to", false)
                );
                let image = f.build(target, stress, &body);
                f.check(&f.run(target, &image, 0, ""), b"84\n");
                assert_eq!(fs::read(f.0.join("outer-to")).unwrap(), b"outer");
                assert_eq!(fs::read(f.0.join("inner-to")).unwrap(), b"inner");
            }
        }
    }
}

#[test]
fn windows_filesystem_dword_errors_return_to_running_before_diagnostics() {
    for (fault, operation) in [
        (51, "Dir#isDirectory"),
        (52, "Dir#current"),
        (53, "Dir#current"),
        (54, "Dir#mkdir"),
        (55, "Dir#delete"),
        (56, "Dir#move"),
        (57, "FileOutput#delete"),
    ] {
        let f = Fixture::new();
        fs::create_dir(f.0.join("directory")).unwrap();
        let path = f.path("directory", false);
        let body = if operation == "Dir#current" {
            "println(Dir#current())".to_owned()
        } else if operation == "Dir#move" {
            format!(
                "def op(p: String): Unit = Dir#move(p,{:?})\nop({path:?})",
                f.path("dest", false)
            )
        } else {
            format!("def op(p: String) = {operation}(p)\nprintln(op({path:?}))")
        };
        let image = f.build(TARGETS[1], false, &format!("{body}\nprintln(99)"));
        let r = f.run(TARGETS[1], &image, fault, "");
        let err = String::from_utf8_lossy(&r.stderr);
        assert!(!r.status.success(), "{fault}: {r:?}");
        assert!(err.contains("failed"), "{fault}: {err}");
        assert!(!r.stdout.windows(2).any(|b| b == b"99"), "{fault}: {r:?}");
    }
}

#[test]
fn arm_filesystem_carry_errors_and_false_predicates_survive_native_exit() {
    for (fault, operation) in [
        ("getcwd", "Dir#current"),
        ("mkdir", "Dir#mkdir"),
        ("rmdir", "Dir#delete"),
        ("rename", "Dir#move"),
        ("unlink", "FileOutput#delete"),
        ("access", "Dir#exists"),
        ("stat", "Dir#isDirectory"),
    ] {
        let f = Fixture::new();
        let path = f.path("unused", false);
        let body = if operation == "Dir#current" {
            "println(Dir#current())".to_owned()
        } else if operation == "Dir#move" {
            format!("Dir#move({path:?},{:?})", f.path("dest", false))
        } else {
            format!("{operation}({path:?})")
        };
        let predicate = fault == "access" || fault == "stat";
        let body = if predicate {
            format!("println({body})")
        } else {
            body
        };
        let image = f.build(TARGETS[2], false, &format!("{body}\nprintln(99)"));
        let r = f.run(TARGETS[2], &image, 0, fault);
        let err = String::from_utf8_lossy(&r.stderr);
        assert_eq!(r.status.success(), predicate, "{fault}: {r:?}");
        if predicate {
            assert_eq!(r.stdout, b"false\n99\n", "{err}");
        } else {
            assert!(err.contains("failed"), "{fault}: {err}");
            assert!(r.stdout.is_empty(), "{r:?}");
        }
    }
}

#[test]
fn filesystem_calls_run_against_the_actual_windows_kernel() {
    if !fs::read_to_string("/proc/sys/kernel/osrelease")
        .is_ok_and(|s| s.to_lowercase().contains("microsoft"))
    {
        return;
    }
    for (stress, drive) in [(false, false), (true, false), (false, true), (true, true)] {
        let f = if drive {
            Fixture::windows_drive()
        } else {
            Fixture::new()
        };
        fs::write(f.0.join("file"), b"contents").unwrap();
        let image = f.build(TARGETS[1], stress, &f.program(true));
        f.check(
            &windows_production::run_native_image(&f.0, &image).unwrap(),
            EXPECTED,
        );
        fs::create_dir(f.0.join("entries")).unwrap();
        for name in ["alpha", "beta"] {
            fs::write(f.0.join("entries").join(name), b"x").unwrap();
        }
        let path = f.path("entries", true);
        let body = format!(
            "def list(p: String): String = join(Dir#list(p), \",\")\nprintln(list({path:?}))"
        );
        let image = f.build(TARGETS[1], stress, &body);
        f.check(
            &windows_production::run_native_image(&f.0, &image).unwrap(),
            b"alpha,beta\n",
        );
    }
}

#[test]
fn linux_directory_syscall_errors_do_not_return_partial_results() {
    for (syscall, errno) in [("open", "EACCES"), ("getdents64", "EIO"), ("close", "EIO")] {
        let f = Fixture::new();
        fs::create_dir(f.0.join("entries")).unwrap();
        fs::write(f.0.join("entries/alpha"), b"x").unwrap();
        let path = f.path("entries", false);
        let image=f.build(TARGETS[0],false,&format!("def list(p: String): String = join(Dir#list(p), \",\")\nprintln(list({path:?}))\nprintln(99)"));
        let r = Command::new("strace")
            .args(["-f", "-qq", "-e"])
            .arg(format!("trace={syscall}"))
            .arg("-e")
            .arg(format!("inject={syscall}:error={errno}:when=1"))
            .arg(image)
            .output()
            .unwrap();
        assert!(!r.status.success(), "{syscall}: {r:?}");
        assert!(r.stdout.is_empty(), "{syscall}: {r:?}");
        assert!(
            String::from_utf8_lossy(&r.stderr).contains("failed to"),
            "{syscall}: {r:?}"
        );
        assert!(
            String::from_utf8_lossy(&r.stderr).contains("INJECTED"),
            "{syscall}: {r:?}"
        );
    }
}

#[test]
fn linux_filesystem_reports_real_os_failures_and_tolerates_empty_mkdirs() {
    for operation in ["Dir#mkdir", "Dir#delete", "Dir#move"] {
        let f = Fixture::new();
        fs::create_dir(f.0.join("directory")).unwrap();
        fs::write(f.0.join("directory/child"), b"x").unwrap();
        let path = if operation == "Dir#move" {
            f.path("missing", false)
        } else {
            f.path("directory", false)
        };
        let body = if operation == "Dir#move" {
            format!(
                "def op(p: String): Unit = Dir#move(p,{:?})",
                f.path("dest", false)
            )
        } else {
            format!("def op(p: String): Unit = {operation}(p)")
        };
        let image = f.build(
            TARGETS[0],
            false,
            &format!("{body}\nop({path:?})\nprintln(99)"),
        );
        let r = f.run(TARGETS[0], &image, 0, "");
        assert!(!r.status.success(), "{r:?}");
        assert!(r.stdout.is_empty(), "{r:?}");
        assert!(
            String::from_utf8_lossy(&r.stderr).contains("failed"),
            "{r:?}"
        );
    }
    for target in TARGETS {
        let f = Fixture::new();
        let image = f.build(target, false, "Dir#mkdirs(\"\")\nprintln(84)");
        let r = if target == TARGETS[2] {
            Command::new(aarch64_macho::emulator_python().unwrap())
                .arg(Path::new(env!("CARGO_MANIFEST_DIR")).join("tools/run-macho-aarch64.py"))
                .arg(image)
                .output()
                .unwrap()
        } else {
            f.run(target, &image, 0, "")
        };
        assert!(r.status.success(), "{r:?}");
        assert_eq!(r.stdout, b"84\n", "{r:?}");
        assert_eq!(
            native_nonbuffer_entries(&gc_stats(&String::from_utf8_lossy(&r.stderr))),
            0
        );
    }
}
