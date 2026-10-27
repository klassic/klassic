//! Actual Windows loader/kernel execution on Windows and WSL interop hosts.
use super::*;
pub(super) fn native_host() -> bool {
    cfg!(windows)
        || fs::read_to_string("/proc/sys/kernel/osrelease")
            .is_ok_and(|s| s.to_ascii_lowercase().contains("microsoft"))
}
fn windows_path(path: &Path) -> String {
    if cfg!(windows) {
        return path.to_string_lossy().into_owned();
    }
    let result = Command::new("wslpath")
        .arg("-w")
        .arg(path)
        .output()
        .unwrap();
    assert!(result.status.success(), "{result:?}");
    String::from_utf8(result.stdout).unwrap().trim().into()
}
struct Fixture {
    scratch: Scratch,
    image: PathBuf,
}
impl Fixture {
    fn new(idle: bool) -> Self {
        Self::with_tail(idle, "")
    }
    fn with_tail(idle: bool, tail: &str) -> Self {
        let scratch = Scratch(temp_path("windows_native", "dir"));
        fs::create_dir(&scratch.0).unwrap();
        let source = scratch.0.join("source.kl");
        let image = scratch.0.join("native.exe");
        fs::write(
            &source,
            if idle {
                SOURCE.replace("println(total)", "println(total)\nsleep(5000)")
            } else {
                SOURCE.into()
            },
        )
        .unwrap();
        if !tail.is_empty() {
            let original = fs::read_to_string(&source).unwrap();
            fs::write(&source, format!("{original}\n{tail}\nprintln(99)\n")).unwrap();
        }
        let output = build(
            &[
                "--target",
                "x86_64-pc-windows-msvc",
                "--gc=generational",
                "--gc-log",
                "--gc-max-heap=16MiB",
                "--gc-min-heap=128KiB",
                if idle {
                    "--gc-uncommit-delay=1500ms"
                } else {
                    "--gc-uncommit-delay=0"
                },
            ],
            &source,
            &image,
        );
        assert!(output.status.success(), "{output:?}");
        Self { scratch, image }
    }
    fn run(&self, idle: bool, affine: bool) -> Output {
        run_image(&self.scratch.0, &self.image, idle, affine)
    }
}
pub(super) fn run_image(directory: &Path, image: &Path, idle: bool, affine: bool) -> Output {
    run_image_with_input(directory, image, idle, affine, None)
}
pub(super) fn run_image_with_input(
    directory: &Path,
    image: &Path,
    idle: bool,
    affine: bool,
    input: Option<&Path>,
) -> Output {
    let script = include_str!("windows_native_runner.ps1")
        .replace(
            "__IMAGE__",
            &format!("'{}'", windows_path(image).replace('\'', "''")),
        )
        .replace("__IDLE__", if idle { "$true" } else { "$false" })
        .replace("__AFFINE__", if affine { "$true" } else { "$false" })
        .replace("__HAS_STDIN__", if input.is_some() { "$true" } else { "$false" })
        .replace("__STDIN__", &input.map_or_else(String::new, |path| format!(
            "$bytes = [System.IO.File]::ReadAllBytes('{}')\n    $p.StandardInput.BaseStream.Write($bytes, 0, $bytes.Length)\n    $p.StandardInput.Close()", windows_path(path).replace('\'', "''"))));
    let path = directory.join("runner.ps1");
    // Windows PowerShell 5.1 needs a BOM to read non-ASCII paths reliably.
    let mut bytes = vec![0xff, 0xfe];
    bytes.extend(script.encode_utf16().flat_map(u16::to_le_bytes));
    fs::write(&path, bytes).unwrap();
    let child = Command::new("powershell.exe")
        .args([
            "-NoLogo",
            "-NoProfile",
            "-NonInteractive",
            "-ExecutionPolicy",
            "Bypass",
            "-File",
        ])
        .arg(windows_path(&path))
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .unwrap();
    super::capture(
        child,
        std::time::Duration::from_secs(140),
        "Windows native runner exceeded its bounded child waits",
    )
}
fn check(output: &Output) -> std::collections::HashMap<String, u64> {
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(output.status.success(), "{output:?}");
    assert_eq!(output.stdout, b"153600\n64\n8595182865\n", "{stderr}");
    let v = gc_stats(&stderr);
    assert_eq!(v["objects"], v["published"], "{stderr}");
    assert_eq!(v["unmarked_copied"], 0, "{stderr}");
    assert_eq!(v["young_unmarked_copied"], 0, "{stderr}");
    assert!(
        v["collections"] > 0 && v["worker_scanned"] > 0 && v["young_worker_units"] > 0,
        "{stderr}"
    );
    assert_eq!(
        v["heap_committed_bytes"],
        (v["heap_commit_pages"] - v["heap_uncommit_pages"]) * 131072,
        "{stderr}"
    );
    v
}
#[test]
fn windows_native_worker_progresses_on_one_cpu_and_preserves_live_values() {
    if !native_host() {
        eprintln!("native Windows loader/kernel test requires Windows or WSL interop");
        return;
    }
    let f = Fixture::new(false);
    for _ in 0..3 {
        let output = f.run(false, true);
        let v = check(&output);
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(stderr.contains("win-native: affinity=1"), "{stderr}");
        assert!(
            v["heap_uncommit_pages"] > 0 && v["heap_recommit_pages"] > 0,
            "{stderr}"
        );
    }
}
#[test]
fn windows_native_idle_worker_decommits_while_owner_sleeps() {
    if !native_host() {
        eprintln!("native Windows loader/kernel test requires Windows or WSL interop");
        return;
    }
    let f = Fixture::new(true);
    let output = f.run(true, false);
    let v = check(&output);
    let stderr = String::from_utf8_lossy(&output.stderr);
    // Native metadata growth/retirement uses the same actual boundary as
    // this sleep. Extra paired transitions do not invalidate idle decommit.
    assert_eq!(v["native_io_entries"], v["native_io_exits"], "{stderr}");
    assert!(
        v["native_io_entries"] > v["native_buffer_writes"],
        "missing Native sleep boundary: {stderr}"
    );
    assert_eq!(v["native_copyout_chunks"], 0, "{stderr}");
    assert_eq!(v["heap_uncommit_delay_ns"], 1_500_000_000);
    assert!(v["heap_uncommit_pages"] > 0, "{stderr}");
    let line = stderr
        .lines()
        .find(|s| s.starts_with("win-native:"))
        .unwrap();
    let drop: u64 = line
        .split_whitespace()
        .find_map(|s| s.strip_prefix("private_drop="))
        .unwrap()
        .parse()
        .unwrap();
    let samples = line
        .split_whitespace()
        .find_map(|s| s.strip_prefix("samples="))
        .unwrap();
    assert_eq!(samples.split(',').count(), 9, "{stderr}");
    assert!(drop >= 131072, "{stderr}");
}

#[test]
fn windows_native_explicit_exit_keeps_status_and_stops_worker() {
    if !native_host() {
        eprintln!("native Windows loader/kernel test requires Windows or WSL interop");
        return;
    }
    for (code, tail) in [
        (7, "exit(7)"),
        (23, "mutable status = 17\nexit(status + 6)"),
    ] {
        let f = Fixture::with_tail(false, tail);
        let output = f.run(false, true);
        assert_eq!(output.status.code(), Some(code), "{output:?}");
        assert_eq!(output.stdout, b"153600\n64\n8595182865\n", "{output:?}");
    }
}
