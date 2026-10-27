//! Standalone canonical C embedding-runtime checks. This has no dependency on
//! the C/LLVM language emitter or the direct Rust production runtime.
#![cfg(all(target_os = "linux", target_arch = "x86_64"))]
use std::io::Read;
use std::path::{Path, PathBuf};
use std::process::{Command, Output, Stdio};
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::{Duration, Instant};
struct Scratch(PathBuf);
impl Scratch {
    fn new() -> Self {
        static NEXT: AtomicU64 = AtomicU64::new(0);
        let p = std::env::temp_dir().join(format!(
            "klassic-gen-conformance-{}-{}-{}",
            std::process::id(),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos(),
            NEXT.fetch_add(1, Ordering::Relaxed)
        ));
        std::fs::create_dir(&p).unwrap();
        Self(p)
    }
}
impl Drop for Scratch {
    fn drop(&mut self) {
        if std::thread::panicking() {
            eprintln!(
                "canonical backend failure artifacts retained at {}",
                self.0.display()
            );
            return;
        }
        let _ = std::fs::remove_dir_all(&self.0);
    }
}
fn command_ok(command: &mut Command) -> Output {
    let output = command.output().unwrap();
    assert!(
        output.status.success(),
        "{command:?}: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    output
}
fn find_clang() -> Option<&'static str> {
    let compiler = ["clang-18", "clang-17", "clang-16", "clang-15", "clang"]
        .into_iter()
        .find(|name| {
            Command::new(name)
                .arg("--version")
                .output()
                .is_ok_and(|o| o.status.success())
        });
    assert!(
        compiler.is_some(),
        "canonical C core tests require a C compiler"
    );
    compiler
}
fn sanitizer() -> Option<&'static str> {
    std::env::var_os("KLASSIC_GEN_SANITIZE").map(|value| {
        if value == "thread" {
            "thread"
        } else {
            "address,undefined"
        }
    })
}
fn compile_c(clang: &str, source: &Path, object: &Path) {
    assert!(
        source.is_file(),
        "canonical C source missing: {}",
        source.display()
    );
    let mut compile = Command::new(clang);
    compile.args([
        "-std=c11",
        "-D_DEFAULT_SOURCE",
        "-O1",
        "-g",
        "-Wall",
        "-Wextra",
        "-Werror",
        "-pthread",
        "-c",
    ]);
    if let Some(sanitizer) = sanitizer() {
        compile
            .arg(format!("-fsanitize={sanitizer}"))
            .arg("-fno-omit-frame-pointer");
    }
    command_ok(compile.arg(source).arg("-o").arg(object));
}
fn runtime_directory() -> PathBuf {
    let path = std::env::var_os("KLASSIC_GEN_RUNTIME_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|| Path::new(env!("CARGO_MANIFEST_DIR")).join("runtime/gc"));
    eprintln!("canonical runtime source: {}", path.display());
    path
}
fn runtime_objects(clang: &str, gc: &Path, scratch: &Path) -> Vec<PathBuf> {
    [
        "klassic_gen_runtime",
        "klassic_gen_remembered",
        "klassic_gen_policy",
        "klassic_gen_string",
        "klassic_gen_scalar",
        "klassic_gen_verify",
        "gc_native_driver",
    ]
    .into_iter()
    .map(|name| {
        let object = scratch.join(format!("{name}.o"));
        compile_c(clang, &gc.join(format!("{name}.c")), &object);
        object
    })
    .collect()
}
fn link_program(
    clang: &str,
    gc: &Path,
    input: &Path,
    objects: &[PathBuf],
    runtime: Option<&Path>,
    binary: &Path,
) {
    let mut command = Command::new(clang);
    if let Some(sanitizer) = sanitizer() {
        command
            .arg(format!("-fsanitize={sanitizer}"))
            .args(["-fno-omit-frame-pointer", "-no-pie"]);
    }
    command
        .args(["-O2", "-Werror", "-Wno-override-module"])
        .arg(input)
        .arg("-I")
        .arg(gc)
        .args(objects);
    if let Some(runtime) = runtime {
        command.arg(runtime);
    }
    command.args(["-pthread", "-ldl", "-lm", "-o"]).arg(binary);
    command_ok(&mut command);
}
fn run_bounded(path: &Path) -> Output {
    run_bounded_args(path, &[])
}
fn run_bounded_args(path: &Path, arguments: &[&str]) -> Output {
    // Match the existing C sanitizer runner: use deterministic layout only
    // when the host actually permits it. A startup error still fails the test.
    let no_aslr = std::env::var("KLASSIC_GEN_SANITIZE").as_deref() == Ok("thread")
        && Command::new("setarch")
            .args(["x86_64", "-R", "true"])
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .status()
            .is_ok_and(|status| status.success());
    let mut command = if no_aslr {
        let mut command = Command::new("setarch");
        command.args(["x86_64", "-R"]).arg(path);
        command
    } else {
        Command::new(path)
    };
    let mut child = command
        .args(arguments)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();
    // Drain both pipes while the process runs. Per-cycle verification can
    // exceed a host's small pipe capacity without being a runtime deadlock.
    let collect = |mut pipe: Box<dyn Read + Send>| {
        std::thread::spawn(move || {
            let mut bytes = Vec::new();
            pipe.read_to_end(&mut bytes).unwrap();
            bytes
        })
    };
    let stdout = collect(Box::new(child.stdout.take().unwrap()));
    let stderr = collect(Box::new(child.stderr.take().unwrap()));
    let started = Instant::now();
    loop {
        if let Some(status) = child.try_wait().unwrap() {
            return Output {
                status,
                stdout: stdout.join().unwrap(),
                stderr: stderr.join().unwrap(),
            };
        }
        if started.elapsed() > Duration::from_secs(45) {
            let _ = child.kill();
            let _ = child.wait();
            let _ = stdout.join();
            let stderr = stderr.join().unwrap();
            panic!(
                "canonical backend runtime timed out: {}",
                String::from_utf8_lossy(&stderr)
            );
        }
        std::thread::sleep(Duration::from_millis(10));
    }
}

#[test]
fn canonical_remembered_backing_uses_real_old_runs_and_returns_its_budget() {
    let Some(clang) = find_clang() else {
        eprintln!("clang unavailable: remembered backing execution skipped");
        return;
    };
    let gc = runtime_directory();
    let scratch = Scratch::new();
    let binary = scratch.0.join("remembered-backing");
    let object = scratch.0.join("remembered.o");
    let main = scratch.0.join("main.o");
    compile_c(clang, &gc.join("klassic_gen_remembered.c"), &object);
    compile_c(clang, &gc.join("klassic_gen_remembered_test.c"), &main);
    link_program(clang, &gc, &main, &[object], None, &binary);
    let output = run_bounded(&binary);
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(
        String::from_utf8_lossy(&output.stdout)
            .contains("budget and private target cancellation PASS")
    );
}
#[test]
fn canonical_public_c_runtime_preserves_aliases_cycles_and_large_allocations() {
    let Some(clang) = find_clang() else {
        eprintln!("clang unavailable: public C runtime execution skipped");
        return;
    };
    let gc = runtime_directory();
    let scratch = Scratch::new();
    let objects = runtime_objects(clang, &gc, &scratch.0);
    let main = scratch.0.join("public-main.o");
    compile_c(clang, &gc.join("klassic_gen_runtime_test.c"), &main);
    let binary = scratch.0.join("public-runtime");
    link_program(clang, &gc, &main, &objects, None, &binary);
    for (name, args, marker) in [
        ("pressure", &[][..], "canonical C runtime:"),
        ("stress", &["stress"][..], "canonical C runtime stress:"),
    ] {
        eprintln!("executing public canonical C runtime/{name}");
        let output = run_bounded_args(&binary, args);
        assert!(
            output.status.success(),
            "public C runtime/{name}: status={} stderr={}",
            output.status,
            String::from_utf8_lossy(&output.stderr)
        );
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(
            stdout.contains(marker) && stdout.contains("PASS"),
            "public C runtime/{name}: {stdout}"
        );
    }
}
#[test]
fn canonical_c_os_failures_preserve_owned_receipts_and_retry_actual_operations() {
    let Some(clang) = find_clang() else {
        return;
    };
    let gc = runtime_directory();
    let scratch = Scratch::new();
    let main = scratch.0.join("retry.o");
    let binary = scratch.0.join("retry");
    // This diagnostic TU includes the real implementation to inspect private
    // receipts; linking another copy would duplicate runtime state.
    compile_c(clang, &gc.join("klassic_gen_os_retry_test.c"), &main);
    link_program(clang, &gc, &main, &[], None, &binary);
    for args in [
        &[][..],
        &["minimum"][..],
        &["after-effect"][..],
        &["minimum-after-effect"][..],
    ] {
        let output = run_bounded_args(&binary, args);
        assert_no_sanitizer_failure(&output);
        assert!(
            output.status.success(),
            "{args:?}: {}",
            String::from_utf8_lossy(&output.stderr)
        );
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(
            stdout.contains("canonical C native OS failure retry:") && stdout.contains("PASS"),
            "{stdout}"
        );
    }
    let output = run_bounded_args(&binary, &["permanent"]);
    assert_no_sanitizer_failure(&output);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        !output.status.success()
            && stderr.contains("32 attempts, original mapping and budget remain owned")
            && stderr.contains("metadata retirement OS retry limit"),
        "{}: {stderr}",
        output.status
    );
}

#[test]
fn canonical_c_minimum_backing_and_verification_validate_real_heap_state() {
    let Some(clang) = find_clang() else {
        return;
    };
    let gc = runtime_directory();
    let scratch = Scratch::new();
    let main = scratch.0.join("policy.o");
    let binary = scratch.0.join("policy");
    compile_c(clang, &gc.join("klassic_gen_policy_test.c"), &main);
    link_program(clang, &gc, &main, &[], None, &binary);
    for mode in ["minimum", "rounded"] {
        let output = run_bounded_args(&binary, &[mode]);
        assert_no_sanitizer_failure(&output);
        assert!(
            output.status.success(),
            "{mode}: {}",
            String::from_utf8_lossy(&output.stderr)
        );
        assert!(
            String::from_utf8_lossy(&output.stdout)
                .contains("root/heap/forward verification and shutdown PASS")
        );
    }
    for mode in [
        "corrupt-root",
        "corrupt-field",
        "corrupt-forward",
        "invalid-minimum",
    ] {
        let output = run_bounded_args(&binary, &[mode]);
        assert_no_sanitizer_failure(&output);
        let stderr = String::from_utf8_lossy(&output.stderr);
        let marker = if mode == "invalid-minimum" {
            "minimum heap exceeds hard maximum"
        } else {
            "requested heap verification"
        };
        assert!(
            !output.status.success() && stderr.contains(marker),
            "{mode}: {}: {stderr}",
            output.status
        );
    }
}

#[test]
fn canonical_c_old_mark_retains_children_of_floating_physical_young_objects() {
    let Some(clang) = find_clang() else {
        return;
    };
    let gc = runtime_directory();
    let scratch = Scratch::new();
    let main = scratch.0.join("old-young.o");
    let binary = scratch.0.join("old-young");
    compile_c(clang, &gc.join("klassic_gen_old_young_test.c"), &main);
    link_program(clang, &gc, &main, &[], None, &binary);
    let output = run_bounded(&binary);
    assert_no_sanitizer_failure(&output);
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(
        String::from_utf8_lossy(&output.stdout)
            .contains("actual Old handoff, retention and rootless shutdown PASS")
    );
    let output = run_bounded_args(&binary, &["cancel-reader"]);
    assert_no_sanitizer_failure(&output);
    assert!(
        !output.status.success()
            && String::from_utf8_lossy(&output.stderr)
                .contains("private cleanup still has driver readers"),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
}

#[test]
fn canonical_c_utf8_handles_output_aliases_and_rejects_invalid_encoding() {
    let Some(clang) = find_clang() else {
        return;
    };
    let gc = runtime_directory();
    let scratch = Scratch::new();
    let objects = runtime_objects(clang, &gc, &scratch.0);
    let main = scratch.0.join("utf8.o");
    compile_c(
        clang,
        &Path::new(env!("CARGO_MANIFEST_DIR")).join("runtime/gc/klassic_gen_string_test.c"),
        &main,
    );
    let binary = scratch.0.join("utf8");
    link_program(clang, &gc, &main, &objects, None, &binary);
    let output = run_bounded(&binary);
    assert_no_sanitizer_failure(&output);
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(
        output
            .stdout
            .starts_with(&[0xe7, 0x81, 0xaf, 0xf0, 0x9f, 0x98, 0x80, 0, b'\n', b'\n'])
    );
    assert!(
        String::from_utf8_lossy(&output.stdout)
            .contains("aliased outputs, Unicode/NUL, actual GC and native output PASS")
    );
    for mode in ["truncated", "surrogate", "overlong"] {
        let output = run_bounded_args(&binary, &[mode]);
        assert_no_sanitizer_failure(&output);
        assert!(
            !output.status.success() && String::from_utf8_lossy(&output.stderr).contains("UTF-8"),
            "{}",
            String::from_utf8_lossy(&output.stderr)
        );
    }
}

#[test]
fn canonical_c_delayed_uncommit_observes_real_free_lifetimes_and_native_preparation() {
    let Some(clang) = find_clang() else {
        return;
    };
    let gc = runtime_directory();
    let scratch = Scratch::new();
    let main = scratch.0.join("delay.o");
    let binary = scratch.0.join("delay");
    compile_c(clang, &gc.join("klassic_gen_uncommit_delay_test.c"), &main);
    link_program(clang, &gc, &main, &[], None, &binary);
    for mode in ["deterministic", "wall", "race"] {
        let output = run_bounded_args(&binary, &[mode]);
        assert_no_sanitizer_failure(&output);
        assert!(output.status.success(), "{mode}: {output:?}");
        assert!(
            String::from_utf8_lossy(&output.stdout)
                .contains("actual clock/FREE lifetime/minimum/OS retry/shutdown PASS")
        );
    }
    let output = run_bounded_args(&binary, &["permanent"]);
    assert_no_sanitizer_failure(&output);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        !output.status.success()
            && stderr.contains("32 callbacks retained actual backing/drop ownership")
            && stderr.contains("backing and sweep claim remain owned"),
        "{output:?}"
    );
}

fn assert_no_sanitizer_failure(output: &Output) {
    let stderr = String::from_utf8_lossy(&output.stderr);
    for marker in [
        "WARNING: ThreadSanitizer",
        "ERROR: AddressSanitizer",
        "runtime error:",
    ] {
        assert!(
            !stderr.contains(marker),
            "unexpected sanitizer failure: {stderr}"
        );
    }
}

#[test]
fn canonical_native_worker_allocation_keeps_real_tlab_and_cleanup_ownership() {
    let clang = find_clang().expect("C compiler required");
    let gc = runtime_directory();
    let scratch = Scratch::new();
    let main = scratch.0.join("native-worker.o");
    let binary = scratch.0.join("native-worker");
    compile_c(clang, &gc.join("gc_native_worker_allocation_test.c"), &main);
    link_program(clang, &gc, &main, &[], None, &binary);
    let output = run_bounded(&binary);
    assert_no_sanitizer_failure(&output);
    assert!(output.status.success(), "{output:?}");
    assert!(String::from_utf8_lossy(&output.stdout).contains("full resource disposal PASS"));
}
