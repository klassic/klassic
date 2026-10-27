//! Canonical RootCell LLVM lowering. This suite separately verifies emitted
//! IR and executes it against the real shared C adapter when linked below.
#![cfg(all(target_os = "linux", target_arch = "x86_64"))]
use klassic_native::{
    GenerationalBackendConfig, GenerationalModuleSource, compile_source_to_llvm_generational,
    compile_source_to_llvm_generational_with_modules,
};
use std::path::PathBuf;
use std::process::Command;
use std::sync::atomic::{AtomicU64, Ordering};
struct Scratch(PathBuf);
impl Scratch {
    fn new() -> Self {
        static NEXT: AtomicU64 = AtomicU64::new(0);
        let path = std::env::temp_dir().join(format!(
            "klassic-gen-llvm-{}-{}-{}",
            std::process::id(),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos(),
            NEXT.fetch_add(1, Ordering::Relaxed)
        ));
        std::fs::create_dir(&path).unwrap();
        Self(path)
    }
}
impl Drop for Scratch {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.0);
    }
}
fn clang() -> Option<String> {
    std::env::var("KLASSIC_CLANG").ok().or_else(|| {
        ["clang-18", "clang-17", "clang-16", "clang-15", "clang"]
            .into_iter()
            .find(|name| {
                Command::new(name)
                    .arg("--version")
                    .output()
                    .is_ok_and(|o| o.status.success())
            })
            .map(str::to_owned)
    })
}
fn module(source: &str) -> String {
    let ir = compile_source_to_llvm_generational(
        "canonical-test",
        source,
        GenerationalBackendConfig::default(),
    )
    .unwrap();
    assert!(
        !ir.contains("@klassic_gc_"),
        "legacy collector must not leak into canonical lowering"
    );
    assert!(!ir.contains("@gc_bad_mask") && !ir.contains("@gc_strip_mask"));
    assert!(
        !ir.contains("inttoptr") && !ir.contains("getelementptr i8"),
        "no raw managed address may escape facade operations"
    );
    let clang = clang().expect("Clang is required for LLVM object validation");
    let s = Scratch::new();
    let path = s.0.join("program.ll");
    std::fs::write(&path, &ir).unwrap();
    let out = Command::new(clang)
        .args(["-O2", "-Werror", "-Wno-override-module", "-c"])
        .arg(&path)
        .arg("-o")
        .arg(s.0.join("program.o"))
        .output()
        .unwrap();
    assert!(
        out.status.success(),
        "invalid LLVM IR:\n{}\n{ir}",
        String::from_utf8_lossy(&out.stderr)
    );
    ir
}

#[test]
fn canonical_llvm_runs_module_capture_with_the_real_collector() {
    let clang = clang().expect("Clang is required for LLVM runtime validation");
    let scratch = Scratch::new();
    let ir = compile_source_to_llvm_generational_with_modules(
        "collector",
        "import samples.base as M\nimport samples.base as N\nval r = record { x: M#answer() + N#number - 41 }\nmutable i = 0\nwhile (i < 20) { val t = record { x: i }; i += 1 }\nprintln(r.x)\nprintln(0.2F + 0.1F)\n",
        &[GenerationalModuleSource::new("samples.base", "base.kl", "module samples.base\nprintln(\"module\")\nval number: Int = 41\ndef count(n: Int): Int = if (n <= 0) number else count(n - 1) + 1\ndef answer(): Int = count(1)\n")],
        GenerationalBackendConfig { flags: 1, ..Default::default() },
    ).unwrap();
    let source = scratch.0.join("program.ll");
    let object = scratch.0.join("program.o");
    let executable = scratch.0.join("program");
    std::fs::write(&source, ir).unwrap();
    let sanitizer = std::env::var_os("KLASSIC_GEN_SANITIZE").map(|mode| match mode.to_str() {
        Some("address") => "address,undefined",
        Some("thread") => "thread",
        _ => panic!("unknown sanitizer mode"),
    });
    let mut ir_compile = Command::new(&clang);
    if let Some(sanitizer) = sanitizer {
        ir_compile.arg(format!("-fsanitize={sanitizer}"));
    }
    let compiled = ir_compile
        .args(["-O1", "-Werror", "-Wno-override-module", "-c"])
        .arg(&source)
        .arg("-o")
        .arg(&object)
        .output()
        .unwrap();
    assert!(
        compiled.status.success(),
        "{}",
        String::from_utf8_lossy(&compiled.stderr)
    );
    let gc = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("runtime/gc");
    let runtime = std::path::Path::new(env!("CARGO_BIN_EXE_klassic"))
        .parent()
        .unwrap()
        .join("libklassic_runtime.a");
    assert!(
        runtime.is_file(),
        "build the workspace to produce the runtime archive"
    );
    let mut command = Command::new(clang);
    if let Some(sanitizer) = sanitizer {
        command
            .arg(format!("-fsanitize={sanitizer}"))
            .args(["-fno-omit-frame-pointer", "-no-pie"]);
    }
    command
        .args(["-std=c11", "-D_DEFAULT_SOURCE", "-O1", "-pthread"])
        .arg(object)
        .arg("-I")
        .arg(&gc);
    for unit in [
        "klassic_gen_runtime",
        "klassic_gen_remembered",
        "klassic_gen_policy",
        "klassic_gen_string",
        "klassic_gen_scalar",
        "klassic_gen_verify",
        "gc_native_driver",
    ] {
        command.arg(gc.join(format!("{unit}.c")));
    }
    let build = command
        .arg(runtime)
        .args(["-ldl", "-lm", "-o"])
        .arg(&executable)
        .output()
        .unwrap();
    assert!(
        build.status.success(),
        "{}",
        String::from_utf8_lossy(&build.stderr)
    );
    use std::io::Read;
    use std::process::Stdio;
    use std::time::{Duration, Instant};
    let mut child = Command::new(executable)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();
    let mut stdout = child.stdout.take().unwrap();
    let mut stderr = child.stderr.take().unwrap();
    let out = std::thread::spawn(move || {
        let mut b = Vec::new();
        stdout.read_to_end(&mut b).unwrap();
        b
    });
    let err = std::thread::spawn(move || {
        let mut b = Vec::new();
        stderr.read_to_end(&mut b).unwrap();
        b
    });
    let deadline = Instant::now() + Duration::from_secs(60);
    let status = loop {
        if let Some(status) = child.try_wait().unwrap() {
            break status;
        }
        if Instant::now() >= deadline {
            child.kill().unwrap();
            child.wait().unwrap();
            panic!("canonical LLVM program timed out");
        }
        std::thread::sleep(Duration::from_millis(10));
    };
    let stdout = out.join().unwrap();
    let stderr = err.join().unwrap();
    assert!(status.success(), "{}", String::from_utf8_lossy(&stderr));
    assert_eq!(stdout, b"module\n42\n0.3\n");
}
#[test]
fn canonical_records_use_rooted_allocation_and_location_access() {
    let ir = module(
        "val r = record { x: 42; flag: true; d: 3.5 }\nprintln(r.x)\nprintln(r.flag)\nprintln(r.d)",
    );
    assert!(ir.contains("call void @kg_alloc_into(i64 24, i64 2"));
    assert!(ir.contains("call void @kg_load_ref_into"));
    assert!(ir.contains("call i64 @kg_load_i64"));
}
#[test]
fn branch_block_record_results_and_assignment_stay_rooted() {
    module(
        "mutable r = if (true) { val a = record { x: 11 }; a } else record { x: 99 }\nprintln(r.x)\nr = if (false) record { x: 44 } else { val b = record { x: 22 }; b }\nprintln(r.x)",
    );
}
#[test]
fn loop_sites_reuse_entry_roots_and_functions_release_their_frames() {
    let ir = module(
        "def sumRec(a: Int, b: Int): Int = { val r = record { x: a; y: b }; r.x + r.y }\nmutable i = 0\nmutable n = 0\nwhile (i < 20) { n = n + sumRec(i, 1); i = i + 1 }\nprintln(n)",
    );
    assert!(ir.contains("call void @kg_root_release"));
    for line in ir.lines().filter(|l| l.contains("call ptr @kg_root_new")) {
        assert!(!line.contains("@klassic_gc"));
    }
}
#[test]
fn canonical_logical_rhs_with_allocation_is_control_dependent() {
    let ir = module(
        "def probe(n: Int): Bool = { val r = record { x: n }; 1 / r.x > 0 }\nprintln(false && probe(0))\nprintln(true || probe(0))",
    );
    assert!(ir.contains("logicrhs") && ir.contains("phi i1"));
    assert!(!ir.contains(" = and i1") && !ir.contains(" = or i1"));
}
#[test]
fn canonical_controls_are_carried_and_unknown_controls_are_diagnosed() {
    assert!(
        compile_source_to_llvm_generational(
            "unit-parameter",
            "def f(u: Unit): Int = 1\nprintln(1)",
            Default::default()
        )
        .is_err()
    );
    let ir = compile_source_to_llvm_generational(
        "config",
        "println(1)",
        GenerationalBackendConfig {
            uncommit_delay_ns: None,
            hard_max: 16 << 20,
            min_heap: 128 << 10,
            soft_max: 8 << 20,
            flags: 6,
        },
    )
    .unwrap();
    assert!(ir.contains("@kg_configure(i64 16777216, i64 131072, i64 8388608, i64 6)"));
    assert!(
        compile_source_to_llvm_generational(
            "invalid",
            "println(1)",
            GenerationalBackendConfig {
                flags: 8,
                ..Default::default()
            }
        )
        .is_err()
    );
    let ir = compile_source_to_llvm_generational(
        "strings",
        "def same(s: String): String = s\nprintln(same(\"x\") + \"あ\")",
        Default::default(),
    )
    .unwrap();
    assert!(
        ir.contains("@kg_string_from_utf8")
            && ir.contains("@kg_string_concat")
            && ir.contains("@kg_string_println")
    );
    assert!(ir.contains("ptr %kg_return_out"));
}
