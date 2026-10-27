//! C-source lowering for the supported canonical scalar and record subset.
#![cfg(all(target_os = "linux", target_arch = "x86_64"))]
use klassic_native::{
    GenerationalBackendConfig, GenerationalModuleSource, compile_source_to_c_generational,
    compile_source_to_c_generational_with_modules,
};
use std::process::Command;
fn cc_available() -> bool {
    Command::new("cc")
        .arg("--version")
        .output()
        .is_ok_and(|o| o.status.success())
}
#[test]
fn canonical_c_scalar_source_initializes_and_polls_the_shared_adapter() {
    let source=compile_source_to_c_generational("scalar", "def inc(n: Int): Int = n + 1\nmutable i = 0\nwhile (i < 5) { i = inc(i) }\nprintln(i)\nprintln(1.5)\nprintln(1.0e999)", GenerationalBackendConfig {uncommit_delay_ns: None,hard_max:16<<20,min_heap:128<<10,soft_max:8<<20,flags:0}).unwrap();
    assert!(source.contains("#include \"klassic_gen_runtime.h\""));
    assert!(source.contains(
        "kg_configure(UINT64_C(16777216), UINT64_C(131072), UINT64_C(8388608), UINT64_C(0))"
    ));
    assert!(source.contains("for (;;) {\n        kg_poll();"));
    assert!(!source.contains("KStr") && !source.contains("klassic_gc"));
    assert!(cc_available(), "C compiler required for object validation");
    let dir = std::env::temp_dir().join(format!(
        "klassic-gen-c-{}-{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ));
    std::fs::create_dir(&dir).unwrap();
    let file = dir.join("program.c");
    std::fs::write(&file, &source).unwrap();
    let output = Command::new("cc")
        .args(["-std=c11", "-O2", "-Wall", "-Wextra", "-Werror", "-c"])
        .arg(&file)
        .arg("-I")
        .arg(std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("runtime/gc"))
        .arg("-o")
        .arg(dir.join("program.o"))
        .output()
        .unwrap();
    let _ = std::fs::remove_dir_all(dir);
    assert!(
        output.status.success(),
        "{}\n{source}",
        String::from_utf8_lossy(&output.stderr)
    );
}
#[test]
fn canonical_c_records_box_scalars_and_keep_root_sites_outside_loops() {
    let source = compile_source_to_c_generational("record", "def sumRec(a: Int, b: Int): Int = { val r = record { x: a; y: b }; r.x + r.y }\nmutable r = if (true) { val a = record { x: 11; flag: true; d: 1.5 }; a } else record { x: 99; flag: false; d: 2.5 }\nprintln(r.x)\nprintln(r.flag)\nprintln(r.d)\nr = record { x: 22; flag: false; d: 3.5 }\nmutable i = 0\nmutable n = 0\nwhile (i < 20) { n = n + sumRec(i, 1); i = i + 1 }\nprintln(n)\nprintln(r.x)", Default::default()).unwrap();
    assert!(source.contains("kg_alloc_into(UINT64_C(24), UINT64_C(2)"));
    assert!(source.contains("kg_load_ref_into") && source.contains("kg_store_i64"));
    assert!(!source.contains("klassic_gc") && !source.contains("KStr"));
    assert!(cc_available(), "C compiler required for object validation");
    let dir = std::env::temp_dir().join(format!(
        "klassic-gen-c-record-{}-{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ));
    std::fs::create_dir(&dir).unwrap();
    let path = dir.join("record.c");
    std::fs::write(&path, &source).unwrap();
    let output = Command::new("cc")
        .args(["-std=c11", "-O2", "-Wall", "-Wextra", "-Werror", "-c"])
        .arg(&path)
        .arg("-I")
        .arg(std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("runtime/gc"))
        .arg("-o")
        .arg(dir.join("record.o"))
        .output()
        .unwrap();
    let _ = std::fs::remove_dir_all(dir);
    assert!(
        output.status.success(),
        "{}\n{source}",
        String::from_utf8_lossy(&output.stderr)
    );
}
#[test]
fn canonical_c_never_falls_back_to_unmanaged_kstr_for_heap_strings() {
    assert!(
        compile_source_to_c_generational(
            "unit-parameter",
            "def f(u: Unit): Int = 1\nprintln(1)",
            Default::default()
        )
        .is_err()
    );
    let source = compile_source_to_c_generational(
        "strings",
        "def same(s: String): String = s\nprintln(same(\"x\") + \"あ\")",
        Default::default(),
    )
    .unwrap();
    assert!(
        source.contains("kg_string_from_utf8")
            && source.contains("kg_string_concat")
            && source.contains("kg_string_println")
    );
    assert!(!source.contains("KStr") && !source.contains("klassic_gc"));
    assert!(
        compile_source_to_c_generational("unsupported", "println(toString(1))", Default::default())
            .is_err()
    );
}

#[test]
fn canonical_c_runs_rooted_records_with_the_real_collector() {
    let dir = std::env::temp_dir().join(format!(
        "klassic-gen-c-exec-{}-{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ));
    std::fs::create_dir(&dir).unwrap();
    let source = compile_source_to_c_generational_with_modules(
        "collector",
        "import samples.base as M\nimport samples.base as N\nval r = record { x: M#answer() + N#number - 41 }\nmutable i = 0\nwhile (i < 20) { val t = record { x: i }; i += 1 }\nprintln(r.x)\nprintln(0.2F + 0.1F)\n",
        &[GenerationalModuleSource::new("samples.base", "base.kl", "module samples.base\nprintln(\"module\")\nval number: Int = 41\ndef count(n: Int): Int = if (n <= 0) number else count(n - 1) + 1\ndef answer(): Int = count(1)\n")],
        GenerationalBackendConfig { flags: 1, ..Default::default() },
    ).unwrap();
    let source_file = dir.join("program.c");
    let executable = dir.join("program");
    std::fs::write(&source_file, source).unwrap();
    let gc = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("runtime/gc");
    let runtime = std::path::Path::new(env!("CARGO_BIN_EXE_klassic"))
        .parent()
        .unwrap()
        .join("libklassic_runtime.a");
    assert!(
        runtime.is_file(),
        "build the workspace to produce the runtime archive"
    );
    let compiler = ["clang-18", "clang-17", "clang-16", "clang-15", "clang"]
        .into_iter()
        .find(|name| {
            Command::new(name)
                .arg("--version")
                .output()
                .is_ok_and(|o| o.status.success())
        })
        .expect("Clang is required for the real-runtime test");
    let mut command = Command::new(compiler);
    if let Some(mode) = std::env::var_os("KLASSIC_GEN_SANITIZE") {
        let sanitizer = match mode.to_str() {
            Some("address") => "address,undefined",
            Some("thread") => "thread",
            _ => panic!("unknown sanitizer mode"),
        };
        command
            .arg(format!("-fsanitize={sanitizer}"))
            .args(["-fno-omit-frame-pointer", "-no-pie"]);
    }
    command
        .args(["-std=c11", "-D_DEFAULT_SOURCE", "-O1", "-pthread"])
        .arg(&source_file)
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
        let mut bytes = Vec::new();
        stdout.read_to_end(&mut bytes).unwrap();
        bytes
    });
    let err = std::thread::spawn(move || {
        let mut bytes = Vec::new();
        stderr.read_to_end(&mut bytes).unwrap();
        bytes
    });
    let deadline = Instant::now() + Duration::from_secs(60);
    let status = loop {
        if let Some(status) = child.try_wait().unwrap() {
            break status;
        }
        if Instant::now() >= deadline {
            child.kill().unwrap();
            child.wait().unwrap();
            panic!("canonical C program timed out");
        }
        std::thread::sleep(Duration::from_millis(10));
    };
    let stdout = out.join().unwrap();
    let stderr = err.join().unwrap();
    assert!(status.success(), "{}", String::from_utf8_lossy(&stderr));
    assert_eq!(stdout, b"module\n42\n0.3\n");
    std::fs::remove_dir_all(dir).unwrap();
}

#[test]
fn canonical_c_reports_module_errors_at_their_source() {
    let module = GenerationalModuleSource::new(
        "samples.bad",
        "foreign.kl",
        "module samples.bad\nval number: Int = true\n",
    );
    let error = compile_source_to_c_generational_with_modules(
        "main.kl",
        "import samples.bad as B\nprintln(B#number)\n",
        &[module],
        Default::default(),
    )
    .unwrap_err();
    assert!(error.to_string().contains("foreign.kl"), "{error}");
}
