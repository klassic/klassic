#![cfg(all(target_os = "linux", target_arch = "x86_64"))]

use std::process::Command;

#[test]
fn canonical_cli_emits_compilable_source_and_rejects_unavailable_executables() {
    let dir = std::env::temp_dir().join(format!(
        "klassic-cli-source-{}-{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ));
    std::fs::create_dir(&dir).unwrap();
    let input = dir.join("main.kl");
    std::fs::write(
        dir.join("values.kl"),
        "module values\nval number: Int = 42\n",
    )
    .unwrap();
    std::fs::write(&input, "import values as M\nprintln(M#number)\n").unwrap();
    let compiler = ["clang-18", "clang-17", "clang-16", "clang-15", "clang"]
        .into_iter()
        .find(|name| {
            Command::new(name)
                .arg("--version")
                .output()
                .is_ok_and(|o| o.status.success())
        })
        .expect("Clang is required for canonical source validation");
    for (backend, extension) in [("c", "c"), ("llvm", "ll")] {
        let source = dir.join(format!("output.{extension}"));
        let run = Command::new(env!("CARGO_BIN_EXE_klassic"))
            .args([
                "--gc=generational",
                "--gc-stress",
                "--backend",
                backend,
                "build",
            ])
            .arg(&input)
            .arg("-o")
            .arg(&source)
            .output()
            .unwrap();
        assert!(
            run.status.success(),
            "{}",
            String::from_utf8_lossy(&run.stderr)
        );
        let text = std::fs::read_to_string(&source).unwrap();
        assert!(text.contains("kg_init") && !text.contains("klassic_gc"));
        if backend == "c" {
            assert!(
                text.contains("kg_configure(UINT64_C(0), UINT64_C(0), UINT64_C(0), UINT64_C(1))")
            );
        } else {
            assert!(text.contains("call void @kg_configure(i64 0, i64 0, i64 0, i64 1)"));
        }
        let mut command = Command::new(compiler);
        command
            .args(["-Werror", "-Wno-override-module", "-c"])
            .arg(&source);
        if backend == "c" {
            command
                .arg("-I")
                .arg(std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("runtime/gc"));
        }
        let compile = command
            .arg("-o")
            .arg(dir.join("program.o"))
            .output()
            .unwrap();
        assert!(
            compile.status.success(),
            "{}",
            String::from_utf8_lossy(&compile.stderr)
        );
        let output = dir.join("existing.bin");
        std::fs::write(&output, b"keep").unwrap();
        let rejected = Command::new(env!("CARGO_BIN_EXE_klassic"))
            .args(["--gc=generational", "--backend", backend, "build"])
            .arg(&input)
            .arg("-o")
            .arg(&output)
            .output()
            .unwrap();
        assert!(!rejected.status.success());
        assert_eq!(std::fs::read(output).unwrap(), b"keep");
        std::fs::write(&source, b"keep source").unwrap();
        for flags in [
            &["--gc-poison"][..],
            &["--gc-log"][..],
            &["--gc-log-format=json"][..],
            &["--gc-max-heap=64MiB"][..],
            &["--gc-soft-max-heap=32MiB"][..],
            &["--gc-max-metadata=64MiB"][..],
            &["--gc-min-heap=16MiB"][..],
            &["--gc-uncommit-delay=1s"][..],
            &["--gc-pre-touch"][..],
            &["--gc-large-pages=off"][..],
            &["--target", "x86_64-pc-windows-msvc"][..],
        ] {
            let rejected = Command::new(env!("CARGO_BIN_EXE_klassic"))
                .args(["--gc=generational", "--backend", backend])
                .args(flags)
                .arg("build")
                .arg(&input)
                .arg("-o")
                .arg(&source)
                .output()
                .unwrap();
            assert!(!rejected.status.success(), "{backend} {flags:?}");
            assert_eq!(std::fs::read(&source).unwrap(), b"keep source");
        }
    }
    let foreign = dir.join("values.kl");
    std::fs::write(&foreign, "module values\nval number: Int = true\n").unwrap();
    for (backend, extension) in [("c", "c"), ("llvm", "ll")] {
        let output = dir.join(format!("bad.{extension}"));
        std::fs::write(&output, b"keep").unwrap();
        let rejected = Command::new(env!("CARGO_BIN_EXE_klassic"))
            .args(["--gc=generational", "--backend", backend, "build"])
            .arg(&input)
            .arg("-o")
            .arg(&output)
            .output()
            .unwrap();
        assert!(!rejected.status.success());
        assert!(String::from_utf8_lossy(&rejected.stderr).contains(&*foreign.to_string_lossy()));
        assert_eq!(std::fs::read(output).unwrap(), b"keep");
    }
    std::fs::remove_dir_all(dir).unwrap();
}
