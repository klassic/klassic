//! Ordinary scalar and image/stack-buffer console output publishes Native.
use super::*;
#[path = "native_console_linux.rs"]
mod linux;
const SOURCE: &str = r#"
enum Box { case Box(value: Int) }
val keep = Box(84)
mutable n = 40
def integer(v: Int): Int = v + 2
println(integer(n))
println(0)
println(9223372036854775807)
println(-9223372036854775808)
mutable yes = true
println(yes)
yes = false
println(yes)
println(2.5)
mutable d = 2.5
println(d + 0.25)
println(())
println(null)
println("literal")
println([integer(n), n])
println(keep match { case Box(v) => v })
"#;

#[test]
fn native_console_scalar_stack_and_rodata_writes_match_evaluator_on_all_direct_targets() {
    let expected = evaluate("native_console_eval", SOURCE);
    for target in [
        "x86_64-unknown-linux-gnu",
        "x86_64-pc-windows-msvc",
        "aarch64-apple-darwin",
    ] {
        for stress in [false, true] {
            let directory = temp_path("native_console", "dir");
            fs::create_dir(&directory).unwrap();
            let source = directory.join("source.kl");
            let image = directory.join("image.bin");
            fs::write(&source, SOURCE).unwrap();
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
            let compiled = build(&flags, &source, &image);
            assert!(
                compiled.status.success(),
                "{target}: {}",
                String::from_utf8_lossy(&compiled.stderr)
            );
            let outputs = if target == "x86_64-pc-windows-msvc" {
                let mut out = vec![windows_production::run(
                    &windows_production::driver(&directory),
                    &image,
                    0,
                )];
                if let Some(native) = windows_production::run_native_image(&directory, &image) {
                    out.push(native);
                }
                out
            } else if target == "aarch64-apple-darwin" {
                vec![
                    Command::new(aarch64_macho::emulator_python().unwrap())
                        .arg(
                            Path::new(env!("CARGO_MANIFEST_DIR"))
                                .join("tools/run-macho-aarch64.py"),
                        )
                        .arg(&image)
                        .env("KLASSIC_MACHO_REQUIRE_NATIVE_CONSOLE", "1")
                        .output()
                        .unwrap(),
                ]
            } else {
                vec![Command::new(&image).output().unwrap()]
            };
            for out in outputs {
                let stderr = String::from_utf8_lossy(&out.stderr);
                assert!(out.status.success(), "{target}: {stderr}");
                assert_eq!(out.stdout, expected.as_bytes(), "{target}: {stderr}");
                let stats = gc_stats(&stderr);
                assert_eq!(stats["native_copyout_chunks"], 0, "{target}: {stderr}");
                assert!(stats["native_buffer_writes"] >= 13);
                assert_eq!(stats["native_io_entries"], stats["native_buffer_writes"]);
                assert_eq!(stats["native_io_exits"], stats["native_buffer_writes"]);
                if target == "aarch64-apple-darwin" {
                    assert!(stderr.contains(&format!(
                        "native-console-model: writes={}\n",
                        stats["native_buffer_writes"]
                    )));
                }
            }
            fs::remove_dir_all(directory).unwrap();
        }
    }
}
