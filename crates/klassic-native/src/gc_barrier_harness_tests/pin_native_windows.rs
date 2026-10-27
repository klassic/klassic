//! A freestanding Clang Win64 callback embedded into a self-contained direct
//! PE. The identical image runs in the API model and the actual Windows loader.
use crate::{GcTestProgram, NativeCodeGenerator, NativeTarget, NativeTargetContext, SourceFile};
use std::{fmt::Write, path::Path, process::Command, sync::OnceLock};

static CALLBACK: OnceLock<Vec<u8>> = OnceLock::new();
fn callback_code() -> &'static [u8] {
    CALLBACK.get_or_init(|| {
        let directory =
            std::env::temp_dir().join(format!("klassic-pin-coff-{}", std::process::id()));
        std::fs::create_dir_all(&directory).unwrap();
        let manifest = Path::new(env!("CARGO_MANIFEST_DIR"));
        let object = directory.join("body.obj");
        let compiler = std::env::var("KLASSIC_CLANG").unwrap_or_else(|_| {
            ["clang-15", "clang"]
                .into_iter()
                .find(|cc| {
                    Command::new(cc)
                        .arg("--version")
                        .output()
                        .is_ok_and(|o| o.status.success())
                })
                .expect("Win64 C conformance requires Clang")
                .to_owned()
        });
        let compile = Command::new(compiler)
            .args([
                "--target=x86_64-pc-windows-msvc",
                "-std=c11",
                "-ffreestanding",
                "-O2",
                "-fno-stack-protector",
                "-fno-unwind-tables",
                "-fno-asynchronous-unwind-tables",
                "-fno-vectorize",
                "-fno-slp-vectorize",
                "-c",
            ])
            .arg(manifest.join("tests/pin_abi_freestanding_body.c"))
            .arg("-I")
            .arg(manifest.join("../../runtime/gc"))
            .arg("-o")
            .arg(&object)
            .output()
            .unwrap();
        assert!(
            compile.status.success(),
            "{}",
            String::from_utf8_lossy(&compile.stderr)
        );
        let bytes = std::fs::read(object).unwrap();
        let u16_at =
            |off: usize| u16::from_le_bytes(bytes[off..off + 2].try_into().unwrap()) as usize;
        let u32_at =
            |off: usize| u32::from_le_bytes(bytes[off..off + 4].try_into().unwrap()) as usize;
        assert_eq!(u16_at(0), 0x8664);
        assert_eq!(u16_at(16), 0);
        let mut code = None;
        for i in 0..u16_at(2) {
            let section = 20 + i * 40;
            if &bytes[section..section + 6] == b".text\0" {
                assert_eq!(
                    u16_at(section + 32),
                    0,
                    "embedded callback requires zero COFF relocations"
                );
                let size = u32_at(section + 16);
                let offset = u32_at(section + 20);
                assert!(size > 0 && offset + size <= bytes.len());
                code = Some(bytes[offset..offset + size].to_vec());
            }
        }
        let code = code.expect("Win64 C callback .text");
        std::fs::remove_dir_all(directory).unwrap();
        code
    })
}
fn body(g: &mut NativeCodeGenerator, mode: u64) {
    super::pin_abi::scope_body_with_callback(g, mode, Some(callback_code()));
}
fn image(mode: u64) -> Vec<u8> {
    let target = NativeTargetContext::for_target(NativeTarget::WindowsX86_64);
    let expr = klassic_syntax::parse_inline_expression("<native-win-pin>", "1").unwrap();
    let mut g = NativeCodeGenerator::new(
        SourceFile::new("<native-win-pin>", "1"),
        None,
        target.platform,
    );
    g.gc_generational = true;
    g.gc_max_heap = 16 << 20;
    g.gc_min_heap = 128 << 10;
    g.gc_test = Some(GcTestProgram {
        body,
        arg: mode,
        no_worker_thread: false,
    });
    crate::write_executable_for_target(target, g.compile(&expr).unwrap())
}
fn expected(mode: u64) -> String {
    format!(
        "result=77\nstage=5\naddress=0\nerror=0\ncalls=1\nretries={}\ncleanup_yields={}\nroots=0\nleases=0\nborrows=0\nadopted=1\n",
        u8::from(mode == 2),
        u8::from(mode == 1)
    )
}
#[test]
fn production_public_pin_scope_embeds_relocation_free_win64_c_in_complete_pe() {
    for mode in 0..4 {
        let image = image(mode);
        if let Some(directory) = std::env::var_os("KLASSIC_GC_PIN_NATIVE_WINDOWS_DIR") {
            let directory = std::path::PathBuf::from(directory);
            std::fs::create_dir_all(&directory).unwrap();
            std::fs::write(directory.join(format!("pin-{mode}.exe")), &image).unwrap();
            std::fs::write(
                directory.join(format!("pin-{mode}.expected")),
                expected(mode),
            )
            .unwrap();
            let script = include_str!("../../tests/pin_native_windows_runner.ps1");
            let mut bytes = vec![0xff, 0xfe];
            bytes.extend(script.encode_utf16().flat_map(u16::to_le_bytes));
            std::fs::write(directory.join("run-native.ps1"), bytes).unwrap();
        }
        let mut header = String::from(
            "#define GC_WINDOWS_PRODUCTION_FIXTURE 1\n#define GC_WINDOWS_PRODUCTION_MODE 0\nstatic const unsigned char production_fixture[]={",
        );
        for byte in image {
            write!(header, "{byte},").unwrap();
        }
        header.push_str("};\n");
        let output = crate::gc_atomic_tests::run_c_driver(
            "windows_production_fixture.h",
            &header,
            "windows_production_harness.c",
        );
        assert_eq!(output, expected(mode), "mode={mode}");
    }
}
