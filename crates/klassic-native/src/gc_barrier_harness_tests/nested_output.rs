//! Compile ordinary nested renderers with real Full movement at every copy.
use crate::gc_codegen::production::test_support::*;
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};
use crate::{NativeCodeGenerator, NativeTarget, NativeTargetContext, SourceFile};
use std::fmt::Write;

pub(crate) const SOURCE: &str = r#"
record Pair { left: String; right: String }
enum Pick<a> { case Both(left: a, right: a); case Missing }
enum Mono { case Wrap(left: String, right: String); case Stop }
def fresh(s: String): String = s + "!"
println([fresh("a"), fresh("b")])
println(#Pair(fresh("c"), fresh("d")))
println(Both(fresh("e"), fresh("f")))
println(Wrap(fresh("g"), fresh("h")))
println(%(fresh("i"), fresh("j")))
println(%["k!": fresh("l")])
println([[fresh("m"), fresh("n")], [fresh("o")]])
println([#Pair(fresh("p"), fresh("q")), #Pair(fresh("r"), fresh("s"))])
println(fresh("tail"))
"#;
pub(crate) const EXPECTED: &[u8] = b"[a!, b!]\n#Pair(c!, d!)\nBoth(e!, f!)\nWrap(g!, h!)\n%(i!, j!)\n%[k!: l!]\n[[m!, n!], [o!]]\n[#Pair(p!, q!), #Pair(r!, s!)]\ntail!\n";

pub(crate) fn lowered() -> crate::DesugaredEnums {
    crate::desugar_enums(
        klassic_syntax::parse_inline_expression("<nested-output>", SOURCE).unwrap(),
    )
}
fn body(g: &mut NativeCodeGenerator, _: u64) {
    let runtime = g.gc_gen.as_ref().unwrap().data.runtime;
    g.mov_data_addr(R::V3, runtime);
    g.load_ptr_disp32(R::V3, R::V3, 0);
    bind_primary_owner(g);
    let count = rooted_write_collect_between_chunks(g);
    let depth = g.asm.data_label_with_i64s(&[0]);
    rooted_write_depth(g);
    g.mov_data_addr(R::V1, depth);
    g.store_ptr_disp32(R::V1, 0, R::V0);
    g.gc_console_output = true;
    g.compile_top_level(&lowered().0).unwrap();
    g.gc_console_output = false;
    rooted_write_depth(g);
    g.mov_data_addr(R::V1, depth);
    g.load_ptr_disp32(R::V1, R::V1, 0);
    g.cmp_reg_reg(R::V0, R::V1);
    let failed = g.create_text_label();
    let done = g.create_text_label();
    g.jcc_label(C::NotEqual, failed);
    g.mov_data_addr(R::V1, count);
    g.load_ptr_disp32(R::V0, R::V1, 0);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, done);
    g.bind_text_label(failed);
    g.plat_exit(1);
    g.bind_text_label(done);
}

#[test]
fn production_nested_output_compiles_both_x86_targets_with_real_full_interleavings() {
    for target in [NativeTarget::LinuxX86_64, NativeTarget::WindowsX86_64] {
        for stress in [false, true] {
            let windows = target == NativeTarget::WindowsX86_64;
            let target = NativeTargetContext::for_target(target);
            let (expr, names, shapes, variants, _) = lowered();
            let mut g = NativeCodeGenerator::new(
                SourceFile::new("<nested-output>", SOURCE),
                None,
                target.platform,
            );
            g.lowered_enum_names = names;
            g.mono_enum_shapes = shapes;
            g.mono_enum_variants = variants;
            g.gc_stress = stress;
            g.gc_generational = true;
            g.gc_max_heap = 16 << 20;
            g.gc_min_heap = 128 << 10;
            g.gc_test = Some(crate::GcTestProgram {
                body,
                arg: 0,
                no_worker_thread: false,
            });
            let image = crate::write_executable_for_target(target, g.compile(&expr).unwrap());
            if windows {
                if let Some(directory) =
                    std::env::var_os("KLASSIC_GC_NESTED_OUTPUT_NATIVE_WINDOWS_DIR")
                {
                    let directory = std::path::PathBuf::from(directory);
                    std::fs::create_dir_all(&directory).unwrap();
                    let mode = u8::from(stress);
                    std::fs::write(directory.join(format!("nested-{mode}.exe")), &image).unwrap();
                    std::fs::write(directory.join(format!("nested-{mode}.expected")), EXPECTED)
                        .unwrap();
                    let script = include_str!("../../tests/pin_native_windows_runner.ps1")
                        .replace("pin-", "nested-")
                        .replace("$mode in 0..3", "$mode in 0..1")
                        .replace("native pin callback", "native nested output");
                    let mut encoded = vec![0xff, 0xfe];
                    encoded.extend(script.encode_utf16().flat_map(u16::to_le_bytes));
                    std::fs::write(directory.join("run-native.ps1"), encoded).unwrap();
                }
                let mut header = String::from(
                    "#define GC_WINDOWS_PRODUCTION_FIXTURE 1\n#define GC_WINDOWS_PRODUCTION_MODE 0\nstatic const unsigned char production_fixture[]={",
                );
                header.insert_str(
                    0,
                    &format!(
                        "#define GC_REQUIRE_NATIVE_CONSOLE_BORROWS {}\n",
                        native_buffer_borrow_offset()
                    ),
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
                assert_eq!(output.as_bytes(), EXPECTED);
            } else {
                let directory = std::env::temp_dir()
                    .join(format!("klassic-nested-output-{}", std::process::id()));
                std::fs::create_dir_all(&directory).unwrap();
                let path = directory.join("image");
                std::fs::write(&path, image).unwrap();
                use std::os::unix::fs::PermissionsExt;
                std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o700)).unwrap();
                let out = std::process::Command::new(&path).output().unwrap();
                assert!(
                    out.status.success(),
                    "{}",
                    String::from_utf8_lossy(&out.stderr)
                );
                assert_eq!(out.stdout, EXPECTED);
                std::fs::remove_dir_all(directory).unwrap();
            }
        }
    }
}
