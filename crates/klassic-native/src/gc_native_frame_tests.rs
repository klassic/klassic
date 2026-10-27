use super::{finish_code, generator, run_c_driver};
use crate::portable_asm::{PortableAsm, Reg as P};
use crate::{NativeCodeGenerator, Reg as R};

fn x86_code(windows: bool, locals: u32, backward: bool) -> (Vec<u8>, usize, usize) {
    let mut g = generator();
    g.is_windows = windows;
    let entry = g.create_text_label();
    let leaf = g.create_text_label();
    let emit_leaf = |g: &mut NativeCodeGenerator| {
        g.bind_text_label(leaf);
        let at = g.asm.text.len();
        g.native_entry_begin(0);
        g.mov_imm64(P::V0, 0x4c45_4146);
        g.native_entry_end();
        g.ret();
        at
    };
    let mut leaf_at = 0;
    if backward {
        leaf_at = emit_leaf(&mut g);
    }
    g.bind_text_label(entry);
    let entry_at = g.asm.text.len();
    g.native_entry_begin(locals);
    g.mov_reg_reg(P::V11, P::V7);
    g.store_ptr_disp32(P::V11, 0, P::V7);
    g.asm.mov_reg_reg(R::Rax, R::Rsp);
    g.store_ptr_disp32(P::V11, 8, P::V0);
    g.mov_text_addr(P::V0, leaf);
    g.store_ptr_disp32(P::V11, 16, P::V0);
    if locals != 0 {
        g.store_rbp_slot(8, P::V7);
    }
    for reg in [
        P::V3,
        P::OwnerContext,
        P::ColorStrip,
        P::GoodColor,
        P::BadMask,
        P::V7,
        P::V6,
    ] {
        g.mov_imm64(reg, 0);
    }
    for xmm in 6..16_u8 {
        g.asm.byte(0x66);
        if xmm >= 8 {
            g.asm.byte(0x45);
        }
        g.asm
            .bytes(&[0x0f, 0xef, 0xc0 | ((xmm & 7) << 3) | (xmm & 7)]);
    }
    if locals != 0 {
        g.load_rbp_slot(P::V11, 8);
    }
    g.mov_imm64(P::V0, 0x444f_4e45);
    g.store_ptr_disp32(P::V11, 24, P::V0);
    g.mov_imm64(P::V0, 0x1234_5678_9abc_def0);
    g.native_entry_end();
    g.ret();
    if !backward {
        leaf_at = emit_leaf(&mut g);
    }
    (finish_code(g), entry_at, leaf_at)
}

#[test]
fn native_frame_rejects_unaligned_and_unencodable_locals() {
    for windows in [false, true] {
        for locals in [1, 15, 4096, u32::MAX] {
            let mut g = generator();
            g.is_windows = windows;
            assert!(
                std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                    g.native_entry_begin(locals);
                }))
                .is_err()
            );
            assert!(g.asm.text.is_empty());
        }
    }
}

#[test]
fn native_frames_execute_from_c_and_export_cross_architecture_fixtures() {
    let export = std::env::var_os("KLASSIC_NATIVE_FRAME_FIXTURES").map(std::path::PathBuf::from);
    if let Some(dir) = &export {
        std::fs::create_dir_all(dir).unwrap();
    }
    let mut header = String::from(
        "typedef struct {const unsigned char *code; unsigned bytes,entry,leaf,locals,windows;} FrameFixture;\n",
    );
    let mut entries = String::new();
    let mut manifest = String::new();
    let mut count = 0;
    for arch in ["sysv", "win64", "arm64"] {
        for locals in [0, 16, 256, 4080] {
            for backward in [false, true] {
                let (code, entry, leaf) = if arch == "arm64" {
                    crate::aarch64::native_frame_fixture::code(locals, backward)
                } else {
                    x86_code(arch == "win64", locals, backward)
                };
                let name = format!("{arch}_{locals}_{}", u8::from(backward));
                manifest.push_str(&format!("{name}\t{arch}\t{locals}\t{entry}\t{leaf}\n"));
                if let Some(dir) = &export {
                    std::fs::write(dir.join(format!("{name}.bin")), &code).unwrap();
                }
                if (cfg!(target_arch = "aarch64") && arch == "arm64")
                    || (cfg!(target_arch = "x86_64") && arch != "arm64")
                {
                    let bytes = code.iter().map(u8::to_string).collect::<Vec<_>>().join(",");
                    header.push_str(&format!(
                        "static const unsigned char code_{count}[]={{{bytes}}};\n"
                    ));
                    entries.push_str(&format!(
                        "{{code_{count},sizeof(code_{count}),{entry},{leaf},{locals},{}}},\n",
                        u8::from(arch == "win64")
                    ));
                    count += 1;
                }
            }
        }
    }
    if let Some(dir) = &export {
        std::fs::write(dir.join("cases.tsv"), manifest).unwrap();
    }
    assert!(count > 0, "unsupported native conformance host");
    header.push_str(&format!(
        "static const FrameFixture fixtures[]={{{entries}}};\n"
    ));
    let stdout = run_c_driver("native_frame_code.h", &header, "native_frame_harness.c");
    assert!(stdout.contains("native frame C calls: PASS"), "{stdout}");
}
