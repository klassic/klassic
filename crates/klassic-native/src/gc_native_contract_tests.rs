//! Contract tests for finite leaves, before the direct owner dispatcher lands.
use super::{finish_code, generator, run_c_driver};
use crate::{NativeCodeGenerator, Reg};

fn wrapped(body: impl FnOnce(&mut NativeCodeGenerator)) -> Vec<u8> {
    let mut g = generator();
    let entry = g.asm.create_text_label();
    for r in [Reg::Rbx, Reg::R12, Reg::R13, Reg::R14, Reg::R15] {
        g.asm.push_reg(r);
    }
    // Fourth native argument is ordinary counter storage, never a managed ref.
    g.asm.mov_reg_reg(Reg::R12, Reg::Rcx);
    g.asm.call_label(entry);
    for r in [Reg::R15, Reg::R14, Reg::R13, Reg::R12, Reg::Rbx] {
        g.asm.pop_reg(r);
    }
    g.asm.ret();
    g.asm.bind_text_label(entry);
    body(&mut g);
    g.asm.ret();
    finish_code(g)
}

fn count(g: &mut NativeCodeGenerator, offset: i32) {
    g.asm.push_reg(Reg::Rax);
    g.asm.load_ptr_disp32(Reg::Rax, Reg::R12, offset);
    g.asm.add_reg_imm32(Reg::Rax, 1);
    g.asm.store_ptr_disp32(Reg::R12, offset, Reg::Rax);
    g.asm.pop_reg(Reg::Rax);
}

#[test]
fn shared_native_leaf_hooks_and_cleanup_quantum_execute() {
    let metadata = wrapped(|g| {
        crate::gc_codegen::root_lifetime::metadata::emit_metadata_step_tracked(
            g,
            &mut |g| {
                count(g, 0);
                g.asm.store_ptr_disp32(Reg::R12, 16, Reg::R8);
            },
            &mut |g| count(g, 8),
        );
    });
    let mut header = String::new();
    for (name, bytes) in std::iter::once(("metadata".to_owned(), metadata)).chain(
        [1, 7, 256].into_iter().map(|quantum| {
            (
                format!("clear_{quantum}"),
                wrapped(|g| crate::gc_codegen::root_lifetime::emit_clear_chunk_bounded(g, quantum)),
            )
        }),
    ) {
        let bytes = bytes
            .iter()
            .map(u8::to_string)
            .collect::<Vec<_>>()
            .join(",");
        header.push_str(&format!(
            "static const unsigned char {name}_code[] = {{{bytes}}};\n"
        ));
    }
    let stdout = run_c_driver(
        "gc_native_contract_code.h",
        &header,
        "native_contract_harness.c",
    );
    assert!(
        stdout.contains("shared native leaf hooks and finite cleanup: PASS"),
        "{stdout}"
    );
}
