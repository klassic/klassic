//! Execute ordinary scope-exit lowering with a root-retirement observer.
use super::{generator, run_c_driver};
use crate::portable_asm::{Condition, PortableAsm, Reg as P};

#[test]
fn gc_scope_retires_roots_before_reusing_stack_storage() {
    let mut g = generator();
    g.gc_shadow_pop = g.create_text_label();
    let mut entries = Vec::new();
    for roots in [1, 2, 8] {
        entries.push(g.asm.text.len());
        // Preserve Win64 nonvolatile argument registers around the fixture.
        g.push_reg(P::V7);
        g.push_reg(P::V6);
        g.push_reg(P::V5);
        g.mov_reg_reg(P::V5, P::V4);
        // Keep an outer scope's storage alive while the inner scope exits.
        g.sub_reg_imm8(P::V4, 16);
        g.next_stack_offset = 16;
        g.push_scope();
        g.mov_imm64(P::V1, 0x1510_1234_5678_9000);
        for _ in 0..roots {
            g.push_reg(P::V1);
            g.next_stack_offset += 8;
        }
        *g.scope_gc_root_counts.last_mut().unwrap() = roots;
        g.mov_reg_reg(P::V6, P::V4); // youngest root slot
        g.mov_imm64(P::V2, 0); // corrupted slots observed before retirement
        g.pop_scope(); // actual ordinary ELF/PE compiler lowering
        assert_eq!(g.next_stack_offset, 16);
        g.mov_reg_reg(P::V0, P::V2);
        g.leave();
        g.pop_reg(P::V6);
        g.pop_reg(P::V7);
        g.ret();
    }
    g.bind_text_label(g.gc_shadow_pop);
    // Observe each root immediately before unregistering it. CALL's return
    // address must not have overwritten any still-registered frame slot.
    g.load_ptr_disp32(P::V0, P::V6, 0);
    g.cmp_reg_reg(P::V0, P::V1);
    let intact = g.create_text_label();
    g.jcc_label(Condition::Equal, intact);
    g.add_reg_imm32(P::V2, 1);
    g.bind_text_label(intact);
    g.add_reg_imm32(P::V6, 8);
    g.ret();
    let mut object = g.asm.finish();
    for relocation in &object.relocs {
        let crate::RelocTarget::Text(label) = relocation.target else {
            panic!("unexpected non-text relocation");
        };
        assert!(matches!(relocation.kind, crate::RelocKind::Rel32));
        let delta = i32::try_from(object.text_labels[label.0] as i64 - (relocation.pos + 4) as i64)
            .unwrap();
        object.text[relocation.pos..relocation.pos + 4].copy_from_slice(&delta.to_le_bytes());
    }
    let bytes = object
        .text
        .iter()
        .map(u8::to_string)
        .collect::<Vec<_>>()
        .join(",");
    let offsets = entries
        .iter()
        .map(usize::to_string)
        .collect::<Vec<_>>()
        .join(",");
    let header = format!(
        "static const unsigned char code[] = {{{bytes}}};\nstatic const unsigned long entries[] = {{{offsets}}};\n"
    );
    let output = run_c_driver(
        "gc_scope_lifetime_code.h",
        &header,
        "scope_lifetime_harness.c",
    );
    assert!(
        output.contains("scope roots retain stack storage through retirement PASS"),
        "{output}"
    );
}
