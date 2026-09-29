//! Finite buffer publication / owned TLAB close for barrier rendezvous.
use super::*;
use crate::gc_abi::{
    AllocationPage as P, HeapPool as H, MetadataLeasePool as M, RootedStoreRuntime as D,
};
fn native<B: PortableAsm>(b: &mut Emit<'_, B>, offset: usize) {
    b.value(field(G, offset, 0));
    b.a.mov_reg_reg(R::V10, R::V0);
    b.value(field(G, offset_of!(D, heap_lock), 0));
    b.a.mov_reg_reg(R::V7, R::V0);
    b.a.call_native_unary(R::V10);
}
#[allow(dead_code)]
pub(crate) fn emit<B: PortableAsm>(a: &mut B) {
    let invalid = a.create_text_label();
    let done = a.create_text_label();
    let close = a.create_text_label();
    let release = a.create_text_label();
    let publish = a.create_text_label();
    let bind = a.create_text_label();
    let result = a.create_text_label();
    let call_publish = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(CTX, R::V6);
    a.store_rbp_slot(OP, R::V2);
    let mut b = Emit { a };
    b.pointer(local(G), invalid);
    b.pointer(local(CTX), invalid);
    b.branch(local(OP), n(83), C::Below, invalid);
    b.branch(local(OP), n(85), C::Above, invalid);
    b.branch(
        field(CTX, offset_of!(MutatorContext, abi_version), 4),
        n(crate::gc_abi::ABI_VERSION as u64),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(CTX, offset_of!(MutatorContext, thread_id), 0),
        n(0),
        C::Equal,
        invalid,
    );
    b.branch(
        field(CTX, offset_of!(MutatorContext, state), 4),
        n(0),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(CTX, offset_of!(MutatorContext, access_epoch), 8),
        n(0),
        C::NotEqual,
        invalid,
    );
    b.branch(local(OP), n(85), C::Equal, close);
    b.set(local(STATE), n(2));
    b.branch(local(OP), n(83), C::Equal, call_publish);
    b.set(local(STATE), n(3));
    b.a.bind_text_label(call_publish);
    b.a.load_rbp_slot(R::V7, CTX);
    b.a.mov_imm64(R::V6, 0);
    b.a.load_rbp_slot(R::V2, STATE);
    b.a.call_label(publish);
    b.a.cmp_reg_imm8(R::V0, 0);
    b.a.jcc_label(C::NotEqual, invalid);
    b.a.mov_imm64(R::V0, 82);
    b.a.jmp_label(done);
    b.a.bind_text_label(close);
    b.pointer(field(G, offset_of!(D, heap_pool), 0), invalid);
    for offset in [
        offset_of!(D, heap_lock),
        offset_of!(D, lock),
        offset_of!(D, unlock),
    ] {
        b.branch(field(G, offset, 0), n(0), C::Equal, invalid);
    }
    native(&mut b, offset_of!(D, lock));
    b.set(local(STATE), n(44));
    b.set(local(SEG), field(G, offset_of!(D, heap_pool), 0));
    b.set(local(SEG), field(SEG, offset_of!(M, heap), 0));
    b.pointer(local(SEG), release);
    b.set(local(REG), field(SEG, offset_of!(H, pages), 0));
    b.pointer(local(REG), release);
    b.set(local(USED), field(SEG, offset_of!(H, page_count), 0));
    b.branch(local(USED), n(0), C::Equal, release);
    b.branch(
        local(USED),
        n(crate::gc_abi::MAX_OFFSET >> 17),
        C::AboveOrEqual,
        release,
    );
    const {
        assert!(size_of::<P>() == 112);
    }
    b.set(
        local(NEXT),
        add(
            add(shl(local(USED), 6), shl(local(USED), 5)),
            shl(local(USED), 4),
        ),
    );
    b.branch(add(local(REG), local(NEXT)), local(REG), C::Below, release);
    b.set(
        local(PREV),
        field(CTX, offset_of!(MutatorContext, tlab_start), 0),
    );
    b.branch(
        local(PREV),
        field(SEG, offset_of!(H, heap_base), 0),
        C::Below,
        release,
    );
    b.set(
        local(PREV),
        sub(local(PREV), field(SEG, offset_of!(H, heap_base), 0)),
    );
    b.branch(
        and(local(PREV), n((1 << 17) - 1)),
        n(0),
        C::NotEqual,
        release,
    );
    b.set(local(NEXT), shr(local(PREV), 17));
    b.branch(local(NEXT), n(0), C::Equal, release);
    b.branch(local(NEXT), local(USED), C::Above, release);
    b.set(local(NEXT), sub(local(NEXT), n(1)));
    b.set(
        local(REG),
        add(
            local(REG),
            add(
                add(shl(local(NEXT), 6), shl(local(NEXT), 5)),
                shl(local(NEXT), 4),
            ),
        ),
    );
    b.branch(
        field(REG, offset_of!(P, heap_base), 0),
        field(SEG, offset_of!(H, heap_base), 0),
        C::NotEqual,
        release,
    );
    b.branch(
        field(REG, offset_of!(P, start), 0),
        local(PREV),
        C::NotEqual,
        release,
    );
    b.branch(
        field(REG, offset_of!(P, run_head), 0),
        local(NEXT),
        C::NotEqual,
        release,
    );
    b.branch(
        field(REG, offset_of!(P, limit), 0),
        shl(add(local(USED), n(1)), 17),
        C::Above,
        release,
    );
    b.a.load_rbp_slot(R::V7, CTX);
    b.a.load_rbp_slot(R::V6, REG);
    b.a.mov_imm64(R::V2, 1);
    b.a.call_label(bind);
    b.a.cmp_reg_imm8(R::V0, 0);
    b.a.jcc_label(C::NotEqual, release);
    b.set(local(STATE), n(82));
    b.a.bind_text_label(release);
    native(&mut b, offset_of!(D, unlock));
    b.value(local(STATE));
    b.a.jmp_label(result);
    b.a.bind_text_label(invalid);
    b.a.mov_imm64(R::V0, 44);
    b.a.bind_text_label(result);
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.ret();
    b.a.bind_text_label(publish);
    super::buffers::emit_buffer_step(b.a);
    b.a.ret();
    b.a.bind_text_label(bind);
    crate::gc_codegen::heap::emit_tlab_page_locked(b.a);
    b.a.ret();
}
