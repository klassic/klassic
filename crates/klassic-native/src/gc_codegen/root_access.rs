//! Bounded rooted forwarding or canonical guard acquisition; gc_root_access.h.
use super::*;
use crate::gc_abi::{RootAccess, RootFollow, RootRequest};

fn call<A: PortableAsm>(b: &mut Emit<'_, A>, label: A::TextLabel, g: E, q: i32, op: u64) {
    b.value(g);
    b.a.mov_reg_reg(R::V7, R::V0);
    b.a.mov_imm64(R::V2, op);
    b.a.load_rbp_slot(R::V6, q);
    b.a.call_label(label);
    b.a.store_rbp_slot(PREV, R::V0);
}

#[allow(dead_code)]
pub(crate) fn emit_root_access<A: PortableAsm>(a: &mut A) {
    const {
        assert!(std::mem::size_of::<RootAccess>() == 128);
    }
    let status: [_; 9] = std::array::from_fn(|_| a.create_text_label());
    let root = a.create_text_label();
    let acquire = a.create_text_label();
    let metadata = a.create_text_label();
    let resolve = a.create_text_label();
    let forwarded = a.create_text_label();
    let release = a.create_text_label();
    let done = a.create_text_label();
    let after = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    for bytes in [112, 80] {
        a.sub_reg_imm8(R::V4, bytes);
    }
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    for (slot, offset) in [(REG, 152), (SEG, 184)] {
        a.mov_reg_reg(R::V0, R::V5);
        a.sub_reg_imm32(R::V0, offset);
        a.store_rbp_slot(slot, R::V0);
    }
    let mut b = Emit { a };
    b.pointer(local(Q), status[1]);
    b.set(
        local(NEXT),
        add(local(Q), n(offset_of!(RootAccess, access) as u64)),
    );
    for off in (0..56).step_by(8) {
        b.branch(field(NEXT, off, 0), n(0), C::NotEqual, status[1]);
    }
    b.set(
        local(KIND),
        add(local(Q), n(offset_of!(RootFollow, cleanup) as u64)),
    );
    for off in [0, 8, 16] {
        b.branch(field(KIND, off, 0), n(0), C::NotEqual, status[7]);
    }
    b.set(local(CTX), field(Q, offset_of!(RootFollow, context), 0));
    b.pointer(local(CTX), status[1]);
    b.branch(
        field(
            Q,
            offset_of!(RootFollow, root) + offset_of!(RootRequest, value),
            0,
        ),
        n(0),
        C::NotEqual,
        status[1],
    );
    for off in (0..40).step_by(8) {
        b.set(
            field(REG, off, 0),
            field(Q, offset_of!(RootFollow, root) + off, 0),
        );
    }
    call(&mut b, root, local(CTX), REG, 3);
    b.branch(local(PREV), n(0), C::NotEqual, status[1]);
    b.branch(field(REG, 32, 0), n(0), C::Equal, status[0]);
    b.set(field(SEG, 0, 0), field(REG, 32, 0));
    for off in [8, 16, 24] {
        b.set(field(SEG, off, 0), n(0));
    }
    call(&mut b, acquire, local(G), SEG, 0);
    b.branch(local(PREV), n(2), C::Equal, status[5]);
    b.branch(local(PREV), n(3), C::Equal, status[6]);
    b.branch(local(PREV), n(0), C::NotEqual, status[1]);
    for off in [0, 8, 16] {
        b.set(field(KIND, off, 0), field(SEG, off + 8, 0));
    }
    b.set(field(NEXT, 0, 0), field(REG, 32, 0));
    b.set(field(NEXT, 16, 0), local(CTX));
    call(&mut b, resolve, field(KIND, 0, 0), NEXT, 0);
    b.branch(local(PREV), n(0), C::Equal, status[8]);
    b.branch(local(PREV), n(3), C::Equal, forwarded);
    let wait = b.a.create_text_label();
    let exhausted = b.a.create_text_label();
    b.branch(local(PREV), n(2), C::Equal, wait);
    b.branch(local(PREV), n(4), C::Equal, exhausted);
    b.set(local(STATE), n(1));
    b.a.jmp_label(release);
    b.a.bind_text_label(forwarded);
    b.set(field(REG, 32, 0), field(NEXT, 8, 0));
    call(&mut b, root, local(CTX), REG, 1);
    let advanced = b.a.create_text_label();
    let flush = b.a.create_text_label();
    b.branch(local(PREV), n(0), C::Equal, advanced);
    b.branch(local(PREV), n(3), C::Equal, flush);
    b.branch(local(PREV), n(4), C::Equal, exhausted);
    b.set(local(STATE), n(1));
    b.a.jmp_label(release);
    for (label, value) in [(wait, 2), (exhausted, 6), (advanced, 3), (flush, 4)] {
        b.a.bind_text_label(label);
        b.set(local(STATE), n(value));
        b.a.jmp_label(release);
    }
    b.a.bind_text_label(release);
    for off in (0..56).step_by(8) {
        b.set(field(NEXT, off, 0), n(0));
    }
    call(&mut b, metadata, local(G), KIND, 1);
    b.branch(local(PREV), n(0), C::NotEqual, status[7]);
    b.a.load_rbp_slot(R::V0, STATE);
    b.a.jmp_label(done);
    for (value, label) in status.into_iter().enumerate() {
        b.a.bind_text_label(label);
        b.a.mov_imm64(R::V0, value as u64);
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.jmp_label(after);
    b.a.bind_text_label(root);
    crate::gc_codegen::roots::emit_step(b.a);
    b.a.ret();
    b.a.bind_text_label(acquire);
    super::reference_lease::emit_reference_lease(b.a);
    b.a.ret();
    b.a.bind_text_label(metadata);
    super::metadata::emit_metadata_step(b.a);
    b.a.ret();
    b.a.bind_text_label(resolve);
    super::resolve::emit_resolve_access(b.a);
    b.a.ret();
    b.a.bind_text_label(after);
}

/// V7=pool (RELEASE only), V6=RootAccess, V2=EXIT/RELEASE.
#[allow(dead_code)]
pub(crate) fn emit_cleanup<A: PortableAsm>(a: &mut A) {
    use crate::gc_abi::{MetadataLeaseRequest as L, ResolveRequest as T, SourceAccessRequest as S};
    let invalid = a.create_text_label();
    let release = a.create_text_label();
    let exited = a.create_text_label();
    let failed_release = a.create_text_label();
    let done = a.create_text_label();
    let after = a.create_text_label();
    let resolve = a.create_text_label();
    let metadata = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    a.store_rbp_slot(OP, R::V2);
    let mut b = Emit { a };
    b.branch(local(OP), n(1), C::Above, invalid);
    b.pointer(local(Q), invalid);
    b.set(local(REG), field(Q, offset_of!(RootFollow, context), 0));
    b.pointer(local(REG), invalid);
    b.set(
        local(NEXT),
        add(local(Q), n(offset_of!(RootAccess, access) as u64)),
    );
    b.set(
        local(KIND),
        add(local(Q), n(offset_of!(RootFollow, cleanup) as u64)),
    );
    b.pointer(field(KIND, offset_of!(L, page), 0), invalid);
    b.branch(
        field(KIND, offset_of!(L, incarnation), 0),
        n(0),
        C::Equal,
        invalid,
    );
    b.branch(
        and(field(KIND, offset_of!(L, token), 0), n(u32::MAX as u64)),
        n(0),
        C::Equal,
        invalid,
    );
    b.branch(
        shr(field(KIND, offset_of!(L, token), 0), 32),
        n(0),
        C::Equal,
        invalid,
    );
    b.branch(
        field(REG, offset_of!(MutatorContext, abi_version), 4),
        n(crate::gc_abi::ABI_VERSION as u64),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(REG, offset_of!(MutatorContext, thread_id), 0),
        n(0),
        C::Equal,
        invalid,
    );
    b.branch(local(OP), n(1), C::Equal, release);
    b.branch(
        field(NEXT, offset_of!(T, scope) + offset_of!(S, context), 0),
        local(REG),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(NEXT, offset_of!(T, scope) + offset_of!(S, incarnation), 0),
        field(KIND, offset_of!(L, incarnation), 0),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(NEXT, offset_of!(T, scope) + offset_of!(S, held), 0),
        n(0),
        C::Equal,
        invalid,
    );
    call(
        &mut b,
        resolve,
        field(KIND, offset_of!(L, page), 0),
        NEXT,
        1,
    );
    b.branch(local(PREV), n(0), C::Equal, exited);
    b.value(local(PREV));
    b.a.jmp_label(done);
    b.a.bind_text_label(exited);
    for off in (0..std::mem::size_of::<T>()).step_by(8) {
        b.set(field(NEXT, off, 0), n(0));
    }
    b.a.mov_imm64(R::V0, 0);
    b.a.jmp_label(done);
    b.a.bind_text_label(release);
    b.pointer(local(G), invalid);
    for off in (0..std::mem::size_of::<T>()).step_by(8) {
        b.branch(field(NEXT, off, 0), n(0), C::NotEqual, invalid);
    }
    b.branch(
        field(REG, offset_of!(MutatorContext, access_epoch), 8),
        n(0),
        C::NotEqual,
        invalid,
    );
    call(&mut b, metadata, local(G), KIND, 1);
    b.branch(local(PREV), n(0), C::NotEqual, failed_release);
    b.a.mov_imm64(R::V0, 0);
    b.a.jmp_label(done);
    b.a.bind_text_label(failed_release);
    b.a.mov_imm64(R::V0, 7);
    b.a.jmp_label(done);
    b.a.bind_text_label(invalid);
    b.a.mov_imm64(R::V0, 1);
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.jmp_label(after);
    b.a.bind_text_label(resolve);
    super::resolve::emit_resolve_access(b.a);
    b.a.ret();
    b.a.bind_text_label(metadata);
    super::metadata::emit_metadata_step(b.a);
    b.a.ret();
    b.a.bind_text_label(after);
}
