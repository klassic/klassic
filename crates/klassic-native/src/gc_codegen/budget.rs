//! Native metadata accounting under the heap lock; contract: gc_budget.h.
use super::*;
use crate::gc_abi::{AllocationPage, HeapPool, MetadataCharge};
macro_rules! heap {
    ($f:ident) => {
        field(G, offset_of!(HeapPool, $f), 0)
    };
}
macro_rules! charge {
    ($f:ident) => {
        field(Q, offset_of!(MetadataCharge, $f), 0)
    };
}

/// V7=heap,V6=unique charge,V2=reserve/commit/abort/release; status 0..2.
#[allow(dead_code)]
pub(crate) fn emit_budget_step<A: PortableAsm>(a: &mut A) {
    let status: [_; 3] = std::array::from_fn(|_| a.create_text_label());
    let invalid = status[1];
    let settle = a.create_text_label();
    let release = a.create_text_label();
    let abort = a.create_text_label();
    let unreserve = a.create_text_label();
    let done = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    a.store_rbp_slot(OP, R::V2);
    let mut b = Emit { a };
    b.branch(local(OP), n(3), C::Above, invalid);
    b.branch(heap!(page_count), n(0), C::Equal, invalid);
    b.branch(
        heap!(page_count),
        n(u64::MAX / std::mem::size_of::<AllocationPage>() as u64),
        C::Above,
        invalid,
    );
    b.value(heap!(page_count));
    b.a.mov_imm64(R::V1, std::mem::size_of::<AllocationPage>() as u64);
    b.a.mul_unsigned_wide(R::V1);
    b.a.store_rbp_slot(USED, R::V0);
    b.branch(heap!(metadata_committed), local(USED), C::Below, invalid);
    b.branch(
        heap!(metadata_committed),
        heap!(metadata_max),
        C::Above,
        invalid,
    );
    b.branch(
        heap!(metadata_pending),
        sub(heap!(metadata_max), heap!(metadata_committed)),
        C::Above,
        invalid,
    );
    b.branch(charge!(bytes), n(0), C::Equal, invalid);
    b.branch(local(OP), n(0), C::NotEqual, settle);
    for v in [charge!(owner), charge!(allocation), charge!(stage)] {
        b.branch(v, n(0), C::NotEqual, invalid);
    }
    b.branch(
        charge!(bytes),
        sub(
            sub(heap!(metadata_max), heap!(metadata_committed)),
            heap!(metadata_pending),
        ),
        C::Above,
        status[2],
    );
    b.set(
        heap!(metadata_pending),
        add(heap!(metadata_pending), charge!(bytes)),
    );
    b.set(charge!(owner), local(G));
    b.set(charge!(stage), n(1));
    b.a.jmp_label(status[0]);
    b.a.bind_text_label(settle);
    b.branch(charge!(owner), local(G), C::NotEqual, invalid);
    b.branch(local(OP), n(3), C::Equal, release);
    b.branch(charge!(stage), n(1), C::NotEqual, invalid);
    b.branch(charge!(bytes), heap!(metadata_pending), C::Above, invalid);
    b.branch(local(OP), n(2), C::Equal, abort);
    b.pointer(charge!(allocation), invalid);
    b.branch(
        add(charge!(allocation), charge!(bytes)),
        charge!(allocation),
        C::Below,
        invalid,
    );
    b.set(
        heap!(metadata_committed),
        add(heap!(metadata_committed), charge!(bytes)),
    );
    b.set(charge!(stage), n(2));
    b.a.jmp_label(unreserve);
    b.a.bind_text_label(abort);
    b.branch(charge!(allocation), n(0), C::NotEqual, invalid);
    b.set(charge!(stage), n(3));
    b.a.bind_text_label(unreserve);
    b.set(
        heap!(metadata_pending),
        sub(heap!(metadata_pending), charge!(bytes)),
    );
    b.a.jmp_label(status[0]);
    b.a.bind_text_label(release);
    b.branch(charge!(stage), n(2), C::NotEqual, invalid);
    b.branch(charge!(allocation), n(0), C::NotEqual, invalid);
    b.branch(
        charge!(bytes),
        sub(heap!(metadata_committed), local(USED)),
        C::Above,
        invalid,
    );
    b.set(
        heap!(metadata_committed),
        sub(heap!(metadata_committed), charge!(bytes)),
    );
    b.set(charge!(stage), n(3));
    b.a.jmp_label(status[0]);
    for (value, label) in status.into_iter().enumerate() {
        b.a.bind_text_label(label);
        b.a.mov_imm64(R::V0, value as u64);
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(done);
    b.a.leave();
}
