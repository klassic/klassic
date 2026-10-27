//! Native entry seals only the executing producer's authenticated owner list.
//! That producer cannot attach/retire its own regions during this synchronous
//! call; worker cleanup cannot unlink an active region from this owner list.
//! The executing context/storage lifetime therefore protects the list cursor;
//! each actual journal transport retains its ordinary counted kind-4 lease.
//! This is not the all-owner GC checkpoint or a claim of bounded total I/O-entry
//! latency: it yields the collector ticket between bounded transport slices.
use super::*;
const CURSOR: usize = BASE + offset_of!(State, own_cursor);
const PREVIOUS: usize = BASE + offset_of!(State, own_previous);
const SCOPE: usize = BASE + offset_of!(State, own_scope);

pub(super) fn emit<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, consume: E::TextLabel) {
    a.bind_text_label(l.root_journal_seal_owner);
    let saved = [R::V6, R::V7, R::V8, R::V9, R::V10, R::V11];
    for r in saved {
        a.push_reg(r);
    }
    load_rt(a, R::V0, OWNER_LOCKED);
    a.cmp_reg_imm32(R::V0, 1);
    a.jcc_label(C::NotEqual, l.fail);
    load_rt(a, R::V0, RT_PENDING_REF);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    store_rt_imm(a, PRESSURE, 0);
    store_rt_imm(a, REMAINING, SLICE);
    load_rt(a, R::V0, RT_CTX + offset_of!(MutatorContext, root_segments));
    store_rt(a, CURSOR, R::V0);
    store_rt_imm(a, PREVIOUS, 0);
    store_rt_imm(a, SCOPE, 0);
    let again = a.create_text_label();
    let done = a.create_text_label();
    let ordered = a.create_text_label();
    let room = a.create_text_label();
    a.bind_text_label(again);
    load_rt(a, R::V8, CURSOR);
    a.test_reg_reg(R::V8, R::V8);
    a.jcc_label(C::Equal, done);
    a.load_ptr_disp32(R::V0, R::V8, d(offset_of!(RootRegistration, registered)));
    a.cmp_reg_imm32(R::V0, 1);
    a.jcc_label(C::NotEqual, l.fail);
    a.load_ptr_disp32(R::V0, R::V8, d(offset_of!(RootRegistration, context)));
    a.cmp_reg_reg(R::V0, R::OwnerContext);
    a.jcc_label(C::NotEqual, l.fail);
    a.load_ptr_disp32(
        R::V0,
        R::V8,
        d(offset_of!(RootRegistration, owner_previous)),
    );
    load_rt(a, R::V1, PREVIOUS);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, l.fail);
    a.load_ptr_disp32(R::V0, R::V8, d(offset_of!(RootRegistration, scope)));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, l.fail);
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::Equal, ordered);
    load_rt(a, R::V1, SCOPE);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::AboveOrEqual, l.fail);
    a.bind_text_label(ordered);
    store_rt(a, SCOPE, R::V0);
    load_rt(a, R::V0, REMAINING);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, room);
    yield_slice(a, l);
    a.bind_text_label(room);
    load_rt(a, R::V0, REMAINING);
    a.sub_reg_imm8(R::V0, 1);
    store_rt(a, REMAINING, R::V0);
    copy::bump(a, BASE + offset_of!(State, regions), R::V0);
    load_rt(a, R::V0, CURSOR);
    a.call_label(consume);
    load_rt(a, R::V8, CURSOR);
    store_rt(a, PREVIOUS, R::V8);
    a.load_ptr_disp32(R::V0, R::V8, d(offset_of!(RootRegistration, owner_next)));
    store_rt(a, CURSOR, R::V0);
    a.jmp_label(again);
    a.bind_text_label(done);
    for r in saved.into_iter().rev() {
        a.pop_reg(r);
    }
    a.ret();
}
