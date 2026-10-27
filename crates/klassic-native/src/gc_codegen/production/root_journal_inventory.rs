//! Journal discovery over real registered storage, independently of all three
//! cell scan lanes. The reader cursor pins each registration across yields.
//! No generation cell-scan credit is claimed by this journal-only walk.
use super::*;
use crate::gc_abi::RootWalk;
use crate::gc_codegen::production::{lea_rt, load_rt, store_rt};
const WALK: usize = BASE + offset_of!(State, inventory);
const REG: usize = WALK + offset_of!(RootWalk, lease) + offset_of!(RootLease, registration);

fn advance<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, op: u64) {
    lea_rt(a, R::V7, registered_roots::REGISTRY);
    lea_rt(a, R::V6, WALK);
    a.mov_imm64(R::V2, op);
    a.call_label(l.root_walk_step);
    let valid = a.create_text_label();
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, valid);
    a.cmp_reg_imm8(R::V0, 2);
    a.jcc_label(C::NotEqual, l.fail);
    a.bind_text_label(valid);
}
pub(super) fn emit<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, consume: E::TextLabel) {
    for r in [R::V6, R::V7, R::V8, R::V9, R::V10, R::V11] {
        a.push_reg(r);
    }
    load_rt(a, R::V0, OWNER_LOCKED);
    a.cmp_reg_imm8(R::V0, 1);
    a.jcc_label(C::NotEqual, l.fail);
    // This owner is stopped until every discovered journal is handed off.
    // Other owner contexts are not yet admitted by production coordination.
    load_rt(a, R::V0, RT_PENDING_REF);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    advance(a, l, 0);
    let again = a.create_text_label();
    let done = a.create_text_label();
    let room = a.create_text_label();
    a.bind_text_label(again);
    load_rt(a, R::V0, WALK + offset_of!(RootWalk, active));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    load_rt(a, R::V0, REMAINING);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, room);
    yield_slice(a, l);
    a.bind_text_label(room);
    // Discovering a region costs a unit even when its journal is absent/empty.
    // The reader lease was obtained before releasing the lock above.
    load_rt(a, R::V0, REMAINING);
    a.sub_reg_imm8(R::V0, 1);
    store_rt(a, REMAINING, R::V0);
    copy::bump(a, BASE + offset_of!(State, regions), R::V0);
    load_rt(a, R::V0, REG);
    a.call_label(consume);
    // This lane visits journals, not cells. It explicitly claims zero cell
    // completion bits; the independent Old/Young/relocation walks do that work.
    store_rt_imm(a, WALK + offset_of!(RootWalk, mask), 0);
    store_rt_imm(a, WALK + offset_of!(RootWalk, completed_mask), 0);
    load_rt(a, R::V0, WALK + offset_of!(RootWalk, limit));
    store_rt(a, WALK + offset_of!(RootWalk, cursor), R::V0);
    advance(a, l, 1);
    a.jmp_label(again);
    a.bind_text_label(done);
    // The actual walk must have released its final reader lease.
    load_rt(
        a,
        R::V0,
        WALK + offset_of!(RootWalk, lease) + offset_of!(RootLease, kind),
    );
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    // No synthetic MarkQueue or RootAudit result is produced here.
    for r in [R::V11, R::V10, R::V9, R::V8, R::V7, R::V6] {
        a.pop_reg(r);
    }
    a.ret();
}
