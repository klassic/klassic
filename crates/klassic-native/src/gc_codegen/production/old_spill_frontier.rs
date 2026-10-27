//! Retained Old overflow jobs are strong roots across Young movement. A node
//! owns an actual native page holder until its destination takes that holder.
use super::super::mark_queue::{self as q, SpillNode};
use super::super::*;

fn field(member: usize) -> i32 {
    d(member)
}
fn validate<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, record: R, node: R) {
    a.load_ptr_disp32(R::V0, node, field(offset_of!(SpillNode, state)));
    a.cmp_reg_imm32(R::V0, 1);
    a.jcc_label(C::NotEqual, l.fail);
    a.load_ptr_disp32(R::V0, node, field(offset_of!(SpillNode, record)));
    a.cmp_reg_reg(R::V0, record);
    a.jcc_label(C::NotEqual, l.fail);
    a.load_ptr_disp32(R::V0, node, field(offset_of!(SpillNode, epoch)));
    load_rt(a, R::V1, RT_MARK_CYCLES);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, l.fail);
    a.load_ptr_disp32(R::V0, node, field(offset_of!(SpillNode, serial)));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, l.fail);
    a.load_ptr_disp32(R::V11, node, field(offset_of!(SpillNode, page)));
    a.test_reg_reg(R::V11, R::V11);
    a.jcc_label(C::Equal, l.fail);
    a.load_ptr_disp32(R::V0, R::V11, d(offset_of!(AllocationPage, incarnation)));
    a.load_ptr_disp32(R::V1, node, field(offset_of!(SpillNode, incarnation)));
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, l.fail);
    a.load_ptr_disp32(
        R::V0,
        R::V11,
        d(offset_of!(AllocationPage, metadata_leases)),
    );
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, l.fail);
}
/// V9=source record, V0=new link. Update only the retained predecessor or head.
fn link<E: PortableAsm>(a: &mut E) {
    load_rt(a, R::V1, q::relocate_previous());
    let head = a.create_text_label();
    let done = a.create_text_label();
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::Equal, head);
    q::node(a, false, R::V1, R::V1);
    a.store_ptr_disp32(R::V1, field(offset_of!(SpillNode, next)), R::V0);
    a.jmp_label(done);
    a.bind_text_label(head);
    store_rt(a, q::head(false), R::V0);
    a.bind_text_label(done);
}
/// All source-node writes precede releasing its last metadata owner.
fn clear_source<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    a.load_ptr_disp32(R::V11, R::V10, field(offset_of!(SpillNode, page)));
    a.mov_imm64(R::V0, 0);
    for off in (0..size_of::<SpillNode>()).step_by(8) {
        a.store_ptr_disp32(R::V10, d(off), R::V0);
    }
    q::release_page(a, l, R::V11);
}

pub(super) fn pending<E: PortableAsm>(a: &mut E, pending: E::TextLabel) {
    load_rt(a, R::V0, q::relocate_current());
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, pending);
}

pub(super) fn scan<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    forward: E::TextLabel,
    paused: E::TextLabel,
) {
    let done = a.create_text_label();
    let top = a.create_text_label();
    load_rt(a, R::V0, RT_MARK_ACTIVE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    a.bind_text_label(top);
    load_rt(a, R::V9, q::relocate_current());
    a.test_reg_reg(R::V9, R::V9);
    a.jcc_label(C::Equal, done);
    scan::spend(a, paused);
    load_rt(a, R::V9, q::relocate_current()); // spend uses V9 as scratch
    q::node(a, false, R::V9, R::V10);
    validate(a, l, R::V9, R::V10);
    a.load_ptr_disp32(R::V0, R::V10, field(offset_of!(SpillNode, reference)));
    a.push_reg(R::V9);
    a.push_reg(R::V9);
    a.call_label(forward);
    a.mov_reg_reg(R::V2, R::V0);
    a.shr_reg_imm8(R::V2, 16);
    mark_epoch::set(a, R::V2, R::V6, R::V7);
    load_rt(a, R::V6, RT_HEAP_BASE);
    a.add_reg_reg(R::V6, R::V2);
    a.load_ptr_disp32(R::V6, R::V6, copy::MARK_RECORD);
    q::page_for_offset(a); // V8 is the actual destination run head.
    a.shl_reg_imm8(R::V2, 16);
    colours::load_current(a, R::V1);
    a.or_reg_reg(R::V2, R::V1);
    a.pop_reg(R::V9);
    a.pop_reg(R::V9);
    q::node(a, false, R::V9, R::V10);
    validate(a, l, R::V9, R::V10);
    // The producer may have appended while forwarding. Read next afterward.
    a.load_ptr_disp32(R::V0, R::V10, field(offset_of!(SpillNode, next)));
    store_rt(a, q::relocate_current(), R::V0);
    a.load_ptr_disp32(R::V0, R::V9, d(offset_of!(MarkRecord, object_id)));
    a.load_ptr_disp32(R::V1, R::V6, d(offset_of!(MarkRecord, object_id)));
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, l.fail);
    let same = a.create_text_label();
    let coalesce = a.create_text_label();
    a.cmp_reg_reg(R::V9, R::V6);
    a.jcc_label(C::Equal, same);
    q::node(a, false, R::V6, R::V7);
    a.load_ptr_disp32(R::V0, R::V7, field(offset_of!(SpillNode, state)));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, coalesce);
    for off in (0..size_of::<SpillNode>()).step_by(8) {
        a.load_ptr_disp32(R::V0, R::V7, d(off));
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, l.fail);
    }
    q::retain_page(a, l, R::V8); // Fund before releasing the source holder.
    for off in (0..size_of::<SpillNode>()).step_by(8) {
        a.load_ptr_disp32(R::V0, R::V10, d(off));
        a.store_ptr_disp32(R::V7, d(off), R::V0);
    }
    a.store_ptr_disp32(R::V7, field(offset_of!(SpillNode, record)), R::V6);
    a.store_ptr_disp32(R::V7, field(offset_of!(SpillNode, reference)), R::V2);
    a.store_ptr_disp32(R::V7, field(offset_of!(SpillNode, page)), R::V8);
    a.load_ptr_disp32(R::V0, R::V8, d(offset_of!(AllocationPage, incarnation)));
    a.store_ptr_disp32(R::V7, field(offset_of!(SpillNode, incarnation)), R::V0);
    a.mov_reg_reg(R::V0, R::V6);
    link(a);
    load_rt(a, R::V0, q::tail(false));
    a.cmp_reg_reg(R::V0, R::V9);
    let transferred = a.create_text_label();
    a.jcc_label(C::NotEqual, transferred);
    store_rt(a, q::tail(false), R::V6);
    a.bind_text_label(transferred);
    store_rt(a, q::relocate_previous(), R::V6);
    clear_source(a, l);
    a.jmp_label(top);

    a.bind_text_label(coalesce);
    validate(a, l, R::V6, R::V7);
    // A white copy may already have acquired a real same-epoch spill job.
    // Preserve that job and retire only the duplicate source obligation.
    a.store_ptr_disp32(R::V7, field(offset_of!(SpillNode, reference)), R::V2);
    load_rt(a, R::V0, q::relocate_current());
    link(a);
    load_rt(a, R::V0, q::tail(false));
    a.cmp_reg_reg(R::V0, R::V9);
    let untail = a.create_text_label();
    a.jcc_label(C::NotEqual, untail);
    load_rt(a, R::V0, q::relocate_previous());
    store_rt(a, q::tail(false), R::V0);
    a.bind_text_label(untail);
    load_rt(a, R::V0, q::count(false));
    a.cmp_reg_imm32(R::V0, 2);
    a.jcc_label(C::Below, l.fail);
    a.sub_reg_imm8(R::V0, 1);
    store_rt(a, q::count(false), R::V0);
    clear_source(a, l);
    a.jmp_label(top);

    a.bind_text_label(same);
    a.store_ptr_disp32(R::V10, field(offset_of!(SpillNode, reference)), R::V2);
    store_rt(a, q::relocate_previous(), R::V9);
    a.jmp_label(top);
    a.bind_text_label(done);
}
