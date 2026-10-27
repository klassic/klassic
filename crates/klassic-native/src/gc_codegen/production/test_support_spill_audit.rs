//! Test-only full audit after physical source retirement. No production pause
//! path calls this walk; every checked node already owns its native receipt.
use super::super::mark_queue::{self as q, SpillNode};
use super::super::*;

/// Native debug offsets for observing real progress in an exported ELF.
pub(crate) fn spill_debug_offsets() -> Vec<(&'static str, usize)> {
    vec![
        ("lock_next", RT_LOCK_NEXT),
        ("lock_serving", RT_LOCK_SERVING),
        ("phase_open", RT_PHASE_OPEN),
        ("retire_stage", RT_RETIRE_STAGE),
        ("old_active", RT_MARK_ACTIVE),
        ("old_epoch", RT_MARK_CYCLES),
        ("old_stack_len", RT_MSTACK_LEN),
        ("old_object", RT_M_OBJ),
        ("old_field", RT_M_FIELD),
        ("old_relocate_index", RT_M_RELOCATE_INDEX),
        ("young_active", young_mark::ACTIVE),
        ("young_done", young_mark::DONE),
        ("young_epoch", young_mark::EPOCH),
        ("young_stack_len", young_mark::LEN),
        ("young_object", young_mark::OBJECT),
        ("young_old_index", young_mark::OLD_INDEX),
        ("young_old_bound", young_mark::OLD_BOUND),
        ("holders", q::HOLDERS),
        ("old_spill_count", q::count(false)),
        ("old_spill_head", q::head(false)),
        ("old_spill_tail", q::tail(false)),
        ("old_active_page", q::active_page(false)),
        ("young_spill_count", q::count(true)),
        ("young_active_page", q::active_page(true)),
        ("spill_frozen", q::discovery_frozen()),
        ("spill_discovery_current", q::discovery_current()),
        ("spill_discovery_tail", q::discovery_tail()),
        ("spill_relocate_current", q::relocate_current()),
        ("spill_relocate_previous", q::relocate_previous()),
        ("phase_round_kind", phase_round::KIND),
        ("phase_round_resuming", phase_round::RESUMING),
        ("root_pending", root_work::PENDING),
        ("metadata_cursor", metadata_leases::CURSOR),
        ("handshake", participants::HANDSHAKE),
    ]
}

pub(crate) fn audit_old_spill_destinations(g: &mut crate::NativeCodeGenerator) {
    let rt = g.gc_gen.take().unwrap();
    let l = &rt.labels;
    let errors = (0..11).map(|_| g.create_text_label()).collect::<Vec<_>>();
    let release = g.create_text_label();
    worker::lock(g, l);
    load_rt(g, R::V9, q::head(false));
    load_rt(g, R::V6, q::count(false));
    g.mov_imm64(R::V7, 0);
    let top = g.create_text_label();
    let done = g.create_text_label();
    g.bind_text_label(top);
    g.test_reg_reg(R::V9, R::V9);
    g.jcc_label(C::Equal, done);
    g.test_reg_reg(R::V6, R::V6);
    g.jcc_label(C::Equal, errors[0]);
    q::node(g, false, R::V9, R::V10);
    g.load_ptr_disp32(R::V0, R::V10, d(offset_of!(SpillNode, state)));
    g.cmp_reg_imm32(R::V0, 1);
    g.jcc_label(C::NotEqual, errors[1]);
    g.load_ptr_disp32(R::V0, R::V10, d(offset_of!(SpillNode, record)));
    g.cmp_reg_reg(R::V0, R::V9);
    g.jcc_label(C::NotEqual, errors[2]);
    g.load_ptr_disp32(R::V2, R::V10, d(offset_of!(SpillNode, reference)));
    g.shr_reg_imm8(R::V2, 16);
    q::page_for_offset(g);
    g.load_ptr_disp32(R::V0, R::V10, d(offset_of!(SpillNode, page)));
    g.cmp_reg_reg(R::V0, R::V8);
    g.jcc_label(C::NotEqual, errors[3]);
    g.load_ptr_disp32(R::V0, R::V8, d(offset_of!(AllocationPage, incarnation)));
    g.load_ptr_disp32(R::V1, R::V10, d(offset_of!(SpillNode, incarnation)));
    g.cmp_reg_reg(R::V0, R::V1);
    g.jcc_label(C::NotEqual, errors[4]);
    g.load_ptr_disp32(R::V0, R::V8, d(offset_of!(AllocationPage, state)));
    let committed = g.create_text_label();
    g.cmp_reg_imm32(R::V0, crate::gc_abi::PAGE_ALLOCATING as i32);
    g.jcc_label(C::Equal, committed); // Actual live owner TLAB is committed.
    g.cmp_reg_imm32(R::V0, crate::gc_abi::PAGE_READY as i32);
    g.jcc_label(C::Below, errors[5]); // No Free/Reserved/Committing source.
    g.cmp_reg_imm32(R::V0, crate::gc_abi::PAGE_CLOSED as i32);
    g.jcc_label(C::Above, errors[6]); // No releasing/evacuated/uncommitted source.
    g.cmp_reg_imm32(R::V0, crate::gc_abi::PAGE_RUN_TAIL as i32);
    g.jcc_label(C::Equal, errors[7]); // Holder identifies the actual run head.
    g.bind_text_label(committed);
    g.load_ptr_disp32(R::V0, R::V8, d(offset_of!(AllocationPage, metadata_leases)));
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::Equal, errors[8]);
    load_rt(g, R::V8, RT_HEAP_BASE);
    g.add_reg_reg(R::V8, R::V2);
    g.load_ptr_disp32(R::V0, R::V8, copy::MARK_RECORD);
    g.cmp_reg_reg(R::V0, R::V9);
    g.jcc_label(C::NotEqual, errors[9]);
    g.sub_reg_imm8(R::V6, 1);
    g.add_reg_imm32(R::V7, 1);
    g.load_ptr_disp32(R::V9, R::V10, d(offset_of!(SpillNode, next)));
    g.jmp_label(top);
    g.bind_text_label(done);
    g.test_reg_reg(R::V6, R::V6);
    g.jcc_label(C::NotEqual, errors[10]);
    g.jmp_label(release);
    for (i, label) in errors.into_iter().enumerate() {
        g.bind_text_label(label);
        g.mov_imm64(R::V7, (-(i as i64 + 1)) as u64);
        g.jmp_label(release);
    }
    g.bind_text_label(release);
    g.push_reg(R::V7);
    g.push_reg(R::V7);
    worker::unlock(g, l);
    g.pop_reg(R::V0);
    g.pop_reg(R::V0);
    g.gc_gen = Some(rt);
}
