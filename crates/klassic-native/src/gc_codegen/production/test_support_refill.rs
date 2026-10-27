//! Force a real peer to consume a completed Young window before the interrupted
//! fresh allocator resumes. No page, phase, ACK or completion is manufactured.
use super::super::*;

pub(crate) fn pause_refill_return_for_peer(g: &mut crate::NativeCodeGenerator) -> crate::DataLabel {
    let hook = g.create_text_label();
    let body = g.create_text_label();
    g.asm.data.resize(g.asm.data.len().next_multiple_of(8), 0);
    let observed = g.asm.data_label_with_i64s(&[0; 3]);
    let l = &mut g.gc_gen.as_mut().unwrap().labels;
    l.alloc_after_collect_hook = Some(hook);
    let (end, poll, relax, clock, fail) = (
        l.moving_end_actor,
        l.owner_checkpoint_poll,
        l.worker.as_ref().unwrap().hooks.relax,
        l.worker.as_ref().unwrap().hooks.clock,
        l.fail,
    );
    g.jmp_label(body);
    g.bind_text_label(hook);
    quiescence::save(g);
    g.push_reg(R::V5);
    g.mov_reg_reg(R::V5, R::V4);
    g.sub_reg_imm8(R::V4, 64);
    let done = g.create_text_label();
    let peer = g.create_text_label();
    load_rt(g, R::V0, RT_CTX + offset_of!(MutatorContext, thread_id));
    g.cmp_reg_imm32(R::V0, 2);
    g.jcc_label(C::NotEqual, peer);
    g.mov_data_addr(R::V1, observed);
    g.load_ptr_disp32(R::V0, R::V1, 0);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, done);
    // Once the retained graph is Old, the next Young window has no copied
    // Young pages. Real peers can consume its whole physical allocation chain.
    load_rt(g, R::V0, capacity::SEALED);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, done);
    g.mov_imm64(R::V0, 1);
    g.store_ptr_disp32(R::V1, 0, R::V0);
    g.call_label(clock);
    g.mov_imm64(R::V1, 30_000_000_000);
    g.add_reg_reg(R::V0, R::V1);
    g.store_rbp_slot(8, R::V0);
    let finish = g.create_text_label();
    let check = g.create_text_label();
    let full = g.create_text_label();
    let wait = g.create_text_label();
    g.bind_text_label(finish);
    g.call_label(end);
    g.call_label(poll);
    load_rt(g, R::V0, RT_PHASE_OPEN);
    load_rt(g, R::V1, RT_RETIRE_STAGE);
    g.or_reg_reg(R::V0, R::V1);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::Equal, check);
    g.call_label(clock);
    g.load_rbp_slot(R::V1, 8);
    g.cmp_reg_reg(R::V0, R::V1);
    g.jcc_label(C::AboveOrEqual, fail);
    g.call_label(relax);
    g.jmp_label(finish);
    g.bind_text_label(check);
    // Leave this Running owner unacknowledged only during the controlled wait.
    // Peers allocate/close their actual TLABs until another real round requests
    // its ACK. The production allocator must service that round on return.
    load_rt(g, R::V0, RT_NEXT_PAGE);
    load_rt(g, R::V1, RT_SPACE_END);
    g.cmp_reg_reg(R::V0, R::V1);
    g.jcc_label(C::Below, wait);
    load_rt(g, R::V0, quiescence::REQUEST);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, full);
    g.bind_text_label(wait);
    g.call_label(clock);
    g.load_rbp_slot(R::V1, 8);
    g.cmp_reg_reg(R::V0, R::V1);
    g.jcc_label(C::AboveOrEqual, fail);
    g.call_label(relax);
    g.jmp_label(check);
    g.bind_text_label(full);
    g.mov_data_addr(R::V1, observed);
    g.mov_imm64(R::V0, 1);
    g.store_ptr_disp32(R::V1, 8, R::V0);
    load_rt(g, R::V0, allocation_records::REVISION);
    g.store_ptr_disp32(R::V1, 16, R::V0);
    g.jmp_label(done);
    g.bind_text_label(peer);
    // A peer pauses at its first real refill return, so it cannot finish its
    // whole workload before the selected owner reaches an empty-Young cycle.
    // It still acknowledges real rounds while waiting for the selected owner.
    g.call_label(clock);
    g.mov_imm64(R::V1, 30_000_000_000);
    g.add_reg_reg(R::V0, R::V1);
    g.store_rbp_slot(8, R::V0);
    let peer_wait = g.create_text_label();
    g.bind_text_label(peer_wait);
    g.mov_data_addr(R::V1, observed);
    g.atomic_load_acquire(R::V0, R::V1);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, done);
    g.call_label(poll);
    g.call_label(clock);
    g.load_rbp_slot(R::V1, 8);
    g.cmp_reg_reg(R::V0, R::V1);
    g.jcc_label(C::AboveOrEqual, fail);
    g.call_label(relax);
    g.jmp_label(peer_wait);
    g.bind_text_label(done);
    g.leave();
    quiescence::restore(g);
    g.ret();
    g.bind_text_label(body);
    observed
}

pub(crate) fn refill_return_observed(
    g: &mut crate::NativeCodeGenerator,
    data: crate::DataLabel,
    word: i32,
) {
    g.mov_data_addr(R::V1, data);
    g.load_ptr_disp32(R::V0, R::V1, word * 8);
}
