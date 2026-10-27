//! Real copier overlap: all owners leave genuine Native scopes through the
//! actual open Young relocation. The real worker waits outside its ticket.
use super::*;
const BLOCK: i32 = 0;
const PARKED: i32 = 8;
const ARRIVED: i32 = 16;
const DONE: i32 = 24;
const PEAK: i32 = 32;
const REMAINING: i32 = 40;
const TARGET: i32 = 48;
const SOURCE: i32 = 56;
const PHASE: i32 = 64;
const BANK: i32 = 72;
const WORKER_ACTIVE: i32 = 80;
const PEAK_READY: i32 = 88;
const LAST_READY: i32 = 96;
const HOLD: i32 = 104;
const RELEASE_LAST: i32 = 112;
const ENABLED: i32 = 120;

fn load(g: &mut crate::NativeCodeGenerator, data: crate::DataLabel, field: i32) {
    g.mov_data_addr(R::V1, data);
    g.add_reg_imm32(R::V1, field);
    g.atomic_load_acquire(R::V0, R::V1);
}
fn store(g: &mut crate::NativeCodeGenerator, data: crate::DataLabel, field: i32, value: u64) {
    g.mov_data_addr(R::V1, data);
    g.add_reg_imm32(R::V1, field);
    g.mov_imm64(R::V0, value);
    g.atomic_store_release(R::V1, R::V0);
}
fn increment(g: &mut crate::NativeCodeGenerator, data: crate::DataLabel, field: i32) {
    g.mov_data_addr(R::V7, data);
    g.add_reg_imm32(R::V7, field);
    let retry = g.create_text_label();
    g.bind_text_label(retry);
    g.atomic_load_acquire(R::V0, R::V7);
    g.mov_reg_reg(R::V8, R::V0);
    g.mov_reg_reg(R::V6, R::V0);
    g.add_reg_imm32(R::V6, 1);
    g.atomic_compare_exchange(R::V7, R::V6);
    g.cmp_reg_reg(R::V0, R::V8);
    g.jcc_label(C::NotEqual, retry);
    g.add_reg_imm32(R::V0, 1);
}
fn deadline(g: &mut crate::NativeCodeGenerator, clock: crate::TextLabel) {
    g.call_label(clock);
    g.mov_imm64(R::V10, 10_000_000_000);
    g.add_reg_reg(R::V10, R::V0);
}
fn wait(
    g: &mut crate::NativeCodeGenerator,
    data: crate::DataLabel,
    field: i32,
    value: u64,
    clock: crate::TextLabel,
    relax: crate::TextLabel,
    fail: crate::TextLabel,
) {
    let again = g.create_text_label();
    let done = g.create_text_label();
    g.bind_text_label(again);
    load(g, data, field);
    g.cmp_reg_imm32(R::V0, value as i32);
    g.jcc_label(C::Equal, done);
    g.call_label(clock);
    g.cmp_reg_reg(R::V0, R::V10);
    g.jcc_label(C::AboveOrEqual, fail);
    g.call_label(relax);
    g.jmp_label(again);
    g.bind_text_label(done);
}
pub(crate) fn copy_actor_native_enter(g: &mut crate::NativeCodeGenerator, actor: TestActor) {
    let entry = g.gc_gen.as_ref().unwrap().labels.native_enter_actor;
    select(g, actor);
    graph_call(g, entry);
    restore(g);
}
fn graph_call(g: &mut crate::NativeCodeGenerator, entry: crate::TextLabel) {
    // The ordinary explicit boundary derives the heap from its real owner.
    let fail = g.gc_gen.as_ref().unwrap().labels.fail;
    g.push_reg(R::V3);
    g.mov_imm64(R::V3, 123);
    g.call_label(entry);
    g.cmp_reg_imm32(R::V3, 123);
    g.jcc_label(C::NotEqual, fail);
    g.pop_reg(R::V3);
}
pub(crate) fn prepare_copy_overlap(
    g: &mut crate::NativeCodeGenerator,
    actors: &[TestActor],
    mode: u64,
) -> crate::DataLabel {
    prepare_overlap(g, actors, mode, false)
}
pub(crate) fn prepare_full_copy_overlap(
    g: &mut crate::NativeCodeGenerator,
    actors: &[TestActor],
) -> crate::DataLabel {
    prepare_overlap(g, actors, 0, true)
}
fn prepare_overlap(
    g: &mut crate::NativeCodeGenerator,
    actors: &[TestActor],
    mode: u64,
    full: bool,
) -> crate::DataLabel {
    let mut rt = g.gc_gen.take().unwrap();
    let w = rt.labels.worker.as_ref().unwrap();
    let (clock, relax, fail) = (w.hooks.clock, w.hooks.relax, rt.labels.fail);
    g.asm.data.resize(g.asm.data.len().next_multiple_of(8), 0);
    let data = g.asm.data_label_with_i64s(&[0; 17]);
    store(g, data, HOLD, mode);
    g.mov_data_addr(R::V1, actors[0].pointer);
    g.load_ptr_disp32(R::V0, R::V1, 0);
    g.mov_data_addr(R::V1, data);
    g.store_ptr_disp32(R::V1, TARGET, R::V0);
    let worker_hook = g.create_text_label();
    let begin_hook = g.create_text_label();
    let end_hook = g.create_text_label();
    let body = g.create_text_label();
    rt.labels.worker_before_lock_hook = Some(worker_hook);
    rt.labels.copy_claim_begin_hook = Some(begin_hook);
    rt.labels.copy_claim_end_hook = Some(end_hook);
    g.jmp_label(body);
    g.bind_text_label(worker_hook);
    quiescence::save(g);
    let worker_done = g.create_text_label();
    load(g, data, BLOCK);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::Equal, worker_done);
    if full {
        // Let the real Young/Old snapshots finish. Park only after the actual
        // moving coordinator opens relocation, before the worker takes a ticket.
        load_rt(g, R::V0, RT_PHASE_OPEN);
        g.test_reg_reg(R::V0, R::V0);
        g.jcc_label(C::Equal, worker_done);
    }
    // Only the actual worker writes its epoch here; it has released its ticket.
    load_rt(g, R::V0, worker_view::ACTIVE);
    g.mov_data_addr(R::V1, data);
    g.store_ptr_disp32(R::V1, WORKER_ACTIVE, R::V0);
    store(g, data, PARKED, 1);
    deadline(g, clock);
    wait(g, data, BLOCK, 0, clock, relax, fail);
    store(g, data, PARKED, 0);
    g.bind_text_label(worker_done);
    quiescence::restore(g);
    g.ret();
    g.bind_text_label(begin_hook);
    quiescence::save(g);
    let begin_done = g.create_text_label();
    load(g, data, ENABLED);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::Equal, begin_done);
    load_rt(g, R::V0, root_journal::OWNER_LOCKED);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, fail); // waiting here must not exclude other copiers
    increment(g, data, ARRIVED);
    deadline(g, clock);
    wait(g, data, ARRIVED, actors.len() as u64, clock, relax, fail);
    let other = g.create_text_label();
    load(g, data, TARGET);
    g.cmp_reg_reg(R::OwnerContext, R::V0);
    g.jcc_label(C::NotEqual, other);
    lea_rt(g, R::V1, RT_M_COPYING);
    g.atomic_load_acquire(R::V0, R::V1);
    g.cmp_reg_imm32(R::V0, actors.len() as i32);
    g.jcc_label(C::NotEqual, fail);
    g.mov_data_addr(R::V1, data);
    g.store_ptr_disp32(R::V1, PEAK, R::V0);
    load_rt(g, R::V0, copy_claims::SOURCE);
    g.store_ptr_disp32(R::V1, SOURCE, R::V0);
    load_rt(g, R::V0, RT_CTX + offset_of!(MutatorContext, adopted_phase));
    g.store_ptr_disp32(R::V1, PHASE, R::V0);
    load_rt(g, R::V0, allocation_records::REVISION);
    g.store_ptr_disp32(R::V1, BANK, R::V0);
    store(g, data, PEAK_READY, 1);
    wait(g, data, DONE, actors.len() as u64 - 1, clock, relax, fail);
    lea_rt(g, R::V1, RT_M_COPYING);
    g.atomic_load_acquire(R::V0, R::V1);
    g.cmp_reg_imm32(R::V0, 1);
    g.jcc_label(C::NotEqual, fail);
    g.mov_data_addr(R::V1, data);
    g.store_ptr_disp32(R::V1, REMAINING, R::V0);
    store(g, data, LAST_READY, 1);
    load(g, data, HOLD);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::Equal, begin_done);
    wait(g, data, RELEASE_LAST, 1, clock, relax, fail);
    g.jmp_label(begin_done);
    g.bind_text_label(other);
    wait(g, data, PEAK_READY, 1, clock, relax, fail);
    g.bind_text_label(begin_done);
    quiescence::restore(g);
    g.ret();
    g.bind_text_label(end_hook);
    quiescence::save(g);
    let end_done = g.create_text_label();
    load(g, data, ENABLED);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::Equal, end_done);
    increment(g, data, DONE);
    g.cmp_reg_imm32(R::V0, actors.len() as i32);
    g.jcc_label(C::NotEqual, end_done);
    store(g, data, BLOCK, 0);
    g.bind_text_label(end_done);
    quiescence::restore(g);
    g.ret();
    g.bind_text_label(body);
    g.gc_gen = Some(rt);
    if full {
        store(g, data, BLOCK, 1);
        store(g, data, ENABLED, 1);
    } else {
        copy_prepare_snapshot(g, data);
    }
    data
}
pub(super) fn iteration(g: &mut crate::NativeCodeGenerator, root: crate::DataLabel) {
    let entry = g.gc_gen.as_ref().unwrap().labels.field_load_actor;
    g.mov_data_addr(R::V1, root);
    g.load_ptr_disp32(R::V1, R::V1, 0);
    // The parent retains this registered RootCell throughout every child.
    // Resolve its actual reference through the production value-load barrier;
    // RootCell mutation itself is serialized and would hide copy overlap.
    g.mov_reg_reg(R::V10, R::V1);
    g.atomic_load_acquire(R::V0, R::V1);
    graph_call(g, entry);
    g.load_rbp_slot(R::V1, 8);
    g.load_ptr_disp32(R::V1, R::V1, 40); // driver's actual result-row pointer
    g.store_ptr_disp32(R::V1, 0, R::V0);
    g.load_ptr_disp32(
        R::V2,
        R::V0,
        offset_of!(ObjectHeader, mark_record) as i32 - size_of::<ObjectHeader>() as i32,
    );
    g.store_ptr_disp32(R::V1, 16, R::V2);
    g.load_ptr_disp32(R::V2, R::V2, offset_of!(MarkRecord, object_id) as i32);
    g.store_ptr_disp32(R::V1, 8, R::V2);
}
pub(crate) fn copy_overlap_metric(
    g: &mut crate::NativeCodeGenerator,
    data: crate::DataLabel,
    field: usize,
) {
    load(g, data, field as i32 * 8);
}
pub(crate) fn copy_aggregate(g: &mut crate::NativeCodeGenerator) {
    lea_rt(g, R::V1, RT_M_COPYING);
    g.atomic_load_acquire(R::V0, R::V1);
}

pub(super) fn parent_fault(g: &mut crate::NativeCodeGenerator, data: crate::DataLabel, mode: u64) {
    let rt = g.gc_gen.take().unwrap();
    let w = rt.labels.worker.as_ref().unwrap();
    deadline(g, w.hooks.clock);
    wait(
        g,
        data,
        LAST_READY,
        1,
        w.hooks.clock,
        w.hooks.relax,
        rt.labels.fail,
    );
    if mode == 4 {
        g.gc_gen = Some(rt);
        let exit = g.gc_gen.as_ref().unwrap().labels.native_exit_actor;
        graph_call(g, exit);
        // The RootCell still contains a genuine stale reference. The value
        // barrier resolves the actual winner before this scalar store; no
        // safepoint/raw-address escape occurs between resolve and store.
        g.mov_data_addr(R::V1, data);
        g.load_ptr_disp32(R::V1, R::V1, 128);
        g.mov_reg_reg(R::V10, R::V1);
        g.atomic_load_acquire(R::V0, R::V1);
        let load = g.gc_gen.as_ref().unwrap().labels.field_load_actor;
        graph_call(g, load);
        g.mov_imm64(R::V1, 99);
        g.store_ptr_disp32(R::V0, 0, R::V1);
        let enter = g.gc_gen.as_ref().unwrap().labels.native_enter_actor;
        graph_call(g, enter);
        store(g, data, RELEASE_LAST, 1);
        return;
    }
    worker::lock(g, &rt.labels);
    match mode {
        1 => retire::begin(g, &rt.labels),
        2 => allocation_records::advance(g, &rt.labels),
        3 => copy_claims::complete(g, &rt.labels, RT_SCAN_OLD),
        _ => unreachable!(),
    }
    // An invalid operation was accepted while a different real copier is
    // still live. Exit the entire Linux test process before a later guard or
    // that copier's bounded wait could mask this missing rejection.
    g.asm.mov_imm64(crate::Reg::Rax, 231);
    g.asm.mov_imm64(crate::Reg::Rdi, 0);
    g.asm.syscall();
    g.gc_gen = Some(rt);
}
pub(crate) fn copy_promoted_bytes(g: &mut crate::NativeCodeGenerator) {
    lea_rt(g, R::V1, RT_PROMOTED);
    g.atomic_load_acquire(R::V0, R::V1);
}

/// Reuse the same real worker gate for a later actual collection; no second
/// hook or phase receipt is manufactured.
pub(crate) fn copy_prepare_snapshot(g: &mut crate::NativeCodeGenerator, data: crate::DataLabel) {
    let rt = g.gc_gen.take().unwrap();
    let w = rt.labels.worker.as_ref().unwrap();
    g.call_label(w.young.start);
    young_mark::wait(g, &rt.labels);
    store(g, data, BLOCK, 1);
    worker::lock(g, &rt.labels);
    worker::request_work(g);
    worker::unlock(g, &rt.labels);
    worker::wake_if_pending(g, &rt.labels);
    deadline(g, w.hooks.clock);
    wait(
        g,
        data,
        PARKED,
        1,
        w.hooks.clock,
        w.hooks.relax,
        rt.labels.fail,
    );
    store(g, data, ENABLED, 1);
    g.gc_gen = Some(rt);
}
pub(crate) fn copy_worker_release(g: &mut crate::NativeCodeGenerator, data: crate::DataLabel) {
    store(g, data, BLOCK, 0);
}
fn accepted(g: &mut crate::NativeCodeGenerator) {
    g.asm.mov_imm64(crate::Reg::Rax, 231);
    g.asm.mov_imm64(crate::Reg::Rdi, 0);
    g.asm.syscall();
}
pub(crate) fn primary_copy_probe(g: &mut crate::NativeCodeGenerator) -> TestActor {
    g.asm.data.resize(g.asm.data.len().next_multiple_of(8), 0);
    let pointer = g.asm.data_label_with_i64s(&[0]);
    g.mov_data_addr(R::V1, pointer);
    g.store_ptr_disp32(R::V1, 0, R::OwnerContext);
    TestActor { pointer }
}
pub(crate) fn install_copy_fault(
    g: &mut crate::NativeCodeGenerator,
    data: crate::DataLabel,
    mode: u64,
) {
    let mut rt = g.gc_gen.take().unwrap();
    let begin = g.create_text_label();
    let end = g.create_text_label();
    let body = g.create_text_label();
    rt.labels.copy_claim_begin_hook = Some(begin);
    rt.labels.copy_claim_end_hook = Some(end);
    g.jmp_label(body);
    g.bind_text_label(begin);
    quiescence::save(g);
    let done = g.create_text_label();
    load(g, data, ENABLED);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::Equal, done);
    if mode == 0 {
        load(g, data, ARRIVED);
        g.test_reg_reg(R::V0, R::V0);
        let first = g.create_text_label();
        g.jcc_label(C::Equal, first);
        accepted(g); // an illegal recursive admission was accepted
        g.bind_text_label(first);
        store(g, data, ARRIVED, 1);
        copy_claims::begin(g, &rt.labels, RT_SCAN_OLD);
        accepted(g);
    } else if mode == 6 {
        accepted(g); // aggregate overflow reached the body
    } else {
        copy_claims::corrupt_actual_claim(g, mode);
    }
    g.bind_text_label(done);
    quiescence::restore(g);
    g.ret();
    g.bind_text_label(end);
    load(g, data, ENABLED);
    let ordinary = g.create_text_label();
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::Equal, ordinary);
    accepted(g); // stop before any later guard can mask invalid acceptance
    g.bind_text_label(ordinary);
    g.ret();
    g.bind_text_label(body);
    if mode == 6 {
        store_rt_imm(g, RT_M_COPYING, u64::MAX);
    }
    g.gc_gen = Some(rt);
}
pub(crate) fn invoke_actual_copy(g: &mut crate::NativeCodeGenerator, root: crate::DataLabel) {
    let entry = g.gc_gen.as_ref().unwrap().labels.field_load_actor;
    g.mov_data_addr(R::V1, root);
    g.load_ptr_disp32(R::V1, R::V1, 0);
    g.mov_reg_reg(R::V10, R::V1);
    g.atomic_load_acquire(R::V0, R::V1);
    graph_call(g, entry);
    accepted(g);
}
pub(crate) fn disable_copy_probe_hooks(g: &mut crate::NativeCodeGenerator) {
    let l = &mut g.gc_gen.as_mut().unwrap().labels;
    l.copy_claim_begin_hook = None;
    l.copy_claim_end_hook = None;
}
pub(crate) fn native_young_return_fault(
    g: &mut crate::NativeCodeGenerator,
    actor: TestActor,
    mode: u64,
) {
    let rt = g.gc_gen.take().unwrap();
    worker::lock(g, &rt.labels);
    match mode {
        0 => store_rt_imm(g, young_mark::VALID, 0),
        1 | 2 => {
            let field = if mode == 1 {
                allocation_records::REVISION
            } else {
                young_mark::EPOCH
            };
            load_rt(g, R::V0, field);
            g.add_reg_imm32(R::V0, if mode == 1 { 2 } else { 1 });
            store_rt(g, field, R::V0);
        }
        4 => {
            load_rt(g, R::V0, RT_RY_HI);
            store_rt(g, RT_RY_LO, R::V0);
        }
        _ => (),
    }
    worker::unlock(g, &rt.labels);
    g.gc_gen = Some(rt);
    select(g, actor);
    if mode == 3 {
        load_rt(g, R::V0, RT_CTX + offset_of!(MutatorContext, adopted_phase));
        g.add_reg_imm32(R::V0, 0x10000);
        store_rt(g, RT_CTX + offset_of!(MutatorContext, adopted_phase), R::V0);
    }
    let exit = g.gc_gen.as_ref().unwrap().labels.native_exit_actor;
    graph_call(g, exit);
    accepted(g);
}

pub(crate) fn copy_probe_root(
    g: &mut crate::NativeCodeGenerator,
    state: crate::DataLabel,
    root: crate::DataLabel,
) {
    g.mov_data_addr(R::V1, root);
    g.load_ptr_disp32(R::V0, R::V1, 0);
    g.mov_data_addr(R::V1, state);
    g.store_ptr_disp32(R::V1, 128, R::V0);
}

/// Observe real destination publication immediately before the actual source
/// CAS. Only the real mutator forwarder invokes this hook; no ready bit is set.
pub(crate) fn copy_ready_observer(
    g: &mut crate::NativeCodeGenerator,
    state: crate::DataLabel,
) -> crate::DataLabel {
    let mut rt = g.gc_gen.take().unwrap();
    g.asm.data.resize(g.asm.data.len().next_multiple_of(8), 0);
    let data = g.asm.data_label_with_i64s(&[0; 5]);
    let hook = g.create_text_label();
    let body = g.create_text_label();
    rt.labels.copy_ready_hook = Some(hook);
    g.jmp_label(body);
    g.bind_text_label(hook);
    quiescence::save(g);
    let done = g.create_text_label();
    load(g, state, ENABLED);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::Equal, done);
    load_rt(g, R::V0, RT_SCAN_NEW);
    g.shr_reg_imm8(R::V0, 16);
    load_rt(g, R::V1, RT_HEAP_BASE);
    g.add_reg_reg(R::V0, R::V1);
    g.add_reg_imm32(
        R::V0,
        offset_of!(ObjectHeader, state) as i32 - size_of::<ObjectHeader>() as i32,
    );
    g.atomic_load_acquire(R::V0, R::V0);
    g.mov_data_addr(R::V2, data);
    g.store_ptr_disp32(R::V2, 0, R::V0);
    g.cmp_reg_imm32(R::V0, crate::gc_abi::OBJECT_PUBLISHED as i32);
    g.jcc_label(C::NotEqual, rt.labels.fail);
    load_rt(g, R::V0, RT_PENDING_REF);
    g.store_ptr_disp32(R::V2, 8, R::V0);
    load_rt(
        g,
        R::V0,
        RT_CTX + offset_of!(MutatorContext, private_producers),
    );
    g.store_ptr_disp32(R::V2, 16, R::V0);
    load_rt(g, R::V0, RT_SCAN_OLD);
    g.add_reg_imm32(R::V0, copy::DESCRIPTOR);
    g.atomic_load_acquire(R::V0, R::V0);
    g.store_ptr_disp32(R::V2, 24, R::V0);
    load_rt(g, R::V0, RT_M_COPYING);
    g.store_ptr_disp32(R::V2, 32, R::V0);
    g.bind_text_label(done);
    quiescence::restore(g);
    g.ret();
    g.bind_text_label(body);
    g.gc_gen = Some(rt);
    data
}
pub(crate) fn copy_ready_metric(
    g: &mut crate::NativeCodeGenerator,
    data: crate::DataLabel,
    field: usize,
) {
    load(g, data, field as i32 * 8);
}
pub(crate) fn resolve_copy(g: &mut crate::NativeCodeGenerator, root: crate::DataLabel) {
    let entry = g.gc_gen.as_ref().unwrap().labels.field_load_actor;
    g.mov_data_addr(R::V1, root);
    g.load_ptr_disp32(R::V10, R::V1, 0);
    g.atomic_load_acquire(R::V0, R::V10);
    graph_call(g, entry);
}

pub(crate) fn full_copy_wait_parked(g: &mut crate::NativeCodeGenerator, data: crate::DataLabel) {
    let rt = g.gc_gen.take().unwrap();
    worker::wake_if_pending(g, &rt.labels);
    let w = rt.labels.worker.as_ref().unwrap();
    deadline(g, w.hooks.clock);
    wait(
        g,
        data,
        PARKED,
        1,
        w.hooks.clock,
        w.hooks.relax,
        rt.labels.fail,
    );
    g.gc_gen = Some(rt);
}
