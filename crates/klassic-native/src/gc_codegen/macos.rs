//! Native Darwin ARM64 worker/TLS/event emission. libSystem owns pthread stacks
//! and TLS. These helpers run outside heap-access guards and use native AAPCS64.
use super::{Assembler, BranchKind as B, Cond, Label, Reg as R};
use crate::macho::ImportSymbol as I;

/// Immutable launch record, retained until successful pthread_join. Context is
/// a pointer to the common MutatorContext or worker payload, not a second ABI.
#[repr(C)]
#[allow(dead_code)]
pub(super) struct WorkerLaunch {
    entry: u64,
    context: u64,
    key: u64,
}

/// LP64 Darwin pthread_mutex_t is 64 bytes and pthread_cond_t is 48 bytes,
/// including their 8-byte signatures. Keep this storage nonmoving; never copy
/// initialized pthread objects. Owner initializes before publication and destroys
/// after every user has joined. `initialized` tracks partial ownership for retry.
#[repr(C)]
#[allow(dead_code)]
pub(super) struct ParkEvent {
    mutex: [u64; 8],
    condition: [u64; 6],
    notified: u64,
    initialized: u64,
}
const COND: u32 = 64;
const NOTIFIED: u32 = 112;
const INITIALIZED: u32 = 120;

fn prologue(a: &mut Assembler) {
    a.push_frame_record();
    a.push(R::X19);
    a.push(R::X20);
}
fn epilogue(a: &mut Assembler) {
    a.pop(R::X20);
    a.pop(R::X19);
    a.pop_frame_record();
    a.ret();
}
fn call_int(a: &mut Assembler, symbol: I) {
    a.call_import(symbol);
    a.word(0x2a00_03e0); // mov w0, w0: int return, unspecified high half
}
fn address(a: &mut Assembler, dst: R, offset: u32) {
    a.mov_imm64(R::X10, offset as u64);
    a.add_reg(dst, R::X19, R::X10);
}
fn write_state(a: &mut Assembler, state: u64) {
    a.mov_imm64(R::X1, state);
    a.str_imm(R::X1, R::X19, INITIALIZED);
}

/// `(bytes) -> pointer_or_null`, with 16 KiB Apple Silicon page granularity.
/// The owner reserves budget first and calls outside locks/source guards.
#[allow(dead_code)]
pub(super) fn emit_metadata_map(a: &mut Assembler) {
    let fail = a.new_label();
    let done = a.new_label();
    prologue(a);
    a.branch(fail, B::CompareZero(R::X0));
    a.mov_imm64(R::X9, 16383);
    a.and_reg(R::X9, R::X0, R::X9);
    a.branch(fail, B::CompareNonZero(R::X9));
    a.mov_reg(R::X1, R::X0);
    a.mov_imm64(R::X0, 0);
    a.mov_imm64(R::X2, 3); // PROT_READ | PROT_WRITE
    a.mov_imm64(R::X3, 0x1002); // MAP_ANON | MAP_PRIVATE
    a.mov_imm64(R::X4, u64::MAX); // fd=-1
    a.mov_imm64(R::X5, 0);
    a.call_import(I::Mmap);
    a.mov_imm64(R::X1, u64::MAX); // MAP_FAILED, not an int return
    a.cmp_reg(R::X0, R::X1);
    a.branch(done, B::Conditional(Cond::Ne));
    a.bind(fail);
    a.mov_imm64(R::X0, 0);
    a.bind(done);
    epilogue(a);
}

/// Native AAPCS64 `(reserved_charge, spec) -> OK/INVALID/OS_ERROR`.
/// Prepare backing in native state; budget commit and registration stay separate.
#[allow(dead_code)]
pub(super) fn emit_roots_prepare(a: &mut Assembler) {
    let map = a.new_label();
    a.mov_reg(R::X5, R::X0);
    a.mov_reg(R::X4, R::X1);
    crate::gc_codegen::native_roots::emit_prepare(a, 16384, map);
    a.bind(map);
    a.mov_reg(R::X0, R::X5);
    emit_metadata_map(a);
}

/// Native AAPCS64 owned-charge disposal, after exclusive retirement.
#[allow(dead_code)]
pub(super) fn emit_roots_dispose(a: &mut Assembler) {
    let unmap = a.new_label();
    a.mov_reg(R::X5, R::X0);
    crate::gc_codegen::native_roots::emit_dispose(a, 16384, unmap);
    a.bind(unmap);
    a.mov_reg(R::X0, R::X5);
    a.mov_reg(R::X1, R::X4);
    emit_metadata_unmap(a);
}

/// `(base, bytes) -> bool`. Failure retains the mapping for owner retry.
#[allow(dead_code)]
pub(super) fn emit_metadata_unmap(a: &mut Assembler) {
    let fail = a.new_label();
    let done = a.new_label();
    prologue(a);
    for reg in [R::X0, R::X1] {
        a.branch(fail, B::CompareZero(reg));
        a.mov_imm64(R::X9, 16383);
        a.and_reg(R::X9, reg, R::X9);
        a.branch(fail, B::CompareNonZero(R::X9));
    }
    call_int(a, I::Munmap);
    a.branch(fail, B::CompareNonZero(R::X0));
    a.mov_imm64(R::X0, 1);
    a.branch(done, B::Unconditional);
    a.bind(fail);
    a.mov_imm64(R::X0, 0);
    a.bind(done);
    epilogue(a);
}

/// Native `(base, bytes) -> bool`, matching the C POSIX decommit adapter.
/// Darwin MADV_FREE=5: apple-oss-distributions/xnu bsd/sys/mman.h.
#[allow(dead_code)]
pub(super) fn emit_heap_decommit(a: &mut Assembler) {
    let fail = a.new_label();
    let done = a.new_label();
    prologue(a);
    for reg in [R::X0, R::X1] {
        a.branch(fail, B::CompareZero(reg));
        a.mov_imm64(R::X9, 16383);
        a.and_reg(R::X9, reg, R::X9);
        a.branch(fail, B::CompareNonZero(R::X9));
    }
    a.add_reg(R::X9, R::X0, R::X1);
    a.cmp_reg(R::X9, R::X0);
    a.branch(fail, B::Conditional(Cond::Cc));
    a.mov_reg(R::X19, R::X0);
    a.mov_reg(R::X20, R::X1);
    a.mov_imm64(R::X2, 0);
    call_int(a, I::Mprotect);
    a.branch(fail, B::CompareNonZero(R::X0));
    a.mov_reg(R::X0, R::X19);
    a.mov_reg(R::X1, R::X20);
    a.mov_imm64(R::X2, 5);
    call_int(a, I::Madvise);
    a.branch(fail, B::CompareNonZero(R::X0));
    a.mov_imm64(R::X0, 1);
    a.branch(done, B::Unconditional);
    a.bind(fail);
    a.mov_imm64(R::X0, 0);
    a.bind(done);
    epilogue(a);
}

/// `(launch*, pthread_t_out*) -> errno_or_0`. pthread_create supplies the native
/// stack and guard. A failed startup always leaves the output null. A successful
/// startup still needs join and an exit-status check (TLS setup can fail later).
#[allow(dead_code)]
pub(super) fn emit_start(a: &mut Assembler, thunk: Label) {
    let done = a.new_label();
    prologue(a);
    a.mov_reg(R::X19, R::X1);
    a.mov_reg(R::X3, R::X0);
    a.mov_reg(R::X0, R::X19);
    a.mov_imm64(R::X1, 0);
    a.str_imm(R::X1, R::X19, 0);
    a.branch(thunk, B::Address(R::X2));
    call_int(a, I::PthreadCreate);
    a.branch(done, B::CompareZero(R::X0));
    a.mov_imm64(R::X1, 0);
    a.str_imm(R::X1, R::X19, 0);
    a.bind(done);
    epilogue(a);
}

/// `(launch*) -> void* status`, used as the pthread callback. The entry shares
/// that status convention. TLS failure bypasses entry or overrides success;
/// returning from this callback exits only the worker thread.
#[allow(dead_code)]
pub(super) fn emit_thunk(a: &mut Assembler) {
    let done = a.new_label();
    prologue(a);
    a.mov_reg(R::X19, R::X0);
    a.ldr_imm(R::X0, R::X19, 16);
    a.ldr_imm(R::X1, R::X19, 8);
    call_int(a, I::PthreadSetSpecific);
    a.branch(done, B::CompareNonZero(R::X0));
    a.ldr_imm(R::X0, R::X19, 8);
    a.ldr_imm(R::X16, R::X19, 0);
    a.word(0xd63f_0200); // blr x16: entry(context)
    a.mov_reg(R::X20, R::X0);
    a.ldr_imm(R::X0, R::X19, 16);
    a.mov_imm64(R::X1, 0);
    call_int(a, I::PthreadSetSpecific);
    a.branch(done, B::CompareNonZero(R::X0));
    a.mov_reg(R::X0, R::X20);
    a.bind(done);
    epilogue(a);
}

/// Native pthread signatures. Key storage is u64, and keys outlive every worker
/// using them. Join retains thread/launch ownership on failure. No errno lookup:
/// pthread APIs return the error directly; getspecific returns a pointer.
#[allow(dead_code)]
pub(super) fn emit_leaf(a: &mut Assembler, api: I) {
    assert!(matches!(
        api,
        I::PthreadJoin
            | I::PthreadKeyCreate
            | I::PthreadKeyDelete
            | I::PthreadSetSpecific
            | I::PthreadGetSpecific
    ));
    a.push_frame_record();
    if api == I::PthreadGetSpecific {
        a.call_import(api);
    } else {
        call_int(a, api);
    }
    a.pop_frame_record();
    a.ret();
}

#[allow(dead_code)]
pub(super) fn emit_join(a: &mut Assembler) {
    emit_leaf(a, I::PthreadJoin);
}
#[allow(dead_code)]
pub(super) fn emit_key_create(a: &mut Assembler) {
    emit_leaf(a, I::PthreadKeyCreate);
}
#[allow(dead_code)]
pub(super) fn emit_key_delete(a: &mut Assembler) {
    emit_leaf(a, I::PthreadKeyDelete);
}

/// `(zero_initialized_event*) -> errno_or_0`. On failure, `initialized` says
/// which resources remain owned; destroy can retry their cleanup. Never publish
/// an event until initialization succeeded (mask == 3).
#[allow(dead_code)]
pub(super) fn emit_event_init(a: &mut Assembler) {
    let invalid = a.new_label();
    let done = a.new_label();
    let rollback = a.new_label();
    prologue(a);
    a.mov_reg(R::X19, R::X0);
    a.ldr_imm(R::X1, R::X19, INITIALIZED);
    a.branch(invalid, B::CompareNonZero(R::X1));
    a.mov_imm64(R::X1, 0);
    a.str_imm(R::X1, R::X19, NOTIFIED);
    call_int(a, I::PthreadMutexInit);
    a.branch(done, B::CompareNonZero(R::X0));
    write_state(a, 1);
    address(a, R::X0, COND);
    a.mov_imm64(R::X1, 0);
    call_int(a, I::PthreadCondInit);
    a.branch(rollback, B::CompareNonZero(R::X0));
    write_state(a, 3);
    a.branch(done, B::Unconditional);
    a.bind(rollback);
    a.mov_reg(R::X20, R::X0);
    a.mov_reg(R::X0, R::X19);
    call_int(a, I::PthreadMutexDestroy);
    a.branch(done, B::CompareNonZero(R::X0)); // still owns the mutex; preserve mask 1
    write_state(a, 0);
    a.mov_reg(R::X0, R::X20);
    a.branch(done, B::Unconditional);
    a.bind(invalid);
    a.mov_imm64(R::X0, 22); // EINVAL
    a.bind(done);
    epilogue(a);
}

/// Destroy only quiescent events. Clear each ownership bit after that destroy
/// succeeds, so a failed later destroy can be retried without double destruction.
#[allow(dead_code)]
pub(super) fn emit_event_destroy(a: &mut Assembler) {
    let done = a.new_label();
    prologue(a);
    a.mov_reg(R::X19, R::X0);
    for (bit, offset, api) in [
        (2_u64, COND, I::PthreadCondDestroy),
        (1, 0, I::PthreadMutexDestroy),
    ] {
        let next = a.new_label();
        a.ldr_imm(R::X1, R::X19, INITIALIZED);
        a.mov_imm64(R::X2, bit);
        a.and_reg(R::X1, R::X1, R::X2);
        a.branch(next, B::CompareZero(R::X1));
        address(a, R::X0, offset);
        call_int(a, api);
        a.branch(done, B::CompareNonZero(R::X0));
        a.ldr_imm(R::X1, R::X19, INITIALIZED);
        a.mov_imm64(R::X2, !bit);
        a.and_reg(R::X1, R::X1, R::X2);
        a.str_imm(R::X1, R::X19, INITIALIZED);
        a.bind(next);
    }
    a.mov_imm64(R::X0, 0);
    a.bind(done);
    epilogue(a);
}

/// One coalescing notification per worker, protected by a mutex. Recheck the
/// predicate in a loop: a wake can precede park and cond_wait may wake spuriously.
/// Callers may not hold heap-access guards across this blocking operation.
#[allow(dead_code)]
pub(super) fn emit_park(a: &mut Assembler) {
    emit_notification(a, false);
}
#[allow(dead_code)]
pub(super) fn emit_wake(a: &mut Assembler) {
    emit_notification(a, true);
}

fn emit_notification(a: &mut Assembler, wake: bool) {
    let invalid = a.new_label();
    let unlock = a.new_label();
    let done = a.new_label();
    prologue(a);
    a.mov_reg(R::X19, R::X0);
    a.ldr_imm(R::X1, R::X19, INITIALIZED);
    a.mov_imm64(R::X2, 3);
    a.cmp_reg(R::X1, R::X2);
    a.branch(invalid, B::Conditional(Cond::Ne));
    call_int(a, I::PthreadMutexLock);
    a.branch(done, B::CompareNonZero(R::X0));
    a.mov_imm64(R::X20, 0);
    if wake {
        a.mov_imm64(R::X1, 1);
        a.str_imm(R::X1, R::X19, NOTIFIED);
        address(a, R::X0, COND);
        call_int(a, I::PthreadCondSignal);
        a.mov_reg(R::X20, R::X0);
    } else {
        let recheck = a.new_label();
        let ready = a.new_label();
        a.bind(recheck);
        a.ldr_imm(R::X1, R::X19, NOTIFIED);
        a.branch(ready, B::CompareNonZero(R::X1));
        address(a, R::X0, COND);
        a.mov_reg(R::X1, R::X19);
        call_int(a, I::PthreadCondWait);
        a.mov_reg(R::X20, R::X0);
        a.branch(unlock, B::CompareNonZero(R::X0));
        a.branch(recheck, B::Unconditional);
        a.bind(ready);
        a.mov_imm64(R::X1, 0);
        a.str_imm(R::X1, R::X19, NOTIFIED);
    }
    a.bind(unlock);
    a.mov_reg(R::X0, R::X19);
    call_int(a, I::PthreadMutexUnlock);
    a.branch(done, B::CompareZero(R::X20));
    a.mov_reg(R::X0, R::X20); // first failure takes precedence over unlock failure
    a.branch(done, B::Unconditional);
    a.bind(invalid);
    a.mov_imm64(R::X0, 22);
    a.bind(done);
    epilogue(a);
}

#[test]
fn gc_macos_launch_and_event_layout() {
    use std::mem::{align_of, offset_of, size_of};
    assert_eq!(size_of::<WorkerLaunch>(), 24);
    assert_eq!(offset_of!(WorkerLaunch, key), 16);
    assert_eq!(size_of::<ParkEvent>(), 128);
    assert_eq!(align_of::<ParkEvent>(), 8);
    assert_eq!(offset_of!(ParkEvent, condition), 64);
    assert_eq!(offset_of!(ParkEvent, notified), 112);
    assert_eq!(offset_of!(ParkEvent, initialized), 120);
}

#[allow(dead_code)]
pub(super) fn emit_builtin_registry_prepare(a: &mut Assembler) {
    let map = a.new_label();
    a.mov_reg(R::X5, R::X0);
    a.mov_reg(R::X4, R::X1);
    crate::gc_codegen::native_builtin_registry::emit_prepare(a, 16384, map);
    a.bind(map);
    a.mov_reg(R::X0, R::X5);
    emit_metadata_map(a);
}

#[allow(dead_code)]
pub(super) fn emit_startup_prepare(a: &mut Assembler) {
    let map = a.new_label();
    a.mov_reg(R::X5, R::X0);
    a.mov_reg(R::X4, R::X1);
    crate::gc_codegen::root_lifetime::native_startup::emit_prepare(a, 16384, map);
    a.bind(map);
    a.mov_reg(R::X0, R::X5);
    emit_metadata_map(a);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::aarch64::DataLabel;
    use crate::portable_asm::{PortableAsm, Reg as P};

    fn checked_call(a: &mut Assembler, function: Label, failed: Label) {
        a.branch(function, B::Link);
        a.branch(failed, B::CompareNonZero(R::X0));
    }

    // All helpers and both worker callbacks are emitted into this standalone
    // image. The native test exercises pthreads; the optional instruction
    // emulator exercises error paths with an explicitly modeled OS API.
    fn fixture() -> (Vec<u8>, String) {
        let mut a = Assembler::default();
        let names = [
            "start",
            "thunk",
            "join",
            "key_create",
            "key_delete",
            "tls_set",
            "tls_get",
            "event_init",
            "event_destroy",
            "park",
            "wake",
            "entry",
            "metadata_map",
            "metadata_unmap",
            "heap_decommit",
            "native_roots_prepare",
            "native_roots_dispose",
            "native_builtin_registry_prepare",
            "native_startup_prepare",
        ];
        let labels: Vec<_> = names.iter().map(|_| a.new_label()).collect();
        let [
            start,
            thunk,
            join,
            key_create,
            key_delete,
            tls_set,
            tls_get,
            event_init,
            event_destroy,
            park,
            wake,
            entry,
            metadata_map,
            metadata_unmap,
            _heap_decommit,
            _native_roots_prepare,
            _native_roots_dispose,
            _native_builtin_registry_prepare,
            _native_startup_prepare,
        ] = labels[..]
        else {
            unreachable!()
        };
        let failed = a.new_label();
        let metadata = a.reserve_data_cells(1);
        a.mov_imm64(R::X0, 16384);
        a.branch(metadata_map, B::Link);
        a.branch(failed, B::CompareZero(R::X0));
        a.load_data_address(R::X1, metadata);
        a.str_imm(R::X0, R::X1, 0);
        a.ldr_imm(R::X1, R::X0, 0);
        a.branch(failed, B::CompareNonZero(R::X1));
        a.mov_imm64(R::X1, 0x51);
        a.str_imm(R::X1, R::X0, 0);
        let context_step = a.new_label();
        let context =
            a.reserve_data_cells(std::mem::size_of::<crate::gc_abi::MutatorContext>() / 8);
        let key = a.reserve_data_cells(1);
        let counter = a.reserve_data_cells(1);
        let parent_tls = a.reserve_data_cells(1);
        let tids = a.reserve_data_cells(2);
        let results = a.reserve_data_cells(2);
        let contexts = [a.reserve_data_cells(8), a.reserve_data_cells(8)];
        let launches = [a.reserve_data_cells(3), a.reserve_data_cells(3)];
        let events = [a.reserve_data_cells(16), a.reserve_data_cells(16)];

        // This isolated ABI context is not registered. Exercise the same state
        // publication body as C before starting the OS worker fixture.
        a.load_data_address(R::X19, context);
        a.mov_imm64(R::X1, crate::gc_abi::ABI_VERSION as u64);
        a.str_imm(R::X1, R::X19, 0); // unpublished version + initial Running
        a.mov_imm64(R::X1, 0x1234_5678_8765_4321);
        a.str_imm(R::X1, R::X19, 8);
        for (expected, desired) in [(0, 2), (2, 0), (0, 1), (1, 0), (0, 3), (3, 4)] {
            a.mov_reg(R::X0, R::X19);
            a.mov_imm64(R::X1, expected);
            a.mov_imm64(R::X2, desired);
            checked_call(&mut a, context_step, failed);
            a.ldr_imm(R::X0, R::X19, 0);
            a.mov_imm64(R::X1, (desired << 32) | crate::gc_abi::ABI_VERSION as u64);
            a.cmp_reg(R::X0, R::X1);
            a.branch(failed, B::Conditional(Cond::Ne));
            a.ldr_imm(R::X0, R::X19, 8);
            a.mov_imm64(R::X1, 0x1234_5678_8765_4321);
            a.cmp_reg(R::X0, R::X1);
            a.branch(failed, B::Conditional(Cond::Ne));
        }

        a.load_data_address(R::X0, key);
        a.mov_imm64(R::X1, 0);
        checked_call(&mut a, key_create, failed);
        a.load_data_address(R::X0, key);
        a.ldr_imm(R::X0, R::X0, 0);
        a.load_data_address(R::X1, parent_tls);
        checked_call(&mut a, tls_set, failed);
        for i in 0..2 {
            a.load_data_address(R::X0, events[i]);
            checked_call(&mut a, event_init, failed);
            // Notification delivered before park must persist and be consumed.
            a.load_data_address(R::X0, events[i]);
            checked_call(&mut a, wake, failed);
            a.load_data_address(R::X0, events[i]);
            checked_call(&mut a, park, failed);
            a.load_data_address(R::X19, contexts[i]);
            a.load_data_address(R::X1, events[i]);
            a.str_imm(R::X1, R::X19, 0);
            a.load_data_address(R::X1, key);
            a.ldr_imm(R::X1, R::X1, 0);
            a.str_imm(R::X1, R::X19, 8);
            a.load_data_address(R::X1, counter);
            a.str_imm(R::X1, R::X19, 16);
            a.mov_imm64(R::X1, 1 << i);
            a.str_imm(R::X1, R::X19, 24);
            a.load_data_address(R::X0, launches[i]);
            a.branch(entry, B::Address(R::X1));
            a.str_imm(R::X1, R::X0, 0);
            a.str_imm(R::X19, R::X0, 8);
            a.ldr_imm(R::X1, R::X19, 8);
            a.str_imm(R::X1, R::X0, 16);
            a.load_data_address(R::X1, DataLabel(tids.0 + i * 8));
            checked_call(&mut a, start, failed);
        }
        for event in events {
            a.load_data_address(R::X0, event);
            checked_call(&mut a, wake, failed);
        }
        for (i, context) in contexts.into_iter().enumerate() {
            a.load_data_address(R::X0, DataLabel(tids.0 + i * 8));
            a.ldr_imm(R::X0, R::X0, 0);
            a.load_data_address(R::X1, DataLabel(results.0 + i * 8));
            checked_call(&mut a, join, failed);
            a.load_data_address(R::X0, DataLabel(results.0 + i * 8));
            a.ldr_imm(R::X0, R::X0, 0);
            a.branch(failed, B::CompareNonZero(R::X0));
            a.load_data_address(R::X19, context);
            a.ldr_imm(R::X0, R::X19, 32);
            a.cmp_reg(R::X0, R::X19);
            a.branch(failed, B::Conditional(Cond::Ne));
            a.ldr_imm(R::X0, R::X19, 48);
            a.mov_imm64(R::X1, 15);
            a.and_reg(R::X0, R::X0, R::X1);
            a.branch(failed, B::CompareNonZero(R::X0));
        }
        a.load_data_address(R::X0, counter);
        a.ldr_imm(R::X0, R::X0, 0);
        a.mov_imm64(R::X1, 3);
        a.cmp_reg(R::X0, R::X1);
        a.branch(failed, B::Conditional(Cond::Ne));
        // Exactly one OR saw zero; the other saw the first worker's bit.
        a.load_data_address(R::X0, contexts[0]);
        a.ldr_imm(R::X0, R::X0, 40);
        a.load_data_address(R::X1, contexts[1]);
        a.ldr_imm(R::X1, R::X1, 40);
        let second_won = a.new_label();
        let ordered = a.new_label();
        a.branch(second_won, B::CompareNonZero(R::X0));
        a.mov_imm64(R::X2, 1);
        a.cmp_reg(R::X1, R::X2);
        a.branch(failed, B::Conditional(Cond::Ne));
        a.branch(ordered, B::Unconditional);
        a.bind(second_won);
        a.branch(failed, B::CompareNonZero(R::X1));
        a.mov_imm64(R::X2, 2);
        a.cmp_reg(R::X0, R::X2);
        a.branch(failed, B::Conditional(Cond::Ne));
        a.bind(ordered);
        a.load_data_address(R::X0, key);
        a.ldr_imm(R::X0, R::X0, 0);
        a.branch(tls_get, B::Link);
        a.load_data_address(R::X1, parent_tls);
        a.cmp_reg(R::X0, R::X1);
        a.branch(failed, B::Conditional(Cond::Ne));
        for event in events {
            for _ in 0..2 {
                // already-destroyed retry is harmless
                a.load_data_address(R::X0, event);
                checked_call(&mut a, event_destroy, failed);
            }
        }
        a.load_data_address(R::X0, key);
        a.ldr_imm(R::X0, R::X0, 0);
        checked_call(&mut a, key_delete, failed);
        a.load_data_address(R::X0, metadata);
        a.ldr_imm(R::X0, R::X0, 0);
        a.ldr_imm(R::X1, R::X0, 0);
        a.mov_imm64(R::X2, 0x51);
        a.cmp_reg(R::X1, R::X2);
        a.branch(failed, B::Conditional(Cond::Ne));
        a.mov_imm64(R::X1, 16384);
        a.branch(metadata_unmap, B::Link);
        a.branch(failed, B::CompareZero(R::X0));
        a.mov_imm64(R::X0, 0);
        a.mov_imm64(R::X16, super::super::SYS_EXIT as u64);
        a.svc_0x80();
        a.bind(failed);
        a.mov_imm64(R::X0, 1);
        a.mov_imm64(R::X16, super::super::SYS_EXIT as u64);
        a.svc_0x80();
        for (&name, &label) in names.iter().zip(&labels) {
            a.bind(label);
            match name {
                "metadata_map" => emit_metadata_map(&mut a),
                "metadata_unmap" => emit_metadata_unmap(&mut a),
                "heap_decommit" => emit_heap_decommit(&mut a),
                "native_roots_prepare" => emit_roots_prepare(&mut a),
                "native_roots_dispose" => emit_roots_dispose(&mut a),
                "native_builtin_registry_prepare" => emit_builtin_registry_prepare(&mut a),
                "native_startup_prepare" => emit_startup_prepare(&mut a),
                "start" => emit_start(&mut a, thunk),
                "thunk" => emit_thunk(&mut a),
                "join" => emit_join(&mut a),
                "key_create" => emit_key_create(&mut a),
                "key_delete" => emit_key_delete(&mut a),
                "tls_set" => emit_leaf(&mut a, I::PthreadSetSpecific),
                "tls_get" => emit_leaf(&mut a, I::PthreadGetSpecific),
                "event_init" => emit_event_init(&mut a),
                "event_destroy" => emit_event_destroy(&mut a),
                "park" => emit_park(&mut a),
                "wake" => emit_wake(&mut a),
                "entry" => {
                    a.word(0x9100_03e1); // mov x1, sp before prologue
                    prologue(&mut a);
                    a.mov_reg(R::X19, R::X0);
                    a.str_imm(R::X1, R::X19, 48);
                    a.ldr_imm(R::X0, R::X19, 8);
                    a.branch(tls_get, B::Link);
                    a.str_imm(R::X0, R::X19, 32);
                    a.ldr_imm(R::X0, R::X19, 0);
                    a.branch(park, B::Link);
                    let done = a.new_label();
                    a.branch(done, B::CompareNonZero(R::X0));
                    a.ldr_imm(R::X5, R::X19, 16);
                    a.ldr_imm(R::X4, R::X19, 24);
                    a.atomic_fetch_or(P::V7, P::V6);
                    a.str_imm(R::X0, R::X19, 40);
                    a.mov_imm64(R::X0, 0);
                    a.bind(done);
                    epilogue(&mut a);
                }
                _ => unreachable!(),
            }
        }
        let mut offsets = String::from("main\t0\n");
        for (name, label) in names.iter().zip(labels) {
            offsets.push_str(&format!("{name}\t{}\n", a.labels[label.0].unwrap()));
        }
        use crate::gc_atomic_tests::Operation;
        for (name, operation) in [
            ("context_transition", Operation::ContextTransition),
            ("atomic_load32", Operation::Load32),
            ("atomic_store32", Operation::Store32),
            ("atomic_cas32", Operation::Cas32),
            ("handshake_step", Operation::HandshakeStep),
            ("tlab_allocate", Operation::TlabAllocate),
            ("pool_step", Operation::PoolStep),
            ("tlab_page", Operation::TlabPage),
            ("descriptor_register", Operation::DescriptorRegister),
            ("descriptor_scan", Operation::DescriptorScan),
            ("root_step", Operation::RootStep),
            ("root_scan", Operation::RootScan),
            ("root_drain", Operation::RootDrain),
            ("root_life", Operation::RootLife),
            ("root_clear", Operation::RootClear),
            ("root_scan_leased", Operation::RootScanLeased),
            ("root_walk", Operation::RootWalk),
            ("root_walk_scan", Operation::RootWalkScan),
            ("root_walk_heal", Operation::RootWalkHeal),
            ("root_walk_item", Operation::RootWalkItem),
            ("relocation_gate", Operation::RelocationGate),
            ("pin_page", Operation::PinPage),
            ("pin_registry_bind", Operation::PinRegistryBind),
            ("pin_registry_unbind", Operation::PinRegistryUnbind),
            ("pin_registry_pin", Operation::PinRegistryPin),
            ("pin_registry_unpin", Operation::PinRegistryUnpin),
            ("pin_registry_select", Operation::PinRegistrySelect),
            ("pin_registry_reopen", Operation::PinRegistryReopen),
            ("pin_selection", Operation::PinSelection),
            ("pin_relocation_claim", Operation::PinRelocationClaim),
            ("pin_source_drain", Operation::PinSourceDrain),
            ("pin_source_uncommit", Operation::PinSourceUncommit),
            ("root_pin_prepare", Operation::RootPinPrepare),
            ("root_pin_enter", Operation::RootPinEnter),
            ("root_pin_exit", Operation::RootPinExit),
            ("root_pin_cleanup", Operation::RootPinCleanup),
            ("root_pin_cancel", Operation::RootPinCancel),
            ("root_pin_finish", Operation::RootPinFinish),
            ("pin_dispatch", Operation::PinDispatch),
            ("native_pin_publish", Operation::NativePinPublish),
            ("native_pin_prepare", Operation::NativePinPrepare),
            ("native_pin_retire", Operation::NativePinRetire),
            ("native_pin_dispose", Operation::NativePinDispose),
            ("native_pin_release", Operation::NativePinRelease),
            ("native_pin_close", Operation::NativePinClose),
            ("native_pin_progress", Operation::NativePinProgress),
            ("native_pin_wait", Operation::NativePinWait),
            ("native_source_uncommit", Operation::NativeSourceUncommit),
            ("root_finish_notify", Operation::RootFinishNotify),
            ("relocation_job", Operation::RelocationJob),
            ("relocation_complete", Operation::RelocationComplete),
            ("relocation_claim", Operation::RelocationClaim),
            ("marked_relocation", Operation::MarkedRelocation),
            ("relocate_discard", Operation::RelocateDiscard),
            ("relocation_cancel", Operation::RelocationCancel),
            ("alloc_into", Operation::AllocInto),
            ("detach_step", Operation::DetachStep),
            ("buffer_step", Operation::BufferStep),
            ("buffer_drain", Operation::BufferDrain),
            ("metadata_step", Operation::MetadataStep),
            ("budget_step", Operation::BudgetStep),
            ("reference_lease", Operation::ReferenceLease),
            ("forward_step", Operation::ForwardStep),
            ("source_access", Operation::SourceAccess),
            ("forward_lookup", Operation::ForwardLookup),
            ("resolve_access", Operation::ResolveAccess),
            ("relocate_copy", Operation::RelocateCopy),
            ("relocate_publish", Operation::RelocatePublish),
            ("relocate_target", Operation::RelocateTarget),
            ("page_release", Operation::PageRelease),
            ("guarded_store", Operation::GuardedStore),
            ("guarded_observe", Operation::GuardedObserve),
            ("guarded_identity", Operation::GuardedIdentity),
            ("guarded_block_size", Operation::GuardedBlockSize),
            ("guarded_logical_cas", Operation::GuardedLogicalCas),
            ("guarded_load", Operation::GuardedLoad),
            ("guarded_scalar", Operation::GuardedScalar),
            ("guarded_heal", Operation::GuardedHeal),
            ("root_follow", Operation::RootFollow),
            ("pending_follow", Operation::PendingFollow),
            ("pending_access", Operation::PendingAccess),
            ("pending_mark", Operation::PendingMark),
            ("weak_load", Operation::WeakLoad),
            ("weak_release", Operation::WeakRelease),
            ("weak_follow", Operation::WeakFollow),
            ("weak_incoming", Operation::WeakIncoming),
            ("weak_table_access", Operation::WeakTableAccess),
            ("weak_create", Operation::WeakCreate),
            ("weak_mark", Operation::WeakMark),
            ("weak_capture", Operation::WeakCapture),
            ("weak_access", Operation::WeakAccess),
            ("weak_finish", Operation::WeakFinish),
            ("weak_scan", Operation::WeakScan),
            ("weak_relocation", Operation::WeakRelocation),
            ("weak_liveness", Operation::WeakLiveness),
            ("survivor_age", Operation::SurvivorAge),
            ("marked_survivor_age", Operation::MarkedSurvivorAge),
            ("promotion_remember", Operation::PromotionRemember),
            ("relocation_turn", Operation::RelocationTurn),
            ("forward_compress", Operation::ForwardCompress),
            ("heap_incoming", Operation::HeapIncoming),
            ("heap_walk", Operation::HeapWalk),
            ("object_walk", Operation::ObjectWalk),
            ("heap_audit_slice", Operation::HeapAuditSlice),
            ("heap_audit", Operation::HeapAudit),
            ("rooted_observation", Operation::RootedObservation),
            ("rooted_scalar", Operation::RootedScalar),
            ("rooted_mark", Operation::RootedMark),
            ("rooted_mark_spill", Operation::RootedMarkSpill),
            ("rooted_mark_set", Operation::RootedMarkSet),
            ("rooted_mark_publish", Operation::RootedMarkPublish),
            ("rooted_load", Operation::RootedLoad),
            ("rooted_value", Operation::RootedValue),
            ("rooted_store_commit", Operation::RootedStoreCommit),
            ("rooted_store_dispatch", Operation::RootedStoreDispatch),
            ("rooted_load_dispatch", Operation::RootedLoadDispatch),
            ("barrier_poll", Operation::BarrierPoll),
            ("barrier_assist", Operation::BarrierAssist),
            ("native_root_publish", Operation::NativeRootPublish),
            ("rooted_store", Operation::RootedStore),
            ("rooted_store_attempt", Operation::RootedStoreAttempt),
            ("rooted_load_admit", Operation::RootedLoadAdmit),
            ("rooted_load_heal", Operation::RootedLoadHeal),
            ("root_access_cleanup", Operation::RootAccessCleanup),
            ("root_access", Operation::RootAccess),
            ("mark_prepare", Operation::MarkPrepare),
            ("mark_retarget", Operation::MarkRetarget),
            ("worker_step", Operation::WorkerStep),
            ("root_journal", Operation::RootJournal),
            ("buffer_job", Operation::BufferJob),
            ("root_audit", Operation::RootAudit),
            ("root_retire_audit", Operation::RootRetireAudit),
            ("mark_end", Operation::MarkEnd),
            ("retire_inventory", Operation::RetireInventory),
            ("mark_phase", Operation::MarkPhase),
            ("cycle_step", Operation::CycleStep),
            ("remembered_step", Operation::RememberedStep),
            ("remembered_inventory", Operation::RememberedInventory),
            ("remembered_load", Operation::RememberedLoad),
            ("remembered_transfer", Operation::RememberedTransfer),
            ("remembered_owner", Operation::RememberedOwner),
            ("remembered_walk", Operation::RememberedWalk),
            ("remembered_lease", Operation::RememberedLease),
            ("mark_route", Operation::MarkRoute),
            ("initialization", Operation::Initialization),
            ("initialization_store", Operation::InitializationStore),
            ("initialization_copy", Operation::InitializationCopy),
            ("metadata_grow", Operation::MetadataGrow),
            ("builtin_prepare", Operation::BuiltinPrepare),
            ("root_storage_prepare", Operation::RootStoragePrepare),
            ("root_storage_can_dispose", Operation::RootStorageCanDispose),
            ("allocation_shape", Operation::AllocationShape),
            ("allocation_start", Operation::AllocationStart),
            ("native_startup", Operation::NativeStartup),
            ("native_startup_release", Operation::NativeStartupRelease),
            ("native_startup_commit", Operation::NativeStartupCommit),
            ("allocation_sample", Operation::AllocationSample),
            ("allocation_pressure", Operation::AllocationPressure),
            ("cycle_pressure", Operation::CyclePressure),
            ("cycle_mark_start", Operation::CycleMarkStart),
            ("relocation_phase", Operation::RelocationPhase),
            ("relocation_retire", Operation::RelocationRetire),
            (
                "heap_allocation_pressure",
                Operation::HeapAllocationPressure,
            ),
            ("root_cancel_empty", Operation::RootCancelEmpty),
            ("root_copy", Operation::RootCopy),
            ("mark_step", Operation::MarkStep),
            ("mark_spill", Operation::MarkSpill),
            ("mark_scan", Operation::MarkScan),
            ("mark_scan_commit", Operation::MarkScanCommit),
            ("source_drain", Operation::SourceDrain),
            ("source_uncommit", Operation::SourceUncommit),
            ("source_retire", Operation::SourceRetire),
            ("forward_incoming", Operation::ForwardIncoming),
        ] {
            if name == "context_transition" {
                a.bind(context_step);
            }
            offsets.push_str(&format!("{name}\t{}\n", a.code.len()));
            a.mov_reg(R::X5, R::X0);
            a.mov_reg(R::X4, R::X1);
            crate::gc_atomic_tests::emit_body(&mut a, operation);
            a.ret();
        }
        a.finish();
        assert_eq!(a.imports.len(), 18);
        let image = crate::macho::write_executable(
            a.code,
            a.rodata,
            &a.fixups,
            "klassic-workers-test",
            a.bss_len as u64,
            &a.imports,
        );
        (image, offsets)
    }

    #[test]
    fn gc_macos_worker_fixture() {
        let (image, offsets) = fixture();
        assert_eq!(&image[..4], &0xfeed_facf_u32.to_le_bytes());
        for symbol in [
            I::Mmap,
            I::Munmap,
            I::Mprotect,
            I::Madvise,
            I::PthreadCreate,
            I::PthreadJoin,
            I::PthreadCondWait,
            I::PthreadKeyDelete,
        ] {
            assert!(
                image
                    .windows(symbol.name().len())
                    .any(|w| w == symbol.name().as_bytes())
            );
        }
        assert_eq!(offsets.lines().count(), 193);
        assert!(offsets.lines().any(|line| line.starts_with("pin_page\t")));
        if let Some(directory) = std::env::var_os("KLASSIC_GC_MACOS_WORKER_DIR") {
            let directory = std::path::PathBuf::from(directory);
            std::fs::create_dir_all(&directory).unwrap();
            std::fs::write(directory.join("worker.macho"), image).unwrap();
            let (metadata, positions) = crate::aarch64::gc_metadata_test_payload();
            std::fs::write(directory.join("metadata.bin"), metadata).unwrap();
            let layout = [
                "map",
                "unmap",
                "map_import",
                "unmap_import",
                "roots_prepare",
                "roots_dispose",
                "builtin_registry_prepare",
                "startup_prepare",
            ]
            .into_iter()
            .zip(positions)
            .map(|(name, offset)| format!("{name}\t{offset}\n"))
            .collect::<String>();
            std::fs::write(directory.join("metadata.tsv"), layout).unwrap();
            std::fs::write(directory.join("functions.tsv"), offsets).unwrap();
            use crate::gc_abi::MutatorContext;
            use std::mem::{offset_of, size_of};
            std::fs::write(
                directory.join("context-layout.tsv"),
                format!(
                    "size\t{}\nstate\t{}\naccess_epoch\t{}\nabi_version\t{}\nallocated_bytes\t{}\nprivate_producers\t{}\n",
                    size_of::<MutatorContext>(),
                    offset_of!(MutatorContext, state),
                    offset_of!(MutatorContext, access_epoch),
                    crate::gc_abi::ABI_VERSION,
                    offset_of!(MutatorContext, allocated_bytes),
                    offset_of!(MutatorContext, private_producers),
                ),
            )
            .unwrap();
        }
    }

    #[test]
    fn gc_macos_adr_forward_and_backward() {
        let mut a = Assembler::default();
        let before = a.new_label();
        let after = a.new_label();
        a.bind(before);
        a.branch(after, B::Address(R::X2)); // +8
        a.branch(before, B::Address(R::X3)); // -4
        a.bind(after);
        a.finish();
        let words: Vec<_> = a
            .code
            .chunks_exact(4)
            .map(|w| u32::from_le_bytes(w.try_into().unwrap()))
            .collect();
        assert_eq!(words, [0x1000_0042, 0x10ff_ffe3]);
    }

    #[cfg(all(target_os = "macos", target_arch = "aarch64"))]
    #[test]
    fn gc_macos_native_workers_tls_events_and_atomic_or() {
        use std::os::unix::fs::PermissionsExt;
        let path = std::env::temp_dir().join(format!("klassic-workers-{}", std::process::id()));
        std::fs::write(&path, fixture().0).unwrap();
        std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o755)).unwrap();
        let output = crate::gc_atomic_tests::run_with_deadline(&path);
        std::fs::remove_file(&path).unwrap();
        assert!(
            output.status.success(),
            "native worker fixture failed: {output:?}"
        );
    }
}
