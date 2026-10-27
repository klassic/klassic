//! Rooted copy-out, legacy borrowed-cursor I/O and explicit owner boundaries.
//! Workers may trace/copy while native code blocks; only the owner can flip
//! phases or retire source storage.
//! Legacy borrowed scopes have no allocation/owner poll or native callback.
//! Standalone string output owns a frame root and reloads it between chunks.
//! Explicit native returns adopt fixed-bank marks, one still-open Young
//! relocation, or a completed relocation receipt covering their root scopes.
//! Opt-in moving rounds also authorize root-only return within their retained
//! source or Retire window. Borrowed cursors never adopt movement; callbacks
//! and pins remain separate protocols.
use super::*;
use crate::gc_codegen::production::{OwnerState, lea_rt, load_rt, owner_state, store_rt};
#[path = "native_adoption.rs"]
pub(super) mod adoption;

#[repr(C)]
pub(super) struct State {
    phase: u64,
    borrowed: u64,
    bank: u64,
    root_scope: u64,
    adoptions: u64,
    entries: u64,
    exits: u64,
    chunks: u64,
    buffer_writes: u64,
}
pub(super) const ENTRIES: usize = owner_state::offset(offset_of!(OwnerState, native_io.entries));
pub(super) const EXITS: usize = owner_state::offset(offset_of!(OwnerState, native_io.exits));
pub(super) const CHUNKS: usize = owner_state::offset(offset_of!(OwnerState, native_io.chunks));
pub(super) const BUFFER_WRITES: usize =
    owner_state::offset(offset_of!(OwnerState, native_io.buffer_writes));
pub(super) const PHASE: usize = owner_state::offset(offset_of!(OwnerState, native_io.phase));
pub(super) const ADOPTIONS: usize =
    owner_state::offset(offset_of!(OwnerState, native_io.adoptions));
pub(super) const BANK: usize = owner_state::offset(offset_of!(OwnerState, native_io.bank));
pub(super) const SCOPE_BORROWED: usize =
    owner_state::offset(offset_of!(OwnerState, native_io.borrowed));
pub(super) const ROOT_SCOPE: usize =
    owner_state::offset(offset_of!(OwnerState, native_io.root_scope));
pub(super) const BORROWS: usize = offset_of!(Runtime, native_borrows);
pub(super) const SAVED: [R; 11] = [
    R::OwnerContext,
    R::V0,
    R::V1,
    R::V2,
    R::V3,
    R::V6,
    R::V7,
    R::V8,
    R::V9,
    R::V10,
    R::V11,
];

pub(super) fn emit<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, runtime: E::DataLabel) {
    quiescence::emit(a, l);
    for (entry, entering, explicit) in [
        (l.native_enter, true, false),
        (l.native_exit, false, false),
        (l.native_enter_actor, true, true),
        (l.native_exit_actor, false, true),
    ] {
        a.bind_text_label(entry);
        for r in SAVED {
            a.push_reg(r);
        }
        if explicit {
            if entering {
                owner_state::bind_running(a, l);
                quiescence::poll(a, l);
            } else {
                owner_state::bind_native_return(a, l);
            }
        } else {
            load_runtime(a, runtime);
        }
        if entering {
            // Initialized pending objects and root journal publications become
            // visible before releasing Running. No private producer may escape.
            a.call_label(l.publish_pending);
        }
        let retry = a.create_text_label();
        let ready = a.create_text_label();
        a.bind_text_label(retry);
        worker::lock(a, l);
        if explicit {
            lea_rt(a, R::V1, quiescence::REQUEST);
            a.atomic_load_acquire(R::V0, R::V1);
            a.test_reg_reg(R::V0, R::V0);
            a.jcc_label(C::Equal, ready);
            worker::unlock(a, l);
            if entering {
                a.call_label(l.owner_checkpoint_poll);
            } else if let Some(w) = &l.worker {
                a.call_label(w.hooks.relax);
            }
            a.jmp_label(retry);
        }
        a.bind_text_label(ready);
        if entering {
            if explicit {
                // Registered walks skip allocation cell zero. An excluded
                // owner cannot retain its old scratch reference across source
                // retirement: its next allocation would journal that dead word
                // into a later active snapshot. Publish and empty it before
                // sealing the journal and releasing Running.
                // A capacity wait can retry this boundary. An empty scratch
                // must not create another 0 -> 0 journal producer each time.
                let empty = a.create_text_label();
                load_rt(a, R::V0, RT_ALLOC_ROOT + HANDLE_CELL);
                a.atomic_load_acquire(R::V0, R::V0);
                a.test_reg_reg(R::V0, R::V0);
                a.jcc_label(C::Equal, empty);
                collect::clear_scratch(a, l);
                a.bind_text_label(empty);
                adoption::private_closed(a, l);
                let wait_tail = a.create_text_label();
                let funded = a.create_text_label();
                moving_round::admission::native_tail(a, l, wait_tail);
                a.jmp_label(funded);
                a.bind_text_label(wait_tail);
                worker::request_work(a);
                worker::unlock(a, l);
                worker::wake_if_pending(a, l);
                a.call_label(l.moving_end_actor);
                a.call_label(l.owner_checkpoint_poll);
                if let Some(w) = &l.worker {
                    a.call_label(w.hooks.relax);
                }
                a.jmp_label(retry);
                a.bind_text_label(funded);
                alloc::close_owner_tlab(a, l);
                a.call_label(l.root_journal_seal_owner);
                // An owner-local journal slice may have released the ticket.
                let sealed = a.create_text_label();
                lea_rt(a, R::V1, quiescence::REQUEST);
                a.atomic_load_acquire(R::V0, R::V1);
                a.test_reg_reg(R::V0, R::V0);
                a.jcc_label(C::Equal, sealed);
                worker::unlock(a, l);
                a.call_label(l.owner_checkpoint_poll);
                a.jmp_label(retry);
                a.bind_text_label(sealed);
            } else {
                a.call_label(l.root_journal_seal);
            }
        }
        load_rt(a, R::V0, RT_CTX + offset_of!(MutatorContext, adopted_phase));
        if entering {
            store_rt(a, PHASE, R::V0);
            load_rt(a, R::V0, allocation_records::REVISION);
            store_rt(a, BANK, R::V0);
            load_rt(
                a,
                R::V0,
                registered_roots::REGISTRY + offset_of!(crate::gc_abi::RootRegistry, last_scope),
            );
            store_rt(a, ROOT_SCOPE, R::V0);
            store_rt_imm(a, SCOPE_BORROWED, u64::from(!explicit));
            super::participants::step(a, l, 3);
            if !explicit {
                change_borrow(a, l, true);
            }
            copy::bump(a, ENTRIES, R::V0);
        } else {
            load_rt(a, R::V0, SCOPE_BORROWED);
            a.cmp_reg_imm32(R::V0, i32::from(!explicit));
            a.jcc_label(C::NotEqual, l.fail);
            if explicit {
                adoption::return_snapshot(a, l);
            } else {
                // This single-owner scope cannot adopt a different phase while
                // borrowing a heap cursor. Reject accidental coordination changes.
                load_rt(a, R::V0, RT_CTX + offset_of!(MutatorContext, adopted_phase));
                load_rt(a, R::V1, PHASE);
                a.cmp_reg_reg(R::V0, R::V1);
                a.jcc_label(C::NotEqual, l.fail);
                lea_rt(
                    a,
                    R::V2,
                    registered_roots::REGISTRY + offset_of!(crate::gc_abi::RootRegistry, phase),
                );
                a.atomic_load_acquire(R::V0, R::V2);
                a.cmp_reg_reg(R::V0, R::V1);
                a.jcc_label(C::NotEqual, l.fail);
            }
            super::participants::step(a, l, 4);
            if !explicit {
                change_borrow(a, l, false);
            }
            store_rt_imm(a, SCOPE_BORROWED, 0);
            copy::bump(a, EXITS, R::V0);
        }
        worker::unlock(a, l);
        if entering {
            worker::wake_if_pending(a, l);
        }
        for r in SAVED.into_iter().rev() {
            a.pop_reg(r);
        }
        a.ret();
    }

    a.bind_text_label(l.sleep_millis);
    for r in SAVED {
        a.push_reg(r);
    }
    a.cmp_reg_imm32(R::V0, 0);
    a.jcc_label(C::Less, l.fail);
    load_runtime(a, runtime);
    // Only a scalar crosses this boundary. Obtain the same real binding and
    // native ABI consumed by foreign adapters; no heap cursor is borrowed.
    a.push_reg(R::V0);
    a.push_reg(R::V0);
    native_api::compiler_bootstrap(a, l);
    native_api::compiler_transition(a, l, true);
    a.pop_reg(R::V7);
    a.pop_reg(R::V7);
    a.push_reg(R::V3);
    a.push_reg(R::V3);
    a.plat_sleep_millis();
    a.pop_reg(R::V3);
    a.pop_reg(R::V3);
    native_api::compiler_transition(a, l, false);
    for r in SAVED.into_iter().rev() {
        a.pop_reg(r);
    }
    a.ret();

    a.bind_text_label(l.write_heap_string);
    for r in SAVED {
        a.push_reg(r);
    }
    load_runtime(a, runtime);
    a.mov_reg_reg(R::V10, R::V7);
    a.load_ptr_disp32(R::V9, R::V0, 0);
    a.mov_reg_reg(R::V8, R::V0);
    a.add_reg_imm32(R::V8, 8);
    let next = a.create_text_label();
    let sized = a.create_text_label();
    let done = a.create_text_label();
    a.bind_text_label(next);
    a.test_reg_reg(R::V9, R::V9);
    a.jcc_label(C::Equal, done);
    a.mov_imm64(R::V1, COPY_OUT_BYTES);
    a.cmp_reg_reg(R::V9, R::V1);
    a.jcc_label(C::AboveOrEqual, sized);
    a.mov_reg_reg(R::V1, R::V9);
    a.bind_text_label(sized);
    a.mov_reg_reg(R::V2, R::V1);
    a.mov_reg_reg(R::V6, R::V8);
    lea_rt(a, R::V7, copy_out_offset());
    a.add_reg_reg(R::V8, R::V1);
    a.sub_reg_reg(R::V9, R::V1);
    a.rep_movsb();
    a.call_label(l.native_enter);
    // Syscalls/native shims may overwrite every portable scratch register.
    for r in [R::V3, R::V8, R::V9, R::V10] {
        a.push_reg(r);
    }
    lea_rt(a, R::V6, copy_out_offset());
    a.mov_reg_reg(R::V7, R::V10);
    a.plat_write_buffer();
    for r in [R::V10, R::V9, R::V8, R::V3] {
        a.pop_reg(r);
    }
    // Error/short write still leaves native state before the caller resumes,
    // preserving the existing print API's write-result behavior.
    a.call_label(l.native_exit);
    copy::bump(a, CHUNKS, R::V0);
    a.jmp_label(next);
    a.bind_text_label(done);
    for r in SAVED.into_iter().rev() {
        a.pop_reg(r);
    }
    a.ret();

    emit_rooted_string(a, l, runtime);
}

/// String output is a safepoint. Its caller roots all other live managed
/// values, including enclosing renderers' parent objects and cursors.
/// V0 is the string payload and V7 the fd. The temporary frame owns a real
/// RootCell before the first poll, and only an offset survives native writes.
fn emit_rooted_string<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, runtime: E::DataLabel) {
    a.bind_text_label(l.write_rooted_string);
    for r in SAVED {
        a.push_reg(r);
    }
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    // Five portable pushes provide 40 bytes on x86 and preserve native call
    // alignment; AArch64's wider stack slots provide at least the same range.
    for _ in 0..5 {
        a.push_reg(R::V0);
    }
    a.store_rbp_slot(8, R::V0);
    a.store_rbp_slot(16, R::V7);
    load_runtime(a, runtime);
    a.mov_reg_reg(R::V0, R::V5);
    a.sub_reg_imm8(R::V0, 8);
    a.call_label(l.root_adopt_actor);
    native_api::compiler_bootstrap(a, l);
    a.mov_imm64(R::V0, 0);
    a.store_rbp_slot(24, R::V0);
    let next = a.create_text_label();
    let sized = a.create_text_label();
    let done = a.create_text_label();
    a.bind_text_label(next);
    a.load_rbp_slot(R::V0, 8);
    a.call_label(l.root_load_actor);
    a.load_ptr_disp32(R::V9, R::V0, 0);
    a.load_rbp_slot(R::V8, 24);
    a.sub_reg_reg(R::V9, R::V8);
    a.jcc_label(C::Below, l.fail);
    a.test_reg_reg(R::V9, R::V9);
    a.jcc_label(C::Equal, done);
    a.mov_imm64(R::V1, COPY_OUT_BYTES);
    a.cmp_reg_reg(R::V9, R::V1);
    a.jcc_label(C::AboveOrEqual, sized);
    a.mov_reg_reg(R::V1, R::V9);
    a.bind_text_label(sized);
    a.store_rbp_slot(32, R::V1);
    a.mov_reg_reg(R::V6, R::V0);
    a.add_reg_imm32(R::V6, 8);
    a.add_reg_reg(R::V6, R::V8);
    lea_rt(a, R::V7, copy_out_offset());
    a.rep_movsb();
    native_api::compiler_transition(a, l, true);
    a.load_rbp_slot(R::V7, 16);
    lea_rt(a, R::V6, copy_out_offset());
    a.load_rbp_slot(R::V2, 32);
    a.push_reg(R::V3);
    a.push_reg(R::V3);
    a.plat_write_buffer();
    a.pop_reg(R::V3);
    a.pop_reg(R::V3);
    native_api::compiler_transition(a, l, false);
    copy::bump(a, CHUNKS, R::V0);
    a.load_rbp_slot(R::V0, 24);
    a.load_rbp_slot(R::V1, 32);
    a.add_reg_reg(R::V0, R::V1);
    a.store_rbp_slot(24, R::V0);
    #[cfg(test)]
    {
        // Real collection interleavings in the emitted-code conformance
        // harness. Release output contains neither this hook nor its storage.
        let resume = a.create_text_label();
        load_rt(
            a,
            R::V11,
            owner_state::offset(offset_of!(OwnerState, rooted_write_hook)),
        );
        a.test_reg_reg(R::V11, R::V11);
        a.jcc_label(C::Equal, resume);
        a.load_rbp_slot(R::V7, 8);
        a.call_native_unary(R::V11);
        a.bind_text_label(resume);
    }
    a.jmp_label(next);
    a.bind_text_label(done);
    a.call_label(l.root_pop_actor);
    a.leave();
    for r in SAVED.into_iter().rev() {
        a.pop_reg(r);
    }
    a.ret();
}

/// Legacy heap-cursor scopes forbid phase changes even when no journal record
/// remains. Explicit no-raw-borrow scopes permit their checked Old adoption.
/// All writes/reads are under the collector ticket; no registry walk is needed.
pub(super) fn require_phase_clear<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    load_rt(a, R::V0, BORROWS);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
}
pub(super) fn change_borrow<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, enter: bool) {
    load_rt(a, R::V0, BORROWS);
    if enter {
        a.add_reg_imm32(R::V0, 1);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::Equal, l.fail);
    } else {
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::Equal, l.fail);
        a.sub_reg_imm8(R::V0, 1);
    }
    store_rt(a, BORROWS, R::V0);
}
