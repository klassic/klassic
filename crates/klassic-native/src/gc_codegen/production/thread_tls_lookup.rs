//! Authenticate opaque OS values only against retained runtime-owned nodes.
//! A lookup visits one node per collector ticket, preserving native backing
//! through unlink and preventing a lookup cursor from observing freed storage.
use super::*;
pub(super) fn emit<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    lookup_begin(a, l);
    lookup_step(a, l);
    current(a, l, false);
    current(a, l, true);
}

/// Exclusive native package retained through terminal stage 2. No fields are
/// managed references. Cursor comes only from the runtime-owned native list;
/// the opaque OS value is compared but never dereferenced. Abandoning stage 1
/// would retain a reader/key borrow and is forbidden for these trusted entries.
#[repr(C)]
pub(super) struct Lookup {
    prepare_attach: u64,
    value: u64,
    thread: u64,
    cursor: u64,
    pub(super) output: u64,
    status: u64,
    stage: u64,
}
fn lookup_load<E: PortableAsm>(a: &mut E, r: R, off: usize) {
    a.load_rbp_slot(R::V8, 8);
    a.load_ptr_disp32(r, R::V8, d(off));
}
fn lookup_store<E: PortableAsm>(a: &mut E, off: usize, r: R) {
    a.load_rbp_slot(R::V8, 8);
    a.store_ptr_disp32(R::V8, d(off), r);
}
fn lookup_set<E: PortableAsm>(a: &mut E, off: usize, value: u64) {
    a.mov_imm64(R::V0, value);
    lookup_store(a, off, R::V0);
}
fn lookup_begin<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    a.bind_text_label(l.tls_lookup_begin);
    frame(a);
    let pending = a.create_text_label();
    let failed = a.create_text_label();
    let empty = a.create_text_label();
    let done = a.create_text_label();
    lookup_load(a, R::V0, offset_of!(Lookup, stage));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    lookup_set(a, offset_of!(Lookup, output), 0);
    worker::lock_raw(a, l);
    ready(a, pending);
    counter(a, l, BORROWS, true);
    counter(a, l, READERS, true);
    key(a);
    load_rt(a, R::V0, HEAD);
    lookup_store(a, offset_of!(Lookup, cursor), R::V0);
    lookup_set(a, offset_of!(Lookup, stage), 1);
    worker::unlock_raw(a, l);
    os(a, l.tls_os.get);
    a.store_rbp_slot(16, R::V0);
    worker::lock_raw(a, l);
    a.load_rbp_slot(R::V0, 16);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, failed);
    for (off, local) in [
        (offset_of!(Lookup, value), 32),
        (offset_of!(Lookup, thread), 24),
    ] {
        a.load_rbp_slot(R::V0, local);
        lookup_store(a, off, R::V0);
    }
    a.load_rbp_slot(R::V0, 32);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, empty);
    a.mov_imm64(R::V0, 2);
    a.jmp_label(done);
    a.bind_text_label(empty);
    a.mov_imm64(R::V0, 0);
    a.bind_text_label(failed);
    lookup_store(a, offset_of!(Lookup, status), R::V0);
    counter(a, l, READERS, false);
    counter(a, l, BORROWS, false);
    lookup_set(a, offset_of!(Lookup, stage), 2);
    lookup_load(a, R::V0, offset_of!(Lookup, status));
    a.jmp_label(done);
    a.bind_text_label(pending);
    a.mov_imm64(R::V0, 2);
    a.bind_text_label(done);
    worker::unlock_raw(a, l);
    end(a);
}
/// At most one list node per ticket acquisition, with no OS calls inside it.
fn lookup_step<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    a.bind_text_label(l.tls_lookup_step);
    frame(a);
    let terminal = a.create_text_label();
    let invalid = a.create_text_label();
    let advance = a.create_text_label();
    let inherited = a.create_text_label();
    let finish = a.create_text_label();
    let done = a.create_text_label();
    worker::lock_raw(a, l);
    lookup_load(a, R::V0, offset_of!(Lookup, stage));
    a.cmp_reg_imm32(R::V0, 2);
    a.jcc_label(C::Equal, terminal);
    a.cmp_reg_imm32(R::V0, 1);
    a.jcc_label(C::NotEqual, l.fail);
    lookup_load(a, R::V9, offset_of!(Lookup, cursor));
    a.test_reg_reg(R::V9, R::V9);
    a.jcc_label(C::Equal, invalid);
    lookup_load(a, R::V0, offset_of!(Lookup, value));
    a.cmp_reg_reg(R::V0, R::V9);
    a.jcc_label(C::NotEqual, advance);
    // V9 was obtained from the owned list under a real retained reader.
    a.load_ptr_disp32(R::V0, R::V9, d(offset_of!(OwnerState, native_tls)));
    a.cmp_reg_imm32(R::V0, 2);
    a.jcc_label(C::NotEqual, invalid);
    a.load_ptr_disp32(R::V0, R::V9, d(offset_of!(OwnerState, native_tls_thread)));
    lookup_load(a, R::V1, offset_of!(Lookup, thread));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, invalid);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, inherited);
    lookup_store(a, offset_of!(Lookup, output), R::V9);
    a.mov_imm64(R::V0, 0);
    a.jmp_label(finish);
    a.bind_text_label(inherited);
    lookup_load(a, R::V0, offset_of!(Lookup, prepare_attach));
    a.cmp_reg_imm32(R::V0, 1);
    a.jcc_label(C::NotEqual, invalid);
    // Only a known installed runtime association with a DIFFERENT physical
    // owner can be inherited. Drop this child's OS value, never the parent's
    // association/user/list state. Retain both key and native cursor lifetime
    // across the OS operation; no unknown OS pointer has been dereferenced.
    key(a);
    worker::unlock_raw(a, l);
    os(a, l.tls_os.set); // value is zero; no GC ownership has been installed here
    a.store_rbp_slot(16, R::V0);
    worker::lock_raw(a, l);
    a.load_rbp_slot(R::V0, 16);
    a.jmp_label(finish);
    a.bind_text_label(advance);
    a.load_ptr_disp32(R::V0, R::V9, d(offset_of!(OwnerState, native_tls_next)));
    lookup_store(a, offset_of!(Lookup, cursor), R::V0);
    a.mov_imm64(R::V0, 2);
    a.jmp_label(done);
    a.bind_text_label(invalid);
    a.mov_imm64(R::V0, 3);
    a.bind_text_label(finish);
    lookup_store(a, offset_of!(Lookup, status), R::V0);
    counter(a, l, READERS, false);
    counter(a, l, BORROWS, false);
    lookup_set(a, offset_of!(Lookup, stage), 2);
    a.bind_text_label(terminal);
    lookup_load(a, R::V0, offset_of!(Lookup, status));
    a.bind_text_label(done);
    worker::unlock_raw(a, l);
    end(a);
}
/// V7 = native u64 output. Return 0 (owned current context or empty), 2 (key
/// unavailable), 3 (foreign/inherited value), 6 (OS failure). Output is zero on
/// refusal. A valid result is owned by this OS thread; it must retain its own
/// association until done using the context and cannot transfer that borrow.
fn current<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, prepare: bool) {
    a.bind_text_label(if prepare {
        l.tls_prepare_attach
    } else {
        l.tls_current
    });
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm32(R::V4, 80);
    a.store_rbp_slot(8, R::V7);
    a.push_reg(R::V3);
    a.push_reg(R::OwnerContext);
    for local in (16..=72).step_by(8) {
        a.mov_imm64(R::V0, 0);
        a.store_rbp_slot(local, R::V0);
    }
    if prepare {
        a.mov_imm64(R::V0, 1);
        a.store_rbp_slot(72 - offset_of!(Lookup, prepare_attach) as i32, R::V0);
    }
    a.mov_reg_reg(R::V7, R::V5);
    a.sub_reg_imm32(R::V7, 72);
    a.call_label(l.tls_lookup_begin);
    let again = a.create_text_label();
    let done = a.create_text_label();
    let publish = a.create_text_label();
    a.bind_text_label(again);
    a.cmp_reg_imm32(R::V0, 2);
    a.jcc_label(C::NotEqual, done);
    // Stage 0 means initialization is not ready: there is no lease to step.
    a.load_rbp_slot(R::V1, 72 - offset_of!(Lookup, stage) as i32);
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::Equal, done);
    a.mov_reg_reg(R::V7, R::V5);
    a.sub_reg_imm32(R::V7, 72);
    a.call_label(l.tls_lookup_step);
    a.jmp_label(again);
    a.bind_text_label(done);
    a.load_rbp_slot(R::V1, 8);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, publish);
    a.mov_imm64(R::V2, 0);
    a.store_ptr_disp32(R::V1, 0, R::V2);
    end(a);
    a.bind_text_label(publish);
    a.load_rbp_slot(R::V2, 72 - offset_of!(Lookup, output) as i32);
    a.store_ptr_disp32(R::V1, 0, R::V2);
    end(a);
}
