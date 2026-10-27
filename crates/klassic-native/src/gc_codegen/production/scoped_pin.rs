//! Resumable scoped ownership over the real strong-root, private-copy and
//! metadata-lease protocols. Only Ready publishes an address. Tokens have a
//! stable native address and creator affinity; copying their bytes is invalid.
//! New returns 2 only after taking ownership of the independent root. Status 4
//! leaves the token empty: retain the source frame root and retry New later.
use super::*;

const CAPACITY: usize = 128;
pub(super) use crate::gc_abi::ScopedPinToken as Token;
#[repr(C)]
struct Family {
    identity: u64,
    address: u64,
    count: u64,
    closing: u64,
}
#[repr(C)]
pub(super) struct State {
    families: [Family; CAPACITY],
}
#[repr(C)]
pub(super) struct Owner {
    caller: u64,
    source: u64,
    raw: u64,
    family: u64,
    empty: u64,
}
const OWN: usize = owner_state::offset(offset_of!(OwnerState, scoped_pin));
const CALLER: usize = OWN + offset_of!(Owner, caller);
const SOURCE: usize = OWN + offset_of!(Owner, source);
const RAW: usize = OWN + offset_of!(Owner, raw);
const FAMILY: usize = OWN + offset_of!(Owner, family);
const EMPTY: usize = OWN + offset_of!(Owner, empty);
const FAMILIES: usize = offset_of!(Runtime, scoped_pins) + offset_of!(State, families);
const STAGE: usize = offset_of!(Token, stage);
const ADDRESS: usize = offset_of!(Token, address);
const IDENTITY: usize = offset_of!(Token, identity);
const ROOT: usize = offset_of!(Token, root);
const DEST: usize = offset_of!(Token, destination);
const REFERENCE: usize = offset_of!(Token, reference);
const TOKEN_FAMILY: usize = offset_of!(Token, family);
const USES: usize = offset_of!(Token, uses);

fn ptr<E: PortableAsm>(a: &mut E, r: R, off: usize) {
    load_rt(a, r, CALLER);
    if off != 0 {
        a.add_reg_imm32(r, d(off));
    }
}
fn get<E: PortableAsm>(a: &mut E, r: R, off: usize) {
    ptr(a, R::V8, 0);
    a.load_ptr_disp32(r, R::V8, d(off));
}
fn put<E: PortableAsm>(a: &mut E, off: usize, r: R) {
    ptr(a, R::V8, 0);
    a.store_ptr_disp32(R::V8, d(off), r);
}
fn set<E: PortableAsm>(a: &mut E, off: usize, n: u64) {
    a.mov_imm64(R::V0, n);
    put(a, off, R::V0);
}
fn begin<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, entry: E::TextLabel) {
    a.bind_text_label(entry);
    a.push_reg(R::V3);
    a.push_reg(R::OwnerContext);
    owner_state::bind_running(a, l);
    load_rt(a, R::V1, CALLER);
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::NotEqual, l.fail);
    store_rt(a, CALLER, R::V7);
    store_rt(a, SOURCE, R::V0);
    a.call_label(l.publish_pending);
    quiescence::poll(a, l);
}
fn finish<E: PortableAsm>(a: &mut E) {
    leave(a);
    a.ret();
}
fn leave<E: PortableAsm>(a: &mut E) {
    a.mov_imm64(R::V1, 0);
    for off in (0..size_of::<Owner>()).step_by(8) {
        store_rt(a, OWN + off, R::V1);
    }
    a.pop_reg(R::OwnerContext);
    a.pop_reg(R::V3);
}
fn validate<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, invalid: E::TextLabel) {
    load_rt(a, R::V7, CALLER);
    a.test_reg_reg(R::V7, R::V7);
    a.jcc_label(C::Equal, invalid);
    a.mov_reg_reg(R::V0, R::V7);
    a.and_reg_imm32(R::V0, 7);
    a.jcc_label(C::NotEqual, invalid);
    get(a, R::V0, offset_of!(Token, holder));
    a.cmp_reg_reg(R::V0, R::V7);
    a.jcc_label(C::NotEqual, invalid);
    get(a, R::V0, offset_of!(Token, runtime));
    a.cmp_reg_reg(R::V0, R::V3);
    a.jcc_label(C::NotEqual, invalid);
    get(a, R::V0, offset_of!(Token, context));
    a.cmp_reg_reg(R::V0, R::OwnerContext);
    a.jcc_label(C::NotEqual, invalid);
    ptr(a, R::V7, ROOT);
    a.call_label(l.native_root_validate);
    a.test_reg_reg(R::V2, R::V2);
    a.jcc_label(C::NotEqual, invalid);
}
fn resolve<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    // Drop private raw scratch before the adapter's real safepoint.
    store_rt_imm(a, RAW, 0);
    ptr(a, R::V7, ROOT);
    a.call_label(l.native_root_resolve);
    a.test_reg_reg(R::V2, R::V2);
    a.jcc_label(C::NotEqual, l.fail);
    store_rt(a, RAW, R::V0);
}
fn large<E: PortableAsm>(a: &mut E, no: E::TextLabel) {
    load_rt(a, R::V0, RAW);
    load_rt(a, R::V1, RT_HEAP_BASE);
    a.sub_reg_reg(R::V0, R::V1);
    generation::test_role(a, R::V0, generation::LARGE);
    a.jcc_label(C::NotEqual, no);
}
/// Ticket held. Scan the bounded table by actual logical identity; keep the
/// first vacant slot separately. No raw address supplies strong liveness.
fn find_family<E: PortableAsm>(a: &mut E) {
    store_rt_imm(a, FAMILY, 0);
    store_rt_imm(a, EMPTY, 0);
    get(a, R::V10, IDENTITY);
    lea_rt(a, R::V9, FAMILIES);
    a.mov_imm64(R::V6, CAPACITY as u64);
    let again = a.create_text_label();
    let vacant = a.create_text_label();
    let next = a.create_text_label();
    let found = a.create_text_label();
    let done = a.create_text_label();
    a.bind_text_label(again);
    a.load_ptr_disp32(R::V0, R::V9, 0);
    a.cmp_reg_reg(R::V0, R::V10);
    a.jcc_label(C::Equal, found);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, vacant);
    a.jmp_label(next);
    a.bind_text_label(vacant);
    load_rt(a, R::V0, EMPTY);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, next);
    store_rt(a, EMPTY, R::V9);
    a.bind_text_label(next);
    a.add_reg_imm32(R::V9, d(size_of::<Family>()));
    a.sub_reg_imm8(R::V6, 1);
    a.jcc_label(C::NotEqual, again);
    a.jmp_label(done);
    a.bind_text_label(found);
    store_rt(a, FAMILY, R::V9);
    a.bind_text_label(done);
}
fn clear_family<E: PortableAsm>(a: &mut E) {
    load_rt(a, R::V9, FAMILY);
    a.mov_imm64(R::V0, 0);
    for off in (0..size_of::<Family>()).step_by(8) {
        a.store_ptr_disp32(R::V9, d(off), R::V0);
    }
    put(a, TOKEN_FAMILY, R::V0);
}
/// Pending scoped work may drive the already enrolled moving coordinator.
/// Legacy callers retain their existing collection/poll contract; this entry
/// never silently enrolls extra owners or uses the single-owner finish path.
fn drive<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    store_rt_imm(a, RAW, 0);
    worker::lock(a, l);
    let done = a.create_text_label();
    let finish_move = a.create_text_label();
    let start = a.create_text_label();
    let after = a.create_text_label();
    load_rt(a, R::V0, moving_round::ENABLED);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    load_rt(a, R::V0, RT_PHASE_OPEN);
    load_rt(a, R::V1, RT_RETIRE_STAGE);
    a.or_reg_reg(R::V0, R::V1);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, finish_move);
    load_rt(a, R::V0, old_cycle::REQUEST);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, start);
    a.jmp_label(done);
    for (label, entry) in [
        (finish_move, l.moving_end_actor),
        (start, l.moving_start_actor),
    ] {
        a.bind_text_label(label);
        worker::unlock(a, l);
        a.call_label(entry);
        a.jmp_label(after);
    }
    a.bind_text_label(done);
    worker::unlock(a, l);
    a.bind_text_label(after);
    worker::wake_if_pending(a, l);
}
fn retry_progress<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let done = a.create_text_label();
    a.cmp_reg_imm8(R::V0, 2);
    a.jcc_label(C::NotEqual, done);
    drive(a, l);
    a.mov_imm64(R::V0, 2);
    a.bind_text_label(done);
}
fn create<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    begin(a, l, l.scoped_pin_new_actor); // V0=source frame RootCell, V7=zero token
    let invalid = a.create_text_label();
    let done = a.create_text_label();
    load_rt(a, R::V7, CALLER);
    a.test_reg_reg(R::V7, R::V7);
    a.jcc_label(C::Equal, invalid);
    a.mov_reg_reg(R::V0, R::V7);
    a.and_reg_imm32(R::V0, 7);
    a.jcc_label(C::NotEqual, invalid);
    for off in (0..size_of::<Token>()).step_by(8) {
        a.load_ptr_disp32(R::V0, R::V7, d(off));
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, invalid);
    }
    load_rt(a, R::V0, SOURCE);
    a.call_label(l.root_load_actor);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, invalid);
    load_rt(a, R::V1, RT_HEAP_BASE);
    a.mov_reg_reg(R::V2, R::V0);
    a.sub_reg_reg(R::V2, R::V1);
    a.jcc_label(C::Below, invalid);
    a.cmp_reg_imm32(R::V2, geometry(a).heap_bytes as i32);
    a.jcc_label(C::AboveOrEqual, invalid);
    // The unique native root authenticates and resolves the source again
    // after its own safepoint; no raw result is carried across it.
    ptr(a, R::V7, ROOT);
    load_rt(a, R::V0, SOURCE);
    a.call_label(l.native_root_new_actor);
    let admitted = a.create_text_label();
    a.cmp_reg_imm8(R::V0, 2);
    a.jcc_label(C::NotEqual, admitted);
    a.mov_imm64(R::V0, 4); // Unowned retry must not be confused with owned Pending.
    a.bind_text_label(admitted);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, done);
    load_rt(a, R::V0, CALLER);
    put(a, offset_of!(Token, holder), R::V0);
    put(a, offset_of!(Token, runtime), R::V3);
    put(a, offset_of!(Token, context), R::OwnerContext);
    resolve(a, l);
    a.load_ptr_disp32(R::V0, R::V0, copy::MARK_RECORD);
    a.load_ptr_disp32(R::V0, R::V0, d(offset_of!(MarkRecord, object_id)));
    put(a, IDENTITY, R::V0);
    set(a, STAGE, 2);
    a.mov_imm64(R::V0, 2);
    a.jmp_label(done);
    a.bind_text_label(invalid);
    a.mov_imm64(R::V0, 3);
    a.bind_text_label(done);
    finish(a);
}
fn progress<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    begin(a, l, l.scoped_pin_step_actor);
    let invalid = a.create_text_label();
    let pending = a.create_text_label();
    let moving = a.create_text_label();
    let cancel = a.create_text_label();
    let ready = a.create_text_label();
    let pending_locked = a.create_text_label();
    let done_locked = a.create_text_label();
    let done = a.create_text_label();
    validate(a, l, invalid);
    get(a, R::V0, STAGE);
    a.cmp_reg_imm8(R::V0, 3);
    a.jcc_label(C::Equal, ready);
    a.cmp_reg_imm8(R::V0, 2);
    a.jcc_label(C::NotEqual, invalid);
    resolve(a, l);
    large(a, moving);
    get(a, R::V0, DEST);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, cancel);
    a.jmp_label(ready);
    a.bind_text_label(cancel);
    store_rt_imm(a, RAW, 0);
    ptr(a, R::V7, DEST);
    a.call_label(l.pin_destination_cancel_actor);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, done);
    // Cancel can poll; never reuse its pre-poll canonical address.
    resolve(a, l);
    large(a, moving);
    a.jmp_label(ready);
    a.bind_text_label(moving);
    get(a, R::V0, DEST);
    a.test_reg_reg(R::V0, R::V0);
    let watch = a.create_text_label();
    a.jcc_label(C::NotEqual, watch);
    load_rt(a, R::V0, RAW);
    ptr(a, R::V7, DEST);
    a.call_label(l.pin_destination_reserve_resolved);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, done);
    a.jmp_label(pending);
    a.bind_text_label(watch);
    load_rt(a, R::V0, RAW);
    ptr(a, R::V7, DEST);
    a.call_label(l.pin_destination_watch_resolved);
    a.cmp_reg_imm8(R::V0, 1);
    a.jcc_label(C::Above, done);
    // Demand a real Full; caller/scheduler drives the actual coordinator.
    // Pending owns the independent root and destination between steps.
    worker::lock(a, l);
    store_rt_imm(a, old_cycle::REQUEST, old_cycle::CAPACITY);
    worker::request_work(a);
    worker::unlock(a, l);
    a.jmp_label(pending);
    a.bind_text_label(ready);
    get(a, R::V0, STAGE);
    a.cmp_reg_imm8(R::V0, 3);
    let acquire = a.create_text_label();
    a.jcc_label(C::NotEqual, acquire);
    a.mov_imm64(R::V0, 0);
    a.jmp_label(done);
    a.bind_text_label(acquire);
    worker::lock(a, l);
    for field in [RT_PHASE_OPEN, RT_RETIRE_STAGE] {
        load_rt(a, R::V0, field);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, pending_locked);
    }
    load_rt(a, R::V0, RAW);
    load_rt(a, R::V1, RT_HEAP_BASE);
    a.sub_reg_reg(R::V0, R::V1);
    a.shr_reg_imm8(R::V0, 17);
    large_object::record(a, R::V0);
    a.load_ptr_disp32(R::V0, R::V8, pin_destination::RESERVATION);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, pending_locked);
    find_family(a);
    load_rt(a, R::V9, FAMILY);
    let existing = a.create_text_label();
    a.test_reg_reg(R::V9, R::V9);
    a.jcc_label(C::NotEqual, existing);
    load_rt(a, R::V9, EMPTY);
    a.test_reg_reg(R::V9, R::V9);
    a.jcc_label(C::Equal, pending_locked);
    store_rt(a, FAMILY, R::V9);
    a.bind_text_label(existing);
    a.load_ptr_disp32(R::V0, R::V9, d(offset_of!(Family, closing)));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, pending_locked);
    load_rt(a, R::V0, RAW);
    encode(a, R::V0, R::V1);
    put(a, REFERENCE, R::V0);
    ptr(a, R::V6, REFERENCE);
    lea_rt(a, R::V7, metadata_leases::POOL);
    a.call_label(l.metadata_reference_step);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, done_locked);
    load_rt(a, R::V9, FAMILY);
    get(a, R::V0, IDENTITY);
    a.store_ptr_disp32(R::V9, d(offset_of!(Family, identity)), R::V0);
    load_rt(a, R::V0, RAW);
    a.store_ptr_disp32(R::V9, d(offset_of!(Family, address)), R::V0);
    put(a, ADDRESS, R::V0);
    a.load_ptr_disp32(R::V0, R::V9, d(offset_of!(Family, count)));
    a.add_reg_imm32(R::V0, 1);
    a.jcc_label(C::Equal, l.fail);
    a.store_ptr_disp32(R::V9, d(offset_of!(Family, count)), R::V0);
    put(a, TOKEN_FAMILY, R::V9);
    set(a, STAGE, 3);
    a.mov_imm64(R::V0, 0);
    a.jmp_label(done_locked);
    a.bind_text_label(pending_locked);
    a.mov_imm64(R::V0, 2);
    a.bind_text_label(done_locked);
    worker::unlock(a, l);
    a.jmp_label(done);
    a.bind_text_label(pending);
    a.mov_imm64(R::V0, 2);
    a.jmp_label(done);
    a.bind_text_label(invalid);
    a.mov_imm64(R::V0, 3);
    a.bind_text_label(done);
    retry_progress(a, l);
    worker::wake_if_pending(a, l);
    finish(a);
}
fn release<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    begin(a, l, l.scoped_pin_release_actor);
    let invalid = a.create_text_label();
    let pending = a.create_text_label();
    let ending = a.create_text_label();
    let retire_root = a.create_text_label();
    let release_lease = a.create_text_label();
    let adopt = a.create_text_label();
    let clear = a.create_text_label();
    let pending_locked = a.create_text_label();
    let unlock = a.create_text_label();
    let done = a.create_text_label();
    validate(a, l, invalid);
    get(a, R::V0, USES);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, pending);
    get(a, R::V0, STAGE);
    a.cmp_reg_imm8(R::V0, 5);
    a.jcc_label(C::Equal, retire_root);
    a.cmp_reg_imm8(R::V0, 4);
    a.jcc_label(C::Equal, ending);
    a.cmp_reg_imm8(R::V0, 2);
    a.jcc_label(C::Below, invalid);
    a.cmp_reg_imm8(R::V0, 3);
    a.jcc_label(C::Above, invalid);
    worker::lock(a, l);
    // Decrement exactly once before any cancellation/release can poll.
    set(a, STAGE, 4);
    set(a, ADDRESS, 0);
    get(a, R::V0, TOKEN_FAMILY);
    a.test_reg_reg(R::V0, R::V0);
    let no_family = a.create_text_label();
    a.jcc_label(C::Equal, no_family);
    store_rt(a, FAMILY, R::V0);
    find_family(a);
    load_rt(a, R::V9, FAMILY);
    get(a, R::V0, TOKEN_FAMILY);
    a.cmp_reg_reg(R::V0, R::V9);
    a.jcc_label(C::NotEqual, l.fail);
    a.load_ptr_disp32(R::V0, R::V9, d(offset_of!(Family, count)));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, l.fail);
    a.sub_reg_imm8(R::V0, 1);
    a.store_ptr_disp32(R::V9, d(offset_of!(Family, count)), R::V0);
    let last = a.create_text_label();
    a.jcc_label(C::Equal, last);
    set(a, TOKEN_FAMILY, 0);
    a.jmp_label(no_family);
    a.bind_text_label(last);
    load_rt(a, R::V0, CALLER);
    a.store_ptr_disp32(R::V9, d(offset_of!(Family, closing)), R::V0);
    a.bind_text_label(no_family);
    worker::unlock(a, l);
    a.bind_text_label(ending);
    get(a, R::V0, DEST);
    a.test_reg_reg(R::V0, R::V0);
    let cancelled = a.create_text_label();
    a.jcc_label(C::Equal, cancelled);
    ptr(a, R::V7, DEST);
    a.call_label(l.pin_destination_cancel_actor);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, done);
    a.bind_text_label(cancelled);
    resolve(a, l);
    large(a, release_lease);
    worker::lock(a, l);
    find_family(a);
    load_rt(a, R::V9, FAMILY);
    a.test_reg_reg(R::V9, R::V9);
    a.jcc_label(C::Equal, adopt);
    a.load_ptr_disp32(R::V0, R::V9, d(offset_of!(Family, closing)));
    load_rt(a, R::V1, CALLER);
    a.cmp_reg_reg(R::V0, R::V1);
    // Another actual ready/ending token owns the page's eventual adoption.
    a.jcc_label(C::NotEqual, unlock);
    a.bind_text_label(adopt);
    load_rt(a, R::V0, RAW);
    a.load_ptr_disp32(R::V1, R::V0, copy::PAYLOAD);
    a.cmp_reg_imm32(R::V1, MAX_PAYLOAD as i32);
    a.jcc_label(C::Above, clear);
    a.call_label(l.pin_adopt_small);
    a.cmp_reg_imm8(R::V0, 2);
    a.jcc_label(C::Equal, pending_locked);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    a.bind_text_label(clear);
    load_rt(a, R::V9, FAMILY);
    a.test_reg_reg(R::V9, R::V9);
    a.jcc_label(C::Equal, unlock);
    clear_family(a);
    a.bind_text_label(unlock);
    worker::unlock(a, l);
    a.bind_text_label(release_lease);
    store_rt_imm(a, RAW, 0);
    get(a, R::V0, REFERENCE + 24);
    a.test_reg_reg(R::V0, R::V0);
    let no_lease = a.create_text_label();
    a.jcc_label(C::Equal, no_lease);
    ptr(a, R::V7, REFERENCE + 8);
    a.call_label(l.metadata_lease_release_actor);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    a.bind_text_label(no_lease);
    set(a, STAGE, 5);
    a.bind_text_label(retire_root);
    ptr(a, R::V7, ROOT);
    a.call_label(l.native_root_release_actor);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, done);
    a.mov_imm64(R::V0, 0);
    ptr(a, R::V8, 0);
    for off in (0..size_of::<Token>()).step_by(8) {
        a.store_ptr_disp32(R::V8, d(off), R::V0);
    }
    a.jmp_label(done);
    a.bind_text_label(pending_locked);
    worker::unlock(a, l);
    a.bind_text_label(pending);
    a.mov_imm64(R::V0, 2);
    a.jmp_label(done);
    a.bind_text_label(invalid);
    a.mov_imm64(R::V0, 3);
    a.bind_text_label(done);
    retry_progress(a, l);
    worker::wake_if_pending(a, l);
    finish(a);
}
pub(super) fn emit<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    create(a, l);
    progress(a, l);
    release(a, l);
    native_use(a, l, false);
    native_use(a, l, true);
    callback(a, l, false);
    callback(a, l, true);
}

/// A pin use holds its token's actual independent root and metadata lease.
/// Native entry is root-only: unrelated objects remain movable, and the
/// legacy whole-heap borrowed-cursor counter is never incremented here.
fn native_use<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, ending: bool) {
    let entry = if ending {
        a.bind_text_label(l.scoped_pin_native_exit_actor);
        a.call_label(l.native_exit_actor);
        l.scoped_pin_end_running_actor
    } else {
        l.scoped_pin_native_enter_actor
    };
    begin(a, l, entry);
    let invalid = a.create_text_label();
    let done = a.create_text_label();
    validate(a, l, invalid);
    get(a, R::V0, STAGE);
    a.cmp_reg_imm8(R::V0, 3);
    a.jcc_label(C::NotEqual, invalid);
    worker::lock(a, l);
    get(a, R::V0, USES);
    if ending {
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::Equal, l.fail);
        a.sub_reg_imm8(R::V0, 1);
    } else {
        a.add_reg_imm32(R::V0, 1);
        a.jcc_label(C::Equal, l.fail);
    }
    put(a, USES, R::V0);
    worker::unlock(a, l);
    a.mov_imm64(R::V0, 0);
    if !ending {
        leave(a);
        a.call_label(l.native_enter_actor);
        a.ret();
    } else {
        a.jmp_label(done);
    }
    a.bind_text_label(invalid);
    a.mov_imm64(R::V0, 3);
    a.bind_text_label(done);
    finish(a);
}
/// A native callback temporarily resumes the actual Running owner without
/// ending the outer pin use. Nested native calls take their own use count.
fn callback<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, returning: bool) {
    let entry = if returning {
        l.scoped_pin_callback_return_actor
    } else {
        a.bind_text_label(l.scoped_pin_callback_enter_actor);
        a.call_label(l.native_exit_actor);
        a.create_text_label()
    };
    begin(a, l, entry);
    let invalid = a.create_text_label();
    let done = a.create_text_label();
    validate(a, l, invalid);
    get(a, R::V0, STAGE);
    a.cmp_reg_imm8(R::V0, 3);
    a.jcc_label(C::NotEqual, invalid);
    get(a, R::V0, USES);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, invalid);
    a.mov_imm64(R::V0, 0);
    if returning {
        leave(a);
        a.call_label(l.native_enter_actor);
        a.ret();
    } else {
        a.jmp_label(done);
    }
    a.bind_text_label(invalid);
    a.mov_imm64(R::V0, 3);
    a.bind_text_label(done);
    finish(a);
}

#[cfg(test)]
pub(crate) fn token_size() -> usize {
    size_of::<Token>()
}
#[cfg(test)]
pub(crate) fn token_field(index: usize) -> usize {
    [STAGE, ADDRESS, IDENTITY, TOKEN_FAMILY, USES][index]
}
