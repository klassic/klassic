//! Physical accessibility, distinct from shared-pool roles and reservations.
//! The commit lock serializes page transitions; a caller publishes allocation
//! tops only after ensure returns. Payload copy/initialization occurs afterward.
use super::*;
use crate::gc_codegen::production::{lea_rt, load_rt, store_rt};
pub(super) mod grouped;
mod pretouch;
#[repr(C)]
pub(super) struct State {
    minimum: u64,
    committed: u64,
    peak: u64,
    commits: u64,
    recommits: u64,
    uncommits: u64,
    pretouched: u64,
    lock: u64,
    fail_after: u64,
    scan_cursor: u64,
    delay_ns: u64,
    report: [u64; 7],
}
macro_rules! f { ($($n:ident:$v:ident),* $(,)?) => { $(
    pub(super) const $n: usize = offset_of!(Runtime, commit.$v);
)* }; }
f! { MINIMUM:minimum, COMMITTED:committed, PEAK:peak, COMMITS:commits,
RECOMMITS:recommits, UNCOMMITS:uncommits, PRETOUCHED:pretouched, LOCK:lock, FAIL_AFTER:fail_after, SCAN_CURSOR:scan_cursor, DELAY_NS:delay_ns }
pub(super) const OWNER_TICK: usize = owner_state::offset(offset_of!(OwnerState, maintenance_tick));
pub(crate) struct MemoryLabels<T> {
    pub reserve: T,
    pub commit: T,
    pub decommit: T,
    /// Release an entire separately owned mapping: V7=base,V6=bytes -> bool.
    pub release: T,
}
/// Offset -> ensure its physical page is accessible, preserving every register.
pub(super) fn ensure<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, offset: R) {
    a.push_reg(R::V0);
    a.push_reg(R::V0);
    a.mov_reg_reg(R::V0, offset);
    a.shr_reg_imm8(R::V0, 17);
    a.call_label(l.ensure_heap_page);
    a.pop_reg(R::V0);
    a.pop_reg(R::V0);
}
pub(super) fn startup<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let bytes = a.gc_heap_minimum();
    store_rt_imm(a, MINIMUM, bytes);
    store_rt_imm(a, DELAY_NS, a.gc_uncommit_delay_ns());
    load_rt(a, R::V7, RT_HEAP_BASE);
    a.add_reg_imm32(R::V7, HEAP_PAGE_BYTES as i32);
    a.mov_imm64(R::V6, bytes);
    a.call_label(l.memory.commit);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(
        C::Equal,
        if a.gc_large_pages() == crate::GcLargePages::Required {
            l.large_pages_failed
        } else {
            l.heap_commit_failed
        },
    );
    if a.gc_pre_touch() {
        load_rt(a, R::V7, RT_HEAP_BASE);
        a.add_reg_imm32(R::V7, HEAP_PAGE_BYTES as i32);
        a.mov_imm64(R::V6, bytes);
        pretouch::emit(a);
    }
    store_rt_imm(a, COMMITTED, bytes);
    store_rt_imm(a, PEAK, bytes);
    store_rt_imm(a, COMMITS, bytes / HEAP_PAGE_BYTES);
    lea_rt(a, R::V8, layout(a).committed_pages as usize + 8);
    a.mov_imm64(R::V7, bytes / HEAP_PAGE_BYTES);
    a.mov_imm64(R::V0, 1);
    let next = a.create_text_label();
    a.bind_text_label(next);
    a.store_ptr_disp32(R::V8, 0, R::V0);
    a.add_reg_imm32(R::V8, 8);
    a.sub_reg_imm8(R::V7, 1);
    a.jcc_label(C::NotEqual, next);
}
const SAVED: [R; 8] = [R::V1, R::V2, R::V6, R::V7, R::V8, R::V9, R::V10, R::V11];
fn lock<E: PortableAsm>(a: &mut E) {
    let retry = a.create_text_label();
    a.bind_text_label(retry);
    lea_rt(a, R::V8, LOCK);
    a.mov_imm64(R::V0, 0);
    a.mov_imm64(R::V7, 1);
    a.atomic_compare_exchange(R::V8, R::V7);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, retry);
}
fn unlock<E: PortableAsm>(a: &mut E) {
    lea_rt(a, R::V8, LOCK);
    a.mov_imm64(R::V0, 0);
    a.atomic_store_release(R::V8, R::V0);
}
fn page_tag<E: PortableAsm>(a: &mut E) {
    a.mov_reg_reg(R::V9, R::V10);
    a.shl_reg_imm8(R::V9, 3);
    a.add_reg_reg(R::V9, R::V3);
    a.add_reg_imm32(R::V9, layout(a).committed_pages as i32);
}
fn address<E: PortableAsm>(a: &mut E) {
    a.mov_reg_reg(R::V7, R::V10);
    a.shl_reg_imm8(R::V7, 17);
    load_rt(a, R::V6, RT_HEAP_BASE);
    a.add_reg_reg(R::V7, R::V6);
    a.mov_imm64(R::V6, HEAP_PAGE_BYTES);
}
fn save<E: PortableAsm>(a: &mut E) {
    for r in SAVED {
        a.push_reg(r);
    }
    a.mov_reg_reg(R::V10, R::V0);
}
fn restore<E: PortableAsm>(a: &mut E) {
    for r in SAVED.into_iter().rev() {
        a.pop_reg(r);
    }
    a.ret();
}
/// V0=logical page index (1..=physical pages). Other portable registers survive.
pub(super) fn emit<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    if a.gc_large_pages() == crate::GcLargePages::Required {
        grouped::emit(a, l);
        return;
    }
    a.bind_text_label(l.ensure_heap_page);
    save(a);
    a.cmp_reg_imm8(R::V10, 1);
    a.jcc_label(C::Below, l.fail);
    a.cmp_reg_imm32(R::V10, geometry(a).pages as i32);
    a.jcc_label(C::Above, l.fail);
    lock(a);
    a.mov_reg_reg(R::V9, R::V10);
    a.shl_reg_imm8(R::V9, 3);
    a.add_reg_reg(R::V9, R::V3);
    a.add_reg_imm32(R::V9, layout(a).generations as i32);
    a.atomic_load_acquire(R::V0, R::V9);
    let owned = a.create_text_label();
    a.cmp_reg_imm8(R::V0, generation::YOUNG as i8);
    a.jcc_label(C::Equal, owned);
    a.cmp_reg_imm8(R::V0, generation::OLD as i8);
    a.jcc_label(C::Equal, owned);
    a.cmp_reg_imm8(R::V0, generation::LARGE as i8);
    a.jcc_label(C::NotEqual, l.fail);
    a.bind_text_label(owned);
    // Every binding/ensure cancels a previous Free observation, including
    // reuse of an already committed page between two idle scans.
    a.mov_reg_reg(R::V9, R::V10);
    a.shl_reg_imm8(R::V9, 3);
    a.add_reg_reg(R::V9, R::V3);
    a.add_reg_imm32(R::V9, layout(a).free_since as i32);
    a.mov_imm64(R::V0, 0);
    a.store_ptr_disp32(R::V9, 0, R::V0);
    page_tag(a);
    a.atomic_load_acquire(R::V0, R::V9);
    let ready = a.create_text_label();
    a.cmp_reg_imm8(R::V0, 1);
    a.jcc_label(C::Equal, ready);
    a.cmp_reg_imm8(R::V0, 2);
    a.jcc_label(C::Above, l.fail);
    // No counter/tag is changed until the OS has made the page accessible.
    for r in [R::V9, R::V10, R::V0, R::V0] {
        a.push_reg(r);
    }
    let normal = a.create_text_label();
    let checked = a.create_text_label();
    load_rt(a, R::V0, FAIL_AFTER);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, normal);
    a.sub_reg_imm8(R::V0, 1);
    store_rt(a, FAIL_AFTER, R::V0);
    a.jcc_label(C::NotEqual, normal);
    // Inject the same false return as a failed OS operation, through the
    // ordinary status check rather than a separate failure-only branch.
    a.mov_imm64(R::V0, 0);
    a.jmp_label(checked);
    a.bind_text_label(normal);
    address(a);
    a.call_label(l.memory.commit);
    a.bind_text_label(checked);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, l.heap_commit_failed);
    for r in [R::V0, R::V0, R::V10, R::V9] {
        a.pop_reg(r);
    }
    if a.gc_pre_touch() {
        address(a);
        pretouch::emit(a);
    }
    let first = a.create_text_label();
    a.cmp_reg_imm8(R::V0, 2);
    a.jcc_label(C::NotEqual, first);
    copy::bump(a, RECOMMITS, R::V0);
    a.bind_text_label(first);
    a.mov_imm64(R::V0, 1);
    a.atomic_store_release(R::V9, R::V0);
    copy::bump(a, COMMITS, R::V0);
    load_rt(a, R::V0, COMMITTED);
    a.add_reg_imm32(R::V0, HEAP_PAGE_BYTES as i32);
    store_rt(a, COMMITTED, R::V0);
    load_rt(a, R::V1, PEAK);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, ready);
    store_rt(a, PEAK, R::V0);
    a.bind_text_label(ready);
    unlock(a);
    restore(a);

    // This primitive is called only under the collector lock/quiescent owner.
    // Free roles imply source payload/metadata cleanup is complete. No timer or
    // automatic uncommit policy is implemented by this entry point itself.
    a.bind_text_label(l.uncommit_heap_page);
    save(a);
    a.cmp_reg_imm8(R::V10, 1);
    a.jcc_label(C::Below, l.fail);
    a.cmp_reg_imm32(R::V10, geometry(a).pages as i32);
    a.jcc_label(C::Above, l.fail);
    lock(a);
    let skip = a.create_text_label();
    a.mov_reg_reg(R::V9, R::V10);
    a.shl_reg_imm8(R::V9, 3);
    a.add_reg_reg(R::V9, R::V3);
    a.add_reg_imm32(R::V9, layout(a).generations as i32);
    a.atomic_load_acquire(R::V0, R::V9);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, skip);
    a.mov_reg_reg(R::V9, R::V10);
    a.shl_reg_imm8(R::V9, 3);
    a.add_reg_reg(R::V9, R::V3);
    a.add_reg_imm32(R::V9, layout(a).old_sources as i32);
    a.atomic_load_acquire(R::V0, R::V9);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, skip);
    a.mov_reg_reg(R::V0, R::V10);
    a.sub_reg_imm32(R::V0, 1);
    scan::page_ptr(a, R::V8, R::V0, R::V1);
    a.load_ptr_disp32(R::V0, R::V8, d(offset_of!(AllocationPage, state)));
    a.cmp_reg_imm8(R::V0, PAGE_READY as i8);
    a.jcc_label(C::NotEqual, skip);
    a.load_ptr_disp32(R::V0, R::V8, d(offset_of!(AllocationPage, owner)));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, skip);
    a.load_ptr_disp32(R::V8, R::V8, d(offset_of!(AllocationPage, forward_table)));
    a.load_ptr_disp32(
        R::V0,
        R::V8,
        d(offset_of!(crate::gc_abi::ForwardTable, used)),
    );
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, skip);
    page_tag(a);
    a.atomic_load_acquire(R::V0, R::V9);
    a.cmp_reg_imm8(R::V0, 1);
    a.jcc_label(C::NotEqual, skip);
    load_rt(a, R::V0, COMMITTED);
    a.sub_reg_imm32(R::V0, HEAP_PAGE_BYTES as i32);
    load_rt(a, R::V1, MINIMUM);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, skip);
    for r in [R::V9, R::V9] {
        a.push_reg(r);
    }
    address(a);
    a.call_label(l.memory.decommit);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, l.heap_decommit_failed);
    for r in [R::V9, R::V9] {
        a.pop_reg(r);
    }
    a.mov_imm64(R::V0, 2);
    a.atomic_store_release(R::V9, R::V0);
    load_rt(a, R::V0, COMMITTED);
    a.sub_reg_imm32(R::V0, HEAP_PAGE_BYTES as i32);
    store_rt(a, COMMITTED, R::V0);
    copy::bump(a, UNCOMMITS, R::V0);
    unlock(a);
    a.mov_imm64(R::V0, 1);
    restore(a);
    a.bind_text_label(skip);
    unlock(a);
    a.mov_imm64(R::V0, 0);
    restore(a);
}

const _: () = assert!(UNCOMMITS - MINIMUM == 5 * size_of::<u64>());

/// Exit reporting reads a coherent copy even if idle maintenance resumes
/// between individual write syscalls. Called only with the collector lock.
pub(super) fn snapshot<E: PortableAsm>(a: &mut E) {
    for source in [
        MINIMUM, COMMITTED, PEAK, COMMITS, RECOMMITS, UNCOMMITS, PRETOUCHED,
    ] {
        load_rt(a, R::V0, source);
        store_rt(a, report_offset(source), R::V0);
    }
}
pub(super) const fn report_offset(source: usize) -> usize {
    offset_of!(Runtime, commit.report) + source - MINIMUM
}

#[cfg(test)]
mod tests {
    #[test]
    fn compiler_api_rejects_pre_touch_without_the_generational_runtime() {
        let config = crate::NativeCompilerConfig {
            gc_pre_touch: true,
            ..Default::default()
        };
        let error = crate::compile_source_for_target("touch.kl", "1", config).unwrap_err();
        assert!(error.to_string().contains("require --gc=generational"));
    }

    #[test]
    fn compiler_api_validates_minimum_and_effective_maximum_before_emission() {
        for (generation, minimum, maximum) in [
            (false, 4 << 20, 16 << 20),
            (true, 0, 16 << 20),
            (true, 131073, 16 << 20),
            (true, 32 << 20, 16 << 20),
        ] {
            let config = crate::NativeCompilerConfig {
                gc_generational: generation,
                gc_min_heap: Some(minimum),
                gc_max_heap: Some(maximum),
                ..Default::default()
            };
            assert!(crate::compile_source_for_target("heap.kl", "1", config).is_err());
        }
        let config = crate::NativeCompilerConfig {
            gc_generational: true,
            gc_min_heap: Some(16 << 20),
            gc_max_heap: Some(16 << 20),
            ..Default::default()
        };
        let image = crate::compile_source_for_target("heap.kl", "1", config).unwrap();
        assert_eq!(&image[..4], b"\x7fELF");
    }
}
