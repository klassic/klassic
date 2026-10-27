//! One host-selected geometry for heap mapping, metadata and generation budgets.
use super::*;
use crate::gc_codegen::production::OwnerState;
pub(crate) const DEFAULT_CAPACITY: u64 = 256 << 20;
pub(crate) const MIN_CAPACITY: u64 = 16 << 20;
#[derive(Clone, Copy)]
pub(super) struct Geometry {
    pub heap_bytes: u64,
    pub pages: u64,
    pub rbits_words: u64,
    pub bank_bytes: u64,
    pub young_pages: u64,
    pub young_min: u64,
    pub young_bytes: u64,
    pub old_bytes: u64,
    pub old_min: u64,
    pub old_max: u64,
}
impl Geometry {
    pub(super) fn new(capacity: u64) -> Result<Self, &'static str> {
        if capacity < MIN_CAPACITY {
            return Err("GC maximum heap must be at least 16 MiB");
        }
        if !capacity.is_multiple_of(HEAP_PAGE_BYTES) {
            return Err("GC maximum heap must be a multiple of 128 KiB");
        }
        // All metadata offsets and heap bounds use signed 32-bit immediates.
        // Reject before deriving sizes, rather than wrapping any emitted bound.
        if capacity > (i32::MAX as u64 - HEAP_PAGE_BYTES) {
            return Err("GC maximum heap exceeds direct-emitter address limits");
        }
        if build_layout(capacity).total > i32::MAX as u64 {
            return Err("GC maximum heap requires metadata beyond direct-emitter address limits");
        }
        let pages = capacity / HEAP_PAGE_BYTES;
        let young_pages = maximum_young_pages(capacity);
        let young_bytes = young_pages * HEAP_PAGE_BYTES;
        let old_bytes = pages / 3 * HEAP_PAGE_BYTES;
        Ok(Self {
            heap_bytes: capacity + HEAP_PAGE_BYTES,
            pages,
            rbits_words: (capacity + HEAP_PAGE_BYTES) / 512,
            bank_bytes: pages * old_runs::WORDS * 8,
            young_pages,
            young_min: 64.min(young_pages),
            young_bytes,
            old_bytes,
            old_min: old_bytes / 4,
            old_max: old_bytes - young_bytes,
        })
    }
}
pub(crate) fn validate(capacity: u64) -> Result<(), &'static str> {
    Geometry::new(capacity).map(|_| ())
}

/// Hard reservation bounds both allocation/copy streams. Soft windows may
/// grow back to this limit for protected-copy safety, so arena sizing must
/// not follow an advisory target or the current active window.
pub(super) const fn maximum_young_pages(payload: u64) -> u64 {
    let pages = payload / HEAP_PAGE_BYTES / 6;
    if pages < HALF { pages } else { HALF }
}
pub(super) const fn mark_records(payload: u64) -> u64 {
    maximum_young_pages(payload) * CELLS_PER_PAGE
}
pub(super) const fn mark_bank_bytes(payload: u64) -> u64 {
    mark_records(payload) * size_of::<MarkRecord>() as u64
}

/// Byte offsets inside the metadata mapping.
#[derive(Clone, Copy)]
pub(super) struct Layout {
    pub(super) pages: u64,
    pub(super) starts: u64,
    pub(super) tables: u64,
    pub(super) cells: u64,
    pub(super) marks: u64,
    pub(super) owner: u64,
    pub(super) roots: u64,
    pub(super) roots_bytes: u64,
    pub(super) remset: u64,
    pub(super) remset2: u64,
    pub(super) rbits: u64,
    pub(super) mstack: u64,
    pub(super) ystack: u64,
    pub(super) copy_marks: u64,
    pub(super) generations: u64,
    pub(super) owner_cards: u64,
    pub(super) pool_history: u64,
    pub(super) old_runs: u64,
    pub(super) old_sources: u64,
    pub(super) committed_pages: u64,
    pub(super) free_since: u64,
    pub(super) large_records: u64,
    pub(super) colours: u64,
    pub(super) relocate_pages: u64,
    pub(super) metadata_lease_slots: u64,
    pub(super) weak_entries: u64,
    pub(super) total: u64,
    pub(super) events: u64,
}

pub(super) const fn build_layout(payload: u64) -> Layout {
    let pool_pages = payload / HEAP_PAGE_BYTES;
    let entries = pool_pages + 1;
    let heap_bytes = payload + HEAP_PAGE_BYTES;
    let rbits_words = heap_bytes / 8 / 64;
    let old_bank_bytes = pool_pages * old_runs::WORDS * 8;
    let pages = round_up(size_of::<Runtime>() as u64, 4096);
    let starts = pages + pool_pages * size_of::<AllocationPage>() as u64;
    let tables = starts + pool_pages * STARTS_WORDS * 8;
    let cells = round_up(tables + pool_pages * size_of::<ForwardTable>() as u64, 64);
    let marks = cells + pool_pages * CELLS_PER_PAGE * size_of::<ForwardCell>() as u64;
    let owner = round_up(marks + 2 * mark_bank_bytes(payload), 64);
    let roots = owner + offset_of!(OwnerState, roots) as u64;
    let roots_bytes = size_of::<RootStorage>() as u64
        + ROOT_CELLS * size_of::<RootCell>() as u64
        + JOURNAL_ENTRIES * size_of::<RootChange>() as u64;
    let remset = round_up(roots + roots_bytes, 64);
    let remset2 = round_up(remset + REMSET_ENTRIES * 8, 64);
    let rbits = round_up(remset2 + REMSET_ENTRIES * 8, 64);
    let mstack = round_up(rbits + 2 * rbits_words * 8, 64);
    let ystack = round_up(mstack + MSTACK_ENTRIES * 8, 64);
    let copy_marks = round_up(ystack + MSTACK_ENTRIES * 8, 64);
    let generations = round_up(copy_marks + (heap_bytes / HEAP_PAGE_BYTES) * 8, 64);
    let owner_cards = round_up(generations + entries * 8, 64);
    let pool_history = round_up(owner_cards + (heap_bytes / slot_owner::CARD_BYTES) * 8, 64);
    let old_runs = round_up(pool_history + entries * 8, 64);
    let old_sources = round_up(old_runs + 2 * old_bank_bytes, 64);
    let committed_pages = round_up(old_sources + entries * 8, 64);
    let free_since = round_up(committed_pages + entries * 8, 64);
    let large_records = round_up(free_since + entries * 8, 64);
    let colours = round_up(large_records + entries * 32, 64);
    let relocate_pages = round_up(colours + 32 * 8, 64);
    let metadata_lease_slots = round_up(relocate_pages + pool_pages * 16, 64);
    let weak_entries = round_up(
        metadata_lease_slots + super::metadata_leases::CAPACITY * 32,
        64,
    );
    let total = round_up(weak_entries + super::weak::CAPACITY * 56, 4096);
    Layout {
        pages,
        starts,
        tables,
        cells,
        marks,
        owner,
        roots,
        roots_bytes,
        remset,
        remset2,
        rbits,
        mstack,
        ystack,
        copy_marks,
        generations,
        owner_cards,
        pool_history,
        old_runs,
        old_sources,
        committed_pages,
        free_since,
        large_records,
        colours,
        relocate_pages,
        metadata_lease_slots,
        weak_entries,
        events: total,
        total,
    }
}

pub(super) fn layout<E: PortableAsm>(a: &E) -> Layout {
    let mut l = build_layout(a.gc_heap_capacity());
    if a.gc_json_events() {
        l.events = round_up(l.total, a.gc_metadata_alignment());
        l.total = super::trace::checked_total(l.events, a.gc_event_capacity())
            .expect("validated JSON history geometry");
    }
    l
}
pub(super) fn geometry<E: PortableAsm>(a: &E) -> geometry::Geometry {
    geometry::Geometry::new(a.gc_heap_capacity()).expect("validated GC geometry")
}

/// Advisory sizing only. Safety bounds and actual reservations use geometry().
/// The minimum policy geometry preserves the collector's bootstrap/reserve floors.
pub(super) fn policy_geometry<E: PortableAsm>(a: &E) -> Geometry {
    let soft = a.gc_heap_soft_maximum();
    let mut policy = Geometry::new(soft.max(MIN_CAPACITY)).expect("validated soft heap target");
    if soft < MIN_CAPACITY {
        // Soft targets may be smaller than a viable whole collector heap.
        // Four Young pages cover the copier/fresh-stream bootstrap; these
        // advisory floors never reduce the hard protected-copy reserve.
        policy.young_pages = (soft / HEAP_PAGE_BYTES / 6).max(4);
        policy.young_min = policy.young_pages.min(64);
        policy.young_bytes = policy.young_pages * HEAP_PAGE_BYTES;
        policy.old_bytes = (soft / HEAP_PAGE_BYTES / 3).max(1) * HEAP_PAGE_BYTES;
        policy.old_min = policy.old_bytes / 4;
        policy.old_max = policy
            .old_bytes
            .saturating_sub(policy.young_bytes)
            .max(HEAP_PAGE_BYTES);
    }
    policy
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn geometry_preserves_guard_and_separate_generation_reservations_at_all_sizes() {
        for capacity in [16, 32, 192, 256, 512, 1024].map(|m| m << 20) {
            let g = Geometry::new(capacity).unwrap();
            let l = build_layout(capacity);
            assert_eq!(g.pages * HEAP_PAGE_BYTES, capacity);
            assert_eq!(g.heap_bytes, capacity + HEAP_PAGE_BYTES);
            assert!(2 * g.old_bytes + 2 * g.young_bytes <= capacity);
            assert!(l.total < i32::MAX as u64);
            assert!(l.old_sources + (g.pages + 1) * 8 <= l.committed_pages);
            assert!(l.committed_pages + (g.pages + 1) * 8 <= l.free_since);
            assert!(l.free_since + (g.pages + 1) * 8 <= l.colours);
            assert!(l.colours + 32 * 8 <= l.total);
        }
    }
    #[test]
    fn public_compiler_config_rejects_invalid_limits_before_codegen() {
        for (generational, capacity) in [(false, 16 << 20), (true, 1), (true, u64::MAX)] {
            let config = crate::NativeCompilerConfig {
                gc_generational: generational,
                gc_max_heap: Some(capacity),
                ..Default::default()
            };
            let e = crate::compile_source_for_target("heap.kl", "1", config).unwrap_err();
            assert!(e.diagnostic().message.contains("heap"));
        }
    }
    #[test]
    fn public_compiler_config_validates_advisory_target_and_preserves_hard_geometry() {
        for (generational, soft) in [
            (false, 32 << 20),
            (true, 0),
            (true, 16 << 20 | 1),
            (true, 8 << 20),
            (true, 512 << 20),
        ] {
            let config = crate::NativeCompilerConfig {
                gc_generational: generational,
                gc_soft_max_heap: Some(soft),
                ..Default::default()
            };
            let error = crate::compile_source_for_target("soft.kl", "1", config).unwrap_err();
            assert!(error.diagnostic().message.contains("heap"));
        }
        assert!(crate::validate_gc_soft_heap_limits(128 << 10, 128 << 10, 16 << 20).is_ok());
    }

    #[test]
    fn public_compiler_config_checks_the_separate_metadata_budget_before_codegen() {
        for (generational, bytes) in [(false, 4096), (true, 0), (true, 4097), (true, u64::MAX)] {
            let config = crate::NativeCompilerConfig {
                gc_generational: generational,
                gc_max_metadata: Some(bytes),
                ..Default::default()
            };
            let error = crate::compile_source_for_target("metadata.kl", "1", config).unwrap_err();
            assert!(error.diagnostic().message.contains("metadata"));
        }
    }
}
