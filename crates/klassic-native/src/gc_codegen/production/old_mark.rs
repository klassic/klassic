//! Old-cycle state, independent of Young relocation and its remap parity.
//! The Old tracer follows both physical generations to discover Old reachability.
//! Young collection traces only its roots/remset and copies reachable Young data.
#[repr(C)]
pub(super) struct State {
    pub(super) mark_active: u64,
    pub(super) mark_done: u64,
    /// Constant-time readiness: 0 idle, 2 Ready for owner activation.
    pub(super) mp_state: u64,
    pub(super) mp_requests: u64,
    pub(super) mp_cancels: u64,
    pub(super) mark_valid: u64,
    pub(super) mstack_len: u64,
    pub(super) m_relocate_index: u64,
    /// Encoded GcRef retains an unfinished field-scan frontier.
    pub(super) m_obj: u64,
    pub(super) m_field: u64,
    pub(super) m_count: u64,
    /// Root-cell mark cursor/bound, captured at mark start and drained by quanta.
    pub(super) m_root_index: u64,
    pub(super) m_root_bound: u64,
    pub(super) object_bound: u64,
    pub(super) mark_cycles: u64,
    pub(super) marked: u64,
    pub(super) mark_drains: u64,
    pub(super) request_full: u64,
    pub(super) cycle_checks: u64,
    pub(super) cycle_retries: u64,
    pub(super) planned_cycles: u64,
    /// Collector-lock admissions and owner-only fresh black allocations.
    pub(super) trace_bytes: u64,
    pub(super) black_bytes: u64,
}

pub(super) const OBJECT_BOUND: usize = std::mem::offset_of!(super::Runtime, old_mark.object_bound);
