//! Byte offsets of the `Runtime` block fields, for emitted code.
use super::Runtime;
use crate::gc_codegen::production::{OwnerState, owner_state};
use std::mem::offset_of;

pub(super) const RT_FRESH_SIZE: usize = owner_state::offset(offset_of!(OwnerState, fresh_size));
pub(super) const RT_FRESH_TAG: usize = owner_state::offset(offset_of!(OwnerState, fresh_tag));
pub(super) const RT_ALLOCATION_STALLS: usize =
    owner_state::offset(offset_of!(OwnerState, allocation_stalls));

pub(super) const RT_M_ROOT_INDEX: usize = offset_of!(Runtime, old_mark.m_root_index);
pub(super) const RT_M_ROOT_BOUND: usize = offset_of!(Runtime, old_mark.m_root_bound);
pub(super) const RT_R_ROOT_INDEX: usize = offset_of!(Runtime, r_root_index);
pub(super) const RT_R_ROOT_BOUND: usize = offset_of!(Runtime, r_root_bound);
pub(super) const RT_R_ROOT_WORK: usize = offset_of!(Runtime, r_root_work);

pub(super) const RT_CTX: usize = owner_state::offset(offset_of!(OwnerState, ctx));
pub(super) const RT_BUILTINS: usize = offset_of!(Runtime, builtins);
pub(super) const RT_OLD_CHARGED_BYTES: usize = offset_of!(Runtime, old_charged_bytes);
pub(super) const RT_OLD_LIMIT_BYTES: usize = offset_of!(Runtime, old_limit_bytes);
pub(super) const RT_YOUNG_LIMIT_PAGES: usize = offset_of!(Runtime, young_limit_pages);
pub(super) const RT_HEAP_CAPACITY_BYTES: usize = offset_of!(Runtime, heap_capacity_bytes);
pub(super) const RT_HEAP_BASE: usize = offset_of!(Runtime, heap_base);
pub(super) const RT_PAGES: usize = offset_of!(Runtime, pages);
pub(super) const RT_NEXT_PAGE: usize = offset_of!(Runtime, next_page);
pub(super) const RT_SEGMENT: usize = owner_state::offset(offset_of!(OwnerState, segment));
pub(super) const RT_JOURNAL: usize = owner_state::offset(offset_of!(OwnerState, journal));
pub(super) const RT_CELL_SP: usize = owner_state::offset(offset_of!(OwnerState, cell_sp));
pub(super) const RT_ALLOC_SIZE: usize = owner_state::offset(offset_of!(OwnerState, alloc_size));
pub(super) const RT_ADOPT_SLOT: usize = owner_state::offset(offset_of!(OwnerState, adopt_slot));
pub(super) const RT_BIND_PAGE: usize = owner_state::offset(offset_of!(OwnerState, bind_page));
pub(super) const RT_PENDING_REF: usize = owner_state::offset(offset_of!(OwnerState, pending_ref));
pub(super) const RT_PENDING_PAGE: usize = owner_state::offset(offset_of!(OwnerState, pending_page));
pub(super) const RT_PENDING_CELL: usize = owner_state::offset(offset_of!(OwnerState, pending_cell));
pub(super) const RT_PUBLISHED: usize = offset_of!(Runtime, published);
pub(super) const RT_OBJECTS: usize = offset_of!(Runtime, objects);
pub(super) const RT_VERIFIED: usize = offset_of!(Runtime, verified);
pub(super) const RT_MARKS: usize = offset_of!(Runtime, marks);
pub(super) const RT_PAGES_USED: usize = offset_of!(Runtime, pages_used);
pub(super) const RT_SPACE_BASE: usize = offset_of!(Runtime, space_base);
pub(super) const RT_SPACE_END: usize = offset_of!(Runtime, space_end);
pub(super) const RT_COLLECTING: usize = owner_state::offset(offset_of!(OwnerState, collecting));
pub(super) const RT_COLLECTIONS: usize = offset_of!(Runtime, collections);
pub(super) const RT_SCAN_INDEX: usize = offset_of!(Runtime, scan_index);
pub(super) const RT_SCAN_OLD: usize = owner_state::offset(offset_of!(OwnerState, scan_old));
pub(super) const RT_SAVED_SIZE: usize = owner_state::offset(offset_of!(OwnerState, saved_size));
pub(super) const RT_SAVED_ELEMENTS: usize =
    owner_state::offset(offset_of!(OwnerState, saved_elements));
pub(super) const RT_SAVED_TAG: usize = owner_state::offset(offset_of!(OwnerState, saved_tag));
pub(super) const RT_SAVED_DESCRIPTOR: usize =
    owner_state::offset(offset_of!(OwnerState, saved_descriptor));
pub(super) const RT_FROM_BASE: usize = offset_of!(Runtime, from_base);
pub(super) const RT_FROM_END: usize = offset_of!(Runtime, from_end);
pub(super) const RT_FROM_MARKS: usize = offset_of!(Runtime, from_marks);
pub(super) const RT_ALLOC_TAG: usize = owner_state::offset(offset_of!(OwnerState, alloc_tag));
pub(super) const RT_CS_PAGE: usize = offset_of!(Runtime, cs_page);
pub(super) const RT_CS_OFF: usize = offset_of!(Runtime, cs_off);
pub(super) const RT_CS_FIELD: usize = offset_of!(Runtime, cs_field);
pub(super) const RT_CS_COUNT: usize = offset_of!(Runtime, cs_count);
pub(super) const RT_CS_KIND: usize = offset_of!(Runtime, cs_kind);
pub(super) const RT_CS_COMPLETED: usize = offset_of!(Runtime, cs_completed);
pub(super) const RT_FORWARD_CELL: usize = owner_state::offset(offset_of!(OwnerState, forward_cell));
pub(super) const RT_SCAN_NEW: usize = owner_state::offset(offset_of!(OwnerState, scan_new));
pub(super) const RT_OLD_TOP: usize = offset_of!(Runtime, old_top);
pub(super) const RT_OLD_BASE: usize = offset_of!(Runtime, old_base);
pub(super) const RT_OLD_FROM: usize = offset_of!(Runtime, old_from);
pub(super) const RT_OLD_FROM_END: usize = offset_of!(Runtime, old_from_end);
pub(super) const RT_FULL: usize = offset_of!(Runtime, full);
pub(super) const RT_FULL_COLLECTIONS: usize = offset_of!(Runtime, full_collections);
pub(super) const RT_PHASE_OPEN: usize = offset_of!(Runtime, phase_open);
pub(super) const RT_BUDGET: usize = offset_of!(Runtime, budget);
pub(super) const RT_RY_LO: usize = offset_of!(Runtime, ry_lo);
pub(super) const RT_RY_HI: usize = offset_of!(Runtime, ry_hi);
pub(super) const RT_RO_LO: usize = offset_of!(Runtime, ro_lo);
pub(super) const RT_RO_HI: usize = offset_of!(Runtime, ro_hi);
pub(super) const RT_HEALED: usize = offset_of!(Runtime, healed);
pub(super) const RT_OLD_SCAN: usize = offset_of!(Runtime, old_scan);
pub(super) const RT_PROMOTED: usize = offset_of!(Runtime, promoted);
pub(super) const RT_REMSET_LEN: usize = offset_of!(Runtime, remset_len);
pub(super) const RT_RS_INDEX: usize = offset_of!(Runtime, rs_index);
pub(super) const RT_REMSET_BASE: usize = offset_of!(Runtime, remset_base);
pub(super) const RT_REMSET_PREV_LEN: usize = offset_of!(Runtime, remset_prev_len);
pub(super) const RT_REMSET_FLIPS: usize = offset_of!(Runtime, remset_flips);
pub(super) const RT_REMSET_FLIPPED: usize = offset_of!(Runtime, remset_flipped);
pub(super) const RT_REMSET_KEPT: usize = offset_of!(Runtime, remset_kept);
pub(super) const RT_LOCK_NEXT: usize = offset_of!(Runtime, lock_next);
pub(super) const RT_LOCK_SERVING: usize = offset_of!(Runtime, lock_serving);
pub(super) const RT_WORK_SEQ: usize = offset_of!(Runtime, work_seq);
pub(super) const RT_WORKER_ALIVE: usize = offset_of!(Runtime, worker_alive);
pub(super) const RT_WORKER_DONE: usize = offset_of!(Runtime, worker_done);
pub(super) const RT_WORKER_SCANNED: usize = offset_of!(Runtime, worker_scanned);
pub(super) const RT_WAKE_PENDING: usize = offset_of!(Runtime, wake_pending);
pub(super) const RT_CS_OLD: usize = offset_of!(Runtime, cs_old);
pub(super) const RT_BOUNDS: usize = offset_of!(Runtime, bounds);
pub(super) const RT_COPY_LOW: usize = offset_of!(Runtime, copy_low);
pub(super) const RT_FROM_COPY_LOW: usize = offset_of!(Runtime, from_copy_low);
pub(super) const RT_RETIRE_STAGE: usize = offset_of!(Runtime, retire_stage);
pub(super) const RT_RETIRE_CURSOR: usize = offset_of!(Runtime, retire_cursor);
pub(super) const RT_RETIRE_END: usize = offset_of!(Runtime, retire_end);
pub(super) const RT_RETIRE_UNITS: usize = offset_of!(Runtime, retire_units);
pub(super) const RT_CB_PAGE: usize = offset_of!(Runtime, cb_page);
pub(super) const RT_CB_OFF: usize = offset_of!(Runtime, cb_off);
pub(super) const RT_PROGRESS: usize = offset_of!(Runtime, progress);
pub(super) const RT_W_PAGE: usize = offset_of!(Runtime, w_page);
pub(super) const RT_W_TOP: usize = offset_of!(Runtime, w_top);
pub(super) const RT_W_LIMIT: usize = offset_of!(Runtime, w_limit);
pub(super) const RT_W_SRC: usize = offset_of!(Runtime, w_src);
pub(super) const RT_W_NEW: usize = offset_of!(Runtime, w_new);
pub(super) const RT_W_DESC: usize = offset_of!(Runtime, w_desc);
pub(super) const RT_W_PROMOTED: usize = offset_of!(Runtime, w_promoted);
pub(super) const RT_W_UNMARKED: usize = offset_of!(Runtime, w_unmarked);
pub(super) const RT_WORKER_COPIES: usize = offset_of!(Runtime, worker_copies);
pub(super) const RT_WORKER_LOST: usize = offset_of!(Runtime, worker_lost);
pub(super) const RT_MUTATOR_COPIES: usize =
    owner_state::offset(offset_of!(OwnerState, mutator_copies));
pub(super) const RT_MUTATOR_LOST: usize = owner_state::offset(offset_of!(OwnerState, mutator_lost));
pub(super) const RT_M_COPYING: usize = offset_of!(Runtime, m_copying);
pub(super) const RT_MARK_ACTIVE: usize = offset_of!(Runtime, old_mark.mark_active);
pub(super) const RT_MARK_DONE: usize = offset_of!(Runtime, old_mark.mark_done);
pub(super) const RT_MARK_VALID: usize = offset_of!(Runtime, old_mark.mark_valid);
pub(super) const RT_MSTACK_LEN: usize = offset_of!(Runtime, old_mark.mstack_len);
pub(super) const RT_M_RELOCATE_INDEX: usize = offset_of!(Runtime, old_mark.m_relocate_index);
pub(super) const RT_M_OBJ: usize = offset_of!(Runtime, old_mark.m_obj);
pub(super) const RT_M_FIELD: usize = offset_of!(Runtime, old_mark.m_field);
pub(super) const RT_M_COUNT: usize = offset_of!(Runtime, old_mark.m_count);
pub(super) const RT_MARK_CYCLES: usize = offset_of!(Runtime, old_mark.mark_cycles);
pub(super) const RT_MARKED: usize = offset_of!(Runtime, old_mark.marked);
pub(super) const RT_MARK_DRAINS: usize = offset_of!(Runtime, old_mark.mark_drains);
pub(super) const RT_UNMARKED: usize = offset_of!(Runtime, unmarked);
pub(super) const RT_REMSET_DROPPED: usize = offset_of!(Runtime, remset_dropped);
pub(super) const RT_LIT_MARK: usize = offset_of!(Runtime, lit_mark);
pub(super) const RT_NEXT_OBJECT_ID: usize = offset_of!(Runtime, next_object_id);
pub(super) const RT_REQUEST: usize = owner_state::offset(offset_of!(OwnerState, request));
pub(super) const RT_INTO: usize = owner_state::offset(offset_of!(OwnerState, into));
pub(super) const RT_RESULT: usize = owner_state::offset(offset_of!(OwnerState, result));
pub(super) const RT_ALLOC_ROOT: usize = owner_state::offset(offset_of!(OwnerState, alloc_root));
pub(super) const RT_FRAME_ROOT: usize = owner_state::offset(offset_of!(OwnerState, frame_root));
pub(super) const RT_SPEC: usize = owner_state::offset(offset_of!(OwnerState, spec));
pub(super) const RT_ACCESS: usize = owner_state::offset(offset_of!(OwnerState, access));
pub(super) const RT_PUBLISH: usize = owner_state::offset(offset_of!(OwnerState, publish));

pub(super) const RT_MP_STATE: usize = offset_of!(Runtime, old_mark.mp_state);
pub(super) const RT_MP_REQUESTS: usize = offset_of!(Runtime, old_mark.mp_requests);
pub(super) const RT_MP_CANCELS: usize = offset_of!(Runtime, old_mark.mp_cancels);

pub(super) const RT_DM_SLICES: usize =
    offset_of!(Runtime, drain) + offset_of!(super::drain::Stats, mark_slices);
pub(super) const RT_DM_YIELDS: usize =
    offset_of!(Runtime, drain) + offset_of!(super::drain::Stats, mark_yields);
pub(super) const RT_DM_MAX: usize =
    offset_of!(Runtime, drain) + offset_of!(super::drain::Stats, mark_max_units);
pub(super) const RT_DM_WORKER: usize =
    offset_of!(Runtime, drain) + offset_of!(super::drain::Stats, worker_mark_units);
pub(super) const RT_DM_ASSISTED: usize =
    offset_of!(Runtime, drain) + offset_of!(super::drain::Stats, mark_worker_units);
pub(super) const RT_DM_START: usize =
    offset_of!(Runtime, drain) + offset_of!(super::drain::Stats, mark_worker_start);

pub(super) const RT_DR_SLICES: usize =
    offset_of!(Runtime, drain) + offset_of!(super::drain::Stats, relocate_slices);

pub(super) const RT_DR_YIELDS: usize =
    offset_of!(Runtime, drain) + offset_of!(super::drain::Stats, relocate_yields);

pub(super) const RT_DR_MAX: usize =
    offset_of!(Runtime, drain) + offset_of!(super::drain::Stats, relocate_max_units);

pub(super) const RT_DR_WORKER: usize =
    offset_of!(Runtime, drain) + offset_of!(super::drain::Stats, worker_relocate_units);

pub(super) const RT_DR_ASSISTED: usize =
    offset_of!(Runtime, drain) + offset_of!(super::drain::Stats, relocate_worker_units);

pub(super) const RT_DR_START: usize =
    offset_of!(Runtime, drain) + offset_of!(super::drain::Stats, relocate_worker_start);

pub(super) const RT_END_CHECKS: usize =
    offset_of!(Runtime, drain) + offset_of!(super::drain::Stats, end_checks);
pub(super) const RT_END_RETRIES: usize =
    offset_of!(Runtime, drain) + offset_of!(super::drain::Stats, end_retries);

pub(super) const RT_CYCLE_CHECKS: usize =
    offset_of!(Runtime, drain) + offset_of!(super::drain::Stats, cycle_checks);

pub(super) const RT_CYCLE_RETRIES: usize =
    offset_of!(Runtime, drain) + offset_of!(super::drain::Stats, cycle_retries);

pub(super) const RT_CYCLE_READY: usize =
    offset_of!(Runtime, drain) + offset_of!(super::drain::Stats, ready_cycles);
