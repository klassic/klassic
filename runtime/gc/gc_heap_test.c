#define _GNU_SOURCE
#include "gc_native_buffer_incoming_cases.h"
#include "gc_native_buffer_quiescence_cases.h"
#include "gc_pin_page_cases.h"
#include "gc_pin_registry_cases.h"
#include "gc_root_pin_cases.h"
#include "gc_native_pin_cases.h"
#include "gc_native_heap_cases.h"
#include "gc_native_runtime_cases.h"
#include "gc_native_allocation_cases.h"
#include "gc_native_allocation_retire_cases.h"
#include "gc_native_bitmap_cases.h"
#include "gc_native_pin_wait_cases.h"
#include "gc_native_pin_registry_cases.h"
#include "gc_pin_source_release_cases.h"
#include "gc_heap_incoming_cases.h"
#include "gc_heap_walk_cases.h"
#include "gc_object_walk_cases.h"
#include "gc_heap_audit_slice_cases.h"
#include "gc_heap_audit_cases.h"
#include "gc_root_cancel_cases.h"
#include "gc_root_copy_cases.h"
#include "gc_logical_cas_cases.h"
#include "gc_heap_pressure_cases.h"
#include "gc_cycle_pressure_cases.h"
#include "gc_cycle_mark_start_cases.h"
#include "gc_native_cycle_cases.h"
#include "gc_native_graph_cases.h"
#include "gc_relocation_phase_cases.h"
#include "gc_root_storage_cases.h"
#include "gc_root_storage_disposal_race.h"
#include "gc_native_roots_cases.h"
#include "gc_native_root_publish_cases.h"
#include "gc_pending_follow_cases.h"
#include "gc_allocation_shape_cases.h"
#include "gc_native_builtin_registry_cases.h"
#include "gc_builtin_descriptor_cases.h"
#include "gc_mark_initializing_race.h"
#include "gc_initialization_cases.h"
#include "gc_scalar_cases.h"
#include "gc_rooted_scalar_cases.h"
#include "gc_rooted_store_commit_cases.h"
#include "gc_rooted_store_attempt_cases.h"
#include "gc_rooted_store_cases.h"
#include "gc_rooted_store_concurrency.h"
#include "gc_barrier_assist_cases.h"
#include "gc_initialization_store_cases.h"
#include "gc_initialization_copy_cases.h"
#include "gc_metadata_grow_cases.h"
#include "gc_native_metadata_cases.h"
#include "gc_root_journal_concurrency.h"
#include "gc_metadata_concurrency.h"
#include "gc_mark_cases.h"
#include "gc_mark_scan_commit_cases.h"
#include "gc_mark_scan_cases.h"
#include "gc_source_drain_cases.h"
#include "gc_source_uncommit_cases.h"
#include "gc_source_retire_cases.h"
#include "gc_source_retire_pipeline.h"
#include "gc_forward_incoming_cases.h"
#include "gc_mark_concurrency.h"
#include "gc_root_access_cases.h"
#include "gc_survivor_age_cases.h"
#include "gc_promotion_remember_cases.h"
#include "gc_relocation_turn_cases.h"
#include "gc_forward_compress_cases.h"
#include "gc_cycle_cases.h"
#include "gc_remembered_cases.h"
#include "gc_remembered_inventory_cases.h"
#include "gc_remembered_transfer_cases.h"
#include "gc_remembered_owner_cases.h"
#include "gc_remembered_owner_race.h"
#include "gc_remembered_load_cases.h"
#include "gc_remembered_pipeline.h"
#include "gc_remembered_walk_cases.h"
#include "gc_mark_route_cases.h"
#include "gc_mark_move_pipeline.h"
#include "gc_mark_spill_cases.h"
#include "gc_forward_concurrency.h"
#include "gc_heap_cases.h"
#include "gc_relocation_job_cases.h"
#include "gc_relocation_complete_cases.h"
#include "gc_relocation_claim_cases.h"
#include "gc_relocation_cancel_cases.h"
#include "gc_alloc_into_cases.h"
#include "gc_alloc_start_cases.h"
#include "gc_relocation_claim_concurrency.h"
#include "gc_forward_table_concurrency.h"
#include "gc_resolve_chain.h"
#include "gc_relocate_cases.h"
#include "gc_relocate_target_cases.h"
#include "gc_relocate_pipeline.h"
#include "gc_page_release_cases.h"
#include "gc_page_release_pipeline.h"
#include "gc_barrier_cases.h"
#include "gc_barrier_pipeline.h"
#include "gc_barrier_race.h"
#include "gc_root_access_cleanup_race.h"
#include "gc_logical_cas_order.h"
#include "gc_barrier_load_cases.h"
#include "gc_barrier_heal_cases.h"
#include "gc_barrier_load_pipeline.h"
#include "gc_pages_cases.h"
#include "gc_descriptor_concurrency.h"
#include "gc_pages_pipeline.h"
#include <pthread.h>
#include <sched.h>

#include "gc_weak_release_cases.h"
#include "gc_weak_table_cases.h"
#include "gc_weak_table_access_cases.h"
#include "gc_native_weak_cases.h"
#include "gc_weak_mark_cases.h"
#include "gc_weak_liveness_cases.h"
#include "gc_weak_process_cases.h"
#include "gc_weak_scan_cases.h"
#include "gc_weak_scan_concurrency.h"
#include "gc_weak_cycle_cases.h"
#include "gc_weak_incoming_cases.h"
#include "gc_weak_follow_cases.h"
int main(void) {
    gc_run_pin_page_cases(gc_pin_page_locked);
    gc_run_pin_registry_cases(gc_pin_registry_bind_locked,gc_pin_registry_unbind_locked,
        gc_pin_registry_step_locked,gc_pin_selection_locked,gc_pin_relocation_claim_locked);
    gc_run_root_pin_cases(gc_root_pin_c_ops());
    gc_run_pin_dispatch_cases(gc_root_pin_dispatch);
    gc_run_native_pin_cases(gc_root_pin_dispatch,gc_native_pin_c_ops());
    gc_run_native_pin_registry_cases();
    gc_run_native_heap_cases();
    gc_run_native_runtime_cases();
    gc_run_native_bitmap_cases();
    gc_run_native_pin_wait_cases(gc_native_pin_wait_arm_locked,gc_pin_event_notify,gc_pin_event_park,gc_root_finish_notify_locked);
    gc_run_pin_source_release_cases(gc_pin_source_drain_entry_locked,gc_pin_source_uncommit_entry_locked);
    gc_run_weak_table_access_cases(gc_weak_table_step_locked);
    gc_run_weak_follow_cases(gc_weak_follow_step_locked);
    gc_run_weak_incoming_cases(gc_pending_follow_locked,gc_metadata_step_locked,gc_weak_incoming_step_locked);
    gc_run_heap_incoming_cases(gc_heap_incoming);
    gc_run_heap_walk_cases(gc_heap_walk_locked);
    gc_run_object_walk_cases(gc_object_walk_locked);
    gc_run_heap_audit_slice_cases(gc_heap_audit_slice);
    gc_run_heap_audit_cases(gc_heap_audit_step);
    gc_run_worker_cases(gc_worker_step_locked);
    gc_run_root_journal_cases(gc_root_journal_step);
    gc_run_buffer_job_cases(gc_buffer_job_step);
    gc_run_native_buffer_quiescence_cases(gc_buffer_job_step);
    gc_run_native_buffer_incoming_cases();
    gc_run_root_audit_cases(gc_root_audit_step_locked);
    gc_run_root_retire_audit_cases(gc_root_retire_audit_step_locked);
    gc_run_mark_end_cases(gc_mark_end_check_locked);
    gc_run_retire_inventory_cases(gc_retire_inventory_check_locked);
    gc_run_mark_phase_cases(gc_mark_phase_step_locked);
    gc_run_weak_mark_cases(gc_mark_phase_step_locked,gc_weak_load_into_locked,gc_weak_mark_step_locked);
    gc_run_weak_liveness_cases(gc_guarded_weak_liveness);
    gc_run_weak_process_cases(&(WeakProcessOps){gc_reference_lease_locked,gc_pending_follow_locked,gc_pending_access_locked,gc_resolve_access,gc_guarded_weak_liveness,gc_metadata_step_locked},gc_weak_process_capture_locked,gc_weak_process_access_step,gc_weak_process_finish_locked);
    gc_run_weak_scan_cases(&(WeakProcessOps){gc_reference_lease_locked,gc_pending_follow_locked,gc_pending_access_locked,gc_resolve_access,gc_guarded_weak_liveness,gc_metadata_step_locked},&(WeakTableOps){gc_root_step,gc_reference_lease_locked,gc_weak_load_into_locked,gc_weak_release_locked},gc_weak_scan_step_locked,gc_weak_process_access_step,gc_weak_process_finish_locked);
    gc_run_weak_scan_concurrency(&(WeakProcessOps){gc_reference_lease_locked,gc_pending_follow_locked,gc_pending_access_locked,gc_resolve_access,gc_guarded_weak_liveness,gc_metadata_step_locked},&(WeakTableOps){gc_root_step,gc_reference_lease_locked,gc_weak_load_into_locked,gc_weak_release_locked},gc_weak_process_access_step,gc_weak_process_finish_locked,gc_weak_scan_step_locked);
    gc_run_weak_cycle_cases(&(WeakCycleOps){{gc_reference_lease_locked,gc_pending_follow_locked,gc_pending_access_locked,gc_resolve_access,gc_guarded_weak_liveness,gc_metadata_step_locked},{gc_root_step,gc_reference_lease_locked,gc_weak_load_into_locked,gc_weak_release_locked},gc_weak_mark_step_locked,gc_pending_mark,gc_root_journal_step,gc_mark_step_locked,gc_mark_scan_slice,gc_root_audit_step_locked,gc_root_walk_scan,gc_descriptor_register,gc_relocation_phase_step_locked,gc_weak_process_access_step,gc_weak_process_finish_locked,gc_weak_scan_step_locked,gc_weak_relocation_begin_locked});
    gc_run_cycle_mark_start_cases(gc_cycle_mark_start_locked);
    gc_run_native_cycle_cases(gc_cycle_mark_start_locked);
    gc_run_native_graph_cases(&(NativeGraphOps){gc_try_allocate_start,gc_cycle_mark_start_locked,gc_pending_mark,gc_mark_scan_slice,gc_mark_scan_commit_locked});
    gc_run_relocation_phase_cases(gc_relocation_phase_step_locked);
    gc_run_relocation_retire_cases(gc_relocation_retire_step_locked);
    gc_run_cycle_cases(gc_cycle_step_locked);
    gc_run_remembered_cases(gc_remembered_step_locked);
    gc_run_remembered_inventory_cases(gc_remembered_inventory_step_locked);
    gc_run_remembered_transfer_cases(gc_remembered_transfer_locked);
    gc_run_remembered_owner_cases(gc_remembered_owner);
    gc_run_remembered_lease_cases(gc_remembered_lease_locked);
    gc_run_remembered_walk_cases(gc_remembered_walk_locked,gc_remembered_inventory_step_locked);
    gc_run_remembered_owner_races(gc_remembered_owner);
    gc_run_remembered_end(gc_remembered_inventory_step_locked,gc_mark_end_check_locked);
    gc_run_root_journal_races(gc_root_journal_step,gc_root_step);
    GraphLifecycle lifecycle={gc_worker_step_locked,gc_handshake_step_locked,gc_root_life_locked,gc_root_clear_chunk,gc_root_scan_leased,gc_root_journal_step,gc_buffer_job_step,gc_root_audit_step_locked,gc_mark_end_check_locked,gc_mark_phase_step_locked,gc_root_walk_scan,gc_root_walk_heal,gc_root_walk_item,gc_relocation_gate_locked,gc_relocation_complete_locked,gc_relocation_claim_locked,gc_cycle_step_locked,gc_cycle_mark_start_locked,gc_guarded_identity,gc_guarded_block_size,gc_marked_relocation_claim_locked,gc_relocation_phase_step_locked,gc_relocation_retire_step_locked,gc_root_retire_audit_step_locked,gc_retire_inventory_check_locked,gc_source_retire_locked};
    gc_run_mark_retarget_cases(gc_mark_retarget_locked,gc_mark_step_locked);
    MarkMoveOps move={gc_pool_step_locked,gc_tlab_page_locked,gc_try_tlab_allocate,gc_descriptor_register,
        gc_reference_lease_locked,gc_metadata_step_locked,gc_resolve_access,gc_root_step,gc_root_follow_locked,
        gc_guarded_mark_prepare,gc_mark_retarget_locked,gc_mark_step_locked,gc_mark_spill_locked,gc_mark_scan_slice,
        gc_forward_step,gc_source_access_step,gc_relocate_target_locked,gc_relocate_copy,gc_relocate_publish,gc_survivor_age_locked,gc_marked_survivor_age_locked};
    gc_run_mark_move_pipeline(&move,gc_remembered_inventory_step_locked,gc_promotion_remember_locked);
    gc_run_remembered_pipeline(&move,gc_remembered_inventory_step_locked,gc_remembered_load_into,gc_guarded_mark_route,gc_remembered_transfer_locked,gc_remembered_owner,gc_remembered_lease_locked,gc_root_drain,gc_promotion_remember_locked);
    gc_run_mark_prepare_cases(gc_guarded_mark_prepare);
    gc_run_mark_route_cases(gc_guarded_mark_route);
    gc_run_root_cancel_cases(gc_root_cancel_empty);
    gc_run_root_copy_cases(gc_root_copy);
    gc_run_logical_cas_cases(gc_guarded_compare_exchange);
    gc_run_identity_cases(gc_guarded_identity);
    gc_run_block_size_cases(gc_guarded_block_size);
    gc_run_observe_cases(gc_guarded_observe_into);
    gc_run_heap_pressure_cases(gc_heap_allocation_pressure);
    gc_run_cycle_pressure_cases(gc_cycle_pressure_locked);
    gc_run_root_storage_cases(gc_root_storage_prepare);
    gc_run_root_storage_disposal_cases(gc_root_storage_can_dispose);
    gc_run_root_disposal_race(gc_root_storage_can_dispose);
    gc_run_allocation_shape_cases(gc_allocation_shape);
    gc_run_native_metadata_cases(gc_budget_step_locked,NULL);
    NativeMetadataOps builtin_native_ops=gc_native_metadata_ops();
    gc_run_native_builtin_registry_cases(gc_descriptor_register,gc_builtin_descriptors_prepare,gc_budget_step_locked,&builtin_native_ops);
    gc_run_native_roots_cases(gc_root_storage_prepare,gc_root_storage_can_dispose,gc_budget_step_locked,&builtin_native_ops);
    gc_run_weak_load_cases(gc_weak_load_into_locked);
    gc_run_weak_release_cases(gc_weak_release_locked,gc_weak_load_into_locked);
    gc_run_weak_table_cases(&(WeakTableOps){gc_root_step,gc_reference_lease_locked,gc_weak_load_into_locked,gc_weak_release_locked},gc_weak_create_step_locked);
    gc_run_native_weak_cases(gc_budget_step_locked,&builtin_native_ops,&(WeakTableOps){gc_root_step,gc_reference_lease_locked,gc_weak_load_into_locked,gc_weak_release_locked});
    gc_run_pending_follow_cases(gc_pending_follow_locked,gc_pending_access_locked);
    gc_run_native_root_publish_cases(gc_native_roots_publish_locked,&builtin_native_ops);
    gc_run_metadata_grow_cases(gc_metadata_grow_locked);
    gc_run_scalar_cases(gc_guarded_scalar);
    gc_run_rooted_scalar_cases(gc_rooted_scalar);
    gc_run_rooted_store_commit_cases(gc_rooted_store_commit);
    gc_run_rooted_store_attempt_cases(gc_rooted_store_attempt);
    gc_run_rooted_store_dispatch_cases(gc_rooted_store_dispatch);
    gc_run_rooted_store_concurrency(gc_rooted_store_dispatch,gc_rooted_load_dispatch,gc_barrier_poll,gc_pending_mark);
    gc_run_barrier_assist_cases(gc_barrier_assist,gc_barrier_poll);
    gc_run_initialization_cases(gc_guarded_initialization_publish);
    gc_run_initialization_store_cases(gc_guarded_initialization_store);
    gc_run_initialization_copy_cases(gc_guarded_initialization_copy);
    gc_run_mark_initializing_races(gc_guarded_mark_route,gc_source_access_step,gc_guarded_initialization_publish);
    gc_run_guarded_load_cases(gc_guarded_load_into);
    gc_run_guarded_heal_cases(gc_guarded_heal);
    gc_run_remembered_load_cases(gc_remembered_load_into);
    gc_run_guarded_load_pipeline(gc_guarded_mark_prepare,gc_mark_step_locked,gc_mark_scan_slice,gc_root_follow_locked,gc_guarded_load_into,gc_guarded_store,gc_pool_step_locked,gc_tlab_page_locked,gc_try_tlab_allocate,gc_descriptor_register,gc_reference_lease_locked,gc_metadata_step_locked,gc_resolve_access,gc_root_step,gc_root_drain,gc_forward_step,gc_source_access_step,gc_relocate_target_locked,gc_relocate_copy,gc_relocate_publish,gc_root_access_locked,gc_guarded_scalar,gc_root_copy,gc_guarded_compare_exchange,gc_guarded_identity,gc_guarded_observe_into,gc_root_access_cleanup,gc_rooted_observation,gc_mark_spill_locked,gc_rooted_mark,gc_rooted_mark_spill,gc_rooted_mark_set,gc_rooted_mark_publish,gc_guarded_heal,gc_rooted_load,gc_worker_step_locked,gc_rooted_load_admit_locked,gc_rooted_value,gc_rooted_store);
    gc_run_guarded_store_cases(gc_guarded_store);
    gc_run_guarded_store_races(gc_guarded_store,gc_source_access_step);
    gc_run_logical_cas_order(gc_guarded_compare_exchange,gc_source_access_step);
    gc_run_guarded_store_moves(gc_guarded_store,gc_guarded_heal,gc_source_access_step,gc_forward_step,gc_relocate_copy,gc_relocate_publish,gc_relocate_target_locked,gc_remembered_inventory_step_locked);
    gc_run_page_release_cases(gc_page_release_locked);
    gc_run_page_release_pipeline(gc_pool_step_locked,gc_page_release_locked,gc_budget_step_locked,gc_tlab_page_locked,gc_try_tlab_allocate,gc_relocate_target_locked,gc_relocate_copy,gc_forward_step,gc_source_access_step);
    gc_run_relocate_pipeline(gc_pool_step_locked,gc_tlab_page_locked,gc_try_tlab_allocate,gc_relocate_target_locked,gc_relocate_copy,gc_relocate_publish,gc_source_access_step,gc_reference_lease_locked,gc_metadata_step_locked,gc_resolve_access,gc_relocation_gate_locked,gc_source_drain,gc_page_release_locked,gc_relocation_job_locked,gc_relocation_complete_locked,gc_relocation_claim_locked,gc_relocate_discard_locked,gc_relocation_cancel_locked,gc_relocation_turn_locked);
    gc_run_relocate_target_cases(gc_relocate_target_locked);
    gc_run_relocate_cases(gc_relocate_copy);
    gc_run_relocate_publish_cases(gc_relocate_copy,gc_relocate_publish);
    gc_run_resolve_cases(gc_resolve_access);
    gc_run_root_follow_cases(gc_root_follow_locked);
    gc_run_root_access_cases(gc_root_access_locked);
    gc_run_survivor_age_cases(gc_survivor_age_locked);
    gc_run_marked_survivor_age_cases(gc_marked_survivor_age_locked);
    gc_run_promotion_remember_cases(gc_promotion_remember_locked);
    gc_run_relocation_turn_cases(gc_relocation_turn_locked);
    gc_run_forward_compress_cases(gc_forward_compress_locked);
    gc_run_root_access_cleanup_cases(gc_root_access_cleanup);
    gc_run_root_access_cleanup_races(gc_root_access_locked,gc_root_access_cleanup,gc_source_access_step,gc_forward_step);
    gc_run_resolve_chains(gc_root_follow_locked,gc_resolve_access,gc_reference_lease_locked,gc_metadata_step_locked,gc_forward_step,gc_source_access_step,gc_root_step,gc_root_drain,gc_relocate_copy,gc_relocate_publish,gc_relocate_target_locked,gc_pool_step_locked,gc_forward_compress_locked,gc_forward_incoming,gc_pending_follow_locked,gc_pending_access_locked,gc_pending_mark);
    gc_run_forward_lookup_cases(gc_forward_lookup);
    gc_run_forward_cases(gc_forward_step);
    gc_run_forward_races(gc_forward_step,gc_source_access_step,gc_resolve_access,gc_relocate_copy,gc_relocate_publish,gc_guarded_scalar);
    gc_run_source_access_cases(gc_source_access_step);
    gc_run_mark_cases(gc_mark_step_locked);
    gc_run_mark_scan_commit_cases(gc_mark_scan_commit_locked);
    gc_run_mark_spill_cases(gc_mark_step_locked,gc_mark_spill_locked);
    gc_run_mark_scan_cases(gc_mark_scan_slice);
    gc_run_source_drain_cases(gc_source_drain);
    gc_run_source_uncommit_cases(gc_source_uncommit_locked);
    gc_run_source_retire_cases(gc_source_retire_locked);
    gc_run_source_retire_pipeline(gc_source_retire_locked,gc_pool_step_locked,gc_budget_step_locked,gc_source_uncommit_locked,gc_weak_follow_step_locked,gc_weak_incoming_step_locked,gc_weak_table_step_locked,gc_native_source_uncommit_entry,gc_native_heap_decommit_range);
    gc_run_forward_incoming_cases(gc_forward_incoming);
    gc_run_relocation_gate_cases(gc_relocation_gate_locked);
    gc_run_relocation_job_cases(gc_relocation_job_locked);
    gc_run_relocation_complete_cases(gc_relocation_complete_locked);
    gc_run_relocation_claim_cases(gc_relocation_claim_locked);
    gc_run_marked_relocation_cases(gc_marked_relocation_claim_locked);
    gc_run_relocate_discard_cases(gc_relocate_discard_locked);
    gc_run_relocation_cancel_cases(gc_relocation_cancel_locked);
    gc_run_alloc_into_cases(gc_try_tlab_allocate_into);
    gc_run_alloc_start_cases(gc_try_allocate_start);
    gc_run_native_allocation_cases(gc_try_allocate_start);
    gc_run_native_allocation_retire_cases(gc_try_allocate_start);
    gc_run_relocation_claim_races(gc_relocation_claim_locked,gc_relocation_gate_locked,gc_forward_step,gc_source_access_step);
    GraphRememberedOps remembered={&move,gc_remembered_inventory_step_locked,gc_remembered_owner,gc_remembered_load_into,gc_remembered_walk_locked,gc_remembered_lease_locked,gc_remembered_transfer_locked,gc_barrier_buffer_step,gc_root_access_locked,gc_promotion_remember_locked,gc_rooted_load,gc_rooted_load_admit_locked,gc_pending_follow_locked,gc_pending_access_locked};
    gc_run_mark_graph(&lifecycle,&remembered,gc_guarded_mark_route,gc_mark_step_locked,gc_mark_spill_locked,gc_mark_scan_slice,gc_metadata_step_locked,gc_budget_step_locked,gc_reference_lease_locked,gc_resolve_access,gc_guarded_store,gc_guarded_heal,gc_pool_step_locked,gc_tlab_page_locked,gc_try_tlab_allocate_into,gc_descriptor_register,gc_root_step,gc_source_drain,gc_source_uncommit_locked,gc_forward_incoming,gc_forward_compress_locked,gc_heap_incoming,gc_heap_walk_locked,gc_object_walk_locked,gc_heap_audit_slice,gc_heap_audit_step);
    gc_run_reference_lease_cases(gc_reference_lease_locked);
    gc_run_budget_cases(gc_budget_step_locked,gc_pool_step_locked);
    gc_run_metadata_cases(gc_metadata_step_locked);
    gc_run_metadata_races(gc_metadata_step_locked,gc_barrier_buffer_step,gc_buffer_job_step,gc_pool_step_locked,gc_tlab_page_locked,gc_handshake_step_locked);
    gc_run_descriptor_cases(gc_descriptor_register, gc_descriptor_scan);
    gc_run_builtin_descriptor_cases(gc_descriptor_register,gc_descriptor_scan,gc_builtin_descriptors_prepare);
    gc_run_descriptor_races(gc_descriptor_register, gc_descriptor_scan);
    gc_run_tlab_cases(gc_try_tlab_allocate);
    gc_run_pool_cases(gc_pool_step_locked);
    gc_run_binding_cases(gc_tlab_page_locked);
    gc_run_page_pipeline(gc_pool_step_locked, gc_tlab_page_locked, gc_try_allocate_start,
        gc_descriptor_register, gc_descriptor_scan, gc_root_step,
        &(PagePublicationOps){gc_reference_lease_locked,gc_root_access_locked,gc_metadata_grow_locked,gc_budget_step_locked,gc_metadata_step_locked,gc_source_access_step,gc_guarded_initialization_publish,gc_guarded_initialization_store,gc_guarded_initialization_copy,gc_guarded_scalar,
            gc_guarded_mark_route,gc_mark_step_locked,gc_mark_scan_slice,
            gc_worker_step_locked,gc_root_life_locked,gc_root_scan_leased,gc_root_journal_step,
            gc_root_clear_chunk,gc_handshake_step_locked,gc_mark_phase_step_locked,gc_root_audit_step_locked,gc_forward_lookup,gc_mark_spill_locked,NULL,gc_builtin_descriptors_prepare,gc_root_storage_prepare,gc_root_storage_can_dispose,NULL,NULL,NULL,gc_native_startup_request_locked,NULL,gc_native_startup_commit_locked,gc_allocation_sample,gc_mark_scan_commit_locked,gc_allocation_pressure,gc_cycle_step_locked,gc_heap_allocation_pressure,gc_root_copy,gc_native_startup_release_locked,gc_root_access_cleanup,gc_cycle_pressure_locked,gc_cycle_mark_start_locked,gc_native_roots_publish_locked});
    gc_run_forward_table_races(gc_try_tlab_allocate,gc_forward_lookup,gc_source_access_step);
    return 0;
}
