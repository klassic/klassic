#ifndef KLASSIC_GEN_RUNTIME_INTERNAL_H
#define KLASSIC_GEN_RUNTIME_INTERNAL_H
#ifndef _DEFAULT_SOURCE
#define _DEFAULT_SOURCE 1
#endif
#include "klassic_gen_runtime.h"
#include "gc_native_allocation.h"
#include "gc_native_selected_source.h"
#include "gc_native_root_mark.h"
#include "gc_native_mark_scan.h"
#include "gc_rooted_load_dispatch.h"
#include "gc_rooted_store_dispatch.h"
#include "gc_rooted_scalar.h"
#include "gc_alloc_into.h"
#include "gc_initialization.h"
#include "klassic_gen_pin.h"
#include "gc_root_pin.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef _WIN32
#include <windows.h>
typedef SRWLOCK KgMutex;
typedef CONDITION_VARIABLE KgCondition;
typedef DWORD KgOwnerId;
#else
#include <pthread.h>
#include <sched.h>
typedef pthread_mutex_t KgMutex;
typedef pthread_cond_t KgCondition;
typedef pthread_t KgOwnerId;
#endif
_Noreturn void kg_fail(const char *operation,uint64_t status);
typedef struct KgRun {
    NativeAllocationRun native; /* first; NativeRuntimeOwner.allocations links these */
    MetadataCharge control;
    MetadataCharge remembered_charge;
    void *remembered;
    uint64_t selected_serial,selected_generation,selected_epoch,selected_cycle,closed_phase;
    NativeSelectedSource selection;
    uint64_t pin_retired;
} KgRun;
typedef struct KgRootBlock {
    struct KgRootBlock *next;
    MetadataCharge control,storage;
    MetadataCharge *charge;
    RootStorage *roots;
    uint64_t live,initial;
} KgRootBlock;
typedef struct KgMutator {
    struct KgRuntime *runtime;
    struct KgMutator *previous,*next;
    MetadataCharge control,rings;
    MutatorContext local_context;
    ThreadRegistration local_participant;
    WorkerRequest local_worker;
    MutatorContext *context;
    ThreadRegistration *participant;
    WorkerRequest *worker;
    KgOwnerId owner;
    KgRun *current;
    KgRootBlock local_initial_roots,*initial_roots,*root_blocks;
    RootCell *store_observed;
    uint64_t state,pins,initializing,bootstrap,root_revision_seen,publications;
} KgMutator;
typedef struct KgRuntime {
    NativeRuntimeOwner native;
    NativeHeapOps os;
    NativeRunCommitter payload_commit;
    NativePageDecommitter payload_decommit;
    NativePageDecommitter idle_decommit;
    MetadataCharge policy_charge;
    void *policy;
    void *lease_backing;
    void *pin_inventory;
    PinPageRegistry *pin_registry;
    KgMutex lock;
    KgCondition changed;
    KgOwnerId owner;
    KgRun *current;
    KgRootBlock initial_roots,*root_blocks,*allocation_roots;
    uint64_t hard_max,min_heap,soft_max,flags,initialized,closing;
    uint64_t uncommit_delay_ns;
    uint64_t root_capacity,journal_capacity,initializing;
    void *driver;
    KgMutator primary,*mutators;
    uint64_t mutator_count,mutator_generation,attaching,detaching;
    uint64_t root_revision,root_publications,admission_closed;
    void (*root_sync)(struct KgRuntime *);
} KgRuntime;
enum { KG_MUTATOR_PRIVATE=0,KG_MUTATOR_REGISTERED=1,KG_MUTATOR_DETACHING=2,KG_MUTATOR_DETACHED=3 };
KgMutator *kg_current_mutator(KgRuntime *);
MutatorContext *kg_current_context(KgRuntime *);
bool kg_current_initializing(KgRuntime *);
MutatorContext *kg_context_acquire_after_locked(KgRuntime *,uint64_t after_identity);
void kg_context_release_locked(KgRuntime *,MutatorContext *);
uint64_t kg_root_revision_locked(KgRuntime *);
uint64_t kg_root_publications_locked(KgRuntime *);
void kg_root_revision_sync_locked(KgRuntime *);
void kg_root_publication_begin(KgRuntime *);
void kg_root_publication_end(KgRuntime *);
const MutatorContext *kg_driver_cleanup_actor(KgRuntime *);

typedef struct {
    void *(*reserve)(size_t);
    bool (*commit)(void *,size_t);
    bool (*decommit)(void *,size_t);
    bool (*release)(void *,size_t);
} KgLeaseOps;
enum { KG_LEASE_OK=0,KG_LEASE_WAIT=1,KG_LEASE_LIMIT=2,KG_LEASE_INVALID=3 };
void kg_lease_initialize(KgRuntime *,const MutatorContext *,uint64_t,const KgLeaseOps *);
uint64_t kg_lease_grow_native(KgRuntime *,const MutatorContext *);
uint64_t kg_lease_grow_native_min(KgRuntime *,const MutatorContext *,uint64_t);
bool kg_lease_growing_locked(KgRuntime *);
void kg_lease_pressure(KgRuntime *);
void kg_lease_pressure_min(KgRuntime *,uint64_t);
void kg_lease_dispose_native(KgRuntime *,const MutatorContext *);
enum { KG_PIN_SELECTED=0,KG_PIN_SKIP=1,KG_PIN_FULL=2,KG_PIN_INVALID=3 };
uint64_t kg_pin_select_locked(KgRuntime *,KgRun *);
bool kg_pin_page_active_locked(KgRuntime *,KgRun *);
uint64_t kg_pin_source_seal_locked(KgRuntime *,KgRun *,uint64_t);
bool kg_pin_source_selected_locked(KgRuntime *,KgRun *);
bool kg_pin_source_retire_locked(KgRuntime *,KgRun *);
uint64_t kg_pin_remembered_detach(KgRuntime *,KgRun *,const MutatorContext *);
void kg_pin_owner_can_detach_locked(KgRuntime *,const MutatorContext *);
void kg_pin_dispose_native(KgRuntime *,const MutatorContext *);
bool kg_managed_callback_active(void);
void kg_threads_shutdown_native(KgRuntime *);
void kg_threads_shutdown_request_locked(KgRuntime *);
enum { KG_PRESSURE_HEAP=1,KG_PRESSURE_ROOTS=2,KG_PRESSURE_METADATA=3,KG_PRESSURE_STRESS=4 };
/* Driver entries are invoked without the shared mutex and without a raw
 * guard. step drives bounded shared leaves and retains no raw guard on return.
 * Transport/assistance can span multiple leaves. reference must complete canonical mark handoff for
 * the captured publication phase before its caller acknowledges the journal.
 * pressure requests work; the caller keeps its roots and assists/waits until
 * admission succeeds. stop drains/joins every worker before final disposal. */
void kg_driver_start(KgRuntime *runtime);
void kg_driver_verify(KgRuntime *runtime);
void kg_driver_step(KgRuntime *runtime);
void kg_driver_pressure(KgRuntime *runtime,uint64_t reason);
void kg_driver_reference(KgRuntime *runtime,uint64_t reference,uint64_t phase);
void kg_driver_stop(KgRuntime *runtime);
static inline void kg_lock(KgRuntime *r){
#ifdef _WIN32
    AcquireSRWLockExclusive(&r->lock);
#else
    if(pthread_mutex_lock(&r->lock))kg_fail("mutex lock",1);
#endif
}
static inline void kg_unlock(KgRuntime *r){
    if(r->root_sync)r->root_sync(r);
#ifdef _WIN32
    ReleaseSRWLockExclusive(&r->lock);
#else
    if(pthread_mutex_unlock(&r->lock))kg_fail("mutex unlock",1);
#endif
}
static inline void kg_notify(KgRuntime *r){
#ifdef _WIN32
    WakeAllConditionVariable(&r->changed);
#else
    if(pthread_cond_broadcast(&r->changed))kg_fail("condition notify",1);
#endif
}
static inline void kg_yield(void){
#ifdef _WIN32
    SwitchToThread();
#else
    sched_yield();
#endif
}
/* Shared bounded OS retry policy, also used by standalone native adapters. */
static inline void kg_os_retry(uint64_t *failures,const char *operation){
    if(!failures||*failures>=31)kg_fail(operation,GC_NATIVE_METADATA_OS_ERROR);
    ++*failures;kg_yield();
}
static inline MetadataLeasePool *kg_leases(KgRuntime *r){return &gc_native_runtime_work(&r->native)->leases;}
static inline DescriptorRegistry *kg_descriptors(KgRuntime *r){
    return &((NativeBuiltinRegistry *)(uintptr_t)r->native.descriptor_charge.allocation)->registry;
}
/* Facade services usable by the collector. Caller holds no mutex/guard.
 * Root journals remain leased through driver_reference. Native boundaries close
 * the real current TLAB; the next allocation obtains a new actual run. */
void kg_drain_journals(KgRuntime *runtime);
void kg_enter_native(KgRuntime *runtime);
void kg_leave_native(KgRuntime *runtime);
void kg_close_tlab_locked(KgRuntime *runtime);
RootRequest kg_root_request_locked(KgRuntime *runtime,const RootCell *root);
KgRun *kg_reserve_run(KgRuntime *runtime,uint64_t pages,uint64_t generation);
void kg_release_run_control(KgRuntime *runtime,KgRun *run);
KgRun *kg_try_reserve_run_native(KgRuntime *runtime,uint64_t pages,uint64_t generation,
    const MutatorContext *actor,uint64_t *status);
void kg_release_run_control_native(KgRuntime *runtime,KgRun *run,const MutatorContext *actor);
/* Stable retirement storage must be outside the mapping it disposes. */
typedef struct {MetadataCharge charge;uint64_t failures;} KgMetadataRetire;
void kg_metadata_prepare_retry(KgRuntime *,MetadataCharge *);
uint64_t kg_metadata_retire_step_native(KgRuntime *,MetadataCharge *,KgMetadataRetire *,const MutatorContext *);
void kg_prepare_run_native_retry(KgRuntime *,KgRun *,const MutatorContext *);
void kg_cancel_run_native_retry(KgRuntime *,KgRun *,const MutatorContext *);
uint64_t kg_driver_cleanup_enter(KgRuntime *,KgRun *);
void kg_driver_cleanup_leave(KgRuntime *,uint64_t);

#endif
