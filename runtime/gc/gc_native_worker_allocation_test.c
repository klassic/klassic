#define _GNU_SOURCE
#include "gc_native_allocation_retire.h"
#include "gc_native_records.h"
#include "gc_native_source_uncommit.h"
#include "gc_test_threads.h"
#include <assert.h>
#include <stdio.h>

typedef struct {
    NativeRuntimeOwner runtime;
    NativeHeapOps os;
    GcTestMutex mutex;
    _Atomic unsigned done;
} WorkerAllocationTest;
static void lock_test(WorkerAllocationTest *t){assert(!gc_test_mutex_lock(&t->mutex));}
static void unlock_test(WorkerAllocationTest *t){assert(!gc_test_mutex_unlock(&t->mutex));}
static void *prepare_target_on_worker(void *arg){
    WorkerAllocationTest *t=arg;NativeAllocationRun target={0};MutatorContext actor={.abi_version=GC_ABI_VERSION};
    lock_test(t);assert(t->runtime.handshake.last_identity<UINT64_MAX);
    actor.thread_id=++t->runtime.handshake.last_identity;
    assert(gc_native_allocation_reserve_target_locked(&target,&t->runtime,1,4,GC_OLD,&t->os.metadata)==GC_POOL_OK);
    assert(target.lease.relocation==1&&target.lease.generation==GC_OLD);
    unlock_test(t);
    atomic_init(&actor.state,GC_RUNNING);atomic_init(&actor.access_epoch,0);
    assert(gc_native_allocation_prepare_native(&target,&t->os.metadata,&actor)==GC_POOL_INVALID);
    atomic_store_explicit(&actor.state,GC_BLOCKED_NATIVE,memory_order_release);
    assert(gc_native_allocation_prepare_native(&target,&t->os.metadata,&actor)==GC_POOL_OK);
    lock_test(t);
    assert(gc_native_allocation_publish_locked(&target)==GC_POOL_OK);
    assert(gc_native_allocation_page(&target)->generation==GC_OLD);
    assert(gc_native_allocation_cancel_locked(&target)==GC_POOL_OK);
    unlock_test(t);
    /* The real registered mutator remains Running with its actual TLAB. A
     * collector OS operation must use this worker's owned native context. */
    assert(gc_native_allocation_cleanup(&target,&t->os.metadata)==GC_POOL_INVALID);
    assert(gc_native_allocation_cleanup_native(&target,&t->os.metadata,&actor)==GC_POOL_OK);
    lock_test(t);assert(gc_native_allocation_finish_locked(&target)==GC_POOL_OK);unlock_test(t);
    atomic_store_explicit(&t->done,1,memory_order_release);return NULL;
}
int main(void){
    WorkerAllocationTest t={.os=gc_native_heap_ops()};NativeAllocationRun source={0};
    assert(!gc_test_mutex_init(&t.mutex,NULL));
    NativeRuntimeConfig config={{4*GC_HEAP_PAGE_BYTES,2*GC_HEAP_PAGE_BYTES,GC_HEAP_PAGE_BYTES,256*t.os.metadata.quantum},8,8,64,{8,8,8}};
    assert(gc_native_runtime_prepare(&t.runtime,&config,&t.os)==GC_NATIVE_METADATA_OK);
    assert(gc_native_runtime_activate(&t.runtime)==GC_NATIVE_METADATA_OK);
    uint64_t baseline=t.runtime.backing.heap.metadata_committed;
    assert(gc_native_allocation_reserve_locked(&source,&t.runtime,1,4,&t.os.metadata)==GC_POOL_OK);
    assert(gc_worker_step_locked(&t.runtime.roots,&t.runtime.worker,GC_WK_ENTER_NATIVE)==GC_WK_OK);
    assert(gc_native_allocation_prepare(&source,&t.os.metadata)==GC_POOL_OK);
    assert(gc_native_allocation_publish_locked(&source)==GC_POOL_OK);
    assert(gc_worker_step_locked(&t.runtime.roots,&t.runtime.worker,GC_WK_LEAVE_NATIVE)==GC_WK_OK);
    assert(gc_native_allocation_bind_locked(&source)==GC_POOL_OK);
    uint64_t tlab=t.runtime.context.tlab_top;
    GcTestThread worker;assert(!gc_test_thread_create(&worker,NULL,prepare_target_on_worker,&t));
    while(!atomic_load_explicit(&t.done,memory_order_acquire))gc_test_yield();
    assert(!gc_test_thread_join(worker,NULL));
    assert(t.runtime.context.tlab_top==tlab&&source.stage==4&&t.runtime.allocations==(uintptr_t)&source);
    assert(atomic_load(&t.runtime.context.state)==GC_RUNNING&&t.runtime.handshake.members==1);
    assert(gc_native_allocation_close_locked(&source)==GC_POOL_OK);
    /* Empty run: no managed value was ever allocated or published anywhere.
     * That actual invariant supplies its whole-range death proof. */
    assert(!source.marks.used);
    assert(gc_native_allocation_dead_seal_locked(&source)==GC_SR_DONE);
    SourceUncommit payload={.head_index=source.lease.head_index,.page_count=source.lease.page_count,.incarnation=source.lease.incarnation};
    assert(gc_source_uncommit_locked(&t.runtime.backing.heap,&payload,GC_SU_BEGIN)==GC_SU_OK);
    assert(gc_worker_step_locked(&t.runtime.roots,&t.runtime.worker,GC_WK_ENTER_NATIVE)==GC_WK_OK);
    assert(gc_native_source_uncommit_payload(&t.runtime.backing.heap,&payload,NULL)==GC_NATIVE_METADATA_OK);
    assert(gc_source_uncommit_locked(&t.runtime.backing.heap,&payload,GC_SU_FINISH)==GC_SU_OK);
    NativeRunRetireCheck check={0};assert(gc_native_allocation_retire_check_begin_locked(&source,&check)==GC_NSI_MORE);
    uint64_t status;do{status=gc_native_allocation_retire_check_next_locked(&check);}while(status==GC_NSI_MORE);
    assert(status==GC_NSI_DONE);
    assert(gc_native_allocation_retire_begin_locked(&source,&check)==GC_SR_MORE);
    assert(gc_native_allocation_retire_import_locked(&source)==GC_SR_DONE);
    assert(gc_native_allocation_retire_cleanup_native(&source,&t.os.metadata,&t.runtime.context)==GC_POOL_OK);
    do{status=gc_native_allocation_retire_step_locked(&source);}while(status==GC_SR_MORE);
    assert(status==GC_SR_DONE);
    assert(gc_native_records_begin_locked(&source)==GC_SR_DONE);
    assert(gc_native_records_dispose_native(&source,&t.os.metadata,&t.runtime.context)==GC_POOL_OK);
    assert(gc_native_records_finish_locked(&source)==GC_POOL_OK);
    assert(!t.runtime.allocations&&!t.runtime.backing.heap.committed&&t.runtime.backing.heap.metadata_committed==baseline);
    assert(gc_worker_step_locked(&t.runtime.roots,&t.runtime.worker,GC_WK_LEAVE_NATIVE)==GC_WK_OK);
    RootStorage *storage=(void *)(uintptr_t)t.runtime.root_charge.allocation;RootLease lease={0};
    RootLifeRequest q={.registration=(uintptr_t)&storage->registration,.scope=storage->segment.scope,
        .context=(uintptr_t)&t.runtime.context,.lease=(uintptr_t)&lease};
    assert(gc_root_life_locked(&t.runtime.roots,&q,GC_RL_RETIRE)==GC_RL_OK);
    assert(gc_root_life_locked(&t.runtime.roots,&q,GC_RL_CLEAN)==GC_RL_OK);
    assert(gc_root_clear_chunk(&q)==GC_RL_OK);
    assert(gc_root_life_locked(&t.runtime.roots,&q,GC_RL_FINISH)==GC_RL_OK);
    assert(gc_worker_step_locked(&t.runtime.roots,&t.runtime.worker,GC_WK_ENTER_NATIVE)==GC_WK_OK);
    assert(gc_native_runtime_dispose(&t.runtime,&t.os)==GC_NATIVE_METADATA_OK);
    assert(!gc_test_mutex_destroy(&t.mutex));
    puts("native allocation OS worker: Old relocation reservation/preparation/cleanup preserves live mutator TLAB; full resource disposal PASS");
    return 0;
}
