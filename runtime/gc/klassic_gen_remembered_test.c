/* OS-backed metadata ownership test, separate from collector acceptance. */
#include "klassic_gen_remembered.h"
#include <assert.h>
_Noreturn void kg_fail(const char *operation,uint64_t status){
    fprintf(stderr,"remembered backing test: %s (%llu)\n",operation,(unsigned long long)status);abort();
}
int main(void){
    KgRuntime r={0};r.os=gc_native_heap_ops();assert(!pthread_mutex_init(&r.lock,NULL));
    NativeRuntimeConfig config={.heap={8*GC_HEAP_PAGE_BYTES,4*GC_HEAP_PAGE_BYTES,GC_HEAP_PAGE_BYTES,128*r.os.metadata.quantum},
        .root_cells=8,.root_changes=8,.descriptor_capacity=64,.work={8,8,8}};
    assert(!gc_native_runtime_prepare(&r.native,&config,&r.os));assert(!gc_native_runtime_activate(&r.native));
    assert(gc_worker_step_locked(&r.native.roots,&r.native.worker,GC_WK_ENTER_NATIVE)==GC_WK_OK);
    uint64_t baseline=r.native.backing.heap.metadata_committed;
    for(unsigned i=0;i<16;i++){
        KgRun run={0};assert(!gc_native_allocation_reserve_generation_locked(&run.native,&r.native,1,64,GC_OLD,&r.os.metadata));
        assert(!gc_native_allocation_prepare(&run.native,&r.os.metadata));assert(!gc_native_allocation_publish_locked(&run.native));
        uint64_t before=r.native.backing.heap.metadata_committed;
        assert(kg_remembered_attach(&r,&run,&r.native.context)==GC_BUDGET_OK);
        assert(run.remembered&&run.remembered_charge.stage==2&&r.native.remembered.members==1);
        assert(r.native.backing.heap.metadata_committed==before+run.remembered_charge.bytes);
        assert(kg_remembered_detach(&r,&run,&r.native.context)==GC_RS_OK);
        assert(!run.remembered&&!run.remembered_charge.stage&&!r.native.remembered.members);
        assert(r.native.backing.heap.metadata_committed==before);
        assert(!gc_native_allocation_cancel_locked(&run.native));assert(!gc_native_allocation_cleanup(&run.native,&r.os.metadata));
        assert(!gc_native_allocation_finish_locked(&run.native));assert(r.native.backing.heap.metadata_committed==baseline);
    }
    assert(gc_worker_step_locked(&r.native.roots,&r.native.worker,GC_WK_LEAVE_NATIVE)==GC_WK_OK);
    RootStorage *storage=(void *)(uintptr_t)r.native.root_charge.allocation;
    RootLease lease={0};RootLifeRequest q={.registration=(uintptr_t)&storage->registration,.scope=storage->segment.scope,
        .context=(uintptr_t)&r.native.context,.lease=(uintptr_t)&lease};
    assert(!gc_root_life_locked(&r.native.roots,&q,GC_RL_RETIRE));assert(!gc_root_life_locked(&r.native.roots,&q,GC_RL_CLEAN));
    assert(!gc_root_clear_chunk(&q));assert(!gc_root_life_locked(&r.native.roots,&q,GC_RL_FINISH));
    assert(!gc_native_runtime_dispose(&r.native,&r.os));assert(!pthread_mutex_destroy(&r.lock));
    puts("OS-backed Old remembered bitmap attach/detach, budget and private target cancellation PASS");return 0;
}
