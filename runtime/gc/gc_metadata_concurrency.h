/* Conformance owner: real page preparation, lease tokens and buffer handoff.
 * References remain symbolic; this does not implement marking or retirement. */
#ifndef KLASSIC_GC_METADATA_CONCURRENCY_H
#define KLASSIC_GC_METADATA_CONCURRENCY_H
#include "gc_metadata_cases.h"
#include "gc_buffers.h"
#include "gc_buffer_job_cases.h"
#include "gc_handshake.h"
#include "gc_pages_pipeline.h"
#ifdef _WIN32
typedef HANDLE MetadataThread;
static void metadata_yield(void) { SwitchToThread(); }
#else
#include <pthread.h>
#include <sched.h>
typedef pthread_t MetadataThread;
static void metadata_yield(void) { sched_yield(); }
#endif
typedef uint64_t (*MetadataBufferStep)(MutatorContext *,const BarrierEntry *,uint64_t);
typedef uint64_t (*MetadataBufferDrain)(BarrierDrain *);
typedef struct {
    PoolFixture f;
    MetadataLeasePool leases;
    MetadataLeaseSlot slots[2];
    MutatorContext ctx;
    BarrierEntry entry;
    MetadataStep metadata;
    BufferJobStep buffer_job;
    HandshakeState handshake;ThreadRegistration participant;
    uint64_t (*handshake_step)(HandshakeState *,ThreadRegistration *,uint64_t);
    unsigned selector;
    _Atomic uint64_t ready, resume;
#ifdef _WIN32
    CRITICAL_SECTION lock;
#else
    pthread_mutex_t lock;
#endif
} MetadataRace;
static void metadata_lock(MetadataRace *r) {
#ifdef _WIN32
    EnterCriticalSection(&r->lock);
#else
    assert(pthread_mutex_lock(&r->lock)==0);
#endif
}
static void metadata_unlock(MetadataRace *r) {
#ifdef _WIN32
    LeaveCriticalSection(&r->lock);
#else
    assert(pthread_mutex_unlock(&r->lock)==0);
#endif
}
static uint64_t metadata_call(MetadataRace *r,MetadataLeaseRequest *q,uint64_t op) {
    metadata_lock(r);uint64_t result=r->metadata(&r->leases,q,op);metadata_unlock(r);return result;
}
/* A deliberately premature FREE must not bypass the metadata hold. Restore
 * the state before unlocking; this is fault injection, NOT page retirement. */
static void metadata_reuse_attempt(MetadataRace *r,PoolStep pool) {
    metadata_lock(r);
    AllocationPage before;memcpy(&before,&r->f.pages[0],sizeof(before));
    atomic_store(&r->f.pages[0].state,GC_PAGE_FREE);
    PageLease next={.page_count=1};
    assert(pool(&r->f.h,&next,GC_POOL_RESERVE)==GC_POOL_OK);
    assert(next.head_index==1); // head zero is still held by an entry/job
    assert(pool(&r->f.h,&next,GC_POOL_ABORT)==GC_POOL_OK);
    atomic_store(&r->f.pages[0].state,GC_PAGE_CLOSED);
    assert(!memcmp(&before,&r->f.pages[0],sizeof(before)));
    metadata_unlock(r);
}
#ifdef _WIN32
static DWORD WINAPI metadata_reader(void *arg)
#else
static void *metadata_reader(void *arg)
#endif
{
    MetadataRace *r=arg;
    while(atomic_load_explicit(&r->resume,memory_order_acquire)<1)metadata_yield();
    BarrierEntry out;
    BarrierJob job={.identity=r->ctx.thread_id,.selector=r->selector,.output=(uintptr_t)&out,.capacity=1};
    metadata_lock(r);assert(r->buffer_job(&r->ctx,&job,GC_BJ_CLAIM)==GC_BJ_OK);metadata_unlock(r);
    assert(r->buffer_job(&r->ctx,&job,GC_BJ_COPY)==GC_BJ_OK&&job.written==1);
    assert(out.slot.metadata_lease&&out.slot.page_incarnation==r->f.pages[0].incarnation);
    MetadataLeaseRequest q={.token=out.slot.metadata_lease};
    assert(metadata_call(r,&q,GC_ML_RESOLVE)==GC_ML_OK);
    assert(q.page==(uintptr_t)&r->f.pages[0]&&q.incarnation==out.slot.page_incarnation);
    // Context and ring storage remain retained while the copied token is live.
    atomic_store_explicit(&r->ready,1,memory_order_release);
    while(atomic_load_explicit(&r->resume,memory_order_acquire)<2)metadata_yield();
    assert(metadata_call(r,&q,GC_ML_RESOLVE)==GC_ML_OK);
    MetadataLeaseRequest duplicate=q;
    assert(metadata_call(r,&q,GC_ML_RELEASE)==GC_ML_OK);
    assert(metadata_call(r,&duplicate,GC_ML_RELEASE)==GC_ML_INVALID);
    metadata_lock(r);assert(r->buffer_job(&r->ctx,&job,GC_BJ_ACK)==GC_BJ_OK);metadata_unlock(r);
    atomic_store_explicit(&r->ready,2,memory_order_release);
    return 0;
}
static void gc_run_metadata_races(MetadataStep metadata,MetadataBufferStep step,
    BufferJobStep buffer_job,PoolStep pool,TlabPage bind,uint64_t (*handshake_step)(HandshakeState *,ThreadRegistration *,uint64_t)) {
    for(unsigned round=0;round<64;round++) {
        MetadataRace r={.metadata=metadata,.buffer_job=buffer_job,.handshake_step=handshake_step,.selector=round%2};
#ifdef _WIN32
        InitializeCriticalSection(&r.lock);
#else
        assert(pthread_mutex_init(&r.lock,NULL)==0);
#endif
        pool_fixture(&r.f,0);
        size_t bytes=9*GC_HEAP_PAGE_BYTES;
        void *mapping=pool_vm_reserve(bytes);r.f.h.heap_base=(uintptr_t)mapping;
        metadata_lock(&r);assert(pool(&r.f.h,&r.f.l,GC_POOL_RESERVE)==GC_POOL_OK);metadata_unlock(&r);
        pool_vm_commit((char *)mapping+GC_HEAP_PAGE_BYTES,GC_HEAP_PAGE_BYTES);
        void *starts=calloc(1,GC_HEAP_PAGE_BYTES/128);assert(starts);
        r.f.l.starts=(uintptr_t)starts;r.f.l.committed_mapping=1;
        r.ctx=(MutatorContext){.abi_version=GC_ABI_VERSION};r.participant.context=(uintptr_t)&r.ctx;
        metadata_lock(&r);
        assert(handshake_step(&r.handshake,&r.participant,GC_HS_REGISTER)==GC_HS_OK);
        assert(pool(&r.f.h,&r.f.l,GC_POOL_COMMIT)==GC_POOL_OK);
        assert(bind(&r.ctx,&r.f.pages[0],0)==0);assert(bind(&r.ctx,&r.f.pages[0],1)==0);
        metadata_unlock(&r);
        r.leases=(MetadataLeasePool){(uintptr_t)&r.f.h,(uintptr_t)r.slots,2,1,0};
        r.slots[0].next_free=2;
        MetadataLeaseRequest q={(uintptr_t)&r.f.pages[0],r.f.pages[0].incarnation,0};
        assert(metadata_call(&r,&q,GC_ML_ACQUIRE)==GC_ML_OK);
        uint64_t token=q.token;
        BufferCursor *b=r.selector?&r.ctx.remembered:&r.ctx.satb;
        *b=(BufferCursor){.entries=(uintptr_t)&r.entry,.capacity=1};
        BarrierEntry entry={.previous={UINT64_C(0x12341510)},
            .slot={.owner={UINT64_C(0x43211510)},.byte_offset=8,.record_epoch=round+1}};
        entry.slot.metadata_lease=token;entry.slot.page_incarnation=q.incarnation;
        assert(step(&r.ctx,&entry,r.selector)==GC_BB_OK);
        // A failed append transfers nothing: caller rolls its new lease back.
        MetadataLeaseRequest rejected={q.page,q.incarnation,0};
        assert(metadata_call(&r,&rejected,GC_ML_ACQUIRE)==GC_ML_OK);
        entry.slot.metadata_lease=rejected.token;
        assert(step(&r.ctx,&entry,r.selector)==GC_BB_FULL);
        assert(metadata_call(&r,&rejected,GC_ML_RELEASE)==GC_ML_OK);
        assert(r.f.pages[0].metadata_leases==1&&r.leases.active==1);
        metadata_reuse_attempt(&r,pool);
        MetadataThread reader;
#ifdef _WIN32
        reader=CreateThread(NULL,0,metadata_reader,&r,0,NULL);assert(reader);
#else
        assert(pthread_create(&reader,NULL,metadata_reader,&r)==0);
#endif
        assert(step(&r.ctx,NULL,r.selector+2)==GC_BB_OK);
        atomic_store_explicit(&r.resume,1,memory_order_release);
        while(atomic_load_explicit(&r.ready,memory_order_acquire)<1)metadata_yield();
        assert(!atomic_load(&b->consumed)&&atomic_load(&b->published)==1);
        BarrierJob competing={.identity=r.ctx.thread_id,.selector=r.selector,.capacity=1};
        metadata_lock(&r);assert(buffer_job(&r.ctx,&competing,GC_BJ_CLAIM)==GC_BJ_WAIT);
        assert(gc_context_transition_locked(&r.ctx,GC_RUNNING,GC_DETACHING)==GC_CONTEXT_OK);
        assert(handshake_step(&r.handshake,&r.participant,GC_HS_DETACH)==GC_HS_WAIT);
        // Fault injection: even an erroneously consumed prefix must not let
        // detach free an active job control. Restore before the consumer runs.
        atomic_store(&b->consumed,1);
        assert(handshake_step(&r.handshake,&r.participant,GC_HS_DETACH)==GC_HS_WAIT);
        atomic_store(&b->consumed,0);metadata_unlock(&r);
        metadata_lock(&r);assert(r.leases.active==1&&r.f.pages[0].metadata_leases==1);metadata_unlock(&r);
        metadata_reuse_attempt(&r,pool);
        atomic_store_explicit(&r.resume,2,memory_order_release);
        while(atomic_load_explicit(&r.ready,memory_order_acquire)<2)metadata_yield();
        metadata_lock(&r);assert(handshake_step(&r.handshake,&r.participant,GC_HS_DETACH)==GC_HS_OK);metadata_unlock(&r);
        assert(atomic_load(&b->consumed)==1&&!r.handshake.members&&!r.ctx.satb_consumer.serial&&!r.ctx.remembered_consumer.serial);
        q.token=0;assert(metadata_call(&r,&q,GC_ML_ACQUIRE)==GC_ML_OK);
        assert((uint32_t)q.token==(uint32_t)token&&q.token!=token);
        MetadataLeaseRequest stale={.token=token};
        assert(metadata_call(&r,&stale,GC_ML_RESOLVE)==GC_ML_INVALID);
        assert(metadata_call(&r,&q,GC_ML_RELEASE)==GC_ML_OK);
#ifdef _WIN32
        assert(WaitForSingleObject(reader,30000)==WAIT_OBJECT_0);assert(CloseHandle(reader));
        DeleteCriticalSection(&r.lock);
#else
        assert(pthread_join(reader,NULL)==0);assert(pthread_mutex_destroy(&r.lock)==0);
#endif
        assert(!r.leases.active&&!r.f.pages[0].metadata_leases);
        free(starts);pool_vm_release(mapping,bytes); // teardown after all owners joined
    }
    puts("metadata leases: 64 buffer-full rollback, delayed-job retention and stale-token rounds PASS");
    puts("buffer jobs: 64 registered SATB/remset pending tokens, detach waits and ACK rounds PASS");
}
#endif
