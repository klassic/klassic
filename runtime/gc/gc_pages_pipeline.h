/* Conformance-only OS owner. Production OS adapters must obey the same lease
 * transfer rules; these callbacks are not linked into direct generated code. */
#ifndef KLASSIC_GC_PAGES_PIPELINE_H
#define KLASSIC_GC_PAGES_PIPELINE_H
#include "gc_metadata_grow.h"
#include "gc_budget.h"
#include "gc_native_metadata.h"
#include "gc_heap_cases.h"
#include "gc_pages_cases.h"
#include "gc_descriptor_cases.h"
#include "gc_builtin_descriptors.h"
#include "gc_native_builtin_registry.h"
#include "gc_native_roots.h"
#include "gc_native_startup_cases.h"
#include "gc_native_startup_commit_cases.h"
#include "gc_allocation_sample_cases.h"
#include "gc_allocation_pressure_cases.h"
#include "gc_cycle.h"
#include "gc_cycle_pressure.h"
#include "gc_cycle_mark_start.h"
#include "gc_roots.h"
#include "gc_alloc_into.h"
#include "gc_alloc_start.h"
#include "gc_initialization.h"
#include "gc_reference_lease.h"
#include "gc_source_access.h"
#include "gc_mark_route.h"
#include "gc_mark_scan.h"
#include "gc_mark_scan_commit.h"
#include "gc_root_copy.h"
#include "gc_worker.h"
#include "gc_root_lifetime.h"
#include "gc_root_journal.h"
#include "gc_root_access.h"
#include "gc_mark_phase.h"
#include "gc_forward_table.h"
#include "gc_mark_spill.h"
#ifdef _WIN32
#include <windows.h>
#else
#include <sys/mman.h>
#include <pthread.h>
#include <sched.h>
#include <time.h>
#endif
static void *pool_vm_reserve(size_t bytes) {
#ifdef _WIN32
    void *p = VirtualAlloc(NULL, bytes, MEM_RESERVE, PAGE_NOACCESS);
    assert(p);
#else
    void *p = mmap(NULL, bytes, PROT_NONE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    assert(p != MAP_FAILED);
#endif
    return p;
}
static void pool_vm_commit(void *address, size_t bytes) {
#ifdef _WIN32
    assert(VirtualAlloc(address, bytes, MEM_COMMIT, PAGE_READWRITE) == address);
#else
    assert(mprotect(address, bytes, PROT_READ | PROT_WRITE) == 0);
#endif
}
static void pool_vm_decommit(void *address, size_t bytes) {
#ifdef _WIN32
    assert(VirtualFree(address, bytes, MEM_DECOMMIT));
#else
    assert(mmap(address, bytes, PROT_NONE, MAP_PRIVATE | MAP_ANONYMOUS | MAP_FIXED, -1, 0) == address);
#endif
}
static void pool_vm_release(void *address, size_t bytes) {
#ifdef _WIN32
    (void)bytes;
    assert(VirtualFree(address, 0, MEM_RELEASE));
#else
    assert(munmap(address, bytes) == 0);
#endif
}

static FILE *binding_vectors;
static void binding_compare(HeapFixture *a, HeapFixture *c, TlabPage candidate, uint64_t op, uint64_t expected) {
    HeapFixture x, y;
    memcpy(&x, a, sizeof(x)); memcpy(&y, c, sizeof(y));
    assert(candidate(&a->ctx, &a->page, op) == expected);
    assert(gc_tlab_page_locked(&c->ctx, &c->page, op) == expected);
    if (binding_vectors) {
        HeapFixture before, after;
        memcpy(&before, &y, sizeof(before)); memcpy(&after, c, sizeof(after));
        uint64_t record[] = {op, expected, heap_vector_normalize(&before, c), heap_vector_normalize(&after, c)};
        assert(fwrite(record, sizeof(record), 1, binding_vectors) == 1);
        assert(fwrite(&before, sizeof(before), 1, binding_vectors) == 1);
        assert(fwrite(&after, sizeof(after), 1, binding_vectors) == 1);
    }
    if (expected) {
        assert(memcmp(a, &x, sizeof(x)) == 0);
        assert(memcmp(c, &y, sizeof(y)) == 0);
        // Deliberately out-of-range fixture pointers cannot be relocated;
        // each implementation has already proved complete preservation.
        return;
    }
    memcpy(&x, a, sizeof(x)); memcpy(&y, c, sizeof(y));
    assert(heap_vector_normalize(&x, a) == heap_vector_normalize(&y, c));
    assert(memcmp(&x, &y, sizeof(x)) == 0);
    if (!expected) {
        assert(atomic_load(&a->page.state) == (op ? GC_PAGE_CLOSED : GC_PAGE_ALLOCATING));
        assert(a->page.owner == (op ? 0 : a->ctx.thread_id));
        assert(op ? !a->ctx.tlab_top : a->ctx.tlab_top == a->page.heap_base + a->page.start);
    }
}
static void binding_fixture(HeapFixture *f, uint64_t op) {
    heap_fixture_init(f);
    if (!op) {
        f->ctx.tlab_start = f->ctx.tlab_top = f->ctx.tlab_end = 0;
        f->page.owner = 0;
        atomic_store(&f->page.state, GC_PAGE_READY);
    }
}
static void gc_run_binding_cases(TlabPage candidate) {
    HeapFixture *a = malloc(sizeof(*a)), *c = malloc(sizeof(*c));
    assert(a && c);
    unsigned cases = 0;
    const char *path = getenv("KLASSIC_GC_BINDING_VECTORS");
    if (path) {
        binding_vectors = fopen(path, "wb");
        assert(binding_vectors && fwrite("KLBIND01", 8, 1, binding_vectors) == 1);
        uint64_t header[] = {sizeof(HeapFixture), offsetof(HeapFixture, ctx), offsetof(HeapFixture, page),
            sizeof(heap_pointer_fields)/sizeof(*heap_pointer_fields)};
        assert(fwrite(header, sizeof(header), 1, binding_vectors) == 1);
        assert(fwrite(heap_pointer_fields, sizeof(heap_pointer_fields), 1, binding_vectors) == 1);
    }
    const unsigned faults[] = {0,1,2,3,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31};
    for (uint64_t op = 0; op < 2; op++) {
        for (unsigned f = 0; f < sizeof(faults)/sizeof(*faults); f++) {
            binding_fixture(a, op); binding_fixture(c, op);
            heap_fault(a, faults[f]); heap_fault(c, faults[f]);
            binding_compare(a, c, candidate, op, GC_ALLOC_INVALID); cases++;
        }
        binding_fixture(a, op); binding_fixture(c, op);
        a->page.starts++; c->page.starts++;
        binding_compare(a, c, candidate, op, GC_ALLOC_INVALID); cases++;
        binding_fixture(a, op); binding_fixture(c, op);
        a->page.identity_base = c->page.identity_base = UINT64_MAX;
        binding_compare(a, c, candidate, op, GC_ALLOC_INVALID); cases++;
        binding_fixture(a, op); binding_fixture(c, op);
        binding_compare(a, c, candidate, 2, GC_ALLOC_INVALID); cases++;
        binding_compare(a, c, candidate, op, GC_ALLOC_OK); cases++;
        binding_compare(a, c, candidate, op, GC_ALLOC_INVALID); cases++;
    }
    binding_fixture(a, 0); binding_fixture(c, 0);
    binding_compare(a, c, candidate, 0, GC_ALLOC_OK); cases++;
    assert(gc_try_tlab_allocate(&a->ctx, &a->request) == GC_ALLOC_OK);
    assert(gc_try_tlab_allocate(&c->ctx, &c->request) == GC_ALLOC_OK);
    binding_compare(a, c, candidate, 1, GC_ALLOC_OK); cases++;
    for(uint64_t op=0;op<2;op++) {
        binding_fixture(a,op);binding_fixture(c,op);
        atomic_store(&a->ctx.state,GC_DETACHING);atomic_store(&c->ctx.state,GC_DETACHING);
        binding_compare(a,c,candidate,op,op?GC_ALLOC_OK:GC_ALLOC_INVALID);cases++;
    }
    free(a); free(c);
    printf("%u C/candidate TLAB bind/close cases PASS\n", cases);
    if (binding_vectors) { assert(fclose(binding_vectors) == 0); binding_vectors = NULL; }
}

typedef struct {
    uint64_t (*acquire)(MetadataLeasePool *,ReferenceLease *);
    uint64_t (*root_access)(MetadataLeasePool *,RootAccess *);
    uint64_t (*grow)(MetadataLeasePool *,uint64_t);
    uint64_t (*budget)(HeapPool *,MetadataCharge *,uint64_t);
    uint64_t (*metadata)(MetadataLeasePool *,MetadataLeaseRequest *,uint64_t);
    uint64_t (*access)(ForwardCell *,SourceAccessRequest *,uint64_t);
    uint64_t (*publish)(const GuardedMarkPrepare *);
    uint64_t (*initialize)(const GuardedMarkPrepare *,const InitializationField *);
    uint64_t (*initialize_bytes)(const GuardedMarkPrepare *,const InitializationBytes *);
    uint64_t (*scalar)(GuardedScalar *,uint64_t);
    uint64_t (*route)(GuardedMarkRoute *);
    uint64_t (*mark)(MarkQueue *,MarkRequest *,uint64_t);
    uint64_t (*scan)(MarkScan *);
    uint64_t (*worker)(RootRegistry *,WorkerRequest *,uint64_t);
    uint64_t (*life)(RootRegistry *,RootLifeRequest *,uint64_t);
    uint64_t (*leased)(RootScanJob *);
    uint64_t (*journal)(RootRegistry *,RootJournalRequest *,uint64_t);
    uint64_t (*clear)(RootLifeRequest *);
    uint64_t (*handshake)(HandshakeState *,ThreadRegistration *,uint64_t);
    uint64_t (*phase)(RootRegistry *,MarkPhaseControl *,uint64_t);
    uint64_t (*audit)(RootRegistry *,RootAudit *,uint64_t);
    uint64_t (*lookup)(const ForwardTable *,ForwardLookup *);
    uint64_t (*spill)(MarkQueue *,MarkRequest *,uint64_t);
    const NativeMetadataOps *native_metadata;
    void (*builtin_prepare)(BuiltinDescriptors *);
    RootStoragePreparer root_storage_prepare;
    RootStorageDisposalCheck root_storage_can_dispose;
    NativeRootPreparer native_root_prepare;
    NativeRootDisposer native_root_dispose;
    NativeBuiltinRegistryPreparer native_builtin_registry_prepare;
    NativeStartupReserver native_startup;
    NativeStartupPreparer native_startup_prepare;
    NativeStartupCommitter native_startup_commit;
    AllocationSampler allocation_sample;
    MarkScanCommitter scan_commit;
    AllocationPressure allocation_pressure;
    uint64_t (*cycle)(CycleLedger *,CycleTicket *,uint64_t);
    HeapAllocationPressure heap_pressure;
    RootCopier root_copy;
    NativeStartupReleaser startup_release;
    RootAccessCleanup access_cleanup;
    CyclePressureStep cycle_pressure;
    CycleMarkStarter cycle_mark_start;
    NativeRootPublisher native_root_publish;
} PagePublicationOps;
/* Single-slot ownership relay: release/acquire command/completion handoffs
 * serialize registry, queue and lease operations. No actor accesses those
 * controls while the other owns the command; heap scanning is outside them. */
typedef struct {
    const PagePublicationOps *ops;RootRegistry *roots;HandshakeState *handshake;
    MetadataLeasePool *leases;DescriptorRegistry *registry;
    AllocationPage *page;ForwardCell *cell;MarkQueue *queue;
    _Atomic uint64_t ready,command,claimed,scan_allowed,completed,stop;
    _Atomic uint64_t rendezvous_request,rendezvous_done,rendezvous_phase;
    _Atomic uint64_t metadata_wait,metadata_resume;
    uint64_t owner_progress,duplicate_children,spilled_children,scanned_jobs;RootCell *child_root;
    uint64_t scan_slices[2],examined_slots[2],loaded_slots[2],scan_yields[2];
    MutatorContext *allocation_owner;uint64_t observed_allocated;AllocationSample allocation_window;
#ifdef _WIN32
    HANDLE thread;
#else
    pthread_t thread;
#endif
} PageMarkWorker;
static uint64_t page_clock_ns(void){
#ifdef _WIN32
    LARGE_INTEGER ticks,frequency;assert(QueryPerformanceFrequency(&frequency)&&frequency.QuadPart>0);
    assert(QueryPerformanceCounter(&ticks)&&ticks.QuadPart>=0);
    uint64_t seconds=(uint64_t)ticks.QuadPart/(uint64_t)frequency.QuadPart;
    uint64_t remainder=(uint64_t)ticks.QuadPart%(uint64_t)frequency.QuadPart;
    assert(seconds<=UINT64_MAX/UINT64_C(1000000000)&&remainder<=UINT64_MAX/UINT64_C(1000000000));
    uint64_t fraction=remainder*UINT64_C(1000000000)/(uint64_t)frequency.QuadPart;
    assert(seconds*UINT64_C(1000000000)<=UINT64_MAX-fraction);
    return seconds*UINT64_C(1000000000)+fraction;
#else
    struct timespec now;assert(clock_gettime(CLOCK_MONOTONIC,&now)==0&&now.tv_sec>=0&&now.tv_nsec>=0&&now.tv_nsec<1000000000);
    assert((uint64_t)now.tv_sec<=(UINT64_MAX-(uint64_t)now.tv_nsec)/UINT64_C(1000000000));
    return (uint64_t)now.tv_sec*UINT64_C(1000000000)+(uint64_t)now.tv_nsec;
#endif
}
static void page_mark_yield(void){
#ifdef _WIN32
    SwitchToThread();
#else
    sched_yield();
#endif
}
#ifdef _WIN32
static DWORD WINAPI page_mark_worker(void *argument)
#else
static void *page_mark_worker(void *argument)
#endif
{
    PageMarkWorker *w=argument;const PagePublicationOps *ops=w->ops;
    MutatorContext ctx={.abi_version=GC_ABI_VERSION};ThreadRegistration participant={.context=(uintptr_t)&ctx};
    WorkerRequest worker={(uintptr_t)w->handshake,(uintptr_t)&participant};
    assert(ops->worker(w->roots,&worker,GC_WK_ATTACH)==GC_WK_OK);
    assert(ops->allocation_sample(w->allocation_owner,&w->allocation_window,page_clock_ns())==GC_SAMPLE_BASELINE);
    assert(!w->allocation_window.total_bytes);
    atomic_store_explicit(&w->ready,1,memory_order_release);
    uint64_t completed=0,rendezvous=0;
    while(!atomic_load_explicit(&w->stop,memory_order_acquire)){
        uint64_t allocated=atomic_load_explicit(&w->allocation_owner->allocated_bytes,memory_order_acquire);
        assert(allocated>=w->observed_allocated);w->observed_allocated=allocated;
        uint64_t request=atomic_load_explicit(&w->rendezvous_request,memory_order_acquire);
        if(request!=rendezvous){
            assert(request==rendezvous+1&&atomic_load(&w->command)==completed);
            uint64_t expected=w->handshake->active?GC_WK_PARK:GC_WK_OK;
            assert(ops->worker(w->roots,&worker,GC_WK_POLL)==expected);
            assert(atomic_load(&ctx.adopted_phase)==w->roots->phase&&!atomic_load(&ctx.access_epoch));
            atomic_store_explicit(&w->rendezvous_phase,atomic_load(&ctx.adopted_phase),memory_order_relaxed);
            rendezvous=request;atomic_store_explicit(&w->rendezvous_done,request,memory_order_release);
            continue;
        }
        uint64_t command=atomic_load_explicit(&w->command,memory_order_acquire);
        if(command==completed){page_mark_yield();continue;}
        assert(command==completed+1);
        MarkRequest job={0};assert(ops->mark(w->queue,&job,GC_MARK_TAKE)==GC_MARK_OK);
        atomic_store_explicit(&w->claimed,command,memory_order_release);
        while(atomic_load_explicit(&w->scan_allowed,memory_order_acquire)<command)page_mark_yield();
        uint64_t child_index=0,generation=w->queue->generation;
        for(;;){
            SourceAccessRequest scope={(uintptr_t)&ctx,job.reference>>16,job.incarnation,0,0};
            assert(ops->access(w->cell,&scope,GC_SA_ENTER)==GC_SA_OK);
            uint64_t children[4]={UINT64_MAX,UINT64_MAX,UINT64_MAX,UINT64_MAX};
            MarkScan scan={.job=(uintptr_t)&job,.heap_base=w->page->heap_base,.registry=(uintptr_t)w->registry,
                .output=(uintptr_t)children,.capacity=2};
            uint64_t scan_status=ops->scan(&scan);
            assert(scan_status==(w->child_root&&!job.cursor?GC_SCAN_MORE:GC_SCAN_DONE));
            assert(scan.written==(w->child_root?1u:0u));
            assert(scan.examined==(w->child_root?2u:0u));
            assert(ops->access(w->cell,&scope,GC_SA_EXIT)==GC_SA_OK);
            // The unchanged parent claim and owner's child root protect each
            // result until an offer transfers its lease or duplicate cleanup releases it.
            for(uint64_t slot=0;slot<scan.written;slot++,child_index++){
                uint64_t i=child_index;
                assert(children[slot]&&children[slot]==atomic_load_explicit(&w->child_root->value,memory_order_acquire));
                ReferenceLease child={.reference=children[slot]},unchanged=child;
                uint64_t acquired=ops->acquire(w->leases,&child);
                if(acquired==GC_ML_FULL){
                    assert(w->queue->generation==GC_OLD&&i==1&&!memcmp(&child,&unchanged,sizeof(child)));
                    assert(!atomic_load(&ctx.access_epoch)&&w->queue->in_flight==1&&w->queue->overflow_count==1&&job.cursor==i*16);
                    assert(w->leases->active==w->leases->capacity);
                    assert(ops->worker(w->roots,&worker,GC_WK_ENTER_NATIVE)==GC_WK_OK);
                    uint64_t wait=atomic_fetch_add_explicit(&w->metadata_wait,1,memory_order_release)+1;
                    while(atomic_load_explicit(&w->metadata_resume,memory_order_acquire)<wait)page_mark_yield();
                    assert(ops->worker(w->roots,&worker,GC_WK_LEAVE_NATIVE)==GC_WK_OK);
                    assert(atomic_load(&ctx.adopted_phase)==w->roots->phase&&!atomic_load(&ctx.access_epoch));
                    acquired=ops->acquire(w->leases,&child);
                }
                assert(acquired==GC_ML_OK);
                AllocationPage *page=(AllocationPage *)(uintptr_t)child.lease.page;
                ForwardLookup lookup={children[slot]>>16,child.lease.incarnation,0};
                assert(ops->lookup((const ForwardTable *)(uintptr_t)page->forward_table,&lookup)==GC_FL_OK);
                ForwardCell *cell=(ForwardCell *)(uintptr_t)lookup.cell;
                SourceAccessRequest access={(uintptr_t)&ctx,lookup.source_offset,lookup.incarnation,0,0};
                assert(ops->access(cell,&access,GC_SA_ENTER)==GC_SA_OK);
                GuardedMarkRoute routed={.generation=w->queue->generation,.prepare={.scope=(uintptr_t)&access,
                    .page=(uintptr_t)page,.reference=children[slot],.phase=atomic_load(&ctx.adopted_phase),.lease=child.lease}};
                assert(ops->route(&routed)==GC_ROUTE_READY&&!routed.prepare.lease.token);
                child.lease=(MetadataLeaseRequest){0};
                assert(ops->access(cell,&access,GC_SA_EXIT)==GC_SA_OK);
                MarkRequest before=routed.prepare.output;
                uint64_t status=ops->mark(w->queue,&routed.prepare.output,GC_MARK_OFFER);
                assert(!memcmp(&before,&routed.prepare.output,sizeof(before))&&w->queue->in_flight==1&&job.cursor==i*16);
                if(w->queue->generation==GC_OLD&&!i){
                    assert(status==GC_MARK_FULL&&!w->queue->overflow_count);
                    assert(ops->spill(w->queue,&routed.prepare.output,GC_MARK_OFFER_OR_SPILL)==GC_MARK_OK);
                    assert(!routed.prepare.output.metadata_lease&&w->queue->overflow_count==1);
                    assert(ops->mark(w->queue,NULL,GC_MARK_CHECK)==GC_MARK_BUSY);w->spilled_children++;
                }else{
                    assert(status==GC_MARK_DUPLICATE);
                    MetadataLeaseRequest released_child={.token=routed.prepare.output.metadata_lease};
                    assert(ops->metadata(w->leases,&released_child,GC_ML_RESOLVE)==GC_ML_OK);
                    assert(ops->metadata(w->leases,&released_child,GC_ML_RELEASE)==GC_ML_OK);
                    w->duplicate_children++;
                }
                assert(w->leases->active==1+w->queue->overflow_count+(w->queue->generation==GC_OLD&&i==0?1u:0u));
            }
            // Account only a slice whose returned children have all transferred
            // protection. A metadata retry above must not count or rescan it twice.
            w->scan_slices[generation]++;
            w->examined_slots[generation]+=scan.examined;
            w->loaded_slots[generation]+=scan.written;
            MarkRequest stale=job;
            uint64_t next_cursor=scan.next_cursor;
            assert(ops->scan_commit(w->queue,&scan,scan_status)==GC_MARK_OK&&!scan.job);
            if(scan_status==GC_SCAN_DONE)break;
            assert(!atomic_load(&ctx.access_epoch));
            assert(!job.index&&!job.metadata_lease&&w->queue->queued==1&&!w->queue->in_flight);
            assert(ops->worker(w->roots,&worker,GC_WK_POLL)==GC_WK_OK);
            assert(ops->mark(w->queue,&job,GC_MARK_TAKE)==GC_MARK_OK);
            assert(job.cursor==next_cursor&&job.reference==stale.reference&&job.serial!=stale.serial);
            assert(ops->mark(w->queue,&stale,GC_MARK_FINISH)==GC_MARK_INVALID);
            w->scan_yields[generation]++;
        }
        w->scanned_jobs++;
        assert(!job.index&&!job.serial);
        MetadataLeaseRequest released={.token=job.metadata_lease};
        assert(ops->metadata(w->leases,&released,GC_ML_RESOLVE)==GC_ML_OK);
        assert(ops->metadata(w->leases,&released,GC_ML_RELEASE)==GC_ML_OK);
        // FINISH frees the single task slot, but overflow still owns the
        // child's strong work and metadata lease. Complete it before replying.
        if(w->queue->overflow_count)assert(ops->mark(w->queue,NULL,GC_MARK_CHECK)==GC_MARK_BUSY);
        uint64_t refill;
        while((refill=ops->spill(w->queue,NULL,GC_MARK_REFILL))==GC_MARK_OK){
            MarkRequest child_job={0};assert(ops->mark(w->queue,&child_job,GC_MARK_TAKE)==GC_MARK_OK);
            MetadataLeaseRequest child_lease={.token=child_job.metadata_lease};
            assert(ops->metadata(w->leases,&child_lease,GC_ML_RESOLVE)==GC_ML_OK);
            AllocationPage *page=(AllocationPage *)(uintptr_t)child_lease.page;
            ForwardLookup lookup={child_job.reference>>16,child_job.incarnation,0};
            assert(ops->lookup((const ForwardTable *)(uintptr_t)page->forward_table,&lookup)==GC_FL_OK);
            ForwardCell *cell=(ForwardCell *)(uintptr_t)lookup.cell;
            SourceAccessRequest child_scope={(uintptr_t)&ctx,lookup.source_offset,lookup.incarnation,0,0};
            assert(ops->access(cell,&child_scope,GC_SA_ENTER)==GC_SA_OK);
            uint64_t children[4]={0};
            MarkScan child_scan={.job=(uintptr_t)&child_job,.heap_base=page->heap_base,.registry=(uintptr_t)w->registry,
                .output=(uintptr_t)children,.capacity=4};
            assert(ops->scan(&child_scan)==GC_SCAN_DONE&&!child_scan.written&&!child_scan.examined);w->scanned_jobs++;
            w->scan_slices[generation]++;
            assert(ops->access(cell,&child_scope,GC_SA_EXIT)==GC_SA_OK);
            assert(ops->scan_commit(w->queue,&child_scan,GC_SCAN_DONE)==GC_MARK_OK&&!child_scan.job);
            assert(ops->metadata(w->leases,&child_lease,GC_ML_RELEASE)==GC_ML_OK);
        }
        assert(refill==GC_MARK_EMPTY&&!w->leases->active);
        completed=command;atomic_store_explicit(&w->completed,completed,memory_order_release);
    }
    assert(ops->allocation_sample(w->allocation_owner,&w->allocation_window,page_clock_ns())==GC_SAMPLE_OK);
    uint64_t final_allocated=atomic_load_explicit(&w->allocation_owner->allocated_bytes,memory_order_acquire);
    assert(final_allocated>=w->observed_allocated);w->observed_allocated=final_allocated;
    assert(completed==6&&rendezvous==4&&!atomic_load(&ctx.access_epoch));
    assert(gc_context_transition_locked(&ctx,GC_RUNNING,GC_DETACHING)==GC_CONTEXT_OK);
    assert(ops->handshake(w->handshake,&participant,GC_HS_DETACH)==GC_HS_OK);
    return 0;
}
static void page_pipeline_publish(const PagePublicationOps *ops,MetadataLeasePool *leases,
        MutatorContext *ctx,AllocationPage *page,ForwardCell *cell,const RootRequest *root,
        DescriptorRegistry *registry,MarkQueue *queues,uint64_t generation,PageMarkWorker *worker,
        const RootRequest *child_root,TlabPage bind,WorkerRequest *owner){
    assert(worker->registry==registry);
    worker->child_root=child_root?(RootCell *)(uintptr_t)child_root->handle.cell:NULL;
    RootCell *held=(RootCell *)(uintptr_t)root->handle.cell;
    uint64_t reference=atomic_load_explicit(&held->value,memory_order_acquire);
    assert(reference&&reference==root->value);
    if(leases->capacity==2){
        ReferenceLease growth={.reference=reference};
        assert(ops->acquire(leases,&growth)==GC_ML_OK);
        assert(ops->grow(leases,3)==GC_ML_OK);
        assert(ops->metadata(leases,&growth.lease,GC_ML_RELEASE)==GC_ML_OK);
    }
    RootAccess rooted={.follow={.context=(uintptr_t)ctx,.root=*root}};
    rooted.follow.root.value=0;
    assert(ops->root_access(leases,&rooted)==GC_RAC_READY);
    ReferenceLease acquired={.reference=rooted.access.reference,.lease=rooted.follow.cleanup};
    SourceAccessRequest scope=rooted.access.scope;
    rooted=(RootAccess){0}; // move guard/lease ownership to scope/acquired
    assert(acquired.reference==reference&&scope.held==(uintptr_t)cell);
    assert(acquired.lease.page==(uintptr_t)page&&page->metadata_leases==1);
    GuardedMarkPrepare q={.scope=(uintptr_t)&scope,.page=(uintptr_t)page,
        .reference=reference,.phase=atomic_load(&ctx->adopted_phase),.lease=acquired.lease};
    GuardedMarkPrepare before=q;
    GuardedMarkRoute routed={.generation=generation,.prepare=q},unready=routed;
    assert(ops->route(&routed)==GC_ROUTE_WAIT&&!memcmp(&routed,&unready,sizeof(routed)));
    assert(ops->access(cell,&scope,GC_SA_EXIT)==GC_SA_OK);
    assert(!atomic_load(&ctx->access_epoch)&&atomic_load(&held->value)==reference);
    assert(ops->access(cell,&scope,GC_SA_ENTER)==GC_SA_OK);
    if(!child_root){
        unsigned char source[256];for(unsigned i=0;i<256;i++)source[i]=(unsigned char)(i*37+11);
        uint64_t payload_bytes=((ObjectHeader *)(uintptr_t)(page->heap_base+(reference>>16)-sizeof(ObjectHeader)))->payload_bytes;
        uint64_t offset=0;
        do{
            uint64_t bytes=payload_bytes-offset;if(bytes>256)bytes=256;
            InitializationBytes part={(uintptr_t)registry,offset,(uintptr_t)source,bytes};
            assert(ops->initialize_bytes(&q,&part)==GC_INIT_OK);
            assert(!memcmp((void *)(uintptr_t)(page->heap_base+(reference>>16)+offset),source,bytes));
            offset+=bytes;
            assert(ops->access(cell,&scope,GC_SA_EXIT)==GC_SA_OK);
            assert(ops->metadata(leases,&acquired.lease,GC_ML_RELEASE)==GC_ML_OK);
            assert(!leases->active&&!page->metadata_leases&&!atomic_load(&ctx->access_epoch));
            scope=(SourceAccessRequest){0};acquired=(ReferenceLease){0};q=(GuardedMarkPrepare){0};
            page_mark_yield(); // no cached scope/metadata ownership crosses this boundary
            rooted=(RootAccess){.follow={.context=(uintptr_t)ctx,.root=*root}};
            rooted.follow.root.value=0;
            if(offset==bytes){
                // Exhaust native lease capacity between chunks, while the
                // registered root remains the sole owner of the receiver.
                ReferenceLease pressure[3]={{0}};
                for(unsigned i=0;i<3;i++){
                    pressure[i].reference=atomic_load(&held->value);
                    assert(ops->acquire(leases,&pressure[i])==GC_ML_OK);
                }
                RootAccess unchanged=rooted;
                assert(ops->root_access(leases,&rooted)==GC_RF_METADATA_FULL);
                assert(!memcmp(&rooted,&unchanged,sizeof(rooted))&&!atomic_load(&ctx->access_epoch));
                assert(atomic_load(&held->value)==reference);
                for(unsigned i=0;i<3;i++)assert(ops->metadata(leases,&pressure[i].lease,GC_ML_RELEASE)==GC_ML_OK);
            }
            assert(ops->root_access(leases,&rooted)==GC_RAC_READY);
            acquired=(ReferenceLease){.reference=rooted.access.reference,.lease=rooted.follow.cleanup};
            scope=rooted.access.scope;rooted=(RootAccess){0};
            assert(acquired.reference==reference&&scope.held==(uintptr_t)cell);
            assert(acquired.lease.page==(uintptr_t)page&&page->metadata_leases==1);
            q=(GuardedMarkPrepare){.scope=(uintptr_t)&scope,.page=acquired.lease.page,
                .reference=acquired.reference,.phase=atomic_load(&ctx->adopted_phase),.lease=acquired.lease};
        }while(offset<payload_bytes);
    }
    if(child_root){
        uint64_t child=atomic_load_explicit(&worker->child_root->value,memory_order_acquire);
        assert(child&&child==child_root->value);
        ObjectHeader *header=(ObjectHeader *)(uintptr_t)(page->heap_base+(reference>>16)-sizeof(ObjectHeader));
        assert(header->descriptor_id==43&&header->payload_bytes==32);
        for(unsigned slot=0;slot<4;slot++){
            InitializationField field={(uintptr_t)registry,slot*8,slot?child:UINT64_MAX,slot&1};
            assert(ops->initialize(&q,&field)==GC_INIT_OK);
            // A repeated reference write must use the normal deletion barrier.
            if(slot&1)assert(ops->initialize(&q,&field)==GC_INIT_INVALID);
        }
    }
    before=q;routed=(GuardedMarkRoute){.generation=generation,.prepare=q};
    assert(ops->publish(&q)==GC_INIT_OK&&!memcmp(&q,&before,sizeof(q)));
    assert(ops->publish(&q)==GC_INIT_INVALID&&!memcmp(&q,&before,sizeof(q)));
    if(child_root){
        GuardedScalar scalar={(uintptr_t)&scope,(uintptr_t)page,(uintptr_t)registry,0,q.phase,0};
        assert(ops->scalar(&scalar,GC_SCALAR_LOAD)==GC_SCALAR_OK&&scalar.value==UINT64_MAX);
        scalar.value=17;assert(ops->scalar(&scalar,GC_SCALAR_STORE)==GC_SCALAR_OK);
        scalar.value=0;assert(ops->scalar(&scalar,GC_SCALAR_LOAD)==GC_SCALAR_OK&&scalar.value==17);
        scalar.byte_offset=16;
        assert(ops->scalar(&scalar,GC_SCALAR_LOAD)==GC_SCALAR_OK&&scalar.value==child_root->value);
        scalar.byte_offset=8;GuardedScalar unchanged=scalar;
        assert(ops->scalar(&scalar,GC_SCALAR_LOAD)==GC_SCALAR_INVALID&&!memcmp(&scalar,&unchanged,sizeof(scalar)));
    }
    for(unsigned pass=0;pass<2;pass++){
        if(pass){
            generation^=1;acquired=(ReferenceLease){.reference=reference};
            assert(ops->acquire(leases,&acquired)==GC_ML_OK);
            assert(ops->access(cell,&scope,GC_SA_ENTER)==GC_SA_OK);
            q.lease=acquired.lease;q.phase=atomic_load(&ctx->adopted_phase);routed=(GuardedMarkRoute){.generation=generation,.prepare=q};
        }
    if(!child_root&&generation==GC_OLD){
        // Keep this ready scalar white in Old: its registered root and
        // unconsumed publication journal remain counted until parent discovery.
        ObjectHeader *header=(ObjectHeader *)(uintptr_t)(page->heap_base+(reference>>16)-sizeof(ObjectHeader));
        assert(!((MarkRecord *)(uintptr_t)header->mark_record)->old_epoch);
        assert(ops->access(cell,&scope,GC_SA_EXIT)==GC_SA_OK);
        assert(ops->metadata(leases,&acquired.lease,GC_ML_RELEASE)==GC_ML_OK);
        assert(!leases->active&&atomic_load(&held->value)==reference);
        continue;
    }
    assert(ops->route(&routed)==GC_ROUTE_READY&&!routed.prepare.lease.token);
    acquired.lease=(MetadataLeaseRequest){0}; // ownership is now the pending offer
    assert(ops->access(cell,&scope,GC_SA_EXIT)==GC_SA_OK);
    MarkQueue *queue=&queues[generation];
    MetadataLeaseRequest pressure={0};
    if(child_root&&generation==GC_OLD){
        ReferenceLease held_pressure={.reference=reference};
        assert(ops->acquire(leases,&held_pressure)==GC_ML_OK);pressure=held_pressure.lease;
    }
    assert(ops->mark(queue,&routed.prepare.output,GC_MARK_OFFER)==GC_MARK_OK);
    uint64_t command=atomic_load_explicit(&worker->completed,memory_order_acquire)+1;
    worker->page=page;worker->cell=cell;worker->queue=queue;
    atomic_store_explicit(&worker->command,command,memory_order_release);
    while(atomic_load_explicit(&worker->claimed,memory_order_acquire)<command)page_mark_yield();
    assert(ops->mark(queue,NULL,GC_MARK_CHECK)==GC_MARK_BUSY&&!atomic_load(&ctx->access_epoch));
    for(unsigned i=0;i<1024;i++)worker->owner_progress++;
    atomic_store_explicit(&worker->scan_allowed,command,memory_order_release);
    while(atomic_load_explicit(&worker->completed,memory_order_acquire)<command){
        uint64_t wait=atomic_load_explicit(&worker->metadata_wait,memory_order_acquire);
        if(wait>atomic_load_explicit(&worker->metadata_resume,memory_order_relaxed)){
            // The worker handed metadata/registry ownership back while native.
            // Its parent and overflow child remain counted; no guard is held.
            assert(pressure.token&&worker->handshake->native==1&&leases->active==3);
            assert(queue->in_flight==1&&queue->overflow_count==1&&ops->mark(queue,NULL,GC_MARK_CHECK)==GC_MARK_BUSY);
            assert(atomic_load(&held->value)==reference&&atomic_load(&worker->child_root->value)==child_root->value);
            // A real rendezvous while the scanner retains a partial job and
            // its second slice's protected result. No heap traversal occurs
            // while stopped. Release/acquire handoff owns these native controls.
            MarkQueue before_queue=*queue;
            uint64_t before_leases=leases->active;
            assert(ops->worker(worker->roots,owner,GC_WK_POLL)==GC_WK_CLOSE_TLAB);
            assert(bind(ctx,page,1)==GC_ALLOC_OK);
            assert(!ctx->tlab_top&&!page->owner&&!atomic_load(&ctx->access_epoch));
            assert(worker->roots->phase<=UINT64_MAX-UINT64_C(0x10000));
            worker->roots->phase+=UINT64_C(0x10000);
            assert(ops->handshake(worker->handshake,NULL,GC_HS_BEGIN)==GC_HS_OK);
            assert(worker->handshake->expected==1&&worker->handshake->native==1);
            assert(ops->worker(worker->roots,owner,GC_WK_POLL)==GC_WK_PARK);
            assert(ops->handshake(worker->handshake,NULL,GC_HS_END)==GC_HS_OK);
            assert(ops->worker(worker->roots,owner,GC_WK_POLL)==GC_WK_OK);
            assert(!memcmp(queue,&before_queue,sizeof(before_queue))&&leases->active==before_leases);
            assert(ops->metadata(leases,&pressure,GC_ML_RELEASE)==GC_ML_OK);
            atomic_store_explicit(&worker->metadata_resume,wait,memory_order_release);
        }
        worker->owner_progress++;page_mark_yield();
    }
    assert(!pressure.token);
    assert(ops->mark(queue,NULL,GC_MARK_CHECK)==GC_MARK_EMPTY);
    assert(!leases->active&&!page->metadata_leases&&!atomic_load(&ctx->access_epoch));
    assert(atomic_load(&held->value)==reference);
    }
}
static uint64_t page_dispose_roots(MetadataCharge *charge,const NativeMetadataOps *ops,const PagePublicationOps *publication){
    return publication->native_root_dispose?publication->native_root_dispose(charge):
        gc_native_roots_dispose(charge,ops,publication->root_storage_can_dispose);
}
static uint64_t page_prepare_roots(MetadataCharge *charge,uint64_t owner,const NativeMetadataOps *ops,const PagePublicationOps *publication,uint64_t cells,uint64_t entries){
    RootStorageSpec spec={owner,cells,entries,charge->bytes};
    return publication->native_root_prepare?publication->native_root_prepare(charge,&spec):
        gc_native_roots_prepare(charge,owner,cells,entries,ops,publication->root_storage_prepare);
}
static void *page_root_map_failure(size_t bytes){(void)bytes;return NULL;}
static NativeMetadataOps page_startup_native;
static unsigned page_startup_maps,page_startup_fail_map;
static void *page_startup_map(size_t bytes){
    return ++page_startup_maps==page_startup_fail_map?NULL:page_startup_native.map(bytes);
}
static bool page_startup_unmap_failure(void *address,size_t bytes){(void)address;(void)bytes;return false;}
static void page_startup_failure(const PagePublicationOps *ops,const NativeMetadataOps *native,
        HeapPool *heap,MutatorContext *ctx,RootRegistry *registry,WorkerRequest *worker,DescriptorRegister reg){
    HeapPool before=*heap;
    MetadataCharge roots={0},descriptors={0};
    NativeStartupRequest startup={(uintptr_t)&roots,(uintptr_t)&descriptors,3,2,64,native->quantum};
    assert(!ctx->root_segments&&!registry->active&&!atomic_load(&ctx->access_epoch));
    assert(ops->native_startup(heap,&startup)==GC_BUDGET_OK);
    assert(ops->worker(registry,worker,GC_WK_ENTER_NATIVE)==GC_WK_OK);
    NativeMetadataOps failure=*native;failure.map=page_startup_map;
    page_startup_native=*native;page_startup_maps=0;page_startup_fail_map=ops->native_root_prepare?1:2;
    NativeStartupPreparationOps prepare={&failure,ops->native_root_prepare,NULL,
        ops->root_storage_prepare,ops->builtin_prepare,reg};
    NativeStartupPreparationOps first_failure=prepare;first_failure.roots=NULL;
    page_startup_fail_map=1;
    MetadataCharge r=roots,d=descriptors;HeapPool initial=*heap;
    assert(gc_native_startup_prepare(&startup,ctx->thread_id,&first_failure)==GC_NATIVE_METADATA_OS_ERROR);
    assert(!memcmp(&r,&roots,sizeof(r))&&!memcmp(&d,&descriptors,sizeof(d))&&!memcmp(&initial,heap,sizeof(*heap)));
    page_startup_maps=0;page_startup_fail_map=ops->native_root_prepare?1:2;
    assert(gc_native_startup_prepare(&startup,ctx->thread_id,&prepare)==GC_NATIVE_METADATA_OS_ERROR);
    assert(!descriptors.allocation&&descriptors.stage==1&&roots.allocation&&roots.stage==1);
    RootStorage *storage=(void *)(uintptr_t)roots.allocation;
    RootStorage saved;memcpy(&saved,storage,sizeof(saved));
    MetadataCharge held=roots;HeapPool charged=*heap;
    failure=*native;failure.unmap=page_startup_unmap_failure;
    assert(gc_native_roots_dispose(&roots,&failure,ops->root_storage_can_dispose)==GC_NATIVE_METADATA_OS_ERROR);
    assert(!memcmp(&held,&roots,sizeof(roots))&&!memcmp(&charged,heap,sizeof(*heap)));
    assert(!memcmp(&saved,storage,sizeof(saved))&&!ctx->root_segments&&!registry->active);
    assert(ops->worker(registry,worker,GC_WK_LEAVE_NATIVE)==GC_WK_OK);
    assert(ops->startup_release(heap,&startup)==GC_BUDGET_INVALID);
    assert(!memcmp(&held,&roots,sizeof(roots))&&!memcmp(&charged,heap,sizeof(*heap)));
    assert(descriptors.stage==1&&heap->metadata_pending==before.metadata_pending+roots.bytes+descriptors.bytes);
    assert(ops->worker(registry,worker,GC_WK_ENTER_NATIVE)==GC_WK_OK);
    assert(page_dispose_roots(&roots,native,ops)==GC_NATIVE_METADATA_OK);
    assert(ops->worker(registry,worker,GC_WK_LEAVE_NATIVE)==GC_WK_OK);
    assert(ops->startup_release(heap,&startup)==GC_BUDGET_OK&&roots.stage==3&&descriptors.stage==3);
    assert(!memcmp(&before,heap,sizeof(*heap))&&!ctx->root_segments&&!registry->active);
    puts("native startup rollback: second mapping and root disposal failures retain private ownership until retry PASS");
}
/* A later startup stage can fail after admission was committed but before
 * publishing any roots/descriptor consumers. Both backing lifetimes are private;
 * either disposal order is legal, but accounting cannot be returned early. */
static void page_startup_committed_cleanup(const PagePublicationOps *ops,const NativeMetadataOps *native,
        HeapPool *heap,MutatorContext *ctx,RootRegistry *registry,WorkerRequest *worker,DescriptorRegister reg){
    for(unsigned order=0;order<2;order++){
        HeapPool before=*heap;MetadataCharge roots={0},descriptors={0};
        NativeStartupRequest startup={(uintptr_t)&roots,(uintptr_t)&descriptors,3,2,64,native->quantum};
        assert(!ctx->root_segments&&!registry->active&&!atomic_load(&ctx->access_epoch));
        assert(ops->native_startup(heap,&startup)==GC_BUDGET_OK);
        NativeStartupPreparationOps prepare={native,ops->native_root_prepare,ops->native_builtin_registry_prepare,
            ops->root_storage_prepare,ops->builtin_prepare,reg};
        assert(ops->worker(registry,worker,GC_WK_ENTER_NATIVE)==GC_WK_OK);
        assert((ops->native_startup_prepare?ops->native_startup_prepare(&startup,ctx->thread_id):
            gc_native_startup_prepare(&startup,ctx->thread_id,&prepare))==GC_NATIVE_METADATA_OK);
        assert(ops->worker(registry,worker,GC_WK_LEAVE_NATIVE)==GC_WK_OK);
        assert(ops->native_startup_commit(heap,&startup)==GC_BUDGET_OK);
        assert(roots.stage==2&&descriptors.stage==2&&roots.allocation&&descriptors.allocation);
        HeapPool committed=*heap;
        assert(committed.metadata_pending==before.metadata_pending&&
            committed.metadata_committed==before.metadata_committed+roots.bytes+descriptors.bytes);
        assert(gc_root_storage_can_dispose((void *)(uintptr_t)roots.allocation)==GC_ROOT_STORAGE_OK);
        for(unsigned step=0;step<2;step++){
            bool is_root=(order==step);MetadataCharge *charge=is_root?&roots:&descriptors;
            MetadataCharge r=roots,d=descriptors;
            NativeMetadataOps failure=*native;failure.unmap=page_startup_unmap_failure;
            assert(ops->worker(registry,worker,GC_WK_ENTER_NATIVE)==GC_WK_OK);
            uint64_t result=is_root?gc_native_roots_dispose(charge,&failure,ops->root_storage_can_dispose):
                gc_native_metadata_dispose(charge,&failure);
            assert(result==GC_NATIVE_METADATA_OS_ERROR);
            assert(!memcmp(&r,&roots,sizeof(r))&&!memcmp(&d,&descriptors,sizeof(d)));
            assert(ops->worker(registry,worker,GC_WK_LEAVE_NATIVE)==GC_WK_OK);
            assert(ops->startup_release(heap,&startup)==GC_BUDGET_INVALID);
            assert(!memcmp(heap,&committed,sizeof(*heap))&&!memcmp(&r,&roots,sizeof(r))&&!memcmp(&d,&descriptors,sizeof(d)));
            assert(ops->worker(registry,worker,GC_WK_ENTER_NATIVE)==GC_WK_OK);
            result=is_root?page_dispose_roots(charge,native,ops):gc_native_metadata_dispose(charge,native);
            assert(result==GC_NATIVE_METADATA_OK&&!charge->allocation);
            assert(ops->worker(registry,worker,GC_WK_LEAVE_NATIVE)==GC_WK_OK);
            assert(roots.stage==2&&descriptors.stage==2&&!ctx->root_segments&&!registry->active);
            if(!step){
                r=roots;d=descriptors;
                assert(ops->startup_release(heap,&startup)==GC_BUDGET_INVALID);
                assert(!memcmp(heap,&committed,sizeof(*heap))&&!memcmp(&r,&roots,sizeof(r))&&!memcmp(&d,&descriptors,sizeof(d)));
            }
        }
        assert(ops->startup_release(heap,&startup)==GC_BUDGET_OK);
        assert(roots.stage==3&&descriptors.stage==3&&!roots.allocation&&!descriptors.allocation);
        assert(!memcmp(heap,&before,sizeof(*heap))&&!atomic_load(&ctx->access_epoch));
        assert(ops->startup_release(heap,&startup)==GC_BUDGET_INVALID);
        assert(!memcmp(heap,&before,sizeof(*heap)));
    }
    puts("native startup committed cleanup: both disposal orders retain budgets through failed unmaps and release exactly once PASS");
}
static void gc_run_page_pipeline(PoolStep pool, TlabPage bind,
        uint64_t (*allocate_start)(MutatorContext *,const AllocationStart *),
        DescriptorRegister register_descriptor, DescriptorScanner scan_descriptor,
        uint64_t (*root_step)(MutatorContext *, RootRequest *, uint64_t),
        const PagePublicationOps *publication) {
    AllocationPage pages[8] = {0};
    MarkRecord records[4] = {0};
    ForwardTable forwards[4] = {0};
    ForwardCell cells[4][2] = {0};
    MarkArena marks = {(uintptr_t)records,4,0};
    size_t reservation_bytes = 9 * GC_HEAP_PAGE_BYTES;
    void *mapping = pool_vm_reserve(reservation_bytes);
    HeapPool h = {.heap_base = (uintptr_t)mapping, .pages = (uintptr_t)pages, .page_count = 8,
        .hard_max = 8 * GC_HEAP_PAGE_BYTES, .soft_max = 2 * GC_HEAP_PAGE_BYTES,
        .relocation_reserve = 2 * GC_HEAP_PAGE_BYTES, .metadata_max = sizeof(pages) + 8 * GC_HEAP_PAGE_BYTES / 128 + 2 * GC_HEAP_PAGE_BYTES,
        .metadata_committed = sizeof(pages)};
    // Charge the entire committed mapping, including unused capacity/padding.
    NativeMetadataOps native_metadata=publication->native_metadata?*publication->native_metadata:gc_native_metadata_ops();
    MetadataCharge lease_charge={0};
    assert(gc_native_metadata_size(GC_HEAP_PAGE_BYTES,native_metadata.quantum,&lease_charge.bytes)==GC_NATIVE_METADATA_OK);
    uint64_t metadata_before=h.metadata_committed;
    // Failed admission must not create a reservation or change the charge.
    MetadataCharge oversized={.bytes=h.metadata_max},oversized_before=oversized;
    HeapPool admission_before=h;
    assert(publication->budget(&h,&oversized,GC_BUDGET_RESERVE)==GC_BUDGET_LIMIT);
    assert(!memcmp(&oversized,&oversized_before,sizeof(oversized))&&!memcmp(&h,&admission_before,sizeof(h)));
    // Model an OS failure before obtaining any mapping, then retry normally.
    MetadataCharge failed={.bytes=GC_HEAP_PAGE_BYTES};
    assert(publication->budget(&h,&failed,GC_BUDGET_RESERVE)==GC_BUDGET_OK);
    assert(h.metadata_pending==GC_HEAP_PAGE_BYTES);
    assert(publication->budget(&h,&failed,GC_BUDGET_ABORT)==GC_BUDGET_OK);
    assert(!h.metadata_pending&&h.metadata_committed==metadata_before);
    assert(publication->budget(&h,&lease_charge,GC_BUDGET_RESERVE)==GC_BUDGET_OK);
    assert(gc_native_metadata_prepare(&lease_charge,&native_metadata)==GC_NATIVE_METADATA_OK);
    MetadataLeaseSlot *lease_slots=(void *)(uintptr_t)lease_charge.allocation;
    for(uint64_t i=0;i<lease_charge.bytes;i++)assert(!((unsigned char *)lease_slots)[i]);
    assert(publication->budget(&h,&lease_charge,GC_BUDGET_COMMIT)==GC_BUDGET_OK);
    assert(!h.metadata_pending&&h.metadata_committed==metadata_before+lease_charge.bytes);
    lease_slots[0].next_free=2;
    MetadataLeasePool leases={(uintptr_t)&h,(uintptr_t)lease_slots,2,1,0};
    // Inject commit failure, then bitmap allocation failure, then failed cleanup.
    // Budget is released only after the adapter confirms cleanup succeeded.
    for (unsigned failure = 1; failure <= 3; failure++) {
        PageLease l = {.page_count = 2};
        assert(pool(&h, &l, GC_POOL_RESERVE) == GC_POOL_OK);
        assert(h.pending == 2 * GC_HEAP_PAGE_BYTES && h.committed == 0);
        void *address = (unsigned char *)mapping + (l.head_index + 1) * GC_HEAP_PAGE_BYTES;
        size_t bytes = l.page_count * GC_HEAP_PAGE_BYTES;
        if (failure > 1) {
            pool_vm_commit(address, bytes);
            l.committed_mapping = 1;
            assert(*(uint64_t *)address == 0); // recommit after rollback discards dirty data
            *(uint64_t *)address = UINT64_C(0xabcdef0123456789);
            // Bitmap allocation fails, so no initialized page may be published.
            assert(pool(&h, &l, GC_POOL_COMMIT) == GC_POOL_INVALID);
            if (failure == 3) {
                HeapPool before = h;
                assert(pool(&h, &l, GC_POOL_ABORT) == GC_POOL_INVALID);
                assert(memcmp(&h, &before, sizeof(h)) == 0 && l.committed_mapping == 1);
            }
            pool_vm_decommit(address, bytes);
            l.committed_mapping = 0;
        }
        assert(pool(&h, &l, GC_POOL_ABORT) == GC_POOL_OK);
        assert(!h.pending && !h.metadata_pending && !h.committed);
    }
    RootRegistry root_registry={0};HandshakeState handshake={0};
    MutatorContext ctx={.abi_version=GC_ABI_VERSION};
    ThreadRegistration participant={.context=(uintptr_t)&ctx};
    WorkerRequest worker={(uintptr_t)&handshake,(uintptr_t)&participant};
    assert(publication->worker(&root_registry,&worker,GC_WK_ATTACH)==GC_WK_OK);
    assert(ctx.thread_id&&handshake.members==1&&atomic_load(&ctx.adopted_phase)==root_registry.phase);
    MarkTask mark_tasks[2]={0};MarkQueue queues[2]={0};
    RootAudit audits[2]={0};MarkPhaseControl phases[2]={0};
    CycleLedger demand={0};CycleTicket started_cycles[2]={{.kind=0},{.kind=1}};
    RememberedInventory remembered={.epoch=0x10000,.last_phase=0x10000};
    for(unsigned generation=0;generation<2;generation++){
        queues[generation]=(MarkQueue){.tasks=(uintptr_t)&mark_tasks[generation],.capacity=1,
            .free_head=1,.epoch=1,.generation=generation};
        audits[generation]=(RootAudit){.queue=(uintptr_t)&queues[generation],.context=(uintptr_t)&ctx};
        phases[generation]=(MarkPhaseControl){.handshake=(uintptr_t)&handshake,
            .audit=(uintptr_t)&audits[generation],.generation=generation,.remembered=(uintptr_t)&remembered};
        // Fresh heap: no prior colors, remembered slots or TLABs to retire.
        // Prepare only MarkStart; the shared coordinator publishes ConcurrentMark.
        unsigned shift=generation*4;
        root_registry.phase=((root_registry.phase+UINT64_C(0x10000))&~(UINT64_C(15)<<shift))|(UINT64_C(1)<<shift);
        CycleTicket requested={.kind=generation};
        assert(publication->cycle(&demand,&requested,GC_CY_REQUEST)==GC_CY_OK);
        CycleMarkStart admission={(uintptr_t)&root_registry,(uintptr_t)&phases[generation]};
        assert(publication->cycle_mark_start(&demand,&started_cycles[generation],&admission)==GC_CY_OK);
        assert(demand.started[generation]==1&&!demand.completed[generation]);
        assert(publication->phase(&root_registry,&phases[generation],GC_MP_ACTIVATE)==GC_MP_WAIT);
        assert(publication->worker(&root_registry,&worker,GC_WK_POLL)==GC_WK_PARK);
        assert(atomic_load(&ctx.adopted_phase)==root_registry.phase);
        assert(publication->phase(&root_registry,&phases[generation],GC_MP_ACTIVATE)==GC_MP_ADVANCED);
        assert(publication->phase(&root_registry,&phases[generation],GC_MP_RELEASE)==GC_MP_WAIT);
        assert(publication->worker(&root_registry,&worker,GC_WK_POLL)==GC_WK_PARK);
        assert(publication->phase(&root_registry,&phases[generation],GC_MP_RELEASE)==GC_MP_MARKING);
        assert(publication->worker(&root_registry,&worker,GC_WK_POLL)==GC_WK_OK);
    }
    assert((root_registry.phase&255)==0x22&&handshake.generation==4&&!handshake.active);
    assert(atomic_load(&ctx.adopted_phase)==root_registry.phase);

    uint64_t invalid_root_bytes=17;
    assert(gc_native_roots_size(0,4,native_metadata.quantum,&invalid_root_bytes)==GC_NATIVE_METADATA_INVALID&&invalid_root_bytes==17);
    assert(gc_native_roots_size(6,0,native_metadata.quantum,&invalid_root_bytes)==GC_NATIVE_METADATA_INVALID&&invalid_root_bytes==17);
    assert(gc_native_roots_size(UINT64_MAX,4,native_metadata.quantum,&invalid_root_bytes)==GC_NATIVE_METADATA_INVALID&&invalid_root_bytes==17);
    assert(gc_native_roots_size(6,UINT64_MAX,native_metadata.quantum,&invalid_root_bytes)==GC_NATIVE_METADATA_INVALID&&invalid_root_bytes==17);
    gc_run_native_startup_cases(native_metadata.quantum,publication->native_startup);
    gc_run_startup_commit_cases(publication->native_startup_commit);
    gc_run_startup_release_cases(publication->startup_release);
    gc_run_allocation_sample_cases(publication->allocation_sample);
    gc_run_allocation_pressure_cases(publication->allocation_pressure);
    page_startup_failure(publication,&native_metadata,&h,&ctx,&root_registry,&worker,register_descriptor);
    page_startup_committed_cleanup(publication,&native_metadata,&h,&ctx,&root_registry,&worker,register_descriptor);
    MetadataCharge root_charges[2]={{0}},*root_charge=&root_charges[0];
    MetadataCharge descriptor_charge={0};
    NativeStartupRequest startup={(uintptr_t)root_charge,(uintptr_t)&descriptor_charge,3,2,64,native_metadata.quantum};
    assert(publication->native_startup(&h,&startup)==GC_BUDGET_OK);
    assert(publication->worker(&root_registry,&worker,GC_WK_ENTER_NATIVE)==GC_WK_OK);
    MetadataCharge root_before=*root_charge;
    NativeMetadataOps failed_roots=native_metadata;failed_roots.map=page_root_map_failure;
    assert(gc_native_roots_prepare(root_charge,0,3,2,&native_metadata,publication->root_storage_prepare)==GC_NATIVE_METADATA_INVALID);
    assert(!memcmp(root_charge,&root_before,sizeof(*root_charge)));
    assert(gc_native_roots_prepare(root_charge,ctx.thread_id,3,2,&failed_roots,publication->root_storage_prepare)==GC_NATIVE_METADATA_OS_ERROR);
    assert(!memcmp(root_charge,&root_before,sizeof(*root_charge))&&h.metadata_pending==root_charge->bytes+descriptor_charge.bytes);
    NativeStartupPreparationOps prepare={&native_metadata,publication->native_root_prepare,
        publication->native_builtin_registry_prepare,publication->root_storage_prepare,
        publication->builtin_prepare,register_descriptor};
    gc_run_startup_prepare_rejections(&startup,ctx.thread_id,&prepare,publication->native_startup_prepare);
    assert((publication->native_startup_prepare?publication->native_startup_prepare(&startup,ctx.thread_id):
        gc_native_startup_prepare(&startup,ctx.thread_id,&prepare))==GC_NATIVE_METADATA_OK);
    assert(!ctx.root_segments&&!root_registry.active&&root_charge->stage==1&&descriptor_charge.stage==1);
    assert(publication->worker(&root_registry,&worker,GC_WK_LEAVE_NATIVE)==GC_WK_OK);
    assert(publication->native_startup_commit(&h,&startup)==GC_BUDGET_OK);
    NativeRoots *root_storage=(void *)(uintptr_t)root_charge->allocation;
    RootSegment *root_segment=&root_storage->segment;
    BufferCursor *root_buffer=&root_storage->journal;
    RootRegistration *registrations[2]={&root_storage->registration,NULL};
    assert(registrations[0]->segment==(uintptr_t)root_segment&&!atomic_load(&registrations[0]->cleanup_cursor));
    RootLease root_leases[2]={{0}};
    RootLifeRequest lives[2]={{.registration=(uintptr_t)registrations[0],.lease=(uintptr_t)&root_leases[0],.context=(uintptr_t)&ctx},
        {.lease=(uintptr_t)&root_leases[1],.context=(uintptr_t)&ctx}};
    assert(publication->native_root_publish(&root_registry,&ctx,root_charge)==GC_RL_OK);
    lives[0].scope=registrations[0]->scope;
    assert(ctx.root_segments==(uintptr_t)registrations[0]&&root_registry.active==1);
    RootRequest root = {.segment = (uintptr_t)root_segment};

    NativeBuiltinRegistry *descriptor_storage=(void *)(uintptr_t)descriptor_charge.allocation;
    const TypeDescriptor *desc=&descriptor_storage->builtins.raw;
    DescriptorRegistry registry=descriptor_storage->registry;
    uint64_t mixed_bitmap = 10;
    TypeDescriptor mixed = {.id = 43, .alignment = 8, .fixed_bytes = 32,
        .fixed_bitmap = (uintptr_t)&mixed_bitmap, .fixed_bitmap_words = 1};
    assert(register_descriptor(&registry, desc) == GC_DESC_DUPLICATE);
    assert(register_descriptor(&registry, &mixed) == GC_DESC_OK);
    PageMarkWorker marker={.ops=publication,.roots=&root_registry,.handshake=&handshake,.leases=&leases,.registry=&registry,.allocation_owner=&ctx};
#ifdef _WIN32
    marker.thread=CreateThread(NULL,0,page_mark_worker,&marker,0,NULL);assert(marker.thread);
#else
    assert(pthread_create(&marker.thread,NULL,page_mark_worker,&marker)==0);
#endif
    while(!atomic_load_explicit(&marker.ready,memory_order_acquire))page_mark_yield();
    assert(handshake.members==2);
    uint64_t previous_id = 0,allocation_phases[2],total_allocated=0;
    for (unsigned round = 0; round < 2; round++) {
        if(round){
            RootRequest held=root;
            assert(root_step(&ctx,&root,GC_ROOT_RESERVE)==GC_ROOT_FULL);
            assert(!memcmp(&root,&held,sizeof(root)));
            assert(root_step(&ctx,&held,GC_ROOT_READ_RAW)==GC_ROOT_OK&&held.value);
            uint64_t retained=held.value,old_scope=registrations[0]->scope;
            root_charge=&root_charges[1];
            assert(gc_native_roots_size(3,2,native_metadata.quantum,&root_charge->bytes)==GC_NATIVE_METADATA_OK);
            assert(publication->budget(&h,root_charge,GC_BUDGET_RESERVE)==GC_BUDGET_OK);
            assert(publication->worker(&root_registry,&worker,GC_WK_ENTER_NATIVE)==GC_WK_OK);
            assert(page_prepare_roots(root_charge,ctx.thread_id,&native_metadata,publication,3,2)==GC_NATIVE_METADATA_OK);
            assert(publication->worker(&root_registry,&worker,GC_WK_LEAVE_NATIVE)==GC_WK_OK);
            assert(publication->budget(&h,root_charge,GC_BUDGET_COMMIT)==GC_BUDGET_OK);
            root_storage=(void *)(uintptr_t)root_charge->allocation;
            root_segment=&root_storage->segment;root_buffer=&root_storage->journal;
            registrations[1]=&root_storage->registration;
            lives[1].registration=(uintptr_t)registrations[1];
            assert(publication->native_root_publish(&root_registry,&ctx,root_charge)==GC_RL_OK);
            lives[1].scope=registrations[1]->scope;
            assert(root_registry.active==2&&registrations[1]->scope>old_scope);
            assert(ctx.root_segments==(uintptr_t)registrations[1]);
            assert(root_step(&ctx,&held,GC_ROOT_READ_RAW)==GC_ROOT_OK&&held.value==retained);
            root=(RootRequest){.segment=(uintptr_t)root_segment};
        }
        MetadataCharge attached_charge=*root_charge;
        assert(page_dispose_roots(root_charge,&native_metadata,publication)==GC_NATIVE_METADATA_INVALID);
        assert(!memcmp(root_charge,&attached_charge,sizeof(attached_charge)));
        allocation_phases[round]=atomic_load(&ctx.adopted_phase);
        PageLease l = {.page_count = round ? 1 : 2};
        assert(pool(&h, &l, GC_POOL_RESERVE) == GC_POOL_OK);
        void *address = (unsigned char *)mapping + (l.head_index + 1) * GC_HEAP_PAGE_BYTES;
        size_t bytes = l.page_count * GC_HEAP_PAGE_BYTES;
        pool_vm_commit(address, bytes);
        l.committed_mapping = 1;
        assert(*(uint64_t *)address == 0);
        memset(address, 0, bytes); // OS/page preparation, outside the metadata lock
        l.starts = (uintptr_t)calloc(1, bytes / 128);
        assert(l.starts);
        assert(pool(&h, &l, GC_POOL_COMMIT) == GC_POOL_OK);
        assert(!l.starts && !l.committed_mapping); // pool now owns both resources
        AllocationPage *page = &pages[l.head_index];
        forwards[round]=(ForwardTable){(uintptr_t)cells[round],2,0};
        page->forward_table=(uintptr_t)&forwards[round];
        assert(bind(&ctx, page, 0) == GC_ALLOC_OK);
        assert(bind(&ctx, page, 0) == GC_ALLOC_INVALID); // cannot steal/rebind a live TLAB
        AllocationResult output = {0};
        AllocationRequest request = {.page=(uintptr_t)page,.output=(uintptr_t)&output,.marks=(uintptr_t)&marks};
        AllocationShape allocation_shape={GC_DESC_RAW_BYTES,round?0:GC_HEAP_PAGE_BYTES,(uintptr_t)&request};
        AllocationStart start={(uintptr_t)&registry,(uintptr_t)&allocation_shape,(uintptr_t)&root};
        // Abandon a refill attempt while its reserved output is still empty.
        allocation_shape.payload_bytes=2*GC_HEAP_PAGE_BYTES;
        uint64_t before_top=ctx.tlab_top,before_marks=marks.used,before_revision=atomic_load(&ctx.root_revision);
        uint64_t before_journal=atomic_load(&root_buffer->published);
        assert(allocate_start(&ctx,&start)==GC_ALLOC_REFILL);
        assert(atomic_load(&ctx.allocated_bytes)==total_allocated);
        assert(!output.reference&&ctx.tlab_top==before_top&&marks.used==before_marks);
        assert(atomic_load(&ctx.root_revision)==before_revision&&atomic_load(&root_buffer->published)==before_journal);
        assert(root_step(&ctx,&root,GC_ROOT_READ_RAW)==GC_ROOT_INVALID);
        allocation_shape.payload_bytes=round?0:GC_HEAP_PAGE_BYTES;
        assert(allocate_start(&ctx,&start)==GC_ALLOC_OK);
        total_allocated+=round?48:GC_HEAP_PAGE_BYTES+32;
        assert(atomic_load(&ctx.allocated_bytes)==total_allocated);
        assert(request.descriptor==(uintptr_t)desc);
        assert(output.object_id > previous_id);
        previous_id = output.object_id;
        uint64_t user = output.reference >> 16;
        ObjectHeader *header = (ObjectHeader *)((unsigned char *)mapping + user - sizeof(ObjectHeader));
        assert(header->payload_bytes == request.elements && header->descriptor_id == GC_DESC_RAW_BYTES);
        assert(((MarkRecord *)(uintptr_t)header->mark_record)->object_id == output.object_id && atomic_load(&header->state) == GC_OBJECT_INITIALIZING);
        if (request.elements) {
            assert(((unsigned char *)mapping)[user] == 0);
            assert(((unsigned char *)mapping)[user + request.elements - 1] == 0);
        }
        uint64_t offsets[4] = {UINT64_MAX, UINT64_MAX, UINT64_MAX, UINT64_MAX};
        DescriptorScan shape = {header->descriptor_id, header->payload_bytes, 0, 4,
            (uintptr_t)offsets, 4, 0, 0};
        assert(scan_descriptor(&registry, &shape) == GC_SCAN_DONE);
        assert(!shape.examined && !shape.written && shape.next_offset == request.elements);
        page_pipeline_publish(publication,&leases,&ctx,page,&cells[round][0],&root,&registry,queues,round,&marker,NULL,bind,&worker);
        RootRequest child_root=root;
        allocation_shape.descriptor_id=43;allocation_shape.payload_bytes=32;
        assert(allocate_start(&ctx,&start)==GC_ALLOC_OK);
        total_allocated+=64;assert(atomic_load(&ctx.allocated_bytes)==total_allocated);
        assert(request.descriptor==(uintptr_t)&mixed&&!request.elements);
        assert(output.object_id > previous_id); previous_id = output.object_id;
        user = output.reference >> 16;
        header = (ObjectHeader *)((unsigned char *)mapping + user - sizeof(ObjectHeader));
        assert(header->descriptor_id == 43 && header->payload_bytes == 32);
        // Shape metadata is safe to consult for an INITIALIZING object; this
        // does not publish its payload or authorize a collector to read it.
        shape = (DescriptorScan){header->descriptor_id, header->payload_bytes, 0, 4,
            (uintptr_t)offsets, 4, 0, 0};
        assert(scan_descriptor(&registry, &shape) == GC_SCAN_DONE);
        assert(shape.examined == 4 && shape.written == 2 && offsets[0] == 8 && offsets[1] == 24);
        assert(offsets[2] == UINT64_MAX && offsets[3] == UINT64_MAX);
        page_pipeline_publish(publication,&leases,&ctx,page,&cells[round][1],&root,&registry,queues,round,&marker,&child_root,bind,&worker);
        assert(publication->worker(&root_registry,&worker,GC_WK_POLL)==GC_WK_OK);
        assert(bind(&ctx, page, 1) == GC_ALLOC_INVALID); // already closed for the mid-scan rendezvous
        assert(atomic_load(&ctx.allocated_bytes)==total_allocated);
        assert(!ctx.tlab_start && !ctx.tlab_top && !ctx.tlab_end && !page->owner);
        assert(atomic_load(&page->state) == GC_PAGE_CLOSED);
        assert(pool(&h, &l, GC_POOL_ABORT) == GC_POOL_INVALID); // closed != reclaimable
        // Both actors are outside guards and all previous commands completed.
        // Relay ownership of the registry while preserving pending journals.
        assert(atomic_load_explicit(&marker.completed,memory_order_acquire)==(round+1)*3);
        uint64_t published=atomic_load(&root_buffer->published),consumed=atomic_load(&root_buffer->consumed);
        assert(published>consumed&&root_registry.active==round+1);
        root_registry.phase+=UINT64_C(0x10000);
        assert(publication->handshake(&handshake,NULL,GC_HS_BEGIN)==GC_HS_OK);
        assert(publication->worker(&root_registry,&worker,GC_WK_POLL)==GC_WK_PARK);
        assert(handshake.acknowledged==1&&handshake.expected==2);
        uint64_t rendezvous_sequence=round*2+1;
        atomic_store_explicit(&marker.rendezvous_request,rendezvous_sequence,memory_order_release);
        while(atomic_load_explicit(&marker.rendezvous_done,memory_order_acquire)<rendezvous_sequence)page_mark_yield();
        assert(handshake.acknowledged==2&&atomic_load(&marker.rendezvous_phase)==root_registry.phase);
        assert(atomic_load(&root_buffer->published)==published&&atomic_load(&root_buffer->consumed)==consumed);
        assert(publication->handshake(&handshake,NULL,GC_HS_END)==GC_HS_OK);
        assert(publication->worker(&root_registry,&worker,GC_WK_POLL)==GC_WK_OK);
        atomic_store_explicit(&marker.rendezvous_request,rendezvous_sequence+1,memory_order_release);
        while(atomic_load_explicit(&marker.rendezvous_done,memory_order_acquire)<rendezvous_sequence+1)page_mark_yield();
        assert(atomic_load(&marker.rendezvous_phase)==atomic_load(&ctx.adopted_phase)&&!handshake.active);

    }
    assert(atomic_load_explicit(&marker.completed,memory_order_acquire)==6&&marker.owner_progress>=6144);
    atomic_store_explicit(&marker.stop,1,memory_order_release);
#ifdef _WIN32
    assert(WaitForSingleObject(marker.thread,INFINITE)==WAIT_OBJECT_0);assert(CloseHandle(marker.thread));
#else
    assert(pthread_join(marker.thread,NULL)==0);
#endif
    assert(handshake.members==1&&handshake.last_identity==2&&marker.duplicate_children==6&&marker.spilled_children==2&&marker.scanned_jobs==8);
    assert(marker.scan_slices[GC_YOUNG]==6&&marker.scan_slices[GC_OLD]==6);
    assert(marker.scan_yields[GC_YOUNG]==2&&marker.scan_yields[GC_OLD]==2);
    assert(marker.examined_slots[GC_YOUNG]==8&&marker.examined_slots[GC_OLD]==8);
    assert(marker.loaded_slots[GC_YOUNG]==4&&marker.loaded_slots[GC_OLD]==4);
    puts("mark phase handoff: partial scan and overflow survive blocked-worker rendezvous and phase adoption PASS");
    puts("mark slices: guarded scan, poll and reclaim preserve cursor, child leases and exact per-generation work PASS");
    assert(total_allocated==GC_HEAP_PAGE_BYTES+208&&marker.observed_allocated==total_allocated);
    assert(marker.allocation_window.elapsed_ns&&marker.allocation_window.delta_bytes==total_allocated);
    puts("allocation sampling: real monotonic window observes exact successful allocation delta PASS");
    assert(h.committed<=h.hard_max&&h.pending<=h.hard_max-h.committed&&h.relocation_reserve<=h.hard_max-h.committed-h.pending);
    uint64_t headroom=h.hard_max-h.committed-h.pending-h.relocation_reserve;
    assert(publication->allocation_pressure(&marker.allocation_window,headroom,marker.allocation_window.elapsed_ns)==GC_PRESSURE_QUIET);
    assert(marker.allocation_window.elapsed_ns<=UINT64_MAX/4);
    assert(publication->allocation_pressure(&marker.allocation_window,headroom,marker.allocation_window.elapsed_ns*4)==GC_PRESSURE_REQUEST);
    // Marking is active, so measured pressure requests the following cycle.
    // This fixture does not START/FINISH that second cycle
    // or claim that observed allocation alone proves any collection complete.
    assert(publication->heap_pressure(&h,&marker.allocation_window,marker.allocation_window.elapsed_ns)==GC_PRESSURE_REQUEST);
    CycleTicket ticket={.kind=GC_CYCLE_YOUNG};
    CyclePressure cycle_pressure={(uintptr_t)&h,(uintptr_t)&marker.allocation_window,marker.allocation_window.elapsed_ns};
    assert(publication->cycle_pressure(&demand,&ticket,&cycle_pressure)==GC_CP_REQUESTED);
    assert(ticket.cycle==2&&demand.requested[0]==2&&demand.started[0]==1&&!demand.completed[0]);
    assert(publication->cycle(&demand,&ticket,GC_CY_POLL)==GC_CY_WAIT);
    puts("heap pressure pipeline: soft target requests collection while hard-limit forecast remains quiet PASS");
    puts("allocation pressure pipeline: measured demand and reserve-adjusted headroom request an uncompleted cycle PASS");
    puts("allocation accounting: concurrent samples and TLAB refill preserve exact successful block bytes PASS");
    assert(atomic_load(&marker.metadata_wait)==2&&atomic_load(&marker.metadata_resume)==2&&!handshake.native);
    assert(h.committed == 3 * GC_HEAP_PAGE_BYTES && h.pressure && !h.pending && !h.metadata_pending);
    // Model a caller-owned return slot in an independently allocated segment.
    // Its lifetime must not depend on the callee's source storage surviving.
    MetadataCharge return_charge={0};
    assert(gc_native_roots_size(1,2,native_metadata.quantum,&return_charge.bytes)==GC_NATIVE_METADATA_OK);
    assert(publication->budget(&h,&return_charge,GC_BUDGET_RESERVE)==GC_BUDGET_OK);
    assert(publication->worker(&root_registry,&worker,GC_WK_ENTER_NATIVE)==GC_WK_OK);
    assert(page_prepare_roots(&return_charge,ctx.thread_id,&native_metadata,publication,1,2)==GC_NATIVE_METADATA_OK);
    assert(publication->worker(&root_registry,&worker,GC_WK_LEAVE_NATIVE)==GC_WK_OK);
    assert(publication->budget(&h,&return_charge,GC_BUDGET_COMMIT)==GC_BUDGET_OK);
    NativeRoots *return_storage=(void *)(uintptr_t)return_charge.allocation;
    RootLease return_lease={0};
    RootLifeRequest return_life={.registration=(uintptr_t)&return_storage->registration,
        .lease=(uintptr_t)&return_lease,.context=(uintptr_t)&ctx};
    assert(publication->native_root_publish(&root_registry,&ctx,&return_charge)==GC_RL_OK);
    return_life.scope=return_storage->registration.scope;
    assert(return_storage->registration.owner_next==(uintptr_t)registrations[1]);
    assert(registrations[1]->owner_previous==(uintptr_t)&return_storage->registration);
    RootRequest return_root={.segment=(uintptr_t)&return_storage->segment};
    assert(root_step(&ctx,&return_root,GC_ROOT_RESERVE)==GC_ROOT_OK);
    RootSegment *source_segment=(void *)(uintptr_t)registrations[0]->segment;
    RootCell *source_cell=(RootCell *)(uintptr_t)source_segment->cells+2;
    RootRequest return_source={.segment=(uintptr_t)source_segment,
        .handle={(uintptr_t)source_cell,source_cell->reuse_generation,source_segment->scope},.value=UINT64_MAX};
    uint64_t return_reference=atomic_load(&source_cell->value),return_phase=atomic_load(&ctx.adopted_phase);
    assert(return_reference&&publication->root_copy(&ctx,&return_source,&return_root)==GC_ROOT_OK);
    assert(root_registry.active==3);
    puts("native startup publication: committed roots, growth and caller return scope use registered native packages PASS");
    for(unsigned segment=0;segment<2;segment++){
        RootLifeRequest *life=&lives[segment];
        root_segment=(void *)(uintptr_t)registrations[segment]->segment;
        root_charge=&root_charges[segment];MetadataCharge retiring_charge=*root_charge;
        assert(publication->life(&root_registry,life,GC_RL_RETIRE)==GC_RL_OK);
        assert(page_dispose_roots(root_charge,&native_metadata,publication)==GC_NATIVE_METADATA_INVALID);
        assert(!memcmp(root_charge,&retiring_charge,sizeof(retiring_charge)));
        assert(publication->life(&root_registry,life,GC_RL_CLAIM)==GC_RL_OK&&life->mask==3);
        uint64_t roots[3]={0};RootScanJob scan_roots={.scan={(uintptr_t)root_segment,life->begin,(uintptr_t)roots,3,0},
            .lease=(uintptr_t)&root_leases[segment],.serial=life->serial};
        assert(publication->leased(&scan_roots)==GC_ROOT_OK&&scan_roots.scan.written==3);
        assert(!roots[0]);
        RootChange changes[2];RootLease journal_lease={0};
        RootJournalRequest journal={.registration=(uintptr_t)registrations[segment],.lease=(uintptr_t)&journal_lease,
            .scope=registrations[segment]->scope,.output=(uintptr_t)changes,.capacity=2};
        assert(publication->journal(&root_registry,&journal,GC_RJ_CLAIM)==GC_RJ_OK);
        assert(publication->journal(&root_registry,&journal,GC_RJ_COPY)==GC_RJ_OK&&journal.written==2);
        for (unsigned i = 0; i < 2; i++) {
            uint64_t reference=roots[1+i];
            assert(reference && changes[i].previous == 0 && changes[i].next == reference);
            assert(changes[i].phase==allocation_phases[segment]);
            assert(records[segment*2+i].young_epoch==1&&records[segment*2+i].old_epoch==1);
            const ObjectHeader *object = (const ObjectHeader *)((unsigned char *)mapping +
                (reference >> 16) - sizeof(ObjectHeader));
            assert(object->descriptor_id == (i & 1 ? 43 : GC_DESC_RAW_BYTES));
            assert(atomic_load_explicit(&object->state,memory_order_acquire)==GC_OBJECT_PUBLISHED);
        }
        // Both captured marking obligations completed before either ACK.
        assert(publication->life(&root_registry,life,GC_RL_ACK)==GC_RL_OK);
        assert(publication->life(&root_registry,life,GC_RL_CLEAN)==GC_RL_WAIT);
        assert(publication->journal(&root_registry,&journal,GC_RJ_ACK)==GC_RJ_OK);
        assert(publication->life(&root_registry,life,GC_RL_CLEAN)==GC_RL_OK);
        assert(publication->clear(life)==GC_RL_OK);
        assert(page_dispose_roots(root_charge,&native_metadata,publication)==GC_NATIVE_METADATA_INVALID);
        assert(!memcmp(root_charge,&retiring_charge,sizeof(retiring_charge)));
        assert(publication->life(&root_registry,life,GC_RL_FINISH)==GC_RL_OK);
        uint64_t roots_before=h.metadata_committed,root_bytes=root_charge->bytes;
        assert(publication->worker(&root_registry,&worker,GC_WK_ENTER_NATIVE)==GC_WK_OK);
        assert(page_dispose_roots(root_charge,&native_metadata,publication)==GC_NATIVE_METADATA_OK);
        assert(publication->worker(&root_registry,&worker,GC_WK_LEAVE_NATIVE)==GC_WK_OK);
        assert(publication->budget(&h,root_charge,GC_BUDGET_RELEASE)==GC_BUDGET_OK);
        assert(h.metadata_committed==roots_before-root_bytes&&!h.metadata_pending);
        registrations[segment]=NULL; // no stale root-storage borrow survives unmap
    }
    return_source=(RootRequest){0};source_segment=NULL;source_cell=NULL;
    assert(root_registry.active==1&&!root_registry.retiring&&!root_registry.leases);
    assert(ctx.root_segments==(uintptr_t)&return_storage->registration);
    assert(root_step(&ctx,&return_root,GC_ROOT_READ_RAW)==GC_ROOT_OK&&return_root.value==return_reference);
    RootAccess returned={.follow={.context=(uintptr_t)&ctx,.root=return_root}};
    returned.follow.root.value=0;
    assert(publication->root_access(&leases,&returned)==GC_RAC_READY);
    GuardedScalar returned_value={(uintptr_t)&returned.access.scope,returned.follow.cleanup.page,
        (uintptr_t)&registry,0,atomic_load(&ctx.adopted_phase),0};
    assert(publication->scalar(&returned_value,GC_SCALAR_LOAD)==GC_SCALAR_OK&&returned_value.value==17);
    ObjectHeader *returned_header=(void *)((unsigned char *)mapping+(returned.access.reference>>16)-sizeof(ObjectHeader));
    assert(returned_header->mark_record==(uintptr_t)&records[1]&&records[1].young_epoch==1&&records[1].old_epoch==1);
    RootAccess held_return=returned;
    assert(publication->access_cleanup(&leases,&returned,GC_RAC_RELEASE)==GC_RF_INVALID);
    assert(!memcmp(&returned,&held_return,sizeof(returned))&&leases.active==1);
    assert(publication->access_cleanup(NULL,&returned,GC_RAC_EXIT)==GC_RF_DONE);
    returned_header=NULL; // no managed address is carried into native state
    MetadataLeasePool rejected_pool=leases;rejected_pool.heap=0;
    RootAccess pending_return=returned;MetadataLeasePool held_pool=leases;
    assert(publication->access_cleanup(&rejected_pool,&returned,GC_RAC_RELEASE)==GC_RF_CLEANUP_REQUIRED);
    assert(!memcmp(&returned,&pending_return,sizeof(returned))&&!memcmp(&leases,&held_pool,sizeof(leases)));
    assert(publication->root_access(&leases,&returned)==GC_RF_CLEANUP_REQUIRED);
    assert(!memcmp(&returned,&pending_return,sizeof(returned)));
    assert(publication->worker(&root_registry,&worker,GC_WK_ENTER_NATIVE)==GC_WK_OK);
    assert(publication->worker(&root_registry,&worker,GC_WK_LEAVE_NATIVE)==GC_WK_OK);
    assert(publication->access_cleanup(&leases,&returned,GC_RAC_RELEASE)==GC_RF_DONE&&!leases.active);
    // The original callee packages are already unmapped. Reuse exactly this
    // caller control/root after cleanup, and resolve again before scalar access.
    assert(publication->root_access(&leases,&returned)==GC_RAC_READY);
    returned_value=(GuardedScalar){(uintptr_t)&returned.access.scope,returned.follow.cleanup.page,
        (uintptr_t)&registry,0,atomic_load(&ctx.adopted_phase),0};
    assert(publication->scalar(&returned_value,GC_SCALAR_LOAD)==GC_SCALAR_OK&&returned_value.value==17);
    assert(publication->access_cleanup(NULL,&returned,GC_RAC_EXIT)==GC_RF_DONE);
    assert(publication->access_cleanup(&leases,&returned,GC_RAC_RELEASE)==GC_RF_DONE&&!leases.active);
    assert(root_step(&ctx,&return_root,GC_ROOT_READ_RAW)==GC_ROOT_OK&&return_root.value==return_reference);
    puts("root return cleanup: rejected release and native boundary retain the caller root through balanced control reuse PASS");
    // Retire the caller only after it has consumed the return value. Its own
    // journal and scanner leases are separate from the already-unmapped source.
    assert(publication->life(&root_registry,&return_life,GC_RL_RETIRE)==GC_RL_OK);
    assert(publication->life(&root_registry,&return_life,GC_RL_CLAIM)==GC_RL_OK&&return_life.mask==3);
    uint64_t returned_scan_value=0;
    RootScanJob returned_scan={.scan={(uintptr_t)&return_storage->segment,return_life.begin,(uintptr_t)&returned_scan_value,1,0},
        .lease=(uintptr_t)&return_lease,.serial=return_life.serial};
    assert(publication->leased(&returned_scan)==GC_ROOT_OK&&returned_scan.scan.written==1&&returned_scan_value==return_reference);
    RootLease return_journal_lease={0};RootChange return_change[2];
    RootJournalRequest return_journal={.registration=(uintptr_t)&return_storage->registration,.lease=(uintptr_t)&return_journal_lease,
        .scope=return_storage->registration.scope,.output=(uintptr_t)return_change,.capacity=2};
    assert(publication->journal(&root_registry,&return_journal,GC_RJ_CLAIM)==GC_RJ_OK);
    assert(publication->journal(&root_registry,&return_journal,GC_RJ_COPY)==GC_RJ_OK&&return_journal.written==1);
    assert(!return_change[0].previous&&return_change[0].next==return_reference&&return_change[0].phase==return_phase);
    assert(publication->life(&root_registry,&return_life,GC_RL_ACK)==GC_RL_OK);
    assert(publication->life(&root_registry,&return_life,GC_RL_CLEAN)==GC_RL_WAIT);
    assert(publication->journal(&root_registry,&return_journal,GC_RJ_ACK)==GC_RJ_OK);
    assert(publication->life(&root_registry,&return_life,GC_RL_CLEAN)==GC_RL_OK);
    assert(publication->clear(&return_life)==GC_RL_OK);
    assert(publication->life(&root_registry,&return_life,GC_RL_FINISH)==GC_RL_OK);
    assert(publication->worker(&root_registry,&worker,GC_WK_ENTER_NATIVE)==GC_WK_OK);
    assert(page_dispose_roots(&return_charge,&native_metadata,publication)==GC_NATIVE_METADATA_OK);
    assert(publication->worker(&root_registry,&worker,GC_WK_LEAVE_NATIVE)==GC_WK_OK);
    assert(publication->budget(&h,&return_charge,GC_BUDGET_RELEASE)==GC_BUDGET_OK);
    puts("root return: caller root survives source-segment retirement and native unmap with independent journal ownership PASS");
    assert(!root_registry.active&&!root_registry.retiring&&!root_registry.leases&&!root_registry.inventory);
    assert(!ctx.root_segments&&!ctx.retiring_roots);
    for(unsigned generation=0;generation<2;generation++){
        assert(publication->audit(&root_registry,&audits[generation],GC_RA_BEGIN)==GC_RA_DONE);
        assert(publication->phase(&root_registry,&phases[generation],GC_MP_BEGIN)==GC_MP_ADVANCED);
        assert(publication->phase(&root_registry,&phases[generation],GC_MP_DECIDE)==GC_MP_WAIT);
        assert(publication->worker(&root_registry,&worker,GC_WK_POLL)==GC_WK_PARK);
        assert(publication->phase(&root_registry,&phases[generation],GC_MP_DECIDE)==GC_MP_ADVANCED);
        assert(phases[generation].outcome==4);
        assert(publication->phase(&root_registry,&phases[generation],GC_MP_RELEASE)==GC_MP_WAIT);
        assert(publication->worker(&root_registry,&worker,GC_WK_POLL)==GC_WK_PARK);
        assert(publication->phase(&root_registry,&phases[generation],GC_MP_RELEASE)==GC_MP_SELECT);
        assert(publication->worker(&root_registry,&worker,GC_WK_POLL)==GC_WK_OK);
    }
    assert((root_registry.phase&255)==0x44&&handshake.generation==12&&!handshake.active);
    assert(gc_context_transition_locked(&ctx,GC_RUNNING,GC_DETACHING)==GC_CONTEXT_OK);
    assert(publication->handshake(&handshake,&participant,GC_HS_DETACH)==GC_HS_OK);
    assert(!handshake.members&&!handshake.head);
    assert(!leases.active&&leases.capacity==3);
    // The worker is joined and all queues/roots are drained before storage dies.
    for(unsigned segment=0;segment<2;segment++)
        assert(root_charges[segment].stage==3&&!root_charges[segment].allocation);
    uint64_t charged=h.metadata_committed;
    MetadataCharge held_charge=lease_charge;
    assert(publication->budget(&h,&lease_charge,GC_BUDGET_RELEASE)==GC_BUDGET_INVALID);
    assert(h.metadata_committed==charged&&!memcmp(&lease_charge,&held_charge,sizeof(lease_charge)));
    assert(gc_native_metadata_dispose(&lease_charge,&native_metadata)==GC_NATIVE_METADATA_OK);
    assert(publication->budget(&h,&lease_charge,GC_BUDGET_RELEASE)==GC_BUDGET_OK);
    assert(!h.metadata_pending&&h.metadata_committed==charged-lease_charge.bytes);
    leases.slots=leases.capacity=leases.free_head=0;
    for (unsigned i = 0; i < 8; i++) free((void *)(uintptr_t)pages[i].starts);
    pool_vm_release(mapping, reservation_bytes); // whole test heap, no remaining users
    uint64_t descriptor_bytes=descriptor_charge.bytes,remaining_metadata=h.metadata_committed;
    assert(gc_native_metadata_dispose(&descriptor_charge,&native_metadata)==GC_NATIVE_METADATA_OK);
    assert(publication->budget(&h,&descriptor_charge,GC_BUDGET_RELEASE)==GC_BUDGET_OK);
    assert(h.metadata_committed==remaining_metadata-descriptor_bytes&&!h.metadata_pending);
    puts("root refill: full segment retains live handles while a budgeted successor publishes and retires independently PASS");
    puts("root disposal: active, retiring and cleaning packages reject unmap until registry unlink completes PASS");
    puts("root backing: budgeted native cells and journal retire after all scanner leases PASS");
    puts("initialization root access: chunk boundaries release metadata and reacquire from roots after lease pressure PASS");
    puts("root cancel pipeline: two abandoned refill roots remain null through leased retirement without journal entries PASS");
    puts("descriptor bootstrap: budgeted private registry survives marking and retires after all heap users PASS");
    puts("metadata backing: OS lease storage is budgeted before admission and released after worker retirement PASS");
    puts("initialization metadata wait: two full-pool native waits preserve parent claims and overflow children until capacity returns PASS");
    puts("initialization discovery: two white Old children spill behind claimed parents and finish before worker completion PASS");
    puts("initialization children: rooted nonnull fields yield six duplicate offers with parent claims retained and scalar bits skipped PASS");
    puts("allocation rendezvous: two live owners adopt two new phases after TLAB close without consuming pending journals PASS");
    puts("allocation worker: eight Young/Old jobs cross a registered OS worker while the owner progresses outside guards PASS");
    puts("allocation mark end: retired roots, drained journals and both generation queues permit coordinated Select PASS");
    puts("allocation mark start: two coordinated starts wait for both owner acknowledgements before rooted allocation PASS");
    puts("allocation registration: owner phase adoption, four dual-mark roots and leased retirement complete before detach PASS");
    puts("allocation mark handoff: four OS-backed roots wait before publication and complete guarded Young/Old scans PASS");
    puts("initialization pipeline: four rooted OS-backed allocations publish under real guards and metadata leases PASS");
    puts("page pipeline: real mappings, preparation failures, TLAB rollover, large payload and reserved output roots PASS");
}
#endif
