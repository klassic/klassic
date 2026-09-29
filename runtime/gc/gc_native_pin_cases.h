#ifndef KLASSIC_GC_NATIVE_PIN_CASES_H
#define KLASSIC_GC_NATIVE_PIN_CASES_H
#include "gc_native_pin_progress.h"
#include "gc_native_pin_registry.h"
#include "gc_root_pin_cases.h"
#include "gc_pin_dispatch_cases.h"
#include "gc_worker.h"
#include "gc_pending_mark.h"
#include "gc_root_journal.h"
#include "gc_mark_scan.h"
typedef struct {
    RootPinFixture *owner;MarkRecord record;MarkTask tasks[2];MarkQueue queues[2];
    TypeDescriptor descriptor;_Atomic uint64_t descriptors[2];DescriptorRegistry registry;
} NativePinMark;
static void gc_native_pin_mark_reference(NativePinMark *m,uint64_t reference,uint64_t phase,unsigned generation){
    if(!reference)return;
    RootPinFixture *f=m->owner;MarkQueue *queue=&m->queues[generation];
    PendingMark pending={.context=(uintptr_t)&f->ctx,.current={.reference=reference},
        .queue=(uintptr_t)queue,.generation=generation,.epoch=queue->epoch,.phase=phase};
    unsigned steps=0;
    while(pending.stage!=5){
        assert(++steps<12);
        const uint64_t operations[]={GC_RM_ENTER,GC_RM_ROUTE,GC_RM_EXIT,GC_RM_OFFER,GC_RM_RELEASE};
        assert(pending.stage<5);
        uint64_t status=gc_pending_mark(&f->pool,&pending,operations[pending.stage]);
        assert(status==GC_RAC_READY||status==GC_RM_EXIT_REQUIRED||status==GC_RM_OFFER_REQUIRED||
            status==GC_RM_RELEASE_REQUIRED||status==GC_RM_COMPLETE);
    }
    assert(pending.route_result==(generation==GC_YOUNG&&f->pages[0].generation==GC_OLD?GC_ROUTE_SKIP:GC_ROUTE_READY));
    if(pending.route_result==GC_ROUTE_READY)assert(pending.offer_result==GC_MARK_OK||pending.offer_result==GC_MARK_DUPLICATE);
    assert(!pending.current.lease.token&&!pending.retired.token&&!atomic_load(&f->ctx.access_epoch));
}
static void gc_native_pin_drain_marks(NativePinMark *m,unsigned generation){
    RootPinFixture *f=m->owner;MarkQueue *queue=&m->queues[generation];
    MarkRequest work={0};uint64_t result=gc_mark_step_locked(queue,&work,GC_MARK_TAKE);
    if(result==GC_MARK_EMPTY)return;
    assert(result==GC_MARK_OK);
    MetadataLeaseRequest lease={.token=work.metadata_lease};
    assert(gc_metadata_step_locked(&f->pool,&lease,GC_ML_RESOLVE)==GC_ML_OK);
    ReferenceLease current={work.reference,lease};ResolveRequest access={.scope={.context=(uintptr_t)&f->ctx}};
    assert(gc_pending_access_locked(&f->pool,&current,&access)==GC_RAC_READY);
    uint64_t child=UINT64_MAX;
    MarkScan scan={.job=(uintptr_t)&work,.heap_base=f->heap.heap_base,.registry=(uintptr_t)&m->registry,
        .output=(uintptr_t)&child,.capacity=1};
    assert(gc_mark_scan_slice(&scan)==GC_SCAN_DONE&&!scan.written&&scan.next_cursor==8);
    assert(gc_resolve_access((void *)(uintptr_t)lease.page,&access,GC_RESOLVE_EXIT)==GC_RESOLVE_OK);
    work.cursor=scan.next_cursor;
    assert(gc_mark_step_locked(queue,&work,GC_MARK_FINISH)==GC_MARK_OK);
    assert(gc_metadata_step_locked(&f->pool,&lease,GC_ML_RELEASE)==GC_ML_OK);
    assert(gc_mark_step_locked(queue,NULL,GC_MARK_CHECK)==GC_MARK_EMPTY);
}
/* COPY retains both the ring slots and a counted journal lease until both
 * captured generations have received old/new references and completed work. */
static void gc_native_pin_journal_fixture(RootRegistry *registry,RootRegistration *registration,NativePinMark *m){
    RootLease lease={0};RootChange entries[8];
    RootJournalRequest q={.registration=(uintptr_t)registration,.scope=registration->scope,
        .lease=(uintptr_t)&lease,.output=(uintptr_t)entries,.capacity=8};
    uint64_t result=gc_root_journal_step(registry,&q,GC_RJ_CLAIM);
    if(result==GC_RJ_EMPTY)return;
    assert(result==GC_RJ_OK&&gc_root_journal_step(registry,&q,GC_RJ_COPY)==GC_RJ_OK);
    RootSegment *segment=(void *)(uintptr_t)registration->segment;
    BufferCursor *journal=(void *)(uintptr_t)segment->journal;
    assert(atomic_load(&journal->consumed)==q.begin&&lease.kind==4);
    if(atomic_load(&segment->state)==GC_ROOT_SEGMENT_RETIRING){
        RootLease cleaner={0};RootLifeRequest clean={.registration=q.registration,.scope=q.scope,.lease=(uintptr_t)&cleaner};
        assert(gc_root_life_locked(registry,&clean,GC_RL_CLEAN)==GC_RL_WAIT);
    }
    for(uint64_t i=0;i<q.written;i++)for(unsigned generation=0;generation<2;generation++){
        uint64_t phase=(entries[i].phase>>(4*generation))&15;
        if(phase<GC_MARK_START||phase>GC_MARK_END_CHECK)continue;
        gc_native_pin_mark_reference(m,entries[i].previous,entries[i].phase,generation);
        gc_native_pin_mark_reference(m,entries[i].next,entries[i].phase,generation);
        gc_native_pin_drain_marks(m,generation);
        assert(atomic_load(&journal->consumed)==q.begin&&lease.kind==4&&registry->leases);
    }
    assert(gc_root_journal_step(registry,&q,GC_RJ_ACK)==GC_RJ_OK&&!lease.kind);
    assert(atomic_load(&journal->consumed)==q.end);
}
/* Deleted cells are already null; every captured mark mask still goes through
 * the leased deletion claim before ACK. Journals use the independent consumer. */
static void gc_native_pin_delete_fixture(RootRegistry *registry,RootRegistration *registration){
    if(!registration->limit)return;
    RootLease deletion={0};RootLifeRequest q={.registration=(uintptr_t)registration,
        .scope=registration->scope,.lease=(uintptr_t)&deletion};
    assert(gc_root_life_locked(registry,&q,GC_RL_CLAIM)==GC_RL_OK);
    uint64_t values[2]={UINT64_MAX,UINT64_MAX};
    RootScanJob scan={.scan={.segment=registration->segment,.cursor=q.begin,
        .output=(uintptr_t)values,.capacity=q.end-q.begin},.lease=q.lease,.serial=q.serial};
    assert(gc_root_scan_leased(&scan)==GC_ROOT_OK);
    for(uint64_t i=0;i<scan.scan.written;i++)assert(!values[i]);
    assert(gc_root_life_locked(registry,&q,GC_RL_ACK)==GC_RL_OK);
}
static void gc_native_pin_finish_fixture(RootRegistry *registry,RootRegistration *registration,NativePinMark *marks){
    gc_native_pin_journal_fixture(registry,registration,marks);
    RootLease cleaner={0};RootLifeRequest q={.registration=(uintptr_t)registration,
        .scope=registration->scope,.lease=(uintptr_t)&cleaner};
    assert(gc_root_life_locked(registry,&q,GC_RL_CLEAN)==GC_RL_OK);
    assert(gc_root_clear_chunk(&q)==GC_RL_OK);
    assert(gc_root_life_locked(registry,&q,GC_RL_FINISH)==GC_RL_OK);
}
static bool gc_native_pin_fail_unmap(void *p,size_t bytes){(void)p;(void)bytes;return false;}
typedef struct {
    uint64_t (*publish)(NativePin *,const NativePinStart *);
    uint64_t (*prepare)(NativePin *,PinPageRegistry *,const RootRequest *);
    uint64_t (*retire)(NativePin *);
    uint64_t (*dispose)(NativePin *,const NativeMetadataOps *);
    uint64_t (*release)(NativePin *,HeapPool *);
    uint64_t (*close)(const NativePinRuntime *,NativePin *);
    uint64_t (*worker)(RootRegistry *,WorkerRequest *,uint64_t);
    uint64_t (*progress)(const NativePinRuntime *,NativePin *,WorkerRequest *);
} NativePinOwnerOps;
static inline NativePinOwnerOps gc_native_pin_c_ops(void){
    return (NativePinOwnerOps){gc_native_pin_publish_entry_locked,gc_native_pin_prepare,gc_native_pin_retire_locked,gc_native_pin_dispose,gc_native_pin_release_locked,gc_native_pin_close,gc_worker_step_locked,gc_native_pin_progress_entry};
}
static void gc_native_pin_progress_guard(PinDispatcher dispatch,NativePinOwnerOps owner){
    RootPinFixture f;gc_root_pin_fixture(&f);
    f.ctx=(MutatorContext){.abi_version=GC_ABI_VERSION};
    RootRegistry roots={.phase=0x10000};HandshakeState hs={.last_identity=16};
    ThreadRegistration participant={.context=(uintptr_t)&f.ctx};
    WorkerRequest worker={(uintptr_t)&hs,(uintptr_t)&participant};
    assert(owner.worker(&roots,&worker,GC_WK_ATTACH)==GC_WK_OK);
    PinDispatchLock heap_lock={.context=&f.ctx},registry_lock={.context=&f.ctx};
    assert(!pthread_mutex_init(&heap_lock.mutex,NULL)&&!pthread_mutex_init(&registry_lock.mutex,NULL));
    PinRuntime pins={&f.registry,(uintptr_t)&heap_lock,gc_pin_dispatch_lock,gc_pin_dispatch_unlock};
    NativeMetadataOps native=gc_native_metadata_ops();MetadataCharge charge={.owner=(uintptr_t)&f.heap};
    NativePinRuntime runtime={&pins,&roots,(uintptr_t)&registry_lock,
        gc_pin_dispatch_lock,gc_pin_dispatch_unlock,&f.heap,&native};
    NativePin pin={.registry=(uintptr_t)&roots,.context=(uintptr_t)&f.ctx,.charge=(uintptr_t)&charge,.stage=3};
    pin.self=(uintptr_t)&pin;
    assert(gc_root_pin_prepare(&f.registry,&f.ctx,&f.input,&f.output,&pin.pin)==GC_RPIN_MORE);
    assert(dispatch(&pins,&pin.pin,GC_PIN_STEP)==GC_RPIN_GUARDED);
    assert(gc_handshake_step_locked(&hs,NULL,GC_HS_BEGIN)==GC_HS_OK);
    unsigned locks=heap_lock.locks;
    assert(owner.progress(&runtime,&pin,&worker)==GC_RPIN_MORE);
    assert(!atomic_load(&f.ctx.access_epoch)&&heap_lock.locks==locks&&!registry_lock.locks);
    assert(pin.pin.cancel&&pin.pin.stage==3);
    assert(owner.progress(&runtime,&pin,&worker)==GC_RPIN_WORKER_PARK);
    assert(pin.pin.stage==3&&atomic_load(&f.ctx.state)==GC_AT_SAFEPOINT);
    assert(gc_handshake_step_locked(&hs,NULL,GC_HS_END)==GC_HS_OK);
    atomic_store(&f.ctx.private_producers,1);
    assert(owner.progress(&runtime,&pin,&worker)==GC_RPIN_WORKER_RETRY);
    assert(pin.pin.stage==3);atomic_store(&f.ctx.private_producers,0);
    /* A bounded TLAB close request must not consume or abandon the pin. */
    f.ctx.tlab_start=16;f.ctx.tlab_top=16;f.ctx.tlab_end=32;
    assert(owner.progress(&runtime,&pin,&worker)==GC_RPIN_WORKER_CLOSE_TLAB);
    assert(pin.pin.stage==3);f.ctx.tlab_start=f.ctx.tlab_top=f.ctx.tlab_end=0;
    assert(owner.progress(&runtime,&pin,&worker)==GC_RPIN_MORE);
    assert(pin.pin.stage==5);
    assert(owner.progress(&runtime,&pin,&worker)==GC_RPIN_MORE);
    assert(pin.pin.stage==6&&!pin.pin.address);
    for(unsigned i=0;i<2;i++){
        MetadataLeaseRequest lease=f.records[i].lifetime;
        assert(gc_pin_registry_unbind_locked(&f.registry,&lease)==GC_RG_OK);
    }
    assert(!f.pool.active&&!heap_lock.held&&!registry_lock.held);
    assert(heap_lock.locks==heap_lock.unlocks&&registry_lock.locks==registry_lock.unlocks);
    assert(!pthread_mutex_destroy(&heap_lock.mutex)&&!pthread_mutex_destroy(&registry_lock.mutex));
}
static void gc_run_native_pin_cases(PinDispatcher dispatch,NativePinOwnerOps owner){
    NativeMetadataOps ops=gc_native_metadata_ops();
    gc_native_pin_progress_guard(dispatch,owner);
    for(unsigned marking=0;marking<4;marking++)for(unsigned generation=0;generation<2;generation++)for(unsigned mode=0;mode<5;mode++){
        RootPinFixture f;gc_root_pin_fixture(&f);
        void *payload=aligned_alloc(16,3*GC_HEAP_PAGE_BYTES);assert(payload);
        f.heap.heap_base=(uintptr_t)payload;
        for(unsigned i=0;i<2;i++)f.pages[i].heap_base=(uintptr_t)payload;
        f.pages[0].generation=generation;
        NativePinMark marks={.owner=&f,.record={.object_id=1024},
            .descriptor={.id=1,.alignment=8,.fixed_bytes=8}};
        atomic_store(&marks.descriptors[1],(uintptr_t)&marks.descriptor);
        marks.registry=(DescriptorRegistry){(uintptr_t)marks.descriptors,2};
        for(unsigned i=0;i<2;i++)marks.queues[i]=(MarkQueue){.tasks=(uintptr_t)&marks.tasks[i],
            .capacity=1,.free_head=1,.epoch=5,.generation=i};
        ObjectHeader *header=(void *)((char *)payload+GC_HEAP_PAGE_BYTES);
        *header=(ObjectHeader){.payload_bytes=8,.descriptor_id=1,.mark_record=(uintptr_t)&marks.record,.state=GC_OBJECT_PUBLISHED};
        *(uint64_t *)((char *)payload+GC_HEAP_PAGE_BYTES+32)=77;
        f.ctx=(MutatorContext){.abi_version=GC_ABI_VERSION};
        RootRegistry registry={.phase=0x10000|((marking&1)?GC_CONCURRENT_MARK:0)|((marking&2)?(GC_CONCURRENT_MARK<<4):0)};
        HandshakeState hs={.last_identity=16};
        ThreadRegistration participant={.context=(uintptr_t)&f.ctx};
        WorkerRequest worker={(uintptr_t)&hs,(uintptr_t)&participant};
        assert(gc_worker_step_locked(&registry,&worker,GC_WK_ATTACH)==GC_WK_OK);
        /* Register the caller's distinct scope before crossing any boundary. */
        f.roots.scope=0;atomic_store(&f.roots.state,GC_ROOT_SEGMENT_PREPARED);atomic_store(&f.roots.used,0);
        memset(f.cells,0,sizeof(f.cells));f.journal=(BufferCursor){.entries=(uintptr_t)f.changes,.capacity=8};
        RootRegistration caller={.segment=(uintptr_t)&f.roots};
        RootLifeRequest caller_life={.registration=(uintptr_t)&caller,.context=(uintptr_t)&f.ctx};
        assert(gc_root_life_locked(&registry,&caller_life,GC_RL_ATTACH)==GC_RL_OK);
        f.input=(RootRequest){.segment=(uintptr_t)&f.roots};f.output=f.input;
        assert(gc_root_step(&f.ctx,&f.input,GC_ROOT_RESERVE)==GC_ROOT_OK);
        assert(gc_root_step(&f.ctx,&f.output,GC_ROOT_RESERVE)==GC_ROOT_OK);
        f.input.value=((GC_HEAP_PAGE_BYTES+32)<<16)|0x1510;
        assert(gc_root_step(&f.ctx,&f.input,GC_ROOT_STORE)==GC_ROOT_OK);
        for(unsigned i=0;i<2;i++){
            MetadataLeaseRequest lease=f.records[i].lifetime;
            assert(gc_pin_registry_unbind_locked(&f.registry,&lease)==GC_RG_OK);
        }
        uint64_t bytes=0,inventory_bytes=0;
        assert(gc_native_roots_size(1,8,ops.quantum,&bytes)==GC_NATIVE_METADATA_OK);
        assert(gc_native_pin_registry_size(2,ops.quantum,&inventory_bytes)==GC_NATIVE_METADATA_OK);
        f.heap.metadata_committed=sizeof(f.pages);f.heap.metadata_max=sizeof(f.pages)+bytes+inventory_bytes;
        MetadataCharge charge={.bytes=bytes},inventory_charge={.bytes=inventory_bytes};
        assert(gc_budget_step_locked(&f.heap,&charge,GC_BUDGET_RESERVE)==GC_BUDGET_OK);
        assert(gc_budget_step_locked(&f.heap,&inventory_charge,GC_BUDGET_RESERVE)==GC_BUDGET_OK);
        assert(gc_worker_step_locked(&registry,&worker,GC_WK_ENTER_NATIVE)==GC_WK_OK);
        assert(gc_native_roots_prepare(&charge,17,1,8,&ops,gc_root_storage_prepare)==GC_NATIVE_METADATA_OK);
        assert(gc_native_pin_registry_prepare(&inventory_charge,&f.pool,&ops)==GC_NATIVE_METADATA_OK);
        assert(gc_budget_step_locked(&f.heap,&charge,GC_BUDGET_COMMIT)==GC_BUDGET_OK);
        assert(gc_budget_step_locked(&f.heap,&inventory_charge,GC_BUDGET_COMMIT)==GC_BUDGET_OK);
        assert(gc_worker_step_locked(&registry,&worker,GC_WK_LEAVE_NATIVE)==GC_WK_OK);
        PinPageRegistry *inventory=gc_native_pin_registry_committed(&inventory_charge);assert(inventory);
        for(unsigned i=0;i<2;i++){
            MetadataLeaseRequest lease={(uintptr_t)&f.pages[i],f.pages[i].incarnation,0};
            assert(gc_metadata_step_locked(&f.pool,&lease,GC_ML_ACQUIRE)==GC_ML_OK);
            assert(gc_pin_registry_bind_locked(inventory,&lease)==GC_RG_OK);
            assert(gc_metadata_step_locked(&f.pool,&lease,GC_ML_RELEASE)==GC_ML_OK);
        }
        PinDispatchLock pin_lock={.context=&f.ctx};assert(!pthread_mutex_init(&pin_lock.mutex,NULL));
        PinRuntime pin_runtime={inventory,(uintptr_t)&pin_lock,gc_pin_dispatch_lock,gc_pin_dispatch_unlock};
        PinDispatchLock registry_lock={.context=&f.ctx};
        assert(!pthread_mutex_init(&registry_lock.mutex,NULL));
        NativePinRuntime runtime={&pin_runtime,&registry,(uintptr_t)&registry_lock,
            gc_pin_dispatch_lock,gc_pin_dispatch_unlock,&f.heap,&ops};
        NativePin pin={0};
        NativePinStart start={(uintptr_t)&registry,(uintptr_t)&f.ctx,(uintptr_t)&charge};
        assert(owner.publish(NULL,&start)==GC_RPIN_INVALID);
        assert(owner.publish(&pin,NULL)==GC_RPIN_INVALID);
        assert(owner.publish(&pin,&start)==GC_RPIN_MORE);
        NativePin snapshot=pin;
        assert(owner.publish(&pin,&start)==GC_RPIN_INVALID);
        assert(!memcmp(&pin,&snapshot,sizeof(pin)));
        RootStorage *storage=(void *)(uintptr_t)charge.allocation;
        RootRequest same_segment={.segment=pin.reserved.segment};
        assert(owner.prepare(&pin,inventory,&same_segment)==GC_RPIN_INVALID);
        charge.allocation=0;
        assert(owner.prepare(&pin,inventory,&f.input)==GC_RPIN_INVALID);
        charge.allocation=(uintptr_t)storage;
        assert(!memcmp(&pin,&snapshot,sizeof(pin)));
        NativePin copied=pin;
        assert(owner.prepare(&copied,inventory,&f.input)==GC_RPIN_INVALID);
        if(mode==1||mode==4){
            storage->journal.top=8;atomic_store(&storage->journal.published,8);
            assert(owner.prepare(&pin,inventory,&f.input)==GC_RPIN_FLUSH&&pin.stage==2);
            if(mode==1)atomic_store(&storage->journal.consumed,8);
        }
        if(mode<3){
            assert(owner.prepare(&pin,inventory,&f.input)==GC_RPIN_MORE);
            assert(dispatch(&pin_runtime,&pin.pin,GC_PIN_STEP)==GC_RPIN_GUARDED);
            assert(dispatch(&pin_runtime,&pin.pin,GC_PIN_STEP)==GC_RPIN_READY);
            assert(owner.retire(&pin)==GC_RPIN_WAIT);
            assert(owner.dispose(&pin,&ops)==GC_RPIN_INVALID);
            gc_native_pin_journal_fixture(&registry,&storage->registration,&marks);
            assert(marks.record.young_epoch==((marking&1)&&generation==GC_YOUNG?5:0));
            assert(marks.record.old_epoch==((marking&2)?5:0));
        }
        assert(gc_root_step(&f.ctx,&f.input,GC_ROOT_RELEASE)==GC_ROOT_OK);
        assert(gc_root_cancel_empty(&f.ctx,&f.output)==GC_ROOT_OK);
        assert(gc_root_life_locked(&registry,&caller_life,GC_RL_RETIRE)==GC_RL_OK);
        gc_native_pin_delete_fixture(&registry,&caller);gc_native_pin_finish_fixture(&registry,&caller,&marks);
        assert(f.ctx.root_segments==(uintptr_t)&storage->registration&&registry.active==1);
        RootLease reader={0};RootLifeRequest reading={.registration=(uintptr_t)&storage->registration,
            .scope=storage->segment.scope,.lease=(uintptr_t)&reader};
        assert(gc_root_life_locked(&registry,&reading,GC_RL_ACQUIRE)==GC_RL_OK);
        if(mode<3){
            assert(gc_worker_step_locked(&registry,&worker,GC_WK_ENTER_NATIVE)==GC_WK_OK);
            uint64_t value=0;RootScanJob scan={.scan={.segment=(uintptr_t)&storage->segment,
                .output=(uintptr_t)&value,.capacity=1},.lease=reading.lease,.serial=reading.serial};
            assert(gc_root_scan_leased(&scan)==GC_ROOT_OK&&value==pin.pin.reference);
            assert(hs.native==1&&!atomic_load(&f.ctx.access_epoch));
            assert(gc_worker_step_locked(&registry,&worker,GC_WK_LEAVE_NATIVE)==GC_WK_OK);
            (*(uint64_t *)(uintptr_t)pin.pin.address)++;
            assert(*(uint64_t *)(uintptr_t)pin.pin.address==78);
            assert(owner.close(&runtime,&pin)==GC_RPIN_MORE);
            assert(owner.retire(&pin)==GC_RPIN_WAIT);
            assert(owner.close(&runtime,&pin)==GC_RPIN_MORE&&pin.pin.stage==6);
        }
        gc_native_pin_journal_fixture(&registry,&storage->registration,&marks);
        assert(gc_worker_step_locked(&registry,&worker,GC_WK_ENTER_NATIVE)==GC_WK_OK);
        unsigned locks=registry_lock.locks;
        assert(owner.close(&runtime,&pin)==GC_RPIN_RESUME&&registry_lock.locks==locks);
        registry.phase+=0x10000;
        assert(gc_handshake_step_locked(&hs,NULL,GC_HS_BEGIN)==GC_HS_OK);
        assert(owner.progress(&runtime,&pin,&worker)==GC_RPIN_WORKER_RETRY);
        assert(atomic_load(&f.ctx.state)==GC_BLOCKED_NATIVE&&pin.stage<4);
        assert(gc_handshake_step_locked(&hs,NULL,GC_HS_END)==GC_HS_OK);
        assert(owner.progress(&runtime,&pin,&worker)==GC_RPIN_MORE);
        assert(atomic_load(&f.ctx.state)==GC_RUNNING);
        registry.phase+=0x10000;
        assert(gc_handshake_step_locked(&hs,NULL,GC_HS_BEGIN)==GC_HS_OK);
        assert(owner.progress(&runtime,&pin,&worker)==GC_RPIN_WORKER_PARK);
        assert(atomic_load(&f.ctx.state)==GC_AT_SAFEPOINT&&pin.stage<4);
        assert(owner.progress(&runtime,&pin,&worker)==GC_RPIN_WORKER_PARK);
        assert(gc_handshake_step_locked(&hs,NULL,GC_HS_END)==GC_HS_OK);
        assert(owner.progress(&runtime,&pin,&worker)==GC_RPIN_MORE&&pin.stage==4);
        assert(owner.close(&runtime,&pin)==GC_RPIN_NATIVE);
        assert(owner.dispose(&pin,&ops)==GC_RPIN_INVALID);
        assert(owner.progress(&runtime,&pin,&worker)==GC_RPIN_MORE);
        assert(atomic_load(&f.ctx.state)==GC_BLOCKED_NATIVE);
        assert(owner.progress(&runtime,&pin,&worker)==GC_RPIN_WAIT);
        assert(gc_worker_step_locked(&registry,&worker,GC_WK_LEAVE_NATIVE)==GC_WK_OK);
        gc_native_pin_delete_fixture(&registry,&storage->registration);
        RootLease blocked={0};RootLifeRequest clean={.registration=reading.registration,
            .scope=reading.scope,.lease=(uintptr_t)&blocked};
        assert(gc_root_life_locked(&registry,&clean,GC_RL_CLEAN)==GC_RL_WAIT);
        assert(gc_root_life_locked(&registry,&reading,GC_RL_RELEASE)==GC_RL_OK);
        gc_native_pin_finish_fixture(&registry,&storage->registration,&marks);
        assert(marks.record.young_epoch==((marking&1)&&generation==GC_YOUNG?5:0));
        assert(marks.record.old_epoch==((marking&2)?5:0));
        assert(!registry.inventory&&!registry.active&&!registry.retiring&&!registry.leases);
        assert(!f.ctx.root_segments&&!f.ctx.retiring_roots);
        assert(gc_worker_step_locked(&registry,&worker,GC_WK_ENTER_NATIVE)==GC_WK_OK);
        NativeMetadataOps bad_ops=ops;bad_ops.quantum=3;
        assert(owner.dispose(&pin,&bad_ops)==GC_RPIN_INVALID);
        bad_ops=ops;bad_ops.unmap=NULL;
        assert(owner.dispose(&pin,&bad_ops)==GC_RPIN_INVALID);
        assert(pin.storage==(uintptr_t)storage&&charge.allocation==(uintptr_t)storage);
        if(mode==2){
            NativeMetadataOps fail=ops;fail.unmap=gc_native_pin_fail_unmap;
            MetadataCharge before=charge;uint64_t address=pin.storage;
            runtime.native=&fail;
            assert(owner.progress(&runtime,&pin,&worker)==GC_RPIN_CLEANUP);
            runtime.native=&ops;
            assert(pin.storage==address&&!memcmp(&charge,&before,sizeof(charge)));
        }
        assert(owner.release(&pin,&f.heap)==GC_RPIN_INVALID);
        assert(owner.progress(&runtime,&pin,&worker)==GC_RPIN_MORE&&!charge.allocation&&pin.stage==5);
        assert(owner.dispose(&pin,&ops)==GC_RPIN_INVALID);
        NativePin saved_pin=pin;MetadataCharge saved_charge=charge;HeapPool saved_heap=f.heap;
        f.heap.metadata_committed=sizeof(f.pages);
        assert(owner.release(&pin,&f.heap)==GC_RPIN_INVALID);
        assert(!memcmp(&pin,&saved_pin,sizeof(pin))&&!memcmp(&charge,&saved_charge,sizeof(charge)));
        assert(f.heap.metadata_committed==sizeof(f.pages));f.heap=saved_heap;
        assert(owner.progress(&runtime,&pin,&worker)==GC_RPIN_DONE&&pin.stage==6&&charge.stage==3);
        unsigned heap_locks=pin_lock.locks;
        assert(owner.progress(&runtime,&pin,NULL)==GC_RPIN_DONE&&pin_lock.locks==heap_locks);
        saved_heap=f.heap;
        assert(owner.release(&pin,&f.heap)==GC_RPIN_INVALID);
        assert(!memcmp(&saved_heap,&f.heap,sizeof(f.heap)));
        assert(f.heap.metadata_committed==sizeof(f.pages)+inventory_bytes);
        for(unsigned i=0;i<2;i++){
            MetadataLeaseRequest lease=inventory->records[i].lifetime;
            assert(gc_pin_registry_unbind_locked(inventory,&lease)==GC_RG_OK);
        }
        assert(gc_native_pin_registry_dispose(&inventory_charge,&ops)==GC_NATIVE_METADATA_OK);
        assert(gc_budget_step_locked(&f.heap,&inventory_charge,GC_BUDGET_RELEASE)==GC_BUDGET_OK);
        assert(f.heap.metadata_committed==sizeof(f.pages));
        assert(gc_worker_step_locked(&registry,&worker,GC_WK_LEAVE_NATIVE)==GC_WK_OK);
        atomic_store(&f.ctx.state,GC_DETACHING);
        assert(gc_handshake_step_locked(&hs,&participant,GC_HS_DETACH)==GC_HS_OK&&!hs.members);
        assert(!f.pool.active);free(payload);
        assert(!pin_lock.held&&pin_lock.locks==pin_lock.unlocks);
        assert(!pthread_mutex_destroy(&pin_lock.mutex));
        assert(!registry_lock.held&&registry_lock.locks==registry_lock.unlocks&&registry_lock.locks>1);
        assert(!pthread_mutex_destroy(&registry_lock.mutex));
    }
    puts("native pin progress: 40 worker-bound shutdowns, native retry, phase park and resume PASS");
    puts("native pin close: 40 package shutdowns, boundary requests, balanced locks and retry ownership PASS");
    puts("native pin budget: disposal-gated release, failed credit retry and terminal replay rejection PASS");
    puts("native pin disposal: blocked-only release, live-reader exclusion and OS retry preserve ownership PASS");
    puts("native pin package: registered publication, root reservation retries and exclusive retirement PASS");
    puts("native pin owner: registered roots survive caller retirement, worker native entry, delayed readers and unmap retry PASS");
    puts("native pin mark handoff: 40 young/old/overlapping phase lifecycles retain journals through guarded marking PASS");
    puts("native pin driver: 24 acquired-package lifecycles use mutex dispatch PASS");
    puts("native pin inventory: 40 package lifecycles use committed mapped page registries PASS");
}
#endif
