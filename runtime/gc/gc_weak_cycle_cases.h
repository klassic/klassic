#ifndef KLASSIC_GC_WEAK_CYCLE_CASES_H
#define KLASSIC_GC_WEAK_CYCLE_CASES_H
#include "gc_weak_scan_concurrency.h"
#include "gc_pending_mark.h"
#include "gc_root_journal.h"
#include "gc_mark_scan.h"
#include "gc_weak_relocation.h"
typedef struct {
    WeakProcessOps process;WeakTableOps table;WeakMarkStepper owner;PendingMarker pending;
    uint64_t (*journal)(RootRegistry *,RootJournalRequest *,uint64_t);
    uint64_t (*mark)(MarkQueue *,MarkRequest *,uint64_t);
    uint64_t (*scan)(MarkScan *);
    uint64_t (*audit)(RootRegistry *,RootAudit *,uint64_t);
    uint64_t (*walk)(RootWalkScan *);
    uint64_t (*descriptor)(const DescriptorRegistry *,const TypeDescriptor *);
    RelocationPhaseStep relocate;WeakProcessAccess access;WeakProcessFinish finish;WeakScanStepper weak_scan;WeakRelocationBegin weak_relocate;
} WeakCycleOps;
/* The load/end actors have joined before draining: controls are exclusively
 * owned here. Journal/walk leases remain counted until the actual handoff.
 * Records start unmarked; only mark OFFER updates the selected stamp. */
static void weak_cycle_mark_reference(WeakScanFixture *f,uint64_t reference,const WeakCycleOps *ops){
    if(!reference)return;
    AuditFixture *a=&f->r.phase.e.f;MetadataLeasePool *pool=&f->r.weak.f.pool;
    PendingMark q={.context=(uintptr_t)&a->ctx[0],.current={.reference=reference},.queue=(uintptr_t)&a->queue,
        .generation=a->queue.generation,.epoch=a->queue.epoch,.phase=a->g.phase};
    unsigned steps=0;
    while(q.stage!=5){
        assert(++steps<12);uint64_t operation;
        switch(q.stage){case 0:operation=GC_RM_ENTER;break;case 1:operation=GC_RM_ROUTE;break;
        case 2:operation=GC_RM_EXIT;break;case 3:operation=GC_RM_OFFER;break;case 4:operation=GC_RM_RELEASE;break;default:abort();}
        uint64_t result=ops->pending(pool,&q,operation);
        assert(result==GC_RF_ADVANCED||result==GC_RAC_READY||result==GC_RM_EXIT_REQUIRED||
            result==GC_RM_OFFER_REQUIRED||result==GC_RM_RELEASE_REQUIRED||result==GC_RM_COMPLETE);
    }
    assert(q.route_result==GC_ROUTE_READY&&(q.offer_result==GC_MARK_OK||q.offer_result==GC_MARK_DUPLICATE));
    assert(!q.current.lease.token&&!q.retired.token&&!atomic_load(&a->ctx[0].access_epoch));
}
static unsigned weak_cycle_drain_queue(WeakScanFixture *f,const DescriptorRegistry *registry,const WeakCycleOps *ops){
    AuditFixture *a=&f->r.phase.e.f;MetadataLeasePool *pool=&f->r.weak.f.pool;unsigned scanned=0;
    for(;;){
        MarkRequest work={0};uint64_t result=ops->mark(&a->queue,&work,GC_MARK_TAKE);
        if(result==GC_MARK_EMPTY)break;
        assert(result==GC_MARK_OK&&++scanned<=1);
        MetadataLeaseRequest lease={.token=work.metadata_lease};
        assert(ops->process.metadata(pool,&lease,GC_ML_RESOLVE)==GC_ML_OK&&lease.incarnation==work.incarnation);
        ReferenceLease current={work.reference,lease};ResolveRequest access={.scope={.context=(uintptr_t)&a->ctx[0]}};
        assert(ops->process.access(pool,&current,&access)==GC_RAC_READY);
        uint64_t child=UINT64_MAX;
        MarkScan scan={(uintptr_t)&work,f->r.weak.f.heap.heap_base,(uintptr_t)registry,(uintptr_t)&child,1,0,0,0,0};
        assert(ops->scan(&scan)==GC_SCAN_DONE&&!scan.written&&scan.next_cursor==24);
        assert(ops->process.resolve((void *)(uintptr_t)lease.page,&access,GC_RESOLVE_EXIT)==GC_RESOLVE_OK);
        work.cursor=scan.next_cursor;
        assert(ops->mark(&a->queue,&work,GC_MARK_FINISH)==GC_MARK_OK);
        assert(work.metadata_lease==lease.token&&ops->process.metadata(pool,&lease,GC_ML_RELEASE)==GC_ML_OK);
    }
    assert(ops->mark(&a->queue,NULL,GC_MARK_CHECK)==GC_MARK_EMPTY);return scanned;
}
static unsigned weak_cycle_drain_journal(WeakScanFixture *f,const DescriptorRegistry *registry,const WeakCycleOps *ops){
    AuditFixture *a=&f->r.phase.e.f;RootLease lease={0};RootChange copied={0};
    RootJournalRequest request={.registration=(uintptr_t)&a->r[1],.scope=a->s[1].scope,
        .lease=(uintptr_t)&lease,.output=(uintptr_t)&copied,.capacity=1};
    assert(ops->journal(&a->g,&request,GC_RJ_CLAIM)==GC_RJ_OK);
    uint64_t consumed=atomic_load(&a->buffers[1].consumed);
    assert(ops->journal(&a->g,&request,GC_RJ_COPY)==GC_RJ_OK&&request.written==1);
    assert(lease.kind==4&&a->g.leases&&atomic_load(&a->buffers[1].consumed)==consumed);
    unsigned phase=(unsigned)((copied.phase>>(4*a->queue.generation))&15);
    assert(phase>=GC_MARK_START&&phase<=GC_MARK_END_CHECK);
    weak_cycle_mark_reference(f,copied.previous,ops);weak_cycle_mark_reference(f,copied.next,ops);
    unsigned scanned=weak_cycle_drain_queue(f,registry,ops);
    assert(lease.kind==4&&a->g.leases&&atomic_load(&a->buffers[1].consumed)==consumed);
    assert(ops->journal(&a->g,&request,GC_RJ_ACK)==GC_RJ_OK&&!lease.kind&&!a->g.leases);
    assert(atomic_load(&a->buffers[1].consumed)==request.end);return scanned;
}
static void weak_cycle_audit_roots(WeakScanFixture *f,const DescriptorRegistry *registry,const WeakCycleOps *ops){
    AuditFixture *a=&f->r.phase.e.f;uint64_t result=ops->audit(&a->g,&a->a,GC_RA_BEGIN);unsigned roots=0;
    while(result==GC_RA_ITEM){
        assert(++roots<=3);uint64_t value=0;
        RootWalkScan scan={(uintptr_t)&a->a.walk,a->a.walk.lease.serial,(uintptr_t)&value,1,0};
        assert(ops->walk(&scan)==0&&scan.written==1);
        weak_cycle_mark_reference(f,value,ops);assert(!weak_cycle_drain_queue(f,registry,ops));
        a->a.walk.completed_mask=a->a.walk.mask;
        result=ops->audit(&a->g,&a->a,GC_RA_NEXT);
    }
    assert(result==GC_RA_DONE&&roots==3&&!a->g.leases&&!a->g.walkers);
}
static void gc_run_weak_cycle_cases(const WeakCycleOps *candidate){
    const WeakCycleOps c={{gc_reference_lease_locked,gc_pending_follow_locked,gc_pending_access_locked,
        gc_resolve_access,gc_guarded_weak_liveness,gc_metadata_step_locked},
        {gc_root_step,gc_reference_lease_locked,gc_weak_load_into_locked,gc_weak_release_locked},
        gc_weak_mark_step_locked,gc_pending_mark,gc_root_journal_step,gc_mark_step_locked,gc_mark_scan_slice,
        gc_root_audit_step_locked,gc_root_walk_scan,gc_descriptor_register,gc_relocation_phase_step_locked,gc_weak_process_access_step,gc_weak_process_finish_locked,gc_weak_scan_step_locked,gc_weak_relocation_begin_locked};
    for(unsigned impl=0;impl<2;impl++)for(unsigned generation=0;generation<2;generation++)
    for(unsigned moved=0;moved<2;moved++)for(unsigned mode=0;mode<3;mode++){
        const WeakCycleOps *ops=impl?candidate:&c;WeakScanFixture f;
        weak_scan_fixture_phase(&f,generation,moved,0x90000|(generation?0x20:2),false);
        WeakMarkRace *r=&f.r;AuditFixture *a=&r->phase.e.f;
        assert(!f.records[0].young_epoch&&!f.records[0].old_epoch);
        _Atomic uint64_t descriptors[2]={0};DescriptorRegistry registry={(uintptr_t)descriptors,2};
        TypeDescriptor scalar={.id=1,.alignment=16,.fixed_bytes=24};assert(ops->descriptor(&registry,&scalar)==GC_DESC_OK);
        r->owner_step=ops->owner;r->load=ops->table.load;r->load_first=mode!=0;
        r->admission.participant=(uintptr_t)&r->phase.e.p[1];
        r->output=(RootRequest){.segment=(uintptr_t)&a->s[1],.handle={(uintptr_t)&a->cells[1],1,a->s[1].scope}};
        r->request.context=(uintptr_t)&a->ctx[1];r->request.slot=(uintptr_t)&f.entries[0].slot;
        WeakMarkActor actors[]={{r,1},{r,0}};
#ifdef _WIN32
        InitializeCriticalSection(&r->lock);HANDLE threads[2];
        for(unsigned i=0;i<2;i++){threads[i]=CreateThread(NULL,0,weak_mark_actor,&actors[i],0,NULL);assert(threads[i]);}
        for(unsigned i=0;i<2;i++){assert(WaitForSingleObject(threads[i],30000)==WAIT_OBJECT_0);assert(CloseHandle(threads[i]));}
        DeleteCriticalSection(&r->lock);
#else
        assert(!pthread_mutex_init(&r->lock,NULL));pthread_t threads[2];
        for(unsigned i=0;i<2;i++)assert(!pthread_create(&threads[i],NULL,weak_mark_actor,&actors[i]));
        for(unsigned i=0;i<2;i++)assert(!pthread_join(threads[i],NULL));assert(!pthread_mutex_destroy(&r->lock));
#endif
        assert(r->begun==GC_MP_ADVANCED&&r->loaded==(mode?GC_WEAK_LOADED:GC_WEAK_WAIT));
        phase_ack(&r->phase);
        assert(ops->owner(&a->g,&r->owner,GC_MP_DECIDE)==GC_MP_ADVANCED);
        assert(r->phase.q.outcome==(mode?GC_CONCURRENT_MARK:GC_SELECT));
        phase_ack(&r->phase);
        assert(ops->owner(&a->g,&r->owner,GC_MP_RELEASE)==(mode?GC_MP_RETRY:GC_MP_SELECT));phase_resume(&r->phase);
        if(mode){
            assert(!r->weak.gate.closed&&atomic_load(&a->buffers[1].published)==1&&!atomic_load(&a->buffers[1].consumed));
            assert(weak_cycle_drain_journal(&f,&registry,ops)==1);
            assert((generation?f.records[0].old_epoch:f.records[0].young_epoch)==a->queue.epoch);
            assert((generation?f.records[0].young_epoch:f.records[0].old_epoch)==0);
            if(mode==2){
                r->output.value=0;assert(ops->table.root(&a->ctx[1],&r->output,GC_ROOT_STORE)==GC_ROOT_OK);
                assert(!weak_cycle_drain_journal(&f,&registry,ops));
                assert(!atomic_load(&a->cells[1].value)&&atomic_load(&a->buffers[1].consumed)==2);
            }
            weak_cycle_audit_roots(&f,&registry,ops);
            assert(ops->owner(&a->g,&r->owner,GC_MP_BEGIN)==GC_MP_ADVANCED);phase_ack(&r->phase);
            assert(ops->owner(&a->g,&r->owner,GC_MP_DECIDE)==GC_MP_ADVANCED);
            assert(r->phase.q.outcome==GC_SELECT);phase_ack(&r->phase);
            assert(ops->owner(&a->g,&r->owner,GC_MP_RELEASE)==GC_MP_SELECT);phase_resume(&r->phase);
        }
        assert(r->weak.gate.closed&&!f.records[1].young_epoch&&!f.records[1].old_epoch);
        WeakScan scan={0};WeakScanStart start={&f.table,&r->owner,&a->ctx[0]};
        assert(ops->weak_scan(&scan,&start,GC_WS_BEGIN)==GC_WP_OK);
        RelocationGate relocation_gate={.epoch=8,.closed=1};
        RelocationPhaseControl relocation={.handshake=(uintptr_t)&r->phase.e.h,.gate=(uintptr_t)&relocation_gate,.generation=generation};
        uint64_t selected=a->g.phase;
        assert(ops->weak_relocate(&scan,&a->g,&relocation)==GC_RP_WAIT);
        assert(a->g.phase==selected&&!relocation.state&&r->weak.gate.closed);
        unsigned steps=0;
        for(;;){
            assert(++steps<16);uint64_t status=ops->weak_scan(&scan,NULL,GC_WS_NEXT);
            if(status==GC_WS_DONE)break;if(status==GC_WP_ADVANCED)continue;assert(status==GC_WP_OK);
            unsigned hops=0;do{assert(++hops<4);status=ops->access(&scan.work,GC_WPA_ENTER,0);}while(status==GC_WP_ADVANCED);
            assert(status==GC_WP_OK);uint64_t result=ops->access(&scan.work,GC_WPA_OBSERVE,0);
            assert(result==(scan.cursor==0&&mode?GC_WP_LIVE:GC_WP_DEAD));
            assert(ops->access(&scan.work,GC_WPA_EXIT,0)==GC_WP_OK);
            assert(ops->finish(&scan.work,GC_WPF_COMMIT,0)==result);
        }
        for(unsigned fault=0;fault<16;fault++){
            WeakScanFixture original=f;WeakScan saved_scan=scan;
            RelocationPhaseControl saved_relocation=relocation;RelocationGate saved_gate=relocation_gate;
            WeakScan *input=&scan;RootRegistry *registry=&a->g;RelocationPhaseControl *target=&relocation;
            uint64_t expected=GC_RP_INVALID;
            switch(fault){
            case 0:registry=NULL;break;case 1:target=NULL;break;case 2:input=NULL;break;
            case 3:registry=(void *)((uintptr_t)registry+8);break;
            case 4:relocation.handshake+=8;break;case 5:relocation.generation=1-generation;break;
            case 6:scan.stage=1;expected=GC_RP_WAIT;break;case 7:scan.cursor--;break;
            case 8:r->owner.processor=(uintptr_t)&scan.work;expected=GC_RP_WAIT;break;
            case 9:scan.work.result=1;break;case 10:relocation.state=1;break;
            case 11:relocation_gate.closed=0;expected=GC_RP_WAIT;break;
            case 12:relocation_gate.epoch=UINT64_MAX;expected=GC_RP_EXHAUSTED;break;
            case 13:r->owner.scan=0;break;case 14:atomic_store(&a->ctx[0].adopted_phase,0);break;
            case 15:scan.work.current.reference=1;break;
            }
            WeakScanFixture before=f;WeakScan before_scan=scan;
            RelocationPhaseControl before_relocation=relocation;RelocationGate before_gate=relocation_gate;
            assert(ops->weak_relocate(input,registry,target)==expected);
            assert(!memcmp(&before,&f,sizeof(f))&&!memcmp(&before_scan,&scan,sizeof(scan)));
            assert(!memcmp(&before_relocation,&relocation,sizeof(relocation))&&!memcmp(&before_gate,&relocation_gate,sizeof(relocation_gate)));
            f=original;scan=saved_scan;relocation=saved_relocation;relocation_gate=saved_gate;
        }
        /* Gate pressure cannot reopen weak admission or consume coverage. */
        relocation_gate.closed=0;
        assert(ops->weak_relocate(&scan,&a->g,&relocation)==GC_RP_WAIT);
        assert(scan.stage==2&&r->weak.gate.closed&&a->g.phase==selected&&!relocation.state);
        relocation_gate.closed=1;relocation.generation=1-generation;
        assert(ops->weak_relocate(&scan,&a->g,&relocation)==GC_RP_INVALID);
        relocation.generation=generation;r->phase.q.epoch++;
        assert(ops->weak_relocate(&scan,&a->g,&relocation)==GC_RP_INVALID);
        r->phase.q.epoch--;
        assert(ops->weak_relocate(&scan,&a->g,&relocation)==GC_RP_ADVANCED);
        assert(scan.stage==5&&!r->owner.scan&&!r->owner.processor&&!r->weak.gate.closed&&r->phase.e.h.active);
        assert(ops->weak_relocate(&scan,&a->g,&relocation)==GC_RP_INVALID);
        RootRequest waiting={.segment=(uintptr_t)&a->s[0],.handle={(uintptr_t)&a->cells[0],1,a->s[0].scope}};
        RootedLoadAdmission wa={(uintptr_t)&a->g,(uintptr_t)&r->phase.e.h,(uintptr_t)&r->phase.e.p[0],{0,0}};
        WeakHandle wh={(uintptr_t)&f.table,1,11};
        assert(gc_weak_table_load_locked(&f.table,wh,&wa,&a->ctx[0],&waiting,&ops->table)==GC_WEAK_WAIT);
        assert(!waiting.value&&!atomic_load(&a->cells[0].value));
        phase_ack(&r->phase);
        assert(ops->relocate(&a->g,&relocation,GC_RP_ACTIVATE)==GC_RP_ADVANCED);
        phase_ack(&r->phase);
        assert(ops->relocate(&a->g,&relocation,GC_RP_RELEASE)==GC_RP_RELOCATING);phase_resume(&r->phase);
        assert(!relocation_gate.closed&&relocation_gate.epoch==9);

        RootRequest out={.segment=(uintptr_t)&a->s[0],.handle={(uintptr_t)&a->cells[0],1,a->s[0].scope}};
        RootedLoadAdmission admission={(uintptr_t)&a->g,(uintptr_t)&r->phase.e.h,(uintptr_t)&r->phase.e.p[0],{0,0}};
        WeakHandle dead={(uintptr_t)&f.table,2,12},target={(uintptr_t)&f.table,1,11};
        assert(gc_weak_table_load_locked(&f.table,dead,&admission,&a->ctx[0],&out,&ops->table)==GC_WEAK_EMPTY);
        assert(gc_weak_table_load_locked(&f.table,target,&admission,&a->ctx[0],&out,&ops->table)==(mode?GC_WEAK_LOADED:GC_WEAK_EMPTY));
        assert(out.value==(mode?f.live:0)&&atomic_load(&a->buffers[0].published)==(mode!=0));
        for(unsigned i=0;i<4;i++)if(f.entries[i].slot.serial){WeakHandle h={(uintptr_t)&f.table,i+1,f.entries[i].slot.serial};assert(gc_weak_table_release_locked(&f.table,h,&ops->table)==GC_WEAK_RELEASED);}
        assert(!r->weak.f.pool.active&&!r->weak.f.page.metadata_leases&&!a->g.leases&&!a->queue.queued&&!a->queue.in_flight);
        free(f.heap);
    }
    puts("weak relocation: 384 C/emitted rejected or deferred handoffs preserve state PASS");
    puts("weak mark-to-relocate: 24 C/emitted-owner journal, mark, weak coverage and atomic relocation-adoption lifecycles PASS");
}
#endif
