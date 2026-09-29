#ifndef KLASSIC_GC_WEAK_SCAN_CASES_H
#define KLASSIC_GC_WEAK_SCAN_CASES_H
#include "gc_weak_scan.h"
#include "gc_weak_process_cases.h"
typedef struct {
    WeakMarkRace r;WeakTable table;WeakEntry entries[4];MetadataLeaseSlot leases[8];
    ForwardCell forward[3];MarkRecord records[2];unsigned char *heap;uint64_t live;
} WeakScanFixture;
static void weak_scan_fixture_phase(WeakScanFixture *f,unsigned generation,bool moved,uint64_t phase,bool completed){
    memset(f,0,sizeof(*f));weak_mark_fixture_phase(&f->r,generation,phase);RootRegistry *g=&f->r.phase.e.f.g;
    if(completed){
    assert(gc_weak_mark_end_locked(g,&f->r.owner,GC_MP_BEGIN,gc_mark_phase_step_locked)==GC_MP_ADVANCED);phase_ack(&f->r.phase);
    assert(gc_weak_mark_end_locked(g,&f->r.owner,GC_MP_DECIDE,gc_mark_phase_step_locked)==GC_MP_ADVANCED);phase_ack(&f->r.phase);
    assert(gc_weak_mark_end_locked(g,&f->r.owner,GC_MP_RELEASE,gc_mark_phase_step_locked)==GC_MP_SELECT);phase_resume(&f->r.phase);
    }
    FollowFixture *p=&f->r.weak.f;
    assert(gc_weak_release_locked(&f->r.weak.gate,&f->r.weak.slot,11)==GC_WEAK_RELEASED);
    f->heap=calloc(2,GC_HEAP_PAGE_BYTES);assert(f->heap);p->heap.heap_base=p->page.heap_base=(uintptr_t)f->heap;
    p->pool.slots=(uintptr_t)f->leases;p->pool.capacity=8;p->pool.free_head=1;
    for(unsigned i=0;i<8;i++)f->leases[i].next_free=i==7?0:i+2;
    atomic_store(&p->page.allocated_top,GC_HEAP_PAGE_BYTES+192);atomic_store(&p->starts[0],(1u<<2)|(1u<<6)|(1u<<10));
    p->table=(ForwardTable){(uintptr_t)f->forward,3,3};
    f->records[0]=(MarkRecord){.object_id=42,.young_epoch=completed?f->r.phase.q.epoch:0,.old_epoch=completed?f->r.phase.q.epoch:0};
    f->records[1].object_id=43;
    for(unsigned i=0;i<3;i++){
        uint64_t offset=GC_HEAP_PAGE_BYTES+32+i*64;
        f->forward[i]=(ForwardCell){offset,27,0,0};
        ObjectHeader *header=(void *)(f->heap+offset-sizeof(ObjectHeader));
        *header=(ObjectHeader){24,1,(uintptr_t)&f->records[i==1?1:0],GC_OBJECT_PUBLISHED};
        if(i<2){
            ReferenceLease lease={.reference=(offset<<16)|0x1510};assert(gc_reference_lease_locked(&p->pool,&lease)==GC_ML_OK);
            f->entries[i].slot=(WeakSlot){(uintptr_t)&f->r.weak.gate,11+i,lease.reference,lease.lease};
        }
    }
    f->entries[2].slot=(WeakSlot){.gate=(uintptr_t)&f->r.weak.gate,.serial=13};
    f->table=(WeakTable){&f->r.weak.gate,f->entries,4,4,3,13};
    f->live=f->entries[0].slot.reference;
    if(moved){
        f->live=((GC_HEAP_PAGE_BYTES+160)<<16)|0x1510;
        atomic_store(&f->forward[0].word,f->live|2);
        /* Invalid old header: canonical observation must follow before access. */
        memset(f->heap+GC_HEAP_PAGE_BYTES,0,64);
    }
}
static void weak_scan_fixture(WeakScanFixture *f,unsigned generation,bool moved){
    weak_scan_fixture_phase(f,generation,moved,0x90022,true);
}
static void gc_run_weak_scan_cases(const WeakProcessOps *candidate,const WeakTableOps *table_candidate,WeakScanStepper scan_candidate,WeakProcessAccess access_candidate,WeakProcessFinish finish_candidate){
    const WeakProcessOps c={gc_reference_lease_locked,gc_pending_follow_locked,gc_pending_access_locked,
        gc_resolve_access,gc_guarded_weak_liveness,gc_metadata_step_locked};
    const WeakTableOps tc={gc_root_step,gc_reference_lease_locked,gc_weak_load_into_locked,gc_weak_release_locked};
    for(unsigned impl=0;impl<2;impl++)for(unsigned generation=0;generation<2;generation++)for(unsigned mode=0;mode<9;mode++){
        WeakScanStepper step=impl?scan_candidate:gc_weak_scan_step_locked;
        WeakProcessAccess access=impl?access_candidate:gc_weak_process_access_step;
        WeakProcessFinish finish=impl?finish_candidate:gc_weak_process_finish_locked;
        WeakProcessOps ops=impl?*candidate:c;weak_process_metadata=ops.metadata;
        ops.metadata=weak_process_metadata_retry;weak_process_fail_token=0;
        const WeakTableOps *table_ops=impl?table_candidate:&tc;
        WeakScanFixture f;weak_scan_fixture(&f,generation,mode==8);WeakScan scan={0};
        MutatorContext *ctx=&f.r.phase.e.f.ctx[0];MetadataLeasePool *pool=&f.r.weak.f.pool;
        WeakScanStart start={&f.table,&f.r.owner,ctx};
        WeakHandle live={(uintptr_t)&f.table,1,11},dead={(uintptr_t)&f.table,2,12};
        if(mode==0){
            WeakScanFixture original=f;
            for(unsigned fault=0;fault<12;fault++){
                WeakScan *output=&scan;const WeakScanStart *input=&start;uint64_t op=GC_WS_BEGIN,expected=GC_WP_INVALID;
                switch(fault){
                case 0:output=NULL;break;case 1:input=NULL;break;case 2:op=4;break;
                case 3:scan.work.stage=1;break;case 4:f.table.capacity=0;break;case 5:f.r.weak.gate.pool=0;break;
                case 6:ctx->abi_version=0;break;case 7:atomic_store(&ctx->access_epoch,27);break;
                case 8:f.r.owner.stage=2;break;
                case 9:f.r.owner.processor=(uintptr_t)&scan.work;expected=GC_WP_WAIT;break;
                case 10:f.r.owner.scan=(uintptr_t)&scan.work;expected=GC_WP_WAIT;break;
                case 11:f.r.phase.e.f.queue.epoch++;break;
                }
                WeakScanFixture before=f;WeakScan saved=scan;WeakScanStart saved_start=start;
                assert(step(output,input,op)==expected);
                assert(!memcmp(&before,&f,sizeof(f))&&!memcmp(&saved,&scan,sizeof(scan))&&!memcmp(&saved_start,&start,sizeof(start)));
                f=original;scan=(WeakScan){0};
            }
        }
        assert(step(&scan,&start,GC_WS_BEGIN)==GC_WP_OK);
        if(mode==0){
            WeakScanFixture original=f;WeakScan original_scan=scan;
            for(unsigned fault=0;fault<12;fault++){
                uint64_t op=GC_WS_NEXT,expected=GC_WP_INVALID;
                switch(fault){
                case 0:scan.cursor=scan.capacity+1;break;case 1:scan.phase++;break;
                case 2:scan.epoch++;break;case 3:scan.generation++;break;case 4:scan.rendezvous++;break;
                case 5:scan.entries++;break;case 6:scan.last_serial++;break;case 7:f.r.owner.scan+=8;break;
                case 8:f.r.owner.mark->state=2;break;case 9:atomic_store(&ctx->adopted_phase,0);break;
                case 10:op=GC_WS_FINISH;break;case 11:scan.work.stage=1;expected=GC_WP_WAIT;break;
                }
                WeakScanFixture before=f;WeakScan saved=scan;
                assert(step(&scan,NULL,op)==expected);
                assert(!memcmp(&before,&f,sizeof(f))&&!memcmp(&saved,&scan,sizeof(scan)));
                f=original;scan=original_scan;
            }
        }
        WeakScan other={0};assert(step(&other,&start,GC_WS_BEGIN)==GC_WP_WAIT&&!other.stage);
        WeakProcess foreign={0};assert(gc_weak_process_begin_locked(&f.table,&f.r.owner,ctx,live,&foreign,&ops)==GC_WP_INVALID);
        assert(step(&scan,NULL,GC_WS_FINISH)==GC_WP_INVALID&&f.r.weak.gate.closed);
        if(mode==1){
            ReferenceLease held[6]={0};
            for(unsigned i=0;i<6;i++){held[i].reference=f.live;assert(ops.acquire(pool,&held[i])==GC_ML_OK);}
            assert(step(&scan,NULL,GC_WS_NEXT)==GC_WP_FULL&&!scan.cursor&&!scan.work.stage&&!f.r.owner.processor);
            for(unsigned i=0;i<6;i++)assert(ops.metadata(pool,&held[i].lease,GC_ML_RELEASE)==GC_ML_OK);
        }
        bool intervention=false;unsigned steps=0;
        while(scan.stage==1){
            assert(++steps<32);
            assert(gc_weak_table_load_locked(&f.table,live,&f.r.admission,ctx,&f.r.output,table_ops)==GC_WEAK_WAIT);
            uint64_t cursor=scan.cursor,status=step(&scan,NULL,GC_WS_NEXT);
            if(status==GC_WS_DONE)break;
            if(status==GC_WP_ADVANCED){assert(scan.cursor<=cursor+1);continue;}
            assert(status==GC_WP_OK&&scan.work.stage==1&&scan.cursor==cursor);
            assert(step(&scan,NULL,GC_WS_NEXT)==GC_WP_WAIT);
            assert(step(&scan,NULL,GC_WS_FINISH)==GC_WP_INVALID);
            if(!intervention&&(mode==3||mode==4)){
                intervention=true;
                if(mode==3){
                    assert(finish(&scan.work,GC_WPF_CANCEL,0)==GC_WP_STALE);
                    assert(step(&scan,NULL,GC_WS_NEXT)==GC_WP_ADVANCED&&scan.cursor==cursor);
                }else{
                    uint64_t token=scan.work.current.lease.token;scan.work.current.lease.token+=UINT64_C(1)<<32;
                    assert(step(&scan,NULL,GC_WS_ABORT)==GC_WP_CLEANUP&&scan.stage==1&&scan.work.stage==5);
                    assert(f.r.owner.scan==(uintptr_t)&scan.work&&f.r.owner.processor==(uintptr_t)&scan.work&&f.r.weak.gate.closed);
                    scan.work.current.lease.token=token;
                    assert(step(&scan,NULL,GC_WS_ABORT)==GC_WS_ABORTED&&f.r.weak.gate.closed);
                    assert(!f.r.owner.scan&&!f.r.owner.processor);
                    scan=(WeakScan){0};assert(step(&scan,&start,GC_WS_BEGIN)==GC_WP_OK);
                }
                continue;
            }
            if(mode==2&&cursor==1)assert(gc_weak_table_release_locked(&f.table,dead,table_ops)==GC_WEAK_RELEASED);
            unsigned hops=0;
            do{status=access(&scan.work,GC_WPA_ENTER,0);assert(++hops<4);}while(status==GC_WP_ADVANCED);
            assert(status==GC_WP_OK);
            assert(step(&scan,NULL,GC_WS_ABORT)==GC_WP_INVALID);
            uint64_t result=access(&scan.work,GC_WPA_OBSERVE,0);assert(result==(cursor==0?GC_WP_LIVE:GC_WP_DEAD));
            assert(access(&scan.work,GC_WPA_EXIT,0)==GC_WP_OK);
            if(mode==6&&cursor==1){
                weak_process_fail_token=scan.work.current.lease.token;
                assert(gc_weak_process_commit_locked(&scan.work,&ops)==GC_WP_CLEANUP);
                assert(scan.work.stage==5&&f.r.owner.processor==(uintptr_t)&scan.work);
                assert(step(&scan,NULL,GC_WS_NEXT)==GC_WP_WAIT&&step(&scan,NULL,GC_WS_FINISH)==GC_WP_INVALID);
            }
            assert(finish(&scan.work,GC_WPF_COMMIT,0)==(mode==2&&cursor==1?GC_WP_STALE:result));
            assert(!f.r.owner.processor);
        }
        assert(scan.stage==2&&scan.cursor==4&&pool->active==1);
        assert(f.entries[0].slot.reference==f.live&&!f.entries[1].slot.reference&&!f.entries[2].slot.reference);
        if(mode==5){
            f.r.phase.q.epoch++;assert(step(&scan,NULL,GC_WS_FINISH)==GC_WP_INVALID&&f.r.weak.gate.closed);f.r.phase.q.epoch--;
        }
        if(mode==7){
            f.table.last_serial++;assert(step(&scan,NULL,GC_WS_FINISH)==GC_WP_INVALID&&f.r.weak.gate.closed);f.table.last_serial--;
        }
        assert(step(&scan,NULL,GC_WS_FINISH)==GC_WS_DONE&&!f.r.weak.gate.closed&&!f.r.owner.processor&&!f.r.owner.scan);
        assert(step(&scan,NULL,GC_WS_FINISH)==GC_WP_INVALID);
        assert(gc_weak_table_load_locked(&f.table,dead,&f.r.admission,ctx,&f.r.output,table_ops)==(mode==2?GC_WEAK_INVALID:GC_WEAK_EMPTY));
        assert(!atomic_load(&f.r.phase.e.f.cells[0].value));
        assert(gc_weak_table_load_locked(&f.table,live,&f.r.admission,ctx,&f.r.output,table_ops)==GC_WEAK_LOADED);
        assert(f.r.output.value==f.live&&atomic_load(&f.r.phase.e.f.buffers[0].published)==1);
        for(unsigned i=0;i<4;i++)if(f.entries[i].slot.serial){
            WeakHandle h={(uintptr_t)&f.table,i+1,f.entries[i].slot.serial};assert(gc_weak_table_release_locked(&f.table,h,table_ops)==GC_WEAK_RELEASED);
        }
        assert(!pool->active&&!f.r.weak.f.page.metadata_leases&&!f.table.active&&!atomic_load(&ctx->access_epoch));
        free(f.heap);
    }
    puts("weak scan owner: 96 C/emitted admission and geometry rejections, 4 abort-cleanup retries PASS");
    puts("weak scan: 36 C/emitted bounded table, moved referent, retry, abort and covered-reopen lifecycles PASS");
}
#endif
