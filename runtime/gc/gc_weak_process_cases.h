#ifndef KLASSIC_GC_WEAK_PROCESS_CASES_H
#define KLASSIC_GC_WEAK_PROCESS_CASES_H
#include "gc_weak_process.h"
#include "gc_weak_mark_cases.h"
static uint64_t (*weak_process_metadata)(MetadataLeasePool *,MetadataLeaseRequest *,uint64_t);
static uint64_t weak_process_fail_token;
static uint64_t weak_process_metadata_retry(MetadataLeasePool *pool,MetadataLeaseRequest *q,uint64_t op){
    if(op==GC_ML_RELEASE&&q->token==weak_process_fail_token){weak_process_fail_token=0;return GC_ML_INVALID;}
    return weak_process_metadata(pool,q,op);
}
static void gc_run_weak_process_cases(const WeakProcessOps *candidate,WeakProcessCapture capture_candidate,WeakProcessAccess access_candidate,WeakProcessFinish finish_candidate){
    const WeakProcessOps c={gc_reference_lease_locked,gc_pending_follow_locked,gc_pending_access_locked,
        gc_resolve_access,gc_guarded_weak_liveness,gc_metadata_step_locked};
    for(unsigned impl=0;impl<2;impl++)for(unsigned generation=0;generation<2;generation++)for(unsigned mode=0;mode<10;mode++){
        WeakProcessOps injected=impl?*candidate:c;
        weak_process_metadata=injected.metadata;weak_process_fail_token=0;
        injected.metadata=weak_process_metadata_retry;const WeakProcessOps *ops=&injected;
        WeakMarkRace r;weak_mark_fixture(&r,generation);RootRegistry *g=&r.phase.e.f.g;
        assert(gc_weak_mark_end_locked(g,&r.owner,GC_MP_BEGIN,gc_mark_phase_step_locked)==GC_MP_ADVANCED);phase_ack(&r.phase);
        assert(gc_weak_mark_end_locked(g,&r.owner,GC_MP_DECIDE,gc_mark_phase_step_locked)==GC_MP_ADVANCED);phase_ack(&r.phase);
        assert(gc_weak_mark_end_locked(g,&r.owner,GC_MP_RELEASE,gc_mark_phase_step_locked)==GC_MP_SELECT);phase_resume(&r.phase);
        FollowFixture *f=&r.weak.f;
        unsigned char *heap=calloc(2,GC_HEAP_PAGE_BYTES);assert(heap);
        f->heap.heap_base=f->page.heap_base=(uintptr_t)heap;
        atomic_store(&f->page.allocated_top,GC_HEAP_PAGE_BYTES+64);
        MarkRecord record={.object_id=42};
        if(mode==1)record.young_epoch=record.old_epoch=r.phase.q.epoch;
        ObjectHeader *header=(void *)(heap+GC_HEAP_PAGE_BYTES);
        *header=(ObjectHeader){24,1,(uintptr_t)&record,mode==4?GC_OBJECT_INITIALIZING:GC_OBJECT_PUBLISHED};
        if(mode==7)f->page.generation=GC_OLD;
        WeakEntry entry={.slot=r.weak.slot};r.weak.slot=(WeakSlot){0};
        WeakTable table={&r.weak.gate,&entry,1,0,1,11};WeakHandle handle={(uintptr_t)&table,1,11};
        WeakProcess q={0};MutatorContext *ctx=&r.phase.e.f.ctx[0];
        WeakProcessStart start={&table,&r.owner,ctx,handle};
        WeakProcessCapture capture=impl?capture_candidate:gc_weak_process_capture_locked;
        WeakProcessAccess access=impl?access_candidate:gc_weak_process_access_step;
        WeakProcessFinish finish=impl?finish_candidate:gc_weak_process_finish_locked;
        if(mode==0){
            WeakMarkRace original=r;WeakEntry original_entry=entry;
            WeakTable original_table=table;WeakProcessStart original_start=start;
            for(unsigned fault=2;fault<32;fault++){
                uint64_t expected=GC_WP_INVALID,op=0;
                WeakProcess *output=&q;const WeakProcessStart *input=&start;
                switch(fault){
                case 2:op=1;break;
                case 3:output=NULL;break;
                case 4:input=(void *)((uintptr_t)&start+1);break;
                case 5:q.access.scope.ticket=1;break;
                case 6:ctx->abi_version=0;break;
                case 7:ctx->thread_id=0;break;
                case 8:atomic_store(&ctx->state,GC_AT_SAFEPOINT);break;
                case 9:atomic_store(&ctx->access_epoch,1);break;
                case 10:r.owner.stage=2;break;
                case 11:r.owner.processor=(uintptr_t)&q;break;
                case 12:r.owner.scan=(uintptr_t)&q+8;break;
                case 13:table.gate->closed=0;break;
                case 14:r.owner.mark->state=2;break;
                case 15:r.owner.mark->outcome=GC_CONCURRENT_MARK;break;
                case 16:r.owner.mark->generation=2;break;
                case 17:r.owner.mark->epoch=0;break;
                case 18:atomic_store(&ctx->adopted_phase,0);break;
                case 19:g->phase=0;break;
                case 20:((HandshakeState *)(uintptr_t)table.gate->handshake)->active=1;break;
                case 21:((HandshakeState *)(uintptr_t)table.gate->handshake)->generation++;break;
                case 22:((MarkQueue *)(uintptr_t)((RootAudit *)(uintptr_t)r.owner.mark->audit)->queue)->epoch++;break;
                case 23:start.handle.serial--;expected=GC_WP_STALE;break;
                case 24:start.handle.table+=8;expected=GC_WP_STALE;break;
                case 25:entry.slot.reference=0;entry.slot.lease=(MetadataLeaseRequest){0};expected=GC_WP_EMPTY;break;
                case 26:entry.slot.reference=0;break;
                case 27:entry.slot.lease.token+=UINT64_C(1)<<32;break;
                case 28:entry.slot.lease.incarnation++;break;
                case 29:f->pool.free_head=0;f->pool.active=2;expected=GC_WP_FULL;break;
                case 30:f->slots[f->pool.free_head-1].generation=UINT32_MAX;expected=GC_WP_EXHAUSTED;break;
                case 31:f->page.metadata_leases=UINT64_MAX;expected=GC_WP_EXHAUSTED;break;
                }
                WeakMarkRace before=r;WeakEntry saved_entry=entry;WeakTable saved_table=table;
                WeakProcess saved=q;WeakProcessStart saved_start=start;
                assert(capture(output,input,op)==expected);
                assert(!memcmp(&before,&r,sizeof(r))&&!memcmp(&saved_entry,&entry,sizeof(entry)));
                assert(!memcmp(&saved_table,&table,sizeof(table))&&!memcmp(&saved,&q,sizeof(q)));
                assert(!memcmp(&saved_start,&start,sizeof(start)));
                r=original;entry=original_entry;table=original_table;start=original_start;q=(WeakProcess){0};
            }
        }
        if(mode==1)r.owner.scan=(uintptr_t)&q;
        ReferenceLease blocker={.reference=entry.slot.reference};
        if(mode==2){
            assert(ops->acquire(&f->pool,&blocker)==GC_ML_OK);
            WeakProcess saved=q;WeakEntry before=entry;MetadataLeasePool pool=f->pool;
            assert(capture(&q,&start,0)==GC_WP_FULL);
            assert(!memcmp(&saved,&q,sizeof(q))&&!memcmp(&before,&entry,sizeof(entry))&&!memcmp(&pool,&f->pool,sizeof(pool)));
            assert(ops->metadata(&f->pool,&blocker.lease,GC_ML_RELEASE)==GC_ML_OK);
        }
        assert(capture(&q,&start,0)==GC_WP_OK);
        assert(f->pool.active==2&&f->page.metadata_leases==2&&q.current.lease.token!=entry.slot.lease.token);
        if(mode==0){
            WeakMarkRace original=r;WeakProcess original_q=q;
            for(unsigned fault=0;fault<24;fault++){
                uint64_t expected=GC_WP_INVALID,op=GC_WPA_ENTER,reserved=0;WeakProcess *output=&q;
                switch(fault){
                case 0:output=NULL;break;
                case 1:reserved=1;break;
                case 2:op=3;break;
                case 3:q.stage=2;break;
                case 4:atomic_store(&ctx->access_epoch,27);break;
                case 5:ctx->abi_version=0;break;
                case 6:ctx->thread_id=0;break;
                case 7:atomic_store(&ctx->state,GC_AT_SAFEPOINT);break;
                case 8:r.owner.processor=0;break;
                case 9:r.owner.stage=2;break;
                case 10:r.owner.gate->closed=0;break;
                case 11:q.epoch++;break;
                case 12:q.generation++;break;
                case 13:q.rendezvous++;break;
                case 14:q.phase++;break;
                case 15:((MarkQueue *)(uintptr_t)((RootAudit *)(uintptr_t)r.owner.mark->audit)->queue)->epoch++;break;
                case 16:atomic_store(&ctx->adopted_phase,0);break;
                case 17:q.pool=NULL;break;
                case 18:q.retired=q.current.lease;q.retired.token+=UINT64_C(1)<<32;expected=GC_WP_CLEANUP;break;
                case 19:q.current.reference=0;break;
                case 20:atomic_store(&f->forward.word,5);expected=GC_WP_WAIT;break;
                case 21:atomic_store(&f->forward.access,GC_SOURCE_CLOSED);expected=GC_WP_WAIT;break;
                case 22:atomic_store(&f->forward.access,GC_SOURCE_CLOSED-1);expected=GC_WP_EXHAUSTED;break;
                case 23:q.current.lease.token+=UINT64_C(1)<<32;break;
                }
                WeakMarkRace before=r;WeakProcess saved=q;WeakEntry saved_entry=entry;
                assert(access(output,op,reserved)==expected);
                assert(!memcmp(&before,&r,sizeof(r))&&!memcmp(&saved,&q,sizeof(q))&&!memcmp(&saved_entry,&entry,sizeof(entry)));
                r=original;q=original_q;
            }
        }
        assert(access(&q,GC_WPA_ENTER,0)==GC_WP_OK);
        assert(q.stage==2&&atomic_load(&ctx->access_epoch)==27);
        assert(access(&q,GC_WPA_ENTER,0)==GC_WP_INVALID);
        if(mode==0){
            uint64_t epoch=r.owner.mark->epoch;r.owner.mark->epoch++;
            WeakProcess saved=q;WeakMarkRace before=r;
            assert(access(&q,GC_WPA_OBSERVE,0)==GC_WP_INVALID);
            assert(!memcmp(&saved,&q,sizeof(q))&&!memcmp(&before,&r,sizeof(r)));
            assert(access(&q,GC_WPA_EXIT,0)==GC_WP_OK&&q.stage==1&&!atomic_load(&ctx->access_epoch));
            r.owner.mark->epoch=epoch;
            assert(access(&q,GC_WPA_ENTER,0)==GC_WP_OK);
            uint64_t held=q.access.scope.held;q.access.scope.held=0;saved=q;before=r;
            assert(access(&q,GC_WPA_EXIT,0)==GC_WP_INVALID);
            assert(!memcmp(&saved,&q,sizeof(q))&&!memcmp(&before,&r,sizeof(r)));
            q.access.scope.held=held;
        }
        WeakEntry before=entry;
        assert(finish(&q,GC_WPF_COMMIT,0)==GC_WP_INVALID&&!memcmp(&before,&entry,sizeof(entry)));
        assert(finish(&q,GC_WPF_CANCEL,0)==GC_WP_INVALID);
        if(mode==4){
            assert(access(&q,GC_WPA_OBSERVE,0)==GC_WP_WAIT&&q.stage==2);
            assert(access(&q,GC_WPA_EXIT,0)==GC_WP_OK&&q.stage==1);
            assert(!atomic_load(&ctx->access_epoch)&&f->pool.active==2);
            atomic_store(&header->state,GC_OBJECT_PUBLISHED);
            assert(access(&q,GC_WPA_ENTER,0)==GC_WP_OK);
        }
        bool live=mode==1||(mode==7&&generation==GC_YOUNG);
        uint64_t expected=live?GC_WP_LIVE:GC_WP_DEAD;
        MarkRecord recorded=record;
        assert(access(&q,GC_WPA_OBSERVE,0)==expected&&q.stage==3);
        assert(!memcmp(&recorded,&record,sizeof(record))&&!memcmp(&before,&entry,sizeof(entry)));
        assert(access(&q,GC_WPA_OBSERVE,0)==GC_WP_INVALID);
        if(mode==0)r.owner.stage=2;
        assert(access(&q,GC_WPA_EXIT,0)==GC_WP_OK&&q.stage==4&&!atomic_load(&ctx->access_epoch));
        if(mode==0)r.owner.stage=3;
        if(mode==0){
            WeakMarkRace original=r;WeakProcess original_q=q;WeakEntry original_entry=entry;WeakTable original_table=table;
            for(unsigned fault=0;fault<26;fault++){
                uint64_t operation=GC_WPF_COMMIT,reserved=0,expected=GC_WP_INVALID;WeakProcess *output=&q;
                switch(fault){
                case 0:output=NULL;break;
                case 1:reserved=1;break;
                case 2:operation=2;break;
                case 3:q.stage=2;break;
                case 4:atomic_store(&ctx->access_epoch,27);break;
                case 5:atomic_store(&ctx->state,GC_AT_SAFEPOINT);break;
                case 6:r.owner.processor=0;break;
                case 7:q.epoch++;break;
                case 8:q.pool=NULL;break;
                case 9:q.result=0;break;
                case 10:q.current.lease.token+=UINT64_C(1)<<32;break;
                case 11:entry.slot.lease.incarnation++;expected=GC_WP_STALE;break;
                case 12:table.capacity=0;expected=GC_WP_STALE;break;
                case 13:q.handle.serial--;expected=GC_WP_STALE;break;
                case 14:entry.slot.serial++;table.last_serial++;expected=GC_WP_STALE;break;
                case 15:q.source.reference^=16;expected=GC_WP_STALE;break;
                case 16:operation=GC_WPF_CANCEL;q.current.lease.token+=UINT64_C(1)<<32;expected=GC_WP_CLEANUP;break;
                case 17:operation=GC_WPF_CANCEL;r.owner.processor=0;break;
                case 18:operation=GC_WPF_CANCEL;q.retired=q.current.lease;q.retired.token+=UINT64_C(1)<<32;expected=GC_WP_CLEANUP;break;
                case 19:q.stage=5;q.current.lease.token+=UINT64_C(1)<<32;expected=GC_WP_CLEANUP;break;
                case 20:operation=GC_WPF_CANCEL;r.owner.stage=2;expected=GC_WP_STALE;break;
                case 21:operation=GC_WPF_CANCEL;q.stage=1;expected=GC_WP_STALE;break;
                case 22:q.stage=5;r.owner.stage=2;expected=GC_WP_DEAD;break;
                case 23:q.current.lease.page+=8;break;
                case 24:case 25:
                    /* Exclusive transfer of the slot's obligation to a retired
                     * private holder; neither token is duplicated or dropped. */
                    q.stage=5;q.retired=entry.slot.lease;entry.slot.lease=(MetadataLeaseRequest){0};entry.slot.reference=0;
                    expected=GC_WP_DEAD;
                    if(fault==25){q.current.lease.token+=UINT64_C(1)<<32;expected=GC_WP_CLEANUP;}
                    break;
                }
                WeakMarkRace input_r=r;WeakProcess input_q=q;WeakEntry input_entry=entry;WeakTable input_table=table;
                assert(gc_weak_process_finish_locked(output,operation,reserved)==expected);
                WeakMarkRace expected_r=r;WeakProcess expected_q=q;WeakEntry expected_entry=entry;WeakTable expected_table=table;
                r=input_r;q=input_q;entry=input_entry;table=input_table;
                assert(finish(output,operation,reserved)==expected);
                assert(!memcmp(&expected_r,&r,sizeof(r))&&!memcmp(&expected_q,&q,sizeof(q)));
                assert(!memcmp(&expected_entry,&entry,sizeof(entry))&&!memcmp(&expected_table,&table,sizeof(table)));
                if(fault==25){
                    assert(q.stage==5&&!q.retired.token&&q.current.lease.token&&r.owner.processor==(uintptr_t)&q);
                    assert(f->pool.active==1&&f->page.metadata_leases==1&&!entry.slot.reference);
                    q.current.lease.token=original_q.current.lease.token;
                    assert(finish(&q,GC_WPF_COMMIT,0)==GC_WP_DEAD&&q.stage==6&&!r.owner.processor&&!f->pool.active);
                }
                r=original;q=original_q;entry=original_entry;table=original_table;
            }
            assert(!memcmp(&recorded,&record,sizeof(record)));
        }
        uint64_t private_token=q.current.lease.token;
        if(mode==3){
            const WeakTableOps table_ops={gc_root_step,gc_reference_lease_locked,gc_weak_load_into_locked,gc_weak_release_locked};
            assert(gc_weak_table_release_locked(&table,handle,&table_ops)==GC_WEAK_RELEASED);expected=GC_WP_STALE;
        }
        if(mode==5){
            r.phase.q.epoch++;
            assert(finish(&q,GC_WPF_COMMIT,0)==GC_WP_INVALID&&q.stage==4&&!memcmp(&before,&entry,sizeof(entry)));
            assert(finish(&q,GC_WPF_CANCEL,0)==GC_WP_STALE);expected=GC_WP_STALE;
        }else if(mode==6){
            assert(finish(&q,GC_WPF_CANCEL,0)==GC_WP_STALE);expected=GC_WP_STALE;
        }else{
            if(mode==8||mode==9){
                weak_process_fail_token=mode==8?q.current.lease.token:entry.slot.lease.token;
                /* Inject failure in the C metadata callback, then exercise the
                 * fixed C/emitted entry for the retained cleanup retry. */
                assert(gc_weak_process_commit_locked(&q,ops)==GC_WP_CLEANUP);
                assert(q.stage==(mode==8?5u:4u)&&q.current.lease.token==private_token);
                if(mode==8)assert(!entry.slot.reference&&!entry.slot.lease.token&&f->pool.active==1);
                else assert(!memcmp(&before,&entry,sizeof(entry))&&f->pool.active==2);
            }
            assert(finish(&q,GC_WPF_COMMIT,0)==expected);
        }
        assert(q.stage==6&&!q.current.reference&&!q.current.lease.token&&!q.retired.token);
        assert(finish(&q,GC_WPF_COMMIT,0)==GC_WP_INVALID);
        if(expected==GC_WP_DEAD){
            assert(!entry.slot.reference&&!entry.slot.lease.token&&entry.slot.serial==11&&entry.slot.gate==(uintptr_t)table.gate);
            assert(table.active==1&&!table.free_head&&!f->pool.active&&!f->page.metadata_leases);
            /* Closed admission remains intact; no unrooted result is exposed. */
            assert(table.gate->closed==1);
        }else if(expected==GC_WP_LIVE){
            assert(entry.slot.reference==before.slot.reference&&entry.slot.lease.token==private_token);
            assert(f->pool.active==1&&f->page.metadata_leases==1);
        }else if(mode==3)assert(!table.active&&!f->pool.active&&!f->page.metadata_leases);
        else assert(!memcmp(&before,&entry,sizeof(entry))&&f->pool.active==1);
        if(entry.slot.serial)assert(gc_weak_release_locked(table.gate,&entry.slot,11)==GC_WEAK_RELEASED);
        assert(!f->pool.active&&!f->page.metadata_leases&&!atomic_load(&ctx->access_epoch));
        assert(!memcmp(&recorded,&record,sizeof(record)));
        for(unsigned i=0;i<3;i++)assert(!atomic_load(&r.phase.e.f.cells[i].value)&&!atomic_load(&r.phase.e.f.buffers[i].published));
        free(heap);
    }
    puts("weak finish: 104 C/emitted state comparisons and 4 partial-cleanup retries PASS");
    puts("weak access: 96 C/emitted rejected or retryable ENTER cases and 8 stale-owner guard exits PASS");
    puts("weak capture: 120 C/emitted failure-preservation cases PASS");
    puts("weak processing: 40 C/candidate guarded dead-clear, live-lease, pressure, disposal and cleanup-retry lifecycles PASS");
}
#endif
