#ifndef KLASSIC_GC_WEAK_TABLE_CASES_H
#define KLASSIC_GC_WEAK_TABLE_CASES_H
#include "gc_weak_table.h"
#include "gc_weak_load_cases.h"
typedef struct {
    WeakLoadFixture w;WeakTable table;WeakEntry entries[2];WeakHandle result;
    ReferenceLease blockers[2];
} WeakTableFixture;
static void weak_table_fixture(WeakTableFixture *f){
    memset(f,0,sizeof(*f));weak_load_fixture(&f->w);
    assert(gc_weak_load_into_locked(&f->w.gate,&f->w.request,0)==GC_WEAK_LOADED);
    assert(gc_weak_release_locked(&f->w.gate,&f->w.slot,11)==GC_WEAK_RELEASED);
    f->table=(WeakTable){&f->w.gate,f->entries,2,1,0,0};f->entries[0].next_free=2;
}
static uint64_t weak_table_new_via(WeakTable *table,RootedLoadAdmission *admission,MutatorContext *context,const RootRequest *source,WeakHandle *out,WeakTableCreator create){
    WeakCreateRequest q={(uintptr_t)admission,(uintptr_t)context,(uintptr_t)source,(uintptr_t)out};
    WeakCreateRequest before=q;uint64_t result=create(table,&q,0);assert(!memcmp(&q,&before,sizeof(q)));return result;
}
static void gc_run_weak_table_cases(const WeakTableOps *candidate,WeakTableCreator create_candidate){
    const WeakTableOps c={gc_root_step,gc_reference_lease_locked,gc_weak_load_into_locked,gc_weak_release_locked};
    for(unsigned impl=0;impl<2;impl++){
        const WeakTableOps *ops=impl?candidate:&c;
        WeakTableCreator create=impl?create_candidate:gc_weak_create_step_locked;
        for(unsigned mode=0;mode<24;mode++){
            WeakTableFixture f;weak_table_fixture(&f);WeakLoadFixture *w=&f.w;
            const RootRequest *source=&w->output;uint64_t expected=GC_WEAK_INVALID;
            switch(mode){
            case 0:expected=GC_WEAK_LOADED;break;
            case 1:w->output.value=0;assert(gc_root_step(&w->f.ctx,&w->output,GC_ROOT_STORE)==GC_ROOT_OK);expected=GC_WEAK_LOADED;break;
            case 2:w->gate.closed=1;expected=GC_WEAK_WAIT;break;
            case 3:w->output.handle.reuse_generation++;break;
            case 4:w->registration.registered=0;break;
            case 5:w->admission.registry+=8;break;
            case 6:w->admission.handshake+=8;break;
            case 7:w->handshake.active=1;w->handshake.generation=2;w->handshake.expected=1;expected=GC_WEAK_WAIT;break;
            case 8:atomic_store(&w->f.ctx.adopted_phase,0x20022);expected=GC_WEAK_WAIT;break;
            case 9:
                for(unsigned i=0;i<2;i++){f.blockers[i].reference=w->output.value;assert(ops->acquire(&w->f.pool,&f.blockers[i])==GC_ML_OK);}
                expected=GC_WEAK_FULL;break;
            case 10:f.table.last_serial=UINT64_MAX;expected=GC_WEAK_EXHAUSTED;break;
            case 11:
                for(unsigned i=0;i<2;i++){WeakHandle h={0};assert(weak_table_new_via(&f.table,&w->admission,&w->f.ctx,source,&h,create)==GC_WEAK_LOADED);}
                expected=GC_WEAK_FULL;break;
            case 12:f.entries[0].next_free=1;break;
            case 13:source=NULL;break;
            case 14:f.result.serial=1;break;
            case 15:w->registration.context+=8;break;
            case 16:atomic_store(&w->f.ctx.access_epoch,27);break;
            case 17:atomic_store(&w->f.ctx.state,GC_BLOCKED_NATIVE);break;
            case 18:atomic_store(&w->f.cell.value,1);break;
            case 19:atomic_store(&w->f.starts[0],0);break;
            case 20:f.entries[0].slot.serial=1;break;
            case 21:w->f.segment.owner++;break;
            case 22:w->participant.registered=0;break;
            case 23:w->f.slots[w->f.pool.free_head-1].generation=UINT32_MAX;expected=GC_WEAK_EXHAUSTED;break;
            }
            WeakTableFixture before;memcpy(&before,&f,sizeof(f));
            assert(weak_table_new_via(&f.table,&w->admission,&w->f.ctx,source,&f.result,create)==expected);
            if(expected!=GC_WEAK_LOADED)assert(!memcmp(&before,&f,sizeof(f)));
            else{
                assert(f.table.active==1&&f.table.last_serial==1&&f.table.free_head==2);
                assert(f.result.table==(uintptr_t)&f.table&&f.result.index==1&&f.result.serial==1);
                assert(f.entries[0].slot.reference==atomic_load(&w->f.cell.value));
                assert(w->f.pool.active==(mode==0)&&w->f.page.metadata_leases==(mode==0));
                assert(!memcmp(&before.w.output,&w->output,sizeof(w->output)));
                assert(!memcmp(&before.w.f.journal,&w->f.journal,sizeof(w->f.journal)));
                assert(!memcmp(&before.w.roots,&w->roots,sizeof(w->roots)));
                assert(gc_weak_table_release_locked(&f.table,f.result,ops)==GC_WEAK_RELEASED);
                assert(!f.table.active&&!w->f.pool.active&&!w->f.page.metadata_leases);
            }
        }
        WeakTableFixture f;weak_table_fixture(&f);WeakLoadFixture *w=&f.w;
        WeakHandle stale={0};
        for(unsigned round=0;round<64;round++){
            WeakHandle h={0};assert(weak_table_new_via(&f.table,&w->admission,&w->f.ctx,&w->output,&h,create)==GC_WEAK_LOADED);
            assert(h.index==1&&h.serial==round+1);
            WeakTableFixture before;memcpy(&before,&f,sizeof(f));
            if(round){
                assert(gc_weak_table_release_locked(&f.table,stale,ops)==GC_WEAK_INVALID);
                assert(gc_weak_table_load_locked(&f.table,stale,&w->admission,&w->f.ctx,&w->output,ops)==GC_WEAK_INVALID);
            }
            WeakHandle forged=h;forged.table+=8;
            assert(gc_weak_table_release_locked(&f.table,forged,ops)==GC_WEAK_INVALID);
            forged=h;forged.index=UINT64_MAX;
            assert(gc_weak_table_load_locked(&f.table,forged,&w->admission,&w->f.ctx,&w->output,ops)==GC_WEAK_INVALID);
            assert(!memcmp(&before,&f,sizeof(f)));
            if(round==63){
                uint64_t reference=w->output.value;w->output.value=0;
                assert(ops->root(&w->f.ctx,&w->output,GC_ROOT_STORE)==GC_ROOT_OK);
                assert(gc_weak_table_load_locked(&f.table,h,&w->admission,&w->f.ctx,&w->output,ops)==GC_WEAK_LOADED);
                assert(w->output.value==reference&&atomic_load(&w->f.cell.value)==reference);
                assert(atomic_load(&w->f.journal.published)==3);
            }
            assert(gc_weak_table_release_locked(&f.table,h,ops)==GC_WEAK_RELEASED);
            assert(!f.table.active&&!w->f.pool.active&&!w->f.page.metadata_leases);
            stale=h;
        }
        assert(f.table.last_serial==64&&f.table.free_head==1);
    }
    puts("weak table: 48 C/emitted creation and pressure cases, 128 serial reuse lifecycles and rooted promotion PASS");
}
#endif
