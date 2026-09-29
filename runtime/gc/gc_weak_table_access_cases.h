#ifndef KLASSIC_GC_WEAK_TABLE_ACCESS_CASES_H
#define KLASSIC_GC_WEAK_TABLE_ACCESS_CASES_H
#include "gc_weak_table_cases.h"
typedef struct {
    WeakLoadFixture w;WeakTable table;WeakEntry entries[2];WeakTableRequest request;
} WeakTableAccessFixture;
static void weak_table_access_fixture(WeakTableAccessFixture *f){
    memset(f,0,sizeof(*f));weak_load_fixture(&f->w);
    f->entries[0].slot=f->w.slot;f->w.slot=(WeakSlot){0};
    f->table=(WeakTable){&f->w.gate,f->entries,2,2,1,11};
    f->request=(WeakTableRequest){{(uintptr_t)&f->table,1,11},(uintptr_t)&f->w.admission,(uintptr_t)&f->w.f.ctx,(uintptr_t)&f->w.output};
}
static void gc_run_weak_table_access_cases(WeakTableStepper candidate){
    for(unsigned impl=0;impl<2;impl++)for(unsigned operation=0;operation<2;operation++)for(unsigned mode=0;mode<16;mode++){
        WeakTableStepper step=impl?candidate:gc_weak_table_step_locked;
        WeakTableAccessFixture f;weak_table_access_fixture(&f);WeakLoadFixture *w=&f.w;
        WeakTableRequest *q=&f.request;uint64_t op=operation,expected=GC_WEAK_INVALID;
        if(!operation){
            switch(mode){
            case 0:expected=GC_WEAK_LOADED;break;
            case 1:assert(gc_metadata_step_locked(&w->f.pool,&f.entries[0].slot.lease,GC_ML_RELEASE)==GC_ML_OK);f.entries[0].slot.reference=0;expected=GC_WEAK_EMPTY;break;
            case 2:w->gate.closed=1;expected=GC_WEAK_WAIT;break;
            case 3:q->handle.serial++;break;
            case 4:q->handle.table+=8;break;
            case 5:q->handle.index=0;break;
            case 6:f.entries[0].slot.lease.token+=UINT64_C(1)<<32;break;
            case 7:atomic_store(&w->f.cell.value,f.entries[0].slot.reference);break;
            case 8:w->f.journal.top=w->f.journal.capacity;atomic_store(&w->f.journal.published,w->f.journal.top);expected=GC_WEAK_FLUSH;break;
            case 9:atomic_store(&w->f.ctx.root_revision,UINT64_MAX);expected=GC_WEAK_EXHAUSTED;break;
            case 10:atomic_store(&w->f.ctx.adopted_phase,0x20022);expected=GC_WEAK_WAIT;break;
            case 11:w->handshake.active=1;w->handshake.generation=2;w->handshake.expected=1;expected=GC_WEAK_WAIT;break;
            case 12:w->registration.registered=0;break;
            case 13:op=2;break;
            case 14:q=(void *)((char *)q+1);break;
            case 15:f.entries[0].next_free=2;break;
            }
        }else{
            switch(mode){
            case 0:expected=GC_WEAK_RELEASED;break;
            case 1:w->gate.closed=1;expected=GC_WEAK_RELEASED;break;
            case 2:assert(gc_metadata_step_locked(&w->f.pool,&f.entries[0].slot.lease,GC_ML_RELEASE)==GC_ML_OK);f.entries[0].slot.reference=0;expected=GC_WEAK_RELEASED;break;
            case 3:q->handle.serial++;break;
            case 4:q->handle.table+=8;break;
            case 5:q->handle.index=0;break;
            case 6:f.entries[0].slot.lease.token+=UINT64_C(1)<<32;break;
            case 7:f.table.active=0;break;
            case 8:f.entries[0].slot.reference=0;break;
            case 9:f.entries[0].slot.gate+=8;break;
            case 10:f.entries[0].next_free=2;break;
            case 11:q->admission=q->context=q->output_root=UINT64_MAX;expected=GC_WEAK_RELEASED;break;
            case 12:op=2;break;
            case 13:q=(void *)((char *)q+1);break;
            case 14:assert(step(&f.table,q,GC_WT_RELEASE)==GC_WEAK_RELEASED);break;
            case 15:{
                assert(step(&f.table,q,GC_WT_RELEASE)==GC_WEAK_RELEASED);
                WeakHandle fresh={0};const WeakTableOps ops={gc_root_step,gc_reference_lease_locked,gc_weak_load_into_locked,gc_weak_release_locked};
                assert(gc_weak_table_new_locked(&f.table,&w->admission,&w->f.ctx,&w->output,&fresh,&ops)==GC_WEAK_LOADED);
                assert(fresh.index==1&&fresh.serial==12);break;
            }
            }
        }
        WeakTableAccessFixture before;memcpy(&before,&f,sizeof(f));
        assert(step(&f.table,q,op)==expected);
        assert(!memcmp(&before.request,&f.request,sizeof(f.request)));
        if(expected!=0)assert(!memcmp(&before,&f,sizeof(f)));
        else if(!operation){
            assert(w->output.value==f.entries[0].slot.reference&&atomic_load(&w->f.cell.value)==w->output.value);
            assert(atomic_load(&w->f.journal.published)==1&&f.table.active==1&&w->f.pool.active==1);
        }else{
            WeakSlot zero={0};assert(!memcmp(&f.entries[0].slot,&zero,sizeof(zero)));
            assert(!f.table.active&&f.table.free_head==1&&f.entries[0].next_free==2);
            assert(!w->f.pool.active&&!w->f.page.metadata_leases);
            assert(!memcmp(&w->f.cell,&before.w.f.cell,sizeof(w->f.cell))&&!memcmp(&w->f.journal,&before.w.f.journal,sizeof(w->f.journal)));
        }
        if(operation&&mode==15){
            q->handle.serial=12;assert(step(&f.table,q,GC_WT_LOAD)==GC_WEAK_EMPTY);
            assert(step(&f.table,q,GC_WT_RELEASE)==GC_WEAK_RELEASED&&!f.table.active);
        }
    }
    puts("weak table access: 64 C/emitted rooted-load, release and serial-reuse cases PASS");
}
#endif
