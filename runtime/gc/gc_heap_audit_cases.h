#ifndef KLASSIC_GC_HEAP_AUDIT_CASES_H
#define KLASSIC_GC_HEAP_AUDIT_CASES_H
#include "gc_heap_audit.h"
#include "gc_heap_audit_slice_cases.h"
typedef struct {HeapAuditFixture f;HeapPool heap;MetadataLeasePool pool;MetadataLeaseSlot slot;RelocationGate gate;HeapAudit q;ForwardCell appended[2];MarkRecord extra;} HeapAuditOwnerFixture;
static void heap_audit_owner_fixture(HeapAuditOwnerFixture *f,uint64_t value){
    memset(f,0,sizeof(*f));heap_audit_fixture(&f->f,value,0);
    f->heap.pages=(uintptr_t)&f->f.field.page;f->heap.page_count=1;
    f->pool=(MetadataLeasePool){(uintptr_t)&f->heap,(uintptr_t)&f->slot,1,1,0};
    f->gate=(RelocationGate){.epoch=7,.closed=1};
    f->q=(HeapAudit){.context=(uintptr_t)&f->f.field.ctx,.registry=(uintptr_t)&f->f.field.registry,
        .gate=(uintptr_t)&f->gate,.epoch=7,.phase=0x22,.target_start=128,.target_limit=160,
        .walk={(uintptr_t)&f->pool,(uintptr_t)&f->heap,f->heap.pages,1,0,{0}}};
}
static uint64_t heap_audit_owner_finish(HeapAuditOwnerFixture *f,HeapAuditor step){
    for(unsigned i=0;i<64;i++){
        uint64_t op=f->q.stage==2?GC_HA_SCAN:f->q.stage==3?GC_HA_CONSUME:GC_HA_META;
        uint64_t result=step(&f->pool,&f->q,op);
        assert(!atomic_load(&f->f.field.ctx.access_epoch));
        if(result==GC_HA_DONE||result==GC_HA_DIRTY){assert(!f->pool.active&&!f->q.walk.lease.token);return result;}
        assert(result==GC_HA_MORE||result==GC_HA_SCAN_READY||result==GC_HA_FOUND);
    }
    assert(0);return GC_HA_INVALID;
}
static void gc_run_heap_audit_cases(HeapAuditor candidate){
    const uint64_t ref=(UINT64_C(128)<<16)|0x1510;
    for(unsigned impl=0;impl<2;impl++){
        HeapAuditor step=impl?candidate:gc_heap_audit_step;HeapAuditOwnerFixture f,before;
        assert(step(NULL,NULL,0)==GC_HA_INVALID);
        for(unsigned positive=0;positive<2;positive++){
            heap_audit_owner_fixture(&f,positive?ref:0);
            MetadataLeaseRequest blocker={(uintptr_t)&f.f.field.page,27,0};
            assert(gc_metadata_step_locked(&f.pool,&blocker,GC_ML_ACQUIRE)==GC_ML_OK);
            memcpy(&before,&f,sizeof(f));assert(step(&f.pool,&f.q,GC_HA_META)==GC_HA_FULL&&!memcmp(&f,&before,sizeof(f)));
            assert(gc_metadata_step_locked(&f.pool,&blocker,GC_ML_RELEASE)==GC_ML_OK);
            uint64_t expected=positive?GC_HA_DIRTY:GC_HA_DONE;
            assert(heap_audit_owner_finish(&f,step)==expected&&f.q.findings==positive&&f.q.stage==4);
            memcpy(&before,&f,sizeof(f));assert(step(&f.pool,&f.q,GC_HA_META)==expected&&!memcmp(&f,&before,sizeof(f)));
            assert(step(&f.pool,&f.q,GC_HA_CANCEL)==GC_HA_CANCELLED&&!f.q.findings&&!f.q.stage&&!f.q.walk.cursor);
        }
        for(unsigned stale=0;stale<3;stale++){
            heap_audit_owner_fixture(&f,ref);
            if(stale==2){
                f.appended[0]=(ForwardCell){48,27,0,0};
                f.f.table.cells=(uintptr_t)f.appended;f.f.table.capacity=2;
            }
            assert(step(&f.pool,&f.q,GC_HA_META)==GC_HA_MORE&&f.pool.active==1);
            if(stale==0){assert(gc_relocation_gate_locked(&f.gate,NULL,GC_RG_REOPEN)==GC_RG_OK);}
            if(stale==1)atomic_store(&f.f.field.ctx.adopted_phase,0x33);
            if(stale==2){
                f.appended[1]=(ForwardCell){112,27,0,0};
                f.extra.object_id=43;*(ObjectHeader *)(f.f.field.heap+80)=(ObjectHeader){24,1,(uintptr_t)&f.extra,GC_OBJECT_PUBLISHED};
                atomic_store(&f.f.field.page.allocated_top,144);atomic_store(&f.f.table.used,2);
            }
            memcpy(&before,&f,sizeof(f));assert(step(&f.pool,&f.q,GC_HA_META)==GC_HA_RESTART&&!memcmp(&f,&before,sizeof(f)));
            assert(step(&f.pool,&f.q,GC_HA_CANCEL)==GC_HA_CANCELLED&&!f.pool.active&&!f.q.walk.lease.token);
        }
        heap_audit_owner_fixture(&f,ref);assert(step(&f.pool,&f.q,GC_HA_META)==GC_HA_MORE);
        assert(step(&f.pool,&f.q,GC_HA_META)==GC_HA_SCAN_READY);
        assert(step(&f.pool,&f.q,GC_HA_SCAN)==GC_HA_FOUND&&f.q.stage==3);
        f.q.findings=UINT64_MAX;memcpy(&before,&f,sizeof(f));
        assert(step(&f.pool,&f.q,GC_HA_CONSUME)==GC_HA_EXHAUSTED&&!memcmp(&f,&before,sizeof(f)));
        // Cleanup can retry after a metadata control failure without losing its token.
        f.pool.free_head=1;memcpy(&before,&f,sizeof(f));
        assert(step(&f.pool,&f.q,GC_HA_CANCEL)==GC_HA_CLEANUP_REQUIRED&&!memcmp(&f,&before,sizeof(f)));
        f.pool.free_head=0;assert(step(&f.pool,&f.q,GC_HA_CANCEL)==GC_HA_CANCELLED&&!f.pool.active);
        // A guard awaiting EXIT takes priority over cancellation and META.
        heap_audit_owner_fixture(&f,ref);assert(step(&f.pool,&f.q,GC_HA_META)==GC_HA_MORE);
        assert(step(&f.pool,&f.q,GC_HA_META)==GC_HA_SCAN_READY);
        f.q.slice.access=(ResolveRequest){.reference=f.q.slice.reference,.scope={.context=f.q.context}};
        assert(gc_resolve_access(&f.f.field.page,&f.q.slice.access,GC_RESOLVE_ENTER)==GC_RESOLVE_OK);
        f.q.slice.stage=1;f.q.slice.pending_result=GC_HAS_FOUND;f.q.slice.pending_observed=ref;
        f.q.slice.page=0;memcpy(&before,&f,sizeof(f));
        for(unsigned op=0;op<4;op++)assert(step(&f.pool,&f.q,op)==GC_HA_EXIT_REQUIRED&&!memcmp(&f,&before,sizeof(f)));
        f.q.slice.page=(uintptr_t)&f.f.field.page;assert(step(&f.pool,&f.q,GC_HA_SCAN)==GC_HA_FOUND);
        assert(step(&f.pool,&f.q,GC_HA_CANCEL)==GC_HA_CANCELLED&&!f.pool.active);
        // An unresolved/busy source cannot consume its object key or lease.
        heap_audit_owner_fixture(&f,ref);assert(step(&f.pool,&f.q,GC_HA_META)==GC_HA_MORE);
        assert(step(&f.pool,&f.q,GC_HA_META)==GC_HA_SCAN_READY);
        atomic_store(&f.f.field.cell.word,5);memcpy(&before,&f,sizeof(f));
        assert(step(&f.pool,&f.q,GC_HA_SCAN)==GC_HA_WAIT&&!memcmp(&f,&before,sizeof(f)));
        atomic_store(&f.f.field.cell.word,0);atomic_store(&f.f.field.cell.access,GC_SOURCE_CLOSED-1);
        memcpy(&before,&f,sizeof(f));
        assert(step(&f.pool,&f.q,GC_HA_SCAN)==GC_HA_EXHAUSTED&&!memcmp(&f,&before,sizeof(f)));
        atomic_store(&f.f.field.cell.access,0);
        assert(heap_audit_owner_finish(&f,step)==GC_HA_DIRTY&&f.q.findings==1);
        // Phase adoption after META must fail the payload step without leaking
        // its transient guard; CANCEL remains possible in the new phase.
        heap_audit_owner_fixture(&f,ref);assert(step(&f.pool,&f.q,GC_HA_META)==GC_HA_MORE);
        assert(step(&f.pool,&f.q,GC_HA_META)==GC_HA_SCAN_READY);
        atomic_store(&f.f.field.ctx.adopted_phase,0x33);memcpy(&before,&f,sizeof(f));
        assert(step(&f.pool,&f.q,GC_HA_SCAN)==GC_HA_INVALID&&!memcmp(&f,&before,sizeof(f)));
        assert(step(&f.pool,&f.q,GC_HA_CANCEL)==GC_HA_CANCELLED&&!f.pool.active);
        for(unsigned mismatch=0;mismatch<9;mismatch++){
            heap_audit_owner_fixture(&f,ref);assert(step(&f.pool,&f.q,GC_HA_META)==GC_HA_MORE);
            assert(step(&f.pool,&f.q,GC_HA_META)==GC_HA_SCAN_READY);
            switch(mismatch){case 0:f.q.slice.page=0;break;case 1:f.q.slice.reference+=UINT64_C(16)<<16;break;
            case 2:f.q.slice.context=0;break;case 3:f.q.slice.registry=0;break;case 4:f.q.slice.phase=0x33;break;
            case 5:f.q.slice.target_start=144;break;case 6:f.q.slice.target_limit=176;break;
            case 7:f.q.object.lease=0;break;case 8:f.q.object.incarnation++;break;}
            memcpy(&before,&f,sizeof(f));assert(step(&f.pool,&f.q,GC_HA_SCAN)==GC_HA_INVALID&&!memcmp(&f,&before,sizeof(f)));
            assert(step(&f.pool,&f.q,GC_HA_CANCEL)==GC_HA_CANCELLED&&!f.pool.active);
        }
        for(unsigned fault=0;fault<12;fault++){
            heap_audit_owner_fixture(&f,ref);uint64_t op=GC_HA_META,result=GC_HA_INVALID;
            switch(fault){case 0:op=4;break;case 1:f.q.stage=5;break;case 2:f.q.context=0;break;
            case 3:f.q.registry=0;break;case 4:f.q.gate=0;break;case 5:f.q.epoch=0;break;
            case 6:f.q.target_start=0;break;case 7:f.q.target_limit=128;break;case 8:f.q.phase=8;break;
            case 9:f.q.walk.pool=0;break;case 10:f.gate.closed=0;result=GC_HA_WAIT;break;case 11:op=GC_HA_SCAN;break;}
            memcpy(&before,&f,sizeof(f));assert(step(&f.pool,&f.q,op)==result&&!memcmp(&f,&before,sizeof(f)));
        }
    }
    puts("heap audit owner: C/candidate clean and dirty completion, restart, pressure and retained cancellation cases PASS");
}
#endif
