#ifndef KLASSIC_GC_HEAP_AUDIT_SLICE_CASES_H
#define KLASSIC_GC_HEAP_AUDIT_SLICE_CASES_H
#include "gc_heap_audit_slice.h"
#include "gc_barrier_cases.h"
typedef struct {StoreFixture field;ForwardTable table;HeapAuditSlice q;} HeapAuditFixture;
static void heap_audit_fixture(HeapAuditFixture *f,uint64_t value,uint64_t cursor){
    memset(f,0,sizeof(*f));store_fixture(&f->field,0x22,GC_OLD,value);
    f->table=(ForwardTable){(uintptr_t)&f->field.cell,1,1};f->field.page.forward_table=(uintptr_t)&f->table;
    atomic_store(&f->field.ctx.access_epoch,0);atomic_store(&f->field.cell.access,0);
    f->q=(HeapAuditSlice){.page=(uintptr_t)&f->field.page,.context=(uintptr_t)&f->field.ctx,
        .reference=(UINT64_C(48)<<16)|0x1510,.registry=(uintptr_t)&f->field.registry,
        .phase=0x22,.target_start=128,.target_limit=160,.cursor=cursor};
}
static void gc_run_heap_audit_slice_cases(HeapAuditSlicer candidate){
    const uint64_t ref=(UINT64_C(128)<<16)|0x1510;
    for(unsigned impl=0;impl<2;impl++){
        HeapAuditSlicer step=impl?candidate:gc_heap_audit_slice;HeapAuditFixture f,before;
        assert(step(NULL)==GC_HAS_INVALID&&step((void *)1)==GC_HAS_INVALID);
        for(unsigned value=0;value<3;value++)for(unsigned cursor=0;cursor<=24;cursor+=8){
            heap_audit_fixture(&f,value==0?0:value==1?ref:(UINT64_C(192)<<16)|0x1510,cursor);
            uint64_t expected=cursor>=16?GC_HAS_DONE:cursor==0&&value==1?GC_HAS_FOUND:GC_HAS_MORE;
            memcpy(&before,&f,sizeof(f));assert(step(&f.q)==expected);
            before.q.cursor=expected==GC_HAS_FOUND?cursor:cursor>=16?24:cursor+8;
            before.q.observed=expected==GC_HAS_FOUND?ref:0;
            assert(!memcmp(&f,&before,sizeof(f)));
        }
        for(unsigned fault=0;fault<20;fault++){
            heap_audit_fixture(&f,ref,0);uint64_t expected=GC_HAS_INVALID;
            switch(fault){
            case 0:f.q.page=0;break;case 1:f.q.context=0;break;case 2:f.q.reference=1;break;
            case 3:f.q.registry=0;break;case 4:f.q.phase=0;break;case 5:f.q.target_start=0;break;
            case 6:f.q.cursor=1;break;case 7:f.q.observed=1;break;case 8:f.q.stage=2;break;
            case 9:f.q.pending_cursor=1;break;case 10:f.q.pending_observed=1;break;case 11:f.q.pending_result=1;break;
            case 12:f.q.access.scope.held=1;break;
            case 13:atomic_store(&f.field.cell.word,ref|2);atomic_store(&f.field.cell.access,GC_SOURCE_CLOSED);atomic_store(&f.field.page.state,GC_PAGE_UNCOMMITTED);expected=GC_HAS_FORWARDED;break;
            case 14:atomic_store(&f.field.cell.word,5);expected=GC_HAS_WAIT;break;
            case 15:atomic_store(&f.field.cell.access,GC_SOURCE_CLOSED);expected=GC_HAS_WAIT;break;
            case 16:atomic_store(&f.field.cell.access,GC_SOURCE_CLOSED-1);expected=GC_HAS_EXHAUSTED;break;
            case 17:atomic_store((_Atomic uint64_t *)(f.field.heap+48),1);break;
            case 18:atomic_store(&((ObjectHeader *)(f.field.heap+16))->state,GC_OBJECT_INITIALIZING);break;
            case 19:f.q.page++;break;
            }
            memcpy(&before,&f,sizeof(f));assert(step(&f.q)==expected&&!memcmp(&f,&before,sizeof(f)));
        }
        for(unsigned kind=0;kind<4;kind++)for(unsigned fault=0;fault<3;fault++){
            heap_audit_fixture(&f,kind==1?ref:0,kind==2?24:kind==3?1:0);
            f.q.access=(ResolveRequest){.reference=f.q.reference,.scope={.context=f.q.context}};
            assert(gc_resolve_access(&f.field.page,&f.q.access,GC_RESOLVE_ENTER)==GC_RESOLVE_OK);
            HeapIncoming scan={(uintptr_t)&f.q.access.scope,f.q.page,f.q.registry,f.q.phase,
                f.q.target_start,f.q.target_limit,f.q.cursor,0};
            f.q.stage=1;f.q.pending_result=gc_heap_incoming(&scan);
            f.q.pending_cursor=scan.cursor;f.q.pending_observed=scan.observed;
            uint64_t expected=f.q.pending_result;
            if(fault==0)f.q.page=0;
            if(fault==1)atomic_store(&f.field.ctx.state,GC_BLOCKED_NATIVE);
            if(fault==2)atomic_store(&f.field.cell.access,0);
            memcpy(&before,&f,sizeof(f));
            assert(step(&f.q)==GC_HAS_EXIT_REQUIRED&&!memcmp(&f,&before,sizeof(f)));
            assert(f.q.stage==1&&f.q.access.scope.held&&!f.q.observed);
            f.q.page=(uintptr_t)&f.field.page;atomic_store(&f.field.ctx.state,GC_RUNNING);atomic_store(&f.field.cell.access,1);
            memcpy(&before,&f,sizeof(f));assert(step(&f.q)==expected);
            if(expected!=GC_HAS_INVALID){before.q.cursor=scan.cursor;before.q.observed=scan.observed;}
            before.q.access=(ResolveRequest){0};before.q.stage=before.q.pending_cursor=before.q.pending_observed=before.q.pending_result=0;
            atomic_store(&before.field.ctx.access_epoch,0);atomic_store(&before.field.cell.access,0);
            assert(!memcmp(&f,&before,sizeof(f)));
        }
    }
    puts("heap audit slice: C/candidate bounded scans, discarded forwarding and retained exit ownership PASS");
}
#endif
