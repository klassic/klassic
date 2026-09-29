#ifndef KLASSIC_GC_ROOT_PIN_CASES_H
#define KLASSIC_GC_ROOT_PIN_CASES_H
#include "gc_root_pin.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
typedef struct {
    MutatorContext ctx;RootSegment roots;RootCell cells[2];BufferCursor journal;
    RootChange changes[8];RootRequest input,output;
    HeapPool heap;AllocationPage pages[2];ForwardTable tables[2];ForwardCell forwards[2];
    _Atomic uint64_t starts[2][128];MetadataLeasePool pool;MetadataLeaseSlot slots[6];
    PinPageRecord records[2];PinPageRegistry registry;RootPin pin;
} RootPinFixture;
static void gc_root_pin_callback(RootPinFixture *f){
    assert(atomic_load(&f->ctx.state)==GC_BLOCKED_NATIVE);
    atomic_store(&f->ctx.state,GC_RUNNING);
    RootRequest read=f->input;
    assert(gc_root_step(&f->ctx,&read,GC_ROOT_READ_RAW)==GC_ROOT_OK&&!read.value);
    uint64_t values[2]={0};
    RootScan scan={.segment=(uintptr_t)&f->roots,.output=(uintptr_t)values,.capacity=2};
    assert(gc_root_scan(&scan)==GC_ROOT_OK&&scan.written==2);
    assert(!values[0]&&values[1]==f->pin.reference);
    assert(!atomic_load(&f->ctx.access_epoch)&&f->pin.address);
    (*(uint64_t *)(uintptr_t)f->pin.address)++;
    atomic_store(&f->ctx.state,GC_BLOCKED_NATIVE);
}
static void gc_root_pin_fixture(RootPinFixture *f){
    memset(f,0,sizeof(*f));
    f->ctx=(MutatorContext){.abi_version=GC_ABI_VERSION,.thread_id=17,.adopted_phase=0x10000};
    f->journal=(BufferCursor){.entries=(uintptr_t)f->changes,.capacity=8};
    f->roots=(RootSegment){.owner=17,.scope=101,.cells=(uintptr_t)f->cells,.capacity=2,
        .state=GC_ROOT_SEGMENT_ACTIVE,.journal=(uintptr_t)&f->journal};
    f->input.segment=f->output.segment=(uintptr_t)&f->roots;
    assert(gc_root_step(&f->ctx,&f->input,GC_ROOT_RESERVE)==GC_ROOT_OK);
    assert(gc_root_step(&f->ctx,&f->output,GC_ROOT_RESERVE)==GC_ROOT_OK);
    f->heap=(HeapPool){.pages=(uintptr_t)f->pages,.page_count=2,.heap_base=UINT64_C(0x100000000)};
    f->pool=(MetadataLeasePool){(uintptr_t)&f->heap,(uintptr_t)f->slots,6,1,0};
    f->registry=(PinPageRegistry){&f->pool,f->records,2};
    for(unsigned i=0;i<6;i++)f->slots[i].next_free=i==5?0:i+2;
    for(unsigned i=0;i<2;i++){
        uint64_t start=(i+1)*GC_HEAP_PAGE_BYTES;
        f->pages[i]=(AllocationPage){.heap_base=f->heap.heap_base,.start=start,.limit=start+GC_HEAP_PAGE_BYTES,
            .incarnation=27+i,.identity_base=1024+i,.state=GC_PAGE_CLOSED,.run_head=i,
            .starts=(uintptr_t)f->starts[i],.starts_words=128,.allocated_top=start+48,
            .forward_table=(uintptr_t)&f->tables[i]};
        atomic_store(&f->starts[i][0],4);
        f->forwards[i]=(ForwardCell){.source_offset=start+32,.incarnation=27+i};
        f->tables[i]=(ForwardTable){(uintptr_t)&f->forwards[i],1,1};
        MetadataLeaseRequest lease={(uintptr_t)&f->pages[i],27+i,0};
        assert(gc_metadata_step_locked(&f->pool,&lease,GC_ML_ACQUIRE)==GC_ML_OK);
        assert(gc_pin_registry_bind_locked(&f->registry,&lease)==GC_RG_OK);
        assert(gc_metadata_step_locked(&f->pool,&lease,GC_ML_RELEASE)==GC_ML_OK);
    }
    f->input.value=((GC_HEAP_PAGE_BYTES+32)<<16)|0x1510;
    assert(gc_root_step(&f->ctx,&f->input,GC_ROOT_STORE)==GC_ROOT_OK);
}
typedef struct {
    uint64_t (*prepare)(RootPin *,const RootPinStart *);
    uint64_t (*enter)(RootPin *),(*exit)(RootPin *),(*cleanup)(RootPin *),(*cancel)(RootPin *),(*finish)(RootPin *);
} RootPinOps;
static inline RootPinOps gc_root_pin_c_ops(void){
    return (RootPinOps){gc_root_pin_prepare_entry,gc_root_pin_enter_locked,gc_root_pin_exit,
        gc_root_pin_cleanup_locked,gc_root_pin_cancel_locked,gc_root_pin_finish};
}
static void gc_run_root_pin_cases(RootPinOps ops){
    for(unsigned scenario=0;scenario<9;scenario++){
        RootPinFixture f;gc_root_pin_fixture(&f);
        void *mapping=aligned_alloc(16,3*GC_HEAP_PAGE_BYTES);assert(mapping);
        f.heap.heap_base=(uintptr_t)mapping;
        for(unsigned i=0;i<2;i++){
            f.pages[i].heap_base=(uintptr_t)mapping;
            *(uint64_t *)((char *)mapping+(i+1)*GC_HEAP_PAGE_BYTES+32)=100+i;
        }
        if(scenario==6){f.input.value=0;assert(gc_root_step(&f.ctx,&f.input,GC_ROOT_STORE)==GC_ROOT_OK);}
        if(scenario==7){
            f.journal.top=8;atomic_store(&f.journal.published,8);
            RootPin zero={0};uint64_t original=atomic_load(&f.cells[0].value);
            assert(ops.prepare(&f.pin,&(RootPinStart){(uintptr_t)&f.registry,(uintptr_t)&f.ctx,(uintptr_t)&f.input,(uintptr_t)&f.output})==GC_RPIN_FLUSH);
            assert(!memcmp(&f.pin,&zero,sizeof(zero))&&atomic_load(&f.cells[0].value)==original);
            assert(f.cells[1].active&&!atomic_load(&f.cells[1].value));
            atomic_store(&f.journal.consumed,8);
        }
        assert(ops.prepare(&f.pin,&(RootPinStart){(uintptr_t)&f.registry,(uintptr_t)&f.ctx,(uintptr_t)&f.input,(uintptr_t)&f.output})==GC_RPIN_MORE);
        RootPin copied=f.pin;
        assert(ops.enter(&copied)==GC_RPIN_INVALID);
        /* The original root can change immediately after successful adoption. */
        f.input.value=0;assert(gc_root_step(&f.ctx,&f.input,GC_ROOT_STORE)==GC_ROOT_OK);
        if(scenario==6){
            assert(ops.enter(&f.pin)==GC_RPIN_EMPTY);
        }else{
            unsigned destination=(scenario==1||scenario==8)?1:0;
            if(scenario==8){
                atomic_store(&f.forwards[0].word,5);
                assert(ops.enter(&f.pin)==GC_RPIN_WAIT);
                assert(!f.pin.address&&!atomic_load(&f.ctx.access_epoch)&&f.pool.active==2);
            }
            if(scenario==1||scenario==8){
                atomic_store(&f.forwards[0].word,((2*GC_HEAP_PAGE_BYTES+32)<<16)|0x1512);
                assert(ops.enter(&f.pin)==GC_RPIN_MORE);
                assert((atomic_load(&f.cells[1].value)>>16)==2*GC_HEAP_PAGE_BYTES+32);
                assert(!atomic_load(&f.ctx.access_epoch)&&f.pool.active==2);
            }
            RelocationGateRequest selection={0};
            MetadataLeaseRequest lifetime=f.records[destination].lifetime;
            if(scenario==2)assert(gc_pin_registry_step_locked(&f.registry,&lifetime,&selection,GC_PP_SELECT)==GC_RG_OK);
            assert(ops.enter(&f.pin)==GC_RPIN_GUARDED);
            assert(!f.pin.address&&atomic_load(&f.ctx.access_epoch));
            assert(ops.cancel(&f.pin)==GC_RPIN_INVALID);
            if(scenario==2){
                assert(ops.exit(&f.pin)==GC_RPIN_CLEANUP);
                assert(!atomic_load(&f.ctx.access_epoch)&&!f.pin.address);
                assert(ops.cleanup(&f.pin)==GC_RPIN_WAIT);
                assert(f.pool.active==2);
                assert(gc_pin_registry_step_locked(&f.registry,&lifetime,&selection,GC_PP_REOPEN)==GC_RG_OK);
                assert(ops.enter(&f.pin)==GC_RPIN_GUARDED);
            }
            assert(ops.exit(&f.pin)==GC_RPIN_READY);
            assert(!atomic_load(&f.ctx.access_epoch)&&!atomic_load(&f.forwards[destination].access));
            assert(f.pin.address==f.heap.heap_base+(destination+1)*GC_HEAP_PAGE_BYTES+32);
            assert(gc_pin_registry_step_locked(&f.registry,&lifetime,&selection,GC_PP_SELECT)==GC_RG_WAIT);
            /* A native interval and callback need no ordinary access guard. */
            atomic_store(&f.ctx.state,GC_BLOCKED_NATIVE);
            assert(f.pin.address&&f.records[destination].admission.active==1);
            gc_root_pin_callback(&f);
            assert(*(uint64_t *)(uintptr_t)f.pin.address==101+destination);
            atomic_store(&f.ctx.state,GC_RUNNING);
            if(scenario==3){
                f.journal.top=8;atomic_store(&f.journal.published,8);
            }
            if(scenario==4){
                uint64_t free_head=f.pool.free_head;f.pool.free_head=0;
                uint64_t address=f.pin.address;
                assert(ops.cancel(&f.pin)==GC_RPIN_CLEANUP);
                assert(f.pin.address==address&&f.records[destination].admission.active==1);
                f.pool.free_head=free_head;
            }
            if(scenario==5)atomic_store(&f.ctx.state,GC_BLOCKED_NATIVE);
        }
        assert(ops.cancel(&f.pin)==GC_RPIN_MORE);
        assert(!f.pin.address&&f.pool.active==2&&!f.records[0].admission.active&&!f.records[1].admission.active);
        if(scenario==3){
            assert(ops.finish(&f.pin)==GC_RPIN_FLUSH);
            assert(f.cells[1].active&&atomic_load(&f.cells[1].value));
            atomic_store(&f.journal.consumed,8);
        }
        if(scenario==5){
            assert(ops.finish(&f.pin)==GC_RPIN_INVALID);
            atomic_store(&f.ctx.state,GC_RUNNING);
        }
        assert(ops.finish(&f.pin)==GC_RPIN_DONE);
        assert(!f.cells[1].active&&!atomic_load(&f.cells[1].value));
        assert(ops.finish(&f.pin)==GC_RPIN_INVALID);
        for(unsigned i=0;i<2;i++){
            MetadataLeaseRequest borrowed=f.records[i].lifetime;
            assert(gc_pin_registry_unbind_locked(&f.registry,&borrowed)==GC_RG_OK);
            assert(!f.pages[i].metadata_leases);
        }
        assert(!f.pool.active);
        free(mapping);
    }
    puts("root pin: canonical forwarding, independent root, guard exit, native interval and resumable cleanup PASS");
}
#endif
