#ifndef KLASSIC_GC_ROOT_CANCEL_CASES_H
#define KLASSIC_GC_ROOT_CANCEL_CASES_H
#include "gc_roots_cases.h"
typedef uint64_t (*RootCancelEmpty)(MutatorContext *,const RootRequest *);
static void gc_run_root_cancel_cases(RootCancelEmpty cancel){
    RootFixture *f=malloc(sizeof(*f)),*expected=malloc(sizeof(*expected));assert(f&&expected);
    unsigned cases=0;
    for(unsigned pressure=0;pressure<4;pressure++){
        root_fixture(f);assert(gc_root_step(&f->ctx,&f->request,GC_ROOT_RESERVE)==GC_ROOT_OK);
        f->request.value=0xabc;
        if(pressure&1){f->buffer.top=7;atomic_store(&f->buffer.published,7);memset(f->entries,0x5a,sizeof(f->entries));}
        if(pressure&2)atomic_store(&f->ctx.root_revision,UINT64_MAX);
        memcpy(expected,f,sizeof(*f));expected->cells[0].active=0;
        assert(cancel(&f->ctx,&f->request)==GC_ROOT_OK);
        assert(!memcmp(f,expected,sizeof(*f)));cases++;
        assert(cancel(&f->ctx,&f->request)==GC_ROOT_INVALID);
        assert(!memcmp(f,expected,sizeof(*f)));cases++;
        assert(gc_root_step(&f->ctx,&f->request,GC_ROOT_READ_RAW)==GC_ROOT_INVALID);
    }
    for(unsigned fault=0;fault<12;fault++){
        root_fixture(f);assert(gc_root_step(&f->ctx,&f->request,GC_ROOT_RESERVE)==GC_ROOT_OK);
        switch(fault){
        case 0:f->ctx.abi_version++;break;case 1:f->ctx.thread_id++;break;
        case 2:atomic_store(&f->ctx.state,GC_BLOCKED_NATIVE);break;case 3:atomic_store(&f->ctx.access_epoch,1);break;
        case 4:f->request.handle.scope++;break;case 5:f->request.handle.cell+=sizeof(RootCell);break;
        case 6:f->request.handle.cell++;break;case 7:f->cells[0].active=0;break;
        case 8:f->request.handle.reuse_generation=0;break;case 9:f->request.handle.reuse_generation++;break;
        case 10:atomic_store(&f->cells[0].value,0x12340000);break;
        case 11:atomic_store(&f->segment.state,GC_ROOT_SEGMENT_RETIRING);break;
        }
        memcpy(expected,f,sizeof(*f));
        assert(cancel(&f->ctx,&f->request)==GC_ROOT_INVALID);
        assert(!memcmp(f,expected,sizeof(*f)));cases++;
    }
    assert(cases==20);free(expected);free(f);
    puts("root cancel: 20 empty-root pressure and stale/nonempty rejection cases preserve journal and scan bounds PASS");
}
#endif
