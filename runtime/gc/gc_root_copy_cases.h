#ifndef KLASSIC_GC_ROOT_COPY_CASES_H
#define KLASSIC_GC_ROOT_COPY_CASES_H
#include "gc_roots_cases.h"
#include "gc_root_copy.h"
static void gc_run_root_copy_cases(RootCopier candidate){
    RootFixture *f=malloc(sizeof(*f)),*expected=malloc(sizeof(*expected));assert(f&&expected);
    unsigned cases=0;
    for(unsigned implementation=0;implementation<2;implementation++)for(unsigned test=0;test<24;test++){
        RootCopier copy=implementation?candidate:gc_root_copy;
        root_fixture(f);
        assert(gc_root_step(&f->ctx,&f->request,GC_ROOT_RESERVE)==GC_ROOT_OK);
        RootRequest source=f->request;
        assert(gc_root_step(&f->ctx,&f->request,GC_ROOT_RESERVE)==GC_ROOT_OK);
        atomic_store(&f->cells[0].value,UINT64_C(0x401510));
        atomic_store(&f->cells[1].value,UINT64_C(0x801510));
        source.value=111;f->request.value=222; // stale caller scratch, never copied
        const RootRequest *src=&source,*dst=&f->request;MutatorContext *ctx=&f->ctx;
        uint64_t result=test<5?GC_ROOT_OK:GC_ROOT_INVALID,target=1;
        switch(test){
        case 0:break;
        case 1:atomic_store(&f->cells[0].value,0);break;
        case 2:atomic_store(&f->cells[1].value,0);break;
        case 3:f->request.handle=source.handle;target=0;break;
        case 4:dst=src;target=0;break;
        case 5:f->buffer.top=7;atomic_store(&f->buffer.published,7);result=GC_ROOT_FLUSH;break;
        case 6:f->ctx.abi_version--;break;
        case 7:atomic_store(&f->ctx.state,GC_BLOCKED_NATIVE);break;
        case 8:atomic_store(&f->ctx.access_epoch,1);break;
        case 9:source.handle.reuse_generation++;break;
        case 10:f->request.handle.reuse_generation++;break;
        case 11:atomic_store(&f->segment.state,GC_ROOT_SEGMENT_RETIRING);break;
        case 12:source.handle.scope++;break;
        case 13:f->request.handle.scope++;break;
        case 14:atomic_store(&f->buffer.published,1);break;
        case 15:atomic_store(&f->ctx.root_revision,UINT64_MAX);result=GC_ROOT_EXHAUSTED;break;
        case 16:f->buffer.top=UINT64_MAX;atomic_store(&f->buffer.published,UINT64_MAX);atomic_store(&f->buffer.consumed,UINT64_MAX);result=GC_ROOT_EXHAUSTED;break;
        case 17:f->ctx.thread_id++;break;
        case 18:f->cells[0].active=0;break;
        case 19:f->cells[1].active=0;break;
        case 20:src=NULL;break;
        case 21:dst=NULL;break;
        case 22:ctx=NULL;break;
        case 23:dst=(void *)((uintptr_t)dst+1);break;
        }
        memcpy(expected,f,sizeof(*f));RootRequest saved_source=source;
        if(result==GC_ROOT_OK){
            uint64_t next=atomic_load(&f->cells[0].value),previous=atomic_load(&f->cells[target].value);
            expected->entries[0]=(RootChange){previous,next,101,atomic_load(&f->ctx.adopted_phase)};
            expected->buffer.top=1;atomic_store(&expected->buffer.published,1);
            atomic_store(&expected->cells[target].value,next);atomic_store(&expected->ctx.root_revision,1);
        }
        assert(copy(ctx,src,dst)==result);
        assert(!memcmp(f,expected,sizeof(*f))&&!memcmp(&source,&saved_source,sizeof(source)));cases++;
        if(test==5){
            // Service pressure and replace the protected source before retry.
            // The next invocation must reload its cell, not stale request.value.
            atomic_store(&f->buffer.consumed,7);
            RootRequest replacement=source;replacement.value=UINT64_C(0xc01510);
            assert(gc_root_step(&f->ctx,&replacement,GC_ROOT_STORE)==GC_ROOT_OK);
            memcpy(expected,f,sizeof(*f));
            expected->entries[1]=(RootChange){UINT64_C(0x801510),UINT64_C(0xc01510),101,atomic_load(&f->ctx.adopted_phase)};
            expected->buffer.top=9;atomic_store(&expected->buffer.published,9);
            atomic_store(&expected->cells[1].value,UINT64_C(0xc01510));atomic_store(&expected->ctx.root_revision,2);
            assert(copy(ctx,src,dst)==GC_ROOT_OK);
            assert(!memcmp(f,expected,sizeof(*f))&&!memcmp(&source,&saved_source,sizeof(source)));cases++;
        }
    }
    assert(cases==50);free(expected);free(f);
    puts("root copy: 50 C/candidate journal, alias, null and pressure-retry cases PASS");
}
#endif
