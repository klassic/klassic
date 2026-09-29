#ifndef KLASSIC_GC_ROOT_STORAGE_CASES_H
#define KLASSIC_GC_ROOT_STORAGE_CASES_H
#include "gc_root_storage.h"
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
static void gc_run_root_storage_cases(RootStoragePreparer prepare){
    unsigned char *memory=malloc(16384),*expected=malloc(16384);assert(memory&&expected);
    RootStorage *storage=(void *)(memory+8);unsigned cases=0;
    const uint64_t counts[]={1,3,17,256},entries[]={1,2,7};
    for(unsigned i=0;i<4;i++)for(unsigned j=0;j<3;j++){
        memset(memory,0,16384);memset(memory,0xa5,8);memset(memory+16376,0xa5,8);
        RootStorageSpec spec={17,counts[i],entries[j],sizeof(*storage)+(counts[i]+entries[j])*32},before=spec;
        assert(gc_root_storage_prepare(storage,&spec)==GC_ROOT_STORAGE_OK);
        memcpy(expected,memory,16384);memset(memory+8,0,16368);
        assert(prepare(storage,&spec)==GC_ROOT_STORAGE_OK);
        assert(!memcmp(memory,expected,16384)&&!memcmp(&spec,&before,sizeof(spec)));
        assert(storage->registration.segment==(uintptr_t)&storage->segment);
        assert(storage->segment.cells==(uintptr_t)(storage+1));
        assert(storage->journal.entries==(uintptr_t)(storage+1)+counts[i]*32);
        assert(!storage->registration.registered&&!storage->segment.scope);
        cases++;
    }
    for(unsigned fault=0;fault<9;fault++){
        memset(memory,0,16384);memset(memory,0xa5,8);memset(memory+16376,0xa5,8);
        RootStorageSpec spec={17,3,2,sizeof(*storage)+160};RootStorage *argument=storage;
        switch(fault){
        case 0:spec.owner=0;break;case 1:spec.cells=0;break;case 2:spec.entries=0;break;
        case 3:spec.cells=UINT64_C(0x100000000);break;case 4:spec.entries=UINT64_C(0x100000000);break;
        case 5:spec.bytes--;break;case 6:spec.bytes=UINT64_MAX;break;
        case 7:argument=NULL;break;case 8:argument=(void *)(memory+9);break;
        }
        RootStorageSpec before=spec;memcpy(expected,memory,16384);
        assert(prepare(argument,&spec)==GC_ROOT_STORAGE_INVALID);
        assert(!memcmp(memory,expected,16384)&&!memcmp(&spec,&before,sizeof(spec)));cases++;
    }
    assert(cases==21);free(expected);free(memory);
    puts("root storage: 21 fresh native layout and unchanged invalid-size cases PASS");
}
static void gc_run_root_storage_disposal_cases(RootStorageDisposalCheck check){
    RootStorage storage={0},before;unsigned cases=0;
    for(unsigned state=0;state<3;state++){
        memset(&storage,0,sizeof(storage));
        if(state)storage.registration.segment=(uintptr_t)&storage.segment;
        if(state==2){storage.journal.top=UINT64_MAX;atomic_store(&storage.journal.published,UINT64_MAX);atomic_store(&storage.journal.consumed,UINT64_MAX);}
        memcpy(&before,&storage,sizeof(storage));
        assert(check(&storage)==GC_ROOT_STORAGE_OK&&!memcmp(&storage,&before,sizeof(storage)));cases++;
    }
    size_t offsets[27];unsigned n=0;
    for(size_t off=0;off<sizeof(RootRegistration);off+=8)offsets[n++]=off;
    offsets[n++]=offsetof(RootStorage,segment.scope);offsets[n++]=offsetof(RootStorage,segment.registration);
    offsets[n++]=offsetof(RootStorage,segment.used);offsets[n++]=offsetof(RootStorage,segment.state);
    offsets[n++]=offsetof(RootStorage,journal.top);offsets[n++]=offsetof(RootStorage,journal.published);
    offsets[n++]=offsetof(RootStorage,journal.consumed);assert(n==27);
    for(unsigned i=0;i<n;i++){
        memset(&storage,0,sizeof(storage));uint64_t live=1;
        memcpy((unsigned char *)&storage+offsets[i],&live,8);memcpy(&before,&storage,sizeof(storage));
        assert(gc_root_storage_can_dispose(&storage)==GC_ROOT_STORAGE_INVALID);
        assert(check(&storage)==GC_ROOT_STORAGE_INVALID&&!memcmp(&storage,&before,sizeof(storage)));cases++;
    }
    assert(check(NULL)==GC_ROOT_STORAGE_INVALID);cases++;
    assert(check((const void *)((const unsigned char *)&storage+1))==GC_ROOT_STORAGE_INVALID);cases++;
    assert(!memcmp(&storage,&before,sizeof(storage))&&cases==32);
    puts("root disposal check: 32 private, drained and outstanding-obligation cases preserve storage PASS");
}
#endif
