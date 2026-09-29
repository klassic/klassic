#ifndef KLASSIC_GC_LOGICAL_CAS_CASES_H
#define KLASSIC_GC_LOGICAL_CAS_CASES_H
#include "gc_barrier_cases.h"
static void gc_run_identity_cases(GuardedIdentity candidate){
    StoreFixture *f=malloc(sizeof(*f)),*before=malloc(sizeof(*before));assert(f&&before);
    unsigned count=0;
    for(unsigned impl=0;impl<2;impl++)for(unsigned gen=0;gen<2;gen++)for(unsigned fault=0;fault<16;fault++){
        store_fixture(f,0x22,gen,0);
        uint64_t scope=(uintptr_t)&f->scope,page=(uintptr_t)&f->page;
        ObjectHeader *h=(void *)(f->heap+16);
        switch(fault){
        case 0:break;
        case 1:atomic_store(&f->cell.word,3);break;
        case 2:atomic_store(&f->cell.access,GC_SOURCE_CLOSED|1);break;
        case 3:h->mark_record=0;break;
        case 4:h->mark_record++;break;
        case 5:f->record.object_id=0;break;
        case 6:f->scope.held=0;break;
        case 7:atomic_store(&f->ctx.access_epoch,0);break;
        case 8:atomic_store(&f->ctx.adopted_phase,0);break;
        case 9:atomic_store(&h->state,GC_OBJECT_INITIALIZING);break;
        case 10:f->page.incarnation++;break;
        case 11:atomic_store(&f->ctx.state,1);break;
        case 12:scope=0;break;
        case 13:page=0;break;
        case 14:scope++;break;
        case 15:page++;break;
        }
        memcpy(before,f,sizeof(*f));
        GuardedIdentity identity=impl?candidate:gc_guarded_identity;
        assert(identity(scope,page,0x22)==(fault<3?&f->record:NULL));
        assert(!memcmp(before,f,sizeof(*f)));count++;
    }
    assert(count==64);free(before);free(f);
    puts("guarded identity: 64 C/candidate canonical, closed-admission and invalid access cases PASS");
}
static void gc_run_block_size_cases(GuardedBlockSize candidate){
    StoreFixture *f=malloc(sizeof(*f)),*before=malloc(sizeof(*before));assert(f&&before);
    unsigned count=0;const uint64_t lengths[]={0,1,7,8,15,16,17,24,31,32};
    for(unsigned impl=0;impl<2;impl++)for(unsigned gen=0;gen<2;gen++){
        GuardedBlockSize size=impl?candidate:gc_guarded_block_size;
        for(unsigned i=0;i<10;i++){
            store_fixture(f,0x22,gen,0);ObjectHeader *h=(void *)(f->heap+16);h->payload_bytes=lengths[i];
            if(i%3==1)atomic_store(&f->cell.word,3);
            if(i%3==2){atomic_store(&f->cell.word,5);atomic_store(&f->cell.access,GC_SOURCE_CLOSED|1);}
            memcpy(before,f,sizeof(*f));
            assert(size((uintptr_t)&f->scope,(uintptr_t)&f->page,0x22)==(lengths[i]<=16?48:64));
            assert(!memcmp(before,f,sizeof(*f)));count++;
        }
        for(unsigned fault=0;fault<16;fault++){
            store_fixture(f,0x22,gen,0);ObjectHeader *h=(void *)(f->heap+16);
            uint64_t sp=(uintptr_t)&f->scope,pp=(uintptr_t)&f->page;
            switch(fault){case 0:f->ctx.abi_version=0;break;case 1:atomic_store(&f->ctx.adopted_phase,0);break;
            case 2:f->scope.held=0;break;case 3:atomic_store(&h->state,GC_OBJECT_INITIALIZING);break;
            case 4:f->page.incarnation++;break;case 5:f->page.generation=2;break;
            case 6:h->payload_bytes=33;break;case 7:h->payload_bytes=UINT64_MAX;break;
            case 8:sp=0;break;case 9:sp++;break;case 10:pp=0;break;case 11:pp++;break;
            case 12:f->scope.incarnation=0;break;case 13:atomic_store(&f->ctx.access_epoch,0);break;
            case 14:atomic_store(&f->page.allocated_top,48);break;case 15:atomic_store(&f->cell.word,0x601512);break;}
            memcpy(before,f,sizeof(*f));assert(!size(sp,pp,0x22));assert(!memcmp(before,f,sizeof(*f)));count++;
        }
    }
    assert(count==104);free(before);free(f);
    puts("guarded block size: 104 C/candidate empty, rounded, guarded and rejection cases PASS");
}
static void gc_run_logical_cas_cases(GuardedLogicalCas candidate){
    StoreFixture *f=malloc(sizeof(*f)),*before=malloc(sizeof(*before));assert(f&&before);
    unsigned count=0;const uint64_t colors[]={0,0x1510,0x2520,0x8630};
    for(unsigned implementation=0;implementation<2;implementation++){
        GuardedLogicalCas cas=implementation?candidate:gc_guarded_compare_exchange;
        for(unsigned phase=0;phase<2;phase++)for(unsigned gen=0;gen<2;gen++)
        for(unsigned color=0;color<4;color++)for(unsigned identity=0;identity<3;identity++){
            MarkRecord a={.object_id=17},b={.object_id=17},saved_a=a,saved_b=b;
            uint64_t observed=color?(UINT64_C(96)<<16)|colors[color]:0;
            store_fixture(f,phase?0x22:0,gen,observed);
            if(color==2)f->q.desired=0;
            const MarkRecord *actual=observed?&a:NULL,*expected=identity==0?NULL:identity==1?&a:&b;
            uint64_t status=actual==expected?GC_STORE_OK:GC_STORE_MISMATCH;
            memcpy(before,f,sizeof(*f));
            assert(cas(&f->q,expected,actual)==status);store_check(before,f,status);
            assert(!memcmp(&a,&saved_a,sizeof(a))&&!memcmp(&b,&saved_b,sizeof(b)));count++;
        }
        for(unsigned fault=0;fault<12;fault++){
            MarkRecord a={.object_id=17},b={.object_id=17};
            const MarkRecord *expected=&a,*actual=&a;uint64_t status=GC_STORE_INVALID;
            store_fixture(f,0x22,GC_OLD,(UINT64_C(96)<<16)|0x1510);
            switch(fault){
            case 0:expected=&b;f->ctx.satb.top=3;atomic_store(&f->ctx.satb.published,3);status=GC_STORE_MISMATCH;break;
            case 1:f->ctx.satb.top=3;atomic_store(&f->ctx.satb.published,3);status=GC_STORE_FULL;break;
            case 2:f->ctx.remembered.top=3;atomic_store(&f->ctx.remembered.published,3);status=GC_STORE_FULL;break;
            case 3:atomic_store((_Atomic uint64_t *)(f->heap+48),f->q.desired);status=GC_STORE_RETRY;break;
            case 4:expected=(void *)((uintptr_t)&a+1);break;
            case 5:actual=(void *)((uintptr_t)&a+1);break;
            case 6:actual=NULL;break;
            case 7:b.object_id=0;expected=&b;break;
            case 8:a.object_id=0;break;
            case 9:f->scope.held=0;break;
            case 10:f->q.transferred=1;break;
            case 11:f->q.expected=0;atomic_store((_Atomic uint64_t *)(f->heap+48),0);break;
            }
            memcpy(before,f,sizeof(*f));MarkRecord saved_a=a,saved_b=b;
            assert(cas(&f->q,expected,actual)==status);store_check(before,f,status);
            assert(!memcmp(&a,&saved_a,sizeof(a))&&!memcmp(&b,&saved_b,sizeof(b)));count++;
        }
    }
    assert(count==120);free(before);free(f);
    puts("logical CAS commit: 120 C/candidate identity, color, null, logging and rejection cases PASS");
}
#endif
