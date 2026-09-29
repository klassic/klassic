#ifndef KLASSIC_GC_ROOTED_SCALAR_CASES_H
#define KLASSIC_GC_ROOTED_SCALAR_CASES_H
#include "gc_rooted_scalar.h"
#include "gc_root_follow_cases.h"
#include "gc_pages_pipeline.h"
static void gc_run_rooted_scalar_cases(RootedScalarStep candidate){
    unsigned count=0;
    for(unsigned impl=0;impl<2;impl++)for(unsigned round=0;round<32;round++)for(unsigned scenario=0;scenario<12;scenario++){
        RootedScalarStep step=impl?candidate:gc_rooted_scalar;
        FollowFixture f;follow_fixture(&f,UINT64_C(0x10000)|((round%8)*0x11),1);
        void *mapping=pool_vm_reserve(3*GC_HEAP_PAGE_BYTES);
        pool_vm_commit((char *)mapping+GC_HEAP_PAGE_BYTES,2*GC_HEAP_PAGE_BYTES);
        AllocationPage pages[2]={f.page,f.page};ForwardCell last={2*GC_HEAP_PAGE_BYTES+32,28,0,0};
        ForwardTable table={(uintptr_t)&last,1,1};_Atomic uint64_t starts[128]={4};
        f.heap.heap_base=(uintptr_t)mapping;f.heap.pages=(uintptr_t)pages;f.heap.page_count=2;
        for(unsigned i=0;i<2;i++){
            pages[i].heap_base=(uintptr_t)mapping;pages[i].start=(i+1)*GC_HEAP_PAGE_BYTES;
            pages[i].limit=(i+2)*GC_HEAP_PAGE_BYTES;pages[i].allocated_top=pages[i].start+48;
            pages[i].generation=round%2;pages[i].incarnation=27+i;pages[i].run_head=i;
            ObjectHeader *header=(void *)((char *)mapping+pages[i].start);
            *header=(ObjectHeader){.descriptor_id=1,.payload_bytes=8,.state=GC_OBJECT_PUBLISHED};
            atomic_store((_Atomic uint64_t *)(header+1),i?UINT64_MAX:UINT64_C(0x301510));
        }
        pages[1].starts=(uintptr_t)starts;pages[1].forward_table=(uintptr_t)&table;
        TypeDescriptor descriptor={.id=1,.alignment=8,.fixed_bytes=8};_Atomic uint64_t descriptors[2]={0,(uintptr_t)&descriptor};
        DescriptorRegistry registry={(uintptr_t)descriptors,2};uint64_t bitmap=1;
        RootedScalar q={.receiver={.follow=f.q},.registry=(uintptr_t)&registry,
            .value=UINT64_C(0xdeadbeefffff1510),.operation=(round/2)%2};
        uint64_t initial=q.value,old_value=UINT64_C(0x301510);
        AllocationPage *canonical=&pages[0];
        if(scenario==1||scenario==2){
            uint64_t forwarded=(((pages[1].start+32)<<16)|UINT64_C(0x1510))|2;
            atomic_store(&f.forward.word,scenario==1?5:forwarded);
            if(scenario==1){
                assert(step(&f.pool,&q,GC_RSC_ENTER)==GC_RF_WAIT);
                atomic_store(&f.forward.word,forwarded);
            }else{
                f.journal.top=3;atomic_store(&f.journal.published,3);
                assert(step(&f.pool,&q,GC_RSC_ENTER)==GC_RF_FLUSH);
                atomic_store(&f.journal.consumed,3);
            }
            assert(!f.pool.active&&!atomic_load(&f.ctx.access_epoch));
            assert(step(&f.pool,&q,GC_RSC_ENTER)==GC_RF_ADVANCED);
            assert(!f.pool.active&&!q.receiver.follow.cleanup.token);
            // No payload read of the old source is legal after forwarding.
            pool_vm_decommit((char *)mapping+pages[0].start,GC_HEAP_PAGE_BYTES);
            canonical=&pages[1];old_value=UINT64_MAX;
        }
        if(scenario==3){
            f.pool.free_head=0;f.pool.active=2;
            assert(step(&f.pool,&q,GC_RSC_ENTER)==GC_RF_METADATA_FULL);
            assert(!q.stage&&!q.receiver.follow.cleanup.token&&!atomic_load(&f.ctx.access_epoch));
            f.pool.free_head=1;f.pool.active=0;
        }
        if(scenario==4){descriptor.fixed_bitmap=(uintptr_t)&bitmap;descriptor.fixed_bitmap_words=1;}
        if(scenario==8){
            // Retain a real lease after a prior ENTER cleanup failure. The
            // outer owner must release it before retry or pre-access abort.
            assert(gc_root_access_locked(&f.pool,&q.receiver)==GC_RAC_READY);
            assert(gc_root_access_cleanup(NULL,&q.receiver,GC_RAC_EXIT)==GC_RF_DONE);
            RootedScalar saved=q;
            assert(step(&f.pool,&q,GC_RSC_ENTER)==GC_RF_CLEANUP_REQUIRED&&!memcmp(&q,&saved,sizeof(q)));
            assert(step(&f.pool,&q,GC_RSC_ABORT)==GC_RF_CLEANUP_REQUIRED&&!memcmp(&q,&saved,sizeof(q)));
            assert(step(&f.pool,&q,GC_RSC_RELEASE)==GC_RF_DONE&&!q.stage&&!f.pool.active);
        }
        if(scenario==9){
            assert(step(&f.pool,&q,GC_RSC_ABORT)==GC_RSC_COMPLETE&&q.result==GC_RSC_ABORTED);
            assert(step(&f.pool,&q,GC_RSC_ENTER)==GC_RSC_COMPLETE&&!f.pool.active);
        }else if(scenario==10){
            atomic_store(&f.cell.value,0);
            assert(step(&f.pool,&q,GC_RSC_ENTER)==GC_RF_DONE&&!q.stage&&!f.pool.active&&q.value==initial);
        }else if(scenario==11){
            RootedScalar saved=q;
            assert(step(&f.pool,&q,5)==GC_RF_INVALID&&!memcmp(&q,&saved,sizeof(q)));
            assert(step(&f.pool,NULL,0)==GC_RF_INVALID);
            assert(step(&f.pool,(void *)((char *)&q+1),0)==GC_RF_INVALID);
            assert(step(&f.pool,&q,GC_RSC_ACCESS)==GC_RF_INVALID&&!memcmp(&q,&saved,sizeof(q)));
        }else{
            assert(step(&f.pool,&q,GC_RSC_ENTER)==GC_RAC_READY&&q.stage==1);
            assert(q.receiver.follow.cleanup.page==(uintptr_t)canonical&&f.pool.active==1);
            _Atomic uint64_t *payload=(void *)((char *)mapping+canonical->start+32);
            uint64_t op=scenario==5?GC_RSC_ABORT:GC_RSC_ACCESS;
            assert(step(NULL,&q,op)==GC_RSC_EXIT_REQUIRED&&q.stage==2);
            assert(q.result==(scenario==5?GC_RSC_ABORTED:scenario==4?GC_SCALAR_INVALID:GC_SCALAR_OK));
            assert(q.value==((scenario!=4&&scenario!=5&&!q.operation)?old_value:initial));
            assert(atomic_load(payload)==((scenario!=4&&scenario!=5&&q.operation)?initial:old_value));
            // A duplicate request or late cancellation must not repeat a load
            // or overwrite another actor's later scalar store.
            atomic_store(payload,73);RootedScalar saved=q;
            assert(step(NULL,&q,GC_RSC_ACCESS)==GC_RSC_EXIT_REQUIRED&&!memcmp(&q,&saved,sizeof(q)));
            assert(step(NULL,&q,GC_RSC_ABORT)==GC_RSC_EXIT_REQUIRED&&!memcmp(&q,&saved,sizeof(q)));
            assert(atomic_load(payload)==73);
            if(scenario==6){
                q.receiver.access.scope.ticket=1;saved=q;
                assert(step(NULL,&q,GC_RSC_EXIT)==GC_RSC_EXIT_REQUIRED&&!memcmp(&q,&saved,sizeof(q)));
                assert(f.pool.active==1&&atomic_load(&f.ctx.access_epoch));q.receiver.access.scope.ticket=0;
            }
            assert(step(NULL,&q,GC_RSC_EXIT)==GC_RSC_RELEASE_REQUIRED&&!atomic_load(&f.ctx.access_epoch));
            if(scenario==7){
                f.pool.active=0;saved=q;
                assert(step(&f.pool,&q,GC_RSC_RELEASE)==GC_RF_CLEANUP_REQUIRED&&!memcmp(&q,&saved,sizeof(q)));
                assert(q.receiver.follow.cleanup.token&&canonical->metadata_leases==1);f.pool.active=1;
            }
            assert(step(&f.pool,&q,GC_RSC_RELEASE)==GC_RSC_COMPLETE&&q.stage==4);
            saved=q;assert(step(&f.pool,&q,GC_RSC_RELEASE)==GC_RSC_COMPLETE&&!memcmp(&q,&saved,sizeof(q)));
            assert(step(&f.pool,&q,GC_RSC_ENTER)==GC_RSC_COMPLETE&&!memcmp(&q,&saved,sizeof(q)));
            assert(atomic_load(payload)==73);
        }
        assert(!f.pool.active&&!pages[0].metadata_leases&&!pages[1].metadata_leases&&!atomic_load(&f.ctx.access_epoch));
        pool_vm_release(mapping,3*GC_HEAP_PAGE_BYTES);count++;
    }
    assert(count==768);
    puts("rooted scalar: 768 C/candidate OS-backed canonical accesses, backpressure, abort and exactly-once cleanup cases PASS");
}
#endif
