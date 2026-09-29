#ifndef KLASSIC_GC_ROOTED_STORE_ATTEMPT_CASES_H
#define KLASSIC_GC_ROOTED_STORE_ATTEMPT_CASES_H
#include "gc_rooted_store_attempt.h"
#include "gc_root_follow_cases.h"
#include "gc_pages_pipeline.h"
#include "gc_buffer_job.h"
static void gc_test_store_attempt_consume(FollowFixture *f,const RootedStoreAttempt *q,unsigned selector){
    BarrierEntry entry={0};BarrierJob job={.identity=f->ctx.thread_id,.selector=selector,.output=(uintptr_t)&entry,.capacity=1};
    assert(gc_buffer_job_step(&f->ctx,&job,GC_BJ_CLAIM)==GC_BJ_OK);
    assert(gc_buffer_job_step(&f->ctx,&job,GC_BJ_COPY)==GC_BJ_OK&&job.written==1);
    assert(entry.slot.metadata_lease==(selector?q->commit.remembered.slot.metadata_lease:q->commit.satb.slot.metadata_lease));
    assert(gc_buffer_job_step(&f->ctx,&job,GC_BJ_ACK)==GC_BJ_OK);
    MetadataLeaseRequest held={.token=entry.slot.metadata_lease};
    assert(gc_metadata_step_locked(&f->pool,&held,GC_ML_RESOLVE)==GC_ML_OK);
    assert(gc_metadata_step_locked(&f->pool,&held,GC_ML_RELEASE)==GC_ML_OK);
}
static void gc_run_rooted_store_attempt_cases(RootedStoreAttempter candidate){
    unsigned count=0;
    for(unsigned impl=0;impl<2;impl++)for(unsigned round=0;round<8;round++)for(unsigned scenario=0;scenario<26;scenario++){
        RootedStoreAttempter step=impl?candidate:gc_rooted_store_attempt;
        uint64_t phase=UINT64_C(0x10022)|(round<<8);
        FollowFixture f;follow_fixture(&f,phase,1);MetadataLeaseSlot slots[3]={{.next_free=2},{.next_free=3},{0}};
        f.pool.slots=(uintptr_t)slots;f.pool.capacity=3;
        void *mapping=pool_vm_reserve(2*GC_HEAP_PAGE_BYTES);pool_vm_commit((char *)mapping+GC_HEAP_PAGE_BYTES,GC_HEAP_PAGE_BYTES);
        f.heap.heap_base=f.page.heap_base=(uintptr_t)mapping;f.page.allocated_top=f.page.start+112;
        f.page.generation=(scenario==1||scenario==3)?GC_YOUNG:GC_OLD;
        atomic_store(&f.starts[0],68);
        MarkRecord records[2]={{.object_id=1,.young_epoch=1,.old_epoch=1},{.object_id=2,.young_epoch=1,.old_epoch=1}};
        for(unsigned i=0;i<2;i++){
            ObjectHeader *h=(void *)((char *)mapping+f.page.start+i*64);
            *h=(ObjectHeader){.payload_bytes=8,.descriptor_id=1,.mark_record=(uintptr_t)&records[i],.state=GC_OBJECT_PUBLISHED};
        }
        uint64_t value=((f.page.start+96)<<16)|gc_abi_color((PhaseSnapshot){phase});
        uint64_t previous=(scenario==2||scenario==3)?0:value;
        _Atomic uint64_t *payload=(void *)((char *)mapping+f.page.start+32);atomic_store(payload,previous);
        uint64_t bitmap=1;TypeDescriptor descriptor={.id=1,.alignment=8,.fixed_bytes=8,.fixed_bitmap=(uintptr_t)&bitmap,.fixed_bitmap_words=1};
        _Atomic uint64_t descriptor_slots[2]={0,(uintptr_t)&descriptor};DescriptorRegistry registry={(uintptr_t)descriptor_slots,2};
        RootCell desired={.value=value,.reuse_generation=1,.active=1};
        RootSegment desired_segment={.owner=17,.scope=102,.cells=(uintptr_t)&desired,.capacity=1,.used=1,.state=GC_ROOT_SEGMENT_ACTIVE};
        RootedMarkSet marks={.context=(uintptr_t)&f.ctx,.root={.segment=(uintptr_t)&desired_segment,.handle={(uintptr_t)&desired,1,102}},
            .phase=phase,.state=2,.required=3,.completed=3};
        BarrierEntry satb[2]={0},remembered[2]={0};
        f.ctx.satb=(BufferCursor){.entries=(uintptr_t)satb,.capacity=2};f.ctx.remembered=(BufferCursor){.entries=(uintptr_t)remembered,.capacity=2};
        atomic_store(&f.ctx.private_producers,1);
        RootedStoreAttempt q={.receiver={.follow=f.q},.marks=(uintptr_t)&marks,
            .commit={.registry=(uintptr_t)&registry,.expected=previous}};
        uint64_t result=GC_STORE_OK;
        if(scenario==6){f.pool.capacity=1;slots[0].next_free=0;result=GC_STA_METADATA_FULL;}
        if(scenario==7){f.pool.capacity=2;slots[1].next_free=0;result=GC_STA_METADATA_FULL;}
        if(scenario==8||scenario==9){slots[scenario-7].generation=UINT32_MAX;result=GC_STA_METADATA_EXHAUSTED;}
        if(scenario==10){f.ctx.satb.top=2;atomic_store(&f.ctx.satb.published,2);result=GC_STORE_FULL;}
        if(scenario==11){f.ctx.remembered.top=2;atomic_store(&f.ctx.remembered.published,2);result=GC_STORE_FULL;}
        if(scenario==12){atomic_fetch_xor(&desired.value,0x300);result=GC_STORE_REPREPARE;}
        if(scenario==13){marks.completed=1;result=GC_STORE_INVALID;}
        if(scenario==23){slots[1].page=8;result=GC_STA_METADATA_ERROR;}
        if(scenario==4||scenario==19||scenario==20){
            if(scenario==19){atomic_store(&f.cell.value,0);assert(step(&f.pool,&q,GC_STA_ENTER)==GC_RF_DONE);}
            if(scenario==20){
                RootedStoreAttempt saved=q;assert(step(&f.pool,&q,5)==GC_RF_INVALID&&!memcmp(&q,&saved,sizeof(q)));
                assert(step(&f.pool,NULL,0)==GC_RF_INVALID&&step(&f.pool,(void *)((char *)&q+1),0)==GC_RF_INVALID);
            }
            assert(step(NULL,&q,GC_STA_ABORT)==GC_STA_COMPLETE&&q.result==GC_STA_ABORTED);
        }else{
            if(scenario==21){
                assert(gc_root_access_locked(&f.pool,&q.receiver)==GC_RAC_READY);
                assert(gc_root_access_cleanup(NULL,&q.receiver,GC_RAC_EXIT)==GC_RF_DONE);
                assert(step(&f.pool,&q,GC_STA_ENTER)==GC_RF_CLEANUP_REQUIRED);
                assert(step(&f.pool,&q,GC_STA_RELEASE)==GC_RF_DONE&&!q.stage);
            }
            if(scenario==24){
                atomic_store(&f.forward.word,5);assert(step(&f.pool,&q,GC_STA_ENTER)==GC_RF_WAIT&&!q.stage&&!f.pool.active);
                atomic_store(&f.forward.word,0);
            }
            uint64_t entered=step(&f.pool,&q,GC_STA_ENTER);
            assert(entered==(result>=GC_STA_METADATA_FULL?GC_STA_EXIT_REQUIRED:GC_RAC_READY));
            if(entered==GC_RAC_READY){
                assert(f.pool.active==3&&q.entries[0].token&&q.entries[1].token&&q.entries[0].token!=q.entries[1].token);
                RootedStoreAttempt saved=q;assert(step(&f.pool,&q,GC_STA_RELEASE)==GC_RAC_READY&&!memcmp(&q,&saved,sizeof(q)));
                if(scenario==5||scenario==17){result=GC_STA_ABORTED;assert(step(NULL,&q,GC_STA_ABORT)==GC_STA_EXIT_REQUIRED);}
                else{
                    if(scenario==14){atomic_store(payload,0);result=GC_STORE_RETRY;}
                    assert(step(NULL,&q,GC_STA_ACCESS)==GC_STA_EXIT_REQUIRED);
                }
            }
            assert(q.result==result&&q.stage==2);
            uint64_t mask=result==GC_STORE_OK?((previous?1:0)|(f.page.generation==GC_OLD?2:0)):0;
            assert(q.commit.transferred==mask);
            for(unsigned i=0;i<2;i++)if(mask&(1u<<i))assert(!q.entries[i].token&&!q.entries[i].page);
            uint64_t after=atomic_load(payload);RootedStoreAttempt saved=q;
            assert(step(NULL,&q,GC_STA_ACCESS)==GC_STA_EXIT_REQUIRED&&!memcmp(&q,&saved,sizeof(q)));
            assert(step(NULL,&q,GC_STA_ABORT)==GC_STA_EXIT_REQUIRED&&!memcmp(&q,&saved,sizeof(q)));
            assert(atomic_load(payload)==after);
            if(scenario==15){q.receiver.access.scope.ticket=1;saved=q;assert(step(NULL,&q,GC_STA_EXIT)==GC_STA_EXIT_REQUIRED&&!memcmp(&q,&saved,sizeof(q)));q.receiver.access.scope.ticket=0;}
            if(scenario==23)slots[1].page=0; // restore the injected invalid free-slot metadata
            assert(step(NULL,&q,GC_STA_EXIT)==GC_STA_RELEASE_REQUIRED&&!atomic_load(&f.ctx.access_epoch));
            // Consumer may finish its transferred entries before this owner
            // releases the receiver's own lease. It must not be released twice.
            if(scenario==22)for(unsigned i=0;i<2;i++)if(mask&(1u<<i)){
                gc_test_store_attempt_consume(&f,&q,i);
            }
            if(scenario==16){uint64_t head=f.pool.free_head;f.pool.free_head=4;saved=q;assert(step(&f.pool,&q,GC_STA_RELEASE)==GC_STA_RELEASE_REQUIRED&&!memcmp(&q,&saved,sizeof(q)));f.pool.free_head=head;}
            assert(step(&f.pool,&q,GC_STA_RELEASE)==GC_STA_RELEASE_REQUIRED&&q.stage==4);
            for(unsigned i=0;i<2;i++){
                if(scenario==17){uint64_t head=f.pool.free_head;f.pool.free_head=4;saved=q;assert(step(&f.pool,&q,GC_STA_RELEASE)==GC_STA_RELEASE_REQUIRED&&!memcmp(&q,&saved,sizeof(q)));f.pool.free_head=head;}
                assert(step(&f.pool,&q,GC_STA_RELEASE)==(i?GC_STA_COMPLETE:GC_STA_RELEASE_REQUIRED));
            }
            assert(q.result==result&&!q.entries[0].token&&!q.entries[1].token);
            if(scenario!=22)for(unsigned i=0;i<2;i++)if(mask&(1u<<i)){
                gc_test_store_attempt_consume(&f,&q,i);
            }
        }
        RootedStoreAttempt saved=q;
        for(unsigned op=0;op<5;op++)assert(step(&f.pool,&q,op)==GC_STA_COMPLETE&&!memcmp(&q,&saved,sizeof(q)));
        assert(!f.pool.active&&!f.page.metadata_leases&&!atomic_load(&f.ctx.access_epoch));
        assert(atomic_load(&f.ctx.private_producers)==1); // belongs to the outer store
        pool_vm_release(mapping,2*GC_HEAP_PAGE_BYTES);count++;
    }
    assert(count==416);
    puts("rooted store attempt: 416 C/candidate OS-backed transfer, partial admission, abort and cleanup retry schedules PASS");
}
#endif
