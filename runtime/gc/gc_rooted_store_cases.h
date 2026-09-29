#ifndef KLASSIC_GC_ROOTED_STORE_CASES_H
#define KLASSIC_GC_ROOTED_STORE_CASES_H
#include "gc_rooted_store.h"
#include "gc_rooted_load_admit.h"
#include "gc_root_follow_cases.h"
#include "gc_pages_pipeline.h"
#include "gc_buffer_job.h"
#include "gc_mark_scan.h"
/* Exclusive OS-backed schedules execute the complete owner, with real roots,
 * journals, admission, queues and leases. Injected field updates model competing
 * writers; this fixture is not a proof of concurrent cycle termination. */
static void gc_run_rooted_store_cases(RootedStorer candidate){
    unsigned cases=0,rearmed=0;
    for(unsigned impl=0;impl<2;impl++)for(unsigned active=0;active<2;active++)for(unsigned scenario=0;scenario<12;scenario++)for(unsigned boundary=0;boundary<(scenario==3?64u:1u);boundary++){
        RootedStorer step=impl?candidate:gc_rooted_store;
        FollowFixture f;follow_fixture(&f,UINT64_C(0x10000)|(active?0x22:0),1);
        MetadataLeaseSlot slots[8]={0};for(unsigned i=0;i<7;i++)slots[i].next_free=i+2;
        f.pool.slots=(uintptr_t)slots;f.pool.capacity=8;
        void *mapping=pool_vm_reserve(2*GC_HEAP_PAGE_BYTES);pool_vm_commit((char *)mapping+GC_HEAP_PAGE_BYTES,GC_HEAP_PAGE_BYTES);
        f.heap.heap_base=f.page.heap_base=(uintptr_t)mapping;f.page.generation=scenario==0?GC_YOUNG:GC_OLD;
        MarkRecord record={.object_id=1};ObjectHeader *h=(void *)((char *)mapping+f.page.start);
        *h=(ObjectHeader){.payload_bytes=8,.descriptor_id=1,.mark_record=(uintptr_t)&record,.state=GC_OBJECT_PUBLISHED};
        uint64_t ref=atomic_load(&f.cell.value),desired=scenario==1?0:ref;
        _Atomic uint64_t *field=(void *)((char *)h+sizeof(*h));atomic_store(field,scenario==2?0:ref);
        uint64_t bitmap=1;TypeDescriptor descriptor={.id=1,.alignment=8,.fixed_bytes=8,.fixed_bitmap=(uintptr_t)&bitmap,.fixed_bitmap_words=1};
        _Atomic uint64_t descriptor_slots[2]={0,(uintptr_t)&descriptor};DescriptorRegistry registry={(uintptr_t)descriptor_slots,2};
        RootCell roots[2]={{.value=desired,.reuse_generation=1,.active=1},{.reuse_generation=1,.active=1}};
        RootChange changes[32]={0};BufferCursor journal={(uintptr_t)changes,32,0,0,0};
        RootSegment segment={.owner=17,.scope=102,.cells=(uintptr_t)roots,.capacity=2,.used=2,.state=GC_ROOT_SEGMENT_ACTIVE,.journal=(uintptr_t)&journal};
        RootRequest value={.segment=(uintptr_t)&segment,.handle={(uintptr_t)&roots[0],1,102}};
        RootRequest observed={.segment=(uintptr_t)&segment,.handle={(uintptr_t)&roots[1],1,102}};
        RootedStore q={.value.observation={.receiver.follow={.context=(uintptr_t)&f.ctx,.root=value},.output=value},
            .observation={.receiver.follow=f.q,.registry=(uintptr_t)&registry,.output=observed}};
        MarkTask tasks[2]={0};MarkQueue queues[2]={0};
        for(unsigned g=0;g<2;g++)queues[g]=(MarkQueue){.tasks=(uintptr_t)&tasks[g],.capacity=1,.free_head=1,.epoch=1,.generation=g};
        HandshakeState hs={.members=1,.last_identity=17};ThreadRegistration participant={.context=(uintptr_t)&f.ctx,.identity=17,.registered=1,.registry=(uintptr_t)&hs};hs.head=(uintptr_t)&participant;
        RootRegistry registered={.phase=atomic_load(&f.ctx.adopted_phase)};
        WorkerRequest worker={(uintptr_t)&hs,(uintptr_t)&participant};
        RootedLoadAdmission admission={(uintptr_t)&registered,(uintptr_t)&hs,(uintptr_t)&participant,{(uintptr_t)&queues[0],(uintptr_t)&queues[1]}};
        assert(gc_rooted_load_admit_locked(&admission,&q.value)==GC_LA_OK);
        BarrierEntry satb[4]={0},remembered[4]={0};f.ctx.satb=(BufferCursor){.entries=(uintptr_t)satb,.capacity=4};f.ctx.remembered=(BufferCursor){.entries=(uintptr_t)remembered,.capacity=4};
        MetadataLeaseRequest pressure[8]={0};unsigned held_pressure=0;
        unsigned ticks=0,physical=0,retries=0;bool injected=false,cancelled=false,completed=false;
        uint64_t result=0;
        while(ticks<200){
            uint64_t action=gc_rooted_store_next(&q);
            if(action==GC_RLOAD_DONE||action==GC_RLOAD_CANCELLED||action==GC_RLOAD_ERROR||action==GC_RLOAD_NULL_RECEIVER){result=action;completed=true;break;}
            if(scenario==3&&ticks==boundary){
                registered.phase=(registered.phase+UINT64_C(0x10000))^UINT64_C(0x100);
                assert(gc_handshake_step_locked(&hs,NULL,GC_HS_BEGIN)==GC_HS_OK);
                uint64_t park=gc_worker_step_locked(&registered,&worker,GC_WK_POLL);
                assert(park==(atomic_load(&f.ctx.access_epoch)?GC_WK_INVALID:GC_WK_RETRY)&&!hs.acknowledged);
                (void)step(NULL,&q,GC_RLOAD_CANCEL);cancelled=true;action=gc_rooted_store_next(&q);
            }
            if(q.stage==GC_RS_ATTEMPT&&q.attempt.stage==1&&!injected){
                if(scenario==4)atomic_store(field,atomic_load(field)?0:ref);
                if(scenario==5)atomic_fetch_xor(&roots[0].value,0x300);
                if(scenario==6){f.ctx.remembered.top=4;atomic_store(&f.ctx.remembered.published,4);}
                if(scenario>=4&&scenario<=6)injected=true;
            }
            if(q.stage==GC_RS_ATTEMPT&&!q.attempt.stage&&!injected&&(scenario==7||scenario==8)){
                // Leave only one or two slots free: fail the first/second entry
                // admission after the receiver guard was acquired.
                unsigned free_count=scenario==7?1:2;
                while(f.pool.capacity-f.pool.active>free_count){
                    pressure[held_pressure]=(MetadataLeaseRequest){(uintptr_t)&f.page,f.page.incarnation,0};
                    assert(gc_metadata_step_locked(&f.pool,&pressure[held_pressure],GC_ML_ACQUIRE)==GC_ML_OK);held_pressure++;
                }
                injected=true;
            }
            if(q.stage==GC_RS_ATTEMPT&&q.attempt.stage==1)physical++;
            if(scenario==9&&q.stage==GC_RS_OBSERVE&&!injected){atomic_store(&f.cell.value,0);injected=true;}
            if(scenario==10&&q.stage==GC_RS_OBSERVE&&!injected){q.observation.byte_offset=8;injected=true;}
            if(scenario==11&&q.stage==GC_RS_ATTEMPT&&q.attempt.stage==2&&!injected){(void)step(NULL,&q,GC_RLOAD_CANCEL);cancelled=true;injected=true;}
            uint64_t r=step(action==GC_RLOAD_HEAP?&f.pool:NULL,&q,action);ticks++;
            if(r==GC_RLOAD_INVALID||r==GC_RLOAD_RESTART)fprintf(stderr,"store step impl=%u active=%u scenario=%u boundary=%u ticks=%u stage=%llu op=%llu r=%llu child=%llu\n",impl,active,scenario,boundary,ticks,(unsigned long long)q.stage,(unsigned long long)action,(unsigned long long)r,(unsigned long long)q.last);assert(r!=GC_RLOAD_INVALID&&r!=GC_RLOAD_RESTART);
            if(q.stage<GC_RS_COMPLETE)assert(atomic_load(&f.ctx.private_producers)>=1);
            if(q.stage==GC_RS_RETRY){
                retries++;assert(!atomic_load(&f.ctx.access_epoch));
                if(scenario==6){assert(r==GC_RLOAD_FLUSH);atomic_store(&f.ctx.remembered.consumed,4);}
                if(scenario==7||scenario==8){
                    assert(r==GC_RLOAD_METADATA_FULL);
                    for(unsigned i=0;i<held_pressure;i++)assert(gc_metadata_step_locked(&f.pool,&pressure[i],GC_ML_RELEASE)==GC_ML_OK);
                }
            }
        }
        assert(completed&&!atomic_load(&f.ctx.access_epoch)&&!atomic_load(&f.ctx.private_producers));
        if(scenario==9)assert(result==GC_RLOAD_NULL_RECEIVER);
        else if(scenario==10)assert(result==GC_RLOAD_ERROR);
        else if(scenario==3&&cancelled&&q.stage!=GC_RS_COMPLETE)assert(result==GC_RLOAD_CANCELLED);
        else{if(result!=GC_RLOAD_DONE||atomic_load(field)!=desired)fprintf(stderr,"store owner impl=%u active=%u scenario=%u boundary=%u result=%llu stage=%llu last=%llu value=%llu expected=%llu ticks=%u\n",impl,active,scenario,boundary,(unsigned long long)result,(unsigned long long)q.stage,(unsigned long long)q.last,(unsigned long long)atomic_load(field),(unsigned long long)desired,ticks);assert(result==GC_RLOAD_DONE&&atomic_load(field)==desired);assert(physical==(scenario==4||scenario==5||scenario==6?2u:1u));}
        if(scenario>=4&&scenario<=8)assert(injected&&retries==1);
        RootedStore saved=q;uint64_t saved_field=atomic_load(field);
        for(unsigned op=GC_RLOAD_HEAP;op<GC_RLOAD_REARM;op++)assert(step(NULL,&q,op)==result&&!memcmp(&q,&saved,sizeof(q))&&atomic_load(field)==saved_field);
        // Rejected terminal controls must not discharge somebody else's count.
        RootedStore invalid=saved;invalid.cancel=2;invalid.admitted=1;RootedStore invalid_saved=invalid;
        atomic_store(&f.ctx.private_producers,1);
        assert(step(NULL,&invalid,GC_RLOAD_HEAP)==GC_RLOAD_INVALID&&!memcmp(&invalid,&invalid_saved,sizeof(invalid))&&atomic_load(&f.ctx.private_producers)==1);
        atomic_store(&f.ctx.private_producers,0);
        for(unsigned pass=0;pass<2;pass++){
        for(unsigned selector=0;selector<2;selector++){
            BufferCursor *buffer=selector?&f.ctx.remembered:&f.ctx.satb;
            while(atomic_load(&buffer->consumed)<atomic_load(&buffer->published)){
                BarrierEntry entry={0};BarrierJob job={.identity=17,.selector=selector,.output=(uintptr_t)&entry,.capacity=1};
                assert(gc_buffer_job_step(&f.ctx,&job,GC_BJ_CLAIM)==GC_BJ_OK);
                assert(gc_buffer_job_step(&f.ctx,&job,GC_BJ_COPY)==GC_BJ_OK&&job.written==1);
                assert(entry.slot.owner.bits==ref&&entry.slot.byte_offset==0);
                assert(gc_buffer_job_step(&f.ctx,&job,GC_BJ_ACK)==GC_BJ_OK);
                MetadataLeaseRequest held={.token=entry.slot.metadata_lease};assert(gc_metadata_step_locked(&f.pool,&held,GC_ML_RESOLVE)==GC_ML_OK);assert(gc_metadata_step_locked(&f.pool,&held,GC_ML_RELEASE)==GC_ML_OK);
            }
        }
        for(unsigned g=0;g<2;g++)if(queues[g].queued){
            MarkRequest work={0};assert(gc_mark_step_locked(&queues[g],&work,GC_MARK_TAKE)==GC_MARK_OK);
            MetadataLeaseRequest held={.token=work.metadata_lease};assert(gc_metadata_step_locked(&f.pool,&held,GC_ML_RESOLVE)==GC_ML_OK);
            ResolveRequest scope={.reference=work.reference,.scope={.context=(uintptr_t)&f.ctx}};
            assert(gc_resolve_access(&f.page,&scope,GC_RESOLVE_ENTER)==GC_RESOLVE_OK);
            uint64_t child=0;MarkScan scan={(uintptr_t)&work,(uintptr_t)mapping,(uintptr_t)&registry,(uintptr_t)&child,1,0,0,0,0};
            assert(gc_mark_scan_slice(&scan)==GC_SCAN_DONE);if(scan.written)assert((child>>16)==(ref>>16));
            assert(gc_resolve_access(&f.page,&scope,GC_RESOLVE_EXIT)==GC_RESOLVE_OK);work.cursor=scan.next_cursor;
            assert(gc_mark_step_locked(&queues[g],&work,GC_MARK_FINISH)==GC_MARK_OK);assert(gc_metadata_step_locked(&f.pool,&held,GC_ML_RELEASE)==GC_ML_OK);
        }

            assert(!f.pool.active&&!f.page.metadata_leases);
            if(pass||scenario!=3||!cancelled)break;
            // Published old-phase work has now been consumed independently.
            // ACK adopts the new snapshot; rearm is forbidden while parked.
            assert(gc_worker_step_locked(&registered,&worker,GC_WK_POLL)==GC_WK_PARK&&hs.acknowledged==1);
            RootedStore before=q;
            uint64_t parked_result=step(&f.pool,&q,GC_RLOAD_REARM);
            assert(parked_result==(result==GC_RLOAD_DONE?GC_RLOAD_DONE:GC_RLOAD_INVALID)&&!memcmp(&q,&before,sizeof(q)));
            assert(gc_handshake_step_locked(&hs,NULL,GC_HS_END)==GC_HS_OK);
            assert(gc_worker_step_locked(&registered,&worker,GC_WK_POLL)==GC_WK_OK);
            if(result==GC_RLOAD_DONE){assert(step(&f.pool,&q,GC_RLOAD_REARM)==GC_RLOAD_DONE&&!memcmp(&q,&before,sizeof(q)));break;}
            for(unsigned fault=0;fault<16;fault++){
                RootedStore bad=q;
                if(fault==0)bad.admitted=1;
                else if(fault==1)bad.value.admitted=1;
                else if(fault==2)bad.value.fault=1;
                else if(fault==3)bad.value.stage=GC_RLOAD_ROOT_ENTER;
                else if(fault==4)bad.value.marks.state=1;
                else if(fault==5)bad.attempt.stage=1;
                else if(fault==6){bad.attempt.stage=5;bad.attempt.result=GC_STORE_OK;}
                else if(fault==7)bad.value.publication.follow.cleanup.token=1;
                else if(fault==8)bad.value.marks.jobs[0].access.access.scope.held=1;
                else if(fault==9)bad.value.marks.jobs[1].access.follow.cleanup.page=8;
                else if(fault==10)bad.observation.receiver.access.reference=1;
                else if(fault==11)bad.attempt.receiver.access.destination=1;
                else if(fault==12)bad.attempt.entries[0].incarnation=1;
                else if(fault==13)bad.attempt.entries[1].token=1;
                else if(fault==14)bad.value.observation.receiver.follow.context++;
                else bad.observation.receiver.follow.context=0;
                RootedStore unchanged=bad;
                assert(step(&f.pool,&bad,GC_RLOAD_REARM)==GC_RLOAD_INVALID&&!memcmp(&bad,&unchanged,sizeof(bad))&&!atomic_load(&f.ctx.private_producers));
            }
            uint64_t changed=atomic_load(field)?0:ref;atomic_store(field,changed);
            assert(step(&f.pool,&q,GC_RLOAD_REARM)==GC_RLOAD_HEAP);
            RootedStore fresh={.value.observation={.receiver.follow={.context=(uintptr_t)&f.ctx,.root=value},.output=value},
                .observation={.receiver.follow=f.q,.registry=(uintptr_t)&registry,.output=observed}};
            assert(!memcmp(&q,&fresh,sizeof(q))&&!atomic_load(&f.ctx.private_producers));
            // A rearmed request must capture new queues/epochs before doing work.
            assert(step(&f.pool,&q,GC_RLOAD_HEAP)==GC_RLOAD_INVALID&&!memcmp(&q,&fresh,sizeof(q)));
            for(unsigned g=0;g<2;g++)queues[g].epoch++;
            assert(gc_rooted_load_admit_locked(&admission,&q.value)==GC_LA_OK);
            assert(q.value.marks.phase==registered.phase);
            unsigned n=0;
            while(gc_rooted_store_next(&q)!=GC_RLOAD_DONE){
                uint64_t action=gc_rooted_store_next(&q);assert(action==GC_RLOAD_HEAP||action==GC_RLOAD_ACCESS||action==GC_RLOAD_QUEUE);
                assert(++n<100);uint64_t r=step(action==GC_RLOAD_HEAP?&f.pool:NULL,&q,action);
                assert(r==GC_RLOAD_HEAP||r==GC_RLOAD_ACCESS||r==GC_RLOAD_QUEUE||r==GC_RLOAD_DONE);
            }
            uint64_t expected=(desired&~UINT64_C(0xffff))|gc_abi_color((PhaseSnapshot){registered.phase});
            assert(atomic_load(field)==expected&&q.attempt.commit.expected==changed&&q.observation.output.value==changed);
            assert(!atomic_load(&f.ctx.private_producers)&&!atomic_load(&f.ctx.access_epoch));rearmed++;
        }
        assert(!f.pool.active&&!f.page.metadata_leases);pool_vm_release(mapping,2*GC_HEAP_PAGE_BYTES);cases++;
    }
    assert(cases==300);assert(rearmed>0);printf("rooted store rearm: %u cancelled operations cross handshake and fresh admission PASS\n",rearmed);
    puts("rooted store: 300 C/candidate admitted preparation, observation, retry and cancellation schedules PASS");
}
#endif
