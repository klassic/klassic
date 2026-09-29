/* OS-backed target ownership fixture. The single collector owns each target
 * run through cleanup, and strong fixture references retain all logical objects.
 * The pool, binding, allocator, target, copy, forwarding and resolver callbacks
 * are C/emitted runtime leaves; this fixture supplies the OS and scheduler. */
#ifndef KLASSIC_GC_RELOCATE_PIPELINE_H
#define KLASSIC_GC_RELOCATE_PIPELINE_H
#include "gc_relocate_target_cases.h"
#include "gc_resolve_chain.h"
#include "gc_relocation_gate.h"
#include "gc_relocation_turn.h"
#include "gc_source_drain.h"
#include "gc_page_release.h"
#include "gc_relocation_job.h"
#include "gc_relocation_complete.h"
#include "gc_relocation_claim.h"
#include "gc_relocate_discard.h"
#include "gc_relocation_cancel.h"
static void pipeline_discard(RelocationGate *g,RelocationGateRequest *permit,
        uint64_t (*gate)(RelocationGate *,RelocationGateRequest *,uint64_t),
        AllocationPage *target,ForwardCell *source,RelocationCopy *copy,
        uint64_t (*discard)(AllocationPage *,RelocationDiscard *)){
    RelocationDiscard q={.source=(uintptr_t)source,.copy=(uintptr_t)copy};
    uint64_t status;do{
        assert(permit->serial==copy->ticket);
        assert(gate(g,permit,GC_RG_INSPECT)==GC_RG_OK);
        uint64_t before=q.cursor;status=discard(target,&q);
        assert(status==GC_DISCARD_DONE||status==GC_DISCARD_MORE);
        assert(q.cursor>=before&&q.cursor-before<=256&&g->active==1);
        assert(atomic_load(&source->word)==((copy->ticket<<2)|1)&&atomic_load(&source->access)==GC_SOURCE_CLOSED);
        RelocationDiscard copied=q;assert(discard(target,&copied)==GC_DISCARD_INVALID);
        if(status==GC_DISCARD_MORE)metadata_yield();
    }while(status==GC_DISCARD_MORE);
    assert(q.cursor==q.bytes&&discard(target,&q)==GC_DISCARD_DONE);
}
static void gc_run_relocate_pipeline(PoolStep pool_step,TlabPage bind,TlabAllocator allocate,
        RelocateTarget target,RelocateCopy copy,RelocatePublish publish,
        SourceAccess access,ReferenceAcquirer acquire,MetadataStep metadata,Resolver resolve,
        uint64_t (*gate)(RelocationGate *,RelocationGateRequest *,uint64_t),
        uint64_t (*drain)(AllocationPage *,SourceDrain *),
        uint64_t (*release)(HeapPool *,PageRelease *,uint64_t),
        uint64_t (*job_step)(RelocationGate *,RelocationJob *,uint64_t),
        uint64_t (*complete)(RelocationGate *,RelocationCompletion *),
        uint64_t (*start)(RelocationGate *,RelocationClaim *),
        uint64_t (*discard)(AllocationPage *,RelocationDiscard *),
        uint64_t (*cancel_step)(RelocationGate *,RelocationCancellation *),RelocationTurnStep turn){
    unsigned cleanup_jobs=0;
    for(unsigned round=0;round<96;round++){
        bool cancel=round>=32&&round<64;unsigned installed=0;
        RelocationGate admission={.epoch=1,.closed=1},old_admission={.epoch=1,.closed=1};
        RelocationCoordinator coordinator={0};
        RelocationTurn owner={.generation=GC_YOUNG,.gate=(uintptr_t)&admission};
        RelocationTurn waiting={.generation=GC_OLD,.gate=(uintptr_t)&old_admission};
        assert(turn(&coordinator,&owner,GC_RT_REQUEST)==GC_RT_OK);
        assert(turn(&coordinator,&waiting,GC_RT_REQUEST)==GC_RT_OK);
        assert(turn(&coordinator,&owner,GC_RT_ACQUIRE)==GC_RT_OK);
        assert(gate(&admission,NULL,GC_RG_REOPEN)==GC_RG_OK);
        assert(turn(&coordinator,&waiting,GC_RT_ACQUIRE)==GC_RT_WAIT);
        AllocationPage pages[4]={0};ForwardTable tables[2]={0};ForwardCell cells[2][5]={0};
        _Atomic uint64_t starts[2][256]={0};MarkRecord records[5]={0};MarkArena marks={(uintptr_t)records,5,0};
        MetadataLeaseSlot slot={0};
        size_t bytes=5*GC_HEAP_PAGE_BYTES;void *mapping=pool_vm_reserve(bytes);
        uint64_t native=sizeof(pages)+sizeof(tables)+sizeof(cells)+sizeof(records)+sizeof(slot)+
            sizeof(admission)+sizeof(old_admission)+sizeof(coordinator)+sizeof(owner)+sizeof(waiting);
        HeapPool heap={.heap_base=(uintptr_t)mapping,.pages=(uintptr_t)pages,.page_count=4,
            .hard_max=4*GC_HEAP_PAGE_BYTES,.soft_max=2*GC_HEAP_PAGE_BYTES,.relocation_reserve=2*GC_HEAP_PAGE_BYTES,
            .metadata_committed=native,.metadata_max=native+sizeof(starts)};
        MetadataLeasePool leases={(uintptr_t)&heap,(uintptr_t)&slot,1,1,0};
        for(unsigned run=0;run<2;run++){
            PageLease l={.page_count=2,.generation=run?(round%2):GC_YOUNG,.relocation=run};
            assert(pool_step(&heap,&l,GC_POOL_RESERVE)==GC_POOL_OK&&l.head_index==2*run);
            pool_vm_commit((char *)mapping+pages[2*run].start,2*GC_HEAP_PAGE_BYTES);
            l.starts=(uintptr_t)starts[run];l.committed_mapping=1;
            assert(pool_step(&heap,&l,GC_POOL_COMMIT)==GC_POOL_OK);
            tables[run]=(ForwardTable){(uintptr_t)cells[run],5,0};pages[2*run].forward_table=(uintptr_t)&tables[run];
        }
        MutatorContext ctx={.abi_version=GC_ABI_VERSION,.thread_id=17};
        assert(bind(&ctx,&pages[0],0)==GC_ALLOC_OK);
        TypeDescriptor descriptor={.id=1,.alignment=16,.element_bytes=1};
        AllocationResult output={0};AllocationRequest request={(uintptr_t)&pages[0],(uintptr_t)&descriptor,0,(uintptr_t)&output,(uintptr_t)&marks};
        const uint64_t lengths[]={7,1025,0,GC_HEAP_PAGE_BYTES,17};
        uint64_t original[5],destinations[5];
        for(unsigned i=0;i<5;i++){
            request.elements=lengths[i];assert(allocate(&ctx,&request)==GC_ALLOC_OK);original[i]=output.reference;
            ObjectHeader *h=(ObjectHeader *)((char *)mapping+(original[i]>>16)-32);
            records[i].old_epoch=round+1;
            for(uint64_t j=0;j<lengths[i];j++)((unsigned char *)(h+1))[j]=(unsigned char)(i+j*17);
            atomic_store_explicit(&h->state,GC_OBJECT_PUBLISHED,memory_order_release);
        }
        assert(marks.used==5&&bind(&ctx,&pages[0],1)==GC_ALLOC_OK);
        for(unsigned i=0;i<5;i++){
            RelocationPermit permit={0};ForwardCell *source=&cells[0][i];
            RelocationClaim attempt={.permit=(uintptr_t)&permit,.source=(uintptr_t)source,
                .source_offset=source->source_offset,.incarnation=source->incarnation};
            assert(start(&admission,&attempt)==GC_RCL_OWNED);
            RelocationGateRequest job={attempt.permit,attempt.ticket,attempt.epoch};
            SourceAccessRequest select={0,source->source_offset,source->incarnation,0,attempt.ticket};
            assert(access(source,&select,GC_SA_SELECT)==GC_SA_OK&&access(source,&select,GC_SA_CHECK)==GC_SA_OK);
            RelocationCopy q={(uintptr_t)mapping,source->incarnation,attempt.ticket,pages[0].limit,0,0,256,0,0};
            if(cancel&&i==1){
                /* Inject failure before prepare, after prepare, during copy, or
                 * after complete copy but before installation/publication.
                 * This fixture exclusively owns the target's uninstalled top. */
                uint64_t top=atomic_load(&pages[2].allocated_top);
                uint64_t used=atomic_load(&tables[1].used),committed=heap.committed;
                unsigned stage=((round-32)/2)%4;
                if(stage){
                    assert(target(&pages[2],&q,GC_TARGET_PREPARE)==GC_TARGET_OK);
                    if(stage==2){q.budget=7;assert(copy(source,&q)==GC_COPY_MORE&&q.cursor==7);}
                    if(stage==3){uint64_t st;do{st=copy(source,&q);}while(st==GC_COPY_MORE);assert(st==GC_COPY_DONE);}
                    ReferenceLease hidden={.reference=(q.destination<<16)|UINT64_C(0x1510)};
                    assert(acquire(&leases,&hidden)==GC_ML_INVALID);
                }
                assert(gate(&admission,0,GC_RG_CLOSE)==GC_RG_WAIT);
                assert(turn(&coordinator,&owner,GC_RT_RELEASE)==GC_RT_WAIT);
                assert(turn(&coordinator,&waiting,GC_RT_ACQUIRE)==GC_RT_WAIT);
                assert(gate(&admission,0,GC_RG_CHECK)==GC_RG_WAIT);
                RelocationPermit late={0};RelocationGateRequest denied={.permit=(uintptr_t)&late};
                assert(gate(&admission,&denied,GC_RG_ACQUIRE)==GC_RG_WAIT);
                RelocationDiscard abandoned={.source=(uintptr_t)source,.copy=(uintptr_t)&q};
                RelocationCancellation cancellation={.permit=job.permit,.serial=job.serial,.epoch=job.epoch,
                    .target=(uintptr_t)&pages[2],.discard=(uintptr_t)&abandoned};
                SourceAccessRequest reader={(uintptr_t)&ctx,source->source_offset,source->incarnation,0,0};
                uint64_t outcome;
                do{
                    uint64_t before=abandoned.cursor;
                    outcome=cancel_step(&admission,&cancellation);
                    assert(outcome==GC_RCAN_DONE||outcome==GC_RCAN_MORE);
                    assert(abandoned.cursor>=before&&abandoned.cursor-before<=256);
                    if(outcome==GC_RCAN_MORE){
                        assert(gate(&admission,0,GC_RG_CHECK)==GC_RG_WAIT);
                        RelocationCancellation duplicate=cancellation;
                        assert(cancel_step(&admission,&duplicate)==GC_RCAN_INVALID);
                        if(cancellation.stage==3){
                            assert(access(source,&reader,GC_SA_ENTER)==GC_SA_OK);
                        }else{
                            assert(atomic_load(&source->access)==GC_SOURCE_CLOSED);
                            metadata_yield();
                        }
                    }
                }while(outcome==GC_RCAN_MORE);
                assert(reader.held==(uintptr_t)source&&access(source,&reader,GC_SA_EXIT)==GC_SA_OK);
                assert(cancellation.stage==4&&cancel_step(&admission,&cancellation)==GC_RCAN_INVALID);
                cleanup_jobs++;
                assert(atomic_load(&pages[2].allocated_top)==top&&atomic_load(&tables[1].used)==used);
                assert(gate(&admission,0,GC_RG_CHECK)==GC_RG_OK);
                SourceDrain ready={.incarnation=pages[0].incarnation,.top=atomic_load(&pages[0].allocated_top),
                    .used=atomic_load(&tables[0].used),.budget=256};
                assert(drain(&pages[0],&ready)==GC_SD_WAIT&&ready.cursor==1);
                assert(atomic_load(&pages[0].state)==GC_PAGE_CLOSED&&heap.committed==committed);
                assert(turn(&coordinator,&waiting,GC_RT_ACQUIRE)==GC_RT_WAIT);
                assert(turn(&coordinator,&owner,GC_RT_RESUME)==GC_RT_OK);
                assert(coordinator.gate_epoch==admission.epoch&&coordinator.active==1);
                destinations[i]=original[i];continue;
            }
            assert(target(&pages[2],&q,GC_TARGET_PREPARE)==GC_TARGET_OK);
            uint64_t top=atomic_load(&pages[2].allocated_top);
            ReferenceLease hidden={.reference=(q.destination<<16)|UINT64_C(0x1510)};
            assert(acquire(&leases,&hidden)==GC_ML_INVALID);
            if(i==1&&round%4==0){
                q.budget=7;
                assert(copy(source,&q)==GC_COPY_MORE);
                assert(target(&pages[2],&q,GC_TARGET_INSTALL)==GC_TARGET_INVALID);
                assert(acquire(&leases,&hidden)==GC_ML_INVALID);
                assert(atomic_load(&pages[2].allocated_top)==top&&atomic_load(&tables[1].used)==installed);
                // Clean the private range before PREPARE admits a fresh copy
                // request at the same top. Installed metadata is never reset.
                pipeline_discard(&admission,&job,gate,&pages[2],source,&q,discard);cleanup_jobs++;
                q=(RelocationCopy){(uintptr_t)mapping,source->incarnation,attempt.ticket,pages[0].limit,0,0,256,0,0};
                assert(target(&pages[2],&q,GC_TARGET_PREPARE)==GC_TARGET_OK&&q.destination==top+32);
            }
            uint64_t status;do{status=copy(source,&q);}while(status==GC_COPY_MORE);
            assert(status==GC_COPY_DONE&&q.cursor==lengths[i]);
            assert(target(&pages[2],&q,GC_TARGET_INSTALL)==GC_TARGET_OK);
            installed++;assert(atomic_load(&tables[1].used)==installed&&marks.used==5);
            bool interrupted=round>=64&&i==1;
            bool already_published=interrupted&&((round-64)/2)%2;
            destinations[i]=(q.destination<<16)|UINT64_C(0x1510);
            RelocationJob installed_job={.permit=(uintptr_t)&permit,.source=(uintptr_t)source,
                .ticket=q.ticket,.destination=destinations[i]};
            RelocationCompletion completion={.job=(uintptr_t)&installed_job,.copy=(uintptr_t)&q};
            assert(complete(&admission,&completion)==GC_RC_MORE&&completion.stage==1);
            RelocationCompletion copied_completion=completion;
            assert(complete(&admission,&copied_completion)==GC_RC_INVALID);
            assert(job_step(&admission,&installed_job,GC_RLJ_CHECK)==GC_RLJ_WAIT);
            RelocationJob duplicate_job={.permit=(uintptr_t)&permit,.source=(uintptr_t)source,
                .ticket=q.ticket,.destination=destinations[i]};
            assert(job_step(&admission,&duplicate_job,GC_RLJ_BIND)==GC_RLJ_INVALID);
            assert(gate(&admission,&job,GC_RG_RELEASE)==GC_RG_INVALID);
            assert(job_step(&admission,&installed_job,GC_RLJ_FINISH)==GC_RLJ_INVALID);
            if(interrupted){
                /* An installed candidate is no longer a private range. Keep
                 * this job's permit and both mappings through forward recovery;
                 * never rewind INSTALL or reuse the target as abandoned space. */
                if(already_published)assert(publish(source,&q,0x1510)==GC_COPY_DONE);
                assert(gate(&admission,0,GC_RG_CLOSE)==GC_RG_WAIT);
                assert(turn(&coordinator,&owner,GC_RT_RELEASE)==GC_RT_WAIT);
                assert(turn(&coordinator,&waiting,GC_RT_ACQUIRE)==GC_RT_WAIT);
                assert(gate(&admission,0,GC_RG_CHECK)==GC_RG_WAIT);
                RelocationPermit late={0};RelocationGateRequest denied={.permit=(uintptr_t)&late};
                assert(gate(&admission,&denied,GC_RG_ACQUIRE)==GC_RG_WAIT);
                AllocationPage saved_page;memcpy(&saved_page,&pages[2],sizeof(saved_page));
                ForwardCell saved_cells[5];memcpy(saved_cells,cells[1],sizeof(saved_cells));
                uint64_t saved_starts[256];memcpy(saved_starts,starts[1],sizeof(saved_starts));
                RelocationCopy saved_copy=q;
                uint64_t committed=heap.committed,charged=heap.metadata_committed;
                assert(target(&pages[2],&q,GC_TARGET_INSTALL)==GC_TARGET_INVALID);
                RelocationDiscard forbidden={.source=(uintptr_t)source,.copy=(uintptr_t)&q};
                assert(discard(&pages[2],&forbidden)==GC_DISCARD_INVALID&&!forbidden.owner&&!forbidden.cursor&&!forbidden.bytes);
                RelocationCancellation invalid_cancel={.permit=job.permit,.serial=job.serial,.epoch=job.epoch,
                    .target=(uintptr_t)&pages[2],.discard=(uintptr_t)&forbidden};
                assert(cancel_step(&admission,&invalid_cancel)==GC_RCAN_INVALID&&!invalid_cancel.owner&&!invalid_cancel.stage);
                PageRelease cleanup={.head_index=2,.page_count=2,.incarnation=pages[2].incarnation};
                PageRelease saved_cleanup=cleanup;
                assert(release(&heap,&cleanup,GC_RELEASE_BEGIN)==GC_RELEASE_INVALID);
                assert(!memcmp(&cleanup,&saved_cleanup,sizeof(cleanup)));
                if(!already_published){
                    assert(publish(source,&q,UINT64_C(0x10000))==GC_COPY_INVALID);
                    ResolveRequest blocked={.reference=original[i],.scope={.context=(uintptr_t)&ctx}};
                    assert(resolve(&pages[0],&blocked,GC_RESOLVE_ENTER)==GC_RESOLVE_WAIT);
                    assert(!blocked.scope.held&&!atomic_load(&ctx.access_epoch));
                }
                assert(!memcmp(&saved_page,&pages[2],sizeof(saved_page)));
                assert(!memcmp(saved_cells,cells[1],sizeof(saved_cells)));
                assert(!memcmp(saved_starts,starts[1],sizeof(saved_starts)));
                assert(!memcmp(&saved_copy,&q,sizeof(q))&&atomic_load(&tables[1].used)==installed);
                assert(heap.committed==committed&&heap.metadata_committed==charged);
                /* Installation exposes metadata, so preserve a lease across
                 * recovery. The source claim still owns publication; READ is
                 * a receipt check, not permission for another actor to copy. */
                ReferenceLease destination={.reference=destinations[i]};
                assert(acquire(&leases,&destination)==GC_ML_OK);
                RelocationJob copied_job=installed_job;
                assert(job_step(&admission,&copied_job,GC_RLJ_CHECK)==GC_RLJ_INVALID);
                assert(complete(&admission,&completion)==GC_RC_MORE&&completion.stage==2);
                assert(installed_job.state==2);
                ResolveRequest guard={.reference=destinations[i],.scope={.context=(uintptr_t)&ctx}};
                assert(resolve(&pages[2],&guard,GC_RESOLVE_ENTER)==GC_RESOLVE_OK);
                ObjectHeader *header=(ObjectHeader *)((char *)mapping+q.destination-32);
                assert(header->mark_record==(uintptr_t)&records[i]&&header->payload_bytes==lengths[i]);
                for(uint64_t j=0;j<lengths[i];j++)assert(((unsigned char *)(header+1))[j]==(unsigned char)(i+j*17));
                assert(resolve(&pages[2],&guard,GC_RESOLVE_EXIT)==GC_RESOLVE_OK);
                assert(metadata(&leases,&destination.lease,GC_ML_RELEASE)==GC_ML_OK);
                assert(gate(&admission,0,GC_RG_CHECK)==GC_RG_WAIT);
            }else assert(complete(&admission,&completion)==GC_RC_MORE&&completion.stage==2);
            assert(job_step(&admission,&installed_job,GC_RLJ_CHECK)==GC_RLJ_OK&&installed_job.state==2);
            assert(gate(&admission,&job,GC_RG_RELEASE)==GC_RG_INVALID);
            assert(complete(&admission,&completion)==GC_RC_MORE&&completion.stage==3&&installed_job.state==3&&!permit.job);
            assert(job_step(&admission,&installed_job,GC_RLJ_FINISH)==GC_RLJ_INVALID);
            assert(complete(&admission,&completion)==GC_RC_DONE&&completion.stage==4);
            assert(complete(&admission,&completion)==GC_RC_INVALID);
            assert(job_step(&admission,&installed_job,GC_RLJ_CHECK)==GC_RLJ_INVALID);
            if(interrupted){
                assert(gate(&admission,0,GC_RG_CHECK)==GC_RG_OK);
                assert(gate(&admission,&job,GC_RG_RELEASE)==GC_RG_INVALID);
                assert(turn(&coordinator,&waiting,GC_RT_ACQUIRE)==GC_RT_WAIT);
                assert(turn(&coordinator,&owner,GC_RT_RESUME)==GC_RT_OK);
                assert(coordinator.gate_epoch==admission.epoch&&coordinator.active==1);
            }
        }
        assert((destinations[4]>>16)>=pages[3].start); // object start in a run tail
        assert(!admission.active&&!admission.head&&admission.last_serial==5);
        assert(gate(&admission,0,GC_RG_CLOSE)==GC_RG_OK);
        SourceDrain completed={.incarnation=pages[0].incarnation,.top=atomic_load(&pages[0].allocated_top),
            .used=atomic_load(&tables[0].used),.budget=256};
        assert(drain(&pages[0],&completed)==(cancel?GC_SD_WAIT:GC_SD_DONE));
        assert(completed.cursor==(cancel?1:5));
        assert(atomic_load(&pages[0].state)==(cancel?GC_PAGE_CLOSED:GC_PAGE_EVACUATED));
        if(!cancel)pool_vm_decommit((char *)mapping+pages[0].start,2*GC_HEAP_PAGE_BYTES);
        for(unsigned i=0;i<5;i++){
            ReferenceLease old={.reference=original[i]};assert(acquire(&leases,&old)==GC_ML_OK);
            ResolveRequest hop={.reference=original[i],.scope={.context=(uintptr_t)&ctx}};
            if(cancel&&i==1){
                assert(resolve(&pages[0],&hop,GC_RESOLVE_ENTER)==GC_RESOLVE_OK);
                assert(resolve(&pages[0],&hop,GC_RESOLVE_EXIT)==GC_RESOLVE_OK);
            }else assert(resolve(&pages[0],&hop,GC_RESOLVE_ENTER)==GC_RESOLVE_FORWARDED&&hop.destination==destinations[i]);
            assert(metadata(&leases,&old.lease,GC_ML_RELEASE)==GC_ML_OK);
            ReferenceLease current={.reference=destinations[i]};assert(acquire(&leases,&current)==GC_ML_OK);
            AllocationPage *canonical=&pages[cancel&&i==1?0:2];
            assert(current.lease.page==(uintptr_t)canonical);
            ResolveRequest guard={.reference=destinations[i],.scope={.context=(uintptr_t)&ctx}};
            assert(resolve(canonical,&guard,GC_RESOLVE_ENTER)==GC_RESOLVE_OK);
            ObjectHeader *h=(ObjectHeader *)((char *)mapping+(destinations[i]>>16)-32);
            assert(h->payload_bytes==lengths[i]&&h->mark_record==(uintptr_t)&records[i]&&records[i].old_epoch==round+1);
            assert(records[i].object_id==pages[0].identity_base+((cells[0][i].source_offset-pages[0].start)>>4));
            for(uint64_t j=0;j<lengths[i];j++)assert(((unsigned char *)(h+1))[j]==(unsigned char)(i+j*17));
            assert(resolve(canonical,&guard,GC_RESOLVE_EXIT)==GC_RESOLVE_OK);
            assert(metadata(&leases,&current.lease,GC_ML_RELEASE)==GC_ML_OK);
        }
        assert(!leases.active&&!pages[0].metadata_leases&&!pages[2].metadata_leases);
        // This fixture destroys the complete heap after releasing every lease;
        // no source reference/lease can outlive teardown. Production owners
        // must instead establish their incoming-reference/retirement audit.
        assert(turn(&coordinator,&waiting,GC_RT_ACQUIRE)==GC_RT_WAIT);
        pool_vm_release(mapping,bytes);
        assert(turn(&coordinator,&owner,GC_RT_RELEASE)==GC_RT_OK);
        assert(turn(&coordinator,&waiting,GC_RT_ACQUIRE)==GC_RT_OK&&coordinator.active==2);
        assert(gate(&old_admission,NULL,GC_RG_REOPEN)==GC_RG_OK);
        assert(gate(&old_admission,NULL,GC_RG_CLOSE)==GC_RG_OK);
        assert(turn(&coordinator,&waiting,GC_RT_RELEASE)==GC_RT_OK&&!coordinator.active);
    }
    assert(cleanup_jobs==48);
    puts("relocation turn pipeline: 96 real-copy turns retain ownership through cancellation, installed recovery and source teardown before Old admission PASS");
    puts("relocation discard pipeline: 48 bounded cleanups retain admission and 32 installed targets reject discard PASS");
    puts("relocation completion pipeline: 448 installed jobs and 64 moving graph rounds use the shared completion funnel PASS");
    puts("relocation job pipeline: 448 installed jobs authenticate publication and reject completion after permit release PASS");
    puts("relocation installed recovery: 32 interrupted jobs preserve installed targets and complete forwarding before permit release PASS");
    puts("relocation cancel pipeline: 32 resumable cancellations reopen guarded readers before release and reject 32 installed jobs PASS");
    puts("relocation cancellation: 32 failures at four private-copy stages retain source access and drain admission PASS");
    puts("relocation target: 32 packed Young/Old runs, partial cleanup, shared identities and protected-source reads PASS");
}
#endif
