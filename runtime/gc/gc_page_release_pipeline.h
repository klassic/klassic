/* Real OS cleanup/recommit with an exclusively owned private target run.
 * Resource failures are injected before the corresponding cleanup operation;
 * the callback cannot prove that an external deallocator actually succeeded. */
#ifndef KLASSIC_GC_PAGE_RELEASE_PIPELINE_H
#define KLASSIC_GC_PAGE_RELEASE_PIPELINE_H
#include "gc_page_release_cases.h"
#include "gc_relocate_pipeline.h"
#include "gc_budget_cases.h"
static void gc_run_page_release_pipeline(PoolStep pool,PageReleaser release,BudgetStep budget,
        TlabPage bind,TlabAllocator allocate,RelocateTarget target,RelocateCopy copy,ForwardStep forward,SourceAccess access){
    for(unsigned round=0;round<32;round++){
        AllocationPage pages[3]={0};ForwardCell cell={0};ForwardTable source_table={(uintptr_t)&cell,1,0};
        MarkRecord record={0};MarkArena arena={(uintptr_t)&record,1,0};
        size_t reservation=4*GC_HEAP_PAGE_BYTES;void *mapping=pool_vm_reserve(reservation);
        uint64_t baseline=sizeof(pages)+sizeof(cell)+sizeof(source_table)+sizeof(record);
        uint64_t table_bytes=sizeof(ForwardTable)+2*sizeof(ForwardCell);
        HeapPool heap={.heap_base=(uintptr_t)mapping,.pages=(uintptr_t)pages,.page_count=3,
            .hard_max=3*GC_HEAP_PAGE_BYTES,.soft_max=GC_HEAP_PAGE_BYTES,.relocation_reserve=2*GC_HEAP_PAGE_BYTES,
            .metadata_committed=baseline,.metadata_max=baseline+3*GC_HEAP_PAGE_BYTES/128+table_bytes};
        PageLease source={.page_count=1};assert(pool(&heap,&source,GC_POOL_RESERVE)==GC_POOL_OK);
        pool_vm_commit((char *)mapping+pages[0].start,GC_HEAP_PAGE_BYTES);
        void *source_bitmap=calloc(1,GC_HEAP_PAGE_BYTES/128);assert(source_bitmap);
        source.starts=(uintptr_t)source_bitmap;source.committed_mapping=1;assert(pool(&heap,&source,GC_POOL_COMMIT)==GC_POOL_OK);
        pages[0].forward_table=(uintptr_t)&source_table;
        MutatorContext ctx={.abi_version=GC_ABI_VERSION,.thread_id=17};assert(bind(&ctx,&pages[0],0)==GC_ALLOC_OK);
        TypeDescriptor descriptor={.id=1,.alignment=16,.fixed_bytes=1025};AllocationResult out={0};
        AllocationRequest allocation={(uintptr_t)&pages[0],(uintptr_t)&descriptor,0,(uintptr_t)&out,(uintptr_t)&arena};
        assert(allocate(&ctx,&allocation)==GC_ALLOC_OK);
        ObjectHeader *header=(ObjectHeader *)((char *)mapping+(out.reference>>16)-32);
        memset(header+1,0x5a,1025);atomic_store(&header->state,GC_OBJECT_PUBLISHED);assert(bind(&ctx,&pages[0],1)==GC_ALLOC_OK);
        PageRelease forbidden={.head_index=0,.page_count=1,.incarnation=source.incarnation};
        assert(release(&heap,&forbidden,GC_RELEASE_BEGIN)==GC_RELEASE_INVALID);
        PageLease candidate={.page_count=1+round%2,.generation=round%2,.relocation=1};
        assert(pool(&heap,&candidate,GC_POOL_RESERVE)==GC_POOL_OK&&candidate.head_index==1);
        uint64_t bytes=candidate.page_count*GC_HEAP_PAGE_BYTES;void *address=(char *)mapping+pages[1].start;
        pool_vm_commit(address,(size_t)bytes);void *bitmap=calloc(1,(size_t)(bytes/128));assert(bitmap);
        candidate.starts=(uintptr_t)bitmap;candidate.committed_mapping=1;assert(pool(&heap,&candidate,GC_POOL_COMMIT)==GC_POOL_OK);
        uint64_t admitted_committed=heap.committed,admitted_metadata=heap.metadata_committed;
        MetadataCharge charge={.bytes=table_bytes};unsigned mode=round%4;
        if(mode==0){
            heap.metadata_max=heap.metadata_committed+table_bytes-1;
            assert(budget(&heap,&charge,GC_BUDGET_RESERVE)==GC_BUDGET_LIMIT);
        }else{
            assert(budget(&heap,&charge,GC_BUDGET_RESERVE)==GC_BUDGET_OK);
            if(mode==1){
                // Native table allocation fails before any bytes are owned.
                assert(budget(&heap,&charge,GC_BUDGET_ABORT)==GC_BUDGET_OK);
            }else{
                ForwardTable *table=calloc(1,(size_t)table_bytes);assert(table);
                table->cells=(uintptr_t)(table+1);table->capacity=2;charge.allocation=(uintptr_t)table;
                assert(budget(&heap,&charge,GC_BUDGET_COMMIT)==GC_BUDGET_OK);pages[1].forward_table=(uintptr_t)table;
                ForwardRequest claim={cell.source_offset,cell.incarnation,1,0};assert(forward(&cell,&claim,GC_FWD_CLAIM)==GC_FWD_OK);
                SourceAccessRequest select={0,cell.source_offset,cell.incarnation,0,1};
                assert(access(&cell,&select,GC_SA_SELECT)==GC_SA_OK&&access(&cell,&select,GC_SA_CHECK)==GC_SA_OK);
                RelocationCopy job={(uintptr_t)mapping,cell.incarnation,1,pages[0].limit,0,0,256,0,0};
                assert(target(&pages[1],&job,GC_TARGET_PREPARE)==GC_TARGET_OK);
                uint64_t result=copy(&cell,&job);assert(result==GC_COPY_MORE);
                if(mode==3){do{result=copy(&cell,&job);}while(result==GC_COPY_MORE);assert(result==GC_COPY_DONE);}
                assert(atomic_load(&pages[1].allocated_top)==pages[1].start&&!atomic_load(&table->used));
                // job goes out of scope here; no private raw user survives BEGIN.
            }
        }
        PageRelease q={.head_index=candidate.head_index,.page_count=candidate.page_count,.incarnation=candidate.incarnation};
        assert(release(&heap,&q,GC_RELEASE_BEGIN)==GC_RELEASE_OK);
        assert(heap.committed==admitted_committed&&heap.metadata_committed==admitted_metadata+(mode>=2?table_bytes:0));
        assert(atomic_load(&pages[1].state)==GC_PAGE_RELEASING&&!pages[1].starts&&!pages[1].forward_table);
        RelocationCopy rejected={.heap_base=(uintptr_t)mapping};assert(target(&pages[1],&rejected,GC_TARGET_PREPARE)==GC_TARGET_INVALID);
        PageLease blocked={.page_count=3,.relocation=1};assert(pool(&heap,&blocked,GC_POOL_RESERVE)==GC_POOL_HEAP_LIMIT);
        // Simulated cleanup failure: leave each resource owned until success.
        assert(release(&heap,&q,GC_RELEASE_FINISH)==GC_RELEASE_INVALID);
        assert(heap.committed==admitted_committed);
        pool_vm_decommit(address,(size_t)bytes);q.committed_mapping=0;
        assert(release(&heap,&q,GC_RELEASE_FINISH)==GC_RELEASE_INVALID);
        free((void *)(uintptr_t)q.starts);q.starts=0;
        if(q.forward_table){
            assert(release(&heap,&q,GC_RELEASE_FINISH)==GC_RELEASE_INVALID);
            free((void *)(uintptr_t)q.forward_table);q.forward_table=0;charge.allocation=0;
            assert(budget(&heap,&charge,GC_BUDGET_RELEASE)==GC_BUDGET_OK);
        }
        assert(release(&heap,&q,GC_RELEASE_FINISH)==GC_RELEASE_OK);
        assert(heap.committed==GC_HEAP_PAGE_BYTES&&heap.metadata_committed==baseline+GC_HEAP_PAGE_BYTES/128&&!heap.pressure);
        assert(!heap.pending&&!heap.metadata_pending);
        assert(release(&heap,&q,GC_RELEASE_FINISH)==GC_RELEASE_INVALID);
        if(mode>=2){
            ForwardRequest retain={cell.source_offset,cell.incarnation,1,0};assert(forward(&cell,&retain,GC_FWD_RETAIN)==GC_FWD_OK);
            SourceAccessRequest reopen={0,cell.source_offset,cell.incarnation,0,0};assert(access(&cell,&reopen,GC_SA_REOPEN)==GC_SA_OK);
            SourceAccessRequest guard={(uintptr_t)&ctx,cell.source_offset,cell.incarnation,0,0};assert(access(&cell,&guard,GC_SA_ENTER)==GC_SA_OK);
            assert(header->mark_record==(uintptr_t)&record&&record.object_id==out.object_id);
            for(unsigned i=0;i<1025;i++)assert(((unsigned char *)(header+1))[i]==0x5a);
            assert(access(&cell,&guard,GC_SA_EXIT)==GC_SA_OK);
        }
        // The same physical run is reusable only now, with fresh nonwrapping keys.
        heap.metadata_max=baseline+3*GC_HEAP_PAGE_BYTES/128+table_bytes;
        PageLease again={.page_count=candidate.page_count,.generation=round%2,.relocation=1};
        assert(pool(&heap,&again,GC_POOL_RESERVE)==GC_POOL_OK&&again.head_index==candidate.head_index);
        assert(again.incarnation>candidate.incarnation&&again.identity_base>candidate.identity_base);
        pool_vm_commit(address,(size_t)bytes);
        for(uint64_t i=0;i<bytes;i++)assert(((unsigned char *)address)[i]==0);
        again.starts=(uintptr_t)calloc(1,(size_t)(bytes/128));assert(again.starts);again.committed_mapping=1;
        assert(pool(&heap,&again,GC_POOL_COMMIT)==GC_POOL_OK);
        PageRelease stale={.head_index=q.head_index,.page_count=q.page_count,.incarnation=q.incarnation};
        assert(release(&heap,&stale,GC_RELEASE_BEGIN)==GC_RELEASE_INVALID);
        PageRelease fresh={.head_index=again.head_index,.page_count=again.page_count,.incarnation=again.incarnation};
        assert(release(&heap,&fresh,GC_RELEASE_BEGIN)==GC_RELEASE_OK);
        pool_vm_decommit(address,(size_t)bytes);fresh.committed_mapping=0;free((void *)(uintptr_t)fresh.starts);fresh.starts=0;
        assert(release(&heap,&fresh,GC_RELEASE_FINISH)==GC_RELEASE_OK);
        assert(heap.committed==GC_HEAP_PAGE_BYTES&&heap.metadata_committed==baseline+GC_HEAP_PAGE_BYTES/128);
        free(source_bitmap);pool_vm_release(mapping,reservation);
    }
    puts("page release: 32 real partial-copy cleanup, metadata failure, retention and zeroed-recommit rounds PASS");
}
#endif
