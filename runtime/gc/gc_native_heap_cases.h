#ifndef KLASSIC_GC_NATIVE_HEAP_CASES_H
#define KLASSIC_GC_NATIVE_HEAP_CASES_H
#include "gc_native_heap.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>
static void *gc_heap_map_failure(size_t bytes){(void)bytes;return NULL;}
static bool gc_heap_unmap_failure(void *p,size_t bytes){(void)p;(void)bytes;return false;}
static void gc_run_native_heap_cases(void){
    NativeHeapOps ops=gc_native_heap_ops();
    NativeHeapConfig config={4*GC_HEAP_PAGE_BYTES,2*GC_HEAP_PAGE_BYTES,GC_HEAP_PAGE_BYTES,8*ops.metadata.quantum};
    for(unsigned fault=0;fault<7;fault++){
        NativeHeapOwner owner={0},before=owner;NativeHeapConfig bad=config;NativeHeapOps bad_ops=ops;
        if(fault==0)bad.hard_max=0;else if(fault==1)bad.hard_max++;
        else if(fault==2)bad.hard_max=GC_MAX_OFFSET;
        else if(fault==3)bad.soft_max=bad.hard_max+1;
        else if(fault==4)bad.relocation_reserve=bad.hard_max+1;
        else if(fault==5)bad.metadata_max=1;else bad_ops.metadata.quantum=3;
        assert(gc_native_heap_prepare(&owner,&bad,&bad_ops)==GC_NATIVE_METADATA_INVALID);
        assert(!memcmp(&owner,&before,sizeof(owner)));
    }
    for(unsigned mode=0;mode<5;mode++){
        NativeHeapOwner owner={0};NativeHeapOps injected=ops;
        if(mode==0)injected.reserve=gc_heap_map_failure;
        if(mode==1)injected.metadata.map=gc_heap_map_failure;
        uint64_t result=gc_native_heap_prepare(&owner,&config,&injected);
        assert(result==(mode<2?GC_NATIVE_METADATA_OS_ERROR:GC_NATIVE_METADATA_OK));
        if(mode<2){
            assert(owner.stage==1&&!gc_native_heap_prepared(&owner));
            if(mode==1){
                uint64_t retained=owner.reservation;assert(retained&&!owner.table);
                injected=ops;injected.release=gc_heap_unmap_failure;
                assert(gc_native_heap_dispose(&owner,&injected)==GC_NATIVE_METADATA_OS_ERROR);
                assert(owner.stage==4&&owner.reservation==retained&&!gc_native_heap_prepared(&owner));
            }
            assert(gc_native_heap_dispose(&owner,&ops)==GC_NATIVE_METADATA_OK);continue;
        }
        HeapPool *heap=gc_native_heap_prepared(&owner);assert(heap&&heap->page_count==4);
        assert(owner.reservation_bytes==config.hard_max+GC_HEAP_PAGE_BYTES);
        assert(heap->metadata_committed==owner.table_bytes&&owner.table_bytes>=4*sizeof(AllocationPage));
        NativeHeapOwner copied=owner;assert(!gc_native_heap_prepared(&copied));
        assert(gc_native_heap_dispose(&copied,&ops)==GC_NATIVE_METADATA_INVALID);
        assert(gc_native_heap_prepare(&owner,&config,&ops)==GC_NATIVE_METADATA_INVALID);
        for(unsigned generation=0;generation<2;generation++){
            PageLease lease={.page_count=2,.generation=generation};
            assert(gc_pool_step_locked(heap,&lease,GC_POOL_RESERVE)==GC_POOL_OK);
            AllocationPage *pages=(void *)(uintptr_t)heap->pages;
            assert(pages[lease.head_index].start>=GC_HEAP_PAGE_BYTES);
            assert(gc_native_heap_dispose(&owner,&ops)==GC_NATIVE_METADATA_INVALID&&owner.stage==2);
            PageLease stale=lease;stale.incarnation++;
            assert(gc_native_heap_commit_lease(&owner,&stale)==GC_NATIVE_METADATA_INVALID);
            for(unsigned reuse=0;reuse<2;reuse++){
                assert(gc_native_heap_commit_lease(&owner,&lease)==GC_NATIVE_METADATA_OK);
                assert(gc_native_heap_commit_lease(&owner,&lease)==GC_NATIVE_METADATA_INVALID);
                unsigned char *data=(void *)(uintptr_t)(heap->heap_base+pages[lease.head_index].start);
                size_t bytes=(size_t)(lease.page_count<<GC_HEAP_PAGE_SHIFT);
                for(size_t i=0;i<bytes;i++)assert(data[i]==0);
                memset(data,0xa5,bytes);
                assert(gc_pool_step_locked(heap,&lease,GC_POOL_ABORT)==GC_POOL_INVALID);
                assert(gc_native_heap_decommit_lease(&owner,&lease)==GC_NATIVE_METADATA_OK);
                assert(gc_native_heap_decommit_lease(&owner,&lease)==GC_NATIVE_METADATA_INVALID);
            }
            assert(gc_pool_step_locked(heap,&lease,GC_POOL_ABORT)==GC_POOL_OK);
            assert(gc_native_heap_commit_lease(&owner,&lease)==GC_NATIVE_METADATA_INVALID);
        }
        AllocationPage *pages=(void *)(uintptr_t)heap->pages;
        pages[3].metadata_leases=1;
        assert(gc_native_heap_dispose(&owner,&ops)==GC_NATIVE_METADATA_INVALID&&owner.stage==2);
        heap->page_count=1;
        assert(gc_native_heap_dispose(&owner,&ops)==GC_NATIVE_METADATA_INVALID&&owner.stage==2);
        heap->page_count=4;pages[3].metadata_leases=0;pages[3].forward_table=8;
        assert(gc_native_heap_dispose(&owner,&ops)==GC_NATIVE_METADATA_INVALID&&owner.stage==2);
        pages[3].forward_table=0;heap->metadata_committed+=8;
        assert(gc_native_heap_dispose(&owner,&ops)==GC_NATIVE_METADATA_INVALID&&owner.stage==2);
        heap->metadata_committed-=8;
        injected=ops;
        if(mode==3)injected.release=gc_heap_unmap_failure;
        if(mode==4)injected.metadata.unmap=gc_heap_unmap_failure;
        uint64_t reservation=owner.reservation,table=owner.table,bytes=owner.table_bytes;
        result=gc_native_heap_dispose(&owner,&injected);
        if(mode>2){
            assert(result==GC_NATIVE_METADATA_OS_ERROR&&owner.stage==4&&!gc_native_heap_prepared(&owner));
            assert(owner.table==table&&owner.heap.metadata_committed==bytes);
            assert(mode==3?owner.reservation==reservation:owner.reservation==0);
            assert(gc_native_heap_dispose(&owner,&ops)==GC_NATIVE_METADATA_OK);
        }else assert(result==GC_NATIVE_METADATA_OK);
        assert(owner.stage==3&&!owner.reservation&&!owner.table&&!owner.heap.metadata_committed);
        assert(gc_native_heap_dispose(&owner,&ops)==GC_NATIVE_METADATA_INVALID);
    }
    puts("native heap bootstrap: checked guard-offset reservation, rounded table budget and retryable private disposal PASS");
}
#endif
