#ifndef KLASSIC_GC_NATIVE_BITMAP_CASES_H
#define KLASSIC_GC_NATIVE_BITMAP_CASES_H
#include "gc_native_bitmap.h"
#include "gc_native_page_release.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>
static void *gc_bitmap_fail_map(size_t bytes){(void)bytes;return NULL;}
static bool gc_bitmap_fail_unmap(void *address,size_t bytes){(void)address;(void)bytes;return false;}
static unsigned gc_bitmap_payload_calls;
static bool gc_bitmap_payload_partial_failure(void *address,size_t bytes){
    gc_bitmap_payload_calls++;
#ifndef _WIN32
    assert(mprotect(address,bytes,PROT_NONE)==0);
#else
    (void)address;(void)bytes;
#endif
    return false;
}
static void gc_run_native_bitmap_cases(void){
    for(unsigned generation=0;generation<2;generation++)for(unsigned mode=0;mode<7;mode++){
        NativeHeapOps ops=gc_native_heap_ops();NativeHeapOwner heap={0};
        uint64_t aligned_pages=ops.metadata.quantum/(GC_HEAP_PAGE_BYTES>>7);
        assert(aligned_pages>1);
        NativeHeapConfig config={2*aligned_pages*GC_HEAP_PAGE_BYTES,2*aligned_pages*GC_HEAP_PAGE_BYTES,0,16*ops.metadata.quantum};
        assert(gc_native_heap_prepare(&heap,&config,&ops)==GC_NATIVE_METADATA_OK);
        uint64_t baseline=heap.heap.metadata_committed;
        PageLease lease={.page_count=mode==5?aligned_pages:1,.generation=generation};
        assert(gc_pool_step_locked(&heap.heap,&lease,GC_POOL_RESERVE)==GC_POOL_OK);
        uint64_t logical=lease.page_count*(GC_HEAP_PAGE_BYTES>>7);
        NativeBitmapOwner bitmap={0};
        if(mode==0){
            heap.heap.metadata_max=baseline+logical;
            assert(gc_native_bitmap_reserve_locked(&bitmap,&heap.heap,&lease,&ops.metadata)==GC_BUDGET_LIMIT);
            assert(!bitmap.self&&heap.heap.metadata_pending==logical);
            heap.heap.metadata_max=config.metadata_max;
        }
        assert(gc_native_bitmap_reserve_locked(&bitmap,&heap.heap,&lease,&ops.metadata)==GC_BUDGET_OK);
        assert(bitmap.bytes%ops.metadata.quantum==0&&heap.heap.metadata_pending==bitmap.bytes);
        assert(bitmap.padding.bytes==bitmap.bytes-logical);
        if(mode==5)assert(!bitmap.padding.bytes);
        NativeBitmapOwner copy=bitmap;assert(!gc_native_bitmap_valid(&copy));
        NativeMetadataOps injected=ops.metadata;
        if(mode==1)injected.map=gc_bitmap_fail_map;
        uint64_t status=gc_native_bitmap_prepare(&bitmap,&injected);
        assert(status==(mode==1?GC_NATIVE_METADATA_OS_ERROR:GC_NATIVE_METADATA_OK));
        PageRelease release={0};PageRelease *receipt=NULL;
        if(mode!=1){
            assert(lease.starts==bitmap.mapping);
            const unsigned char *data=(void *)(uintptr_t)bitmap.mapping;
            for(uint64_t i=0;i<bitmap.bytes;i++)assert(!data[i]);
            assert(gc_native_heap_commit_lease(&heap,&lease)==GC_NATIVE_METADATA_OK);
            if(mode==3||mode==6){
                uint64_t pending=heap.heap.pending;heap.heap.pending=0;
                assert(gc_native_bitmap_publish_locked(&bitmap)==GC_POOL_INVALID);
                assert(bitmap.stage==2&&bitmap.padding.stage==2&&lease.starts==bitmap.mapping);
                assert(heap.heap.metadata_pending==logical);
                heap.heap.pending=pending;
            }
            if(mode>=3&&mode!=6){
                assert(gc_native_bitmap_publish_locked(&bitmap)==GC_POOL_OK);
                assert(bitmap.stage==3&&lease.stage==2&&!lease.starts&&!lease.committed_mapping);
                assert(heap.heap.metadata_pending==0&&heap.heap.metadata_committed==baseline+bitmap.bytes);
                assert(gc_native_bitmap_dispose(&bitmap,NULL,&ops.metadata)==GC_NATIVE_METADATA_INVALID);
                release=(PageRelease){.head_index=lease.head_index,.page_count=lease.page_count,
                    .incarnation=lease.incarnation};
                assert(gc_page_release_locked(&heap.heap,&release,GC_RELEASE_BEGIN)==GC_RELEASE_OK);
                receipt=&release;
                release.incarnation++;
                assert(gc_native_bitmap_dispose(&bitmap,receipt,&ops.metadata)==GC_NATIVE_METADATA_INVALID);
                release.incarnation--;
            }
        }
        injected=ops.metadata;
        if(mode==2||mode==4){
            injected.unmap=gc_bitmap_fail_unmap;
            uint64_t mapping=bitmap.mapping,pending=heap.heap.metadata_pending,committed=heap.heap.metadata_committed;
            assert(gc_native_bitmap_dispose(&bitmap,receipt,&injected)==GC_NATIVE_METADATA_OS_ERROR);
            assert(bitmap.mapping==mapping&&heap.heap.metadata_pending==pending&&heap.heap.metadata_committed==committed);
            assert(gc_native_bitmap_release_locked(&bitmap)==GC_BUDGET_INVALID);
        }
        assert(gc_native_bitmap_dispose(&bitmap,receipt,&ops.metadata)==GC_NATIVE_METADATA_OK);
        assert(gc_native_bitmap_release_locked(&bitmap)==GC_BUDGET_OK);
        assert(gc_native_bitmap_release_locked(&bitmap)==GC_BUDGET_INVALID);
        if(receipt){
            PageRelease before=release;
            uint64_t committed=heap.heap.committed,metadata=heap.heap.metadata_committed;
            gc_bitmap_payload_calls=0;
            for(unsigned fault=0;fault<7;fault++){
                release=before;
                if(fault==0)release.incarnation++;
                else if(fault==1)release.owner=0;
                else if(fault==2)release.stage=2;
                else if(fault==3)release.committed_mapping=2;
                else if(fault==4)release.page_count=0;
                else if(fault==5)release.head_index=heap.heap.page_count;
                else release.page_count=heap.heap.page_count+1;
                assert(gc_native_page_release_payload(&heap,&release,gc_bitmap_payload_partial_failure)==GC_NATIVE_METADATA_INVALID);
                assert(!gc_bitmap_payload_calls);
            }
            release=before;
            AllocationPage *head=(void *)(uintptr_t)heap.heap.pages;head+=release.head_index;
            atomic_store(&head->allocated_top,head->start+16);
            assert(gc_native_page_release_payload(&heap,&release,gc_bitmap_payload_partial_failure)==GC_NATIVE_METADATA_INVALID);
            assert(!gc_bitmap_payload_calls);atomic_store(&head->allocated_top,head->start);
            if(release.page_count>1){
                AllocationPage *tail=head+release.page_count-1;tail->incarnation++;
                assert(gc_native_page_release_payload(&heap,&release,gc_bitmap_payload_partial_failure)==GC_NATIVE_METADATA_INVALID);
                assert(!gc_bitmap_payload_calls);tail->incarnation--;
            }
            assert(gc_native_page_release_payload(&heap,&release,gc_bitmap_payload_partial_failure)==GC_NATIVE_METADATA_OS_ERROR);
            assert(gc_bitmap_payload_calls==1&&!memcmp(&release,&before,sizeof(release)));
            assert(heap.heap.committed==committed&&heap.heap.metadata_committed==metadata);
            assert(gc_page_release_locked(&heap.heap,&release,GC_RELEASE_FINISH)==GC_RELEASE_INVALID);
            assert(gc_native_page_release_payload(&heap,&release,NULL)==GC_NATIVE_METADATA_OK);
            assert(!release.committed_mapping&&heap.heap.committed==committed);
            assert(gc_native_page_release_payload(&heap,&release,NULL)==GC_NATIVE_METADATA_INVALID);
            assert(gc_page_release_locked(&heap.heap,&release,GC_RELEASE_FINISH)==GC_RELEASE_OK);
        }else{
            if(lease.committed_mapping)assert(gc_native_heap_decommit_lease(&heap,&lease)==GC_NATIVE_METADATA_OK);
            assert(gc_pool_step_locked(&heap.heap,&lease,GC_POOL_ABORT)==GC_POOL_OK);
        }
        assert(heap.heap.metadata_committed==baseline&&!heap.heap.metadata_pending&&!heap.heap.committed&&!heap.heap.pending);
        assert(gc_native_heap_dispose(&heap,&ops)==GC_NATIVE_METADATA_OK);
    }
    puts("native bitmap backing: rounded admission, page publication and retryable detached disposal PASS");
    puts("native page payload release: authenticated detachment, partial OS failure and deferred budget credit PASS");
}
#endif
