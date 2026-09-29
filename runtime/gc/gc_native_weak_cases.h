#ifndef KLASSIC_GC_NATIVE_WEAK_CASES_H
#define KLASSIC_GC_NATIVE_WEAK_CASES_H
#include "gc_native_weak.h"
#include "gc_weak_table_cases.h"
static NativeMetadataOps weak_backing_base;
static bool weak_backing_map_fail,weak_backing_unmap_fail;
static void *weak_backing_map(size_t bytes){return weak_backing_map_fail?NULL:weak_backing_base.map(bytes);}
static bool weak_backing_unmap(void *p,size_t bytes){return !weak_backing_unmap_fail&&weak_backing_base.unmap(p,bytes);}
static void gc_run_native_weak_cases(uint64_t (*budget)(HeapPool *,MetadataCharge *,uint64_t),
        const NativeMetadataOps *native,const WeakTableOps *table_ops){
    weak_backing_base=*native;NativeMetadataOps ops={native->quantum,weak_backing_map,weak_backing_unmap};
    uint64_t bytes=17;
    assert(gc_native_weak_size(0,ops.quantum,&bytes)==GC_NATIVE_METADATA_INVALID&&bytes==17);
    assert(gc_native_weak_size(UINT64_MAX,ops.quantum,&bytes)==GC_NATIVE_METADATA_INVALID&&bytes==17);
    assert(gc_native_weak_size(3,ops.quantum,&bytes)==GC_NATIVE_METADATA_OK);
    for(unsigned mode=0;mode<6;mode++){
        WeakTableFixture f;weak_table_fixture(&f);HeapPool *heap=&f.w.f.heap;
        heap->metadata_committed=sizeof(AllocationPage);heap->metadata_max=sizeof(AllocationPage)+bytes;
        MetadataCharge charge={.bytes=bytes};
        assert(!gc_native_weak_committed(&charge));
        heap->metadata_max--;
        assert(budget(heap,&charge,GC_BUDGET_RESERVE)==GC_BUDGET_LIMIT&&!charge.stage&&!heap->metadata_pending);
        heap->metadata_max++;
        assert(budget(heap,&charge,GC_BUDGET_RESERVE)==GC_BUDGET_OK);
        MetadataCharge before=charge;
        assert(gc_native_weak_prepare(&charge,NULL,3,&ops)==GC_NATIVE_METADATA_INVALID);
        assert(gc_native_weak_prepare(&charge,&f.w.gate,0,&ops)==GC_NATIVE_METADATA_INVALID);
        uint64_t owner=charge.owner;charge.owner+=8;
        assert(gc_native_weak_prepare(&charge,&f.w.gate,3,&ops)==GC_NATIVE_METADATA_INVALID);charge.owner=owner;
        assert(!memcmp(&before,&charge,sizeof(charge)));
        weak_backing_map_fail=mode==0;
        uint64_t status=gc_native_weak_prepare(&charge,&f.w.gate,3,&ops);
        if(!mode){
            assert(status==GC_NATIVE_METADATA_OS_ERROR&&!memcmp(&before,&charge,sizeof(charge)));
            assert(budget(heap,&charge,GC_BUDGET_ABORT)==GC_BUDGET_OK);
        }else{
            assert(status==GC_NATIVE_METADATA_OK&&!gc_native_weak_committed(&charge));
            WeakTable *t=(void *)(uintptr_t)charge.allocation;
            assert(t->capacity==3&&t->free_head==1&&!t->active&&!t->last_serial);
            if(mode>=3){
                assert(budget(heap,&charge,GC_BUDGET_COMMIT)==GC_BUDGET_OK);
                assert(gc_native_weak_committed(&charge)==t);
                WeakHandle handle={0};
                assert(gc_weak_table_new_locked(t,&f.w.admission,&f.w.f.ctx,&f.w.output,&handle,table_ops)==GC_WEAK_LOADED);
                before=charge;
                assert(gc_native_weak_dispose(&charge,&ops)==GC_NATIVE_METADATA_INVALID&&!memcmp(&before,&charge,sizeof(charge)));
                assert(gc_weak_table_release_locked(t,handle,table_ops)==GC_WEAK_RELEASED);
                assert(!f.w.f.pool.active&&!f.w.f.page.metadata_leases);
                /* Shutdown has joined all handle users; no stale handle is used
                 * after unmap. An empty table alone would not justify this. */
            }
            /* Corrupt free-list structure must not authorize unmap. */
            uint64_t next=t->entries[0].next_free;t->entries[0].next_free=1;
            before=charge;
            assert(gc_native_weak_dispose(&charge,&ops)==GC_NATIVE_METADATA_INVALID&&!memcmp(&before,&charge,sizeof(charge)));
            t->entries[0].next_free=next;
            t->entries[2].slot.lease.token=1;
            assert(gc_native_weak_dispose(&charge,&ops)==GC_NATIVE_METADATA_INVALID);
            t->entries[2].slot.lease.token=0;
            weak_backing_unmap_fail=mode==2||mode==4;
            HeapPool saved=*heap;before=charge;
            unsigned char *copy=malloc((size_t)bytes);assert(copy);memcpy(copy,t,(size_t)bytes);
            status=gc_native_weak_dispose(&charge,&ops);
            if(weak_backing_unmap_fail){
                assert(status==GC_NATIVE_METADATA_OS_ERROR&&!memcmp(&before,&charge,sizeof(charge)));
                assert(!memcmp(&saved,heap,sizeof(saved))&&!memcmp(copy,t,(size_t)bytes));
                assert(budget(heap,&charge,mode>=3?GC_BUDGET_RELEASE:GC_BUDGET_ABORT)==GC_BUDGET_INVALID);
                weak_backing_unmap_fail=false;
                assert(gc_native_weak_dispose(&charge,&ops)==GC_NATIVE_METADATA_OK);
            }else assert(status==GC_NATIVE_METADATA_OK);
            free(copy);
            assert(budget(heap,&charge,mode>=3?GC_BUDGET_RELEASE:GC_BUDGET_ABORT)==GC_BUDGET_OK);
        }
        assert(!heap->metadata_pending&&heap->metadata_committed==sizeof(AllocationPage));
        assert(gc_native_weak_dispose(&charge,&ops)==GC_NATIVE_METADATA_INVALID);
    }
    weak_backing_map_fail=weak_backing_unmap_fail=false;
    puts("weak backing: six budget, map, live-handle, free-list and disposal-retry scenarios PASS");
}
#endif
