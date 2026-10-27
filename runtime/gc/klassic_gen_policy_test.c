/* Deliberate corruption uses the actual private heap only in this diagnostic
 * executable; production clients retain the RootCell-only public interface. */
#include "klassic_gen_runtime.c"
#include "gc_native_driver.c"
#include "klassic_gen_remembered.c"
#include "klassic_gen_policy.c"
#include "klassic_gen_verify.c"
#include <assert.h>
int main(int argc,char **argv){
    const char *mode=argc>1?argv[1]:"minimum";
    uint64_t maximum=UINT64_C(8)*1024*1024;
    if(!strcmp(mode,"invalid-minimum")){kg_configure(maximum,maximum+1,0,0);kg_init();return 3;}
    bool negative=!strncmp(mode,"corrupt-",8);
    uint64_t minimum=!strcmp(mode,"rounded")?GC_HEAP_PAGE_BYTES+1:maximum;
    uint64_t floor=(minimum+GC_HEAP_PAGE_BYTES-1)&~(GC_HEAP_PAGE_BYTES-1);
    kg_configure(maximum,minimum,maximum/2,negative?0:5);kg_init();
    assert(kg_committed_bytes()==floor);
    RootCell *raw=kg_root_new(),*record=kg_root_new(),*scratch=kg_root_new();
    kg_alloc_into(8,1,raw);kg_store_i64(raw,0,UINT64_C(0xfeed0123456789ab));
    kg_alloc_into(8,2,record);kg_store_ref(record,0,raw);
    if(negative){
        uint64_t reference=atomic_load_explicit(&raw->value,memory_order_acquire);
        /* Valid GcRef bits naming an aligned interior non-object start. */
        uint64_t bad=reference+(UINT64_C(16)<<16);
        if(!strcmp(mode,"corrupt-root"))atomic_store_explicit(&raw->value,bad,memory_order_release);
        else if(!strcmp(mode,"corrupt-field")){
            uint64_t owner=atomic_load_explicit(&record->value,memory_order_acquire)>>16;
            _Atomic uint64_t *slot=(void *)(uintptr_t)(kg_runtime.native.backing.heap.heap_base+owner);
            atomic_store_explicit(slot,bad,memory_order_release);
        }else if(!strcmp(mode,"corrupt-forward")){
            kg_lock(&kg_runtime);AllocationPage *page=gc_native_allocation_page(&kg_runtime.current->native);
            ForwardTable *table=(void *)(uintptr_t)page->forward_table;ForwardCell *cells=(void *)(uintptr_t)table->cells;
            atomic_store_explicit(&cells[0].word,bad|2,memory_order_release);kg_unlock(&kg_runtime);
        }else return 4;
        kg_verify();return 5;
    }
    kg_alloc_into(0,1,scratch);kg_verify();
    for(unsigned i=0;i<96;i++){
        kg_alloc_into(256,1,scratch);kg_store_i64(scratch,0,i);
        assert(kg_committed_bytes()>=floor&&kg_committed_bytes()<=maximum);
        if(!(i%7)){
            kg_verify();kg_load_ref_into(record,0,scratch);
            assert(kg_load_i64(scratch,0)==UINT64_C(0xfeed0123456789ab));
        }
    }
    kg_verify();assert(kg_verification_count()>10);
    kg_lock(&kg_runtime);KgPolicy *p=kg_policy(&kg_runtime);
    assert(p->verified_roots&&p->verified_objects&&p->verified_fields&&p->verified_forwarding);
    /* Scalar bits shaped like references remain data through descriptor scan. */
    assert(kg_runtime.native.backing.heap.committed<=p->committed);
    kg_unlock(&kg_runtime);
    kg_root_release(raw);kg_root_release(record);kg_root_release(scratch);
    assert(kg_committed_bytes()>=floor);kg_shutdown();
    puts("canonical C backing policy: minimum, reuse, root/heap/forward verification and shutdown PASS");return 0;
}
