/* Real barriers/relocation/OS owners; no alternate heap or collector. */
static void bulk_validated(void *,void *,void *,int);
static void bulk_copied(void *,unsigned long);
#define KG_BULK_VALIDATED_HOOK(r,src,dst,reverse) bulk_validated(r,src,dst,reverse)
#define KG_BULK_COPIED_HOOK(r,n) bulk_copied(r,n)
#include "klassic_gen_runtime.c"
#ifndef KLASSIC_GEN_BULK_IMPL_H
#include "klassic_gen_bulk_impl.h"
#endif
#include "gc_native_driver.c"
#include "klassic_gen_remembered.c"
#include "klassic_gen_policy.c"
#include "klassic_gen_verify.c"
#include <assert.h>
static _Thread_local unsigned move_copy,moved,stored,reject_copy;
static _Thread_local RootCell *moving_source,*moving_destination,*clear_source,*clear_destination;
static void bulk_validated(void *runtime,void *source,void *destination,int reverse){
    (void)reverse;moving_source=source;moving_destination=destination;
    if(clear_source){kg_root_clear(clear_source);kg_root_clear(clear_destination);clear_source=clear_destination=NULL;}
    if(move_copy){
        uint64_t before=atomic_load(&moving_source->value)>>16;
        kg_driver_pressure(runtime,KG_PRESSURE_HEAP);
        moved+=(atomic_load(&moving_source->value)>>16)!=before;
    }
}
static void bulk_copied(void *runtime,unsigned long n){
    /* Argument-error cases must abort before ANY completed destination slot.
     * Exit 87 exposes a partial copy even if a later slot would diagnose. */
    (void)n;if(reject_copy)_Exit(87);stored++;
    if(move_copy){
        uint64_t before=atomic_load(&moving_destination->value)>>16;
        kg_driver_pressure(runtime,KG_PRESSURE_HEAP);
        moved+=(atomic_load(&moving_destination->value)>>16)!=before;
    }
}
static RootCell *sequence(unsigned length,unsigned start){
    RootCell *record=kg_root_new(),*leaf=kg_root_new();kg_alloc_into(length*8,2,record);
    for(unsigned i=0;i<length;i++){kg_alloc_into(8,1,leaf);kg_store_i64(leaf,0,start+i);kg_store_ref(record,i*8,leaf);}
    kg_root_release(leaf);return record;
}
static void values(const RootCell *record,const unsigned *expected,unsigned count){
    RootCell *leaf=kg_root_new();for(unsigned i=0;i<count;i++){kg_load_ref_into(record,i*8,leaf);assert(kg_load_i64(leaf,0)==expected[i]);}kg_root_release(leaf);
}
static void finish(void){
    kg_shutdown();assert(!kg_runtime.native.backing.heap.metadata_committed&&!kg_runtime.native.backing.heap.metadata_pending);
}
static void overlaps(bool relocate){
    RootCell *record=sequence(8,1),*alias=kg_root_new();kg_root_copy(alias,record);
    if(relocate)move_copy=1;
    kg_copy_refs(alias,16,record,0,6);unsigned right[]={1,2,1,2,3,4,5,6};values(record,right,8);
    kg_copy_refs(record,0,alias,16,6);unsigned left[]={1,2,3,4,5,6,5,6};values(record,left,8);
    unsigned before=stored;kg_copy_refs(record,0,alias,0,8);kg_copy_refs(record,64,alias,64,0);assert(stored==before);
    if(relocate)assert(moved>0);
    kg_root_release(alias);kg_root_release(record);finish();puts("bulk overlap: actual logical aliases, both directions, empty/self and canonical movement PASS");
}
static void bindings(void){
    RootCell *source=sequence(4,11),*destination=sequence(4,20),*kept=kg_root_new();kg_root_copy(kept,destination);
    clear_source=source;clear_destination=destination;kg_copy_refs(destination,0,source,0,4);
    assert(!atomic_load(&source->value)&&!atomic_load(&destination->value));
    unsigned expected[]={11,12,13,14};values(kept,expected,4);
    kg_root_release(kept);kg_root_release(destination);kg_root_release(source);finish();
    puts("bulk bindings: actual caller roots cleared after preflight, internal logical receivers remain strong PASS");
}
static void metadata_full(void){
    KgRuntime *r=&kg_runtime;RootCell *record=sequence(2,1);MetadataLeaseRequest held[8192];uint64_t count=0;
    kg_root_copy(record,record);kg_lock(r);uint64_t capacity=kg_leases(r)->capacity;
    ReferenceLease source={.reference=atomic_load(&record->value)};assert(!gc_reference_lease_locked(kg_leases(r),&source));
    assert(capacity<8192);held[count++]=source.lease;
    for(;;){MetadataLeaseRequest token={.page=source.lease.page,.incarnation=source.lease.incarnation};
        uint64_t status=gc_metadata_step_locked(kg_leases(r),&token,GC_ML_ACQUIRE);
        if(status==GC_ML_FULL)break;assert(!status&&count<8192);held[count++]=token;}
    assert(!kg_leases(r)->free_head);kg_unlock(r);
    assert(kgb_identity(r,record,0,16));kg_lock(r);assert(kg_leases(r)->capacity>capacity);
    for(uint64_t i=0;i<count;i++){MetadataLeaseRequest token={.token=held[i].token};
        assert(!gc_metadata_step_locked(kg_leases(r),&token,GC_ML_RESOLVE)&&token.page==held[i].page&&token.incarnation==held[i].incarnation);
        assert(!gc_metadata_step_locked(kg_leases(r),&held[i],GC_ML_RELEASE));}kg_unlock(r);
    kg_root_release(record);finish();puts("bulk preflight FULL: real metadata lease growth retains old tokens, guard cleanup and budget release PASS");
}
static void generations(void){
    RootCell *destination=sequence(8,20),*empty=kg_root_new();
    for(unsigned i=0;i<4;i++)kg_driver_pressure(&kg_runtime,KG_PRESSURE_HEAP);
    kg_lock(&kg_runtime);ReferenceLease old={.reference=atomic_load(&destination->value)};assert(!gc_reference_lease_locked(kg_leases(&kg_runtime),&old));
    assert(((AllocationPage *)(uintptr_t)old.lease.page)->generation==GC_OLD);assert(!gc_metadata_step_locked(kg_leases(&kg_runtime),&old.lease,GC_ML_RELEASE));kg_unlock(&kg_runtime);
    RootCell *source=sequence(8,100);kg_copy_refs(destination,0,source,0,8);kg_root_release(source);
    for(unsigned i=0;i<4;i++)kg_driver_pressure(&kg_runtime,KG_PRESSURE_HEAP);
    unsigned expected[]={100,101,102,103,104,105,106,107};values(destination,expected,8);
    RootCell *nulls=kg_root_new();kg_alloc_into(64,2,nulls);kg_copy_refs(destination,0,nulls,0,8);
    RootCell *check=kg_root_new();for(unsigned i=0;i<8;i++){kg_load_ref_into(destination,i*8,check);assert(!atomic_load(&check->value));}
    kg_root_release(check);kg_root_release(nulls);kg_root_release(empty);kg_root_release(destination);finish();
    puts("bulk barriers: actual Old-to-Young sole edges survive collection, null overwrite and metadata release PASS");
}
typedef struct {RootCell *source,*destination;pthread_barrier_t *start;unsigned index;} BulkOwner;
static void wait_native(pthread_barrier_t *barrier){kg_native_enter();int s=pthread_barrier_wait(barrier);assert(!s||s==PTHREAD_BARRIER_SERIAL_THREAD);kg_native_exit();}
static void *copy_owner(void *argument){
    BulkOwner *job=argument;kg_thread_attach();RootCell *source=kg_root_new(),*destination=kg_root_new(),*temporary=kg_root_new();
    kg_root_import(source,job->source);kg_root_import(destination,job->destination);wait_native(job->start);
    for(unsigned i=0;i<12;i++){
        /* Every writer stores the same logical values into shared receivers;
         * interleaved root/barrier publication is real without a snapshot claim. */
        kg_copy_refs(destination,(i&1)?0:job->index*64,source,0,8);
        kg_alloc_into(8,1,temporary);kg_store_i64(temporary,0,i);kg_poll();
    }
    kg_root_release(temporary);kg_root_release(destination);kg_root_release(source);kg_thread_detach();return NULL;
}
static void concurrent(unsigned count){
    RootCell *source=sequence(8,1),*destination=kg_root_new();kg_alloc_into(count*64,2,destination);
    pthread_t threads[8];BulkOwner jobs[8];pthread_barrier_t start;assert(!pthread_barrier_init(&start,NULL,count+1));
    for(unsigned i=0;i<count;i++){jobs[i]=(BulkOwner){source,destination,&start,i};assert(!pthread_create(&threads[i],NULL,copy_owner,&jobs[i]));}
    wait_native(&start);for(unsigned i=0;i<8;i++)kg_driver_pressure(&kg_runtime,KG_PRESSURE_HEAP);
    kg_native_enter();for(unsigned i=0;i<count;i++)assert(!pthread_join(threads[i],NULL));kg_native_exit();
    RootCell *leaf=kg_root_new();for(unsigned i=0;i<count*8;i++){kg_load_ref_into(destination,i*8,leaf);assert(kg_load_i64(leaf,0)==i%8+1);}
    kg_root_release(leaf);kg_root_release(destination);kg_root_release(source);assert(!pthread_barrier_destroy(&start));finish();
    printf("bulk owners: %u actual shared-graph mutators, Young/Old copy, barrier publications and detach PASS\n",count);
}
int main(int argc,char **argv){
    assert(argc==2);kg_configure(32*1024*1024,1024*1024,16*1024*1024,4);kg_init();
    if(!strcmp(argv[1],"overlap"))overlaps(false);
    else if(!strcmp(argv[1],"moves"))overlaps(true);
    else if(!strcmp(argv[1],"generations"))generations();
    else if(!strcmp(argv[1],"bindings"))bindings();
    else if(!strcmp(argv[1],"full"))metadata_full();
    else if(!strcmp(argv[1],"2")||!strcmp(argv[1],"8"))concurrent((unsigned)atoi(argv[1]));
    else{
        reject_copy=1;
        RootCell *record=sequence(2,1),*other=kg_root_new();kg_alloc_into(16,1,other);
        if(!strcmp(argv[1],"type"))kg_copy_refs(other,0,record,0,1);
        else if(!strcmp(argv[1],"source-type"))kg_copy_refs(record,0,other,0,1);
        else if(!strcmp(argv[1],"null")){kg_root_clear(other);kg_copy_refs(record,0,other,0,1);}
        else if(!strcmp(argv[1],"alignment"))kg_copy_refs(record,1,record,0,1);
        else if(!strcmp(argv[1],"overflow"))kg_copy_refs(record,0,record,0,UINT64_MAX/8+1);
        else if(!strcmp(argv[1],"range"))kg_copy_refs(record,8,record,0,2);
        else if(!strcmp(argv[1],"offset-overflow"))kg_copy_refs(record,UINT64_MAX-7,record,0,2);
        else if(!strcmp(argv[1],"empty-range"))kg_copy_refs(record,24,record,0,0);
        else if(!strcmp(argv[1],"native")){kg_native_enter();kg_copy_refs(record,0,record,0,1);}
        abort();
    }
    return 0;
}
