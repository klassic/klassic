/* Share the real OS/collector fixture; no collector stub or model substitution. */
#define main klassic_byte_pin_fixture_main
#include "klassic_gen_pin_test.c"
#undef main

static RootCell *reference_record(void){
    RootCell *record=kg_root_new(),*child=kg_root_new();kg_alloc_into(8,1,child);kg_store_i64(child,0,41);
    kg_alloc_into(16,2,record);kg_store_ref(record,0,child);kg_store_ref(record,8,record);kg_root_release(child);
    for(unsigned i=0;i<4;i++)kg_driver_pressure(&kg_runtime,KG_PRESSURE_HEAP);
    return record;
}
static void reference_pin(void){
    KgRuntime *r=&kg_runtime;RootCell *record=reference_record(),*child=kg_root_new(),*out=kg_root_new(),*empty=kg_root_new();
    KgPinnedObject *pin=kg_pin_object(record);assert(kg_pinned_object_size(pin)==16);KgPinnedBytes *control=pin_control(pin);uint64_t address=control->pin.address;
    kg_lock(r);AllocationPage *page=(void *)(uintptr_t)control->pin.root.follow.cleanup.page;assert(page->generation==GC_OLD);kg_unlock(r);
    kg_alloc_into(8,1,child);kg_store_i64(child,0,99);kg_native_enter();
    kg_pinned_store_ref(pin,0,child);assert(atomic_load_explicit(&kg_current_context(r)->state,memory_order_acquire)==GC_BLOCKED_NATIVE);
    kg_pinned_load_ref_into(pin,0,out);assert(atomic_load_explicit(&kg_current_context(r)->state,memory_order_acquire)==GC_BLOCKED_NATIVE);
    kg_native_exit();assert(kg_load_i64(out,0)==99);kg_root_clear(child);kg_root_clear(out);
    /* The pinned Old field is now the real surviving strong edge to Young. */
    kg_lock(r);uint64_t old_completed=r->native.cycles.completed[GC_OLD];kg_unlock(r);kg_driver_pressure(r,KG_PRESSURE_STRESS);
    kg_lock(r);assert(r->native.cycles.completed[GC_OLD]==old_completed);kg_unlock(r);
    for(unsigned i=0;i<8;i++){
        kg_driver_pressure(r,KG_PRESSURE_HEAP);kg_native_enter();kg_pinned_load_ref_into(pin,0,out);kg_native_exit();
        assert(kg_load_i64(out,0)==99&&control->pin.address==address);kg_root_clear(out);
    }
    kg_native_enter();kg_pinned_load_ref_into(pin,8,out);kg_native_exit();
    kg_load_ref_into(out,0,child);assert(kg_load_i64(child,0)==99);kg_root_clear(child);kg_root_clear(out);
    kg_native_enter();kg_pinned_store_ref(pin,0,empty);kg_pinned_load_ref_into(pin,0,out);kg_native_exit();
    kg_lock(r);assert(!atomic_load_explicit(&out->value,memory_order_acquire));kg_unlock(r);
    kg_verify();kg_unpin_object(pin);kg_root_release(empty);kg_root_release(out);kg_root_release(child);kg_root_release(record);empty_shutdown();
    puts("public reference pin: Old to Young rooted Native load/store, null prebarrier, cycles and movement preserve child PASS");
}
typedef struct {const RootCell *source;pthread_barrier_t *ready,*go;unsigned identity;} ReferencePinJob;
static void *reference_pin_owner(void *argument){
    ReferencePinJob *job=argument;kg_thread_attach();RootCell *record=kg_root_new(),*out=kg_root_new(),*value=kg_root_new();
    kg_root_import(record,job->source);KgPinnedObject *pin=kg_pin_object(record);kg_root_clear(record);kg_native_enter();
    pin_barrier(job->ready);pin_barrier(job->go);
    for(unsigned i=0;i<24;i++){
        kg_pinned_load_ref_into(pin,0,out);
        assert(atomic_load_explicit(&kg_current_context(&kg_runtime)->state,memory_order_acquire)==GC_BLOCKED_NATIVE);
        kg_native_exit();uint64_t read=kg_load_i64(out,0);assert(read==41||(read>=1000&&read<2000));
        kg_alloc_into(8,1,value);kg_store_i64(value,0,1000+job->identity*24+i);kg_native_enter();
        kg_pinned_store_ref(pin,0,value);
        assert(atomic_load_explicit(&kg_current_context(&kg_runtime)->state,memory_order_acquire)==GC_BLOCKED_NATIVE);
        kg_native_exit();kg_root_clear(out);kg_root_clear(value);kg_native_enter();
    }
    kg_native_exit();kg_unpin_object(pin);kg_root_release(value);kg_root_release(out);kg_root_release(record);kg_thread_detach();return NULL;
}
static void concurrent_reference_pins(unsigned count){
    RootCell *record=reference_record();pthread_barrier_t ready,go;pthread_t threads[8];ReferencePinJob jobs[8];
    assert(!pthread_barrier_init(&ready,NULL,count+1)&&!pthread_barrier_init(&go,NULL,count+1));kg_native_enter();
    for(unsigned i=0;i<count;i++){jobs[i]=(ReferencePinJob){record,&ready,&go,i};assert(!pthread_create(&threads[i],NULL,reference_pin_owner,&jobs[i]));}
    pin_barrier(&ready);pin_barrier(&go);kg_native_exit();
    for(unsigned i=0;i<12;i++){kg_driver_pressure(&kg_runtime,KG_PRESSURE_HEAP);kg_verify();}
    kg_native_enter();for(unsigned i=0;i<count;i++)assert(!pthread_join(threads[i],NULL));kg_native_exit();
    RootCell *out=kg_root_new();kg_load_ref_into(record,0,out);uint64_t result=kg_load_i64(out,0);assert(result>=1000&&result<2000);
    kg_root_release(out);kg_root_release(record);assert(!pthread_barrier_destroy(&ready)&&!pthread_barrier_destroy(&go));empty_shutdown();
    printf("public reference pin: %u actual owners race typed Native field barriers with Young/Old moves and detach PASS\n",count);
}
static void *foreign_reference_pin(void *handle){
    kg_thread_attach();RootCell *out=kg_root_new();kg_native_enter();kg_pinned_load_ref_into(handle,0,out);abort();
}
static void rejected_reference_pin(const char *mode){
    RootCell *root=kg_root_new(),*out=kg_root_new();
    if(strcmp(mode,"null"))kg_alloc_into(8,!strcmp(mode,"raw")?1:2,root);
    KgPinnedObject *pin=kg_pin_object(root);kg_native_enter();
    if(!strcmp(mode,"foreign")){pthread_t thread;assert(!pthread_create(&thread,NULL,foreign_reference_pin,pin));assert(!pthread_join(thread,NULL));}
    else if(!strcmp(mode,"address"))(void)kg_pinned_bytes_address(pin);
    else kg_pinned_load_ref_into(pin,!strcmp(mode,"offset")?8:0,out);
    abort();
}
int main(int argc,char **argv){
    kg_configure(8*1024*1024,0,4*1024*1024,4);kg_init();const char *mode=argc>1?argv[1]:"basic";
    if(!strcmp(mode,"2")||!strcmp(mode,"8"))concurrent_reference_pins((unsigned)strtoul(mode,NULL,10));
    else if(!strcmp(mode,"basic"))reference_pin();
    else rejected_reference_pin(mode);
    return 0;
}
