/* Public pin API with actual collector phases, shared graphs and OS owners. */
static void pin_pre_enter(void *,void *);
static void pin_metadata_full(void *,void *);
#define KG_PIN_PRE_ENTER_HOOK(r,h) pin_pre_enter((r),(h))
#define KG_PIN_METADATA_FULL_HOOK(r,h) pin_metadata_full((r),(h))
#include "klassic_gen_runtime.c"
#include "gc_native_driver.c"
#include "klassic_gen_remembered.c"
#include "klassic_gen_policy.c"
#include "klassic_gen_verify.c"
#include <assert.h>
static void pin_barrier(pthread_barrier_t *b){int s=pthread_barrier_wait(b);assert(!s||s==PTHREAD_BARRIER_SERIAL_THREAD);}
static KgPinnedBytes *pin_control(const KgPinnedObject *handle){
    kg_lock(&kg_runtime);KgPinnedBytes *control=kgp_validate_locked(&kg_runtime,handle);kg_unlock(&kg_runtime);return control;
}
static unsigned fill_pin_enter,pin_full_seen;
static uint64_t pin_full_count,pin_full_base;
static MetadataLeaseRequest pin_full_holders[1024];
static void pin_pre_enter(void *runtime,void *handle){
    if(!fill_pin_enter)return;fill_pin_enter=0;KgRuntime *r=runtime;KgPinnedBytes *h=handle;
    kg_lock(r);MetadataLeasePool *pool=kg_leases(r);assert(pool->capacity==1024&&pool->active==1);
    pin_full_base=pool->slots;RootCell *root=(void *)(uintptr_t)h->pin.root.follow.root.handle.cell;
    ReferenceLease source={.reference=atomic_load_explicit(&root->value,memory_order_acquire)};
    assert(!gc_reference_lease_locked(pool,&source));AllocationPage *page=(void *)(uintptr_t)source.lease.page;
    pin_full_holders[pin_full_count++]=source.lease;source.lease=(MetadataLeaseRequest){0};
    while(pool->free_head){
        MetadataLeaseRequest lease={.page=(uintptr_t)page,.incarnation=page->incarnation};
        assert(!gc_metadata_step_locked(pool,&lease,GC_ML_ACQUIRE));pin_full_holders[pin_full_count++]=lease;
    }
    assert(pin_full_count==1023&&pool->active==1024&&!h->pin.root.follow.cleanup.token);kg_unlock(r);
}
static void pin_metadata_full(void *runtime,void *handle){
    KgRuntime *r=runtime;KgPinnedBytes *h=handle;
    assert(!atomic_load_explicit(&kg_current_context(r)->access_epoch,memory_order_acquire)&&
        !atomic_load_explicit(&kg_current_context(r)->private_producers,memory_order_acquire)&&!h->pin.root.follow.cleanup.token);
    pin_full_seen++;
}
static void empty_shutdown(void){
    kg_shutdown();assert(!kg_runtime.native.backing.heap.metadata_committed&&!kg_runtime.native.backing.heap.metadata_pending&&
        !kg_runtime.pin_inventory&&!kg_runtime.lease_backing);
}
static RootCell *pinned_neighbour_graph(uint64_t bytes){
    RootCell *child=kg_root_new(),*record=kg_root_new(),*anchor=kg_root_new();
    kg_alloc_into(8,1,child);kg_store_i64(child,0,771);kg_native_enter();kg_native_exit();
    kg_alloc_into(bytes,1,anchor);if(bytes>=8)kg_store_i64(anchor,0,77);
    /* The dead record shares the retained pin page; its child occupies a
     * different source run. Skipping only the page without its graph closure
     * would retire that child then fail canonical heap verification. */
    kg_alloc_into(8,2,record);kg_store_ref(record,0,child);kg_root_release(record);kg_root_release(child);
    return anchor;
}
static void managed_callback(void){
    kg_native_exit();RootCell *temporary=kg_root_new();kg_alloc_into(8,1,temporary);kg_store_i64(temporary,0,881);
    kg_driver_pressure(&kg_runtime,KG_PRESSURE_HEAP);assert(kg_load_i64(temporary,0)==881);kg_root_release(temporary);kg_native_enter();
}
static void basic_pin(uint64_t bytes){
    RootCell *anchor=pinned_neighbour_graph(bytes);KgPinnedBytes *pin=kg_pin_bytes(anchor);kg_root_clear(anchor);
    kg_native_enter();void *address=kg_pinned_bytes_address(pin);assert(address&&kg_pinned_bytes_size(pin)==bytes);
    if(bytes>=8)assert(*(uint64_t *)address==77);
    managed_callback();assert(kg_pinned_bytes_address(pin)==address);kg_native_exit();
    for(unsigned i=0;i<8;i++){kg_driver_pressure(&kg_runtime,KG_PRESSURE_HEAP);kg_verify();}
    kg_native_enter();assert(kg_pinned_bytes_address(pin)==address);
    if(bytes>=8)assert(*(uint64_t *)address==77);kg_native_exit();
    kg_lock(&kg_runtime);assert(((KgDriver *)kg_runtime.driver)->pin_visits>0&&kg_runtime.native.cycles.completed[0]>=8&&
        kg_runtime.native.cycles.completed[1]>=8);kg_unlock(&kg_runtime);
    kg_unpin_bytes(pin);kg_root_release(anchor);empty_shutdown();
    puts("public byte pin: independent root, neighbouring graph closure, Young/Old stability and physical retirement PASS");
}
typedef struct {const RootCell *source;pthread_barrier_t *ready,*release;uintptr_t address;} PinThread;
static void *pin_owner(void *argument){
    PinThread *job=argument;kg_thread_attach();RootCell *owned=kg_root_new();kg_root_import(owned,job->source);
    KgPinnedBytes *pin=kg_pin_bytes(owned);kg_root_release(owned);kg_native_enter();
    job->address=(uintptr_t)kg_pinned_bytes_address(pin);assert(*(uint64_t *)job->address==77);
    pin_barrier(job->ready);pin_barrier(job->release);
    assert((uintptr_t)kg_pinned_bytes_address(pin)==job->address&&*(uint64_t *)job->address==77);
    kg_native_exit();kg_unpin_bytes(pin);kg_thread_detach();return NULL;
}
static void shared_pins(unsigned count){
    RootCell *anchor=pinned_neighbour_graph(8);kg_native_enter();
    pthread_barrier_t ready,release;assert(!pthread_barrier_init(&ready,NULL,count+1)&&!pthread_barrier_init(&release,NULL,count+1));
    PinThread jobs[8];pthread_t threads[8];
    for(unsigned i=0;i<count;i++){
        jobs[i]=(PinThread){anchor,&ready,&release,0};assert(!pthread_create(&threads[i],NULL,pin_owner,&jobs[i]));
    }
    pin_barrier(&ready);for(unsigned i=1;i<count;i++)assert(jobs[i].address==jobs[0].address);kg_native_exit();kg_root_clear(anchor);
    for(unsigned i=0;i<8;i++)kg_driver_pressure(&kg_runtime,KG_PRESSURE_HEAP);
    kg_verify();kg_native_enter();pin_barrier(&release);kg_native_exit();
    /* The final permit can race this real selection. SELECT either observes
     * a remaining pin and skips, or closes the page after all permits exit. */
    kg_driver_pressure(&kg_runtime,KG_PRESSURE_HEAP);
    kg_native_enter();for(unsigned i=0;i<count;i++)assert(!pthread_join(threads[i],NULL));kg_native_exit();
    assert(!pthread_barrier_destroy(&ready)&&!pthread_barrier_destroy(&release));
    kg_root_release(anchor);empty_shutdown();printf("public byte pin: %u actual owners, shared permits, concurrent selection/unpin and detach PASS\n",count);
}
static void late_admission(void){
    RootCell *anchor=pinned_neighbour_graph(8);kg_native_enter();KgRuntime *r=&kg_runtime;KgDriver *d=r->driver;
    while(!kgd_acquire(d))kg_yield();kg_lock(r);d->requested[GC_YOUNG]=1;kg_unlock(r);
    for(unsigned tries=0;d->state!=6;tries++){assert(tries<1000000);kgd_adopt(d);kgd_step(d);}
    uint64_t original=atomic_load_explicit(&anchor->value,memory_order_acquire)>>16;
    pthread_barrier_t ready,release;assert(!pthread_barrier_init(&ready,NULL,2)&&!pthread_barrier_init(&release,NULL,2));
    PinThread job={anchor,&ready,&release,0};pthread_t thread;assert(!pthread_create(&thread,NULL,pin_owner,&job));
    for(unsigned tries=0;;tries++){
        kg_lock(r);bool waiting=d->cleanup_waiters>0;kg_unlock(r);if(waiting)break;assert(tries<10000000);kg_yield();
    }
    kg_lock(r);assert(d->state==6&&!r->pin_inventory);kg_unlock(r);
    /* This admission cannot retroactively exclude an already decided source.
     * The real root remains discoverable through copy/heal/incoming audits. */
    for(unsigned tries=0;d->state||d->background_old;tries++){assert(tries<1000000);kgd_adopt(d);kgd_step(d);}
    kgd_leave(d);pin_barrier(&ready);
    assert(job.address!=r->native.backing.heap.heap_base+original);pin_barrier(&release);
    assert(!pthread_join(thread,NULL));assert(!pthread_barrier_destroy(&ready)&&!pthread_barrier_destroy(&release));
    kg_native_exit();kg_root_release(anchor);empty_shutdown();
    puts("public byte pin: actual post-MarkEnd admission waits for completed movement and borrows canonical destination PASS");
}
static void null_pin(void){
    RootCell *root=kg_root_new();KgPinnedBytes *pin=kg_pin_bytes(root);kg_native_enter();
    assert(!kg_pinned_bytes_address(pin)&&!kg_pinned_bytes_size(pin));kg_native_exit();kg_unpin_bytes(pin);
    kg_root_release(root);empty_shutdown();puts("public byte pin: null root, empty adoption and exact root cleanup PASS");
}
typedef struct {pthread_barrier_t ready,release;KgRootBlock *roots;} RetireThread;
static void *retiring_owner(void *argument){
    RetireThread *job=argument;kg_thread_attach();job->roots=kg_current_mutator(&kg_runtime)->root_blocks;
    kg_native_enter();pin_barrier(&job->ready);pin_barrier(&job->release);kg_native_exit();kg_thread_detach();return NULL;
}
static void detach_rendezvous(void){
    KgRuntime *r=&kg_runtime;RetireThread job={0};pthread_t thread;
    assert(!pthread_barrier_init(&job.ready,NULL,2)&&!pthread_barrier_init(&job.release,NULL,2));
    kg_native_enter();assert(!pthread_create(&thread,NULL,retiring_owner,&job));pin_barrier(&job.ready);
    RootStorage *storage=job.roots->roots;RootLease lease={0};
    RootLifeRequest reader={.registration=(uintptr_t)&storage->registration,.scope=storage->segment.scope,.lease=(uintptr_t)&lease};
    kg_lock(r);assert(!gc_root_life_locked(&r->native.roots,&reader,GC_RL_ACQUIRE));kg_unlock(r);pin_barrier(&job.release);
    for(unsigned tries=0;;tries++){
        kg_lock(r);bool retiring=atomic_load_explicit(&storage->segment.state,memory_order_acquire)==GC_ROOT_SEGMENT_RETIRING;kg_unlock(r);
        if(retiring)break;assert(tries<10000000);kg_yield();
    }
    KgDriver *d=r->driver;while(!kgd_acquire(d))kg_yield();kg_lock(r);d->requested[GC_YOUNG]=1;kg_unlock(r);
    /* The actual reader keeps CLEAN waiting while a new real rendezvous needs
     * the retiring OS owner's ACK. It must poll despite that live reader. */
    for(unsigned tries=0;d->state!=3;tries++){assert(tries<10000000);kgd_adopt(d);kgd_step(d);}
    kg_lock(r);assert(atomic_load_explicit(&storage->segment.state,memory_order_acquire)==GC_ROOT_SEGMENT_RETIRING);
    reader.serial=lease.serial;assert(!gc_root_life_locked(&r->native.roots,&reader,GC_RL_RELEASE));
    uint64_t target=r->native.cycles.started[GC_YOUNG];kg_unlock(r);kgd_leave(d);
    /* The retained-reader handshake proof is complete. Ordinary progress now
     * needs real quantum handoff: another owner can hold a journal claim and
     * require the same driver before it can ACK, CLEAN or detach. Never keep
     * busy exclusively until that other owner's whole cycle has completed. */
    for(unsigned tries=0;;tries++){
        kg_lock(r);bool completed=r->native.cycles.completed[GC_YOUNG]>=target;kg_unlock(r);if(completed)break;
        assert(tries<10000000);kg_driver_step(r);kg_yield();
    }
    assert(!pthread_join(thread,NULL));kg_native_exit();
    assert(!pthread_barrier_destroy(&job.ready)&&!pthread_barrier_destroy(&job.release));empty_shutdown();
    puts("actual retiring owner: retained reader plus new handshake receives owner ACK before CLEAN PASS");
}
static void selection_receipts(void){
    KgRuntime *r=&kg_runtime;RootCell *anchor=pinned_neighbour_graph(8);KgPinnedBytes *pin=kg_pin_bytes(anchor);
    for(unsigned i=0;i<3;i++)kg_driver_pressure(r,KG_PRESSURE_HEAP);
    kg_native_enter();KgDriver *d=r->driver;while(!kgd_acquire(d))kg_yield();
    kg_lock(r);d->requested[GC_YOUNG]=d->requested[GC_OLD]=1;kg_unlock(r);KgRun *source=NULL;
    for(unsigned tries=0;!source;tries++){
        assert(tries<10000000);kgd_adopt(d);kgd_step(d);
        if(d->state==12&&d->generation==GC_OLD){
            kg_lock(r);for(NativeAllocationRun *run=(void *)(uintptr_t)r->native.allocations;run;run=(void *)(uintptr_t)run->next){
                KgRun *candidate=(KgRun *)run;
                if(candidate->selected_serial==d->serial&&run->stage==8&&candidate->remembered&&
                   atomic_load_explicit(&gc_native_allocation_page(run)->state,memory_order_acquire)==GC_PAGE_EVACUATED){source=candidate;break;}
            }kg_unlock(r);
        }
    }
    KgPinInventory *inventory=kgp_inventory(r);uint64_t index=source->native.lease.head_index;
    RelocationGateRequest bad=inventory->selected[index];bad.epoch++;
    kgd_native(d,true);void *remembered=source->remembered;
    assert(kg_remembered_detach_with_pin(r,source,&d->actor,&inventory->registry,&bad)==GC_RS_INVALID);
    PinPageRegistry copied=inventory->registry;
    assert(kg_remembered_detach_with_pin(r,source,&d->actor,&copied,&inventory->selected[index])==GC_RS_INVALID);
    kg_lock(r);MetadataLeaseRequest extra={.page=(uintptr_t)gc_native_allocation_page(&source->native),.incarnation=source->native.lease.incarnation};
    assert(!gc_metadata_step_locked(kg_leases(r),&extra,GC_ML_ACQUIRE));kg_unlock(r);
    assert(kg_remembered_detach_with_pin(r,source,&d->actor,&inventory->registry,&inventory->selected[index])==GC_RS_WAIT);
    assert(source->remembered==remembered&&source->remembered_charge.stage==2);
    kg_lock(r);assert(!gc_metadata_step_locked(kg_leases(r),&extra,GC_ML_RELEASE));kg_unlock(r);kgd_native(d,false);
    for(unsigned tries=0;d->state||d->background_old;tries++){assert(tries<10000000);kgd_adopt(d);kgd_step(d);}
    kgd_leave(d);kg_native_exit();kg_unpin_bytes(pin);kg_root_release(anchor);empty_shutdown();
    puts("public byte pin: actual source epoch, inventory identity and other-holder exclusion preserve remembered backing PASS");
}
static void pin_full(void){
    RootCell *root=pinned_neighbour_graph(8);fill_pin_enter=1;KgPinnedBytes *pin=kg_pin_bytes(root);
    kg_lock(&kg_runtime);MetadataLeasePool *pool=kg_leases(&kg_runtime);
    assert(pin_full_seen==1&&pool->slots==pin_full_base&&pool->capacity==1280);
    for(uint64_t i=0;i<pin_full_count;i++){
        MetadataLeaseRequest resolved={.token=pin_full_holders[i].token};assert(!gc_metadata_step_locked(pool,&resolved,GC_ML_RESOLVE));
        assert(resolved.page==pin_full_holders[i].page&&resolved.incarnation==pin_full_holders[i].incarnation);
        assert(!gc_metadata_step_locked(pool,&pin_full_holders[i],GC_ML_RELEASE));
    }
    kg_unlock(&kg_runtime);kg_native_enter();assert(*(uint64_t *)kg_pinned_bytes_address(pin)==77);kg_native_exit();
    kg_unpin_bytes(pin);kg_root_release(root);empty_shutdown();
    puts("public byte pin: actual RootPin ENTER FULL cleans before Native growth and preserves all old tokens PASS");
}
static void rejected_pin(const char *mode){
    if(!strcmp(mode,"invalid-exit")){PendingMark invalid={0};kgd_exit(&invalid);abort();}
    RootCell *root=kg_root_new();kg_alloc_into(8,!strcmp(mode,"record")?2:1,root);
    KgPinnedBytes *pin=kg_pin_bytes(root);
    if(!strcmp(mode,"borrow-running"))(void)kg_pinned_bytes_address(pin);
    else if(!strcmp(mode,"shutdown-live")){kg_root_release(root);kg_shutdown();}
    else if(!strcmp(mode,"released")){kg_unpin_bytes(pin);kg_native_enter();(void)kg_pinned_bytes_address(pin);}
    abort();
}
static NativeMetadataOps pin_real_metadata;
static unsigned pin_successful_maps,pin_map_failures[2],pin_unmap_failures[2];
static uintptr_t pin_unmap_targets[2];
static void *pin_map_retry(size_t bytes){
    unsigned index=pin_successful_maps;
    if(index<2&&pin_map_failures[index]){pin_map_failures[index]--;return NULL;}
    void *address=pin_real_metadata.map(bytes);if(address)pin_successful_maps++;return address;
}
static bool pin_unmap_retry(void *address,size_t bytes){
    for(unsigned i=0;i<2;i++)if((uintptr_t)address==pin_unmap_targets[i]&&pin_unmap_failures[i]){
        pin_unmap_failures[i]--;return false;
    }
    return pin_real_metadata.unmap(address,bytes);
}
static void pin_faults(void){
    KgRuntime *r=&kg_runtime;RootCell *root=pinned_neighbour_graph(8);
    pin_real_metadata=r->os.metadata;pin_map_failures[0]=pin_map_failures[1]=1;
    r->os.metadata.map=pin_map_retry;r->os.metadata.unmap=pin_unmap_retry;
    KgPinnedBytes *pin=kg_pin_bytes(root);assert(!pin_map_failures[0]&&!pin_map_failures[1]);
    pin_unmap_targets[0]=pin_control(pin)->control.allocation;pin_unmap_targets[1]=kgp_inventory(r)->control.allocation;
    pin_unmap_failures[0]=pin_unmap_failures[1]=1;
    kg_native_enter();assert(*(uint64_t *)kg_pinned_bytes_address(pin)==77);kg_native_exit();
    for(unsigned i=0;i<4;i++)kg_driver_pressure(r,KG_PRESSURE_HEAP);
    kg_unpin_bytes(pin);assert(!pin_unmap_failures[0]);kg_root_release(root);empty_shutdown();
    assert(!pin_unmap_failures[1]);puts("public byte pin: control and inventory real OS map/unmap failures retain ownership through retry PASS");
}
static uintptr_t pin_reuse_address;
static size_t pin_reuse_bytes;
static unsigned pin_reuse_mapping;
static void *pin_reuse_map(size_t bytes){
    if(!pin_reuse_mapping&&bytes==pin_reuse_bytes){
        void *address=mmap((void *)pin_reuse_address,bytes,PROT_READ|PROT_WRITE,MAP_PRIVATE|MAP_ANONYMOUS|MAP_FIXED_NOREPLACE,-1,0);
        assert(address!=MAP_FAILED&&(uintptr_t)address==pin_reuse_address);pin_reuse_mapping=1;return address;
    }
    return pin_real_metadata.map(bytes);
}
static void pin_identity_reuse(bool reject_old){
    RootCell *root=pinned_neighbour_graph(8);KgPinnedBytes *old=kg_pin_bytes(root);KgPinnedBytes *control=pin_control(old);
    pin_reuse_address=control->control.allocation;pin_reuse_bytes=(size_t)control->control.bytes;kg_unpin_bytes(old);
    /* The first control was REALLY unmapped and its budget returned. This is a
     * fresh real mmap at that free address, never a cached fake-success unmap. */
    pin_real_metadata=kg_runtime.os.metadata;kg_runtime.os.metadata.map=pin_reuse_map;
    KgPinnedBytes *fresh=kg_pin_bytes(root);kg_runtime.os.metadata.map=pin_real_metadata.map;
    assert(pin_reuse_mapping&&pin_control(fresh)->control.allocation==pin_reuse_address&&old!=fresh);
    kg_native_enter();assert(*(uint64_t *)kg_pinned_bytes_address(fresh)==77&&kg_pinned_bytes_size(fresh)==8);
    if(reject_old){fputs("actual control address reused with a distinct public serial capability\n",stderr);(void)kg_pinned_bytes_address(old);abort();}
    kg_native_exit();kg_unpin_bytes(fresh);kg_root_release(root);empty_shutdown();
    puts("public byte pin: actual unmapped control address reused with distinct serial and exact new-owner cleanup PASS");
}
int main(int argc,char **argv){
    kg_configure(8*1024*1024,0,4*1024*1024,4);kg_init();
    const char *mode=argc>1?argv[1]:"basic";
    if(!strcmp(mode,"2")||!strcmp(mode,"8"))shared_pins((unsigned)strtoul(mode,NULL,10));
    else if(!strcmp(mode,"late"))late_admission();
    else if(!strcmp(mode,"null"))null_pin();
    else if(!strcmp(mode,"detach-rendezvous"))detach_rendezvous();
    else if(!strcmp(mode,"receipts"))selection_receipts();
    else if(!strcmp(mode,"full"))pin_full();
    else if(!strcmp(mode,"faults"))pin_faults();
    else if(!strcmp(mode,"identity")||!strcmp(mode,"reuse"))pin_identity_reuse(!strcmp(mode,"reuse"));
    else if(!strcmp(mode,"record")||!strcmp(mode,"borrow-running")||!strcmp(mode,"shutdown-live")||!strcmp(mode,"released")||!strcmp(mode,"invalid-exit"))rejected_pin(mode);
    else if(!strcmp(mode,"zero"))basic_pin(0);
    else if(!strcmp(mode,"large"))basic_pin(2*GC_HEAP_PAGE_BYTES+32);
    else basic_pin(8);
    return 0;
}
