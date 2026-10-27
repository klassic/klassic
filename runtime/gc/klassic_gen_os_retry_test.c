/* Exercise the real facade and OS collector with failing native OS operations.
 * Inclusion keeps private receipt inspection in this test, without adding a
 * production test ABI, replacing the driver, or fabricating collector proofs. */
#include "klassic_gen_runtime.c"
#include "gc_native_driver.c"
#include "klassic_gen_remembered.c"
#include "klassic_gen_policy.c"
#include "klassic_gen_verify.c"
#include <assert.h>

enum { PREP_PAYLOAD,PREP_BITMAP,PREP_FORWARD,PREP_RECORD,
       CANCEL_PAYLOAD,CANCEL_BITMAP,CANCEL_FORWARD,CANCEL_RECORD,CANCEL_CONTROL,
       SOURCE_PAYLOAD,SOURCE_BITMAP,SOURCE_FORWARD,LOGICAL_RECORD,RUN_CONTROL,
       ROOT_MAP,CONTROL_MAP,ROOT_STORAGE,ROOT_CONTROL,DRIVER_CONTROL,BOOT_ROOT,BOOT_DESC,BOOT_WORK,
       HEAP_TABLE,HEAP_RESERVATION,PARTIAL_RECORD_MAP,CANCEL_UNCOMMITTED,FAULT_COUNT };
static _Atomic unsigned failures[FAULT_COUNT];
static _Atomic unsigned armed[FAULT_COUNT];
static NativeRunCommitter actual_commit;
static NativePageDecommitter actual_decommit;
static KgRun *probe;
static bool partial_preparing,fail_after_effect,cancel_before_commit;
static uint64_t driver_address,bootstrap[4];
static uint64_t root_storages[16],root_controls[16];
static unsigned root_count;
static struct {void *address;size_t bytes;bool live;} mappings[8192];
static pthread_mutex_t mapping_lock=PTHREAD_MUTEX_INITIALIZER;
static unsigned maps_live;
static uintptr_t retry_address[FAULT_COUNT],permanent_address;
static unsigned permanent_attempts;
static uint64_t permanent_budget;
static uint64_t probe_bytes[3],probe_maps[3],root_storage_bytes,run_control_bytes;

static bool fail_once(unsigned kind){
    unsigned expected=1;
    if(!atomic_compare_exchange_strong(&armed[kind],&expected,0))return false;
    atomic_fetch_add(&failures[kind],1);return true;
}
static void require_native(void){
    const MutatorContext *actor=kg_owned_driver?&kg_owned_driver->actor:&kg_runtime.native.context;
    assert(!atomic_load(&actor->access_epoch));
    assert(atomic_load(&actor->state)==GC_BLOCKED_NATIVE||
           (kg_runtime.closing&&!kg_runtime.native.participant.registered&&atomic_load(&actor->state)==GC_DETACHED));
}
static void *failing_map(size_t bytes){
    require_native();
    if(probe&&probe->native.stage==1){
        if(partial_preparing&&bytes==probe->native.mark_records.bytes&&fail_once(PARTIAL_RECORD_MAP))return NULL;
        for(unsigned i=0;i<3;i++)if(bytes==probe_bytes[i]){
            unsigned kind=PREP_BITMAP+i;
            /* A later OS failure must not remap an earlier success. */
            if(i>0)assert(probe->native.bitmap.mapping);
            if(i>1)assert(probe->native.forwarding.allocation);
            if(fail_once(kind))return NULL;
            probe_maps[i]++;
        }
    }else{
        if(bytes==root_storage_bytes&&fail_once(ROOT_MAP))return NULL;
        if(bytes==run_control_bytes&&fail_once(CONTROL_MAP))return NULL;
    }
    void *p=gc_native_metadata_map(bytes);assert(p);
    assert(!pthread_mutex_lock(&mapping_lock));
    unsigned i=0;while(i<8192&&mappings[i].live)i++;assert(i<8192);
    mappings[i].address=p;mappings[i].bytes=bytes;mappings[i].live=true;maps_live++;
    assert(!pthread_mutex_unlock(&mapping_lock));return p;
}
static bool failure_for_mapping(void *p,unsigned *kind){
    uintptr_t a=(uintptr_t)p;KgDriver *d=kg_owned_driver;
    if(probe){
        if(a==probe->native.bitmap.mapping){*kind=CANCEL_BITMAP;return true;}
        if(a==probe->native.forwarding.allocation){*kind=CANCEL_FORWARD;return true;}
        if(a==probe->native.mark_records.allocation){*kind=CANCEL_RECORD;return true;}
        if(a==(uintptr_t)probe){*kind=CANCEL_CONTROL;return true;}
    }
    if(d){
        if(d->state==12&&d->cursor&&d->cursor->native.stage==9){
            NativeAllocationRun *run=&d->cursor->native;
            if(a==run->bitmap.mapping){assert(run->retirement.stage==1);*kind=SOURCE_BITMAP;return true;}
            if(a==run->forwarding.allocation){assert(run->bitmap.stage==4);*kind=SOURCE_FORWARD;return true;}
        }
        if(d->state==13&&d->cursor&&d->cursor->native.stage==11&&a==d->cursor->native.mark_records.allocation){
            assert(!d->cursor->native.import_users);*kind=LOGICAL_RECORD;return true;
        }
        if(d->state==13&&a==d->control_retire.charge.allocation){
            assert(d->control_retire.charge.stage==2);*kind=RUN_CONTROL;return true;
        }
    }
    if(a==driver_address){assert(!kg_runtime.driver);*kind=DRIVER_CONTROL;return true;}
    for(unsigned i=0;i<root_count;i++){
        if(a==root_storages[i]){*kind=ROOT_STORAGE;return true;}
        if(a==root_controls[i]){*kind=ROOT_CONTROL;return true;}
    }
    if(kg_runtime.native.stage==4){
        for(unsigned i=0;i<4;i++)if(a==bootstrap[i]){*kind=BOOT_ROOT+i;return true;}
    }
    return false;
}
static bool failing_unmap(void *p,size_t bytes){
    require_native();unsigned kind;
    if((uintptr_t)p==permanent_address){
        assert(kg_runtime.native.backing.heap.metadata_committed==permanent_budget&&maps_live==1);
        if(++permanent_attempts==32)fputs("permanent OS failure: 32 attempts, original mapping and budget remain owned\n",stderr);
        return false;
    }
    bool identified=failure_for_mapping(p,&kind);
    if(identified&&retry_address[kind]){assert(retry_address[kind]==(uintptr_t)p);retry_address[kind]=0;}
    if(identified&&fail_once(kind)){
        retry_address[kind]=(uintptr_t)p;
        /* Failure leaves both the actual usable mapping and its original
         * charge owned. No success is reported to any controller. */
        assert(bytes);((volatile unsigned char *)p)[0]=((volatile unsigned char *)p)[0];
        return false;
    }
    assert(!pthread_mutex_lock(&mapping_lock));
    for(unsigned i=0;i<8192;i++)if(mappings[i].live&&mappings[i].address==p){
        assert(mappings[i].bytes==bytes);mappings[i].live=false;assert(maps_live);maps_live--;break;
    }
    assert(!pthread_mutex_unlock(&mapping_lock));return gc_native_metadata_unmap(p,bytes);
}
static uint64_t failing_commit(NativeHeapOwner *owner,PageLease *lease){
    require_native();
    if(probe&&lease==&probe->native.lease&&cancel_before_commit&&fail_once(CANCEL_UNCOMMITTED))return GC_NATIVE_METADATA_OS_ERROR;
    if(probe&&lease==&probe->native.lease&&fail_once(PREP_PAYLOAD)){
        assert(!lease->committed_mapping);
        if(fail_after_effect)assert(!(actual_commit?actual_commit:gc_native_heap_commit_lease)(owner,lease));
        return GC_NATIVE_METADATA_OS_ERROR;
    }
    return (actual_commit?actual_commit:gc_native_heap_commit_lease)(owner,lease);
}
static bool failing_decommit(void *address,size_t bytes){
    require_native();unsigned kind=SOURCE_PAYLOAD;
    if(probe){kind=CANCEL_PAYLOAD;assert(probe->native.stage==5);}
    else if(kg_owned_driver){
        KgDriver *d=kg_owned_driver;
        assert(d->state==12&&d->uncommit.stage==1&&d->uncommit.committed_mapping==1);
        AllocationPage *pages=(void *)(uintptr_t)kg_runtime.native.backing.heap.pages;
        assert(pages[d->uncommit.head_index].owner==(uintptr_t)&d->uncommit);
    }
    if(fail_once(kind)){
        /* A real OS partial effect is allowed: failed discard after protection
         * must preserve its receipt/charge and retry the full exact range. */
        if(fail_after_effect)assert((actual_decommit?actual_decommit:gc_native_heap_decommit_range)(address,bytes));
        else assert(!mprotect(address,bytes,PROT_NONE));
        return false;
    }
    return (actual_decommit?actual_decommit:gc_native_heap_decommit_range)(address,bytes);
}
static bool failing_release(void *address,size_t bytes){
    require_native();
    if(fail_once(HEAP_RESERVATION)){assert(kg_runtime.native.backing.stage==4);return false;}
    return gc_native_metadata_unmap(address,bytes);
}
static void preparation_and_cancel(void){
    KgRuntime *r=&kg_runtime;MetadataCharge receipt={0};
    uint64_t baseline=r->native.backing.heap.metadata_committed;
    probe=kg_control_map(r,sizeof(KgRun),&receipt);probe->control=receipt;
    kg_enter_native(r);uint64_t borrowed=kg_driver_cleanup_enter(r,NULL);
    kg_lock(r);uint64_t s=gc_native_allocation_reserve_generation_locked(&probe->native,&r->native,1,512,GC_YOUNG,&r->os.metadata);kg_unlock(r);assert(!s);
    probe_bytes[0]=probe->native.bitmap.bytes;probe_bytes[1]=probe->native.forwarding.bytes;probe_bytes[2]=probe->native.mark_records.bytes;
    assert(probe_bytes[0]!=probe_bytes[1]&&probe_bytes[1]!=probe_bytes[2]&&probe_bytes[0]!=probe_bytes[2]);
    for(unsigned i=PREP_PAYLOAD;i<=CANCEL_CONTROL;i++)atomic_store(&armed[i],1);
    kg_prepare_run_native_retry(r,probe,&r->native.context);
    for(unsigned i=0;i<3;i++)assert(probe_maps[i]==1);
    kg_lock(r);s=gc_native_allocation_publish_locked(&probe->native);kg_unlock(r);assert(!s);
    /* The READY never-bound path exercises actual PageRelease BEGIN and all
     * native cleanup stages, then the independent control receipt. */
    kg_cancel_run_native_retry(r,probe,&r->native.context);probe=NULL;
    assert(!r->native.allocations&&!r->native.backing.heap.committed&&!r->native.backing.heap.pending);
    assert(r->native.backing.heap.metadata_committed==baseline&&!r->native.backing.heap.metadata_pending);
    /* Cancel a second run while preparation is only partially successful.
     * This is the distinct unpublished-lease path (no PageRelease receipt). */
    MetadataCharge second={0};kg_driver_cleanup_leave(r,borrowed);kg_leave_native(r);
    probe=kg_control_map(r,sizeof(KgRun),&second);probe->control=second;
    kg_enter_native(r);borrowed=kg_driver_cleanup_enter(r,NULL);
    kg_lock(r);s=gc_native_allocation_reserve_generation_locked(&probe->native,&r->native,1,512,GC_YOUNG,&r->os.metadata);kg_unlock(r);assert(!s);
    partial_preparing=true;atomic_store(&armed[PARTIAL_RECORD_MAP],1);
    kg_policy_before_prepare(r,probe,&r->native.context);
    s=gc_native_allocation_prepare_native_with(&probe->native,&r->os.metadata,&r->native.context,r->payload_commit);
    kg_policy_after_prepare(r,probe);
    assert(s==GC_NATIVE_RUN_OS_ERROR&&probe->native.stage==1&&probe->native.lease.committed_mapping&&
        probe->native.bitmap.mapping&&probe->native.forwarding.allocation&&!probe->native.mark_records.allocation);
    partial_preparing=false;kg_cancel_run_native_retry(r,probe,&r->native.context);probe=NULL;
    assert(!r->native.allocations&&!r->native.backing.heap.committed&&!r->native.backing.heap.pending&&
        r->native.backing.heap.metadata_committed==baseline&&!r->native.backing.heap.metadata_pending);
    kg_driver_cleanup_leave(r,borrowed);kg_leave_native(r);
    MetadataCharge third={0};probe=kg_control_map(r,sizeof(KgRun),&third);probe->control=third;
    kg_enter_native(r);borrowed=kg_driver_cleanup_enter(r,NULL);
    kg_lock(r);s=gc_native_allocation_reserve_generation_locked(&probe->native,&r->native,1,512,GC_YOUNG,&r->os.metadata);kg_unlock(r);assert(!s);
    cancel_before_commit=true;atomic_store(&armed[CANCEL_UNCOMMITTED],1);
    kg_policy_before_prepare(r,probe,&r->native.context);
    s=gc_native_allocation_prepare_native_with(&probe->native,&r->os.metadata,&r->native.context,r->payload_commit);
    kg_policy_after_prepare(r,probe);
    assert(s==GC_NATIVE_RUN_OS_ERROR&&!probe->native.lease.committed_mapping&&!probe->native.bitmap.mapping);
    uint64_t cached_index=probe->native.lease.head_index;cancel_before_commit=false;
    kg_cancel_run_native_retry(r,probe,&r->native.context);probe=NULL;
    KgPolicy *policy=kg_policy(r);
    assert(!(policy->states[cached_index]&KG_BACKED)||(policy->states[cached_index]&KG_CACHED));
    assert(!r->native.allocations&&!r->native.backing.heap.committed&&!r->native.backing.heap.pending&&
        r->native.backing.heap.metadata_committed==baseline&&!r->native.backing.heap.metadata_pending);
    kg_driver_cleanup_leave(r,borrowed);kg_leave_native(r);
    for(unsigned i=PREP_PAYLOAD;i<=CANCEL_CONTROL;i++)assert(atomic_load(&failures[i])==1);
}
int main(int argc,char **argv){
    fail_after_effect=argc>1&&strstr(argv[1],"after-effect");
    uint64_t minimum=argc>1&&strstr(argv[1],"minimum")?UINT64_C(8)*1024*1024:0;
    kg_configure(UINT64_C(8)*1024*1024,minimum,UINT64_C(4)*1024*1024,5);kg_init();
    assert(kg_committed_bytes()==minimum);
    KgRuntime *r=&kg_runtime;kg_enter_native(r);uint64_t borrowed=kg_driver_cleanup_enter(r,NULL);
    driver_address=(uintptr_t)r->driver;
    assert(!gc_native_roots_size(r->root_capacity,r->journal_capacity,r->os.metadata.quantum,&root_storage_bytes));
    assert(!gc_native_metadata_size(sizeof(KgRun),r->os.metadata.quantum,&run_control_bytes));
    bootstrap[0]=r->native.root_charge.allocation;bootstrap[1]=r->native.descriptor_charge.allocation;
    bootstrap[2]=r->native.work_charge.allocation;bootstrap[3]=r->native.backing.table;
    r->os.metadata.map=failing_map;r->os.metadata.unmap=failing_unmap;r->os.release=failing_release;
    actual_commit=r->payload_commit;actual_decommit=r->payload_decommit;
    r->payload_commit=failing_commit;r->payload_decommit=failing_decommit;
    kg_driver_cleanup_leave(r,borrowed);kg_leave_native(r);
    if(argc>1&&!strcmp(argv[1],"permanent")){
        MetadataCharge charge={0};permanent_address=(uintptr_t)kg_control_map(r,4096,&charge);
        permanent_budget=r->native.backing.heap.metadata_committed;
        kg_control_unmap(r,&charge);assert(!"permanent unmap must fail-stop");
    }
    preparation_and_cancel();
    /* Real roots/stores/copies cause actual Young/Old source, record and
     * control retirement; no phases, liveness bits or proof receipts are set. */
    for(unsigned i=SOURCE_PAYLOAD;i<PARTIAL_RECORD_MAP;i++)atomic_store(&armed[i],1);
    RootCell *number=kg_root_new(),*alias=kg_root_new(),*scratch=kg_root_new();
    kg_alloc_into(8,1,number);kg_store_i64(number,0,UINT64_C(0x123456789abcdef0));kg_root_copy(alias,number);
    for(unsigned i=0;i<24;i++){kg_alloc_into(128,1,scratch);kg_store_i64(scratch,0,i);assert(kg_load_i64(alias,0)==UINT64_C(0x123456789abcdef0));assert(kg_committed_bytes()>=minimum);}
    RootCell *roots[300];for(unsigned i=0;i<300;i++)roots[i]=kg_root_new();
    for(unsigned i=0;i<300;i++)kg_root_release(roots[i]);
    kg_root_release(scratch);kg_root_release(alias);kg_root_release(number);
    kg_enter_native(r);borrowed=kg_driver_cleanup_enter(r,NULL);
    kg_lock(r);assert(kg_policy_verifications_locked(r)==r->native.cycles.completed[0]+r->native.cycles.completed[1]);
    assert(r->native.cycles.completed[0]&&r->native.cycles.completed[1]);kg_unlock(r);
    for(KgRootBlock *b=r->root_blocks;b;b=b->next)if(!b->initial){
        assert(root_count<16);root_storages[root_count]=b->storage.allocation;root_controls[root_count++]=b->control.allocation;
    }
    kg_driver_cleanup_leave(r,borrowed);kg_leave_native(r);kg_shutdown();
    for(unsigned i=0;i<FAULT_COUNT;i++){
        if(atomic_load(&failures[i])!=1)fprintf(stderr,"unexercised fault %u: %u\n",i,atomic_load(&failures[i]));
        assert(atomic_load(&failures[i])==1&&!retry_address[i]);
    }
    assert(!maps_live&&r->native.stage==5&&!r->native.backing.heap.metadata_committed&&!r->native.backing.heap.metadata_pending);
    puts("canonical C native OS failure retry: 26 failures, exact receipts, prefix preservation and complete shutdown PASS");return 0;
}
