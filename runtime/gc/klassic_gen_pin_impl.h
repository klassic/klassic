/* Implementation include, once in runtime.c. A pin uses the shared RootPin
 * controller and a real heap-wide page registry. New admission waits for the
 * actual idle phase owner, so it cannot turn a post-MarkEnd page into a newly
 * excluded source. Existing pinned pages are conservatively traced by the
 * driver's bounded physical-page walk before MarkEnd. Selection and source
 * release consult this same registry and its authenticated selection epochs. */
#ifndef KLASSIC_GEN_PIN_IMPL_H
#define KLASSIC_GEN_PIN_IMPL_H
#include "klassic_gen_remembered.h"
struct KgPinnedBytes {
    MetadataCharge control;
    uint64_t self,identity,payload,descriptor;
    MutatorContext *owner;
    RootPin pin;
    struct KgPinnedBytes *previous,*next;
};
typedef struct {
    MetadataCharge control;
    KgRuntime *runtime;
    PinPageRegistry registry;
    RelocationGateRequest *selected;
    KgPinnedBytes *head;
    uint64_t serial;
} KgPinInventory;
static KgPinInventory *kgp_inventory(KgRuntime *r){
    KgPinInventory *p=r->pin_inventory;
    if(p&&(p->runtime!=r||r->pin_registry!=&p->registry||p->registry.pool!=kg_leases(r)||p->registry.capacity!=r->native.backing.heap.page_count))
        kg_fail("pin inventory lifetime",0);
    return p;
}
static void kgp_initialize_native(KgRuntime *r){
    if(r->pin_inventory)return;
    uint64_t count=r->native.backing.heap.page_count;
    if(count>(UINT64_MAX-sizeof(KgPinInventory))/(sizeof(PinPageRecord)+sizeof(RelocationGateRequest)))kg_fail("pin inventory size",0);
    MetadataCharge control={0};uint64_t status=kgl_control(r,sizeof(KgPinInventory)+count*(sizeof(PinPageRecord)+sizeof(RelocationGateRequest)),&control);
    if(status)kg_fail("pin inventory budget",status);
    KgPinInventory *p=(void *)(uintptr_t)control.allocation;p->control=control;p->runtime=r;
    p->registry=(PinPageRegistry){kg_leases(r),(void *)(p+1),count};
    p->selected=(void *)(p->registry.records+count);
    /* Caller owns the actual idle driver, excluding a second lazy initializer
     * and every selection/inventory reader across this Native OS preparation. */
    kg_lock(r);if(r->pin_inventory)kg_fail("duplicate pin inventory",0);r->pin_inventory=p;r->pin_registry=&p->registry;kg_unlock(r);
}
static PinPageRecord *kgp_record(KgPinInventory *p,KgRun *run){
    return &p->registry.records[run->native.lease.head_index];
}
static uint64_t kgp_bind_locked(KgRuntime *r,KgPinInventory *p,AllocationPage *page){
    uint64_t index=((uintptr_t)page-r->native.backing.heap.pages)/sizeof(AllocationPage);
    if(index>=p->registry.capacity||page->run_head!=index)return KG_PIN_INVALID;
    PinPageRecord *record=&p->registry.records[index];
    if(record->lifetime.token){
        MetadataLeaseRequest checked={.token=record->lifetime.token};
        if(gc_metadata_step_locked(kg_leases(r),&checked,GC_ML_RESOLVE)||checked.page!=(uintptr_t)page||checked.incarnation!=page->incarnation)
            return KG_PIN_INVALID;
        return KG_PIN_SELECTED;
    }
    MetadataLeaseRequest borrowed={.page=(uintptr_t)page,.incarnation=page->incarnation};
    uint64_t status=gc_metadata_step_locked(kg_leases(r),&borrowed,GC_ML_ACQUIRE);
    if(status==GC_ML_FULL)return KG_PIN_FULL;if(status)return KG_PIN_INVALID;
    status=gc_pin_registry_bind_locked(&p->registry,&borrowed);
    uint64_t release=gc_metadata_step_locked(kg_leases(r),&borrowed,GC_ML_RELEASE);
    if(release)return KG_PIN_INVALID;
    return status==GC_RG_OK?KG_PIN_SELECTED:status==GC_PR_FULL?KG_PIN_FULL:KG_PIN_INVALID;
}
uint64_t kg_pin_select_locked(KgRuntime *r,KgRun *run){
    KgPinInventory *p=kgp_inventory(r);if(!p)return KG_PIN_SELECTED;
    AllocationPage *page=gc_native_allocation_page(&run->native);
    uint64_t status=kgp_bind_locked(r,p,page);if(status)return status;
    RelocationGateRequest *selection=&p->selected[run->native.lease.head_index];
    if(selection->epoch||run->pin_retired)return KG_PIN_INVALID;
    status=gc_pin_registry_step_locked(&p->registry,&kgp_record(p,run)->lifetime,selection,GC_PP_SELECT);
    return status==GC_RG_OK?KG_PIN_SELECTED:status==GC_RG_WAIT?KG_PIN_SKIP:KG_PIN_INVALID;
}
bool kg_pin_page_active_locked(KgRuntime *r,KgRun *run){
    KgPinInventory *p=kgp_inventory(r);if(!p)return false;
    PinPageRecord *record=kgp_record(p,run);
    return record->lifetime.token&&record->lifetime.incarnation==run->native.lease.incarnation&&record->admission.active;
}
bool kg_pin_source_selected_locked(KgRuntime *r,KgRun *run){
    KgPinInventory *p=kgp_inventory(r);if(!p)return true;
    if(run->pin_retired)return atomic_load_explicit(&gc_native_allocation_page(&run->native)->state,memory_order_acquire)==GC_PAGE_UNCOMMITTED;
    return !run->pin_retired&&gc_pin_selection_locked(&p->registry,&kgp_record(p,run)->lifetime,
        &p->selected[run->native.lease.head_index])==GC_RG_OK;
}
uint64_t kg_pin_source_seal_locked(KgRuntime *r,KgRun *run,uint64_t budget){
    KgPinInventory *p=kgp_inventory(r);
    if(!p)return gc_native_selected_source_seal_locked(&run->selection,budget);
    NativeSelectedSource *q=&run->selection;
    if(!kg_pin_source_selected_locked(r,run)||!gc_native_selected_source_valid(q)||q->active||q->cursor!=q->used||
       q->live_count+q->dead_count!=q->used||!budget||budget>256)return GC_SD_INVALID;
    ForwardTable *table=(void *)(uintptr_t)q->table;ForwardCell *cells=(void *)(uintptr_t)table->cells;
    uint64_t end=q->used-q->seal_cursor>budget?q->seal_cursor+budget:q->used;
    for(;q->seal_cursor<end;q->seal_cursor++){
        ForwardCell *cell=&cells[q->seal_cursor];uint64_t word=atomic_load_explicit(&cell->word,memory_order_acquire);
        if(cell->incarnation!=q->incarnation||atomic_load_explicit(&cell->access,memory_order_acquire)!=GC_SOURCE_CLOSED||
           (word!=3&&((word&3)!=2||!gc_forward_destination(word&~UINT64_C(3),cell->source_offset))))return GC_SD_WAIT;
    }
    if(q->seal_cursor<q->used)return GC_SD_MORE;
    AllocationPage *page=gc_native_allocation_page(&run->native);
    /* Exactly the authenticated registry lifetime remains. It keeps metadata,
     * never the source payload, alive through the later OS/incoming proofs. */
    if(page->metadata_leases!=1)return GC_SD_WAIT;
    atomic_store_explicit(&page->state,GC_PAGE_EVACUATED,memory_order_release);return GC_SD_DONE;
}
bool kg_pin_source_retire_locked(KgRuntime *r,KgRun *run){
    KgPinInventory *p=kgp_inventory(r);if(!p||run->pin_retired)return true;
    if(!kg_pin_source_selected_locked(r,run))kg_fail("pin source retirement selection",0);
    AllocationPage *page=gc_native_allocation_page(&run->native);if(page->metadata_leases!=1)return false;
    PinPageRecord *record=kgp_record(p,run);MetadataLeaseRequest borrowed=record->lifetime;
    uint64_t status=gc_pin_registry_step_locked(&p->registry,&borrowed,&p->selected[run->native.lease.head_index],GC_PP_REOPEN);
    if(status)kg_fail("pin source selection finish",status);
    status=gc_pin_registry_unbind_locked(&p->registry,&borrowed);if(status)kg_fail("pin source registry release",status);
    /* Final same-lock incoming consume follows immediately. New public pins
     * require idle phase ownership; they cannot enter this Retire interval. */
    run->pin_retired=1;return true;
}
uint64_t kg_pin_remembered_detach(KgRuntime *r,KgRun *run,const MutatorContext *actor){
    KgPinInventory *p=kgp_inventory(r);
    return p?kg_remembered_detach_with_pin(r,run,actor,&p->registry,&p->selected[run->native.lease.head_index]):
        kg_remembered_detach(r,run,actor);
}
void kg_pin_owner_can_detach_locked(KgRuntime *r,const MutatorContext *owner){
    KgPinInventory *p=kgp_inventory(r);if(!p)return;
    for(KgPinnedBytes *h=p->head;h;h=h->next)if(h->owner==owner)kg_fail("live byte pins at owner detach",h->identity);
}
void kg_pin_dispose_native(KgRuntime *r,const MutatorContext *actor){
    KgPinInventory *p=kgp_inventory(r);if(!p)return;
    if(!kgl_native(actor)||r->driver||r->native.allocations||p->head)kg_fail("live pin inventory disposal",0);
    for(uint64_t i=0;i<p->registry.capacity;i++)if(p->registry.records[i].lifetime.token||p->selected[i].epoch)
        kg_fail("pin page lifetime survived retirement",i);
    r->pin_inventory=NULL;r->pin_registry=NULL;kgl_retire_control(r,&p->control,actor);
}
static KgPinnedBytes *kgp_validate_locked(KgRuntime *r,const KgPinnedBytes *wanted){
    KgPinInventory *p=kgp_inventory(r);
    for(KgPinnedBytes *h=p?p->head:NULL;h;h=h->next)if(h->identity==(uintptr_t)wanted){
        if(h->self!=(uintptr_t)h||h->owner!=kg_current_context(r))kg_fail("byte pin owner",0);return h;
    }
    kg_fail("unknown or released byte pin",0);
}
static void kgp_grow_owned(KgRuntime *r){
    kg_lease_pressure(r);
}
static KgPinnedBytes *kgp_pin(const RootCell *root,bool bytes_only){
    KgRuntime *r=&kg_runtime;kg_require_owner(r);
    RootCell *source_root=kg_root_new();kg_root_copy(source_root,root);
    RootCell *reserved=kg_root_new();MetadataCharge control={0};KgPinnedBytes *h=kg_control_map(r,sizeof(*h),&control);
    h->control=control;h->self=(uintptr_t)h;h->owner=kg_context(r);
    for(;;){
        kg_enter_native(r);uint64_t ownership=kg_driver_cleanup_enter(r,NULL);kgp_initialize_native(r);kg_leave_native(r);
        KgPinInventory *p=kgp_inventory(r);bool retry=false,flush=false;
        for(;;){
            kg_lock(r);RootFollow follow={.context=(uintptr_t)kg_context(r),.root=kg_root_request_locked(r,source_root)};
            uint64_t status=gc_root_follow_locked(kg_leases(r),&follow,GC_RF_STEP);
            if(status!=GC_RF_DONE){kg_unlock(r);if(status==GC_RF_METADATA_FULL)kgp_grow_owned(r);
                else if(status==GC_RF_FLUSH){flush=true;break;}
                else if(status!=GC_RF_ADVANCED&&status!=GC_RF_WAIT)kg_fail("pin root canonical follow",status);
                continue;}
            uint64_t value=atomic_load_explicit(&source_root->value,memory_order_acquire);
            if(value){
                ReferenceLease lease={.reference=value};status=gc_reference_lease_locked(kg_leases(r),&lease);
                if(status==GC_ML_FULL){kg_unlock(r);kgp_grow_owned(r);continue;}if(status)kg_fail("pin receiver lifetime",status);
                AllocationPage *page=(void *)(uintptr_t)lease.lease.page;
                if(atomic_load_explicit(&page->state,memory_order_acquire)!=GC_PAGE_CLOSED)retry=true;
                else status=kgp_bind_locked(r,p,page);
                uint64_t released=gc_metadata_step_locked(kg_leases(r),&lease.lease,GC_ML_RELEASE);
                if(released)kg_fail("pin receiver temporary release",released);
                if(retry){kg_unlock(r);break;}
                if(status==KG_PIN_FULL){kg_unlock(r);kg_lease_pressure_min(r,2);continue;}if(status)kg_fail("pin page binding",status);
            }
            RootRequest source=kg_root_request_locked(r,source_root),empty=kg_root_request_locked(r,reserved);
            status=gc_root_pin_prepare(&p->registry,kg_context(r),&source,&empty,&h->pin);kg_unlock(r);
            if(status==GC_RPIN_FLUSH){flush=true;break;}
            if(status!=GC_RPIN_MORE)kg_fail("pin independent root adoption",status);
            break;
        }
        if(retry||flush){kg_enter_native(r);kg_driver_cleanup_leave(r,ownership);kg_leave_native(r);
            if(retry)kg_driver_pressure(r,KG_PRESSURE_HEAP);else kg_drain_journals(r);kg_poll();continue;}
#ifdef KG_PIN_PRE_ENTER_HOOK
        KG_PIN_PRE_ENTER_HOOK(r,h);
#endif
        for(;;){
            kg_lock(r);uint64_t status=gc_root_pin_enter_locked(&h->pin);kg_unlock(r);
            if(status==GC_RPIN_GUARDED){
                MutatorContext *ctx;ObjectHeader *header;uint64_t state;
                if(!gc_guarded_object_state((uintptr_t)&h->pin.root.access.scope,h->pin.root.follow.cleanup.page,
                        atomic_load_explicit(&kg_context(r)->adopted_phase,memory_order_acquire),true,&ctx,&header,&state))
                    kg_fail("pin guarded byte shape",0);
                if(state!=GC_OBJECT_PUBLISHED||(header->descriptor_id!=1&&(bytes_only||header->descriptor_id!=2)))
                    kg_fail(bytes_only?"pin requires published RAW_BYTES":"pin requires published supported object",header->descriptor_id);
                h->payload=header->payload_bytes;h->descriptor=header->descriptor_id;
                do{status=gc_root_pin_exit(&h->pin);}while(status==GC_RPIN_GUARDED);
                if(status==GC_RPIN_READY)break;
            }
            if(status==GC_RPIN_EMPTY)break;
            if(status==GC_RPIN_CLEANUP){kg_lock(r);status=gc_root_pin_cleanup_locked(&h->pin);kg_unlock(r);}
            if(status==GC_RPIN_FULL){
#ifdef KG_PIN_METADATA_FULL_HOOK
                KG_PIN_METADATA_FULL_HOOK(r,h);
#endif
                kgp_grow_owned(r);continue;}
            if(status==GC_RPIN_MORE||status==GC_RPIN_WAIT)continue;
            kg_fail("pin page admission",status);
        }
        kg_lock(r);if(p->serial==UINT64_MAX)kg_fail("pin identity wrap",0);
        h->identity=++p->serial;h->next=p->head;if(h->next)h->next->previous=h;p->head=h;kg_unlock(r);
        kg_enter_native(r);kg_driver_cleanup_leave(r,ownership);kg_leave_native(r);
        kg_root_release(source_root);return (KgPinnedBytes *)(uintptr_t)h->identity;
    }
}
KgPinnedBytes *kg_pin_bytes(const RootCell *root){return kgp_pin(root,true);}
KgPinnedObject *kg_pin_object(const RootCell *root){return kgp_pin(root,false);}
void *kg_pinned_bytes_address(const KgPinnedBytes *pin){
    KgRuntime *r=&kg_runtime;kg_require_owner(r);kg_lock(r);KgPinnedBytes *h=kgp_validate_locked(r,pin);
    if(atomic_load_explicit(&h->owner->state,memory_order_acquire)!=GC_BLOCKED_NATIVE||
       atomic_load_explicit(&h->owner->access_epoch,memory_order_acquire))kg_fail("pinned bytes require Native borrow",0);
    if(h->descriptor&&h->descriptor!=1)kg_fail("reference payload requires rooted pin access",h->descriptor);
    void *address=(void *)(uintptr_t)h->pin.address;kg_unlock(r);return address;
}
uint64_t kg_pinned_bytes_size(const KgPinnedBytes *pin){
    (void)kg_pinned_bytes_address(pin);return kg_pinned_object_size(pin);
}
uint64_t kg_pinned_object_size(const KgPinnedObject *pin){
    KgRuntime *r=&kg_runtime;kg_require_owner(r);kg_lock(r);uint64_t bytes=kgp_validate_locked(r,pin)->payload;kg_unlock(r);return bytes;
}
static RootCell *kgp_reference_receiver(KgRuntime *r,const KgPinnedObject *pin,bool *native){
    kg_require_owner(r);kg_lock(r);KgPinnedBytes *h=kgp_validate_locked(r,pin);
    if(h->descriptor!=2||h->pin.stage!=3)kg_fail("pinned reference access requires nonnull REF_RECORD",h->descriptor);
    uint64_t state=atomic_load_explicit(&h->owner->state,memory_order_acquire);
    if(state!=GC_RUNNING&&state!=GC_BLOCKED_NATIVE)kg_fail("pinned reference owner boundary",state);
    if(atomic_load_explicit(&h->owner->access_epoch,memory_order_acquire)||
       atomic_load_explicit(&h->owner->private_producers,memory_order_acquire))kg_fail("pinned reference owner still has managed work",0);
    RootCell *root=(void *)(uintptr_t)h->pin.root.follow.root.handle.cell;*native=state==GC_BLOCKED_NATIVE;kg_unlock(r);
    if(*native)kg_leave_native(r);return root;
}
void kg_pinned_load_ref_into(const KgPinnedObject *pin,uint64_t offset,RootCell *out){
    KgRuntime *r=&kg_runtime;bool native;RootCell *root=kgp_reference_receiver(r,pin,&native);
    kg_load_ref_into(root,offset,out);if(native)kg_enter_native(r);
}
void kg_pinned_store_ref(const KgPinnedObject *pin,uint64_t offset,const RootCell *value){
    KgRuntime *r=&kg_runtime;bool native;RootCell *root=kgp_reference_receiver(r,pin,&native);
    kg_store_ref(root,offset,value);if(native)kg_enter_native(r);
}
void kg_unpin_bytes(KgPinnedBytes *pin){
    KgRuntime *r=&kg_runtime;kg_require_owner(r);kg_lock(r);KgPinnedBytes *h=kgp_validate_locked(r,pin);
    if(atomic_load_explicit(&h->owner->state,memory_order_acquire)!=GC_RUNNING)kg_fail("unpin requires Running owner",0);
    uint64_t status=gc_root_pin_cancel_locked(&h->pin);kg_unlock(r);
    if(status!=GC_RPIN_MORE)kg_fail("pin address/permit cleanup",status);
    for(;;){
        kg_lock(r);uint64_t segment=h->pin.root.follow.root.segment;status=gc_root_pin_finish(&h->pin);
        if(status==GC_RPIN_DONE){
            bool found=false;
            for(KgRootBlock *b=kg_current_mutator(r)->root_blocks;b;b=b->next)if(segment==(uintptr_t)&b->roots->segment){
                if(!b->live)kg_fail("pin reserved root live count",0);b->live--;found=true;break;
            }
            if(!found)kg_fail("pin reserved root storage lifetime",0);
        }
        kg_unlock(r);
        if(status==GC_RPIN_DONE)break;
        if(status!=GC_RPIN_FLUSH)kg_fail("pin independent root release",status);kg_drain_journals(r);kg_poll();
    }
    kg_lock(r);KgPinInventory *p=kgp_inventory(r);
    if(h->previous)h->previous->next=h->next;else p->head=h->next;
    if(h->next)h->next->previous=h->previous;h->self=0;kg_unlock(r);
    kg_control_unmap(r,&h->control);
}
void kg_unpin_object(KgPinnedObject *pin){kg_unpin_bytes(pin);}
#endif
