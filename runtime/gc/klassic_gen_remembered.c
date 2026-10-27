#include "klassic_gen_remembered.h"
#include "gc_buffer_job.h"
#include "gc_remembered_lease.h"
#include "gc_remembered_owner.h"
#include "gc_remembered_transfer.h"
#include "gc_remembered_walk.h"
#include "gc_promotion_remember.h"

typedef struct {
    RememberedSet set;
    RememberedRegistration registration;
    uint64_t bits[];
} KgRemembered;
static uint64_t kg_rem_dead_finish_locked(KgRuntime *,KgRun *,const RememberedOwner *,
    const RememberedJob *,const KgRememberedResult *);
static KgRemembered *kg_rem_set(KgRun *run){return (KgRemembered *)run->remembered;}
static RememberedInventory *kg_rem_inventory(KgRuntime *r){return &r->native.remembered;}
static KgRun *kg_rem_run_locked(KgRuntime *r,const AllocationPage *page){
    for(NativeAllocationRun *n=(void *)(uintptr_t)r->native.allocations;n;n=(void *)(uintptr_t)n->next){
        if(gc_native_allocation_page(n)==page){
            KgRemembered *s=kg_rem_set((KgRun *)n);
            if(!s||s->set.incarnation!=page->incarnation)kg_fail("Old run has no remembered inventory",page->incarnation);
            return (KgRun *)n;
        }
    }
    kg_fail("remembered page run ownership",page->incarnation);
}
static KgRemembered *kg_rem_page_locked(KgRuntime *r,const AllocationPage *page){return kg_rem_set(kg_rem_run_locked(r,page));}
uint64_t kg_remembered_attach(KgRuntime *r,KgRun *run,const MutatorContext *actor){
    if(!gc_native_allocation_actor_native(&run->native,actor)||run->remembered||run->remembered_charge.stage)
        return GC_BUDGET_INVALID;
    AllocationPage *page=gc_native_allocation_page(&run->native);
    if(page->generation!=GC_OLD||run->native.stage<3||run->native.stage>8)return GC_BUDGET_INVALID;
    uint64_t words=((page->limit-page->start)/8+63)/64;
    if(!words||words>(UINT64_MAX-sizeof(KgRemembered))/16||
       gc_native_metadata_size(sizeof(KgRemembered)+words*16,r->os.metadata.quantum,&run->remembered_charge.bytes))
        return GC_BUDGET_INVALID;
    kg_lock(r);uint64_t status=gc_budget_step_locked(&r->native.backing.heap,&run->remembered_charge,GC_BUDGET_RESERVE);kg_unlock(r);
    if(status){run->remembered_charge=(MetadataCharge){0};return status;}
    uint64_t failures=0;
    while((status=gc_native_metadata_prepare(&run->remembered_charge,&r->os.metadata))==GC_NATIVE_METADATA_OS_ERROR)
        kg_os_retry(&failures,"remembered bitmap mapping OS retry limit");
    if(status)kg_fail("remembered bitmap mapping",status);
    KgRemembered *s=(void *)(uintptr_t)run->remembered_charge.allocation;
    kg_lock(r);
    status=gc_budget_step_locked(&r->native.backing.heap,&run->remembered_charge,GC_BUDGET_COMMIT);
    if(status)kg_fail("remembered bitmap commitment",status);
    RememberedInventory *g=kg_rem_inventory(r);
    s->set=(RememberedSet){.start=page->start,.limit=page->limit,.incarnation=page->incarnation,
        .bits={(uintptr_t)s->bits,(uintptr_t)(s->bits+words)},.words=words,
        .epoch=g->epoch,.previous_epoch=g->previous_epoch,.last_phase=g->epoch};
    s->registration.set=(uintptr_t)&s->set;
    RememberedJob job={0};RememberedInventoryRequest request={(uintptr_t)&s->registration,(uintptr_t)&job,0};
    status=gc_remembered_inventory_step_locked(g,&request,GC_RI_ATTACH);
    if(status)kg_fail("remembered page registration",status);
    run->remembered=s;kg_unlock(r);return GC_BUDGET_OK;
}
static void kg_rem_flip_locked(KgRuntime *r,RememberedRegistration *registration){
    RememberedInventory *g=kg_rem_inventory(r);if(registration->epoch==g->epoch)return;
    RememberedSet *s=(void *)(uintptr_t)registration->set;
    RememberedJob job={.incarnation=s->incarnation,.phase=g->epoch,.budget=256};
    RememberedInventoryRequest request={(uintptr_t)registration,(uintptr_t)&job,0};
    uint64_t status=gc_remembered_inventory_step_locked(g,&request,GC_RI_FLIP);
    if(status)kg_fail("remembered page epoch flip",status);
}
void kg_remembered_begin(KgRuntime *r,uint64_t phase){
    RememberedWalk walk={0};kg_lock(r);
    if(phase!=r->native.remembered.epoch||(phase&15)!=GC_MARK_START)kg_fail("remembered prepared epoch",phase);
    uint64_t status=gc_remembered_walk_locked(kg_rem_inventory(r),&walk,GC_RMW_BEGIN);kg_unlock(r);
    while(status==GC_RMW_ITEM){
        kg_lock(r);kg_rem_flip_locked(r,(void *)(uintptr_t)walk.registration);
        status=gc_remembered_walk_locked(kg_rem_inventory(r),&walk,GC_RMW_NEXT);kg_unlock(r);
    }
    if(status!=GC_RMW_DONE)kg_fail("remembered flip walk",status);
}
void kg_remembered_drain_context(KgRuntime *r,MutatorContext *context){
    for(;;){
        BarrierEntry entries[16];BarrierJob job={.identity=context->thread_id,.selector=1,.output=(uintptr_t)entries,.capacity=16};
        kg_lock(r);uint64_t status=gc_buffer_job_step(context,&job,GC_BJ_CLAIM);kg_unlock(r);
        if(status==GC_BJ_EMPTY)return;
        if(status!=GC_BJ_OK)kg_fail("remembered transport claim",status);
        status=gc_buffer_job_step(context,&job,GC_BJ_COPY);if(status)kg_fail("remembered transport copy",status);
        for(uint64_t i=0;i<job.written;i++){
            BarrierEntry *e=&entries[i];kg_lock(r);
            MetadataLeaseRequest lease={.token=e->slot.metadata_lease};
            status=gc_metadata_step_locked(kg_leases(r),&lease,GC_ML_RESOLVE);
            if(status)kg_fail("remembered transport metadata receipt",status);
            AllocationPage *page=(void *)(uintptr_t)lease.page;
            uint64_t owner=e->slot.owner.bits>>16;
            if(!e->slot.owner.bits||!gc_store_ref_valid(e->slot.owner.bits)||page->generation!=GC_OLD||
               lease.incarnation!=e->slot.page_incarnation||owner<page->start+sizeof(ObjectHeader)||owner>=page->limit||
               e->slot.byte_offset>=page->limit-owner||(e->slot.byte_offset&7))kg_fail("remembered source geometry",owner);
            KgRemembered *s=kg_rem_page_locked(r,page);kg_rem_flip_locked(r,&s->registration);
            RememberedJob record={.offset=owner+e->slot.byte_offset,.incarnation=lease.incarnation,.phase=e->slot.record_epoch};
            RememberedInventoryRequest request={(uintptr_t)&s->registration,(uintptr_t)&record,0};
            status=gc_remembered_inventory_step_locked(kg_rem_inventory(r),&request,GC_RI_RECORD);
            if(status)kg_fail("remembered captured-phase record",status);
            status=gc_metadata_step_locked(kg_leases(r),&lease,GC_ML_RELEASE);
            if(status)kg_fail("remembered transport metadata release",status);
            kg_unlock(r);
        }
        kg_lock(r);status=gc_buffer_job_step(context,&job,GC_BJ_ACK);kg_unlock(r);
        if(status)kg_fail("remembered transport acknowledgement",status);
        return; /* one actual batch; hot owners cannot monopolize the driver */
    }
}
void kg_remembered_scan(KgRuntime *r,KgRememberedVisit visit,void *opaque){
    if(!visit)kg_fail("remembered visitor missing",0);
    RememberedWalk walk={0};kg_lock(r);uint64_t status=gc_remembered_walk_locked(kg_rem_inventory(r),&walk,GC_RMW_BEGIN);kg_unlock(r);
    while(status==GC_RMW_ITEM){
        RememberedRegistration *registration=(void *)(uintptr_t)walk.registration;
        for(;;){
            RememberedJob claim={.budget=256};RememberedInventoryRequest request={(uintptr_t)registration,(uintptr_t)&claim,0};
            kg_lock(r);kg_rem_flip_locked(r,registration);
            uint64_t taken=gc_remembered_inventory_step_locked(kg_rem_inventory(r),&request,GC_RI_TAKE);
            if(taken==GC_RS_MORE){kg_unlock(r);continue;}
            if(taken==GC_RS_EMPTY){kg_unlock(r);break;}
            if(taken!=GC_RS_OK)kg_fail("remembered inventory take",taken);
            RememberedLease retained={.registration=(uintptr_t)registration,.claim=(uintptr_t)&claim};
            uint64_t q=gc_remembered_lease_locked(kg_leases(r),&retained);
            if(q)kg_fail("remembered scan metadata lease",q);
            kg_unlock(r);
            RememberedOwner owner={.registration=(uintptr_t)registration,.claim=(uintptr_t)&claim,.page=retained.lease.page};
            q=gc_remembered_owner(&owner);if(q)kg_fail("remembered source object owner",q);
            KgRememberedResult result={0};visit(r,registration,&claim,&owner,&retained.lease,&result,opaque);
            kg_lock(r);
            if(result.keep>1||(result.target&&result.keep))kg_fail("remembered visitor receipt",result.keep);
            if(result.dead_record){
                KgRun *source=kg_rem_run_locked(r,(void *)(uintptr_t)retained.lease.page);
                q=kg_rem_dead_finish_locked(r,source,&owner,&claim,&result);
            }else if(result.target){
                if(result.dead_epoch)kg_fail("remembered transfer carries death epoch",result.dead_epoch);
                KgRemembered *destination=kg_rem_set(result.target);
                if(!destination)kg_fail("remembered transfer target missing",0);
                kg_rem_flip_locked(r,&destination->registration);
                RememberedTransfer transfer={.source=(uintptr_t)registration,.claim=(uintptr_t)&claim,
                    .destination=(uintptr_t)&destination->registration,.offset=result.target_offset,
                    .incarnation=destination->set.incarnation};
                q=gc_remembered_transfer_locked(kg_rem_inventory(r),&transfer);
            }else{
                if(result.dead_epoch)kg_fail("remembered ACK carries death epoch",result.dead_epoch);
                claim.keep=result.keep;q=gc_remembered_inventory_step_locked(kg_rem_inventory(r),&request,GC_RI_ACK);
            }
            if(q)kg_fail("remembered rooted handoff acknowledgement",q);
            q=gc_metadata_step_locked(kg_leases(r),&retained.lease,GC_ML_RELEASE);
            if(q)kg_fail("remembered scan metadata release",q);
            kg_unlock(r);
        }
        kg_lock(r);status=gc_remembered_walk_locked(kg_rem_inventory(r),&walk,GC_RMW_NEXT);kg_unlock(r);
    }
    if(status!=GC_RMW_DONE)kg_fail("remembered scan inventory walk",status);
}
void kg_remembered_promote(KgRuntime *r,KgRun *run,uint64_t owner,uint64_t descriptor,uint64_t payload,uint64_t phase){
    KgRemembered *s=kg_rem_set(run);if(!s)kg_fail("promotion inventory missing",0);
    PromotionRemember job={.registry=(uintptr_t)kg_descriptors(r),.registration=(uintptr_t)&s->registration,
        .owner=owner,.descriptor_id=descriptor,.payload_bytes=payload,.phase=phase};
    for(;;){
        kg_lock(r);kg_rem_flip_locked(r,&s->registration);
        uint64_t status=gc_promotion_remember_locked(kg_rem_inventory(r),&job);kg_unlock(r);
        if(status==GC_PR_DONE)return;if(status!=GC_PR_MORE)kg_fail("promotion reference slot handoff",status);
    }
}
/* Finite migration claim for a sealed source's CURRENT bank. The ordinary
 * Young TAKE removes previous-bank work; it cannot claim current-only slots.
 * This transition leaves the bit set while issuing a unique source receipt.
 * Transfer moves its existing bank obligations before consuming the receipt;
 * a current-only claim cannot create previous-epoch work in the destination.
 * Source sealing excludes future stores; buffered late entries still retain
 * source metadata and are drained/revisited before outer source retirement. */
static uint64_t kg_rem_migration_take_locked(KgRuntime *r,KgRun *run,RememberedJob *claim,bool *current_origin){
    KgRemembered *owned=kg_rem_set(run);RememberedInventory *g=kg_rem_inventory(r);
    RememberedInventoryRequest check={(uintptr_t)&owned->registration,(uintptr_t)claim,0};
    uint64_t status=gc_remembered_inventory_step_locked(g,&check,GC_RI_VALIDATE);
    if(status)return status;
    RememberedSet *s=&owned->set;AllocationPage *page=gc_native_allocation_page(&run->native);
    if(page->generation!=GC_OLD||page->incarnation!=s->incarnation||page->start!=s->start||page->limit!=s->limit||
       page->owner||atomic_load_explicit(&page->state,memory_order_acquire)!=GC_PAGE_EVACUATED)return GC_RS_INVALID;
    if(s->active_serial)return GC_RS_WAIT;
    if(!s->current_count)return gc_remembered_inventory_step_locked(g,&check,GC_RI_TAKE);
    if(!s->previous_epoch||s->epoch!=g->epoch)return GC_RS_WAIT;
    if(s->last_serial==UINT64_MAX)return GC_RS_EXHAUSTED;
    if(g->claims>=g->members||claim->serial||claim->budget!=256)return GC_RS_INVALID;
    uint64_t cursor=s->cursor,*bits=(void *)(uintptr_t)s->bits[s->current];
    for(uint64_t examined=0;examined<256;examined++){
        uint64_t word=bits[cursor];
        if(word){
            unsigned bit=0;while(!(word&(UINT64_C(1)<<bit)))bit++;
            uint64_t offset=s->start+(cursor*64+bit)*8;if(offset>=s->limit)return GC_RS_INVALID;
            s->active_serial=++s->last_serial;s->active_offset=offset;s->cursor=cursor;g->claims++;
            *claim=(RememberedJob){.offset=offset,.incarnation=s->incarnation,.phase=s->epoch,
                .serial=s->active_serial,.budget=256};*current_origin=true;return GC_RS_OK;
        }
        if(++cursor==s->words)cursor=0;
        if(cursor==s->cursor)return GC_RS_INVALID;
    }
    s->cursor=cursor;return GC_RS_MORE;
}
/* A current-origin migration claim has not consumed a previous-bank bit. If
 * no late previous record arrived while its visitor ran, only future-epoch
 * work moves. Do not manufacture an already-completed Young epoch's debt by
 * using the previous-claim transfer operation. A real TAKE claim must NEVER
 * enter here merely because its previous bit was removed at TAKE time. */
static uint64_t kg_rem_current_transfer_locked(RememberedInventory *g,const RememberedTransfer *q){
    RememberedInventoryRequest check={.registration=q->source,.job=q->claim};
    if(gc_remembered_inventory_step_locked(g,&check,GC_RI_VALIDATE)!=GC_RS_OK)return GC_RS_INVALID;
    check.registration=q->destination;
    if(gc_remembered_inventory_step_locked(g,&check,GC_RI_VALIDATE)!=GC_RS_OK)return GC_RS_INVALID;
    RememberedSet *s=(void *)(uintptr_t)((RememberedRegistration *)(uintptr_t)q->source)->set;
    RememberedSet *d=(void *)(uintptr_t)((RememberedRegistration *)(uintptr_t)q->destination)->set;
    RememberedJob *claim=(void *)(uintptr_t)q->claim;
    if(s==d||(s->start<d->limit&&d->start<s->limit)||claim->incarnation!=s->incarnation||
       !claim->serial||claim->serial!=s->active_serial||claim->offset!=s->active_offset||claim->phase!=s->epoch||claim->keep||
       q->incarnation!=d->incarnation||q->offset<d->start||q->offset>=d->limit||(q->offset&7)||
       s->last_phase>g->last_phase||d->last_phase>g->last_phase)return GC_RS_INVALID;
    if(s->epoch!=g->epoch||d->epoch!=g->epoch)return GC_RS_WAIT;
    uint64_t si=(claim->offset-s->start)/8,di=(q->offset-d->start)/8;
    uint64_t sm=UINT64_C(1)<<(si%64),dm=UINT64_C(1)<<(di%64);si/=64;di/=64;
    uint64_t *sc=(uint64_t *)(uintptr_t)s->bits[s->current]+si,*sp=(uint64_t *)(uintptr_t)s->bits[s->current^1]+si;
    uint64_t *dc=(uint64_t *)(uintptr_t)d->bits[d->current]+di;
    /* A concurrent late record creates an actual previous obligation. Preserve
     * it via the shared transfer transaction instead of clearing it here. */
    if(*sp&sm)return gc_remembered_transfer_locked(g,q);
    uint64_t present=!!(*dc&dm);
    if(!(*sc&sm)||!s->current_count||d->current_count<present||(!present&&d->current_count==(d->limit-d->start)/8)||
       g->claims<1+!!d->active_serial)return GC_RS_INVALID;
    *dc|=dm;d->current_count+=!present;*sc&=~sm;s->current_count--;
    s->active_serial=s->active_offset=0;g->claims--;
    if(s->last_phase>d->last_phase)d->last_phase=s->last_phase;
    return GC_RS_OK;
}
/* Retire one dead owner's remembered obligation after real Old selection and
 * source closure. The visitor established this exact owner's record under its
 * source guard before sealing. Validate its live arena/import ownership again
 * before using the epoch; remove only the uniquely claimed slot, not a map or
 * counter reset. Current records for other owners remain intact. */
static uint64_t kg_rem_dead_finish_locked(KgRuntime *r,KgRun *run,const RememberedOwner *owner,
        const RememberedJob *claim,const KgRememberedResult *result){
    KgRemembered *owned=kg_rem_set(run);RememberedSet *s=&owned->set;RememberedInventory *g=kg_rem_inventory(r);
    RememberedInventoryRequest check={(uintptr_t)&owned->registration,(uintptr_t)claim,0};
    if(gc_remembered_inventory_step_locked(g,&check,GC_RI_VALIDATE)!=GC_RS_OK||!result->dead_record||
       !result->dead_epoch||result->target||result->keep||run->selected_generation!=GC_OLD||
       !run->selected_serial||!run->selected_cycle||
       run->selected_cycle!=r->native.cycle[GC_OLD].ticket.cycle||
       run->selected_epoch!=result->dead_epoch||
       gc_native_runtime_work(&r->native)->queues[GC_OLD].epoch!=result->dead_epoch||
       claim->serial!=s->active_serial||claim->offset!=s->active_offset||claim->phase!=s->epoch||!g->claims)
        return GC_RS_INVALID;
    AllocationPage *page=gc_native_allocation_page(&run->native);ForwardCell *cell=(void *)(uintptr_t)owner->cell;
    if(atomic_load_explicit(&page->state,memory_order_acquire)!=GC_PAGE_EVACUATED||page->owner||
       atomic_load_explicit(&cell->word,memory_order_acquire)!=3||
       atomic_load_explicit(&cell->access,memory_order_acquire)!=GC_SOURCE_CLOSED)return GC_RS_INVALID;
    ForwardTable *table=(void *)(uintptr_t)run->native.forwarding.allocation;
    uint64_t index=((uintptr_t)cell-table->cells)/sizeof(ForwardCell),used=atomic_load_explicit(&table->used,memory_order_acquire);
    if((uintptr_t)cell<table->cells||((uintptr_t)cell-table->cells)%sizeof(ForwardCell)||index>=used||used>table->capacity)return GC_RS_INVALID;
    uint64_t imported=((uint64_t *)(uintptr_t)run->native.imports)[index];
    NativeAllocationRun *origin=&run->native;
    if(imported){
        /* This is the immutable per-copy import installed by the shared copy
         * transaction, not a pointer supplied by the visitor. The retained
         * source-page metadata lease excludes physical import retirement;
         * that import in turn keeps its original arena/control alive. No
         * whole-runtime allocation-list walk belongs in this finite step. */
        origin=(void *)(uintptr_t)imported;
        if(!gc_native_allocation_valid(origin)||origin->runtime!=&r->native||
           !origin->import_users||origin->stage<8||origin->stage>10)return GC_RS_INVALID;
    }
    uint64_t record=(uintptr_t)result->dead_record;
    if(origin->mark_records.stage!=2||origin->marks.records!=origin->mark_records.allocation||
       !origin->marks.used||origin->marks.used>origin->marks.capacity||record<origin->marks.records||
       record-origin->marks.records>=origin->marks.used*sizeof(MarkRecord)||
       (record-origin->marks.records)%sizeof(MarkRecord)||!result->dead_record->object_id||
       result->dead_record->old_epoch==result->dead_epoch)return GC_RS_INVALID;
    if(!imported&&record!=origin->marks.records+index*sizeof(MarkRecord))return GC_RS_INVALID;
    uint64_t word=(claim->offset-s->start)/8,mask=UINT64_C(1)<<(word%64);word/=64;
    uint64_t *current=(uint64_t *)(uintptr_t)s->bits[s->current]+word,*previous=(uint64_t *)(uintptr_t)s->bits[s->current^1]+word;
    uint64_t c=!!(*current&mask),p=!!(*previous&mask),before=!!s->pending;
    if(s->current_count<c||s->pending<p||g->pending<before)return GC_RS_INVALID;
    *current&=~mask;*previous&=~mask;s->current_count-=c;s->pending-=p;
    g->pending=g->pending-before+!!s->pending;g->claims--;s->active_serial=s->active_offset=0;return GC_RS_OK;
}
void kg_remembered_migrate_run(KgRuntime *r,KgRun *run,KgRememberedVisit visit,void *opaque){
    KgRemembered *owned=kg_rem_set(run);if(!owned||!visit)kg_fail("remembered source migration ownership",0);
    for(;;){
        RememberedJob claim={.budget=256};bool current_origin=false;kg_lock(r);kg_rem_flip_locked(r,&owned->registration);
        uint64_t status=kg_rem_migration_take_locked(r,run,&claim,&current_origin);
        if(status==GC_RS_MORE){kg_unlock(r);continue;}
        if(status==GC_RS_EMPTY){kg_unlock(r);return;}
        if(status!=GC_RS_OK)kg_fail("remembered sealed-source migration claim",status);
        RememberedLease retained={.registration=(uintptr_t)&owned->registration,.claim=(uintptr_t)&claim};
        status=gc_remembered_lease_locked(kg_leases(r),&retained);if(status)kg_fail("remembered migration lease",status);
        kg_unlock(r);
        RememberedOwner owner={.registration=(uintptr_t)&owned->registration,.claim=(uintptr_t)&claim,.page=retained.lease.page};
        status=gc_remembered_owner(&owner);if(status)kg_fail("remembered migration owner",status);
        KgRememberedResult result={0};visit(r,&owned->registration,&claim,&owner,&retained.lease,&result,opaque);
        kg_lock(r);
        if(result.dead_record){
            status=kg_rem_dead_finish_locked(r,run,&owner,&claim,&result);
        }else{
            if(!result.target||result.target==run||result.keep||result.dead_epoch)kg_fail("remembered migration canonical target receipt",result.keep);
            KgRemembered *target=kg_rem_set(result.target);if(!target)kg_fail("remembered migration target inventory",0);
            kg_rem_flip_locked(r,&target->registration);
            RememberedTransfer transfer={.source=(uintptr_t)&owned->registration,.claim=(uintptr_t)&claim,
                .destination=(uintptr_t)&target->registration,.offset=result.target_offset,.incarnation=target->set.incarnation};
            status=current_origin?kg_rem_current_transfer_locked(kg_rem_inventory(r),&transfer):
                gc_remembered_transfer_locked(kg_rem_inventory(r),&transfer);
        }
        if(status)kg_fail("remembered sealed-source slot handoff",status);
        status=gc_metadata_step_locked(kg_leases(r),&retained.lease,GC_ML_RELEASE);if(status)kg_fail("remembered migration lease release",status);
        kg_unlock(r);
    }
}
uint64_t kg_remembered_detach_with_pin(KgRuntime *r,KgRun *run,const MutatorContext *actor,
        PinPageRegistry *pins,const RelocationGateRequest *selection){
    if(!gc_native_allocation_actor_native(&run->native,actor))return GC_RS_INVALID;
    KgRemembered *s=kg_rem_set(run);if(!s)return GC_RS_OK;
    RememberedJob job={0};RememberedInventoryRequest request={.registration=(uintptr_t)&s->registration,
        .job=(uintptr_t)&job,.phase=(uintptr_t)gc_native_allocation_page(&run->native)};
    kg_lock(r);
    AllocationPage *page=gc_native_allocation_page(&run->native);
    /* Cancellation of a never-used private READY target has no source domain.
     * It cannot pass DETACH_PAGE's closed-source predicate; validate its actual
     * empty target metadata before using the inventory's private-owner detach. */
    bool unused=atomic_load_explicit(&page->state,memory_order_acquire)==GC_PAGE_READY&&
        atomic_load_explicit(&page->allocated_top,memory_order_acquire)==page->start&&
        !page->owner&&!page->metadata_leases&&run->native.stage==3&&page->forward_table&&
        !atomic_load_explicit(&((ForwardTable *)(uintptr_t)page->forward_table)->used,memory_order_acquire);
    bool selected_pin=false;
    if(pins||selection){
        NativeSelectedSource *proof=&run->selection;
        if(!pins||!selection||pins!=r->pin_registry||pins->pool!=kg_leases(r)||run->native.stage!=8||
           atomic_load_explicit(&page->state,memory_order_acquire)!=GC_PAGE_EVACUATED||
           proof->self!=(uintptr_t)proof||proof->run!=&run->native||proof->incarnation!=page->incarnation||
           proof->generation!=GC_OLD||run->selected_generation!=GC_OLD||proof->cycle!=run->selected_cycle||
           proof->cycle!=r->native.cycle[GC_OLD].ticket.cycle||proof->epoch!=run->selected_epoch||
           proof->gate_epoch!=r->native.cycle[GC_OLD].gate.epoch||
           gc_relocation_gate_locked(&r->native.cycle[GC_OLD].gate,NULL,GC_RG_CHECK)!=GC_RG_OK||
           proof->epoch!=gc_native_runtime_work(&r->native)->queues[GC_OLD].epoch||
           proof->active||proof->cursor!=proof->used||proof->seal_cursor!=proof->used||
           proof->live_count+proof->dead_count!=proof->used||proof->table!=run->native.forwarding.allocation||
           proof->top!=atomic_load_explicit(&page->allocated_top,memory_order_acquire)||
           proof->used!=atomic_load_explicit(&((ForwardTable *)(uintptr_t)proof->table)->used,memory_order_acquire)||
           !run->selected_serial||!pins->records||run->native.lease.head_index>=pins->capacity){kg_unlock(r);return GC_RS_INVALID;}
        MetadataLeaseRequest *lifetime=&pins->records[run->native.lease.head_index].lifetime;
        if(lifetime->page!=(uintptr_t)page||lifetime->incarnation!=page->incarnation||
           gc_pin_selection_locked(pins,lifetime,selection)!=GC_RG_OK){kg_unlock(r);return GC_RS_INVALID;}
        /* The one authenticated holder retains page/table metadata, not this
         * bitmap backing. Every OTHER metadata holder must still have left.
         * RI_DETACH independently checks all real current/previous bits,
         * pending counts, claims, registrations, walks and epoch ownership. */
        if(page->metadata_leases!=1||page->owner){kg_unlock(r);return GC_RS_WAIT;}
        selected_pin=true;
    }
    uint64_t status=gc_remembered_inventory_step_locked(kg_rem_inventory(r),&request,(unused||selected_pin)?GC_RI_DETACH:GC_RI_DETACH_PAGE);
    if(!status)run->remembered=NULL;kg_unlock(r);if(status)return status;
    uint64_t failures=0;
    while((status=gc_native_metadata_dispose(&run->remembered_charge,&r->os.metadata))==GC_NATIVE_METADATA_OS_ERROR)
        kg_os_retry(&failures,"remembered backing unmap OS retry limit");
    if(status)kg_fail("remembered backing unmap",status);
    kg_lock(r);status=gc_budget_step_locked(&r->native.backing.heap,&run->remembered_charge,GC_BUDGET_RELEASE);kg_unlock(r);
    if(status)kg_fail("remembered backing budget release",status);
    run->remembered_charge=(MetadataCharge){0};return GC_RS_OK;
}
uint64_t kg_remembered_detach(KgRuntime *r,KgRun *run,const MutatorContext *actor){
    return kg_remembered_detach_with_pin(r,run,actor,NULL,NULL);
}
