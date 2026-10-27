#ifndef KLASSIC_GEN_WEAK_IMPL_H
#define KLASSIC_GEN_WEAK_IMPL_H
#include "klassic_gen_weak_backing.h"
static bool kgw_lookup(KgWeakInventory *w,KgWeakHandle public,WeakHandle *handle){
    if(public.runtime!=w->incarnation)return false;
    *handle=(WeakHandle){(uintptr_t)&w->table,public.index,public.serial};
    return gc_weak_table_entry_locked(&w->table,*handle)!=NULL;
}
KgWeakHandle kg_weak_new(const RootCell *source){
    KgRuntime *r=&kg_runtime;kg_require_owner(r);
    for(;;){
        kg_boundary(r,GC_WK_POLL);kg_lock(r);KgWeakInventory *w=kgw_owner(r);
        RootRequest input=kg_root_request_locked(r,source);RootFollow follow={.context=(uintptr_t)kg_context(r),.root=input};
        uint64_t canonical=gc_root_follow_locked(kg_leases(r),&follow,GC_RF_STEP);
        if(canonical!=GC_RF_DONE){kg_unlock(r);
            if(canonical==GC_RF_METADATA_FULL)kg_lease_pressure(r);
            else if(canonical!=GC_RF_ADVANCED&&canonical!=GC_RF_WAIT&&canonical!=GC_RF_FLUSH)kg_fail("weak source canonical follow",canonical);
            kg_poll();continue;}
        /* Fresh canonical observation and core CREATE share this lock. No
         * delayed source read may add a weak lease after source closure. */
        RootedLoadAdmission admission;(void)kg_dispatch(r,&admission);WeakHandle handle={0};
        WeakCreateRequest request={(uintptr_t)&admission,(uintptr_t)kg_context(r),(uintptr_t)&input,(uintptr_t)&handle};
        uint64_t status=gc_weak_create_step_locked(&w->table,&request,0);
        bool slots_full=!w->table.free_head;uint64_t incarnation=w->incarnation;kg_unlock(r);
        if(status==GC_WEAK_LOADED)return (KgWeakHandle){incarnation,handle.index,handle.serial};
        if(status==GC_WEAK_FULL){if(slots_full)kgw_pressure(r);else kg_lease_pressure(r);}
        else if(status!=GC_WEAK_WAIT)kg_fail("weak create admission",status);
        kg_poll();
    }
}
bool kg_weak_load_into(KgWeakHandle public,RootCell *out){
    KgRuntime *r=&kg_runtime;kg_require_owner(r);kg_root_clear(out);
    for(;;){
        kg_boundary(r,GC_WK_POLL);
#ifdef KG_WEAK_BEFORE_LOAD_HOOK
        KG_WEAK_BEFORE_LOAD_HOOK(r);
#endif
        kg_lock(r);KgWeakInventory *w=kgw_owner(r);WeakHandle handle;
        if(!kgw_lookup(w,public,&handle)){kg_unlock(r);return false;}
        RootedLoadAdmission admission;(void)kg_dispatch(r,&admission);
        RootRequest output=kg_root_request_locked(r,out);
        WeakTableRequest request={handle,(uintptr_t)&admission,(uintptr_t)kg_context(r),(uintptr_t)&output};
        uint64_t status=gc_weak_table_step_locked(&w->table,&request,GC_WT_LOAD);kg_unlock(r);
#ifdef KG_WEAK_LOAD_WAIT_HOOK
        if(status==GC_WEAK_WAIT)KG_WEAK_LOAD_WAIT_HOOK(r,public);
#endif
        if(status==GC_WEAK_EMPTY)return false;
        if(status==GC_WEAK_LOADED){kg_root_copy(out,out);return true;}
        if(status!=GC_WEAK_WAIT&&status!=GC_WEAK_FLUSH)kg_fail("weak rooted load",status);
        kg_poll();
    }
}
bool kg_weak_release(KgWeakHandle public){
    KgRuntime *r=&kg_runtime;kg_require_owner(r);kg_lock(r);KgWeakInventory *w=kgw_owner(r);WeakHandle handle;
    if(!kgw_lookup(w,public,&handle)){kg_unlock(r);return false;}
    WeakTableRequest request={.handle=handle};uint64_t status=gc_weak_table_step_locked(&w->table,&request,GC_WT_RELEASE);kg_unlock(r);
    if(status!=GC_WEAK_RELEASED)kg_fail("weak explicit release",status);return true;
}
void kg_weak_drop_all(KgRuntime *r){
    kg_lock(r);KgWeakInventory *w=kgw_owner(r);w->closing=1;uint64_t capacity=w->table.capacity;kg_unlock(r);
    for(uint64_t i=0;i<capacity;i++){
        kg_lock(r);WeakEntry *entry=&w->table.entries[i];
        if(entry->slot.serial){WeakTableRequest request={.handle={(uintptr_t)&w->table,i+1,entry->slot.serial}};
            uint64_t status=gc_weak_table_step_locked(&w->table,&request,GC_WT_RELEASE);
            if(status!=GC_WEAK_RELEASED)kg_fail("weak shutdown handle release",status);}
        kg_unlock(r);if(!(i%128))kg_poll();
    }
#ifdef KG_WEAK_AFTER_DROP_HOOK
    KG_WEAK_AFTER_DROP_HOOK(r);
#endif
}
uint64_t kg_weak_mark_locked(KgRuntime *r,unsigned gen,uint64_t op){
    KgWeakInventory *w=kgw_owner(r);
    if(!gc_native_cycle_valid(&r->native,gen))return GC_MP_INVALID;
    if(op==GC_MP_BEGIN){
        if(w->mark.stage||w->mark.processor||w->mark.scan||w->retiring)return GC_MP_WAIT;
        w->mark.mark=&r->native.cycle[gen].marking;
    }else if(w->mark.mark!=&r->native.cycle[gen].marking)return GC_MP_INVALID;
    return gc_weak_mark_step_locked(&r->native.roots,&w->mark,op);
}
bool kg_weak_phase_frozen_locked(KgRuntime *r){
    KgWeakInventory *w=kgw_owner(r);return w->mark.stage==3||w->retiring;
}
static uint64_t kgw_resolve(const AllocationPage *page,ResolveRequest *request,uint64_t op){
    uint64_t status;do{status=gc_resolve_access(page,request,op);}while(op==GC_RESOLVE_EXIT&&status==GC_RESOLVE_WAIT);
    return status;
}
#ifdef KG_WEAK_OBSERVE_HOOK
static uint64_t kgw_test_observe(const GuardedMarkPrepare *access,uint64_t gen,uint64_t epoch){
    uint64_t status=gc_guarded_weak_liveness(access,gen,epoch);KG_WEAK_OBSERVE_HOOK(access,gen,epoch,status);return status;
}
#define KGW_OBSERVE kgw_test_observe
#else
#define KGW_OBSERVE gc_guarded_weak_liveness
#endif
static WeakProcessOps kgw_process_ops(void){
    return (WeakProcessOps){gc_reference_lease_locked,gc_pending_follow_locked,gc_pending_access_locked,
        kgw_resolve,KGW_OBSERVE,gc_metadata_step_locked};
}
#undef KGW_OBSERVE
uint64_t kg_weak_scan_step(KgRuntime *r,MutatorContext *actor){
    kg_lock(r);KgWeakInventory *w=kgw_owner(r);WeakScan *scan=&w->scan;WeakProcessOps ops=kgw_process_ops();
    uint64_t status;
    if(!scan->stage)status=gc_weak_scan_begin_locked(&w->table,&w->mark,actor,scan);
    else if(scan->stage==2){kg_unlock(r);return GC_WS_DONE;}
    else if(!scan->work.stage||scan->work.stage==6)status=gc_weak_scan_next_locked(scan,&ops);
    else if(scan->work.stage==1)status=gc_weak_process_enter_locked(&scan->work,&ops);
    else if(scan->work.stage==4||scan->work.stage==5){status=gc_weak_process_commit_locked(&scan->work,&ops);
#ifdef KG_WEAK_PROCESS_COMMIT_HOOK
        KG_WEAK_PROCESS_COMMIT_HOOK(r,&scan->work);
#endif
    }
    else{kg_unlock(r);kg_fail("weak collector guard across quantum",scan->work.stage);}
    bool guarded=scan->work.stage==2;kg_unlock(r);
    if(guarded){
        status=gc_weak_process_observe(&scan->work,&ops);
        if(status!=GC_WP_LIVE&&status!=GC_WP_DEAD&&status!=GC_WP_WAIT)kg_fail("weak liveness observation",status);
        uint64_t exited=gc_weak_process_exit(&scan->work,&ops);
        if(exited!=GC_WP_OK)kg_fail("weak collector guard exit",exited);
    }
    if(status==GC_WP_FULL||status==GC_WP_EXHAUSTED||status==GC_WS_DONE)return status;
    if(status==GC_WP_INVALID||status==GC_WP_CLEANUP)kg_fail("weak collector processing",status);
    return GC_WP_ADVANCED;
}
uint64_t kg_weak_move_begin_locked(KgRuntime *r,unsigned gen){
    KgWeakInventory *w=kgw_owner(r);
    if(w->mark.mark!=&r->native.cycle[gen].marking)return GC_RP_INVALID;
    uint64_t status=gc_weak_relocation_begin_locked(&w->scan,&r->native.roots,&r->native.cycle[gen].moving);
    if(status==GC_RP_ADVANCED)w->scan=(WeakScan){0};return status;
}
bool kg_weak_rebind_source(KgRuntime *r,KgRun *source){
    kg_lock(r);KgWeakInventory *w=kgw_owner(r);
    if(w->rebind_source!=source){w->rebind_source=source;w->rebind_index=0;w->rebind_capacity=w->table.capacity;w->rebind_serial=w->table.last_serial;}
    if(w->rebind_index==w->rebind_capacity){
        /* New source admissions are excluded by canonical CREATE under the
         * same lock after all source cells forwarded. A prior source lease
         * still delays sealing; a new handle serial/capacity restarts coverage. */
        if((w->table.last_serial!=w->rebind_serial||w->table.capacity!=w->rebind_capacity)&&
           gc_native_allocation_page(&source->native)->metadata_leases>(kg_pin_source_selected_locked(r,source)?1u:0u)){
            w->rebind_index=0;w->rebind_capacity=w->table.capacity;w->rebind_serial=w->table.last_serial;
        }else{kg_unlock(r);return true;}
    }
    uint64_t index=++w->rebind_index;WeakEntry *entry=&w->table.entries[index-1];
    if(entry->slot.serial&&entry->slot.reference){
        MetadataLeaseRequest checked=entry->slot.lease;
        uint64_t status=gc_metadata_step_locked(kg_leases(r),&checked,GC_ML_RESOLVE);
        if(status)kg_fail("weak source token resolve",status);
        if(checked.page==(uintptr_t)gc_native_allocation_page(&source->native)){
            WeakFollowRequest follow={index,entry->slot.serial,{0}};
            status=gc_weak_follow_step_locked(&w->table,&follow,0);
            if(follow.retired.token){uint64_t cleanup=gc_metadata_step_locked(kg_leases(r),&follow.retired,GC_ML_RELEASE);
                if(cleanup)kg_fail("weak forwarding retired lease",cleanup);}
            if(status==GC_RF_METADATA_FULL){w->rebind_index--;kg_unlock(r);return false;}
            if(status!=GC_RF_ADVANCED)kg_fail("weak selected source did not forward",status);
        }
    }
    kg_unlock(r);return false;
}
void kg_weak_retire_begin_locked(KgRuntime *r,unsigned gen){
    KgWeakInventory *w=kgw_owner(r);
    if(w->mark.stage||w->mark.processor||w->mark.scan||w->gate.closed||w->retiring)kg_fail("weak retirement admission",gen);
    w->gate.closed=1;w->retiring=gen+1;w->incoming_source=NULL;w->incoming=(WeakIncoming){0};
}
bool kg_weak_incoming_source_locked(KgRuntime *r,KgRun *source){
    KgWeakInventory *w=kgw_owner(r);
    if(!w->retiring||!w->gate.closed)kg_fail("weak incoming without exclusion",0);
    if(w->incoming_source!=source){w->incoming_source=source;w->incoming=(WeakIncoming){0};
        AllocationPage *page=gc_native_allocation_page(&source->native);
        uint64_t status=gc_weak_incoming_begin_locked(&w->table,page->start,page->limit,&w->incoming);
        if(status!=GC_WI_MORE)kg_fail("weak incoming admission",status);return false;}
    uint64_t status=gc_weak_incoming_next_locked(&w->incoming,gc_metadata_step_locked);
    if(status==GC_WI_FOUND)kg_fail("weak source reference after seal",w->incoming.observed);
    if(status!=GC_WI_DONE&&status!=GC_WI_MORE)kg_fail("weak incoming proof",status);
    return status==GC_WI_DONE;
}
void kg_weak_cycle_finish_locked(KgRuntime *r,unsigned gen){
    KgWeakInventory *w=kgw_owner(r);
    if(w->retiring!=gen+1||w->mark.stage||w->mark.scan||w->mark.processor)kg_fail("weak completion ownership",gen);
    w->retiring=0;w->gate.closed=0;w->incoming=(WeakIncoming){0};w->incoming_source=w->rebind_source=NULL;
}
#endif
