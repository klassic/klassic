/* Controlled actor interleaving: both guards precede selection; the physical
 * store logs before CAS, a stale healer fails, and the copier waits for EXIT.
 * Native fixture ownership keeps source/destination and all references live. */
#ifndef KLASSIC_GC_BARRIER_PIPELINE_H
#define KLASSIC_GC_BARRIER_PIPELINE_H
#include "gc_barrier_cases.h"
#include "gc_source_access_cases.h"
#include "gc_forward_cases.h"
#include "gc_relocate_target_cases.h"
#include "gc_remembered_cases.h"
#include "gc_remembered_inventory_cases.h"
#include "gc_buffer_job.h"
static void gc_run_guarded_store_moves(GuardedStorer store,GuardedHealer heal_step,SourceAccess access,ForwardStep forward,RelocateCopy copy,RelocatePublish publish,RelocateTarget target,RememberedInventoryStep inventory){
    for(unsigned round=0;round<64;round++){
        StoreFixture f;uint64_t old=(UINT64_C(128)<<16)|0x1510;
        store_fixture(&f,0x10022,GC_OLD,old);f.scope.held=0;atomic_store(&f.ctx.access_epoch,0);atomic_store(&f.cell.access,0);
        MutatorContext healer={.abi_version=GC_ABI_VERSION,.thread_id=18,.adopted_phase=f.q.phase};
        SourceAccessRequest heal={(uintptr_t)&healer,48,27,0,0};
        assert(access(&f.cell,&f.scope,GC_SA_ENTER)==GC_SA_OK);assert(access(&f.cell,&heal,GC_SA_ENTER)==GC_SA_OK);
        _Atomic uint64_t *slot=(_Atomic uint64_t *)(f.heap+48);uint64_t observed=atomic_load_explicit(slot,memory_order_acquire);
        ForwardRequest claim={48,27,1,0};assert(forward(&f.cell,&claim,GC_FWD_CLAIM)==GC_FWD_OK);
        SourceAccessRequest select={0,48,27,0,1};assert(access(&f.cell,&select,GC_SA_SELECT)==GC_SA_OK);
        assert(access(&f.cell,&select,GC_SA_CHECK)==GC_SA_WAIT);
        if(round%2==0){
            f.ctx.remembered.top=3;atomic_store(&f.ctx.remembered.published,3);
            assert(store(&f.q)==GC_STORE_FULL&&!f.q.transferred&&!f.ctx.satb.top&&atomic_load(slot)==old);
            // A real consumer frees room; no flush/poll occurs inside a guard.
            atomic_store(&f.ctx.remembered.consumed,3);
        }
        assert(store(&f.q)==GC_STORE_OK&&f.q.transferred==3);
        assert(f.satb[0].previous.bits==old&&f.remembered[0].slot.owner.bits==f.q.remembered.slot.owner.bits);
        assert(atomic_load(&f.ctx.satb.published)==1&&atomic_load(&f.ctx.remembered.published)==f.ctx.remembered.top);
        BarrierEntry pending;
        BarrierJob consumer={.identity=f.ctx.thread_id,.selector=1,.output=(uintptr_t)&pending,.capacity=1};
        assert(gc_buffer_job_step(&f.ctx,&consumer,GC_BJ_CLAIM)==GC_BJ_OK);
        assert(gc_buffer_job_step(&f.ctx,&consumer,GC_BJ_COPY)==GC_BJ_OK&&consumer.written==1);
        assert(pending.slot.owner.bits>>16==48&&pending.slot.page_incarnation==27);
        GuardedHeal repair={.scope=(uintptr_t)&heal,.page=(uintptr_t)&f.page,.registry=(uintptr_t)&f.registry,
            .expected=observed,.desired=(observed&~UINT64_C(0xffff))|(gc_abi_color((PhaseSnapshot){f.q.phase})&~0x30u)|(observed&0x30u),.phase=f.q.phase};
        assert(heal_step(&repair)==GC_HEAL_CHANGED&&atomic_load(slot)==f.q.desired);
        _Atomic uint64_t starts[5]={0};ForwardCell destination_cell={0};ForwardTable table={(uintptr_t)&destination_cell,1,0};
        AllocationPage destination={.heap_base=(uintptr_t)f.heap,.start=4064,.limit=8192,.incarnation=28,.identity_base=700,
            .generation=GC_OLD,.state=GC_PAGE_READY,.starts=(uintptr_t)starts,.starts_words=5,.allocated_top=4064,.forward_table=(uintptr_t)&table};
        RelocationCopy job={(uintptr_t)f.heap,27,1,3072,0,0,8,0,0};
        assert(target(&destination,&job,GC_TARGET_PREPARE)==GC_TARGET_OK);
        assert(copy(&f.cell,&job)==GC_COPY_WAIT);
        assert(access(&f.cell,&heal,GC_SA_EXIT)==GC_SA_OK);
        assert(copy(&f.cell,&job)==GC_COPY_WAIT);
        assert(access(&f.cell,&f.scope,GC_SA_EXIT)==GC_SA_OK);
        assert(access(&f.cell,&select,GC_SA_CHECK)==GC_SA_OK);
        uint64_t status;do{status=copy(&f.cell,&job);}while(status==GC_COPY_MORE);assert(status==GC_COPY_DONE);
        assert(target(&destination,&job,GC_TARGET_INSTALL)==GC_TARGET_OK);
        assert(publish(&f.cell,&job,0x1510)==GC_COPY_DONE);
        assert(atomic_load((_Atomic uint64_t *)(f.heap+4096))==f.q.desired);
        assert(((ObjectHeader *)(f.heap+4096-32))->mark_record==(uintptr_t)&f.record);
        // The copied entry still names the original owner. Resolve its actual
        // forwarding edge, then register the destination SLOT, not its value.
        // Native fixture ownership retains both pages/metadata during this test;
        // production dead-owner checks and lease release are separate work.
        uint64_t maps[2][16]={{0}};
        RememberedSet set={.start=16,.limit=8192,.incarnation=28,.bits={(uintptr_t)maps[0],(uintptr_t)maps[1]},
            .words=16,.epoch=0x10022,.last_phase=0x10022};
        RememberedInventory pages={.epoch=set.epoch,.last_phase=set.epoch};RememberedRegistration registration={.set=(uintptr_t)&set};
        RememberedJob flip={.incarnation=28,.phase=0x21021};
        RememberedInventoryRequest work={(uintptr_t)&registration,(uintptr_t)&flip,flip.phase};
        assert(inventory(&pages,&work,GC_RI_ATTACH)==GC_RS_OK);
        assert(inventory(&pages,&work,GC_RI_BEGIN)==GC_RS_OK&&pages.unflipped==1);
        assert(inventory(&pages,&work,GC_RI_FLIP)==GC_RS_OK&&!pages.unflipped);
        assert(pending.slot.owner.bits>>16==f.cell.source_offset&&pending.slot.page_incarnation==f.cell.incarnation);
        ForwardRequest moved={pending.slot.owner.bits>>16,pending.slot.page_incarnation,0,0};
        assert(forward(&f.cell,&moved,GC_FWD_READ)==GC_FWD_OK);
        uint64_t canonical=moved.destination>>16;
        RememberedJob entry={.offset=canonical+pending.slot.byte_offset,.incarnation=28,.phase=pending.slot.record_epoch};
        work.job=(uintptr_t)&entry;
        assert(inventory(&pages,&work,GC_RI_RECORD)==GC_RS_OK&&set.pending==1&&set.current_count==1&&pages.pending==1);
        assert(gc_buffer_job_step(&f.ctx,&consumer,GC_BJ_ACK)==GC_BJ_OK);
        for(unsigned cycle=0;cycle<2;cycle++) {
            RememberedJob scan={.budget=256};work.job=(uintptr_t)&scan;
            assert(inventory(&pages,&work,GC_RI_TAKE)==GC_RS_OK&&scan.offset==4096);
            assert(inventory(&pages,&work,GC_RI_CHECK)==GC_RS_WAIT&&pages.claims==1&&!pages.pending);
            atomic_store(&f.ctx.adopted_phase,set.epoch);
            SourceAccessRequest reader={(uintptr_t)&f.ctx,canonical,28,0,0};
            assert(access(&destination_cell,&reader,GC_SA_ENTER)==GC_SA_OK);
            assert(atomic_load_explicit((_Atomic uint64_t *)(f.heap+scan.offset),memory_order_acquire)==f.q.desired);
            assert(access(&destination_cell,&reader,GC_SA_EXIT)==GC_SA_OK);
            scan.keep=cycle==0;assert(inventory(&pages,&work,GC_RI_ACK)==GC_RS_OK);
            assert(inventory(&pages,&work,GC_RI_CHECK)==GC_RS_EMPTY);
            flip.phase=(set.epoch+0x10000)^0x1000;work.job=(uintptr_t)&flip;work.phase=flip.phase;
            assert(inventory(&pages,&work,GC_RI_BEGIN)==GC_RS_OK);
            assert(inventory(&pages,&work,GC_RI_FLIP)==GC_RS_OK);
        }
        assert(!set.pending&&!set.current_count&&!set.active_serial);
        assert(inventory(&pages,&work,GC_RI_DETACH)==GC_RS_OK&&!pages.members&&!pages.head);
    }
    puts("guarded store: 64 closed-admission writes, stale-heal rejection and drained-copy rounds PASS");
    puts("remembered slots: 64 buffered moved-slot replays, late entries and repeated flips PASS");
    puts("remembered inventory: 64 moved-buffer handoffs preserve aggregate pending and claim counts PASS");
}
#endif
