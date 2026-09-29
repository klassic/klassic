/* One pin's private registered root package. Backing preparation and budget
 * reserve/commit use gc_native_roots outside locks in native state. This owner
 * publishes that package, reserves the private root and transfers it to
 * RootPin. Only this owner may retire the package; active or partly cleaned
 * pins reject retirement. Keep this control, charge and mapping stable.
 *
 * PUBLISH/RETIRE require the root registry lock. PREPARE runs Running outside
 * locks/guards; the caller root remains protected on all retry paths. RootPin
 * acquisition/cancellation retain their separate heap-lock/guard contracts.
 * After RETIRE, normal root deletion/reader/journal/cleaner work must FINISH
 * before DISPOSE. Drain registered journals with CLAIM/COPY/mark handoff/ACK
 * before CLEAN; CLEANING storage cannot accept a new journal claim.
 * DISPOSE runs outside locks in native state; failed unmap
 * retains the mapping and charge for retry. Budget RELEASE follows successful
 * disposal under the heap lock. Public error unwinding remains a caller loop.
 */
#ifndef KLASSIC_GC_NATIVE_PIN_H
#define KLASSIC_GC_NATIVE_PIN_H
#include "gc_native_roots.h"
#include "gc_root_pin.h"
static inline bool gc_native_pin_valid(const NativePin *q){
    if(!q||((uintptr_t)q&7)||q->self!=(uintptr_t)q||!q->registry||(q->registry&7)||
       !q->context||(q->context&7)||!q->charge||(q->charge&7)||!q->storage||(q->storage&7))return false;
    const MetadataCharge *charge=(void *)(uintptr_t)q->charge;
    return charge->stage==2&&charge->allocation==q->storage;
}
static inline uint64_t gc_native_pin_publish_locked(NativePin *q,RootRegistry *registry,
    MutatorContext *ctx,MetadataCharge *charge){
    if(!q||((uintptr_t)q&7))return GC_RPIN_INVALID;
    const unsigned char *bytes=(const void *)q;
    for(uint64_t i=0;i<sizeof(*q);i++)if(bytes[i])return GC_RPIN_INVALID;
    uint64_t status=gc_native_roots_publish_locked(registry,ctx,charge);
    if(status!=GC_RL_OK)return status==GC_RL_EXHAUSTED?GC_RPIN_EXHAUSTED:GC_RPIN_INVALID;
    q->self=(uintptr_t)q;q->registry=(uintptr_t)registry;q->context=(uintptr_t)ctx;
    q->charge=(uintptr_t)charge;q->storage=charge->allocation;q->stage=1;
    RootStorage *storage=(void *)(uintptr_t)q->storage;
    q->reserved.segment=(uintptr_t)&storage->segment;return GC_RPIN_MORE;
}
static inline uint64_t gc_native_pin_prepare(NativePin *q,PinPageRegistry *pages,const RootRequest *source){
    if(!gc_native_pin_valid(q)||(q->stage!=1&&q->stage!=2)||!source||((uintptr_t)source&7)||
       source->segment==q->reserved.segment)return GC_RPIN_INVALID;
    MutatorContext *ctx=(void *)(uintptr_t)q->context;
    if(q->stage==1){
        uint64_t status=gc_root_step(ctx,&q->reserved,GC_ROOT_RESERVE);
        if(status!=GC_ROOT_OK)return gc_root_pin_root_status(status);
        q->stage=2;
    }
    uint64_t status=gc_root_pin_prepare(pages,ctx,source,&q->reserved,&q->pin);
    if(status==GC_RPIN_MORE)q->stage=3;
    return status;
}
static inline uint64_t gc_native_pin_retire_locked(NativePin *q){
    if(!gc_native_pin_valid(q)||q->stage<1||q->stage>3)return GC_RPIN_INVALID;
    if(q->stage==3&&(!gc_root_pin_valid(&q->pin)||q->pin.stage!=6))return GC_RPIN_WAIT;
    MutatorContext *ctx=(void *)(uintptr_t)q->context;
    if(q->stage==2){
        if(gc_root_cancel_empty(ctx,&q->reserved)!=GC_ROOT_OK)return GC_RPIN_INVALID;
        q->reserved.handle=(RootHandle){0};q->stage=1;
    }
    RootStorage *storage=(void *)(uintptr_t)q->storage;
    RootLifeRequest retire={.registration=(uintptr_t)&storage->registration,
        .context=q->context,.scope=storage->segment.scope};
    uint64_t status=gc_root_life_locked((void *)(uintptr_t)q->registry,&retire,GC_RL_RETIRE);
    if(status==GC_RL_OK){q->stage=4;return GC_RPIN_MORE;}
    return status==GC_RL_EXHAUSTED?GC_RPIN_EXHAUSTED:GC_RPIN_INVALID;
}
static inline uint64_t gc_native_pin_dispose(NativePin *q,const NativeMetadataOps *ops){
    if(!gc_native_pin_valid(q)||q->stage!=4)return GC_RPIN_INVALID;
    const MutatorContext *ctx=(void *)(uintptr_t)q->context;
    if(ctx->abi_version!=GC_ABI_VERSION||!ctx->thread_id||
       atomic_load_explicit(&ctx->state,memory_order_acquire)!=GC_BLOCKED_NATIVE||
       atomic_load_explicit(&ctx->access_epoch,memory_order_acquire)||
       !gc_native_metadata_charge_valid((void *)(uintptr_t)q->charge,ops))return GC_RPIN_INVALID;
    uint64_t status=gc_native_roots_dispose((void *)(uintptr_t)q->charge,ops,gc_root_storage_can_dispose);
    if(status!=GC_NATIVE_METADATA_OK)return status==GC_NATIVE_METADATA_OS_ERROR?GC_RPIN_CLEANUP:GC_RPIN_WAIT;
    q->storage=0;q->stage=5;return GC_RPIN_DONE;
}
static inline uint64_t gc_native_pin_publish_entry_locked(NativePin *q,const NativePinStart *start){
    if(!start||((uintptr_t)start&7))return GC_RPIN_INVALID;
    return gc_native_pin_publish_locked(q,(void *)(uintptr_t)start->registry,
        (void *)(uintptr_t)start->context,(void *)(uintptr_t)start->charge);
}
/* Final owner action under the charge's heap lock, after successful DISPOSE.
 * The context/control/charge must still be live; backing need not be mapped.
 * Failure preserves the outstanding budget obligation for retry. Stage 6 is
 * terminal and may be reclaimed by the caller; replay does not return credit. */
static inline uint64_t gc_native_pin_release_locked(NativePin *q,HeapPool *heap){
    if(!q||((uintptr_t)q&7)||q->self!=(uintptr_t)q||q->stage!=5||q->storage||
       !heap||((uintptr_t)heap&7)||!q->charge||(q->charge&7)||
       !q->context||(q->context&7))return GC_RPIN_INVALID;
    const MutatorContext *ctx=(void *)(uintptr_t)q->context;
    if(ctx->abi_version!=GC_ABI_VERSION||!ctx->thread_id||
       atomic_load_explicit(&ctx->state,memory_order_acquire)!=GC_BLOCKED_NATIVE||
       atomic_load_explicit(&ctx->access_epoch,memory_order_acquire))return GC_RPIN_INVALID;
    if(gc_budget_step_locked(heap,(void *)(uintptr_t)q->charge,GC_BUDGET_RELEASE)!=GC_BUDGET_OK)
        return GC_RPIN_INVALID;
    q->stage=6;return GC_RPIN_DONE;
}
#endif
