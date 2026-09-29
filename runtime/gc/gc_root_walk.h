/* Bounded discovery over the registered inventory, under the SAME lock as
 * lifetime operations. Attach inserts at the head. A walk holds one reader
 * lease as its cursor. CLEAN unlinks only nodes with no readers and completed
 * deletion work. A delayed walk can therefore skip a removed node safely;
 * post-BEGIN attachments stay before its cursor and publish through journals.
 * No phase transition or root traversal occurs in BEGIN. The coordinator must
 * count walkers/leases/journals/retirement jobs in phase handoff/termination.
 */
#ifndef KLASSIC_GC_ROOT_WALK_H
#define KLASSIC_GC_ROOT_WALK_H
#include "gc_root_lifetime.h"
#define GC_RW_BEGIN 0u
#define GC_RW_NEXT 1u
#define GC_RW_ITEM 0u
#define GC_RW_INVALID 1u
#define GC_RW_DONE 2u
#define GC_RW_EXHAUSTED 3u
#define GC_RW_WAIT 4u

static inline uint64_t gc_root_walk_locked(RootRegistry *g, RootWalk *w, uint64_t op) {
    if (op > GC_RW_NEXT) return GC_RW_INVALID;
    RootRegistration *previous = NULL, *next;
    if (op == GC_RW_BEGIN) {
        if (w->active || w->lease.kind || w->pending || w->pending_reference || w->pending_observed || w->pending_index) return GC_RW_INVALID;
        uint64_t young = g->phase & 15, old = (g->phase >> 4) & 15;
        if (young > 7 || old > 7) return GC_RW_INVALID;
        next = (RootRegistration *)(uintptr_t)g->inventory;
    } else {
        if (w->active != 1 || w->registry != (uintptr_t)g || w->lease.kind != 1 ||
            !w->lease.registration || (w->lease.registration & 7) || !w->lease.serial || !g->walkers || !g->leases)
            return GC_RW_INVALID;
        previous = (RootRegistration *)(uintptr_t)w->lease.registration;
        if (previous->registered != 1 || previous->scope != w->lease.scope || !previous->leases ||
            previous->leases > g->leases || w->cursor > w->limit) return GC_RW_INVALID;
        if (w->cursor != w->limit || w->completed_mask != w->mask || w->pending || w->pending_reference || w->pending_observed || w->pending_index) return GC_RW_WAIT;
        next = (RootRegistration *)(uintptr_t)previous->inventory_next;
    }
    uint64_t limit = 0;
    if (next) {
        if (((uintptr_t)next & 7) || next->registered != 1 || !next->scope ||
            (previous ? next->scope >= previous->scope || next->inventory_previous != (uintptr_t)previous :
                        next->inventory_previous != 0) ||
            !next->segment || (next->segment & 7) || next->leases > g->leases) return GC_RW_INVALID;
        RootSegment *s = (RootSegment *)(uintptr_t)next->segment;
        uint64_t state = atomic_load_explicit(&s->state, memory_order_acquire);
        limit = atomic_load_explicit(&s->used, memory_order_acquire);
        if (state < 1 || state > 2 || s->registration != (uintptr_t)next || s->scope != next->scope ||
            !gc_root_segment_valid(s) || limit > s->capacity) return GC_RW_INVALID;
        if (g->last_lease == UINT64_MAX || next->leases == UINT64_MAX ||
            (!previous && (g->leases == UINT64_MAX || g->walkers == UINT64_MAX))) return GC_RW_EXHAUSTED;
    }
    if (previous) {
        previous->leases--;
        if (!next) { g->leases--; g->walkers--; }
    } else {
        w->registry = (uintptr_t)g; w->phase = g->phase;
        uint64_t young = g->phase & 15, old = (g->phase >> 4) & 15;
        w->mask = (young >= 1 && young <= 3 ? 1u : 0u) | (old >= 1 && old <= 3 ? 2u : 0u);
        if (next) { g->leases++; g->walkers++; }
    }
    w->cursor = w->completed_mask = 0; w->limit = limit;
    w->active = next != NULL;
    if (!next) { w->lease.kind = 0; return GC_RW_DONE; }
    next->leases++;
    w->lease = (RootLease){(uintptr_t)next,next->scope,++g->last_lease,1,0};
    return GC_RW_ITEM;
}

/* One walk owner scans outside the registry lock while its cursor lease pins
 * native storage. Each call reads <=256 cells of the captured prefix. New
 * roots after this prefix are publication-journal work. A copied request's old
 * serial is rejected after NEXT reuses the embedded lease control record. */
static inline uint64_t gc_root_walk_scan(RootWalkScan *q) {
    if (!q->walk || (q->walk & 7) || !q->output || (q->output & 7) || !q->capacity ||
        q->capacity > 256 || q->output > UINT64_MAX - q->capacity * 8) return GC_ROOT_INVALID;
    RootWalk *w = (RootWalk *)(uintptr_t)q->walk;
    if (w->pending || w->pending_reference || w->pending_observed || w->pending_index) return GC_ROOT_INVALID;
    if (w->active != 1 || w->lease.kind != 1 || !q->serial || q->serial != w->lease.serial ||
        !w->lease.registration || (w->lease.registration & 7) || w->cursor > w->limit) return GC_ROOT_INVALID;
    RootRegistration *r = (RootRegistration *)(uintptr_t)w->lease.registration;
    if (r->registered != 1 || r->scope != w->lease.scope || !r->segment || (r->segment & 7)) return GC_ROOT_INVALID;
    RootSegment *s = (RootSegment *)(uintptr_t)r->segment;
    uint64_t state = atomic_load_explicit(&s->state, memory_order_acquire);
    uint64_t used = atomic_load_explicit(&s->used, memory_order_acquire);
    if (!gc_root_segment_valid(s) || state < 1 || state > 2 || s->scope != r->scope ||
        s->registration != (uintptr_t)r || used > s->capacity || w->limit > used) return GC_ROOT_INVALID;
    uint64_t n = w->limit - w->cursor;
    if (n > q->capacity) n = q->capacity;
    RootCell *cells = (RootCell *)(uintptr_t)s->cells; uint64_t *out = (uint64_t *)(uintptr_t)q->output;
    for (uint64_t i = 0; i < n; i++) out[i] = atomic_load_explicit(&cells[w->cursor + i].value,memory_order_acquire);
    w->cursor += n; q->written = n;
    return w->cursor == w->limit ? 0 : 2;
}
/* Physical same-object repair by the unique walk owner, outside registry lock.
 * The walk lease pins native cells; expected/desired logical identity and strong
 * reachability are caller obligations. Both references and forwarding metadata
 * remain protected through CAS; no source/address reuse is permitted meanwhile.
 * Preserve every observed color bit: this leaf grants no current-epoch credit.
 * Never read owner-private active/reuse fields. Exact CAS preserves concurrent
 * logical stores/releases, which still journal and increment root_revision.
 * A successful repair changes neither logical roots nor their revision/journal.
 * No poll, allocation, raw managed address or retirement certificate is produced.
 */
#define GC_RWH_OK 0u
#define GC_RWH_INVALID 1u
#define GC_RWH_CHANGED 2u
static inline uint64_t gc_root_walk_heal(RootWalkHeal *q) {
    uintptr_t ignored;
    if(!q->walk||(q->walk&7)||!q->expected||!q->desired||
        (uint16_t)q->expected!=(uint16_t)q->desired||
        !gc_abi_decode_ref((GcRef){q->expected},0,&ignored)||
        !gc_abi_decode_ref((GcRef){q->desired},0,&ignored))return GC_RWH_INVALID;
    RootWalk *w=(RootWalk *)(uintptr_t)q->walk;
    if(w->active!=1||w->lease.kind!=1||!q->serial||q->serial!=w->lease.serial||
        !w->lease.registration||(w->lease.registration&7)||w->cursor>w->limit||q->index>=w->cursor)return GC_RWH_INVALID;
    RootRegistration *r=(RootRegistration *)(uintptr_t)w->lease.registration;
    if(r->registered!=1||r->scope!=w->lease.scope||!r->segment||(r->segment&7))return GC_RWH_INVALID;
    RootSegment *s=(RootSegment *)(uintptr_t)r->segment;
    uint64_t state=atomic_load_explicit(&s->state,memory_order_acquire),used=atomic_load_explicit(&s->used,memory_order_acquire);
    if(!gc_root_segment_valid(s)||state<1||state>2||s->scope!=r->scope||s->registration!=(uintptr_t)r||
        used>s->capacity||w->limit>used)return GC_RWH_INVALID;
    RootCell *cell=(RootCell *)(uintptr_t)s->cells+q->index;uint64_t observed=q->expected;
    return atomic_compare_exchange_strong_explicit(&cell->value,&observed,q->desired,memory_order_acq_rel,memory_order_acquire)?GC_RWH_OK:GC_RWH_CHANGED;
}

/* A single durable snapshot is part of the leased walk, not a borrowed root
 * handle. Its active registry lease/walker count blocks phase termination.
 * CAPTURE loads one root atomically and advances the scan cursor; NEXT and bulk
 * scan cannot bypass an outstanding item. The unique owner may advance only
 * pending_reference along same-object forwarding, acquiring destination
 * metadata before releasing its current token. pending_observed/index remain
 * fixed for exact-CAS repair. Both references are retirement holders until ACK.
 * ACK matches the captured index as well as the walk serial, so a delayed ACK
 * cannot discard a subsequent item in the same segment.
 * ACK certifies queue/phase handoff and completion of any repair; it cannot be
 * used to discard an unoffered nonnull snapshot. No walk/metadata lease alone
 * substitutes for this strong pending-work obligation. No polling occurs here.
 */
#define GC_RWI_CAPTURE 0u
#define GC_RWI_ACK 1u
#define GC_RWI_ITEM 0u
#define GC_RWI_INVALID 1u
#define GC_RWI_DONE 2u
#define GC_RWI_WAIT 3u
static inline uint64_t gc_root_walk_item(RootWalkItem *q,uint64_t op){
    if(op>GC_RWI_ACK||!q->walk||(q->walk&7))return GC_RWI_INVALID;
    RootWalk *w=(RootWalk *)(uintptr_t)q->walk;
    if(w->active!=1||w->lease.kind!=1||!q->serial||q->serial!=w->lease.serial||
        !w->lease.registration||(w->lease.registration&7)||w->cursor>w->limit||w->pending>1)return GC_RWI_INVALID;
    RootRegistration *r=(RootRegistration *)(uintptr_t)w->lease.registration;
    if(r->registered!=1||r->scope!=w->lease.scope||!r->segment||(r->segment&7))return GC_RWI_INVALID;
    RootSegment *s=(RootSegment *)(uintptr_t)r->segment;
    uint64_t state=atomic_load_explicit(&s->state,memory_order_acquire),used=atomic_load_explicit(&s->used,memory_order_acquire);
    if(!gc_root_segment_valid(s)||state<1||state>2||s->scope!=r->scope||s->registration!=(uintptr_t)r||
        used>s->capacity||w->limit>used)return GC_RWI_INVALID;
    if(op==GC_RWI_ACK){
        if(w->pending!=1||!w->cursor||q->index!=w->pending_index||w->pending_index!=w->cursor-1)return GC_RWI_INVALID;
        w->pending=w->pending_reference=w->pending_observed=w->pending_index=0;return GC_RWI_ITEM;
    }
    if(w->pending)return GC_RWI_WAIT;
    if(w->pending_reference||w->pending_observed||w->pending_index)return GC_RWI_INVALID;
    if(w->cursor==w->limit)return GC_RWI_DONE;
    RootCell *cells=(RootCell *)(uintptr_t)s->cells;
    uint64_t value=atomic_load_explicit(&cells[w->cursor].value,memory_order_acquire);
    w->pending_reference=w->pending_observed=value;w->pending_index=q->index=w->cursor;w->pending=1;w->cursor++;
    return GC_RWI_ITEM;
}
#endif
