/* Registered root journal handoff. CLAIM/ACK hold the root registry lock;
 * COPY runs outside it and reads at most 256 entries. One unique lease control
 * record belongs to one job. Do not mix raw gc_root_drain with this consumer.
 * Ring slots are NOT consumed by COPY: they and the counted kind-4 lease retain
 * all old/new references until ACK certifies their phase-specific mark handoff.
 * No operation waits, polls, allocates or accesses a managed object. */
#ifndef KLASSIC_GC_ROOT_JOURNAL_H
#define KLASSIC_GC_ROOT_JOURNAL_H
#include "gc_root_lifetime.h"
#define GC_RJ_CLAIM 0u
#define GC_RJ_COPY 1u
#define GC_RJ_ACK 2u
#define GC_RJ_OK 0u
#define GC_RJ_INVALID 1u
#define GC_RJ_EMPTY 2u
#define GC_RJ_WAIT 3u
#define GC_RJ_EXHAUSTED 4u
static inline uint64_t gc_root_journal_step(RootRegistry *g,RootJournalRequest *q,uint64_t op){
    if(op>GC_RJ_ACK||!q->registration||(q->registration&7)||!q->lease||(q->lease&7))return GC_RJ_INVALID;
    RootRegistration *r=(RootRegistration *)(uintptr_t)q->registration;
    RootLease *l=(RootLease *)(uintptr_t)q->lease;
    if(r->registered!=1||!r->scope||q->scope!=r->scope||!r->segment||(r->segment&7))return GC_RJ_INVALID;
    RootSegment *s=(RootSegment *)(uintptr_t)r->segment;
    uint64_t state=atomic_load_explicit(&s->state,memory_order_acquire);
    if(s->registration!=(uintptr_t)r||s->scope!=r->scope||state<1||state>2||!s->journal||(s->journal&7))return GC_RJ_INVALID;
    BufferCursor *b=(BufferCursor *)(uintptr_t)s->journal;
    if(!b->entries||(b->entries&7)||!b->capacity||b->capacity>UINT32_MAX||
        b->entries>UINT64_MAX-b->capacity*sizeof(RootChange))return GC_RJ_INVALID;
    // Only the claimed consumer can change consumed. Never read producer top.
    uint64_t begin=atomic_load_explicit(&b->consumed,memory_order_acquire);
    uint64_t published=atomic_load_explicit(&b->published,memory_order_acquire);
    if(begin>published||published-begin>b->capacity)return GC_RJ_INVALID;
    if(op==GC_RJ_CLAIM){
        if(l->kind||r->leases>g->leases||!!r->journal_serial!=!!r->journal_end)return GC_RJ_INVALID;
        if(r->journal_serial)return GC_RJ_WAIT;
        if(!q->capacity||q->capacity>256)return GC_RJ_INVALID;
        if(begin==published)return GC_RJ_EMPTY;
        if(g->last_lease==UINT64_MAX||g->leases==UINT64_MAX||r->leases==UINT64_MAX)return GC_RJ_EXHAUSTED;
        uint64_t count=published-begin;if(count>q->capacity)count=q->capacity;
        *l=(RootLease){(uintptr_t)r,r->scope,++g->last_lease,4,begin};
        r->leases++;g->leases++;r->journal_serial=l->serial;r->journal_end=begin+count;
        q->serial=l->serial;q->begin=begin;q->end=r->journal_end;q->written=0;
        return GC_RJ_OK;
    }
    if(l->kind!=4||!q->serial||q->serial!=l->serial||l->registration!=(uintptr_t)r||l->scope!=r->scope||
        r->journal_serial!=l->serial||q->begin!=begin||q->end!=r->journal_end||q->end<=begin||
        q->end>published||q->end-begin>256)return GC_RJ_INVALID;
    uint64_t count=q->end-begin;
    if(op==GC_RJ_COPY){
        if(l->cursor!=begin||q->written||!q->output||(q->output&7)||q->capacity<count||q->capacity>256||
            q->output>UINT64_MAX-q->capacity*sizeof(RootChange))return GC_RJ_INVALID;
        RootChange *entries=(RootChange *)(uintptr_t)b->entries,*out=(RootChange *)(uintptr_t)q->output;
        for(uint64_t i=0;i<count;i++)out[i]=entries[(begin+i)%b->capacity];
        q->written=count;l->cursor=q->end;
        return GC_RJ_OK;
    }
    if(!r->leases||r->leases>g->leases)return GC_RJ_INVALID;
    if(l->cursor!=q->end||q->written!=count)return GC_RJ_WAIT;
    // Caller certifies both sides of every entry were offered for its captured
    // phase, or retained as counted pending work. Empty queue alone cannot ACK.
    atomic_store_explicit(&b->consumed,q->end,memory_order_release);
    r->journal_serial=r->journal_end=0;r->leases--;g->leases--;l->kind=0;
    return GC_RJ_OK;
}
#endif
