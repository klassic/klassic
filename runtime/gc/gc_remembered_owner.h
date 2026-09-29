/* Metadata-only owner candidate for a unique active remembered claim. Its
 * owner retains the registration/set/job and the page/table/start bitmap via
 * a metadata lease or exclusive fixture preparation right. No concurrent ACK
 * or claim reuse is allowed. Mutable inventory counters/list links and heap
 * bytes are never read; source physical pages may already be inaccessible.
 *
 * Acquire allocated_top BEFORE the table's acquire-used snapshot: a slot in
 * that committed prefix must find its published table key and start bit. A
 * concurrently appended table suffix may precede top/start publication and
 * cannot authorize access beyond the captured top. Keys are append-only in
 * increasing order, as in gc_forward_table.h. At most 32 binary-search probes
 * plus the chosen key and its neighbor are read, independent of payload size.
 *
 * Output owner is a BYTE OFFSET, not a GcRef or managed address. It identifies
 * a start-bit-backed predecessor and excludes known next headers. Padding or
 * scalar fields can still yield a candidate: caller follows that same logical
 * owner into a protected root and validates payload/descriptor under canonical
 * access before slot use. Do not invent a mark-good reference or permanently
 * root a dead Old owner. Strong liveness/phase and retirement remain separate.
 * No polling, allocation, ACK or lease transfer occurs. Rejection/MISSING
 * preserve output and the claim; neither is permission to drop pending work.
 */
#ifndef KLASSIC_GC_REMEMBERED_OWNER_H
#define KLASSIC_GC_REMEMBERED_OWNER_H
#include "gc_abi.h"
enum { GC_RO_OK=0, GC_RO_INVALID=1, GC_RO_MISSING=2 };
static inline uint64_t gc_remembered_owner(RememberedOwner *q){
    if(!q||((uintptr_t)q&7)||q->owner||q->byte_offset||q->cell||
       !q->registration||(q->registration&7)||!q->claim||(q->claim&7)||!q->page||(q->page&7))return GC_RO_INVALID;
    RememberedRegistration *r=(RememberedRegistration *)(uintptr_t)q->registration;
    RememberedJob *job=(RememberedJob *)(uintptr_t)q->claim;
    if(!r->registry||(r->registry&7)||!r->identity||!r->set||(r->set&7))return GC_RO_INVALID;
    RememberedSet *s=(RememberedSet *)(uintptr_t)r->set;AllocationPage *p=(AllocationPage *)(uintptr_t)q->page;
    if(!s->incarnation||!job->serial||job->serial!=s->active_serial||job->offset!=s->active_offset||
       job->incarnation!=s->incarnation||job->phase!=s->epoch||r->epoch!=s->epoch||
       !gc_abi_valid_phase((PhaseSnapshot){job->phase})||!(job->phase>>16)||
       !s->start||s->start>=s->limit||s->limit>GC_MAX_OFFSET||((s->start|s->limit)&15)||
       job->offset<s->start||job->offset>=s->limit||(job->offset&7)||
       p->start!=s->start||p->limit!=s->limit||p->incarnation!=s->incarnation||p->generation!=GC_OLD)return GC_RO_INVALID;
    uint64_t state=atomic_load_explicit(&p->state,memory_order_acquire);
    if(state!=GC_PAGE_ALLOCATING&&state!=GC_PAGE_CLOSED&&state!=GC_PAGE_EVACUATED&&state!=GC_PAGE_UNCOMMITTED)return GC_RO_INVALID;
    uint64_t top=atomic_load_explicit(&p->allocated_top,memory_order_acquire);
    if(top<p->start||top>p->limit||(top&15)||!p->forward_table||(p->forward_table&7)||
       !p->starts||(p->starts&7)||p->starts_words<(p->limit-p->start+1023)/1024||
       p->starts_words>UINT64_MAX/8||p->starts>UINT64_MAX-p->starts_words*8)return GC_RO_INVALID;
    ForwardTable *table=(ForwardTable *)(uintptr_t)p->forward_table;
    if(!table->cells||(table->cells&7)||!table->capacity||table->capacity>UINT32_MAX||
       table->cells>UINT64_MAX-table->capacity*sizeof(ForwardCell))return GC_RO_INVALID;
    uint64_t used=atomic_load_explicit(&table->used,memory_order_acquire);
    if(used>table->capacity)return GC_RO_INVALID;
    uint64_t slot=job->offset;if(slot<p->start+32||slot>=top)return GC_RO_MISSING;
    uint64_t lo=0,hi=used;ForwardCell *cells=(ForwardCell *)(uintptr_t)table->cells;
    while(lo<hi){uint64_t mid=lo+((hi-lo)>>1);if(cells[mid].source_offset<=slot)lo=mid+1;else hi=mid;}
    if(!lo)return GC_RO_MISSING;
    ForwardCell *cell=&cells[lo-1];uint64_t owner=cell->source_offset;
    if(owner<p->start+32||owner>slot||(owner&15)||cell->incarnation!=p->incarnation)return GC_RO_INVALID;
    if(lo<used){ForwardCell *next=&cells[lo];
        if(next->incarnation!=p->incarnation||(next->source_offset&15)||next->source_offset<=owner+32||next->source_offset>=p->limit)return GC_RO_INVALID;
        if(slot>=next->source_offset-32)return GC_RO_MISSING;
    }
    uint64_t granule=(owner-p->start)/16;
    _Atomic uint64_t *starts=(_Atomic uint64_t *)(uintptr_t)p->starts;
    if(!(atomic_load_explicit(&starts[granule/64],memory_order_acquire)&(UINT64_C(1)<<(granule%64))))return GC_RO_INVALID;
    q->owner=owner;q->byte_offset=slot-owner;q->cell=(uintptr_t)cell;return GC_RO_OK;
}
#endif
