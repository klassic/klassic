/* Nonmoving forwarding entry, initialized before source selection and kept
 * alive by the separate source/color/metadata retirement protocol. The immutable
 * key is (source offset, page incarnation); GcRef alone cannot supply the latter.
 *
 * word=0 is empty; (ticket<<2)|1 is claimed; destination_ref|2 is published;
 * word=3 is retained. Legal GcRef low bits 0..1 are zero. The single atomic
 * word prevents torn publication of status and destination. Tickets are unique
 * nonzero 62-bit claim-attempt identities and are never replayed. No live entry
 * is reset, no published forwarding is undone, and no wait/loop occurs here.
 * Frozen-publication compression may later shorten a published destination
 * to the same canonical object; it never clears or reclaims this cell.
 *
 * CLAIM does not freeze the source. gc_source_access SELECT/CHECK must close
 * admission and drain every actor before copying. PUBLISH/RETAIN additionally
 * require a closed zero-count gate; this cannot detect unguarded actors. Only the unique winner
 * may PUBLISH after its private destination is fully initialized, preserves the
 * logical MarkRecord, and is protected from reclamation. RETAIN is legal only
 * after abandoning/cleaning its unpublished copy. Losers clean their own private
 * candidates and read/assist elsewhere. These are caller obligations, not
 * proofs performed by this publication leaf. READ never dereferences the heap.
 */
#ifndef KLASSIC_GC_FORWARD_H
#define KLASSIC_GC_FORWARD_H
#include "gc_abi.h"
#define GC_FWD_CLAIM 0u
#define GC_FWD_READ 1u
#define GC_FWD_PUBLISH 2u
#define GC_FWD_RETAIN 3u
#define GC_FWD_OK 0u
#define GC_FWD_INVALID 1u
#define GC_FWD_EMPTY 2u
#define GC_FWD_BUSY 3u
#define GC_FWD_RETAINED 4u
#define GC_FWD_PUBLISHED 5u
static inline bool gc_forward_destination(uint64_t ref,uint64_t source) {
    GcRef encoded;return (ref>>16)!=source&&gc_abi_encode_ref(ref>>16,(uint16_t)ref,&encoded);
}
static inline uint64_t gc_forward_step(ForwardCell *g,ForwardRequest *q,uint64_t op) {
    if(op>GC_FWD_RETAIN||!g->source_offset||(g->source_offset&15)||g->source_offset>GC_MAX_OFFSET||!g->incarnation||
        q->source_offset!=g->source_offset||q->incarnation!=g->incarnation)return GC_FWD_INVALID;
    if(op==GC_FWD_READ) {if(q->ticket||q->destination)return GC_FWD_INVALID;}
    else if(!q->ticket||q->ticket>(UINT64_MAX>>2)||(op!=GC_FWD_PUBLISH&&q->destination))return GC_FWD_INVALID;
    if(op==GC_FWD_PUBLISH&&!gc_forward_destination(q->destination,g->source_offset))return GC_FWD_INVALID;
    if(op==GC_FWD_PUBLISH||op==GC_FWD_RETAIN) {
        if(atomic_load_explicit(&g->access,memory_order_acquire)!=GC_SOURCE_CLOSED)return GC_FWD_INVALID;
        uint64_t expected=(q->ticket<<2)|1;
        uint64_t desired=op==GC_FWD_PUBLISH?q->destination|2:3;
        return atomic_compare_exchange_strong_explicit(&g->word,&expected,desired,memory_order_acq_rel,memory_order_acquire)?GC_FWD_OK:GC_FWD_INVALID;
    }
    uint64_t word;
    if(op==GC_FWD_CLAIM) {
        word=0;uint64_t desired=(q->ticket<<2)|1;
        if(atomic_compare_exchange_strong_explicit(&g->word,&word,desired,memory_order_acq_rel,memory_order_acquire))return GC_FWD_OK;
    } else word=atomic_load_explicit(&g->word,memory_order_acquire);
    if(!word)return GC_FWD_EMPTY;
    if((word&3)==1)return word>>2?GC_FWD_BUSY:GC_FWD_INVALID;
    if(word==3)return GC_FWD_RETAINED;
    uint64_t destination=word&~UINT64_C(3);
    if((word&3)!=2||!gc_forward_destination(destination,g->source_offset))return GC_FWD_INVALID;
    if(op==GC_FWD_CLAIM)return GC_FWD_PUBLISHED;
    q->destination=destination;return GC_FWD_OK;
}
#endif
