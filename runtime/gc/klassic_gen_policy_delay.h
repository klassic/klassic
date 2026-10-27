/* Private implementation included once by klassic_gen_policy.c. An observed
 * FREE page has already completed all source/color retirement. This policy
 * owns backing only: it never publishes page states or invents a PageLease. */
#ifndef KLASSIC_GEN_POLICY_DELAY_H
#define KLASSIC_GEN_POLICY_DELAY_H
#include <time.h>
static inline uint64_t kg_policy_ticks_to_ns(uint64_t ticks,uint64_t frequency){
    if(!ticks||!frequency||ticks/frequency>UINT64_MAX/UINT64_C(1000000000)||
       ticks%frequency>UINT64_MAX/UINT64_C(1000000000))return 0;
    uint64_t seconds=ticks/frequency*UINT64_C(1000000000);
    uint64_t fraction=(ticks%frequency)*UINT64_C(1000000000)/frequency;
    return fraction>UINT64_MAX-seconds?0:seconds+fraction;
}
uint64_t kg_policy_clock(void){
#ifdef _WIN32
    LARGE_INTEGER frequency,ticks;
    if(!QueryPerformanceFrequency(&frequency)||frequency.QuadPart<=0||
       !QueryPerformanceCounter(&ticks)||ticks.QuadPart<=0)return 0;
    return kg_policy_ticks_to_ns((uint64_t)ticks.QuadPart,(uint64_t)frequency.QuadPart);
#else
    struct timespec now;
    if(clock_gettime(CLOCK_MONOTONIC,&now)||now.tv_sec<0||now.tv_nsec<0||now.tv_nsec>=1000000000||
       (uint64_t)now.tv_sec>UINT64_MAX/UINT64_C(1000000000))return 0;
    uint64_t seconds=(uint64_t)now.tv_sec*UINT64_C(1000000000);
    if((uint64_t)now.tv_nsec>UINT64_MAX-seconds)return 0;
    return seconds+(uint64_t)now.tv_nsec;
#endif
}
void kg_policy_poll(KgRuntime *r,const MutatorContext *actor,uint64_t now){
    if(!r->uncommit_delay_ns||!now)return;
    if(!kg_policy_native(actor)||!r->policy||actor!=kg_driver_cleanup_actor(r))kg_fail("idle uncommit native owner",0);
    KgPolicy *p=kg_policy(r);HeapPool *heap=&r->native.backing.heap;
    uint64_t selected=UINT64_MAX;
    kg_lock(r);
    if(p->closing){kg_unlock(r);return;}
    AllocationPage *pages=(void *)(uintptr_t)heap->pages;
    /* At most 64 metadata candidates and one physical page per quantum. */
    for(unsigned budget=0;budget<64;budget++){
        uint64_t i=p->scan_cursor++;if(p->scan_cursor==p->pages)p->scan_cursor=0;
        unsigned char state=p->states[i];AllocationPage *page=&pages[i];
        KgFreeObservation *seen=&kg_policy_times(p)[i];
        if((state&(KG_BACKED|KG_CACHED|KG_DROPPING))!=(KG_BACKED|KG_CACHED)||
           atomic_load_explicit(&page->state,memory_order_acquire)!=GC_PAGE_FREE||
           page->owner||page->metadata_leases||page->starts||page->forward_table){
            p->states[i]&=(unsigned char)~KG_OBSERVED;*seen=(KgFreeObservation){0};continue;
        }
        /* A reserve/abort can complete before before_prepare sees the lease.
         * Real incarnation identity therefore closes that reuse window too. */
        if(!(state&KG_OBSERVED)||seen->incarnation!=page->incarnation){
            *seen=(KgFreeObservation){now,page->incarnation};p->states[i]|=KG_OBSERVED;p->observations++;continue;
        }
        if(now<seen->at){seen->at=now;p->clock_resets++;continue;}
        if(now-seen->at<r->uncommit_delay_ns||
           p->committed-p->dropping<GC_HEAP_PAGE_BYTES||
           p->committed-p->dropping-GC_HEAP_PAGE_BYTES<p->floor)continue;
        /* Same-lock FREE/cache/incarnation authentication and the backing
         * claim precede unlock. Pool reserve may follow, but every preparation
         * waits for SWEEPING before its first payload map/zero/access. Cleanup
         * and all other OS services serialize through this worker busy owner. */
        p->states[i]=KG_BACKED|KG_DROPPING|KG_SWEEPING;
        p->dropping+=GC_HEAP_PAGE_BYTES;selected=i;break;
    }
    kg_unlock(r);
    if(selected==UINT64_MAX)return;
    kg_policy_os_enter(r,actor);uint64_t failures=0;
    NativePageDecommitter decommit=r->idle_decommit?r->idle_decommit:gc_native_heap_decommit_range;
    while(!decommit(kg_policy_address(r,selected),GC_HEAP_PAGE_BYTES))
        kg_os_retry(&failures,"idle uncommit OS retry limit; backing and sweep claim remain owned");
    kg_policy_os_leave(r);
    kg_lock(r);
    if(p->states[selected]!=(KG_BACKED|KG_DROPPING|KG_SWEEPING)||p->dropping<GC_HEAP_PAGE_BYTES||p->committed<GC_HEAP_PAGE_BYTES)
        kg_fail("idle uncommit backing receipt",selected);
    p->states[selected]=0;kg_policy_times(p)[selected]=(KgFreeObservation){0};
    p->dropping-=GC_HEAP_PAGE_BYTES;p->committed-=GC_HEAP_PAGE_BYTES;p->idle_uncommits++;
    kg_notify(r);kg_unlock(r);
}
#endif
