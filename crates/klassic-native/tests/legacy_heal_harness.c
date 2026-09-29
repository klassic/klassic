#define _GNU_SOURCE
#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <sys/mman.h>
#include "gc_legacy_heal_code.h"
#define M0 (UINT64_C(1)<<60)
#define R (UINT64_C(1)<<62)
int main(void) {
    unsigned char *memory=mmap(NULL,sizeof(code),PROT_READ|PROT_WRITE,MAP_PRIVATE|MAP_ANONYMOUS,-1,0);
    assert(memory!=MAP_FAILED);
    memcpy(memory,code,sizeof(code));
    for(unsigned i=0;i<sizeof(fixups)/sizeof(fixups[0]);i++) {
        uint64_t offset;memcpy(&offset,memory+fixups[i],8);
        offset+=(uintptr_t)memory;memcpy(memory+fixups[i],&offset,8);
    }
    assert(mprotect(memory,DATA_START,PROT_READ|PROT_EXEC)==0);
    uint64_t (*slow)(uint64_t *,uint64_t)=(void *)(memory+SLOW_ENTRY);
    void (*trace)(uint64_t)=(void *)(memory+TRACE_ENTRY);
    void (*drain)(void)=(void *)(memory+DRAIN_ENTRY);
    void (*restart)(void)=(void *)(memory+RESTART_ENTRY);
    void (*store)(uint64_t *,uint64_t)=(void *)(memory+STORE_ENTRY);
    void (*store_race)(uint64_t *,uint64_t)=(void *)(memory+STORE_RACE_ENTRY);
    uint64_t (*allocate)(uint64_t,uint64_t)=(void *)(memory+ALLOC_ENTRY);
    _Alignas(16) uint64_t original[4]={32,1,0,0},canonical[4]={32,1,0,0},other[4]={32,1,0,0};
    for(unsigned round=0;round<128;round++) {
        int forwarded=(round>>1)&1,marking=(round>>2)&1;
        uint64_t raw=(uintptr_t)(original+2),target=forwarded?(uintptr_t)(canonical+2):raw;
        original[0]=forwarded?(target|1):32;
        *(uint64_t *)(memory+PHASE)=marking;
        *(uint64_t *)(memory+MARKED)=0;
        uint64_t observed=raw|M0;
        // The reader already observed 'observed'; a writer changes the slot
        // before the slow path runs. Failure must preserve that later store.
        uint64_t replacement=round&1?((uintptr_t)(other+2)|R):0;
        uint64_t slot=replacement;
        assert(slow(&slot,observed)==target);
        assert(slot==replacement);
        assert(*(uint64_t *)(memory+MARKED)==(marking?target:0));
        slot=observed;
        assert(slow(&slot,observed)==target&&slot==(target|R));
    }
    _Alignas(16) uint64_t owner[4]={32,2,(uintptr_t)(original+2)|M0,(uintptr_t)(other+2)|M0};
    uint64_t queue[16]={(uintptr_t)(owner+2)};
    original[0]=(uintptr_t)(canonical+2)|1;
    *(uint64_t *)(memory+TOP)=1;
    *(uint64_t *)(memory+WORK)=(uintptr_t)queue;
    trace(3);
    assert(!*(uint64_t *)(memory+TOP));
    assert(owner[2]==((uintptr_t)(canonical+2)|R));
    assert(owner[3]==((uintptr_t)(other+2)|R));
    assert(*(uint64_t *)(memory+MARKED)==(uintptr_t)(other+2));
    for(unsigned kind=0;kind<2;kind++)for(unsigned pattern=0;pattern<3;pattern++) {
        _Alignas(16) uint64_t array[2050]={sizeof(array),kind?4:2};
        uint64_t *fields=array+2;
        unsigned start=kind?1:0;
        if(kind)fields[0]=UINT64_MAX;
        for(unsigned i=start;i<2048;i++)fields[i]=pattern==2||(pattern==1&&(i&1))?0:((uintptr_t)(original+2)|M0);
        queue[0]=(uintptr_t)fields;
        *(uint64_t *)(memory+TOP)=1;
        trace(0);
        assert(*(uint64_t *)(memory+TOP)==1&&!*(uint64_t *)(memory+CURSOR));
        trace(1);
        assert(!*(uint64_t *)(memory+TOP));
        assert(*(uint64_t *)(memory+CURSOR)==(uintptr_t)(fields+start));
        assert(*(uint64_t *)(memory+END)==(uintptr_t)(fields+2048));
        unsigned cursor=start,quantum=0;
        const unsigned budgets[]={1,7,31,512};
        while(cursor<2048) {
            unsigned budget=budgets[quantum++%4],limit=cursor+budget;
            if(limit>2048)limit=2048;
            trace(budget);
            for(unsigned i=cursor;i<limit;i++)assert(fields[i]==(pattern==2||(pattern==1&&(i&1))?0:((uintptr_t)(canonical+2)|R)));
            if(limit<2048)assert(fields[limit]==(pattern==2||(pattern==1&&(limit&1))?0:((uintptr_t)(original+2)|M0)));
            assert(*(uint64_t *)(memory+CURSOR)==(limit==2048?0:(uintptr_t)(fields+limit)));
            if(kind)assert(fields[0]==UINT64_MAX);
            cursor=limit;
        }
        assert(!*(uint64_t *)(memory+END));
        for(unsigned i=start;i<2048;i++)fields[i]=(uintptr_t)(original+2)|M0;
        *(uint64_t *)(memory+TOP)=1;
        trace(1); // active cursor, empty queue: drain must not finish early
        drain();
        assert(!*(uint64_t *)(memory+CURSOR)&&!*(uint64_t *)(memory+END));
        for(unsigned i=start;i<2048;i++)assert(fields[i]==((uintptr_t)(canonical+2)|R));
    }
    puts("legacy emitted trace: bounded record/list scans and partial-only drain PASS");
    _Alignas(16) uint64_t pending[2050]={sizeof(pending),2};
    _Alignas(16) uint64_t arena[32]={0};
    for(unsigned i=2;i<2050;i++)pending[i]=(uintptr_t)(original+2)|M0;
    queue[0]=(uintptr_t)(pending+2);
    *(uint64_t *)(memory+TOP)=1;
    *(uint64_t *)(memory+PHASE)=1;
    *(uint64_t *)(memory+HEAP_TOP)=(uintptr_t)arena;
    *(uint64_t *)(memory+HEAP_END)=(uintptr_t)(arena+32);
    *(uint64_t *)(memory+END_CALLS)=0;
    trace(1); // leave only the partial scan, no queued object
    for(unsigned i=0;i<4;i++) {
        *(uint64_t *)(memory+QUANTUM)=8192;
        assert(allocate(16,1)==(uintptr_t)(arena+i*4+2));
        assert(*(uint64_t *)(memory+END_CALLS)==(i==3?1:0));
        assert(!!*(uint64_t *)(memory+CURSOR)==(i!=3));
    }
    for(unsigned i=2;i<2050;i++)assert(pending[i]==((uintptr_t)(canonical+2)|R));
    puts("legacy emitted allocation: partial scans defer MarkEnd until completion PASS");
    // A from-scratch restart discards the old scan before draining rebuilt
    // roots. Empty-root/clear-mark stubs isolate cursor cleanup here; an old
    // cursor must not be dereferenced even when its backing is unavailable.
    *(uint64_t *)(memory+CURSOR)=1;
    *(uint64_t *)(memory+END)=UINT64_MAX;
    *(uint64_t *)(memory+TOP)=1;
    restart();
    assert(!*(uint64_t *)(memory+CURSOR)&&!*(uint64_t *)(memory+END)&&!*(uint64_t *)(memory+TOP));
    for(unsigned marking=0;marking<2;marking++)for(unsigned old=0;old<2;old++)for(unsigned next=0;next<3;next++) {
        original[0]=(uintptr_t)(canonical+2)|1;canonical[0]=other[0]=32;
        *(uint64_t *)(memory+PHASE)=marking;
        *(uint64_t *)(memory+TOP)=0;
        uint64_t slot=old?((uintptr_t)(original+2)|M0):0;
        uint64_t value=next==0?0:next==1?(uintptr_t)(other+2):(uintptr_t)(original+2);
        store(&slot,value);
        uint64_t resolved=marking&&next==2?(uintptr_t)(canonical+2):value;
        assert(slot==(resolved?(resolved|R):0));
        assert(!!(canonical[0]&2)==(marking&&(old||next==2)));
        assert(!!(other[0]&2)==(marking&&next==1));
        assert(*(uint64_t *)(memory+TOP)==(uint64_t)(marking?!!(old||next==2)+(next==1):0));
    }
    puts("legacy emitted store: old/new marking, null clear and canonical publication PASS");
    original[0]=canonical[0]=other[0]=32;
    *(uint64_t *)(memory+PHASE)=1;
    *(uint64_t *)(memory+TOP)=0;
    uint64_t slot=(uintptr_t)(original+2)|M0;
    *(uint64_t *)(memory+RACE_SLOT)=(uintptr_t)&slot;
    *(uint64_t *)(memory+RACE_VALUE)=(uintptr_t)(canonical+2)|R;
    store_race(&slot,(uintptr_t)(other+2));
    assert(slot==((uintptr_t)(other+2)|R));
    assert(*(uint64_t *)(memory+RACE_COUNT)==3);
    assert(*(uint64_t *)(memory+TOP)==3);
    assert((original[0]&2)&&(canonical[0]&2)&&(other[0]&2));
    puts("legacy emitted store: failed CAS marks the competing overwritten value PASS");
    assert(munmap(memory,sizeof(code))==0);
    puts("legacy emitted heal: 128 stale observations and trace repair PASS");
}
