#include <stdint.h>
#include <stdio.h>
#define COLOR_MASK (7ULL<<60)
#define M0 (1ULL<<60)
uint64_t gc_bad_mask, gc_strip_mask=~COLOR_MASK, gc_good_color;
uint64_t from_addr, to_addr; int relocated;
uint64_t gc_load_barrier_slow(uint64_t value, uint64_t* slot){
    uint64_t raw = value & gc_strip_mask;
    if (relocated && raw==from_addr) raw=to_addr;
    *slot = raw | gc_good_color;
    return raw;
}
uint64_t gc_safepoint_alloc(void){ relocated=1; return 0; }  /* evacuates + frees from-space */
extern uint64_t use_value(uint64_t* slot);
int main(void){
    from_addr=0x100000; to_addr=0x200000; relocated=0;
    gc_good_color=M0; gc_bad_mask=COLOR_MASK;
    uint64_t slot = from_addr | M0;
    uint64_t r = use_value(&slot);
    printf("used ptr=%llx  (safe=to_addr=%llx, UAF=from_addr=%llx)  ->  %s\n",
      (unsigned long long)r,(unsigned long long)to_addr,(unsigned long long)from_addr,
      r==to_addr?"MOVING-SAFE":"UAF: stale from-space pointer used after evacuation");
    return r==to_addr?0:1;
}
