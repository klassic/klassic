#include <stdint.h>
#include <stdio.h>
#define M0 (1ULL<<60)
#define COLOR_MASK (7ULL<<60)
uint64_t gc_bad_mask, gc_strip_mask=~COLOR_MASK, gc_good_color;
uint64_t from_addr, to_addr;      /* evacuation: from -> to */
uint64_t gc_load_barrier_slow(uint64_t value, uint64_t* slot){
    uint64_t raw = value & gc_strip_mask;
    if (raw == from_addr) raw = to_addr;   /* follow forwarding */
    *slot = raw | gc_good_color;
    return raw;
}
extern uint64_t two_loads(uint64_t* slot);   /* from .ll: loads slot twice via barrier, returns 2nd */
int main(void){
    from_addr=0x100000; to_addr=0x200000;
    gc_good_color=M0; gc_bad_mask=COLOR_MASK;   /* poison: every load slow-paths (relocate-like) */
    uint64_t slot = from_addr | M0;             /* good-colored but poison=all bad */
    uint64_t second = two_loads(&slot);
    printf("2nd load raw=%llx (moving-correct=to_addr=%llx, STALE=from_addr=%llx)\n",
           (unsigned long long)second,(unsigned long long)to_addr,(unsigned long long)from_addr);
    printf("%s\n", second==to_addr ? "MOVING-SAFE" : "UAF-HAZARD (CSE returned stale from-space)");
    return second==to_addr?0:1;
}
