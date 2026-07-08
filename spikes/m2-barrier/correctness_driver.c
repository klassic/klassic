#include <stdint.h>
#include <stdio.h>
#define M0 (1ULL<<60)
#define M1 (1ULL<<61)
#define R  (1ULL<<62)
#define COLOR_MASK (7ULL<<60)
uint64_t gc_bad_mask;
uint64_t gc_strip_mask = ~COLOR_MASK;
uint64_t gc_good_color;
long slow_calls;
uint64_t gc_load_barrier_slow(uint64_t value, uint64_t* slot) {
    slow_calls++;
    uint64_t raw = value & gc_strip_mask;
    *slot = raw | gc_good_color;      /* self-heal to good color */
    return raw;
}
extern uint64_t barrier_load(uint64_t* slot);
int main(void) {
    uint64_t obj = 0xdead;
    uint64_t A = (uint64_t)&obj;      /* canonical: bits 60-62 clear */
    /* Case 1: incremental heal. good=M0, bad catches M1|R. slot starts M1(bad). */
    gc_good_color = M0; gc_bad_mask = M1 | R; slow_calls = 0;
    uint64_t slot = A | M1;
    for (int i = 0; i < 3; i++) {
        uint64_t raw = barrier_load(&slot);
        if (raw != A) { printf("FAIL c1: raw=%llx A=%llx\n",(unsigned long long)raw,(unsigned long long)A); return 1; }
    }
    printf("case1 slow_calls=%ld (want 1: self-heal stuck, no CSE across call) slot_good=%d\n",
           slow_calls, (slot & COLOR_MASK) == M0);
    /* Case 2: poison. bad=COLOR_MASK -> every colored load slow-paths. */
    gc_good_color = M0; gc_bad_mask = COLOR_MASK; slow_calls = 0;
    slot = A | M0;
    for (int i = 0; i < 3; i++) {
        uint64_t raw = barrier_load(&slot);
        if (raw != A) { printf("FAIL c2 poison\n"); return 1; }
    }
    printf("case2 poison slow_calls=%ld (want 3: no fast-path hoist)\n", slow_calls);
    printf("ALL RAW POINTERS CORRECT\n");
    return 0;
}
