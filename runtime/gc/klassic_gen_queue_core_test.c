/* Keep generic array/claimed FOUND semantics alongside the driver's opt-in
 * frozen FIFO proof. These actual native fixtures cover imported record
 * lifetimes, queue/spill leases, partial receivers, claims and retirement. */
#define _GNU_SOURCE
#include "gc_native_allocation_retire_cases.h"
#include "gc_native_graph_cases.h"
int main(void){
    gc_run_native_allocation_cases(gc_try_allocate_start);
    gc_run_native_cycle_cases(gc_cycle_mark_start_locked);
    gc_run_native_allocation_retire_cases(gc_try_allocate_start);
    gc_run_native_graph_cases(&(NativeGraphOps){gc_try_allocate_start,gc_cycle_mark_start_locked,
        gc_pending_mark,gc_mark_scan_slice,gc_mark_scan_commit_locked});
    puts("C queue core: actual source retirement, sticky claimed FOUND, scans, spills and imported arenas PASS");
    return 0;
}
