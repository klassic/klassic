#define _GNU_SOURCE
#include "gc_roots_concurrency.h"
#include "gc_root_lifetime_concurrency.h"
#include "gc_root_walk_concurrency.h"
#include "gc_root_walk_heal_cases.h"
#include "gc_root_walk_item_cases.h"
#include "gc_root_detach_concurrency.h"
#include "gc_detach_cases.h"
#include "gc_buffers_cases.h"
#include "gc_buffers_concurrency.h"
#include <unistd.h>
int main(void) {
    alarm(40);
    gc_run_root_walk_item_cases(gc_root_walk_item,gc_root_walk_locked,gc_root_walk_scan);
    gc_run_root_walk_heal_cases(gc_root_walk_heal);
    gc_run_root_walk_heal_races(gc_root_walk_heal,gc_root_walk_locked,gc_root_walk_scan,gc_root_life_locked);
    gc_run_buffer_cases(gc_barrier_buffer_step,gc_barrier_buffer_drain);
    gc_run_buffer_races(gc_barrier_buffer_step,gc_barrier_buffer_drain);
    gc_run_detach_cases(gc_detach_step_locked);
    gc_run_root_detach_races(gc_root_life_locked, gc_root_clear_chunk, gc_root_scan_leased, gc_handshake_step_locked, gc_root_drain, gc_detach_step_locked, gc_barrier_buffer_step, gc_barrier_buffer_drain);
    gc_run_root_walk_races(gc_root_walk_locked, gc_root_walk_scan, gc_root_life_locked, gc_root_clear_chunk, gc_root_scan_leased);
    gc_run_root_walk_cases(gc_root_walk_locked, gc_root_walk_scan, gc_root_life_locked, gc_root_clear_chunk, gc_root_scan_leased);
    gc_run_root_lifetime_cases(gc_root_life_locked, gc_root_clear_chunk, gc_root_scan_leased);
    gc_run_root_lifetime_races(gc_root_life_locked, gc_root_clear_chunk, gc_root_scan_leased);
    gc_run_root_cases(gc_root_step, gc_root_scan, gc_root_drain);
    gc_run_root_races(gc_root_step, gc_root_scan, gc_root_drain);
    return 0;
}
