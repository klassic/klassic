/* Facade-owned weak inventory. The common runtime mutex serializes every
 * entry/phase/publication; the sole collector owns per-entry scan controls. */
#ifndef KLASSIC_GEN_WEAK_INTERNAL_H
#define KLASSIC_GEN_WEAK_INTERNAL_H
#include "klassic_gen_runtime_internal.h"
#include "klassic_gen_weak.h"
#include "gc_weak_relocation.h"
#include "gc_weak_follow.h"
#include "gc_weak_incoming.h"
void kg_weak_initialize_native(KgRuntime *,const MutatorContext *);
void kg_weak_drop_all(KgRuntime *);
void kg_weak_dispose_native(KgRuntime *,const MutatorContext *);
uint64_t kg_weak_mark_locked(KgRuntime *,unsigned,uint64_t);
bool kg_weak_phase_frozen_locked(KgRuntime *);
uint64_t kg_weak_scan_step(KgRuntime *,MutatorContext *);
uint64_t kg_weak_move_begin_locked(KgRuntime *,unsigned);
bool kg_weak_rebind_source(KgRuntime *,KgRun *);
void kg_weak_retire_begin_locked(KgRuntime *,unsigned);
bool kg_weak_incoming_source_locked(KgRuntime *,KgRun *);
void kg_weak_cycle_finish_locked(KgRuntime *,unsigned);
#endif
