# M-G6 plan: concurrent relocation and marking for `--gc=generational`

Status: steps 1-6 implemented (2026-10-22); the worker (steps 4-5) runs on
Linux x86-64 only. See "Lock-free copying and concurrent marking" at the end
for what is concurrent and what still pauses or locks. Builds on M-G1..M-G5
(`production-integration-plan.md`), where every collection is stop-the-world
semispace copying.

## Goal

Move the expensive work of a Young/full collection out of the pause:
the pause only flips spaces and relocates roots' immediate referents; the
remaining relocation happens lazily through load barriers and incrementally in
allocation-driven quanta (and later on a worker thread), as in ZGC.

## Facts the design rests on

- Every managed load already has a funnel:
  - frame slots: `root_load` (READ_RAW + decode);
  - heap fields read as references: `field_load` (called with the field
    address in R10 by `compile_gc_read_qword`);
  - heap fields read as scalars (generic payloads): `compile_gc_read` plain
    load. This is the hole: it must get the same barrier, using the
    conservative heap-range rule already used by the Cheney scan.
- Every managed store has a funnel: `root_store`, `field_store` (write barrier
  already present for the remembered set), plain scalar `__gc_write`.
- Objects are immutable after construction except through `__gc_write`, so
  SATB is needed only for writes that overwrite a reference (none today; the
  barrier hook exists).
- Forwarding is already recorded in the dead source header
  (`descriptor = FORWARDED`, `mark_record = new reference`).

## Steps

1. Load barrier on every heap reference load: if the address is in the
   from-space (Young semispace being evacuated, or the Old half in a full
   cycle), forward it (copy if needed) and self-heal the loaded location
   (root cell or field). Scalar generic reads take the same path via the
   range rule.
2. Split `collect` into a pause (flip, forward root cells, literal cells and
   remset referents) and a relocation phase that remains open after the
   pause. From-space pages are recycled only when the phase ends.
3. Relocation quanta: each allocation scans a bounded number of to-space /
   promoted objects (the Cheney cursor), so the phase terminates; the next
   flip first finishes any open phase (bounded fallback pause, counted).
4. Marking: replace "copy everything reachable" with a concurrent mark of
   the from-space using the existing MarkRecord epochs, so relocation only
   touches live objects and page selection can skip dense pages.
5. Worker: run steps 3-4 on a GC thread with the existing handshake /
   safepoint emitters (`gc_codegen::handshake`, `threads`), mutator polls at
   allocation and loop back-edges.
6. Ports: PE reuses the x86-64 emission (Windows mapping leaf); Mach-O via
   the AArch64 `PortableAsm` adapter, which the production routines already
   target.

Each step keeps the stress-default `cli_smoke` run (temporary local hook)
free of correctness failures and the default path byte-identical.

## Steps 1-3 as implemented

- The pause now ends after flipping, forwarding root cells, remembered
  fields and collection-literal cells (shallow copies). A relocation phase
  stays open: each allocation scans `QUANTUM_OBJECTS` (64) to-space/promoted
  objects; when both Cheney cursors catch up, `finish` recycles from-space.
  The next collection completes an open phase first.
- Load barrier: `field_load` (reference reads, field address in R10) and the
  scalar `__gc_read` funnel. A loaded word inside this collection's
  from-space ranges is relocated and the field healed (`--gc-log healed=`).
- Forwarding is gated by the from-space ranges, so to-space objects reached
  through healed fields or new allocations are never copied twice (this was
  the cause of a published-after-forwarding crash under stress).
- `gc_deep_equal` follows forwarding records for evacuated operands.
- Evidence: gen tests 16/16; stress-default and plain-default `cli_smoke`
  466 passing each, with every failure an intended rejection or a legacy
  `--gc-log` format check.

Step 6 (PE): done. `--target x86_64-pc-windows-msvc --gc=generational` maps
the runtime through a realigned VirtualAlloc leaf and writes heap strings
through the Win64 write shim; the generational test runs the .exe under WSL
interop when available (stress, full collections and nested enums matched).

Step 6 (Mach-O): done. `--target aarch64-apple-darwin --gc=generational`
emits the same routines through the AArch64 `PortableAsm` adapter, wrapped in
`FlagAsm` (`gc_codegen/macos_generational.rs`): the routines were written for
x86, where `sub`/`and`/`or`/shifts set the flags a following `jcc` reads, and
AArch64's plain forms do not. Roots differ from x86 on purpose: this backend
already roots frame slots by address on its shadow stack (the legacy collector
moves objects too), so the generational collector reaches them through the
backend `literals` hook, which forwards every shadow-stack slot in place;
`root_adopt`/`root_load` are unused there. The legacy `gc_alloc` /
`gc_store_pointer` / `gc_deep_equal` labels become wrappers that keep x1-x12
around the generational routines. `FileInput` reads into a native mapping
instead of a 1 MiB heap buffer (larger than a page). Heap strings are still
written to the kernel straight from the heap (no copy-out buffer as on x86).
Evidence is the unicorn emulator (`tools/run-macho-aarch64.py`), not arm64
hardware; the arm64 CI runner remains the authority.

## Step 5 as implemented (Linux x86-64 only)

- Thread: `startup` calls the backend `spawn` leaf: `clone(CLONE_VM | FS |
  FILES | SIGHAND | THREAD | SYSVSEM)` onto a 256 KiB mapped stack. The child
  jumps to the worker loop (`production/worker.rs`). Every exit becomes
  `exit_group`, so a parked worker never keeps the process alive. If `clone`
  fails, `worker_alive` stays 0 and the allocation quanta of steps 1-3 run.
- Work: a pause (still on the mutator, under the lock) bumps a futex word and
  wakes the worker after `alloc` releases the lock. The worker repeatedly takes
  the lock, runs `cheney` with a 64-object budget, counts the objects into
  `--gc-log worker_scanned=`, and releases the lock. When both cursors catch up
  it sets `worker_done` and parks. With a live worker the mutator does no
  scanning in `quantum`; it only finishes a phase the worker reported scanned,
  at its next allocation (the safepoint). Only then is from-space recycled.
- Synchronisation, honestly: this is coarse-grained. The collector state the
  copy path uses (the shared TLAB through `alloc_raw`, the scratch root and
  root journal, publication, forwarding records, the remembered set) is shared
  between the two threads, and the mutual exclusion is one fair ticket lock
  (yielding with `sched_yield` every 128 failed polls). The mutator takes it in
  `alloc` and the load-barrier slow path, and in `root_adopt/pop/store/load`,
  the write-barrier remember step, `deep_equal` and the `--gc-log` verify only
  while a phase is open (the mutator alone opens and closes phases, and closes
  them under the lock, so a closed phase means the worker is idle). The worker
  copies through the same TLAB as the mutator, under that lock; it does not
  have its own allocation buffer, and copies are not claimed by a header CAS
  (the lock makes the forwarding check-and-copy atomic). What is lock-free:
  the mutator's own computation, its unbarriered field stores and object
  initialisation, which can race with the worker's scan; the worker therefore
  heals a field with a CAS against the word it read, so a racing mutator store
  always wins, and skips the remembered-set append when its heal lost (the
  mutator's own write barrier covers that store). Root cells are written by
  the worker only through the allocation scratch root (cell 0) inside
  `alloc_raw`, under the lock; frame cells are only touched by the mutator.
- Still pausing: the flip with root/remset/literal forwarding, and a
  `finish` when the next collection starts before the worker caught up (it
  completes the scan on the mutator). On one CPU the lock degrades to
  yield-driven hand-offs.
- Evidence: `tests/gc_generational.rs::generational_worker_thread_relocates_concurrently`
  (plain and `--gc-stress`, 5 runs each, `worker_scanned > 0`, output equal to
  the evaluator). Out of tree: the same program 50 x plain + 50 x stress, and
  20 stress runs pinned to one CPU (`taskset -c 0`), all correct. With a
  temporary local hook making `--gc=generational` (and, separately, stress)
  the default, `cli_smoke` passed 495/511 and 496/511; every failure was an
  intended rejection (`--gc-poison`, `--backend c`) or a legacy `--gc-log`
  format check. Default builds of `test-programs/` and `examples/` are
  byte-identical to the previous commit for Linux, Windows and AArch64.

The lock-free copy path and concurrent marking below replace the coarse lock
described in this section.

## Lock-free copying and concurrent marking (Linux x86-64 worker)

Copy protocol (`production/copy.rs`):

- Forwarding is one word: the source header's `descriptor_id` becomes the
  copy's reference bits with `0xf0f0` in the color bits. Both copiers copy
  first and then claim the source with a CAS of that word from its original
  id. A loser turns its tentative copy into a raw-bytes filler (published,
  never traced) and takes the winner's reference (`--gc-log worker_lost=`,
  `mutator_lost=`). The load barrier uses the same protocol and heals the
  field with a CAS against the stale word.
- Worker copies go to worker-private to-space pages taken from the top of
  the semispace ("chain B"); the mutator's TLAB takes pages from the bottom
  ("chain A"). Page reservation is a CAS on one packed word (`next_page |
  copy_low << 32`), so neither side takes a lock for a page. The worker
  bump-allocates privately, writes a published header itself (age-1 shared
  mark record), the payload, the descriptor last, then the page's
  `allocated_top`, so `--gc-log` verify sees `objects == published`.
- Promotions reserve Old space with a CAS on `old_top` and write the
  descriptor last; a scanner waits on a reserved slot whose descriptor is
  still zero. Remembered-set appends reserve their slot with a CAS.
- The mutator's own copies still use its TLAB. While one is in flight a flag
  stops the worker's scan of chain A at that (possibly incomplete) object.
- The scan (`production/scan.rs`) walks chain A, chain B and the promoted
  range; it is complete after a pass that scanned nothing.

Marking (`production/mark.rs`), snapshot-at-the-beginning:

- When a quarter of the semispace is in use and no phase is open, a short
  mark-start pause clears the 1-bit-per-granule bitmap and marks root cells
  and collection-literal cells. The worker traces Young and Old in quanta.
  New objects are marked at allocation; `field_store` marks the reference it
  overwrites (SATB slow path, under the lock; no current program overwrites
  a reference, so it is untested by programs).
- The relocation pause finishes the trace on the mutator if the worker left
  work (`mark_drains=`), then evacuation uses the mark: a remembered field
  whose Young referent is unmarked is dropped instead of copied
  (`remset_dropped=`); copying an unmarked object is counted as a violation
  (`unmarked_copied=`, asserted 0 by the tests). Copying stays
  reachability-driven (Cheney), so a full collection never copied
  unreachable objects before either; the mark's gain is remembered-set
  garbage and the invariant check. Marking and relocation never overlap.

What still pauses or locks:

- Pauses: mark start (bitmap clear plus root/literal marking) and the
  relocation pause (finish an open phase, drain marking, flip, forward
  roots, remembered fields and literal cells). The mutator finishes a phase
  (scan to completion and recycle from-space) at its next allocation after
  the worker reports it scanned, or at the next collection.
- One ticket lock remains, but it no longer serialises copying: the worker
  holds it per quantum, and the mutator takes it only to stop the worker
  (both pauses, phase finish, `--gc-log` verify) and on the SATB slow path.
  Allocation, root operations, the load barrier (including copying),
  `field_store` without SATB and `deep_equal` take no lock.
- `--gc-stress` collects before every allocation, so a phase is always open
  and marking never starts under stress; stress exercises the CAS copy path.
- Windows (PE) and Mach-O have no worker: no marking, plain forwarding
  stores, and the allocation quanta of steps 1-3.

Evidence: `generational_worker_marks_concurrently_and_claims_copies_by_cas`
(mark cycles, worker copies, `unmarked_copied == 0`); race loops in the
commit message.
