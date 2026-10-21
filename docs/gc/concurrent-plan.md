# M-G6 plan: concurrent relocation and marking for `--gc=generational`

Status: steps 1-3 implemented (2026-10-22); steps 4-6 open. Builds on M-G1..M-G5
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

Still open: concurrent marking (step 4), a GC worker thread (step 5), and the
PE / Mach-O ports (step 6). Relocation is incremental on the mutator thread,
not concurrent on another core.
