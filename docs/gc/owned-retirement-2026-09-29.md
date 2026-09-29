# Owned physical run retirement (2026-09-29)

Historical validation snapshot. Subsequent
[logical record ownership work](logical-record-lifetime-2026-09-29.md) extends
these fixtures through final record reclamation and runtime disposal, replacing
the raw teardown described below. The earlier timings/counts remain historical.

`gc_native_allocation_retire.h` joins the runtime allocation inventory to the
existing SourceRetire transaction. The controller's stable embedded receipt
binds cleanup to the exact allocation run. It requires the caller's complete
whole-range source/reference/color proof and successful payload uncommit;
zero metadata leases alone is insufficient.

BEGIN checks that bitmap/table ownership still matches the run. Native cleanup
unmaps the bitmap and forwarding table outside locks, retaining receipts and
charges on failure. STEP releases their charges and advances the existing
bounded tail/head transaction. Already-freed tails may be reused between steps.
Completion moves the run to a records-retained state: it does not unmap logical
mark records, remove their budget charge, or unlink the runtime inventory node.
Copied objects and pending logical work can outlive the source's physical pages.

Six fixtures allocate through actual runtime bootstrap and mapped metadata,
initialize a scalar object under a guard, copy it in bounded chunks, publish
forwarding and update the registered root. They drain its journal with checked
idle-phase obligations, audit both forwarding tables and uncommit source
payload before metadata retirement. There are no additional actors, heap edges,
queues or handles; this supplies the fixture-specific retirement proof.

One-, two- and three-page sources exercise successful cleanup, failed bitmap
unmap, failed table unmap, retry and exact physical metadata credit. Three-page
cases reuse a freed tail before the head is released. Whole-range recommit then
uses a new incarnation. Guarded destination reads verify the copied scalar and
original logical identity while the source record mapping remains retained.
Copied-owner controls and completed transaction replay are rejected.

This is a C ownership controller; the native harness uses generated x86
allocation with it, not a newly generated retirement controller. The fixture
ends all remaining destination/record lifetimes with explicit test-only OS
teardown. It does not claim production logical-record reclamation, complete
shutdown, a concurrent global retirement proof or default compiler integration.

Validation on Linux x86-64 WSL2:

- Focused Clang 15 ASan/UBSan fixture with strict warnings: passed.
- `cargo test -p klassic-native gc_atomic_`: 10 passed, 0 failed/ignored,
  51.27 s; generated allocation runs with the C ownership controller.
- `cargo test --test llvm_gc c_gc_passes_under_sanitizers`: passed, 179.26 s;
  the new marker is required in both C sanitizer suites.
- Strict C syntax, formatting, diff and inventory checks passed:
  27,365 candidate sites / 442 files.

Logs are `/tmp/klassic-owned-retirement-native.log` and
`/tmp/klassic-owned-retirement-sanitizers.log`. These targeted checks do not
replace the earlier full workspace sweep or actual Windows/Darwin execution.
