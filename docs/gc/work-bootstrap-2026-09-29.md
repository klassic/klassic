# Initial work storage ownership (2026-09-29)

`gc_native_work.h` provides the initial mutator's SATB and remembered rings,
metadata lease pool and two generation-specific mark queues in one native
mapping. Explicit capacities are required in `NativeRuntimeConfig.work`; zero
and counts beyond the shared table limits are rejected before allocation.
All arrays and controls are included in a single OS-rounded metadata charge.

Runtime startup admits that charge before mapping root/descriptor/work packages.
Preparation initializes the stable free lists outside locks and in native state.
Queue epoch 1 creates valid empty queues; it does not begin collection. Work
storage is committed before the private context's initially empty buffer cursors
are bound. Activation then publishes the root package as before.

Runtime disposal requires external quiescence and no new admission. It rejects
unpublished/unconsumed buffer entries, active consumer controls, active metadata
leases or either nonempty generation queue. Failed work unmap leaves its mapping
and budget retained for retry; the runtime is already unavailable. Successful
unmap clears context buffer addresses before detach. Work budget joins the same
final accounting checks as heap-table, root and descriptor storage.

The bootstrap fixture now covers thirteen lifecycles, including work-map failure,
work-budget failure before package mapping, work-unmap retry and all earlier
heap/root/descriptor failures. Six invalid capacity configurations preserve size
outputs. Both actual rings fill and use registered CLAIM/COPY/ACK; pending work
blocks disposal. Those ring boundary tests intentionally carry empty entries and
do not claim coverage of a managed store barrier.

Owned allocation initialization now acquires leases from the mapped runtime
pool. The real copy/record-retirement fixture also uses that pool and the actual
Young/Old startup queues for OFFER/TAKE/FINISH and final lease return. These are
live job/record lifetimes through complete runtime disposal, not only allocation
or geometry checks.

Remset/pin inventories, capacity growth, ordinary barrier/cycle routing, OS worker
launch and generated ownership controllers remain required. Default programs
still use the legacy collector. This C startup connection is not G01-G15
completion or cross-platform execution evidence.

Validation on Linux x86-64 WSL2:

- Focused Clang 15 strict-warning ASan/UBSan execution: passed.
- `cargo test -p klassic-native gc_atomic_`: 10 passed, 0 failed/ignored,
  51.69 s.
- `cargo test --test llvm_gc c_gc_passes_under_sanitizers`: passed, 179.57 s;
  the work-bootstrap marker is required in both C sanitizer suites.
- Strict C syntax, formatting, diff and access-inventory checks passed;
  27,508 candidate sites / 444 files.

Logs: `/tmp/klassic-work-bootstrap-native.log` and
`/tmp/klassic-work-bootstrap-sanitizers.log`. These are targeted results after
the earlier full workspace sweep, not a new full workspace or actual
Windows/Darwin execution claim.
