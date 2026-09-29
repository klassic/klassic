# Runtime root marking and nonempty cycle — 2026-09-29

`gc_native_root_mark.h` binds a runtime's selected marking audit and mapped queue
to the existing pending-reference marking protocol. One stable, self-bound
controller captures a leased root-walk item, retains it through follow/access,
route, guard exit, queue offer and cleanup, then permits ACK only after a valid
handoff. Cancellation retains the root item for retry. Forwarded-address repair
uses exact CAS and preserves the observed colors. It is not color retirement.
The entry separates lock-bound operations from raw guarded operations; it does
not poll, scan children or schedule OS workers. Worker registration/lifetime and
lock serialization remain caller obligations.

The runtime-backed fixture allocates thirteen scalar objects and one 35-slot
reference object. The parent includes sharing, a self-reference and nulls. Only
the parent remains in the registered roots at cycle start. Actual native heap,
descriptors, root journal, page leases and logical records come from startup and
owned allocation, rather than stack stand-ins for those inventories.

Root cancellation/retry preserves the captured item. Premature ACK and copied
controllers are rejected. After root handoff, the queue still prevents mark-end
and returns the cycle to ConcurrentMark. Two ordinary task slots force counted
overflow; scans inspect at most three slots per call. All fourteen objects are
marked in twenty-five scan slices, including duplicate/self-edge handling.
Children are handed off before each scan receipt advances the claim; every
metadata lease returns after task completion.

The last root is then released during marking. Its actual deletion journal is
claimed and both sides are processed before ACK. A fresh root audit permits
Select. Selection retains the run; this fixture does not move live objects.
After Retire adoption, the now unreachable graph is one wholly dead run: there
are no external roots, other runs, weak/native handles, private jobs or actors,
and every remaining payload edge is internal to that run. This explicit fixture
proof permits owned payload uncommit, physical metadata retirement and logical
record reclamation. Root/retirement audits, final Idle adoption, ticket completion
and runtime disposal follow. Budgets return to the bootstrap baseline before
shutdown. An empty queue alone is never used as a whole-range death proof.

The C implementation and generated x86-64 allocation, cycle START, pending mark,
scan and scan-commit leaves run the same fixture. The new outer controller remains
C only.

Validation on Linux x86-64 WSL2:

- Focused fixture with strict clang-15, ASan and UBSan: passed.
- `cargo test -p klassic-native gc_atomic_`: 10 passed, 53.47 s.
- Mandatory C ASan/UBSan and TSan: passed, 180.75 s.
- Strict C syntax, formatting, diff and inventory checks: passed; 27,781
  candidate access sites across 449 files.
- Logs: `/tmp/klassic-native-graph-native.log` and
  `/tmp/klassic-native-graph-sanitizers.log`. Full workspace and actual Windows/
  Darwin execution were not rerun for this C-private connection.

The ordinary compiler still uses the legacy collector. This revision does not
establish moving/promotion overlap, arbitrary graph retirement proofs, concurrent
OS worker execution, adaptive policy, generated outer controllers, cross-OS
execution or the remaining full G01-G15 completion gates.
