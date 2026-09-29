# Runtime mark scan ownership — 2026-09-29

`gc_native_mark_scan.h` moves claim/scan/child-handoff ownership out of the graph
fixture into a resumable C runtime controller. The worker provides stable,
accounted native control storage before admission. BEGIN refills at most one
overflow entry and takes one claim from the selected runtime queue. ENTER
obtains a separate canonical receiver lease and access guard through the shared
pending-follow protocol. SCAN reads at most the configured budget (maximum 256).
EXIT precedes receiver cleanup, child offering, locking or polling.

The original claim retains its physical metadata. A scan view keeps that claim's
logical record and cursor while using the canonical receiver's reference and
lease. Each captured child remains in the control until its pending mark offer
or skip is successful and cleanup finishes. Cancellation retains the child for
reset/retry. Commit rejects unfinished children, then YIELD returns the claim to
the queue or FINISH enters a separate final-lease cleanup state. Native intervals
can occur between guarded sections without losing pending work.

The worker owner must register these controls for source-retirement discovery:
queue in-flight counts protect termination, but do not enumerate captured child
references. Worker allocation/accounting, OS registration, scheduling and
metadata-capacity progress remain external obligations. In particular, receiver
canonicalization needs additional lease capacity; returning METADATA_FULL with
the claim preserved is not proof of automatic progress under exhausted budgets.

The existing 14-object graph-cycle fixture now uses this controller for all 25
bounded scan slices. It exercises actual lease-slot exhaustion and recovery,
abandonment before reading, child cancellation/reset, early commit rejection,
copied-control rejection, native intervals, queue overflow and terminal replay.
The graph still completes root/journal handoff and owned retirement/shutdown.
C and generated x86-64 pending-marker, scanner and scan-commit leaves are used.
The fixture does not move the graph during these scans; concurrent forwarding
of this new outer controller still needs direct lifecycle coverage.

Validation on Linux x86-64 WSL2:

- Focused graph fixture with strict clang-15, ASan and UBSan: passed.
- `cargo test -p klassic-native gc_atomic_`: 10 passed, 53.01 s.
- Mandatory C ASan/UBSan and TSan: passed, 181.74 s.
- Strict C syntax, formatting, diff and inventory checks passed; inventory has
  27,870 candidate sites across 450 files.
- Logs: `/tmp/klassic-native-scan-native.log` and
  `/tmp/klassic-native-scan-sanitizers.log`. Full workspace and actual Windows/
  Darwin execution were not rerun for this C-private controller revision.

Ordinary programs
still use the legacy collector. General graph/retirement services, live moving
cycles, generated outer controllers, worker scheduling, default compiler
migration and actual cross-OS/performance gates remain incomplete.
