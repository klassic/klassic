# Root-domain absence gate — 2026-09-29

`gc_native_root_incoming.h` observes the runtime root inventory with a kind-1
reader lease on its current ACTIVE or RETIRING segment. Each NEXT reads one
atomic root cell. A target reference returns FOUND without advancing, so later
canonicalization/removal can be rechecked. The observation is diagnostic and
never authorizes managed payload access. It neither grants marking credit nor
ACKs marking, journal or deletion jobs.

Before leaving a segment, its publication journal and outstanding deletion claim
must be drained. At the end, all root reader/job leases and walkers must finish,
covering snapshots held by other consumers. The observer's own lease prevents
CLEAN from unlinking or clearing its current segment. Advancing pins the next
segment before releasing the previous one; cancellation releases its cursor
even after phase invalidation. A new scope or changed phase forces restart.

This remains an absence proof under the cycle's no-new-target-reference
protocol. Atomic cell reads do not create a snapshot or enforce that protocol.
Unrelated pending root work can conservatively delay the audit; no root work is
drained under a global pause by this helper. Each call is bounded and registry
serialized, and it does not wait or access managed payloads.

The owned source-retirement receipt now requires scan-control, both-queue and
root-domain completion. Root observation starts lazily after the earlier
domains succeed, so a queue/scan finding does not pin roots unnecessarily.
Receipt cancellation releases both queue metadata and root-reader cursors.
All existing owned allocation/copy/graph retirement fixtures use the new gate.

The real graph fixture verifies a live root finding, cancellation with reader
count return, root removal while the old value remains in the journal, pending
reader exclusion, and retiring-segment protection. It executes a genuine
deletion CLAIM/scan/ACK while the observer is live: the claim blocks absence,
then the observer blocks CLEAN until it releases its lease. Copied controls and
stale scope evidence are rejected. The graph still reaches actual owned
retirement and budget return.

Validation on Linux x86-64 WSL2:

- Focused graph/allocation/copied-record fixtures, strict clang-15 with ASan
  and UBSan: passed.
- `cargo test -p klassic-native gc_atomic_`: 10 passed, 51.41 s.
- Mandatory C ASan/UBSan and TSan: passed, 181.06 s.
- Strict C syntax, formatting, diff and inventory checks passed; 28,184
  candidate access sites across 453 files.
- Logs: `/tmp/klassic-root-incoming-native.log` and
  `/tmp/klassic-root-incoming-sanitizers.log`. Full workspace and actual
  Windows/Darwin execution were not rerun for this C-private connection.

These C-private
controllers participate in C/generated x86-64 lifecycle fixtures. General
heap/buffer/handle/forwarding proofs, worker services, moving lifecycle coverage,
default compiler migration and actual cross-OS/performance completion remain
open under the full G01-G15 objective.
