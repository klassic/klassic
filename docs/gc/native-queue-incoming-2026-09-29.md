# Both-generation queue incoming audit — 2026-09-29

`gc_native_queue_incoming.h` audits references in both runtime mark queues,
including queued tasks, in-flight slots and logical-record overflow cells.
NEXT checks one ordinary slot or one spill cell; advancing a spill cursor may
also validate and pin its successor. It never scans payloads, allocates backing
or waits. Calls use registry, both generation and heap locks outside raw guards.

Ordinary task arrays remain owned by the runtime. A spill cursor instead holds
a distinct metadata lease which retains its logical record across calls. A
successor is pinned before releasing the current cursor. Capacity exhaustion
returns WAIT with the original cursor protected. CANCEL releases that protection
even after phase/queue invalidation and is required before dropping an unfinished
audit. No audit reference is treated as an independent strong root.

Epoch, admission serial, task storage/capacity and overflow-count changes force
restart. Tracking overflow count is essential: REFILL moves a spill into an
already-visited ordinary slot without changing the admission serial. Completed
audits are revalidated too. Ordinary TAKE/FINISH can proceed; a finding stays on
its slot until rechecked. DONE clears the prior diagnostic finding.

The owned source-retirement receipt now requires **both** the registered scan
control audit and this two-generation queue audit. It cannot admit metadata
retirement with a queued finding, an unfinished spill cursor or stale queue
evidence. Other incoming domains, no-new-target-reference exclusion, worker
ownership handoff, payload-uncommit authorization and the combined retirement
proof remain external obligations. Metadata capacity expansion/progress is also
not established merely by preserving a cursor on WAIT.

The mapped graph fixture tests spill findings, explicit cancellation, exhaustion
while a current cursor is pinned, successful successor traversal, and REFILL
invalidation with an unchanged serial. The copied-record fixture tests queued
and in-flight findings in both Young and Old lanes, then distinguishes queue
absence from the completed job's still-live metadata lease. All owned retirement
fixtures now consume the combined receipt.

Validation on Linux x86-64 WSL2:

- Focused graph/allocation/copied-record fixtures, strict clang-15 with ASan
  and UBSan: passed.
- `cargo test -p klassic-native gc_atomic_`: 10 passed, 51.36 s.
- Mandatory C ASan/UBSan and TSan: passed, 181.71 s.
- Strict C syntax, formatting, diff and inventory checks passed; 28,074
  candidate access sites across 452 files.
- Logs: `/tmp/klassic-queue-incoming-native.log` and
  `/tmp/klassic-queue-incoming-sanitizers.log`. Full workspace and actual
  Windows/Darwin execution were not rerun for this C-private connection.

These C-private
audits run with C/generated x86-64 lifecycle leaves; the new outer audit is not
emitted code. Actual concurrent queue mutation is serialized by the stated lock
contract; these new fixtures exercise interleavings under exclusive ownership.
Default migration, worker services, other proof domains, cross-OS execution and
the full G01-G15 completion gates remain open.
