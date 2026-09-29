# Scan audit gate for owned source retirement — 2026-09-29

Owned source metadata retirement now requires a `NativeRunRetireCheck`. The
stable receipt binds its scan-control audit to one allocation run, incarnation,
phase and runtime inventory. BEGIN captures that binding; each NEXT executes
at most one control's incoming check under its existing worker-ownership and
registry-lock preconditions. New-target-reference exclusion must remain in
force through receipt consumption.

`gc_native_allocation_retire_begin_locked` now takes the receipt explicitly.
Before it transfers bitmap/table ownership to SourceRetire, the payload must be
uncommitted, the audit complete, the target range and run binding exact, and its
phase/inventory serial current. Missing, incomplete or stale evidence leaves
WAIT; copied or mismatched controls are rejected. Successful admission consumes
the receipt. The existing allocation, copied-record retirement and graph-cycle
fixtures all use this mandatory entry; there is no optional bypass argument.

This adds an enforced native-scan-domain check to the physical metadata release
path. It is **not** the complete range-retirement proof. The scheduler still
owes root/heap/both-queue/buffer/handle/weak/forwarding/color checks, exclusion of
new incoming references and actual worker handoff. It also does not authorize
the earlier payload uncommit step, which retains its existing separate proof
requirements. The default compiler/runtime remains unchanged.

Validation exercises real owned, OS-uncommitted runs. Rejected audit receipts leave
the run, page and heap/budget metadata byte-for-byte unchanged. Cases include
no receipt, unfinished audit, copied receipt, wrong run, incarnation, phase,
inventory serial and range, followed by consumed-receipt replay. The graph
fixture additionally captures FOUND from a genuine live scan control and later
rejects that stale result before performing a fresh audit and actual retirement.
Copied-source and final-copy cleanup continue through the same gate.

Validation on Linux x86-64 WSL2:

- Focused graph/allocation fixture with strict clang-15, ASan and UBSan: passed.
- `cargo test -p klassic-native gc_atomic_`: 10 passed, 53.03 s.
- Mandatory C ASan/UBSan and TSan: passed, 179.55 s.
- Strict C syntax, formatting, diff and inventory checks passed; 27,980
  candidate access sites across 451 files.
- Logs: `/tmp/klassic-retire-gate-native.log` and
  `/tmp/klassic-retire-gate-sanitizers.log`. Full workspace and actual Windows/
  Darwin execution were not rerun for this C-private connection.

The outer receipt
and admission controller remain C-private; generated x86-64 leaves participate
in the same lifecycle fixtures. General retirement services, moving lifecycle
coverage, worker scheduling, direct outer-controller emission, default migration
and cross-OS/performance gates remain open under the full G01-G15 objective.
