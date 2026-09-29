# Native scan inventory and incoming audit — 2026-09-29

Runtime mark-scan admission now links its stable control into an intrusive
runtime inventory. YIELD/FINISH and lease cleanup leave a terminal control
registered until explicit `gc_native_mark_scan_finish_locked` removes it.
Runtime disposal rejects a nonempty inventory. Admission reserves serial space
for every future unlink, so admitting the last representable control cannot
strand existing owners at counter exhaustion. Controls remain C-private.

`gc_native_scan_incoming.h` supplies a bounded incoming-reference audit over
this inventory. One NEXT checks one control's twelve fixed reference fields and
at most 256 captured child slots. It checks the registry serial before borrowing
any cursor: attach/unlink/reuse invalidates both unfinished and completed audits.
FOUND retains the same control for rechecking, and guarded work returns WAIT.
No operation scans heap payloads, repairs references or waits.

The caller must obtain exclusive access to the current control through worker
ownership handoff; the registry lock alone cannot protect fields changed by
guarded scans outside that lock. The whole audit additionally requires a cycle
protocol excluding introduction of target references behind the cursor. These
are synchronization/phase preconditions, not properties inferred by the audit.
OS worker handoff is not implemented by this revision. DONE certifies only this
native-control inventory; queues, roots, buffers, handles, forwarding, leases,
color lifetime and the combined source-retirement proof remain separate.

The actual mapped graph-cycle fixture now verifies:

- A captured child is found before a child metadata lease exists.
- Parent references and live guards prevent a false absence result.
- After a child is handed to the queue and ACKed, its native-control finding
  disappears while the queue remains busy; the two audit domains are distinct.
- Two genuine claims coexist, and the older control can be removed first
  without losing the younger claim or its links.
- Attach/unlink invalidates saved cursors, including a completed audit.
- Exhausted serial admission preserves the queue and fresh control.
- All 14 objects and 25 bounded scan slices still complete through owned
  retirement, budget return and runtime shutdown.

This fixture uses exclusive sequential ownership. It does not test concurrent
inspection against an executing worker. C and generated x86-64 pending-mark,
scan and scan-commit leaves run through the same inventory lifecycle.
Validation on Linux x86-64 WSL2:

- Focused graph fixture with strict clang-15, ASan and UBSan: passed.
- `cargo test -p klassic-native gc_atomic_`: 10 passed, 50.06 s.
- Mandatory C ASan/UBSan and TSan: passed, 171.02 s.
- Strict C syntax, formatting, diff and inventory checks passed; 27,942
  candidate access sites across 451 files.
- Logs: `/tmp/klassic-native-incoming-native.log` and
  `/tmp/klassic-native-incoming-sanitizers.log`. Full workspace and actual
  Windows/Darwin execution were not rerun for this C-private connection.

Ordinary programs still use the legacy collector. This is one additional source
retirement input, not a complete moving collector, worker scheduler, general
retirement proof or satisfaction of the full G01-G15 target.
