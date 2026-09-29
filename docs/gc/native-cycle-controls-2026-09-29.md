# Native cycle controls — 2026-09-29

The C bootstrap owner now retains its cycle ledger, remembered inventory and
stable per-generation mark, relocation and retirement controls. This is a
control-plane connection, not the production collector or its graph services.
Ordinary programs still use the legacy collector.

`gc_native_cycle_begin_locked` previews request admission, the selected mapped
queue's next epoch, mark color, Young remembered epoch and initial handshake.
It commits them together only after admission succeeds. Shared START touches
control records, not the linked participant/root inventory; private preview
pointers are rebased before publication. Counter exhaustion, pending remembered
walks, missing demand and an open relocation gate preserve runtime state.

Completion requires the caller's full collection/retirement proof. It checks a
concurrent retirement root audit with owners acknowledged, releases the pause
for stale work, and requires a second rendezvous adopting Idle before FINISH.
The ledger preview validates the original ticket's owner before rebasing it to
the private ledger. Shutdown rejects unfinished requests and remembered work.

The fixture uses an actual mapped runtime, attached empty root segment and
worker acknowledgements. It starts Old marking, completes two Young cycles
while that lane remains in marking, then completes the Old lane. A request
submitted after START waits for the following cycle. A newer audit invalidates
the sampled owner certificate and forces re-audit outside the pause. Tickets
remain pending at mark end, Retire and before final Idle adoption. All native
backing is released through normal root retirement and runtime disposal.

The graph is explicitly empty: there are no allocated objects, weak/native
handles, candidates or incoming edges. This proves neither nonempty graph
collection nor G09's moving/promotion overlap. The fixture is sequential under
exclusive ownership, not an OS worker scheduling test. C and generated x86-64
START run the same fixture; the outer controller is C only.

Validation on Linux x86-64 WSL2:

- Focused lifecycle fixture: strict clang-15, ASan and UBSan passed.
- `cargo test -p klassic-native gc_atomic_`: 10 passed, 54.43 s.
- Mandatory C ASan/UBSan and TSan test: passed, 180.85 s.
- Strict C syntax, `cargo fmt --check`, `git diff --check` and inventory check
  passed; inventory contains 27,635 candidate sites across 447 files.
- Logs: `/tmp/klassic-native-cycle-native.log` and
  `/tmp/klassic-native-cycle-sanitizers.log`. Full workspace and actual Windows/
  Darwin execution were not rerun for this C-private controller revision.

Remaining work includes nonempty graph routing, whole-cycle proof ownership,
worker/service loops, remset/pin registration, direct controller emission,
compiler representation migration and actual cross-OS/performance gates.
