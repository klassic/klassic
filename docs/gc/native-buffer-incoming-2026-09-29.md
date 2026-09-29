# Registered buffer absence gate — 2026-09-29

`NativeRunRetireCheck` now requires all registered SATB/remset owners to pass
`gc_native_buffer_incoming_next_locked`, in addition to scan controls, both mark
queues and roots. Buffer observation starts after the other domains complete.
Each call checks at most one context under the registry lock. An owner still
Running, private work, undrained ring or active consumer returns WAIT without
advancing. COPY completion alone is insufficient; processing and ACK must finish.

The observer captures the registry's `last_identity`, member count, head and
handshake generation, plus the runtime phase. It checks these snapshots before
borrowing its saved cursor on every NEXT, including after DONE. Register always
increases `last_identity` without wrapping; a detach without registration reduces
member count. Therefore detach/reuse cannot restore the captured snapshot, even
when registration storage and total member count are reused. This lets calls
release the registry lock without retaining a context lease: the saved address
is only dereferenced after mutation has been excluded. NEXT validates the current
entry's binding, links, descending identity and remaining count while locked.

The cycle must exclude new target references behind the cursor from BEGIN through
consumption of DONE. Each successful context handoff is immediately consumed as
absence evidence under that exclusion. Resumed owners may do unrelated work;
DONE does not certify that every owner is simultaneously stopped or all rings
remain empty. The observer does not implement the exclusion protocol. All buffer
consumers must use registered jobs; raw drain snapshots are outside this domain.
No thread-wide walk, buffer scanning or processing is placed inside a phase
transition. A scheduler must release locks before awaiting an owner or consumer.

The owned retirement gate now rejects missing, unfinished, stale or mismatched
buffer evidence before changing page/run/budget ownership. Cancellation needs no
buffer lease release, but still releases queue and root cursors. Existing owned
allocation/copy/graph fixtures perform real token-free append/publish and
CLAIM/COPY/ACK, proving the pending buffer blocks metadata retirement until ACK.
A Running owner also blocks the copied-record receipt until actual native entry.
Null entries carry no managed-reference or metadata-token obligations to invent.

The inventory fixture uses real thread registration and native handoffs. It
advances one step, detaches and frees the next context, then checks RESTART under
ASan. A separate detach/re-register at the exact same address restores member
count but changes identity, invalidating both pending and completed audits.
Handshake-generation changes and copied controls are rejected too. The fixture
removes its extra actors and completes runtime shutdown.

This adds the registered buffer domain to metadata retirement. Earlier payload
uncommit still needs its independent whole-range proof. General heap/native
handle/weak/forwarding and color exclusion, worker/scheduler services, default
compiler migration, actual cross-OS execution and full G01-G15 performance gates
remain incomplete. These are C-private controllers exercised by shared C and
emitted x86-64 lifecycle fixtures, not an ordinary-compiler collector switch.

Validation on Linux x86-64 WSL2:

- Focused inventory, cycle, allocation, copied-record and graph fixtures with
  strict clang-15 and ASan/UBSan: passed.
- `cargo test -p klassic-native gc_atomic_`: 10 passed, 54.50 s.
- Mandatory C ASan/UBSan and TSan: passed, 181.30 s. Both sanitizer builds and
  the emitted native harness require the new inventory fixture marker.
- Strict C and standalone-header syntax, formatting, diff and inventory checks
  passed; 28,325 candidate access sites across 457 files.
- Logs: `/tmp/klassic-buffer-incoming-native.log` and
  `/tmp/klassic-buffer-incoming-sanitizers.log`. Full workspace and actual
  Windows/Darwin execution were not rerun for this connection.
