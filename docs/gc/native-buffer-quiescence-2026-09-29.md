# Per-context buffer quiescence — 2026-09-29

`gc_native_buffer_quiescence_locked` checks one registered context's SATB and
remembered rings in constant bounded work under the registry lock. It requires
native state or an acknowledged active handshake before reading producer-private
`top`. Running and detaching owners return WAIT. Guarded access and private
producers also prevent success. Native entry and handshake acknowledgement
provide owner publication; the lock excludes reentry/resume during the check.

Both rings must satisfy `top == published == consumed`, so an unpublished prefix
cannot disappear behind an apparently empty public ring. Each consumer must
have no active claim. An active consumer returns WAIT before its `cursor` is
read: COPY owns that field outside the registry lock. Completion therefore
requires actual phase-specific processing and registered-job ACK, not just a
finished copy. Registered jobs cannot be mixed with raw drains, whose returned
snapshots are outside this inventory.

The check retains no pointer or certificate and grants no managed access. DONE
is instantaneous; callers must preserve owner exclusion, registration and claim
exclusion through its consumption. It covers one context only and conservatively
waits for all entries, including those unrelated to the source range. It performs
no buffer traversal and no global thread walk during a stopped phase.

The fixture uses real registration, native entry/reentry, buffer append/publish,
consumer CLAIM/COPY/ACK, and handshake BEGIN/ACK/END/RESUME. Both ring selectors
exercise private prefixes with public counters at zero, pending public entries,
and a pthread COPY concurrent with repeated quiescence checks. It rejects an
ended handshake and malformed identity/counters. Entries are null and token-free,
so their complete processing before ACK does not fabricate metadata ownership.
The native harness supplies the emitted x86-64 consumer operation; the C harness
supplies the shared implementation. The observer itself remains C-private.

Whole-registry bounded traversal, registration-change invalidation and a mandatory
buffer input to the owned source-retirement receipt remain unimplemented. A linked
thread entry is not an outside-lock context lease. The current receipt still
requires scan controls, queues and roots, with buffer proof owed by its caller.
Default compiler migration, complete G01-G15 behavior and actual cross-OS and
performance evidence remain open.

Validation on Linux x86-64 WSL2:

- `cargo test -p klassic-native gc_atomic_`: 10 passed, 53.85 s.
- `cargo test --test llvm_gc c_gc_passes_under_sanitizers`: passed, 179.43 s;
  both ASan/UBSan and TSan builds require the new fixture marker.
- Strict clang-15 C syntax, `cargo fmt --check`, `git diff --check` and inventory
  verification passed; 28,247 candidate sites across 455 files.
- Logs: `/tmp/klassic-buffer-quiescence-native.log` and
  `/tmp/klassic-buffer-quiescence-sanitizers.log`.
- Full workspace and actual Windows/Darwin execution were not rerun.

Subsequent connection: [the registered-buffer observer](native-buffer-incoming-2026-09-29.md)
now provides bounded whole-registry traversal and a mandatory input to the owned
source-retirement receipt. The implementation and validation above record the
preceding per-context revision.
