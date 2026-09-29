# Logical record ownership through shutdown (2026-09-29)

`gc_native_records.h` connects runtime-owned copies to their originating mark
arena. A parallel owner-pointer table is included in the rounded forwarding
charge. Original allocations retain their arena until physical retirement;
each installed imported copy increments its originating arena's count under
the heap lock. The originating TLAB must be closed before copying, so its
record count is stable. Failed installation rolls back the private import.

Every copy into these owned runs must use the new installation wrapper. Record
borrowers retain a lease on a physical copy through their last record use,
including private, queued, claimed and completed mark work. The full source
retirement proof and lease checks therefore exclude such borrowers before a
bounded metadata step clears a sidecar slot and returns one arena import.
Physical source reclamation alone still does not free shared logical records.

After its physical run retires and its final imported copy releases ownership,
an arena can be sealed under the heap lock. New imports are rejected before
reading pointers that native disposal may clear. The sealed stage remains
unchanged outside the lock. Failed unmap retains the mapping and charge; only
successful unmap permits budget release and runtime-inventory unlinking.

Wholly dead closed runs have a separate seal entry. Its caller must establish
absence of all root, heap, weak, native, job and incoming-forwarding references
to the whole range and exclude new actors. Zero leases, mark bits or an empty
queue do not establish that proof. The entry checks ownership and leases and
then joins the normal uncommit/retirement path. This supplies an integration
point for the cycle's proof, not an implementation of that global proof.

The six copy/retirement fixtures now exercise both generation queues, a job
that outlives root removal, TAKE, FINISH and the final metadata-lease return.
Neither queue completion nor physical source reuse permits early record free.
They cover failed copy installation, failed final unmap, replay rejection and
late invalid admissions on another thread during native disposal. All arena
charges, physical charges and runtime inventory entries are returned before
real runtime disposal. The five successful allocation fixtures also use owned
dead-range retirement and record cleanup. These paths no longer end with raw
test-only unmapping or fabricated accounting.

The fixture-specific dead-range proof is complete because all objects have
scalar descriptors, registered roots/journals are retired, no other actors or
handles exist, and the sole queued job returns its real lease. This does not
prove the production concurrent collector's graph audit, normal worker
scheduling or default compiler migration. Provisioning/ownership controllers
remain C-only; the native fixture invokes generated x86 allocation within them.

Validation on Linux x86-64 WSL2:

- Focused Clang 15 strict-warning ASan/UBSan execution: passed.
- `cargo test -p klassic-native gc_atomic_`: 10 passed, 0 failed/ignored,
  52.53 s.
- `cargo test --test llvm_gc c_gc_passes_under_sanitizers`: passed, 191.44 s;
  the new logical-lifetime marker is required in both sanitizer suites.
- Strict C syntax, `cargo fmt --check`, `git diff --check` and access inventory
  checks passed; 27,444 candidate sites / 443 files.

Logs: `/tmp/klassic-record-lifetime-native.log` and
`/tmp/klassic-record-lifetime-sanitizers.log`. This is targeted validation after
the earlier workspace sweep, not actual Windows/Darwin execution or a new full
workspace result.
