# Native allocation ownership connection (2026-09-29)

Historical validation snapshot. Subsequent
[logical record ownership work](logical-record-lifetime-2026-09-29.md) replaces
the raw fixture teardown described below with owned physical/logical retirement
and runtime disposal. The earlier timings/counts below are not fresh results.

`gc_native_allocation.h` connects the C bootstrap owner to the shared rooted
allocation entry. It does not change the default legacy collector or claim
G01-G15 completion.

The runtime now retains an intrusive inventory of stable allocation-run
controls. A run admits a Young page lease and OS-rounded bitmap, forwarding
table and mark-record charges under the heap lock. Failed admission returns
pending credits without OS work. Preparation commits/zeros the payload and
maps metadata outside locks in BlockedNative. Budget commit and table
installation precede READY publication; the owner resumes Running before
binding the TLAB.

Allocation looks up the startup descriptor registry and current registered root
segment, then invokes the shared allocation entry. The result remains rooted
and INITIALIZING. Initialization and readiness publication retain their separate
guard/lease/mark obligations. ROOT_FULL, journal pressure, TLAB exhaustion and
metadata exhaustion remain explicit service results. Existing live output
handles cannot be overwritten by this entry.

Cancellation handles only never-bound runs. It separates locked detachment,
native decommit/unmapping and final locked budget release. Failed OS cleanup
retains outstanding ownership and credits. Bound or closed runs are rejected:
source retirement, forwarding retention and the independently shared lifetime
of logical mark records must be handled by the collector. The runtime cannot
be disposed while any allocation run remains in its inventory.

The fixture covers an inventory with non-LIFO removal, five admission paths,
twelve preparation/cancellation paths (map/unmap failure and partial publication),
and five real allocation paths. The latter exercise zero-size and multi-page
payloads, guarded initialization/publication, journal drain/retry, root capacity,
TLAB capacity and mark capacity. The same fixture invokes C allocation and the
generated x86 allocation entry against actual mapped startup/metadata packages.

Successful-allocation fixtures finish their registered root lifecycle, verify
that runtime disposal refuses the retained runs, then end all fixture lifetimes
with raw OS teardown. This teardown supplies no collector budget credit and is
not a production shutdown or installed-run retirement test. No worker, queue,
handle or other actor survives it. Full installed-run ownership handoff, mark
record retirement, service loops and generated provisioning remain required.

Validation on Linux x86-64 WSL2:

- Focused fixture compiled with Clang 15, strict warnings and ASan/UBSan: passed.
- `cargo test -p klassic-native gc_atomic_`: 10 passed, 0 failed/ignored,
  50.02 s. The new fixture calls generated x86 allocation code.
- `cargo test --test llvm_gc c_gc_passes_under_sanitizers`: passed, 178.01 s;
  the required marker runs in both ASan/UBSan and TSan C suites.
- Strict C syntax, `cargo fmt --check`, `git diff --check` and generated
  access inventory check: passed, 27,288 candidate sites / 440 files.

Logs: `/tmp/klassic-allocation-run-native.log` and
`/tmp/klassic-allocation-run-sanitizers.log`. These targeted checks follow the
earlier full workspace sweep; they are not a new full workspace or actual
Windows/Darwin execution claim.
