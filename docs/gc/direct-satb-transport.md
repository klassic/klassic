# Direct previous-referent transport

This is the SATB transport portion of issue #677, following the remembered-slot
sealing change. The direct runtime now defers eligible overwritten references
in a bounded per-owner buffer. The canonical C runtime already has its separate
barrier-entry and buffer-job protocol; this change does not replace that protocol
or establish complete runtime parity. Issue #676 remains unresolved.

Each of the 16 private entries captures a previous referent, the original owner
and byte offset, a generation and its marking epoch, and separate metadata leases
for the owner and previous-referent pages. Mutable RootHandles are not retained.
A lease prevents premature metadata reuse; the pending obligation, followed by
actual mark-queue ownership, supplies reachability. An entry does not mark its
referent merely to claim admission.

Both generations have independent pending counts. MarkEnd and worker completion
account for these counts. Flush transfers the referent into the existing marker
queue/spill protocol before releasing either lease and removing the pending
obligation. An epoch mismatch is rejected. A Young epoch reset cannot silently
remove Old pending work.

Full buffers flush at most 16 entries before admitting another. Exhausted lease
storage falls back to the existing synchronous barrier; a failed second lease
acquisition releases the first lease before fallback. No GC-managed allocation
is used for buffer storage. New-value protection remains synchronous, so deleting
an already protected new value does not create redundant white-reference work.
References already protected in the current epoch keep the existing path
without a collector ticket, work request or completion-hint reset. Admissions
recheck protection after taking the ticket. The implementation still serializes
new admissions with that ticket; this
is not a claim of a lock-free barrier or a final pause-time bound.

Ordinary polls and allocation safepoints flush partial buffers. This is necessary
because end-rendezvous scheduling otherwise waits for marker completion, while
marker completion waits for private obligations. ACK, Native entry, detach and
collection also seal entries. The physical-move admission rejects any remaining
SATB or remembered work. Stores during open movement or source retirement use
the existing synchronous barrier rather than acquiring fresh source leases.

The shared slot-owner lookup now accepts published Young owners as well as Old
owners for SATB capture. Allocation and worker Young copying publish the lookup's
start/card metadata. The existing remembered-slot lookup remains Old-only.

## Focused evidence

Tests cover independent Old/Young/overlapping epochs; partial-buffer handoff on
polls and allocations; non-null overwrite and insertion protection; distinct
previous values at one slot across successive actual epochs; capacity 16/17;
real lease exhaustion and second-lease rollback; fresh and worker-copied Young
interior slots; stale/cross-page hints; captured-epoch rejection; rejection of an unsealed full move before marker
waiting; an actual Young move preserving the Old epoch and relocating the
previous referent in the real Old frontier; synchronous actual field-store
fallback during an open move using the previous referent's logical identity;
and real 1/2/8
OS owners sealing at ACK, Native and detach boundaries. Validation on Linux x86_64 with Rust 1.99.0 is recorded below.

Two initial fixture failures were retained and diagnosed. An unbounded allocation
loop waited for both generations to become idle while continually starting new
work; its original Old cycle had already completed before the fixed nonmoving
bank exhausted. The test now asserts a bounded allocation handoff separately
from poll-driven closure. A parallel fixture passed the actor ABI's deliberate
invalid-runtime sentinel into an internal checkpoint; it now calls that internal
entry with its required bound runtime. Neither correction weakens the runtime's
invariant checks, and passing retries alone were not treated as diagnoses.

The movement fixture also initially enabled independent journal consumption
before constructing its graph. That consumer could legitimately protect a
historical child while only the graph tracer was gated, so the intended private
SATB precondition was not established. Enrollment now follows construction and
publication: the actual enable protocol seals the sole-consumer backlog before
the snapshot. The test retains its white-referent, pending-entry, lease and
movement assertions. New SATB fixtures retain their executable and complete
output through the entire assertion scope.

The open-move fallback test captures the field's actual previous referent after
the worker parks outside its ticket, then checks that referent's identity and
mark obligation. Valid recoloring and earlier field healing are permitted.
Existing late-frontier/forwarding tests continue to cover retention through a
white copy; the new test verifies routing through an actual enrolled field store.

The initial workspace run also exposed a real fast-path regression: three
existing store tests observed collector tickets for already protected values.
The offer path now performs the acquired current-epoch check before taking the
ticket and repeats it under the ticket for an actual admission. The existing
zero-ticket and exact-ticket assertions remain unchanged.


## Validation

The initial workspace run recorded 1,425 passes and three fast-path failures.
After the runtime correction, all 618 `klassic-native` tests passed. The 108-test
GC integration run recorded 107 passes and one console-fixture counter failure:
two real owner metadata mappings added balanced Native intervals beyond its six
console writes. The fixture now preserves the exact six-write count, the observed
blocked Native state and worker reclamation, and balanced total entry/exit counts.
Its focused rerun passed with 97 blocked-state samples and 25 worker decommits.
The other 107 integration tests used the identical corrected runtime bytes.

The remaining five workspace crates passed all 86 tests. Together with the
unchanged successful workspace prefix, this covers 1,514 distinct tests; it is
not an uninterrupted successful full-workspace command. Workspace all-target
Clippy with `-D warnings`, formatting and diff checks passed. Original failure
logs, generated executables and source hashes were retained for review.
