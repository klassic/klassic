# Direct remembered-buffer sealing

The opt-in direct generational collector buffers precise Old-to-Young slots
for enrolled owners. Each owner has at most 16 entries. An entry captures the
owner reference value, field offset, page incarnation, remembered epoch and a
real metadata lease. It never retains a mutable RootHandle as its identity.
This is remembered-slot transport only: previous-referent SATB protection
remains synchronous in the existing field/Young barriers. It does not claim
completion of the broader G06 or all SATB buffering requirements in section 5.2.
Issue #677 remains open for that transport and its MarkEnd enrollment.

## Admission and handoff

The collector ticket serializes store admission, flushing and phase changes.
Only a Running owner can record, including the already-remembered fast path.
An owner flushes before its phase acknowledgement, Native entry or detach.
Acknowledged owners remain parked until adoption and resume. Physical bank
mutation, Young bitmap flip and full remembered-set discard require zero
pending entries globally. The moving rendezvous obtains the peer handoffs;
the empty check rejects a broken protocol rather than silently losing work.

A full buffer drains at most 16 entries before admitting another store.
Metadata lease exhaustion falls back to synchronous registration without
discarding the edge or recursively allocating a managed object. Processing
validates the captured epoch, lease and original slot owner, registers the
slot, then releases its token. The metadata lease is not a strong object root.
Dead owners are subsequently handled by normal remembered scanning/full
collection, rather than being retained indefinitely by transport.

No buffered entry crosses a physical bank move. A Young cycle retains Old
slot identity. A full cycle first seals all buffers and then discards both
sets because every surviving object is promoted; resumed stores register
destination slots for later Young cycles. This conservative exclusion avoids
using a captured source payload after its lifetime ends. It does not introduce
asynchronous forwarding of buffered entries or a lock-free store path.

## C runtime relationship

The canonical C runtime retains its existing registered CLAIM/COPY/ACK jobs,
captured-epoch processing, page inventories and remembered-owner transfer.
Its consumer processes one bounded batch per invocation; its retirement
quiescence gate includes unpublished prefixes and active consumer claims.
The direct implementation uses the earlier complete handoff described above.
These are different schedules with the same required edge/lifetime outcome,
not interchangeable internal buffer representations.

## Validation scope

Rust 1.99 validation covers 1499 distinct tests: 810 passing tests before the
initial full run reached the graph-fixture assertion, then the corrected
603-test native suite and the remaining 86 crate tests. None were ignored.
All-target workspace Clippy and formatting also pass. The runtime source
hashes are unchanged between the full run and the corrected native run;
only the two harness/test files changed. This is combined validation, not a
claim that the initially failing full command passed on retry unchanged.

Generated executable tests cover 1/2/8 actual owners, capacity overflow, lease
exhaustion, Native entry, detach, original RootCell release and exact reuse,
Young/full/Young transitions, physical Old-owner movement and fresh stores
into the destination. They check sole-edge values, pending/admitted/flushed
counts and released leases. Negative tests reject an unsealed flip and stores
from an actually acknowledged or Native owner, including an already registered
slot. Existing remembered scan/retirement tests cover current/previous
independence and post-resume stores during cleanup.
Removing the transport's Running-state guard makes the sealed-owner negative
test fail because the invalid record succeeds; restoring the guard rejects it.

An initial transition fixture expected a new buffered entry when rewriting an
already registered slot; the observed deduplication was valid. The fixture now
uses distinct fields so every tested transition starts with new pending work.
The first full run also exposed a missing graph-test precondition: its shared
hub began Young, so promotion could establish remembered membership before
the first relevant mutator store. A successful executable then reported zero
buffer admissions and failed the Rust assertion. Diagnostic image replays
confirmed Young-before/Old-after with existing remembered membership and no
buffer admissions. The fixture now completes a real full bootstrap and checks
an empty Old hub before starting 1/2/8 owners; admission requirements remain.
This fixture retains its executable through Rust-level assertions as well as
runtime failures. The original successful-exit image was already removed by
the older failure-only retention; its output and the diagnostic replays remain.
This follow-up does not identify or close the separate #676 intermittent
failure, or establish real-hardware platform and latency acceptance.
