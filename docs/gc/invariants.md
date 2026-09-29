# Generational GC proof and test obligations

Status applies to the worktree, not to a hypothetical final collector.

| Invariant | Present evidence | Outstanding obligation |
| --- | --- | --- |
| Null remains zero before address arithmetic | Rust/C ABI conformance | every generated barrier funnel |
| Only aligned starts with legal metadata decode | arithmetic + callback-validator tests | committed heap metadata, guards and forwarding lifecycle |
| Metadata fields/phase word do not mix generations | shared bit definitions and layout tests | atomic generated publication/adoption on each ISA |
| Completed logical stores survive self-heal | exhaustive access-slice model; unconditional-store counterexample | runtime CAS healing and full SATB store barrier |
| Physical root repair preserves owner stores, releases and logical revision | leased exact-CAS C/emitter helper; 804 full-state cases, 64 owner/scanner schedules and 64 relocated-array root checks | production strong-reference handoff, complete remap audit and source/address retirement |
| A root snapshot cannot be forgotten at a forwarding-edge poll | counted pending walk item, index-authenticated ACK, 2,749 C/emitted cases and 64 moving-array graph traversals; NEXT/scan/audit bypass rejection | production snapshot dispatch, source metadata retirement and independent generation overlap |
| Root scanner pressure does not lose its snapshot or overwrite a newer root | 64 registered scanner rounds, 128 native source/destination metadata waits with phase retries, and 32 actual owner null stores preserved by delayed repair | production pressure scheduling, source retirement and platform execution |
| Every relocation claim has admission before the CAS, and losers leave no permit | 431 shared C/emitted full-state cases, 64 two-actor rounds, and all pipeline/graph claims use gate-derived tickets; installed jobs require matching permit serial | canonical heap gate wiring, source metadata/strong-liveness leases and production scheduler |
| An installed job cannot release its permit before authenticated publication and FINISH | 1,134 job / 535 gate full-state C/shared-emitter cases, 448 pipeline jobs and 64 moving graph rounds reject premature release and duplicate binding | canonical heap gate and production enrollment, pre-INSTALL cleanup and final retirement |
| Copy has a single canonical identity | two-worker claim/copy/publication model | C and emitted forwarding protocol, failure and losers |
| No source writer during copy | access-slice model and injected violation | canonical scalar/reference/bulk access throughout compiler |
| Published result is rooted across safepoints | specified output-root ABI and no-poll TLAB-to-reserved-cell fixture | production output-root wrappers, phase/handshake lifetime and all compiler lowering |
| Mark termination counts producers/in-flight jobs | model regressions, shared rendezvous-bound root/owner/buffer/queue predicate, real graph pending SATB rejection | production publication/producer accounting, root/scanner completion and phase handshake |
| A collect request cannot be satisfied by an already-started cycle | shared request-time tickets, 480 C/emitted cases and 16 controlled real C collector pairs | complete generational scheduler and direct production request/wait loop |
| Cycle wait cannot lose completion, satisfy the wrong ticket or reenter during a handshake | predicate-loop native blocking, separate request/completion conditions, 48 ordered races, 24 broadcast cohorts and 24 mixed consecutive-ticket cohorts with per-owner waits | direct production wait and platform scheduling/latency gates |
| Prepared MarkStart boundaries are adopted before concurrent marking | shared START/ACTIVATE/RELEASE, two uninterrupted adoption rendezvous, 6,510 phase cases and 64 graphs with workers registered before START | epoch/color preparation, Young remset flip and production cycle scheduler |
| A late remembered buffer entry cannot disappear behind the scan cursor | two slot banks with counted pending bits/unique claims, late insertion into both banks, 6,638 C/emitted cases and 64 buffered moved-slot replays | production page-inventory registration, Young mark handoff, promotion and metadata retirement |
| Young end includes every registered page's remembered work without scanning pages | shared aggregate inventory and mark-end check; 1,116 inventory, 1,101 end and 6,528 phase cases; two-page final-claim and moved-buffer fixtures | production registration of all Old pages, Young handoff and retirement |
| Buffered slot retains enqueue-time owner | root-reuse/owner-move model; complete-entry buffer handoff and retained page tokens | production store barriers, moving-owner resolution and reference liveness |
| Generation filtering uses the canonical guarded target, not stale source metadata | 4,695 C/emitted routing cases; 64 null and 64 Young-to-Old remembered target moves with retained cleanup and current records | production routing, queue epoch coordination and promotion policy |
| A remembered-page cursor cannot outlive unlinked native inventory storage | shared bounded BEGIN/NEXT/CANCEL; 731 C/emitted cases; real-worker poll/DETACH race | production live-owner binding, failure cancellation and metadata retirement |
| A remembered claim binds metadata to its captured Old run incarnation | shared claim-derived lease; 842 C/emitted cases and 96 protected-source/consumed-claim cases | production owner liveness, recovery and retirement |
| A remembered slot is resolved without dereferencing a protected source mapping | bounded metadata-only predecessor/start validation; 5,936 cases, 32 publication rounds and 96 protected-source lookups with nonzero field offsets | production live-owner binding, dead-owner filtering and retirement |
| Moving an Old owner cannot erase a claimed slot or its next-epoch record | shared locked transfer with independent map/count oracle, 1,769 cases, 96 actual-page transfers and 64 later-epoch revisits | production slot-to-owner lookup, staged-buffer and metadata retirement |
| A remembered slot cannot disappear between load and Young mark offer | shared claim-to-root leaf, 2,333 full-state cases and 64 OS-backed owner/referent move handoffs; ACK follows queue ownership | production consumer dispatch, routing adoption and retirement |
| Young survivors remain in remembered sets | flip/promotion/null rewrite, forgetting/wrap and dead-owner models | runtime bitmaps and memory ordering |
| Old mark survives Young movement | queue/promotion model and shared mark-record regression | runtime metadata, coordination and fairness |
| Old references cannot alias reused addresses | source-holder gate and premature reuse counterexample | all runtime holders, quarantine/protection and memory ordering |
| A publication audit cannot admit a new mover after closure | C/emitter admission gate with 533 full-state cases; graph registers before claim, delays 64 source seals and freezes 32 incoming audits | enrollment of all production move/cancel paths, reference-holder audit and final retirement |
| Weak load/clear linearize with strong marking | specified only | weak table/keep-alive/return-root race tests |
| Pause contains no heap/root/graph traversal | baseline violations documented | concurrent segmented roots/mark/select/retire |
| Native event cleanup retains ownership on failure | emitted Windows API-model tests and ARM64 instruction-emulation failure/retry cases | actual Windows/macOS execution, common lifecycle integration, teardown after all users join |
| Notifications survive wake-before-park and spurious wakes | Linux real futex harness and 384 raw-worker handshake rounds with dedicated owner/coordinator events; Windows event API model; ARM64 protected-predicate loop under API model | direct production lifecycle wiring, Darwin real scheduling and repeated handshakes |
| Blocked owners cannot stall a handshake or access managed storage | C 1/2/8-owner blocked-progress/relocation tests and rejected misuse under ASan+UBSan/TSan | direct emitter lifecycle wiring, all native call sites, handles/copy-out/pin integration |
| Thread membership is stable during a handshake | C controlled late attach/native return against poll, entry and detach; no identity or handshake counter wrap | generational phase/buffer adoption and concurrent scanner retirement leases |
| Each live registration acknowledges a request at most once | 41,615 C/x86 shared-step cases and ARM64 instruction replay, including Detaching; emitted 1/2/8-owner test, 384 consecutive rounds | direct production registry/OS integration; unique entry ownership and scanner lifetimes; target hardware execution |
| State publication never overwrites adjacent ABI fields | emitted u32 guard-page/canary/CAS/publication tests; 1,536 C/x86 transition cases and ARM64 instruction emulation | native ARM64/Windows evidence and direct coordinator integration |
| TLAB failure cannot publish a partial allocation | C/x86 full-storage preservation, overflow/ownership/descriptor/bitmap/refill cases; ARM64 before/after replay | page refill and metadata-allocation failures; rooted result/initialization publication |
| Published object-start metadata has an initialized immutable header | C acquire/release concurrent header/bitmap test; C/x86 prefix and adjacent-byte checks | actual collector scans, initialization/mark accounting, ARM64 hardware ordering |
| Concurrent preparations cannot spend the same quota or virtual slot | heap-lock transaction; 10,496 C/x86 reservation/budget/fragmentation/rollback cases and ARM64 replay | production heap lock/OS integration, all metadata classes and allocation-stall handling |
| Failed preparation retains ownership until cleanup succeeds | real-mapping pipeline with commit/bitmap/cleanup failure injection; abort rejects live mapping/bitmap ownership, stale and shortened leases | production OS failure paths, scanner-safe retirement and reclaiming published runs |
| Closing a TLAB never makes its objects immediately reusable | C/emitted bind/close preservation cases, large-payload/refill pipeline; Closed is distinct from Free | collector scanning, object initialization/mark publication and eventual reclamation |
| Readers observe a complete immutable descriptor and exactly one registration wins | 2,539 C/x86 cases, ARM64 instruction replay; 64 three-thread duplicate/publication races in C sanitizer and emitted x86 drivers | native ARM64/Windows ordering, compiler descriptor tables and registry lifetime/accounting |
| Shape scanning never reports scalar slots and has a finite per-call budget | 12,498 C/x86 cases with independent offset expectations, all bitmap tail widths, partial final bytes and 1 TiB logical arrays; ARM64 replay | production receiver/guard integration and initialization policy; mark jobs now use the scanner in the stationary-heap fixture |
| Root overwrite/clear retains old/new values before mutation | C/x86 storage and full-journal preservation cases; 64 forced delayed-scan/overwrite/clear rounds under C sanitizers and emitted x86 | generational mark consumers, phase lifetime/adoption and in-flight termination |
| Root journal slots are not overwritten before consumer copies finish | release publication/acquire consumption SPSC protocol; 737 drain cases including unsigned counters/ring wrap; C TSan and emitted concurrent producer/consumer | bounded flush scheduling, metadata OOM/backpressure and worker ownership handoff |
| Segment pop performs no root traversal and preserves deferred work | registered queue insertion/freeze; progress-checked deletion claims and captured Young/Old completion mask; bounded cleanup after all readers leave | production phase/root discovery and mark work, compiler/native integration |
| Native root cells are not reused under a scanner | 1,261 lifecycle, 150 cleanup and 384 leased-scan C/x86 cases and ARM replay; 64 delayed-reader/dual-mark/protected-page/same-address reuse rounds under C sanitizers and emitted x86 | actual ARM64/Windows execution, production allocation/metadata accounting, phase and journal handoff |
| Delayed root discovery cannot read reused segment storage or chase new heads forever | cursor reader lease, decreasing scope order, CLEAN unlink after deletion ACK; 1,883 walker and 18,331 captured-prefix C/x86 cases and ARM replay; 64 emitted delayed-walker/unlink/reuse/journal/two-walker rounds | production coordinator must preserve captured phase and deletion/publication obligations; real graph visitors and target hardware execution |
| Context deregistration retains native root and buffer ownership | owner root list and retiring count; FINISH waits on journal consumption; detach requires empty roots/TLAB and fully consumed barrier cursors; 64 delayed-reader/journal/context-reuse rounds in emitted x86 | production detach driver, metadata freeing, actual SATB/remset consumers and compiler roots |
| Pending detach can answer future handshakes without resuming heap access | Detaching ACK/resume preserves state, unique request generation; C/x86 sequence cases, ARM replay and 128 rendezvous during delayed journal drain | production wait/notification and phase adoption while detaching |
| Detach progresses in bounded lock transactions while retaining unfinished resources | composed C/emitted routine, 1,120 full-state cases and ARM replay; concurrent reader/journal fixture now drives it; Detaching TLAB close with new binding rejected | production action execution, notification/wait integration and compiler frame lowering |
| Barrier ring reuse cannot overwrite an entry being copied | SPSC release/acquire publication and consumption; 2,340 producer/1,244 drain C/x86 cases and ARM replay; 64 concurrent private-prefix/wrap/complete-record rounds | production lease acquisition at barriers, phase lifetime and native hardware ordering |
| Detach waits for actual buffered records to transfer | 64 concurrent rounds publish private SATB/remset entries and delay their drains separately; all six entry words verified before context reuse | production scheduling/registry borrows, graph/remset visitors and in-flight termination |
| Buffered metadata remains live after ring consumption | 2,160 C/x86 token cases and ARM replay; 64 real-mapping/buffer-full/delayed-job/stale-token rounds; pool skips held Free slots | production lease budgets, compiler barriers, forwarding/color retirement, graph liveness and in-flight termination |
| Mark acquisition cannot lose its unqueued scan on FULL | lock-serialized stamp/queue commit; 6,117 C/x86 cases and ARM replay | fast-path atomics, metadata admission/OOM and per-worker queues |
| A stale claim cannot finish a resumed or reused task | fresh nonwrapping serial on OFFER and YIELD; full-state rejection cases | production task allocation, worker exit and scheduling ownership |
| Huge pointer arrays are scanned without a whole-object pause | bounded emitted descriptor/atomic slot scan; 1,567 cases plus 1,024-slot graph sliced by two workers | canonical receiver/guard integration, initialization during marking and collector phase scheduling |
| Holding every mark task slot does not prevent recording children | prepared per-generation record cells; 959 overflow cases and ARM replay; actual graph passes with 1/2/16 task slots and matched spill/refill counts | production metadata/lease budgets, OOM handling, worker scheduling and phase termination |

A plain `PhaseSnapshot::transition` does not prove any row's runtime gate.
Neither a cycle sequence nor debug ObjectId can disambiguate a stale GcRef after
its offset and color have both been reused. Runtime completion requires each
pending obligation, the full instruction failure matrix, and actual target-OS
execution/measurement. No STW fallback or permanently retained heap substitutes
for these obligations.

## Native metadata charge ownership

- Page reservations and native metadata charges use the same heap lock and
  `metadata_pending + metadata_committed <= metadata_max` ledger.
- Reserve capacity before native allocation and before publishing any object
  whose mark record or lease storage depends on that capacity.
- A held charge is unique, heap-bound, and immutable except for the external
  allocation field and transitions performed by the accounting leaf.
- Abort/release happen only after successful external cleanup. Retirement of
  records, workers, buffers and jobs is a separate required proof; the budget
  leaf cannot infer it from a zero allocation address.
- Every rejected operation preserves counters and charge state. Closed charges
  cannot be reused without a fresh initialization after ownership has ended.

## Stable records at object allocation

- An allocated object acquires exactly one zeroed, admitted native mark record.
  Its immutable identity is initialized before the release publication of the
  header/start bitmap. The owner-only arena cursor is never a scanner input.
- Arena exhaustion does not advance the TLAB or expose a partial object. Retry
  must supply admitted stable storage; it cannot relocate a used record prefix.
- Physical copies preserve the header's immutable record pointer. Equal debug
  identity values do not authorize substituting a different mark record.
- Record retirement waits for every physical copy and metadata user, including
  both generations' queued, claimed and deferred jobs. Closing a TLAB or
  exhausting its arena is not proof that any record can be freed.

## Reference-directed page metadata retention

- A reference acquisition validates the head/tail geometry and release-published
  object start before consuming any lease token. Tail starts retain the run head.
- Bitmap allocation/lifetime and run identity stay stable under the heap lock.
  Mutator start-bit publication precedes the acquired allocated prefix.
- The helper does not dereference a heap object. Its token retains metadata,
  independently of the caller's strong root, raw access guard and source/color
  retirement proof. GcRef alone carries no page incarnation for stale-bit checks.
- Pool failure leaves the output empty; a held output cannot be overwritten by
  a second acquire. Release uses the token's resolved page/incarnation.

## Atomic forwarding publication

- A stable entry key includes source offset and page incarnation. The key and
  entry storage remain live/immutable through every contender and reader.
- Exactly one successful CAS owns the claim ticket. A valid ticket value is
  not permission to publish unless this actor won and still owns that claim.
- State and full destination occupy one atomic word; acquiring Published sees
  initialization completed before the winning release CAS. Busy exposes no
  destination. No live reset or published-to-retained transition exists.
- Source quiescence for every writer/healer, copy completion, destination
  protection and shared mark/promotion state are external preconditions.
- Every private losing/abandoned candidate is cleaned before ownership ends.
  Retained sources remain protected until the later cycle/retirement decision.

## Source guard exclusion

- ENTER and SELECT compete on the same counted gate. A pre-selection entrant
  is either rejected or counted; copying requires closed admission and count 0.
- All raw-access actors use the same rule, including healers and mark/remset
  scanners. Unguarded actors would invalidate the exclusion proof.
- A unique scope owns one count and one context marker. Failed EXIT retains
  both; retry immediately without blocking/polling. No raw use follows success.
- Exit release RMWs and the acquire quiescence check order completed stores
  before copying. Observing the forwarding claim alone is not quiescence.
- PUBLISH/RETAIN reject an undrained/open gate. REOPEN requires retained
  forwarding and a drained gate; a published source stays closed.
- Source metadata lifetime, strong reachability, phase adoption and retirement
  remain independent obligations. A drained count alone does not permit reclaim.

## Nonmoving source lookup

- Run-head forwarding tables are installed before the first allocation and
  never replaced/resized/reset while any metadata holder or source user lives.
- Only the TLAB owner appends. Source keys increase strictly, incarnation is
  fixed, and release publication of used precedes the start bit/allocated_top.
- Readers acquire a single used prefix and inspect no uncommitted suffix.
  Lookup forms no heap address; metadata retention and strong reachability
  precede lookup, and successful guard admission precedes header/payload reads.
- Failed allocation leaves heap, mark arena, table and output unchanged,
  including metadata exhaustion. Native table storage requires budget admission.
- A table match cannot recognize stale GcRef bits after premature range reuse;
  color/source retirement is still mandatory.

## Bounded resolve/access handoff

- The resolver reads only retained metadata until successful source admission.
  Published source mappings may be physically inaccessible without changing
  this rule. Virtual reuse still requires source/color retirement.
- Each call follows at most one published forwarding edge or attempts one
  guard CAS. No chain walk, helper wait, allocation or lock is hidden inside it.
- FORWARDED has no guard. Its private destination must be adopted into an
  already prepared root without polling, or discarded before retry/poll.
  Prepared root storage does not by itself guarantee journal capacity.
- After a successful ENTER, every raw use finishes before EXIT. EXIT WAIT
  preserves the guard/context marker and requires immediate nonpolling retry.
- An input page metadata lease outlives lookup, guard use and successful EXIT;
  destination metadata is reacquired for the next rooted resolution step.

## Physical copy cursor and publication

- Only the claim owner copies a source, after acquiring closed admission with
  zero source guards. Source liveness/metadata and private committed candidate
  reservations survive every slice, scheduling boundary and cleanup decision.
- Source and destination whole object ranges must not overlap. All arithmetic,
  header and stage checks precede any write; rejected attempts preserve state.
- Cursor progress is unique-owner, monotone and not replayable. A call copies
  at most 256 payload bytes plus fixed header work; no raw pointer is returned.
- The destination shares the source MarkRecord pointer, preserving concurrent
  generation marks and logical identity without duplicating record ownership.
- Complete-copy publication requires a terminal cursor and matching Published
  destination header. Destination metadata/promotion handoff precedes forwarding.
- Partial candidates never become visible through forwarding. Their resources
  are reclaimed before source retention; source/table retirement is separate.
