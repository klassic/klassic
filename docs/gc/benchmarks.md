# GC timing evidence and remaining performance gates

The full performance contract is in `klassic-generational-zgc-instructions.md`
section 11. No dedicated-machine, three-platform generational latency result
is available. The default collector is still legacy.

## C/LLVM aggregate timing boundaries

With `--backend llvm --gc-log`, the C runtime reports nanoseconds from
`CLOCK_MONOTONIC`. A clock failure is diagnosed rather than reported as zero.

| Fields | Interval / count |
| --- | --- |
| `pauses`, `pause_max`, `pause_total` | Rendezvous count and legacy action duration after all expected acknowledgments; retained for compatibility |
| `safepoint_max`, `safepoint_total` | Request publication to all expected owners acknowledged |
| `stopped_max`, `stopped_total` | All expected owners safe through the probe immediately before request release |
| `parks`, `park_max`, `park_total` | Actual mutator park-to-resume intervals, including waiting for later owners and scheduler/lock wakeup delay |

Park samples span consecutive rendezvous if the owner cannot resume between
them; their count need not equal rendezvous count times mutator count. A
BlockedNative owner does not produce a park sample. A handshake wrapper call
without a pending request produces none either. Registration/native reentry
waits, request-lock acquisition, allocation stalls and complete cycle time
are not represented by these fields. Aggregates at process exit are not a
versioned per-cycle event stream or a latency distribution.

`gc_timing_test.c` uses actual handshake threads with an explicitly substituted
deterministic clock: an early owner parks at 100, a late owner at 300, and the
action completes at 500. It requires safepoint time 200, stopped/action 200,
and park durations totaling 600 with maximum 400. A second native-only round
checks that no fake park sample is added. These values test interval boundaries;
they are not measured machine performance. Both sanitizer variants run it.

A further schedule holds the coordinator mutex between two real rendezvous
operations, forcing the next request to precede every waiting owner's resume.
Both owners acknowledge both generations through the production park loop.
Two action durations of 1000 must yield one continuous park sample of 2000
per owner, rather than splitting their latency at a release they never observed.
The test uses the same locked rendezvous operation as the production wrapper;
normal production lock acquisition/release remains unchanged.

## 2026-09-27 Linux LLVM smoke measurement

Host: Linux x86-64, WSL2 `6.18.33.2-microsoft-standard-WSL2`. This was one
unwarmed process on a shared development machine, not a reference benchmark.
The LLVM log regression's 100,000-record churn program retained a two-field
record and printed `3`. It was compiled with `--backend llvm --gc-log` and
executed with the real OS clock. Raw temporary artifacts:
`/tmp/klassic-timing-smoke.{kl,out,log}` and the `.bin` executable.

| Counter | Observed value |
| --- | ---: |
| Collections | 17 |
| Relocations | 51 |
| Allocated bytes | 9,600,096 |
| Rendezvous samples | 34 |
| Action maximum / total (ns) | 57,468 / 606,389 |
| Request-to-safe maximum / total (ns) | 50,427 / 842,882 |
| Stopped maximum / total (ns) | 57,534 / 608,336 |
| Mutator park samples | 19 |
| Park maximum / total (ns) | 174,151 / 2,114,798 |

The differing maxima show why action time alone cannot represent mutator
latency. The sample count cannot support p99.9. There is no throughput baseline,
heap-size scaling, Young/Old overlap, Windows/macOS execution, or 1 ms release
gate claim here. The required 10,000-sample matrix, stall/memory/CPU counters,
JSON events and direct-backend parity remain outstanding.
