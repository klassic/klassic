# Opt-in direct runtime foundation

This integration candidate advances the direct `--gc=generational` runtime.
The default collector remains unchanged. It is a partial implementation, not
completion of the Generational ZGC contract or a latency acceptance result.

## Included implementation

The direct runtime now contains physical heap/metadata ownership, registered
owner contexts and root journals, worker-driven marking, moving rounds,
queue and lease backing, weak/pin/native-thread boundaries, and heap policy
controls. These modules share runtime layout and phase/lifetime state and are
integrated together. Runtime tests emit and execute the actual machine code.
Linux x86-64, Windows x86-64 and AArch64 macOS have distinct host adapters.
Cross-platform model evidence does not substitute for native execution.

The CLI exposes direct heap minimum/hard/soft limits, metadata limits,
uncommit delay, pre-touch, large-page policy, and text/JSON logging controls.
Their target-specific admission is checked before emission. Canonical C/LLVM
source output rejects these newly introduced controls and `--gc-log`; its
existing stress option and module diagnostics are preserved.

## Remaining work

- #676: a loaded two-owner ordinary moving-cycle run previously reported an
  invariant failure. Its original executable was not retained. Later passing
  runs do not identify the cause and do not close this issue.
- #677: per-mutator remembered-buffer sealing across movement is unfinished.
  The separate G07 buffer patch is excluded from this foundation.
- #678 through #682 retain concurrent marking/relocation lifetime, pressure,
  native/weak/thread completion, platform parity and acceptance work.
- The full G01-G15 contract remains incomplete. Pause bounds and real-hardware
  platform acceptance have not been established.

## Review and validation

Review runtime state/dispatch with its `production/` modules, then the direct
compiler/host adapters and generated-code harnesses. The integration retains
main's native ABI/frame conformance tests, entry frame bounds, concurrent
pipe draining, and canonical collector fixes.

Local integration validation on base `bea00f6` passed: workspace build,
`cargo test --locked -- --nocapture` (1492 tests, zero failures or ignored
cases across 36 suites), all-target Clippy and formatting. This includes
canonical regressions and direct moving/resource-lifetime tests. Cross-platform
CI remains required; local execution and API-model checks do not establish
all native-platform or latency acceptance. Failure artifacts and initial logs
are retained separately from the successful run.

## Integration test corrections

The ARM blocked-read supervisor now allows the model's existing 180-second
program deadline to report before its own 190-second cleanup deadline. An
exact original-image replay required 85 seconds and satisfied the native-read
progress predicate; the former 60-second supervisor killed it during preflight.
The model predicates and Windows deadline are unchanged.

The Windows idle-uncommit fixture now completes synchronous large/full cycles
before blocking its owner, matching the native-read fixture. It asserts actual
full/large cycle counts as well as unchanged owner signaling, worker wakeups,
and decommit progress. This establishes completed collection setup; it does
not directly inspect private retired-page leases. The original failed run had
15 worker timeouts with no decommit; its generated image was deleted by the
old cleanup, so failure artifacts are now retained on panic. A successful old
fixture replay had zero full cycles; that is not the missing original snapshot.

Stress-only workloads retain the normal workloads and all original assertions:
ARM retains its depth-200 graph and more than 1000 forced collections; the
native worker fixture retains a 511-node graph through more than 2000 forced
collections and five race repetitions. The initial oversized ARM run was
intentionally interrupted with its exact image retained; it is not a pass.
