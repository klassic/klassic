# Canonical C embedding-runtime foundation

This is a C-only dependency layer for [#683](https://github.com/klassic/klassic/issues/683),
based on the independent oracle/typechecking changes in PR #684. It does not
wire a Rust production collector, C/LLVM language emitter or executable CLI.
The source manifest and validation counts below describe the original PR #685
checkpoint. Later PR #686 supplies the demand-aware #675 fix in current checkouts.
The default collector and unsupported backend selection diagnostics are unchanged.
This C-only PR assigns no G01-G15 achievement counts. The separate ledger for the
larger opt-in workspace foundation currently says **0 met / 15 partial**; that
workspace assessment is not an assessment of this standalone C-only layer.

## Boundary

The seven C units implement the actual RootCell-only embedding facade, collector
driver, remembered backing, physical commitment/delay policy, checker, managed
UTF-8 adapter and checked scalar helpers. Shared allocation/metadata/retirement headers
change with them. All reachable production/test include files are identified by
[the source manifest](checkpoints/canonical-c-core-SHA256SUMS).

The core uses actual OS reservations and a single collector worker/actor. Young
and Old marking, promotion, relocation, reference repair and retirement use real
shared roots/queues/barriers/ownership receipts. Native callers explicitly attach;
2/8-owner, publication, lifetime and shared-moving-graph tests use actual threads.
Weak handles, byte/reference pins, callback/thread results, typed bulk operations
and logical reference CAS are C embedding APIs; this does not imply those language
constructs are implemented in ordinary compiled Klassic programs.

RootCells belong to their registered owner. Imports, stores and loads use the
rooted APIs; native pins keep their creator/token/lifetime until release finishes.
The checker grants no marking or retirement credit. Minimum backing retains
protected, reusable pages; explicit delay uses actual FREE state/incarnation and
monotonic time, with Native/guard-free busy ownership and OS work outside locks.
Failed native operations retain their receipts/charges, including partial effects;
permanent retry exhaustion fails explicitly. Worker terminal admission closes
under the common mutex before join, unpublication and metadata disposal.

The embedding default remains the prior adapter policy (zero delay). The later
CLI layer is responsible for common heap/default-delay normalization. This layer
adds no CLI flags, source selection or toolchain invocation to `cargo build`.
No Rust runtime-printer or ABI changes are needed by this C-only layer; those
remain with their eventual language-emitter/direct-runtime consumers.

## Validation and known gaps

The self-contained tests comprise the four existing C supervisors (17 tests,
128 C cases) and eight extracted pure-C/worker checks for pressure/stress, remembered
backing, real minimum/verification, OS retry/permanent failures, physical Young
survivors, UTF-8 and actual timed uncommit. They require a C compiler and can run
without a C/LLVM emitter or Rust production runtime. ABI and existing leaf tests
remain part of the baseline. Only completed source-point runs count as evidence.
The Linux x86_64 checkpoint passed the full `cargo test` run (864 passed, zero
failed or ignored), ABI 8/8, workspace Clippy and formatting. All 25 C integration tests
passed normally and with ASan+UBSan and TSan. The Linux C tests require Clang;
they fail when it is missing. They are excluded on other platforms, so these
results establish no corresponding platform acceptance.

```sh
export CARGO_TARGET_DIR="$(mktemp -d)"
export PYTHONDONTWRITEBYTECODE=1
export KLASSIC_GEN_RUNTIME_DIR="$PWD/runtime/gc"
cargo build --workspace
cargo test --test gc_abi
cargo test --test generational_c_runtime_core --test generational_c_atomic_ref --test generational_c_bulk --test generational_c_threads --test generational_nmutator_driver -- --test-threads=2
# Repeat with KLASSIC_GEN_SANITIZE=address and =thread; startup failures fail.
```

At the original PR #685 checkpoint, the C units and reachable headers are
byte-identical to the tested shutdown-plus-delay source subset derived from manifest
`3de0716b4db7c94e168b85dc7339a4502719426790d0a725dc7dd737fd2d2ae6`.
Unrelated direct native thread/scoped-pin ABI headers and Rust ABI additions are
excluded; the existing shared C/Rust layouts stay unchanged.

To verify that historical manifest, use a separate checkout at commit
`6ae963e`: `git worktree add --detach ../klassic-c-core-checkpoint 6ae963e`.
From that checkout run
`(cd runtime/gc && sha256sum -c ../../docs/gc/checkpoints/canonical-c-core-SHA256SUMS)`.
The manifest intentionally retains the original checkpoint hashes; PR #686 changes
five production source entries, so it is not a manifest of the latest checkout.

[#675](https://github.com/klassic/klassic/issues/675) identified a gap in the
original checkpoint: with 1023 of 1024 C lease tokens held, a multi-lease operation
could retry without progress. The demand-aware fix and its tests landed in
[PR #686](https://github.com/klassic/klassic/pull/686). The unfinished direct G07
prototype is excluded. The original direct moving-cycle invariant
in [#676](https://github.com/klassic/klassic/issues/676) is still unexplained;
passing C tests do not resolve it. Earlier failed C/LLVM timeout/startup runs also
remain failures, distinct from subsequent controlled passing runs. This C-only
layer cannot establish language/backend, hardware or bounded-latency acceptance.
