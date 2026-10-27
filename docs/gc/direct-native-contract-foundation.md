# Shared leaves and direct native ABI contracts

This is a small dependency layer for [#683](https://github.com/klassic/klassic/issues/683),
stacked on the C runtime foundation in PR #685 and the demand fix in PR #686.
It adds native adapter layouts
and finite shared-leaf extension points. It does not install a direct runtime
dispatcher, publish a native binding, or implement thread attachment/pinning
through these tables. Those implementations remain in the later direct-runtime
change. The existing canonical C facade uses its own public APIs.

The C/Rust layouts describe creator-affine borrowed bindings and address-stable,
initially empty thread/pin tokens. Pending cleanup retains ownership. The headers
document statuses, root-only Native boundaries, and lifetime obligations. The
thread header includes the common binding header so both include orders work
without duplicate typedefs. Existing shared collector layouts remain unchanged.

Metadata and reference-lease emitters accept optional mutation/FULL hooks under
the caller's existing heap ticket. Hooks must preserve registers and cannot
poll or allocate. The original wrappers use empty hooks. Exclusive root cleanup
accepts a work quantum from 1 through 256 while preserving the original
registration/serial/cleaner checks; the original wrapper still clears at most
256 cells. These hooks do not implement growing backing or grant collector
termination credit by themselves.

Validation covers every new C/Rust table layout and field offset, both header
include orders, malformed native requests, and pending scope statuses. A Linux
x86_64 fixture executes real emitted metadata operations, checking exactly-once
mutation/FULL events and no mutation event on resolve/invalid release. It also
executes cleanup with quanta 1, 7 and 256, checking each cursor advance, untouched
suffix, preserved reuse generation and stale/exhausted cleaner rejection.
Its C function tables are contract fixtures, not proof of a registered runtime.

```sh
export CARGO_TARGET_DIR="$(mktemp -d)"
export PYTHONDONTWRITEBYTECODE=1
cargo build --workspace
cargo test --test gc_abi
cargo test -p klassic-native shared_native_leaf_hooks_and_cleanup_quantum_execute
cargo test
cargo clippy --workspace --all-targets -- -D warnings
cargo fmt --all -- --check
```

Build first: existing legacy C CLI tests locate `libklassic_runtime.a`, which
`cargo test` alone does not place at its expected archive path in a fresh target.
The initial full run without that build failed three existing CLI cases with
`libklassic_runtime.a not found`; this is separate from the contract checks.

The tests require a C11 compiler. No compiler invocation is added to the ordinary
Rust build or runtime path. The emitted instruction fixture is Linux x86_64;
this layer establishes no cross-platform runtime acceptance. Direct production
state/host bridges and unfinished G07 work are excluded. The base already includes
the demand-aware #675 fix from PR #686; this layer does not change that policy.
The original moving-cycle invariant in #676 remains unexplained. This layer
assigns no G01-G15 achievement count and does not complete #683.
