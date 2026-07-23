# Precise, thread-safe GC for klassic-native (ZGC-inspired scope)

## Status

Scope decision made without live user sign-off (no response arrived to the
brainstorming clarifying question before the session's goal-hook required
forward progress). **Flag this doc to Kota for review** — the scope call
below (ZGC-*inspired*, not literal ZGC) is the author's best engineering
judgment given the codebase's starting point, not a confirmed requirement.
If the intent was literally colored pointers / load barriers / concurrent
relocation, that is a much larger, different project — see "Rejected scope"
below for why it was set aside for now.

## Problem

The goal is "precise ZGC implementation (works in multi-thread environment)"
for klassic. Research into the current codebase (`crates/klassic-native/src/lib.rs`,
the x86_64 direct-ELF backend) found:

- The existing GC (`emit_gc_alloc_runtime`/`emit_gc_collect_runtime`,
  `lib.rs:38580-38967`) is **already precise**: an explicit shadow stack for
  roots (no conservative stack scanning) and type-tagged heap objects for
  exact tracing. This is a genuine head start.
- It is a **flat, non-moving, stop-the-world mark-sweep** collector — no
  regions, no generations, no relocation, no barriers of any kind.
- **There is no real OS-thread support in this backend at all.** `thread(f)`
  (`compile_thread`, `lib.rs:17159-17192`) is a compile-time trick: it queues
  the lambda body and inlines it into the single main-thread instruction
  stream (`emit_queued_threads`, `lib.rs:17203-17222`). No `clone`/`fork`/
  `futex` syscall is ever emitted; no `lock`-prefixed instruction, atomic op,
  or thread-local-storage mechanism exists anywhere in the hand-rolled x86_64
  assembler (`Assembler`, `lib.rs:41771+`).
- Every GC global (bump pointer, free-list head, root table, shadow stack,
  segment table) is an unsynchronized static cell. Safe today only because
  nothing else can run concurrently with it.

So "works in a multi-thread environment" is not an incremental extension —
it requires building real OS-thread support into this backend from nothing,
*and* making the GC's existing (good) precision survive that change safely.

## Scope decision

**Build:** real `clone()`-based OS threads in klassic-native, plus a
safepoint-based stop-the-world precise GC that is correctly synchronized
across however many real threads are running — preserving the existing
precise (exact shadow-stack + type-tagged) tracing design, generalized to
scan every live thread's roots.

**Not build (this round):** literal ZGC internals — colored pointers,
multi-mapped memory, load barriers on every heap reference, concurrent
marking/relocation while mutators keep running.

### Rejected scope: literal ZGC

ZGC's actual design (colored pointers using spare virtual-address bits,
memory multi-mapped at 3-4 different virtual ranges per color, a load
barrier instrumented into *every single heap pointer load* in the compiled
program, and concurrent region evacuation) is one of the most sophisticated
pieces of systems software that exists. Attempting it here, on top of:

- a from-scratch hand-rolled x86_64 encoder with no `lock`-prefixed
  instructions, no TLS, and no indirect call/jmp support yet,
- a backend with *zero* existing OS-thread support to build the rest on,
- no external assembler/linker/debugger-friendly toolchain to iterate with,

would not produce a working system in any bounded amount of effort, and a
half-built version (e.g. colored pointers without a working load barrier,
or a load barrier that isn't applied at every one of the ~40k lines' worth
of pointer-load call sites) is worse than not attempting it — it would be
silently unsound. A stop-the-world, safepoint-based collector is the
standard, well-understood, *correct* alternative that still delivers on the
two properties that actually matter for the stated goal: **precise** roots
and heap tracing (already true), and **provably correct under real
concurrent OS threads** (the genuinely new, hard part). "ZGC-inspired" here
means: low-pause-*intent*, region-flavored heap segments (already present as
the existing 1 MiB growth segments), and a design that a *future* pass could
still evolve toward concurrent marking — not a claim of ZGC's actual
algorithm.

## Design

### 1. New `Assembler` primitives (`lib.rs`, `Assembler` impl)

- `xchg_mem_reg64(base: Reg, disp: i32, reg: Reg)` — `xchg` with a memory
  operand is implicitly `lock`-equivalent on x86; used for the spinlock
  acquire (atomic swap).
- `mov_imm32_mem` / plain store for the spinlock release (a plain store of 0
  is safe as a mutex unlock on x86's TSO memory model — no fence needed).
- `lock_cmpxchg_mem_reg64(base: Reg, disp: i32, reg: Reg)` — `F0 REX.W 0F B1
  /r`; used for the thread-registry slot claim (CAS an empty-slot sentinel
  to a new thread's id) and any future lock-free needs.
- `lock_xadd_mem_reg64(base: Reg, disp: i32, reg: Reg)` — `F0 REX.W 0F C1
  /r`; used for the global thread counter / next-thread-index allocation.

Each is a small, independently unit-testable addition (encode a tiny
snippet, run it, assert the register/memory result) before anything else
depends on it.

### 2. `PlatformSyscall::Clone`

- New enum variant, syscall number 56 (Linux x86_64), added to
  `syscall_numbers` table + the `PLATFORM_SYSCALL_COUNT` test.
- `emit_clone(flags: u64, child_stack_reg: Reg)`: loads
  `rdi=flags, rsi=child_stack, rdx=0 (ptid unused), r10=0 (ctid unused),
  r8=0 (tls unused)` then `syscall()`. Flags:
  `CLONE_VM | CLONE_FS | CLONE_FILES | CLONE_THREAD | CLONE_SIGHAND |
  CLONE_SYSVSEM` (share address space + thread group; **no** `CLONE_SETTLS`
  — this backend has no TLS mechanism and doesn't need one, since per-thread
  state lives in the thread registry, indexed by a value passed to the new
  thread on its own stack, not accessed via a segment register).
- Parent/child branch on `rax` (`> 0` = parent, got child tid; `== 0` =
  child) exactly like every other syscall's error-branch pattern already in
  this file.

### 3. Per-thread stack

- `mmap(NULL, THREAD_STACK_SIZE, PROT_READ|PROT_WRITE,
  MAP_PRIVATE|MAP_ANONYMOUS, -1, 0)` — same call shape as
  `emit_initialize_gc_heap`'s heap mmap (`lib.rs:38472-38482`).
  `THREAD_STACK_SIZE = 2 MiB` initially (fixed, no growth — a native thread
  overflowing 2 MiB is a clean-diagnostic case for a later pass, out of
  scope here).
- `child_stack = mapped_base + THREAD_STACK_SIZE` (stack grows down, same
  as the main thread) passed as `clone`'s `rsi`.
- The mmap'd base/size, plus a slot for "this thread's shadow-stack
  sub-range base/top/end" and a "parked" flag, are recorded in a new fixed
  **thread registry**: `data_label_with_i64s(&vec![0; SLOTS_PER_THREAD *
  MAX_THREADS])`, same declaration pattern as `gc_segments`
  (`lib.rs:4149`). `MAX_THREADS = 64` initially (matches `GC_MAX_SEGMENTS`'s
  existing precedent of a fixed cap with a clean-diagnostic overflow path,
  not silent truncation).

### 4. Per-thread shadow stack (the actual GC-correctness fix)

The existing shadow stack (`gc_shadow_stack`/`gc_shadow_stack_top`,
`lib.rs:3955-3956`) is one global array. Under real threads this is an
immediate data race and root-set corruption hazard. Fix: carve the existing
fixed-size shadow-stack region into `MAX_THREADS` fixed-size sub-ranges
(`GC_SHADOW_STACK_LEN / MAX_THREADS` entries each) at startup; each thread's
registry entry stores its own sub-range's current top. `emit_gc_shadow_push`/
`emit_gc_shadow_pop_n` are changed to compute their base from "my thread
index's sub-range" instead of the single global — the thread index is
threaded through as an argument (passed in a fixed register by convention
at every call site that currently calls these two helpers, of which there
are a bounded, greppable number) rather than a TLS read, consistent with
"no TLS in this backend" from §2.

### 5. Safepoint-based stop-the-world collection

- New global cells: `gc_safepoint_requested` (i64 flag), and each thread
  registry entry gets a `parked` flag (already covered by §3's per-entry
  layout).
- **Every thread**, at the *same* points where it already checks the
  stack-overflow floor (function prologue — `lib.rs:38089-38093` — a
  natural, already-frequent checkpoint with no new overhead added anywhere
  it wasn't already probing), additionally checks `gc_safepoint_requested`;
  if set, it sets its own `parked` flag and spin-waits (with a `pause`
  instruction in the loop body) until the flag clears.
- **The collecting thread** (whichever thread's allocation triggered GC):
  sets `gc_safepoint_requested`, spin-waits until every registered thread's
  `parked` flag is set (skipping itself and any registry slot marked empty),
  performs mark-sweep using **all** threads' shadow-stack sub-ranges plus
  the existing static root table as roots (the existing `gc_mark_visit`
  logic, `lib.rs:38805-38871`, is unchanged — only the root-enumeration step
  widens from "the one shadow stack" to "every live thread's sub-range"),
  then clears `gc_safepoint_requested`, releasing every parked thread.
- The allocator's bump-pointer/free-list read-modify-write
  (`emit_gc_alloc_attempt`, `lib.rs:38635-38708`) is wrapped in the new
  spinlock (§1) so concurrent allocations from different threads don't
  race the same free-list/bump-pointer cells, independent of the
  stop-the-world protocol (allocation is far more frequent than collection
  and must stay fast/uncontended in the common case — a spinlock around a
  handful of instructions is the right tool, not a syscall-based futex).

### 6. Per-thread stack-overflow floor

`stack_floor` (`lib.rs:3974-3979`) is computed once from the main thread's
`rsp`/`getrlimit`. Fix: move the floor into the thread registry entry too
(computed at thread-creation time from the mmap'd stack's known base, not
`getrlimit`, since a mmap'd region's size is exactly known — simpler than
the main thread's `getrlimit`-based computation). The function-prologue
probe (`lib.rs:38089-38093`) reads "my thread's floor" via the same
thread-index convention as §4, instead of the single global cell.

### 7. `exit` vs `exit_group`

Main-thread/program-level exit (`emit_exit_code`/`emit_exit_success`,
`lib.rs:38030`/`37698`) must switch from `PlatformSyscall::Exit` (60,
single-thread) to a new `PlatformSyscall::ExitGroup` (231) so program exit
correctly terminates every thread, not just the calling one. A thread that
finishes its own body (not the whole program) uses plain `Exit` (60) — new
codegen for its exit stub.

### 8. Language surface: unchanged for now

`thread(f)`'s existing compile-time-inline semantics stay as-is in this
phase. The primitives above are proven via a new, narrowly-scoped internal
capability (not yet reachable from Klassic source) so each piece is
independently testable before touching the existing, tested, deterministic
`thread()` behavior (`tests/cli_smoke.rs:16874-16914` currently asserts
exact deterministic output from two `thread()` calls — redefining `thread()`
to spawn real racy OS threads is a real language-semantics change deserving
its own explicit decision later, once the underlying primitives are proven
solid).

## Phased plan (implementation, PR-sized)

1. **Assembler atomics** (§1) — `xchg_mem_reg64`, `lock_cmpxchg_mem_reg64`,
   `lock_xadd_mem_reg64`. Unit-testable via `klassic-native`'s own Rust
   `#[test]`s that assemble a tiny snippet and execute it (existing pattern:
   `crates/klassic-native/src/lib.rs`'s `mod tests` already does this for
   other instruction encoders).
2. **`clone` + per-thread stack + a trivial two-thread smoke program**
   (§2, §3, §7's `Exit`-vs-`ExitGroup` split) — two threads each write a
   distinct byte to a shared buffer via direct syscalls (no GC heap
   involvement yet), main thread spin-joins both, program exits 0. Proves
   `clone` + stack setup + exit-group correctness before GC changes land.
3. **Thread-safe allocation** (§1's spinlock wired into `emit_gc_alloc_attempt`,
   §4's per-thread shadow stack) — two threads each allocate N records in a
   loop and assert (via the shared spinlock-protected counter from step 1's
   primitives) that the final live count is exactly `2N`, which only holds
   if the allocator is race-free.
4. **Safepoint stop-the-world collection** (§5) — extend step 3's test so
   both threads keep allocating past the heap's initial capacity, forcing a
   real GC cycle mid-run; assert the program completes correctly (no lost
   roots, no double-free, no corrupted free list) and `gc_collect_counter`
   confirms at least one real cycle ran during concurrent allocation.
5. **Per-thread stack-overflow floor** (§6) — a thread that recurses deep
   enough to overflow its own 2 MiB stack gets the existing clean
   diagnostic, not a wild SIGSEGV or (worse) a false-positive probe against
   the *main* thread's floor.
6. **(Follow-up, not this round)** — decide and implement how/whether
   `thread()`'s surface semantics change to use this real infrastructure.

## Testing strategy

Each phase gets its own `tests/cli_smoke.rs`-style integration test (or, for
step 1's pure-assembler pieces, a `klassic-native` crate-level unit test),
following the project's established "one focused change, one test" pattern.
Concurrency correctness is proven by **arithmetic invariants under load**
(exact counts, exact sums) rather than by timing/sleeps, since counts are
deterministic regardless of actual thread interleaving while still only
holding true if synchronization is correct — a race produces a *wrong
count*, not just nondeterministic timing, so these tests fail reliably on a
regression instead of being flaky.

All new codegen is Linux x86_64 (`DirectX86_64` backend) only — `aarch64.rs`,
`macho.rs`, `cbackend.rs` are untouched by this design.
