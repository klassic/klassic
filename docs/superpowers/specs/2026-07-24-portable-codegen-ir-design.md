# Portable codegen abstraction for klassic-native (phase 13)

## Status

Design approved by Kota 2026-07-24, via a live `brainstorming` session
following an explicit AskUserQuestion decision to attempt this now (see
"Scope decision" below). This is the second half of a two-part `/goal`:
"手つかずの部分完璧に実装して。そのあとにアーキテクチャ非依存部分をきりだす"
(perfectly implement the untouched parts, then extract the
architecture-independent parts). The first half (automatic GC
compaction) shipped as phase 11 of
`docs/superpowers/specs/2026-07-24-precise-concurrent-gc-design.md`; a
first, narrow slice of the second half (GC design constants and ABI
documentation, zero behavior change) shipped as that same document's
phase 12. This document covers the actual shared-codegen-abstraction
work phase 12 explicitly deferred.

## Scope decision

A stop-hook evaluating the `/goal` twice judged phase 12's `gc_layout`
extraction insufficient — it wanted the full shared instruction-emission
abstraction, not just constants and prose documentation. Rather than
either rush a large redesign to satisfy an automated judge or keep
resubmitting the same "it's deliberately scoped" argument, this was put
to Kota directly via `AskUserQuestion`, given the real stakes: this
touches the single largest, most load-bearing surface in the codebase
(~40,000 lines of hand-rolled x86-64 instruction emission), immediately
after a session that found and fixed a genuine self-deadlock bug in the
concurrent GC. **Kota explicitly chose "Attempt the full IR extraction
now."** That is a live, confirmed, high-stakes decision — not an
assumption, and not something to second-guess if this work resumes
later.

Within that decision, two further scope questions were also confirmed
live:
- **Success target this session**: build the shared abstraction and
  migrate the x86-64 backend to lower through it for a real (if
  intentionally small) slice of codegen — verified via the existing
  test suite. Explicitly **not** in scope: standing up a working
  AArch64 Linux direct-ELF backend, or expanding `aarch64.rs`'s
  feature set. Those become straightforward once the abstraction
  exists and is proven; attempting them in the same sitting as the
  abstraction's own design would be two architectures' worth of new
  work at once.
- **IR shape**: a trait-based emitter abstraction (`PortableAsm`),
  not a virtual-register IR with a register allocator, and not an
  explicit `Vec<Instruction>` value type that gets lowered in a
  separate pass. Rationale below.

## Why a trait, not a value-type IR

The existing backend compiles by walking the AST and emitting
instructions directly as it goes — there is no "build an IR tree, then
run a lowering pass" structure anywhere in `klassic-native`, and no
register allocator; every one of the 12 usable x86-64 registers
(`Rax, Rcx, Rdx, Rbx, Rsp, Rbp, Rsi, Rdi, R8-R11` — `R12-R15` are
deliberately unused, a long-standing register-budget constraint) is
manually assigned by the person writing each `compile_*`/`emit_*`
function.

Two alternatives were considered and rejected for this phase:

- **Virtual-register IR + register allocator** (the LLVM/Cranelift
  model): instructions reference unlimited virtual registers; a
  separate pass maps them onto each architecture's physical registers.
  Most powerful, and the right long-term answer if this project ever
  needs to target architectures with very different register
  conventions well. But a register allocator is a substantial project
  in its own right, and adopting this model means restructuring
  *how compilation works*, not just *what it targets* — far beyond
  what a single session, immediately after a deadlock-hunting session,
  should attempt on a 40k-line surface with no independent design
  review.
- **Explicit portable instruction list, physical registers**: a
  `Vec<IrOp>` using architecture-neutral mnemonics but still the same
  fixed physical-register-slot model as today (no allocator). Gets
  IR inspectability (you can print/dump it) that the trait approach
  lacks, but still requires restructuring "emit as you walk the AST"
  into "build a list, then lower" — a real structural change, for a
  benefit (inspectability) nothing in this codebase currently needs.

**Chosen: a trait (`PortableAsm`) covering the current `Assembler`'s
public surface, with a neutral `Reg`/`Condition` enum, keeping the
existing manual physical-register-assignment model exactly as it is.**
Migration is then "change which concrete type a function's emitter
parameter is," not "restructure how compilation is organized" — the
lowest-risk path for a 40k-line retrofit, and consistent with how this
campaign has always preferred incremental, narrowly-scoped, fully-
verified steps over large redesigns (see phases 9-10 of the concurrent-
GC document, which retrofitted colored-pointer barriers across ~150
call sites the same way: by category, full test suite green after
every step, coverage measured rather than assumed).

## Architecture

### New module: `crates/klassic-native/src/portable_asm.rs`

Follows the precedent this same campaign already set with
`gc_layout.rs`: a small, focused file living alongside `lib.rs`
(CLAUDE.md's "don't split lib.rs" guidance is about not fragmenting the
~40k lines of existing instruction-emission function bodies, not a ban
on new, genuinely separable files — `aarch64.rs`/`macho.rs`/
`cbackend.rs` already establish that new files are the normal way to
add a separable concern to this crate).

Contents:

- **`Reg` enum**: 12 variants, `V0..V11`, matching the x86-64 backend's
  existing 12-register budget. Deliberately neutral names (not `Rax`/
  `Rbx`) — a portable interface should not describe itself in terms of
  one architecture's register file. Each backend's `PortableAsm` impl
  decides which of its own physical registers each `V*` slot maps to;
  the x86-64 impl maps them 1:1 onto its current `Rax, Rcx, Rdx, Rbx,
  Rsp, Rbp, Rsi, Rdi, R8, R9, R10, R11` (in that order, preserving
  every existing call site's behavior exactly).
- **`Condition` enum**: the existing `Condition::{Equal, NotEqual, Less,
  LessOrEqual, Greater, GreaterOrEqual, Above, AboveOrEqual, Below,
  BelowOrEqual}` is already architecture-neutral in spirit (these are
  semantic comparison outcomes, not x86-64 condition-code mnemonics) —
  moves into this module largely unchanged.
- **`PortableAsm` trait**: covers the current `Assembler`'s public
  surface needed by the migrated slice (see "Migration slice" below)
  plus whatever else is needed as later slices land. Two associated
  types, since labels are opaque, backend-specific handles (indices
  into each backend's own internal tables) that can't be a single
  concrete type shared across architectures:
  The method list below is not a sketch — it was checked against the
  actual bodies of the three functions in the migration slice
  (`emit_print_i64_runtime`, `emit_write_data`, `emit_exit_code`) so
  the trait is complete enough for them on day one, not "roughly
  plausible":
  ```rust
  trait PortableAsm {
      type TextLabel: Copy;
      type DataLabel: Copy;

      fn push_reg(&mut self, r: Reg);
      fn mov_reg_reg(&mut self, dst: Reg, src: Reg);
      fn mov_imm64(&mut self, dst: Reg, imm: u64);
      fn sub_reg_imm8(&mut self, dst: Reg, imm: i8);
      fn lea_reg_rbp_disp8(&mut self, dst: Reg, disp: i8);
      fn cmp_reg_imm8(&mut self, r: Reg, imm: i8);
      fn dec_reg(&mut self, r: Reg);
      fn inc_reg(&mut self, r: Reg);
      fn xor_reg_reg(&mut self, dst: Reg, src: Reg);
      fn neg_reg(&mut self, r: Reg);
      fn div_reg(&mut self, divisor: Reg); // unsigned rdx:rax / divisor
      fn test_reg_reg(&mut self, a: Reg, b: Reg);
      fn leave(&mut self);
      fn ret(&mut self);
      fn syscall(&mut self);
      fn create_text_label(&mut self) -> Self::TextLabel;
      fn bind_text_label(&mut self, label: Self::TextLabel);
      fn jmp_label(&mut self, label: Self::TextLabel);
      fn jcc_label(&mut self, cond: Condition, label: Self::TextLabel);
      fn mov_data_addr(&mut self, dst: Reg, label: Self::DataLabel);

      // Byte-level store at [addr_reg] -- see "Byte sub-registers"
      // below for why these two exist instead of exposing x86-64's
      // Reg8/al/dl concept directly.
      fn store_byte_imm(&mut self, addr_reg: Reg, value: u8);
      fn store_byte_low_bits(&mut self, addr_reg: Reg, src: Reg);
      fn add_low_byte_imm(&mut self, r: Reg, imm: u8);

      // A syscall as one ABI-encapsulating call, not "poke registers
      // then call syscall()" -- see "Syscall ABI encapsulation" below
      // for why.
      fn syscall6(
          &mut self,
          number: u64,
          a1: Reg,
          a2: Reg,
          a3: Reg,
          a4: Reg,
          a5: Reg,
          a6: Reg,
      ) -> Reg; // returns the register now holding the result

      // Extended incrementally as later migration slices need more --
      // see "Non-goals" for why this starts intentionally incomplete.
  }
  ```

  **Syscall ABI encapsulation.** The obvious first draft exposed a
  bare `syscall()` method and let callers place arguments into
  `Reg::V0`/`V7`/`V6`/etc. themselves, mirroring how `emit_write_data`
  does it today (`mov_imm64(Rax, ...)`, `mov_data_addr(Rsi, ...)`,
  ..., `syscall()`, relying on x86-64's specific rax/rdi/rsi/rdx/r10/
  r8/r9 syscall calling convention). That does not generalize: *which*
  register holds the syscall number versus argument 1 versus argument
  2 is itself an architecture-specific ABI fact (x86-64: rax/rdi/rsi/
  rdx/r10/r8/r9; AArch64 Linux: x8/x0-x5) -- exposing raw register
  placement would silently bake x86-64's specific convention into
  every caller, exactly the leak this trait exists to prevent. Instead
  `syscall6` takes the syscall number and up to six argument *source*
  registers (unused trailing args passed as any placeholder register
  the specific syscall ignores -- matching how `emit_syscall_number`
  callers already only set as many argument registers as a given
  syscall needs); each backend's own impl is responsible for shuffling
  those values into whatever physical registers its own architecture's
  syscall ABI requires before trapping. `emit_write_data`/
  `emit_exit_code` call `syscall6` directly instead of the current
  three-step "load rax, load args, call `syscall()`" sequence.

  **Byte sub-registers.** `emit_print_i64_runtime`'s digit-extraction
  loop uses x86-64's aliased byte sub-registers directly (`Reg8::Dl`
  as the low 8 bits of `Rdx`, via `add_reg8_imm8`/`mov_byte_ptr_reg8`)
  -- a real x86-64-specific concept (register aliasing) that AArch64
  doesn't share the same way (it addresses bytes through load/store
  instruction *width*, not a parallel register file). Exposing `Reg8`
  through the portable trait would leak that asymmetry into every
  future backend. Instead the trait expresses the two *operations*
  `emit_print_i64_runtime` actually needs at the semantic level --
  "write a byte to memory" and "add an immediate to the low 8 bits of
  a register" -- letting the x86-64 impl realize them via `Reg8`
  exactly as today, while an AArch64 impl could realize the same
  operations via `strb`/byte-width `add` without ever needing an
  equivalent to `Reg8` to exist.
- **x86-64 adopts it**: the existing `Assembler` struct implements
  `PortableAsm` directly. Its `TextLabel`/`DataLabel` associated types
  are its own existing `TextLabel`/`DataLabel` newtypes, unchanged.
  Every trait method's body is either identical to the existing
  concrete method (renamed) or a one-line call to it — this impl block
  is additive and behavior-preserving by construction.

### Migration slice (this session)

A small, well-understood cluster of foundational runtime helpers,
chosen for minimal dependencies and easy byte-for-byte verification,
and deliberately **not** touching the GC/ZGC machinery just stabilized
this session (retrofitting that code again immediately after a
deadlock hunt would be needless regression risk for a proof-of-concept
migration):

- `emit_print_i64_runtime` (the ad-hoc integer-printing helper used
  throughout the file for runtime `println`/debug output)
- `emit_write_data` (raw `write(2)` of a fixed-length data label)
- `emit_exit_code` (raw `exit(2)` with a status code)

These three were picked because together they exercise a representative
slice of the trait's surface (immediate loads, register moves, data-
label addressing, syscalls, conditional branching, arithmetic via
`print_i64`'s digit-extraction loop) without pulling in GC-specific
concepts (colored pointers, atomics, the mark/sweep/compact machinery)
or control-flow-heavy expression compilation, either of which would
expand this slice's scope unpredictably.

Each becomes a free function in `portable_asm.rs`, generic over
`E: PortableAsm`, taking the emitter and any label values it needs as
explicit parameters (since `self.asm: Assembler` on
`NativeCodeGenerator` is a concrete field, not something a method on
`NativeCodeGenerator` itself can be generic over):

```rust
fn emit_print_i64<E: PortableAsm>(out: &mut E, print_i64: E::TextLabel) {
    // ... identical instruction sequence to the current
    // emit_print_i64_runtime, expressed through `out` instead of
    // `self.asm`.
}
```

`NativeCodeGenerator`'s existing `emit_print_i64_runtime` (etc.) becomes
a thin wrapper: `fn emit_print_i64_runtime(&mut self) {
portable_asm::emit_print_i64(&mut self.asm, self.print_i64); }`. Every
existing call site of `emit_print_i64_runtime`/`emit_write_data`/
`emit_exit_code` elsewhere in `lib.rs` needs no changes at all — the
public method name and signature on `NativeCodeGenerator` stay
identical; only their internal implementation now routes through the
portable trait.

### Non-goals (explicit, not silent scope creep)

- No register allocator; the physical-register-per-slot model stays
  exactly as it is today.
- No AArch64 Linux backend, no changes to `aarch64.rs` or `macho.rs`.
- No migration of the GC/ZGC machinery, the ~150 `__gc_*` builtins, or
  general expression compilation (`compile_expr` and friends) — this
  is explicitly a proof slice, not a claim that the retrofit is
  finished. Coverage should be tracked honestly (which functions are
  migrated vs. not) the same way phases 9-10 tracked colored-pointer
  barrier coverage, rather than declared "done" prematurely.
- The `PortableAsm` trait's method surface starts intentionally
  incomplete — it covers what the migration slice needs, not a
  speculative complete mirror of `Assembler`'s full API. Extending it
  is expected, ordinary work for whichever future slice needs a method
  it doesn't yet have.

## Testing

No new test infrastructure — this is a refactor, not new user-facing
capability. The existing 519-test suite is the correctness oracle: the
migrated functions must produce byte-identical emitted output (verified
indirectly by every existing test that exercises `println`-based output
or `exit` codes continuing to pass unchanged), plus `cargo fmt --check`
and `cargo clippy --all-targets --all-features -- -D warnings` clean.
No behavior is being added, so no new integration tests are needed for
this slice; a future slice that migrates something with previously-
undertested behavior should still add coverage the normal way.

## Error handling

Unchanged. `Result<NativeValue, Diagnostic>` propagation, the
`unsupported(span, "...")` diagnostic path, and every other existing
error-handling convention in `lib.rs` are untouched by this refactor —
the migrated functions are `emit_*_runtime` infrastructure functions
that don't return `Result` today and won't start to.
