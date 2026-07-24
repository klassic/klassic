# Portable Codegen Abstraction (Phase 13) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Introduce a trait-based, architecture-neutral instruction-emission interface (`PortableAsm`) in `klassic-native`, have the existing x86-64 `Assembler` implement it, and migrate three small, self-contained runtime helper functions to be generic over it, as a verified, behavior-preserving proof slice of "extract the architecture-independent parts" of the native backend.

**Architecture:** A new module `crates/klassic-native/src/portable_asm.rs` defines a neutral 12-register `Reg` enum (`V0..V11`), a `Condition` enum, and a `PortableAsm` trait with `TextLabel`/`DataLabel` associated types. The x86-64 `Assembler` implements the trait as a thin wrapper over its existing inherent methods (Rust resolves inherent methods first at every other, unmigrated call site in the crate, so this is purely additive). Three functions (`emit_print_i64_runtime`, `emit_write_data`, `emit_exit_code`) move their bodies into free functions in `portable_asm.rs`, generic over `E: PortableAsm`; `NativeCodeGenerator`'s existing methods of the same name become one-line wrappers that call them with `&mut self.asm`. No other call site in the ~40k-line file changes.

**Tech Stack:** Rust 2024, existing `klassic-native` crate (no new dependencies).

## Global Constraints

- Every task must leave `cargo build --release`, `cargo test --release` (full workspace suite, currently 519 tests, must stay 519 passed / 0 failed), `cargo fmt --check`, and `cargo clippy --all-targets --all-features -- -D warnings` all clean before it is considered done.
- This is a behavior-preserving refactor: no emitted binary's bytes should change. There is no direct byte-diff test for this, so correctness is verified indirectly — every existing test that exercises `println`/exit-code output continuing to pass with byte-identical `stdout`/`stderr`/exit codes is the evidence.
- No register allocator, no AArch64 Linux backend work, no changes to `aarch64.rs`/`macho.rs`, no migration of GC/ZGC machinery or any function beyond the three named above. See `docs/superpowers/specs/2026-07-24-portable-codegen-ir-design.md`'s "Non-goals" section — these are binding for this plan, not optional.
- Full design rationale (why a trait, why `syscall6`, why byte sub-registers are abstracted the way they are) lives in `docs/superpowers/specs/2026-07-24-portable-codegen-ir-design.md`. Read it before starting if anything below is unclear about *why*, not just *what*.
- Commit after every task, using the project's existing commit-message conventions (imperative mood, under ~72 char subject).

---

### Task 1: `portable_asm.rs` module, x86-64 impl, and first migrated function

**Files:**
- Create: `crates/klassic-native/src/portable_asm.rs`
- Modify: `crates/klassic-native/src/lib.rs` (add `mod portable_asm;` declaration; replace the body of `emit_print_i64_runtime`)

**Interfaces:**
- Produces: `portable_asm::Reg` (12-variant enum `V0..V11`), `portable_asm::Condition` (10-variant enum, identical names to the existing x86-64 `Condition`), `portable_asm::PortableAsm` trait (methods listed in Step 1 below), `impl PortableAsm for crate::Assembler`, `impl From<portable_asm::Reg> for crate::Reg`, `impl From<portable_asm::Condition> for crate::Condition`, and `portable_asm::emit_print_i64::<E: PortableAsm>(out: &mut E, print_i64: E::TextLabel, write_syscall_number: u64)`.
- Consumes (from existing `lib.rs`, all private items visible to `portable_asm` as a child module of the crate root): `crate::Assembler` and its inherent methods (`push_reg`, `mov_reg_reg`, `mov_imm64`, `sub_reg_imm8`, `lea_reg_rbp_disp8`, `cmp_reg_imm8`, `dec_reg`, `inc_reg`, `xor_reg_reg`, `neg_reg`, `div_reg`, `test_reg_reg`, `leave`, `ret`, `create_text_label`, `bind_text_label`, `jmp_label`, `jcc_label`, `mov_data_addr`, `mov_byte_ptr_reg_imm8`, `mov_byte_ptr_reg8`, `add_reg8_imm8`, `syscall`), `crate::Reg` (concrete x86-64 register enum, `Rax=0..R11=11`), `crate::Reg8` (`Al=0, Dl=2`), `crate::Condition` (concrete condition enum), `crate::TextLabel`/`crate::DataLabel` (opaque newtypes around `usize`).

This is the largest task in the plan because Rust's dead-code lint requires the trait's types to actually be *constructed* somewhere to avoid `clippy -- -D warnings` failing on an unused enum/trait — so the module, the x86-64 impl, and the first real caller must land together in one commit rather than three separate ones.

- [ ] **Step 1: Write `crates/klassic-native/src/portable_asm.rs`**

```rust
//! Portable codegen abstraction (phase 13): a trait-based instruction-
//! emission interface that migrated klassic-native codegen functions
//! are generic over, instead of being hardcoded to the concrete x86-64
//! `Assembler`. See `docs/superpowers/specs/2026-07-24-portable-codegen-
//! ir-design.md` for the full design rationale (why a trait rather than
//! a virtual-register IR, why syscalls and byte sub-registers need the
//! shapes they have below). This module starts intentionally
//! incomplete -- it covers exactly what the currently-migrated
//! functions need, extended as later migration slices need more.

/// Twelve portable general-purpose register slots, matching the x86-64
/// backend's existing 12-register budget (`Rax, Rcx, Rdx, Rbx, Rsp,
/// Rbp, Rsi, Rdi, R8-R11` -- `R12-R15` are deliberately unused
/// throughout this codebase). Each `PortableAsm` implementation decides
/// which of its own architecture's physical registers each slot maps
/// to; the x86-64 implementation maps them 1:1 onto its current
/// register enum in declaration order, preserving every migrated
/// function's behavior exactly.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Reg {
    V0,
    V1,
    V2,
    V3,
    V4,
    V5,
    V6,
    V7,
    V8,
    V9,
    V10,
    V11,
}

/// Semantic comparison outcomes for conditional jumps -- these describe
/// *what was compared*, not any architecture's condition-code
/// mnemonics, so they carry over unchanged from the x86-64 backend's
/// existing `Condition` enum.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Condition {
    Equal,
    NotEqual,
    Below,
    Above,
    AboveOrEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    NoOverflow,
}

/// A portable instruction-emission interface. See the module doc
/// comment and the design spec for what this deliberately does and
/// does not abstract over.
pub trait PortableAsm {
    /// Opaque handle to a not-yet-placed code-section label. Backend-
    /// specific (an index into that backend's own internal table);
    /// callers only ever create, bind, and reference these through
    /// this trait, never inspect them.
    type TextLabel: Copy;
    /// Opaque handle to a data-section label (e.g. an embedded byte
    /// string). Same opacity rules as `TextLabel`.
    type DataLabel: Copy;

    fn push_reg(&mut self, r: Reg);
    fn mov_reg_reg(&mut self, dst: Reg, src: Reg);
    fn mov_imm64(&mut self, dst: Reg, imm: u64);
    fn sub_reg_imm8(&mut self, dst: Reg, imm: i8);
    /// Load the effective address of `[rbp + disp]` into `dst`. `disp`
    /// is deliberately a single-byte displacement -- matches every
    /// existing x86-64 use, and keeping the range narrow is a
    /// reminder to extend this deliberately if a future caller needs
    /// more, not widen it speculatively now.
    fn lea_reg_rbp_disp8(&mut self, dst: Reg, disp: i8);
    fn cmp_reg_imm8(&mut self, r: Reg, imm: i8);
    fn dec_reg(&mut self, r: Reg);
    fn inc_reg(&mut self, r: Reg);
    fn xor_reg_reg(&mut self, dst: Reg, src: Reg);
    fn neg_reg(&mut self, r: Reg);
    /// Unsigned division, matching the x86-64 `div_reg`'s existing
    /// behavior exactly: divides the backend's own implicit
    /// dividend-register-pair by `divisor`, leaving the quotient and
    /// remainder wherever that architecture's division instruction
    /// naturally places them (on x86-64: rdx:rax / divisor -> quotient
    /// in rax, remainder in rdx).
    fn div_reg(&mut self, divisor: Reg);
    fn test_reg_reg(&mut self, a: Reg, b: Reg);
    /// Standard function-epilogue "restore caller's rbp/rsp" sequence.
    fn leave(&mut self);
    fn ret(&mut self);
    fn create_text_label(&mut self) -> Self::TextLabel;
    fn bind_text_label(&mut self, label: Self::TextLabel);
    fn jmp_label(&mut self, label: Self::TextLabel);
    fn jcc_label(&mut self, cond: Condition, label: Self::TextLabel);
    fn mov_data_addr(&mut self, dst: Reg, label: Self::DataLabel);

    /// Store the immediate byte `value` at `[addr_reg]`.
    fn store_byte_imm(&mut self, addr_reg: Reg, value: u8);
    /// Store the low 8 bits of `src` at `[addr_reg]`.
    fn store_byte_low_bits(&mut self, addr_reg: Reg, src: Reg);
    /// Add `imm` to the low 8 bits of `r`; the rest of `r` is left
    /// unspecified-but-unused by every current caller (matches the
    /// existing x86-64 `add_reg8_imm8`'s behavior: only the low byte
    /// is ever read back afterward by any caller in this codebase).
    fn add_low_byte_imm(&mut self, r: Reg, imm: u8);

    /// Issue a 6-argument syscall: `number` (a plain value the caller
    /// already computed from whatever platform/target lookup it has --
    /// syscall numbering is an ordinary architecture+OS fact, not an
    /// instruction-emission concern this trait abstracts) and up to
    /// six argument *source* registers. Unused trailing arguments may
    /// be any register value; the underlying syscall is expected to
    /// ignore them (matches how existing x86-64 syscall call sites
    /// already leave unused argument registers holding whatever they
    /// held before). Returns the register now holding the syscall's
    /// result (on x86-64: always `Reg::V0`, since results come back in
    /// rax and `V0` maps to rax).
    ///
    /// Deliberately takes source registers rather than letting callers
    /// place values into specific "argument registers" themselves --
    /// which physical register holds the syscall number versus
    /// argument 1 versus argument 2 is itself architecture-specific
    /// ABI knowledge (x86-64: rax/rdi/rsi/rdx/r10/r8/r9; AArch64
    /// Linux: x8/x0-x5) that belongs inside each backend's own
    /// implementation, not leaked into portable caller code. The
    /// current x86-64 implementation assumes argument sources don't
    /// alias a destination register written earlier in its internal
    /// shuffle order (rax, then rdi, then rsi, then rdx, then r10,
    /// then r8, then r9) -- true for every call site migrated so far,
    /// since each pre-loads its arguments into fresh registers
    /// immediately before the call. A future caller needing
    /// conflicting sources would need this method's implementation
    /// extended with a real register-shuffle algorithm first.
    fn syscall6(
        &mut self,
        number: u64,
        a1: Reg,
        a2: Reg,
        a3: Reg,
        a4: Reg,
        a5: Reg,
        a6: Reg,
    ) -> Reg;
}

impl From<Reg> for crate::Reg {
    fn from(r: Reg) -> crate::Reg {
        match r {
            Reg::V0 => crate::Reg::Rax,
            Reg::V1 => crate::Reg::Rcx,
            Reg::V2 => crate::Reg::Rdx,
            Reg::V3 => crate::Reg::Rbx,
            Reg::V4 => crate::Reg::Rsp,
            Reg::V5 => crate::Reg::Rbp,
            Reg::V6 => crate::Reg::Rsi,
            Reg::V7 => crate::Reg::Rdi,
            Reg::V8 => crate::Reg::R8,
            Reg::V9 => crate::Reg::R9,
            Reg::V10 => crate::Reg::R10,
            Reg::V11 => crate::Reg::R11,
        }
    }
}

impl From<Condition> for crate::Condition {
    fn from(c: Condition) -> crate::Condition {
        match c {
            Condition::Equal => crate::Condition::Equal,
            Condition::NotEqual => crate::Condition::NotEqual,
            Condition::Below => crate::Condition::Below,
            Condition::Above => crate::Condition::Above,
            Condition::AboveOrEqual => crate::Condition::AboveOrEqual,
            Condition::Less => crate::Condition::Less,
            Condition::LessEqual => crate::Condition::LessEqual,
            Condition::Greater => crate::Condition::Greater,
            Condition::GreaterEqual => crate::Condition::GreaterEqual,
            Condition::NoOverflow => crate::Condition::NoOverflow,
        }
    }
}

impl PortableAsm for crate::Assembler {
    type TextLabel = crate::TextLabel;
    type DataLabel = crate::DataLabel;

    fn push_reg(&mut self, r: Reg) {
        crate::Assembler::push_reg(self, r.into());
    }
    fn mov_reg_reg(&mut self, dst: Reg, src: Reg) {
        crate::Assembler::mov_reg_reg(self, dst.into(), src.into());
    }
    fn mov_imm64(&mut self, dst: Reg, imm: u64) {
        crate::Assembler::mov_imm64(self, dst.into(), imm);
    }
    fn sub_reg_imm8(&mut self, dst: Reg, imm: i8) {
        crate::Assembler::sub_reg_imm8(self, dst.into(), imm);
    }
    fn lea_reg_rbp_disp8(&mut self, dst: Reg, disp: i8) {
        crate::Assembler::lea_reg_rbp_disp8(self, dst.into(), disp);
    }
    fn cmp_reg_imm8(&mut self, r: Reg, imm: i8) {
        crate::Assembler::cmp_reg_imm8(self, r.into(), imm);
    }
    fn dec_reg(&mut self, r: Reg) {
        crate::Assembler::dec_reg(self, r.into());
    }
    fn inc_reg(&mut self, r: Reg) {
        crate::Assembler::inc_reg(self, r.into());
    }
    fn xor_reg_reg(&mut self, dst: Reg, src: Reg) {
        crate::Assembler::xor_reg_reg(self, dst.into(), src.into());
    }
    fn neg_reg(&mut self, r: Reg) {
        crate::Assembler::neg_reg(self, r.into());
    }
    fn div_reg(&mut self, divisor: Reg) {
        crate::Assembler::div_reg(self, divisor.into());
    }
    fn test_reg_reg(&mut self, a: Reg, b: Reg) {
        crate::Assembler::test_reg_reg(self, a.into(), b.into());
    }
    fn leave(&mut self) {
        crate::Assembler::leave(self);
    }
    fn ret(&mut self) {
        crate::Assembler::ret(self);
    }
    fn create_text_label(&mut self) -> Self::TextLabel {
        crate::Assembler::create_text_label(self)
    }
    fn bind_text_label(&mut self, label: Self::TextLabel) {
        crate::Assembler::bind_text_label(self, label);
    }
    fn jmp_label(&mut self, label: Self::TextLabel) {
        crate::Assembler::jmp_label(self, label);
    }
    fn jcc_label(&mut self, cond: Condition, label: Self::TextLabel) {
        crate::Assembler::jcc_label(self, cond.into(), label);
    }
    fn mov_data_addr(&mut self, dst: Reg, label: Self::DataLabel) {
        crate::Assembler::mov_data_addr(self, dst.into(), label);
    }
    fn store_byte_imm(&mut self, addr_reg: Reg, value: u8) {
        crate::Assembler::mov_byte_ptr_reg_imm8(self, addr_reg.into(), value);
    }
    fn store_byte_low_bits(&mut self, addr_reg: Reg, src: Reg) {
        // Only Reg::V2 (rdx/Dl) is ever passed here by any migrated
        // function so far -- see the design spec's "Byte sub-
        // registers" section. Extend this match if a future migrated
        // function needs a different register's low byte.
        let src_reg8 = match src {
            Reg::V2 => crate::Reg8::Dl,
            other => panic!(
                "store_byte_low_bits: no Reg8 mapping for {other:?} yet -- \
                 only Rdx/Dl is used by any migrated function so far"
            ),
        };
        crate::Assembler::mov_byte_ptr_reg8(self, addr_reg.into(), src_reg8);
    }
    fn add_low_byte_imm(&mut self, r: Reg, imm: u8) {
        let reg8 = match r {
            Reg::V2 => crate::Reg8::Dl,
            other => panic!(
                "add_low_byte_imm: no Reg8 mapping for {other:?} yet -- \
                 only Rdx/Dl is used by any migrated function so far"
            ),
        };
        crate::Assembler::add_reg8_imm8(self, reg8, imm);
    }
    fn syscall6(
        &mut self,
        number: u64,
        a1: Reg,
        a2: Reg,
        a3: Reg,
        a4: Reg,
        a5: Reg,
        a6: Reg,
    ) -> Reg {
        crate::Assembler::mov_imm64(self, crate::Reg::Rax, number);
        crate::Assembler::mov_reg_reg(self, crate::Reg::Rdi, a1.into());
        crate::Assembler::mov_reg_reg(self, crate::Reg::Rsi, a2.into());
        crate::Assembler::mov_reg_reg(self, crate::Reg::Rdx, a3.into());
        crate::Assembler::mov_reg_reg(self, crate::Reg::R10, a4.into());
        crate::Assembler::mov_reg_reg(self, crate::Reg::R8, a5.into());
        crate::Assembler::mov_reg_reg(self, crate::Reg::R9, a6.into());
        crate::Assembler::syscall(self);
        Reg::V0
    }
}

/// Portable version of `NativeCodeGenerator::emit_print_i64_runtime`.
/// Calling convention matches the existing runtime helper exactly:
/// `Reg::V7` (rdi) holds the signed 64-bit value to print, `Reg::V6`
/// (rsi) holds the destination file descriptor, `Reg::V2` (rdx) is a
/// newline flag (nonzero = append `\n`). Writes the decimal ASCII
/// representation via a single `write(2)` syscall using
/// `write_syscall_number` (computed by the caller from its own
/// platform/target lookup, since syscall numbering is not something
/// this trait abstracts).
pub fn emit_print_i64<E: PortableAsm>(
    out: &mut E,
    print_i64: E::TextLabel,
    write_syscall_number: u64,
) {
    out.bind_text_label(print_i64);
    out.push_reg(Reg::V5); // push rbp
    out.mov_reg_reg(Reg::V5, Reg::V4); // rbp = rsp
    out.sub_reg_imm8(Reg::V4, 48); // rsp -= 48 (48-byte digit buffer)
    out.mov_reg_reg(Reg::V10, Reg::V6); // r10 = rsi (fd), survives the digit loop
    out.mov_reg_reg(Reg::V0, Reg::V7); // rax = rdi (value)
    let after_newline = out.create_text_label();
    out.lea_reg_rbp_disp8(Reg::V6, 0); // rsi = &buf[0] (write cursor, grows downward)
    out.mov_imm64(Reg::V1, 0); // rcx = 0 (byte count so far)
    out.cmp_reg_imm8(Reg::V2, 0);
    out.jcc_label(Condition::Equal, after_newline);
    out.lea_reg_rbp_disp8(Reg::V6, -1);
    out.store_byte_imm(Reg::V6, b'\n');
    out.mov_imm64(Reg::V1, 1);
    out.bind_text_label(after_newline);
    out.cmp_reg_imm8(Reg::V0, 0);
    let nonzero = out.create_text_label();
    let digits = out.create_text_label();
    let digit_loop = out.create_text_label();
    let write = out.create_text_label();
    out.jcc_label(Condition::NotEqual, nonzero);
    out.dec_reg(Reg::V6);
    out.store_byte_imm(Reg::V6, b'0');
    out.inc_reg(Reg::V1);
    out.jmp_label(write);
    out.bind_text_label(nonzero);
    out.xor_reg_reg(Reg::V8, Reg::V8); // r8 = 0 (is-negative flag)
    out.cmp_reg_imm8(Reg::V0, 0);
    out.jcc_label(Condition::GreaterEqual, digits);
    out.neg_reg(Reg::V0);
    out.mov_imm64(Reg::V8, 1);
    out.bind_text_label(digits);
    out.mov_imm64(Reg::V3, 10); // rbx = 10 (divisor)
    out.bind_text_label(digit_loop);
    out.xor_reg_reg(Reg::V2, Reg::V2); // rdx = 0 (dividend high half)
    out.div_reg(Reg::V3); // rax /= 10, rdx = rax % 10
    out.add_low_byte_imm(Reg::V2, b'0'); // dl += '0'
    out.dec_reg(Reg::V6);
    out.store_byte_low_bits(Reg::V6, Reg::V2);
    out.inc_reg(Reg::V1);
    out.test_reg_reg(Reg::V0, Reg::V0);
    out.jcc_label(Condition::NotEqual, digit_loop);
    out.cmp_reg_imm8(Reg::V8, 0);
    out.jcc_label(Condition::Equal, write);
    out.dec_reg(Reg::V6);
    out.store_byte_imm(Reg::V6, b'-');
    out.inc_reg(Reg::V1);
    out.bind_text_label(write);
    out.syscall6(
        write_syscall_number,
        Reg::V10, // fd
        Reg::V6,  // buf
        Reg::V1,  // len
        Reg::V0,  // unused
        Reg::V0,  // unused
        Reg::V0,  // unused
    );
    out.leave();
    out.ret();
}
```

- [ ] **Step 2: Wire the module into `lib.rs` and replace `emit_print_i64_runtime`'s body**

Find the module declaration block (currently `mod cbackend;` / `mod aarch64;` / `mod macho;` near line 450):

```rust
#[allow(clippy::result_large_err)]
mod aarch64;
mod macho;
```

Change to:

```rust
#[allow(clippy::result_large_err)]
mod aarch64;
mod macho;
mod portable_asm;
```

Find `emit_print_i64_runtime`'s current body (search `fn emit_print_i64_runtime(&mut self) {`) and replace the entire function body with:

```rust
    fn emit_print_i64_runtime(&mut self) {
        let write_syscall_number = self.platform.syscall_number(PlatformSyscall::Write);
        portable_asm::emit_print_i64(&mut self.asm, self.print_i64, write_syscall_number);
    }
```

- [ ] **Step 3: Build**

Run: `cargo build --release 2>&1 | tail -40`
Expected: `Finished \`release\` profile [optimized] target(s) in ...` with no errors and no warnings. If there's a "never constructed" or "never used" warning on anything in `portable_asm`, re-check that `emit_print_i64` is actually called from the new `emit_print_i64_runtime` wrapper (Step 2) — that's what makes every type in the module reachable.

- [ ] **Step 4: Run the full test suite**

Run: `cargo test --release 2>&1 | tee /tmp/portable-asm-task1.txt | tail -20`
Then: `grep -n "FAILED\|test result:" /tmp/portable-asm-task1.txt`
Expected: every `test result:` line shows `0 failed`; the `cli_smoke` line shows `519 passed; 0 failed`. Any print-related test (search the file for `println` in its source) exercises this exact migrated function — a mismatch there is the first place to look if something's wrong.

- [ ] **Step 5: `cargo fmt --check` and `cargo clippy`**

Run: `cargo fmt --check 2>&1 | head -40`
Expected: no output (clean). If there's a diff, run `cargo fmt` (no `--check`) and re-verify with `--check`.

Run: `cargo clippy --all-targets --all-features -- -D warnings 2>&1 | tail -40`
Expected: `Finished` with no warnings.

- [ ] **Step 6: Commit**

```bash
git add crates/klassic-native/src/portable_asm.rs crates/klassic-native/src/lib.rs
git commit -m "feat: add PortableAsm trait, migrate emit_print_i64_runtime onto it"
```

---

### Task 2: Migrate `emit_write_data`

**Files:**
- Modify: `crates/klassic-native/src/portable_asm.rs` (add `emit_write_data` free function)
- Modify: `crates/klassic-native/src/lib.rs` (replace `emit_write_data`'s body)

**Interfaces:**
- Consumes: `portable_asm::{Reg, PortableAsm}` (from Task 1), same `crate::Assembler` inherent methods as before.
- Produces: `portable_asm::emit_write_data::<E: PortableAsm>(out: &mut E, write_syscall_number: u64, fd: u64, label: E::DataLabel, len: usize)`.

- [ ] **Step 1: Add `emit_write_data` to `portable_asm.rs`**

Append to `crates/klassic-native/src/portable_asm.rs` (after `emit_print_i64`):

```rust
/// Portable version of `NativeCodeGenerator::emit_write_data`: a raw
/// `write(2)` of `len` bytes starting at the data label `label` to
/// file descriptor `fd`, via a single syscall.
pub fn emit_write_data<E: PortableAsm>(
    out: &mut E,
    write_syscall_number: u64,
    fd: u64,
    label: E::DataLabel,
    len: usize,
) {
    out.mov_imm64(Reg::V7, fd); // rdi = fd
    out.mov_data_addr(Reg::V6, label); // rsi = &data
    out.mov_imm64(Reg::V2, len as u64); // rdx = len
    out.syscall6(
        write_syscall_number,
        Reg::V7,
        Reg::V6,
        Reg::V2,
        Reg::V0, // unused
        Reg::V0, // unused
        Reg::V0, // unused
    );
}
```

- [ ] **Step 2: Replace `emit_write_data`'s body in `lib.rs`**

Find (search `fn emit_write_data(&mut self, fd: u64, label: DataLabel, len: usize) {`):

```rust
    fn emit_write_data(&mut self, fd: u64, label: DataLabel, len: usize) {
        self.emit_syscall_number(PlatformSyscall::Write);
        self.asm.mov_imm64(Reg::Rdi, fd);
        self.asm.mov_data_addr(Reg::Rsi, label);
        self.asm.mov_imm64(Reg::Rdx, len as u64);
        self.asm.syscall();
    }
```

Replace with:

```rust
    fn emit_write_data(&mut self, fd: u64, label: DataLabel, len: usize) {
        let write_syscall_number = self.platform.syscall_number(PlatformSyscall::Write);
        portable_asm::emit_write_data(&mut self.asm, write_syscall_number, fd, label, len);
    }
```

Leave `emit_write_data_dynamic_len` (the sibling function right below it) untouched — it is not in this migration slice.

- [ ] **Step 3: Build**

Run: `cargo build --release 2>&1 | tail -40`
Expected: clean build, no errors or warnings.

- [ ] **Step 4: Run the full test suite**

Run: `cargo test --release 2>&1 | tee /tmp/portable-asm-task2.txt | tail -20`
Then: `grep -n "FAILED\|test result:" /tmp/portable-asm-task2.txt`
Expected: `cli_smoke` shows `519 passed; 0 failed`, every other suite `0 failed`. `emit_write_data` backs error-message output (e.g. `klassic gc: out of memory`, stack-overflow messages) throughout the file — any test asserting on `stderr` content is effectively testing this path.

- [ ] **Step 5: `cargo fmt --check` and `cargo clippy`**

Run: `cargo fmt --check 2>&1 | head -40` — expect clean.
Run: `cargo clippy --all-targets --all-features -- -D warnings 2>&1 | tail -40` — expect clean.

- [ ] **Step 6: Commit**

```bash
git add crates/klassic-native/src/portable_asm.rs crates/klassic-native/src/lib.rs
git commit -m "feat: migrate emit_write_data onto PortableAsm"
```

---

### Task 3: Migrate `emit_exit_code`

**Files:**
- Modify: `crates/klassic-native/src/portable_asm.rs` (add `emit_exit_code` free function)
- Modify: `crates/klassic-native/src/lib.rs` (replace `emit_exit_code`'s body)

**Interfaces:**
- Consumes: `portable_asm::{Reg, PortableAsm}` (from Task 1).
- Produces: `portable_asm::emit_exit_code::<E: PortableAsm>(out: &mut E, exit_syscall_number: u64, code: u64)`.

- [ ] **Step 1: Add `emit_exit_code` to `portable_asm.rs`**

Append to `crates/klassic-native/src/portable_asm.rs`:

```rust
/// Portable version of `NativeCodeGenerator::emit_exit_code`: a raw
/// `exit(2)` with the given status code, via a single syscall. Never
/// returns in practice (the process terminates), matching the
/// existing x86-64 helper's behavior.
pub fn emit_exit_code<E: PortableAsm>(out: &mut E, exit_syscall_number: u64, code: u64) {
    out.mov_imm64(Reg::V7, code); // rdi = code
    out.syscall6(
        exit_syscall_number,
        Reg::V7,
        Reg::V0, // unused
        Reg::V0, // unused
        Reg::V0, // unused
        Reg::V0, // unused
        Reg::V0, // unused
    );
}
```

- [ ] **Step 2: Replace `emit_exit_code`'s body in `lib.rs`**

Find (search `fn emit_exit_code(&mut self, code: u64) {`):

```rust
    fn emit_exit_code(&mut self, code: u64) {
        self.emit_syscall_number(PlatformSyscall::Exit);
        self.asm.mov_imm64(Reg::Rdi, code);
        self.asm.syscall();
    }
```

Replace with:

```rust
    fn emit_exit_code(&mut self, code: u64) {
        let exit_syscall_number = self.platform.syscall_number(PlatformSyscall::Exit);
        portable_asm::emit_exit_code(&mut self.asm, exit_syscall_number, code);
    }
```

- [ ] **Step 3: Build**

Run: `cargo build --release 2>&1 | tail -40`
Expected: clean build, no errors or warnings.

- [ ] **Step 4: Run the full test suite**

Run: `cargo test --release 2>&1 | tee /tmp/portable-asm-task3.txt | tail -20`
Then: `grep -n "FAILED\|test result:" /tmp/portable-asm-task3.txt`
Expected: `cli_smoke` shows `519 passed; 0 failed`, every other suite `0 failed`. `emit_exit_code` backs every non-zero-exit-code test in the suite (OOM aborts, stack-overflow aborts, bounds-check errors, and ordinary successful-exit `0`) — this is the most load-bearing of the three migrated functions in terms of test coverage.

- [ ] **Step 5: `cargo fmt --check` and `cargo clippy`**

Run: `cargo fmt --check 2>&1 | head -40` — expect clean.
Run: `cargo clippy --all-targets --all-features -- -D warnings 2>&1 | tail -40` — expect clean.

- [ ] **Step 6: Commit**

```bash
git add crates/klassic-native/src/portable_asm.rs crates/klassic-native/src/lib.rs
git commit -m "feat: migrate emit_exit_code onto PortableAsm"
```

---

### Task 4: Final verification sweep and documentation update

**Files:**
- Modify: `docs/superpowers/specs/2026-07-24-portable-codegen-ir-design.md` (add a "Landed" status note)
- Modify: `docs/superpowers/specs/2026-07-24-precise-concurrent-gc-design.md` (Phase 12 section: cross-reference that phase 13 actually implemented the extraction phase 12 deferred)

**Interfaces:** None (documentation only).

- [ ] **Step 1: Run the full verification sequence one more time, fresh**

```bash
cargo build --release 2>&1 | tail -20
cargo test --release 2>&1 | tee /tmp/portable-asm-final.txt | tail -20
grep -n "FAILED\|test result:" /tmp/portable-asm-final.txt
cargo fmt --check 2>&1 | head -40
cargo clippy --all-targets --all-features -- -D warnings 2>&1 | tail -40
```

Expected: every `test result:` line `0 failed` (519 in `cli_smoke`, matching the count before this plan started — this plan adds no new tests, since it changes no behavior), `fmt`/`clippy` both silent.

- [ ] **Step 2: Add a "Landed" note to the design spec**

Open `docs/superpowers/specs/2026-07-24-portable-codegen-ir-design.md`, find the `## Status` section at the top, and append a new paragraph after the existing status text:

```markdown

**Landed 2026-07-24.** `crates/klassic-native/src/portable_asm.rs` now
holds the `PortableAsm` trait, the neutral `Reg`/`Condition` enums, and
the x86-64 `Assembler`'s implementation of the trait. Three functions
are migrated onto it: `emit_print_i64_runtime`, `emit_write_data`,
`emit_exit_code` -- exactly the slice this document scoped, no more.
Every other call site in the ~40k-line file is unchanged. Full 519-test
suite green, `cargo fmt --check`/`clippy -- -D warnings` clean. The
trait's method surface remains intentionally incomplete (covers only
what these three functions need) -- extending it, and migrating further
functions, is ordinary follow-up work, not a new design decision.
```

- [ ] **Step 3: Cross-reference from the concurrent-GC design doc's phase 12 section**

Open `docs/superpowers/specs/2026-07-24-precise-concurrent-gc-design.md`, find the end of the "Phase 12" section (search for `"Recommended next step, not started:"` — the last paragraph of that section), and append immediately after it:

```markdown

**Update:** the recommended next step above was taken up as its own
design effort -- see
`docs/superpowers/specs/2026-07-24-portable-codegen-ir-design.md`
("Phase 13") for the actual `PortableAsm` trait design and its landed,
verified migration slice.
```

- [ ] **Step 4: Commit**

```bash
git add docs/superpowers/specs/2026-07-24-portable-codegen-ir-design.md docs/superpowers/specs/2026-07-24-precise-concurrent-gc-design.md
git commit -m "docs: record phase 13 -- PortableAsm trait landed and verified"
```

---

## Self-Review Notes (for whoever executes this plan)

- **Spec coverage:** Task 1 covers the spec's "Architecture" section (module, `Reg`/`Condition`, trait, x86-64 impl) and the first migrated function. Tasks 2-3 cover the remaining two functions in the spec's "Migration slice" section. Task 4 covers verification and leaves a clear paper trail, matching the spec's "Testing" section (existing suite as oracle, no new tests needed since no behavior changes).
- **Every trait method in Task 1's `PortableAsm` definition is called by at least one of the three migrated functions** — checked against the actual current bodies of `emit_print_i64_runtime`/`emit_write_data`/`emit_exit_code` in `crates/klassic-native/src/lib.rs` before writing this plan, not sketched from memory.
- **Registers used by each migrated function, traced for `syscall6` conflict-safety** (see the design spec's "Syscall ABI encapsulation" section for why this matters): `emit_print_i64`'s syscall uses sources `V10, V6, V1` for args 1-3, none of which alias an earlier-written destination (`rdi`, `rsi`, `rdx`) in the fixed shuffle order — verified by hand during planning. `emit_write_data` and `emit_exit_code` pre-load their arguments directly into `V7`/`V6`/`V2` (which alias `rdi`/`rsi`/`rdx` under the declared `Reg` mapping), making their shuffles a no-op by construction. If a future migration slice's syscall arguments don't follow this pattern, `syscall6`'s x86-64 implementation needs a real conflict-resolving shuffle before it can be trusted — flagged in the trait's own doc comment, not left implicit.
