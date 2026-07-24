//! Portable codegen abstraction: a trait-based instruction-emission
//! interface that migrated GC codegen functions are generic over,
//! instead of being hardcoded to the concrete x86-64 `Assembler`. This
//! is the first, incremental step toward making the hand-written ZGC
//! collector architecture-independent so a future AArch64 backend can
//! share the collector's design.
//!
//! Design (see `docs/superpowers/specs/2026-07-24-portable-codegen-ir-
//! design.md` for the full rationale): a *trait*, not a virtual-register
//! IR with a register allocator, and not a `Vec<Instruction>` value type
//! lowered in a separate pass. The existing backend emits instructions
//! as it walks the AST with every physical register manually assigned;
//! migration is then "change which concrete type a function's emitter
//! parameter is," not "restructure how compilation works" -- the
//! lowest-risk path for a ~40k-line retrofit.
//!
//! This module starts intentionally incomplete: it covers exactly what
//! the currently-migrated GC functions need, and grows one migration
//! slice at a time. In particular the platform primitives (mmap /
//! write / exit / clock) that some GC functions need -- which on this
//! codebase must handle three emission models (Linux syscall, macOS
//! syscall, Windows shim `call`) -- are deliberately NOT here yet; they
//! arrive with the first GC function that needs them, on a context type
//! that can carry `is_windows`/`platform` state the bare `Assembler`
//! lacks.

/// Portable general-purpose register slots. `V0..V11` match the x86-64
/// backend's 12 freely-assignable registers (mapped 1:1 onto
/// `Rax, Rcx, Rdx, Rbx, Rsp, Rbp, Rsi, Rdi, R8-R11` in declaration
/// order). The three GC-reserved color registers are named by their
/// semantic role rather than a neutral slot number: their meaning is
/// constant across the whole collector, and a future backend maps them
/// to any three of its own callee-saved registers -- the name survives
/// the remap where a bare `V13` would not.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[allow(dead_code)]
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
    /// GC load-barrier strip mask (x86-64: r13).
    ColorStrip,
    /// GC current good color (x86-64: r14).
    GoodColor,
    /// GC bad-color test mask (x86-64: r15).
    BadMask,
}

/// Semantic comparison outcomes for conditional jumps -- these describe
/// *what was compared*, not any architecture's condition-code
/// mnemonics, so they carry over unchanged from the x86-64 `Condition`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[allow(dead_code)]
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
    /// Parity flag set (x86-64 NaN-aware `ucomisd` follow-ups).
    Parity,
}

/// A portable instruction-emission interface. Covers exactly the
/// operations the currently-migrated GC functions need; extended one
/// migration slice at a time.
pub trait PortableAsm {
    /// Opaque handle to a not-yet-placed code-section label.
    type TextLabel: Copy;
    /// Opaque handle to a data-section label.
    type DataLabel: Copy;

    fn create_text_label(&mut self) -> Self::TextLabel;
    fn bind_text_label(&mut self, label: Self::TextLabel);
    fn jmp_label(&mut self, label: Self::TextLabel);
    fn jcc_label(&mut self, cond: Condition, label: Self::TextLabel);
    /// Direct call to a code-section label (pushes a return address).
    fn call_label(&mut self, label: Self::TextLabel);
    fn ret(&mut self);
    /// Standard frame teardown (`mov rsp, rbp; pop rbp` on x86-64).
    fn leave(&mut self);

    fn push_reg(&mut self, r: Reg);
    fn mov_reg_reg(&mut self, dst: Reg, src: Reg);
    fn mov_imm64(&mut self, dst: Reg, imm: u64);
    fn mov_data_addr(&mut self, dst: Reg, label: Self::DataLabel);

    /// `dst = [base + disp]` (64-bit load).
    fn load_ptr_disp32(&mut self, dst: Reg, base: Reg, disp: i32);
    /// `[base + disp] = src` (64-bit store).
    fn store_ptr_disp32(&mut self, base: Reg, disp: i32, src: Reg);
    /// `dst = [rbp - offset]` -- read a local frame slot.
    fn load_rbp_slot(&mut self, dst: Reg, offset: i32);
    /// `[rbp - offset] = src` -- write a local frame slot.
    fn store_rbp_slot(&mut self, offset: i32, src: Reg);

    fn add_reg_reg(&mut self, dst: Reg, src: Reg);
    fn sub_reg_reg(&mut self, dst: Reg, src: Reg);
    fn or_reg_reg(&mut self, dst: Reg, src: Reg);
    fn cmp_reg_reg(&mut self, a: Reg, b: Reg);
    /// Compare `a & b` against zero, setting flags (x86-64 `test`).
    fn test_reg_reg(&mut self, a: Reg, b: Reg);
    fn add_reg_imm32(&mut self, dst: Reg, imm: i32);
    fn sub_reg_imm8(&mut self, r: Reg, imm: i8);
    fn and_reg_imm32(&mut self, r: Reg, imm: i32);
    fn cmp_reg_imm32(&mut self, r: Reg, imm: i32);
    fn shl_reg_imm8(&mut self, r: Reg, imm: u8);
    fn shr_reg_imm8(&mut self, r: Reg, imm: u8);

    // Platform primitives. These are the genuinely platform-DEPENDENT
    // operations the collector needs -- the OS interface, not an
    // instruction encoding. Each backend implements them however its
    // platform requires (Linux/macOS `syscall`, a Windows shim `call`, a
    // future ARM-Linux `svc`), so that per-OS emission stays inside the
    // impl and never leaks into the portable GC code that calls them.
    /// Write `len` bytes at data label `data` to file descriptor `fd`.
    fn plat_write_data(&mut self, fd: u64, data: Self::DataLabel, len: usize);
    /// Terminate the process with status `code` (does not return).
    fn plat_exit(&mut self, code: u64);
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
            Reg::ColorStrip => crate::Reg::R13,
            Reg::GoodColor => crate::Reg::R14,
            Reg::BadMask => crate::Reg::R15,
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
            Condition::Parity => crate::Condition::Parity,
        }
    }
}

// The x86-64 backend implements `PortableAsm` on `NativeCodeGenerator`
// (not the bare `Assembler`) because the platform primitives need the
// generator's `is_windows`/`platform`/`windows` state: `plat_write_data`
// / `plat_exit` reuse the generator's existing, tested `emit_write_data`
// / `emit_exit_code` helpers, which already choose a Linux/macOS syscall
// vs a Windows shim `call`. The instruction methods are 1:1 thin
// wrappers over the inherent `Assembler` methods, so a migrated GC
// function emits byte-identical code. A future AArch64 backend would
// impl this same trait on its own generator type.
impl PortableAsm for crate::NativeCodeGenerator {
    type TextLabel = crate::TextLabel;
    type DataLabel = crate::DataLabel;

    fn create_text_label(&mut self) -> Self::TextLabel {
        self.asm.create_text_label()
    }
    fn bind_text_label(&mut self, label: Self::TextLabel) {
        self.asm.bind_text_label(label);
    }
    fn jmp_label(&mut self, label: Self::TextLabel) {
        self.asm.jmp_label(label);
    }
    fn jcc_label(&mut self, cond: Condition, label: Self::TextLabel) {
        self.asm.jcc_label(cond.into(), label);
    }
    fn call_label(&mut self, label: Self::TextLabel) {
        self.asm.call_label(label);
    }
    fn ret(&mut self) {
        self.asm.ret();
    }
    fn leave(&mut self) {
        self.asm.leave();
    }
    fn push_reg(&mut self, r: Reg) {
        self.asm.push_reg(r.into());
    }
    fn mov_reg_reg(&mut self, dst: Reg, src: Reg) {
        self.asm.mov_reg_reg(dst.into(), src.into());
    }
    fn mov_imm64(&mut self, dst: Reg, imm: u64) {
        self.asm.mov_imm64(dst.into(), imm);
    }
    fn mov_data_addr(&mut self, dst: Reg, label: Self::DataLabel) {
        self.asm.mov_data_addr(dst.into(), label);
    }
    fn load_ptr_disp32(&mut self, dst: Reg, base: Reg, disp: i32) {
        self.asm.load_ptr_disp32(dst.into(), base.into(), disp);
    }
    fn store_ptr_disp32(&mut self, base: Reg, disp: i32, src: Reg) {
        self.asm.store_ptr_disp32(base.into(), disp, src.into());
    }
    fn load_rbp_slot(&mut self, dst: Reg, offset: i32) {
        self.asm.load_rbp_slot(dst.into(), offset);
    }
    fn store_rbp_slot(&mut self, offset: i32, src: Reg) {
        self.asm.store_rbp_slot(offset, src.into());
    }
    fn add_reg_reg(&mut self, dst: Reg, src: Reg) {
        self.asm.add_reg_reg(dst.into(), src.into());
    }
    fn sub_reg_reg(&mut self, dst: Reg, src: Reg) {
        self.asm.sub_reg_reg(dst.into(), src.into());
    }
    fn or_reg_reg(&mut self, dst: Reg, src: Reg) {
        self.asm.or_reg_reg(dst.into(), src.into());
    }
    fn cmp_reg_reg(&mut self, a: Reg, b: Reg) {
        self.asm.cmp_reg_reg(a.into(), b.into());
    }
    fn test_reg_reg(&mut self, a: Reg, b: Reg) {
        self.asm.test_reg_reg(a.into(), b.into());
    }
    fn add_reg_imm32(&mut self, dst: Reg, imm: i32) {
        self.asm.add_reg_imm32(dst.into(), imm);
    }
    fn sub_reg_imm8(&mut self, r: Reg, imm: i8) {
        self.asm.sub_reg_imm8(r.into(), imm);
    }
    fn and_reg_imm32(&mut self, r: Reg, imm: i32) {
        self.asm.and_reg_imm32(r.into(), imm);
    }
    fn cmp_reg_imm32(&mut self, r: Reg, imm: i32) {
        self.asm.cmp_reg_imm32(r.into(), imm);
    }
    fn shl_reg_imm8(&mut self, r: Reg, imm: u8) {
        self.asm.shl_reg_imm8(r.into(), imm);
    }
    fn shr_reg_imm8(&mut self, r: Reg, imm: u8) {
        self.asm.shr_reg_imm8(r.into(), imm);
    }

    fn plat_write_data(&mut self, fd: u64, data: Self::DataLabel, len: usize) {
        self.emit_write_data(fd, data, len);
    }
    fn plat_exit(&mut self, code: u64) {
        self.emit_exit_code(code);
    }
}

/// Portable version of `gc_grow_budget` (raises the soft region budget
/// after an allocation stall). Entry: `V7` (rdi) = pending request's
/// total bytes. Grows the budget to `max(2*budget, committed + N)`
/// capped at `GC_RESERVE_REGIONS`, where `N = ceil(total / region)`.
/// Returns `V0` (rax) = 1 if the budget could grow enough, 0 if even the
/// whole reservation cannot hold the request. Leaf routine.
pub fn emit_gc_grow_budget<E: PortableAsm>(
    out: &mut E,
    entry: E::TextLabel,
    committed_count: E::DataLabel,
    budget_regions: E::DataLabel,
) {
    use crate::gc_layout::{GC_REGION_SHIFT, GC_REGION_SIZE, GC_RESERVE_REGIONS};

    out.bind_text_label(entry);
    let have = out.create_text_label();
    let set = out.create_text_label();
    let fail = out.create_text_label();

    // rcx = N = ceil(total / REGION_SIZE).
    out.mov_reg_reg(Reg::V1, Reg::V7);
    out.add_reg_imm32(Reg::V1, (GC_REGION_SIZE - 1) as i32);
    out.shr_reg_imm8(Reg::V1, GC_REGION_SHIFT);
    // r8 = committed + N (the minimum budget the request needs).
    out.mov_data_addr(Reg::V10, committed_count);
    out.load_ptr_disp32(Reg::V8, Reg::V10, 0);
    out.add_reg_reg(Reg::V8, Reg::V1);
    // rax = budget * 2.
    out.mov_data_addr(Reg::V10, budget_regions);
    out.load_ptr_disp32(Reg::V0, Reg::V10, 0);
    out.shl_reg_imm8(Reg::V0, 1);
    // rax = max(2*budget, committed+N).
    out.cmp_reg_reg(Reg::V0, Reg::V8);
    out.jcc_label(Condition::AboveOrEqual, have);
    out.mov_reg_reg(Reg::V0, Reg::V8);
    out.bind_text_label(have);
    // Cap at GC_RESERVE_REGIONS (unsigned region counts).
    let over_cap = out.create_text_label();
    out.cmp_reg_imm32(Reg::V0, GC_RESERVE_REGIONS as i32);
    out.jcc_label(Condition::Above, over_cap);
    out.jmp_label(set); // rax <= reserve: use it as-is
    out.bind_text_label(over_cap);
    // Over the cap: the request fits only if committed+N <= reserve.
    out.cmp_reg_imm32(Reg::V8, GC_RESERVE_REGIONS as i32);
    out.jcc_label(Condition::Above, fail);
    out.mov_imm64(Reg::V0, GC_RESERVE_REGIONS);
    out.bind_text_label(set);
    out.mov_data_addr(Reg::V10, budget_regions);
    out.store_ptr_disp32(Reg::V10, 0, Reg::V0);
    out.mov_imm64(Reg::V0, 1);
    out.ret();

    out.bind_text_label(fail);
    out.mov_imm64(Reg::V0, 0);
    out.ret();
}

/// Portable version of `gc_bounds_error`: a non-returning subroutine that
/// prints a fixed diagnostic to stderr and exits with status 1. The list
/// / string builtins jump here directly on a detected out-of-range index.
/// This is the first migration to exercise the platform primitives
/// (`plat_write_data` / `plat_exit`): the routine's control flow is
/// architecture-independent, while each backend's impl chooses the actual
/// OS interface (Linux/macOS `syscall` vs Windows shim `call`).
pub fn emit_gc_bounds_error<E: PortableAsm>(
    out: &mut E,
    entry: E::TextLabel,
    text: E::DataLabel,
    text_len: usize,
) {
    out.bind_text_label(entry);
    out.plat_write_data(2, text, text_len);
    out.plat_exit(1);
}

/// Portable version of `gc_drain`: run mark quanta until the worklist is
/// empty. Each iteration traces up to `GC_QUANTUM_POPS` objects via the
/// `gc_trace` subroutine, then loops while `gc_mark_worklist_top != 0`.
/// Sets up a frame (some callees expect an aligned stack) and tears it
/// down with `leave`.
pub fn emit_gc_drain<E: PortableAsm>(
    out: &mut E,
    entry: E::TextLabel,
    gc_trace: E::TextLabel,
    mark_worklist_top: E::DataLabel,
) {
    use crate::gc_layout::GC_QUANTUM_POPS;

    out.bind_text_label(entry);
    out.push_reg(Reg::V5); // rbp
    out.mov_reg_reg(Reg::V5, Reg::V4); // rbp = rsp
    let drain_loop = out.create_text_label();
    out.bind_text_label(drain_loop);
    out.mov_imm64(Reg::V7, GC_QUANTUM_POPS); // rdi = quantum
    out.call_label(gc_trace);
    out.mov_data_addr(Reg::V10, mark_worklist_top);
    out.load_ptr_disp32(Reg::V0, Reg::V10, 0);
    out.test_reg_reg(Reg::V0, Reg::V0);
    out.jcc_label(Condition::NotEqual, drain_loop);
    out.leave();
    out.ret();
}

/// Data-section labels the region-walking GC routines address. Grouped
/// so a migrated routine takes one parameter instead of a long,
/// easy-to-misorder list of individual `DataLabel`s.
#[derive(Clone, Copy)]
pub struct RegionTables<D: Copy> {
    pub heap_base: D,
    pub heap_top: D,
    pub region_base: D,
    pub region_top: D,
    pub committed_count: D,
    pub region_fromspace: D,
}

/// Portable version of `gc_clear_all_marks`: walk every committed region
/// and clear both header mark bits on each live block (an `and -16`,
/// which also leaves size intact since blocks are 16-aligned). Flushes
/// the current region's watermark first, and skips from-space (ghost)
/// regions so their forwarding words are preserved. Uses two frame slots
/// (`[rbp-8]` = region index, `[rbp-16]` = committed bound).
pub fn emit_gc_clear_all_marks<E: PortableAsm>(
    out: &mut E,
    entry: E::TextLabel,
    t: RegionTables<E::DataLabel>,
) {
    use crate::gc_layout::GC_REGION_SHIFT;

    out.bind_text_label(entry);
    out.push_reg(Reg::V5); // rbp
    out.mov_reg_reg(Reg::V5, Reg::V4); // rbp = rsp
    out.sub_reg_imm8(Reg::V4, 16);
    // Flush the current region's watermark so its blocks are covered.
    out.mov_data_addr(Reg::V10, t.heap_base);
    out.load_ptr_disp32(Reg::V11, Reg::V10, 0);
    out.mov_data_addr(Reg::V10, t.region_base);
    out.load_ptr_disp32(Reg::V10, Reg::V10, 0);
    out.sub_reg_reg(Reg::V11, Reg::V10);
    out.shr_reg_imm8(Reg::V11, GC_REGION_SHIFT);
    out.shl_reg_imm8(Reg::V11, 3);
    out.mov_data_addr(Reg::V10, t.region_top);
    out.add_reg_reg(Reg::V10, Reg::V11);
    out.mov_data_addr(Reg::V8, t.heap_top);
    out.load_ptr_disp32(Reg::V0, Reg::V8, 0);
    out.store_ptr_disp32(Reg::V10, 0, Reg::V0);
    // bound = committed_count; idx = 0.
    out.mov_data_addr(Reg::V10, t.committed_count);
    out.load_ptr_disp32(Reg::V0, Reg::V10, 0);
    out.store_rbp_slot(16, Reg::V0);
    out.mov_imm64(Reg::V0, 0);
    out.store_rbp_slot(8, Reg::V0);

    let region_loop = out.create_text_label();
    let region_done = out.create_text_label();
    let block_loop = out.create_text_label();
    let next_region = out.create_text_label();
    out.bind_text_label(region_loop);
    out.load_rbp_slot(Reg::V0, 8);
    out.load_rbp_slot(Reg::V1, 16);
    out.cmp_reg_reg(Reg::V0, Reg::V1);
    out.jcc_label(Condition::AboveOrEqual, region_done);
    // base (rsi) = region_base + idx<<SHIFT; top (r8) = region_top[idx].
    out.mov_reg_reg(Reg::V1, Reg::V0);
    out.shl_reg_imm8(Reg::V1, GC_REGION_SHIFT);
    out.mov_data_addr(Reg::V10, t.region_base);
    out.load_ptr_disp32(Reg::V10, Reg::V10, 0);
    out.add_reg_reg(Reg::V10, Reg::V1);
    out.mov_reg_reg(Reg::V6, Reg::V10);
    out.mov_reg_reg(Reg::V1, Reg::V0);
    out.shl_reg_imm8(Reg::V1, 3);
    out.mov_data_addr(Reg::V10, t.region_top);
    out.add_reg_reg(Reg::V10, Reg::V1);
    out.load_ptr_disp32(Reg::V8, Reg::V10, 0);
    out.cmp_reg_reg(Reg::V8, Reg::V6);
    out.jcc_label(Condition::Equal, next_region);
    // M7: skip from-space (ghost) regions (inert while evac is off).
    // clear_all_marks can run during a degrade while previous-cycle
    // ghosts still exist; their forwarding words must not be masked.
    out.load_rbp_slot(Reg::V0, 8);
    out.shl_reg_imm8(Reg::V0, 3);
    out.mov_data_addr(Reg::V10, t.region_fromspace);
    out.add_reg_reg(Reg::V10, Reg::V0);
    out.load_ptr_disp32(Reg::V0, Reg::V10, 0);
    out.test_reg_reg(Reg::V0, Reg::V0);
    out.jcc_label(Condition::NotEqual, next_region);
    // Walk blocks base..top, clearing both mark bits on each header
    // word0 (`and -16` also clears FWD, which is always 0 here since
    // no block is forwarded when the from-scratch mark runs).
    out.mov_reg_reg(Reg::V11, Reg::V6);
    out.bind_text_label(block_loop);
    out.cmp_reg_reg(Reg::V11, Reg::V8);
    out.jcc_label(Condition::AboveOrEqual, next_region);
    out.load_ptr_disp32(Reg::V0, Reg::V11, 0);
    out.and_reg_imm32(Reg::V0, -16); // size, marks cleared
    out.store_ptr_disp32(Reg::V11, 0, Reg::V0);
    out.add_reg_reg(Reg::V11, Reg::V0);
    out.jmp_label(block_loop);
    out.bind_text_label(next_region);
    out.load_rbp_slot(Reg::V0, 8);
    out.add_reg_imm32(Reg::V0, 1);
    out.store_rbp_slot(8, Reg::V0);
    out.jmp_label(region_loop);
    out.bind_text_label(region_done);
    out.leave();
    out.ret();
}

/// Text labels for the subroutines `gc_stw_mark_complete` calls.
#[derive(Clone, Copy)]
pub struct StwMarkTargets<T: Copy> {
    pub clear_all_marks: T,
    pub mark_roots: T,
    pub drain: T,
}

/// Portable version of `gc_stw_mark_complete`: the from-scratch STW
/// re-mark used to recover from a worklist overflow. Clears the fallback
/// flag, clears all marks, rescans roots, and drains to fixpoint; if the
/// flag is set AGAIN the live frontier genuinely exceeds capacity, so it
/// prints the overflow diagnostic and exits. The diagnostic uses the
/// platform primitives (`plat_write_data` / `plat_exit`).
pub fn emit_gc_stw_mark_complete<E: PortableAsm>(
    out: &mut E,
    entry: E::TextLabel,
    targets: StwMarkTargets<E::TextLabel>,
    stw_fallback_pending: E::DataLabel,
    mark_worklist_top: E::DataLabel,
    worklist_overflow_text: E::DataLabel,
    worklist_overflow_len: usize,
) {
    out.bind_text_label(entry);
    out.push_reg(Reg::V5); // rbp
    out.mov_reg_reg(Reg::V5, Reg::V4); // rbp = rsp
    // Clear the fallback flag: this from-scratch STW mark is the
    // recovery. If the worklist overflows AGAIN during this drain, the
    // live frontier genuinely exceeds capacity and we abort.
    out.mov_data_addr(Reg::V10, stw_fallback_pending);
    out.mov_imm64(Reg::V0, 0);
    out.store_ptr_disp32(Reg::V10, 0, Reg::V0);
    out.call_label(targets.clear_all_marks);
    // Reset the worklist top.
    out.mov_data_addr(Reg::V10, mark_worklist_top);
    out.mov_imm64(Reg::V0, 0);
    out.store_ptr_disp32(Reg::V10, 0, Reg::V0);
    out.call_label(targets.mark_roots);
    out.call_label(targets.drain);
    // If the flag is set again, the worklist overflowed even on the
    // from-scratch mark: genuine over-capacity, abort as before.
    out.mov_data_addr(Reg::V10, stw_fallback_pending);
    out.load_ptr_disp32(Reg::V0, Reg::V10, 0);
    let ok = out.create_text_label();
    out.test_reg_reg(Reg::V0, Reg::V0);
    out.jcc_label(Condition::Equal, ok);
    out.plat_write_data(2, worklist_overflow_text, worklist_overflow_len);
    out.plat_exit(1);
    out.bind_text_label(ok);
    out.leave();
    out.ret();
}

/// Portable version of `gc_free_ghost_regions`: walk every committed
/// region and, for each from-space (ghost) region, push its backing
/// store onto the free-region pool and reset the region's metadata
/// (top = base, fromspace = 0, live = 0). Runs at MarkEnd once the mark
/// has reached fixpoint, so no live slot still references a ghost. Uses
/// two frame slots (`[rbp-8]` = index, `[rbp-16]` = committed bound).
/// Reuses `RegionTables` (committed_count / region_fromspace /
/// region_base / region_top); `free_region_head` and `region_live` are
/// the two labels beyond that set.
pub fn emit_gc_free_ghost_regions<E: PortableAsm>(
    out: &mut E,
    entry: E::TextLabel,
    t: RegionTables<E::DataLabel>,
    free_region_head: E::DataLabel,
    region_live: E::DataLabel,
) {
    use crate::gc_layout::GC_REGION_SHIFT;

    out.bind_text_label(entry);
    out.push_reg(Reg::V5); // rbp
    out.mov_reg_reg(Reg::V5, Reg::V4); // rbp = rsp
    out.sub_reg_imm8(Reg::V4, 16);
    out.mov_data_addr(Reg::V10, t.committed_count);
    out.load_ptr_disp32(Reg::V0, Reg::V10, 0);
    out.store_rbp_slot(16, Reg::V0); // bound
    out.mov_imm64(Reg::V0, 0);
    out.store_rbp_slot(8, Reg::V0); // idx
    let loop_l = out.create_text_label();
    let done_l = out.create_text_label();
    let next_l = out.create_text_label();
    out.bind_text_label(loop_l);
    out.load_rbp_slot(Reg::V0, 8);
    out.load_rbp_slot(Reg::V1, 16);
    out.cmp_reg_reg(Reg::V0, Reg::V1);
    out.jcc_label(Condition::AboveOrEqual, done_l);
    // fromspace[idx]?
    out.mov_reg_reg(Reg::V1, Reg::V0);
    out.shl_reg_imm8(Reg::V1, 3);
    out.mov_data_addr(Reg::V10, t.region_fromspace);
    out.add_reg_reg(Reg::V10, Reg::V1);
    out.load_ptr_disp32(Reg::V11, Reg::V10, 0);
    out.test_reg_reg(Reg::V11, Reg::V11);
    out.jcc_label(Condition::Equal, next_l);
    // base = region_base + (idx << SHIFT)
    out.load_rbp_slot(Reg::V0, 8);
    out.shl_reg_imm8(Reg::V0, GC_REGION_SHIFT);
    out.mov_data_addr(Reg::V10, t.region_base);
    out.load_ptr_disp32(Reg::V10, Reg::V10, 0);
    out.add_reg_reg(Reg::V0, Reg::V10); // rax = base
    // push onto free pool: [base] = free_head; free_head = base
    out.mov_data_addr(Reg::V10, free_region_head);
    out.load_ptr_disp32(Reg::V11, Reg::V10, 0);
    out.store_ptr_disp32(Reg::V0, 0, Reg::V11);
    out.store_ptr_disp32(Reg::V10, 0, Reg::V0);
    // idx*8 in rcx for the three arrays.
    out.load_rbp_slot(Reg::V1, 8);
    out.shl_reg_imm8(Reg::V1, 3);
    // region_top[idx] = base (empty)
    out.mov_data_addr(Reg::V10, t.region_top);
    out.add_reg_reg(Reg::V10, Reg::V1);
    out.store_ptr_disp32(Reg::V10, 0, Reg::V0);
    // fromspace[idx] = 0 ; live[idx] = 0
    out.mov_imm64(Reg::V11, 0);
    out.mov_data_addr(Reg::V10, t.region_fromspace);
    out.add_reg_reg(Reg::V10, Reg::V1);
    out.store_ptr_disp32(Reg::V10, 0, Reg::V11);
    out.mov_data_addr(Reg::V10, region_live);
    out.add_reg_reg(Reg::V10, Reg::V1);
    out.store_ptr_disp32(Reg::V10, 0, Reg::V11);
    out.bind_text_label(next_l);
    out.load_rbp_slot(Reg::V0, 8);
    out.add_reg_imm32(Reg::V0, 1);
    out.store_rbp_slot(8, Reg::V0);
    out.jmp_label(loop_l);
    out.bind_text_label(done_l);
    out.leave();
    out.ret();
}

/// Data-section labels the incremental-mark worklist routines address.
#[derive(Clone, Copy)]
pub struct MarkWorklist<D: Copy> {
    pub header_mark: D,
    pub worklist: D,
    pub worklist_top: D,
    pub stw_fallback_pending: D,
}

/// Portable version of `gc_mark_visit(addr)` (V7/rdi = addr): if `addr`
/// points at an unmarked heap block, set its current-parity mark bit and
/// push it onto the trace worklist; a no-op when null or already marked.
/// Follows an M7 forwarding word first (a ghost redirects to its
/// to-space copy). On worklist overflow the mark bit is already set (so
/// the object is not lost, only its frontier entry), so it raises the
/// STW-fallback flag for a from-scratch re-mark. Note the routine emits
/// its fall-through `leave; ret` at the `bail` label, THEN the overflow
/// handler after it -- the emission order is preserved exactly.
pub fn emit_gc_mark_visit<E: PortableAsm>(
    out: &mut E,
    entry: E::TextLabel,
    w: MarkWorklist<E::DataLabel>,
) {
    use crate::gc_layout::{GC_FWD, GC_MARK_WORKLIST_LEN};

    out.bind_text_label(entry);
    out.push_reg(Reg::V5); // rbp
    out.mov_reg_reg(Reg::V5, Reg::V4); // rbp = rsp

    let bail = out.create_text_label();
    let overflow = out.create_text_label();

    // Null check.
    out.test_reg_reg(Reg::V7, Reg::V7);
    out.jcc_label(Condition::Equal, bail);
    // M7: follow forwarding first. If [rdi-16] is a forwarding word (FWD
    // bit set), rdi points at a ghost -- redirect to the to-space copy so
    // we mark (and later trace) the live object, not the ghost. ORing a
    // mark into a ghost's forwarding word would corrupt the address.
    // Inert while evac is off (FWD never set).
    out.load_ptr_disp32(Reg::V0, Reg::V7, -16);
    let not_fwd = out.create_text_label();
    out.mov_reg_reg(Reg::V1, Reg::V0);
    out.and_reg_imm32(Reg::V1, GC_FWD as i32);
    out.test_reg_reg(Reg::V1, Reg::V1);
    out.jcc_label(Condition::Equal, not_fwd);
    out.and_reg_imm32(Reg::V0, -16); // new user ptr
    out.mov_reg_reg(Reg::V7, Reg::V0);
    out.load_ptr_disp32(Reg::V0, Reg::V7, -16); // to-space header
    out.bind_text_label(not_fwd);
    // Already marked this cycle? (current-parity mark bit set)
    out.mov_data_addr(Reg::V10, w.header_mark);
    out.load_ptr_disp32(Reg::V1, Reg::V10, 0);
    out.test_reg_reg(Reg::V0, Reg::V1);
    out.jcc_label(Condition::NotEqual, bail);
    // Set the current mark bit.
    out.or_reg_reg(Reg::V0, Reg::V1);
    out.store_ptr_disp32(Reg::V7, -16, Reg::V0);
    // Push onto worklist: worklist[top++] = rdi
    out.mov_data_addr(Reg::V10, w.worklist_top);
    out.load_ptr_disp32(Reg::V0, Reg::V10, 0);
    out.cmp_reg_imm32(Reg::V0, GC_MARK_WORKLIST_LEN as i32);
    out.jcc_label(Condition::AboveOrEqual, overflow);
    out.mov_data_addr(Reg::V8, w.worklist);
    out.load_ptr_disp32(Reg::V8, Reg::V8, 0);
    // r9 = base + rax*8
    out.mov_reg_reg(Reg::V9, Reg::V0);
    out.shl_reg_imm8(Reg::V9, 3);
    out.add_reg_reg(Reg::V8, Reg::V9);
    out.store_ptr_disp32(Reg::V8, 0, Reg::V7);
    out.add_reg_imm32(Reg::V0, 1);
    out.store_ptr_disp32(Reg::V10, 0, Reg::V0);

    out.bind_text_label(bail);
    out.leave();
    out.ret();

    out.bind_text_label(overflow);
    // Worklist full. The object's mark bit is already set (above), so it
    // is not lost -- only its frontier (children-to-scan) entry is
    // dropped. Raise the STW-fallback flag: the incremental driver (or
    // gc_stw_mark_complete) will re-mark from scratch, which recovers the
    // dropped frontier. On the from-scratch STW path that re-mark checks
    // this flag after draining and aborts if it is still set (genuine
    // over-capacity).
    out.mov_data_addr(Reg::V10, w.stw_fallback_pending);
    out.mov_imm64(Reg::V0, 1);
    out.store_ptr_disp32(Reg::V10, 0, Reg::V0);
    out.leave();
    out.ret();
}
