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
    fn ret(&mut self);

    fn mov_reg_reg(&mut self, dst: Reg, src: Reg);
    fn mov_imm64(&mut self, dst: Reg, imm: u64);
    fn mov_data_addr(&mut self, dst: Reg, label: Self::DataLabel);

    /// `dst = [base + disp]` (64-bit load).
    fn load_ptr_disp32(&mut self, dst: Reg, base: Reg, disp: i32);
    /// `[base + disp] = src` (64-bit store).
    fn store_ptr_disp32(&mut self, base: Reg, disp: i32, src: Reg);

    fn add_reg_reg(&mut self, dst: Reg, src: Reg);
    fn cmp_reg_reg(&mut self, a: Reg, b: Reg);
    fn add_reg_imm32(&mut self, dst: Reg, imm: i32);
    fn cmp_reg_imm32(&mut self, r: Reg, imm: i32);
    fn shl_reg_imm8(&mut self, r: Reg, imm: u8);
    fn shr_reg_imm8(&mut self, r: Reg, imm: u8);
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

impl PortableAsm for crate::Assembler {
    type TextLabel = crate::TextLabel;
    type DataLabel = crate::DataLabel;

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
    fn ret(&mut self) {
        crate::Assembler::ret(self);
    }
    fn mov_reg_reg(&mut self, dst: Reg, src: Reg) {
        crate::Assembler::mov_reg_reg(self, dst.into(), src.into());
    }
    fn mov_imm64(&mut self, dst: Reg, imm: u64) {
        crate::Assembler::mov_imm64(self, dst.into(), imm);
    }
    fn mov_data_addr(&mut self, dst: Reg, label: Self::DataLabel) {
        crate::Assembler::mov_data_addr(self, dst.into(), label);
    }
    fn load_ptr_disp32(&mut self, dst: Reg, base: Reg, disp: i32) {
        crate::Assembler::load_ptr_disp32(self, dst.into(), base.into(), disp);
    }
    fn store_ptr_disp32(&mut self, base: Reg, disp: i32, src: Reg) {
        crate::Assembler::store_ptr_disp32(self, base.into(), disp, src.into());
    }
    fn add_reg_reg(&mut self, dst: Reg, src: Reg) {
        crate::Assembler::add_reg_reg(self, dst.into(), src.into());
    }
    fn cmp_reg_reg(&mut self, a: Reg, b: Reg) {
        crate::Assembler::cmp_reg_reg(self, a.into(), b.into());
    }
    fn add_reg_imm32(&mut self, dst: Reg, imm: i32) {
        crate::Assembler::add_reg_imm32(self, dst.into(), imm);
    }
    fn cmp_reg_imm32(&mut self, r: Reg, imm: i32) {
        crate::Assembler::cmp_reg_imm32(self, r.into(), imm);
    }
    fn shl_reg_imm8(&mut self, r: Reg, imm: u8) {
        crate::Assembler::shl_reg_imm8(self, r.into(), imm);
    }
    fn shr_reg_imm8(&mut self, r: Reg, imm: u8) {
        crate::Assembler::shr_reg_imm8(self, r.into(), imm);
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
