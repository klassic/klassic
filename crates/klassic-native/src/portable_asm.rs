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
    fn pop_reg(&mut self, r: Reg);
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
    fn and_reg_reg(&mut self, dst: Reg, src: Reg);
    fn or_reg_reg(&mut self, dst: Reg, src: Reg);
    fn xor_reg_reg(&mut self, dst: Reg, src: Reg);
    fn cmp_reg_reg(&mut self, a: Reg, b: Reg);
    /// Compare `a & b` against zero, setting flags (x86-64 `test`).
    fn test_reg_reg(&mut self, a: Reg, b: Reg);
    fn add_reg_imm32(&mut self, dst: Reg, imm: i32);
    fn sub_reg_imm8(&mut self, r: Reg, imm: i8);
    fn sub_reg_imm32(&mut self, r: Reg, imm: i32);
    fn and_reg_imm32(&mut self, r: Reg, imm: i32);
    fn cmp_reg_imm8(&mut self, r: Reg, imm: i8);
    fn cmp_reg_imm32(&mut self, r: Reg, imm: i32);
    fn shl_reg_imm8(&mut self, r: Reg, imm: u8);
    fn shr_reg_imm8(&mut self, r: Reg, imm: u8);
    /// Bit-test: copy bit `bit` of `r` into the carry flag (x86-64 `bt`),
    /// so a following Below/AboveOrEqual jump branches on it without a
    /// scratch register.
    fn bt_reg_imm8(&mut self, r: Reg, bit: u8);
    /// Block copy of V1 (rcx) bytes from [V6/rsi] to [V7/rdi] (x86-64
    /// `rep movsb`); clobbers V1/V6/V7. A byte memcpy primitive; a future
    /// backend lowers it to its own copy loop/instruction.
    fn rep_movsb(&mut self);

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
    /// Read the monotonic clock in nanoseconds into `V0`. The clock
    /// source and its timespec ABI are platform-specific (a Linux/macOS
    /// `clock_gettime` syscall here), so the whole read is one primitive
    /// rather than a syscall op plus portable arithmetic. Clobbers scratch
    /// registers (V0/V1/V8 on x86-64) like any subroutine-free helper.
    fn plat_read_monotonic_ns(&mut self);
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
    fn pop_reg(&mut self, r: Reg) {
        self.asm.pop_reg(r.into());
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
    fn and_reg_reg(&mut self, dst: Reg, src: Reg) {
        self.asm.and_reg_reg(dst.into(), src.into());
    }
    fn or_reg_reg(&mut self, dst: Reg, src: Reg) {
        self.asm.or_reg_reg(dst.into(), src.into());
    }
    fn xor_reg_reg(&mut self, dst: Reg, src: Reg) {
        self.asm.xor_reg_reg(dst.into(), src.into());
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
    fn sub_reg_imm32(&mut self, r: Reg, imm: i32) {
        self.asm.sub_reg_imm32(r.into(), imm);
    }
    fn and_reg_imm32(&mut self, r: Reg, imm: i32) {
        self.asm.and_reg_imm32(r.into(), imm);
    }
    fn cmp_reg_imm8(&mut self, r: Reg, imm: i8) {
        self.asm.cmp_reg_imm8(r.into(), imm);
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
    fn bt_reg_imm8(&mut self, r: Reg, bit: u8) {
        self.asm.bt_reg_imm8(r.into(), bit);
    }
    fn rep_movsb(&mut self) {
        self.asm.rep_movsb();
    }

    fn plat_write_data(&mut self, fd: u64, data: Self::DataLabel, len: usize) {
        self.emit_write_data(fd, data, len);
    }
    fn plat_exit(&mut self, code: u64) {
        self.emit_exit_code(code);
    }
    fn plat_read_monotonic_ns(&mut self) {
        self.emit_read_monotonic_ns_to_rax();
    }
}

/// The three `--gc-log` STW-pause accumulator cells, grouped so pause
/// timing threads through the collector as one parameter.
#[derive(Clone, Copy)]
pub struct PauseCells<D: Copy> {
    /// Timestamp captured at pause entry.
    pub start_ns: D,
    /// Running sum of all pause durations.
    pub total_ns: D,
    /// Longest single pause seen.
    pub max_ns: D,
}

/// Portable version of the `gc_pause_start` STW-pause helper: when
/// `timing` is set (the `--gc-log` build on a platform whose clock the
/// backend supports), capture the monotonic clock into `cells.start_ns`;
/// otherwise emit nothing. Inlined at the start of every STW pause.
pub fn emit_gc_pause_start<E: PortableAsm>(
    out: &mut E,
    timing: bool,
    cells: PauseCells<E::DataLabel>,
) {
    if timing {
        out.plat_read_monotonic_ns();
        out.mov_data_addr(Reg::V10, cells.start_ns);
        out.store_ptr_disp32(Reg::V10, 0, Reg::V0);
    }
}

/// Portable version of the `gc_pause_end` STW-pause helper: when `timing`
/// is set, fold the elapsed pause (`now - cells.start_ns`) into the total
/// and max accumulators; otherwise emit nothing. Inlined at the end of
/// every STW pause.
pub fn emit_gc_pause_end<E: PortableAsm>(
    out: &mut E,
    timing: bool,
    cells: PauseCells<E::DataLabel>,
) {
    if timing {
        out.plat_read_monotonic_ns();
        out.mov_data_addr(Reg::V10, cells.start_ns);
        out.load_ptr_disp32(Reg::V11, Reg::V10, 0);
        out.sub_reg_reg(Reg::V0, Reg::V11); // rax = delta ns
        out.mov_data_addr(Reg::V10, cells.total_ns);
        out.load_ptr_disp32(Reg::V11, Reg::V10, 0);
        out.add_reg_reg(Reg::V11, Reg::V0);
        out.store_ptr_disp32(Reg::V10, 0, Reg::V11);
        out.mov_data_addr(Reg::V10, cells.max_ns);
        out.load_ptr_disp32(Reg::V11, Reg::V10, 0);
        out.cmp_reg_reg(Reg::V0, Reg::V11);
        let keep_max = out.create_text_label();
        out.jcc_label(Condition::LessEqual, keep_max);
        out.store_ptr_disp32(Reg::V10, 0, Reg::V0);
        out.bind_text_label(keep_max);
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
/// Colour a pointer on its way into a heap slot.
///
/// A heap slot holds a *coloured* pointer, which is what makes the load
/// barrier able to tell a stale reference from a current one. Null is left
/// alone: `0 | good_colour` would be a bogus non-null pointer, and a genuine
/// null has to stay a raw zero for the barrier to pass it through.
///
/// Emitted inline rather than called -- it is three instructions, and every
/// store of a reference needs it.
pub fn emit_gc_colour_pointer<E: PortableAsm>(out: &mut E, reg: Reg) {
    let raw = out.create_text_label();
    out.test_reg_reg(reg, reg);
    out.jcc_label(Condition::Equal, raw);
    out.or_reg_reg(reg, Reg::GoodColor);
    out.bind_text_label(raw);
}

/// The shadow stack's push, as one routine both backends call.
///
/// The protocol is the whole point of sharing it: a root is the *address* of
/// a slot, so the collector can rewrite what the slot holds when it moves an
/// object; the table has a fixed length and overflowing it is fatal rather
/// than silent. Written once here, the two backends cannot drift on either.
///
/// Called with the slot's address in the first argument register. Preserves
/// every register the caller can see.
pub fn emit_gc_shadow_push_runtime<E: PortableAsm>(
    out: &mut E,
    entry: E::TextLabel,
    shadow_stack: E::DataLabel,
    shadow_stack_top: E::DataLabel,
    overflow_text: E::DataLabel,
    overflow_text_len: usize,
) {
    use crate::gc_layout::GC_SHADOW_STACK_LEN;
    out.bind_text_label(entry);
    out.push_reg(Reg::V1);
    out.push_reg(Reg::V2);
    out.push_reg(Reg::V3);
    out.mov_data_addr(Reg::V2, shadow_stack_top);
    out.load_ptr_disp32(Reg::V3, Reg::V2, 0);
    let ok = out.create_text_label();
    out.mov_imm64(Reg::V1, GC_SHADOW_STACK_LEN as u64);
    out.cmp_reg_reg(Reg::V3, Reg::V1);
    out.jcc_label(Condition::Below, ok);
    out.plat_write_data(2, overflow_text, overflow_text_len);
    out.plat_exit(1);
    out.bind_text_label(ok);
    out.mov_data_addr(Reg::V1, shadow_stack);
    out.load_ptr_disp32(Reg::V1, Reg::V1, 0);
    out.shl_reg_imm8(Reg::V3, 3);
    out.add_reg_reg(Reg::V1, Reg::V3);
    out.store_ptr_disp32(Reg::V1, 0, Reg::V0);
    out.load_ptr_disp32(Reg::V3, Reg::V2, 0);
    out.add_reg_imm32(Reg::V3, 1);
    out.store_ptr_disp32(Reg::V2, 0, Reg::V3);
    out.pop_reg(Reg::V3);
    out.pop_reg(Reg::V2);
    out.pop_reg(Reg::V1);
    out.ret();
}

/// The shadow stack's pop, as one routine both backends call.
///
/// The underflow check is a self-test of the root bookkeeping: pushes and
/// pops have to balance on every path through every function, and an
/// imbalance is otherwise silent -- a leaked root, or a negative top that
/// makes the next push write outside the table. Aborting turns any mismatch
/// into an immediate failure instead of a rare corruption once the collector
/// is live.
pub fn emit_gc_shadow_pop_runtime<E: PortableAsm>(
    out: &mut E,
    entry: E::TextLabel,
    shadow_stack_top: E::DataLabel,
    underflow_text: E::DataLabel,
    underflow_text_len: usize,
) {
    out.bind_text_label(entry);
    out.push_reg(Reg::V0);
    out.push_reg(Reg::V1);
    out.mov_data_addr(Reg::V0, shadow_stack_top);
    out.load_ptr_disp32(Reg::V1, Reg::V0, 0);
    // Compared against an immediate rather than through a scratch register:
    // this routine promises to preserve every register the caller can see,
    // and it has only saved the two it uses.
    let ok = out.create_text_label();
    out.cmp_reg_imm8(Reg::V1, 0);
    out.jcc_label(Condition::NotEqual, ok);
    out.plat_write_data(2, underflow_text, underflow_text_len);
    out.plat_exit(1);
    out.bind_text_label(ok);
    out.sub_reg_imm32(Reg::V1, 1);
    out.store_ptr_disp32(Reg::V0, 0, Reg::V1);
    out.pop_reg(Reg::V1);
    out.pop_reg(Reg::V0);
    out.ret();
}

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

/// Portable version of `gc_trace` (V7/rdi = pop budget): drain the mark
/// worklist up to the budget, walking each popped pointer object's
/// payload -- stripping the color off each child slot, following an M7
/// forwarding word, recoloring the slot to the good/to-space value, then
/// marking the child (the strong tricolor closure). Handles the
/// pointer-list tag's leading length qword. Frame slots: 8 = remaining
/// budget, 24 = field cursor, 32 = field end. Uses the reserved
/// ColorStrip (R13) and GoodColor (R14) registers.
pub fn emit_gc_trace<E: PortableAsm>(
    out: &mut E,
    entry: E::TextLabel,
    mark_worklist_top: E::DataLabel,
    mark_worklist: E::DataLabel,
    mark_visit: E::TextLabel,
) {
    use crate::gc_layout::{GC_FWD, GC_TYPE_POINTER_LIST, GC_TYPE_POINTER_RECORD};

    out.bind_text_label(entry);
    out.push_reg(Reg::V5); // rbp
    out.mov_reg_reg(Reg::V5, Reg::V4); // rbp = rsp
    out.sub_reg_imm8(Reg::V4, 32);
    // rdi = pop budget for this quantum; [rbp-8] holds the remaining
    // budget, [rbp-24] the field cursor, [rbp-32] the field end.
    out.store_rbp_slot(8, Reg::V7);
    let trace_loop = out.create_text_label();
    let trace_done = out.create_text_label();
    let trace_field_loop = out.create_text_label();
    let trace_skip_field = out.create_text_label();
    out.bind_text_label(trace_loop);
    // Budget exhausted? (an incremental quantum stops here; gc_drain
    // passes a large budget and loops until the worklist empties.)
    out.load_rbp_slot(Reg::V0, 8);
    out.test_reg_reg(Reg::V0, Reg::V0);
    out.jcc_label(Condition::Equal, trace_done);
    out.mov_data_addr(Reg::V10, mark_worklist_top);
    out.load_ptr_disp32(Reg::V1, Reg::V10, 0);
    out.test_reg_reg(Reg::V1, Reg::V1);
    out.jcc_label(Condition::Equal, trace_done);
    // We have work: spend one budget unit (rax still holds it).
    out.sub_reg_imm8(Reg::V0, 1);
    out.store_rbp_slot(8, Reg::V0);
    // top--, fetch worklist[top]
    out.sub_reg_imm8(Reg::V1, 1);
    out.store_ptr_disp32(Reg::V10, 0, Reg::V1);
    out.mov_data_addr(Reg::V8, mark_worklist);
    out.load_ptr_disp32(Reg::V8, Reg::V8, 0);
    out.mov_reg_reg(Reg::V9, Reg::V1);
    out.shl_reg_imm8(Reg::V9, 3);
    out.add_reg_reg(Reg::V8, Reg::V9);
    out.load_ptr_disp32(Reg::V0, Reg::V8, 0);
    // type at [rax - 8]
    out.load_ptr_disp32(Reg::V1, Reg::V0, -8);
    // Save the type tag because the upcoming size load reuses rcx.
    out.mov_reg_reg(Reg::V8, Reg::V1);
    // Tags below GC_TYPE_POINTER_RECORD (free / raw bytes) carry no
    // pointer fields; tags >= it are walked as a packed pointer array,
    // optionally skipping a leading length qword for the list tag.
    out.cmp_reg_imm8(Reg::V1, GC_TYPE_POINTER_RECORD as i8);
    out.jcc_label(Condition::Below, trace_loop);
    // Pointer record / array / list: payload_size = (size & -16) - 16.
    out.load_ptr_disp32(Reg::V1, Reg::V0, -16);
    out.and_reg_imm32(Reg::V1, -16);
    out.sub_reg_imm8(Reg::V1, 16);
    // GC_TYPE_POINTER_LIST stores an integer length at payload offset 0 --
    // skip it so the mark phase does not chase the length as a pointer.
    let after_list_skip = out.create_text_label();
    out.cmp_reg_imm8(Reg::V8, GC_TYPE_POINTER_LIST as i8);
    out.jcc_label(Condition::NotEqual, after_list_skip);
    out.add_reg_imm32(Reg::V0, 8);
    out.sub_reg_imm8(Reg::V1, 8);
    out.bind_text_label(after_list_skip);
    // trace_end = rax + payload_size; trace_cursor = rax
    out.mov_reg_reg(Reg::V9, Reg::V0);
    out.add_reg_reg(Reg::V9, Reg::V1);
    out.store_rbp_slot(32, Reg::V9);
    out.store_rbp_slot(24, Reg::V0);
    out.bind_text_label(trace_field_loop);
    out.load_rbp_slot(Reg::V10, 24);
    out.load_rbp_slot(Reg::V11, 32);
    out.cmp_reg_reg(Reg::V10, Reg::V11);
    out.jcc_label(Condition::AboveOrEqual, trace_loop);
    out.load_ptr_disp32(Reg::V7, Reg::V10, 0);
    // Heap slots hold colored pointers; strip the color before
    // gc_mark_visit dereferences the block header at [rdi-16].
    out.and_reg_reg(Reg::V7, Reg::ColorStrip);
    out.test_reg_reg(Reg::V7, Reg::V7);
    out.jcc_label(Condition::Equal, trace_skip_field);
    // M7: follow forwarding BEFORE recoloring/healing the slot. If the
    // child was already evacuated, its header is a forwarding word; heal
    // the slot to the TO-SPACE address, not a good-colored ghost pointer
    // (a later fast-path load would deref it as a forwarding word ->
    // use-after-free). Inert while evac is off. r11 is scratch here.
    let trace_child_not_fwd = out.create_text_label();
    out.load_ptr_disp32(Reg::V11, Reg::V7, -16);
    out.and_reg_imm32(Reg::V11, GC_FWD as i32);
    out.test_reg_reg(Reg::V11, Reg::V11);
    out.jcc_label(Condition::Equal, trace_child_not_fwd);
    out.load_ptr_disp32(Reg::V11, Reg::V7, -16);
    out.and_reg_imm32(Reg::V11, -16); // to-space user ptr
    out.mov_reg_reg(Reg::V7, Reg::V11);
    out.bind_text_label(trace_child_not_fwd);
    // Recolor-on-trace: write the good-colored (to-space) child back into
    // this field slot (r10 = slot address) BEFORE marking, so a later
    // barriered load of the same field takes the fast path instead of
    // re-entering the slow path. Must precede the call, which clobbers
    // r10; the cursor is reloaded afterward. Inert until the mark color
    // flips (recoloring good->good is a no-op).
    out.mov_reg_reg(Reg::V11, Reg::V7);
    out.or_reg_reg(Reg::V11, Reg::GoodColor);
    out.store_ptr_disp32(Reg::V10, 0, Reg::V11);
    out.call_label(mark_visit);
    out.bind_text_label(trace_skip_field);
    out.load_rbp_slot(Reg::V10, 24);
    out.add_reg_imm32(Reg::V10, 8);
    out.store_rbp_slot(24, Reg::V10);
    out.jmp_label(trace_field_loop);
    out.bind_text_label(trace_done);
    out.leave();
    out.ret();
}

/// Portable version of `gc_sweep`: the region-based sweep. Bumps the
/// collection counter, flushes the current region's watermark, then walks
/// every committed region clearing the current-parity mark on live blocks
/// (rewriting each live header as `size | mark`), recording per-region
/// live bytes for M7 sparsest-first selection, and reclaiming any
/// wholly-dead region's run onto the free-region pool (never the current
/// bump region, never a from-space ghost). Frame slots: 8 = mark bit, 16
/// = per-region live bytes, 40 = region index, 48 = committed bound, 56 =
/// region base. Reuses `RegionTables`; collect_counter / free_region_head
/// / header_mark / region_live are the labels beyond that set.
pub fn emit_gc_sweep<E: PortableAsm>(
    out: &mut E,
    entry: E::TextLabel,
    t: RegionTables<E::DataLabel>,
    collect_counter: E::DataLabel,
    free_region_head: E::DataLabel,
    header_mark: E::DataLabel,
    region_live: E::DataLabel,
) {
    use crate::gc_layout::{GC_REGION_SHIFT, GC_REGION_SIZE};

    out.bind_text_label(entry);
    out.push_reg(Reg::V5); // rbp
    out.mov_reg_reg(Reg::V5, Reg::V4); // rbp = rsp
    out.sub_reg_imm8(Reg::V4, 64);
    // One completed collection cycle per sweep: bump the counter here so
    // both the synchronous and incremental paths count once.
    out.mov_data_addr(Reg::V10, collect_counter);
    out.load_ptr_disp32(Reg::V0, Reg::V10, 0);
    out.add_reg_imm32(Reg::V0, 1);
    out.store_ptr_disp32(Reg::V10, 0, Reg::V0);
    // Flush the current region's watermark (the bump path only updates the
    // global gc_heap_top) so its objects are covered.
    out.mov_data_addr(Reg::V10, t.heap_base);
    out.load_ptr_disp32(Reg::V11, Reg::V10, 0);
    out.mov_data_addr(Reg::V10, t.region_base);
    out.load_ptr_disp32(Reg::V10, Reg::V10, 0);
    out.sub_reg_reg(Reg::V11, Reg::V10); // offset = heap_base - region_base
    out.shr_reg_imm8(Reg::V11, GC_REGION_SHIFT); // cur region index
    out.shl_reg_imm8(Reg::V11, 3); // * 8
    out.mov_data_addr(Reg::V10, t.region_top);
    out.add_reg_reg(Reg::V10, Reg::V11);
    out.mov_data_addr(Reg::V8, t.heap_top);
    out.load_ptr_disp32(Reg::V0, Reg::V8, 0);
    out.store_ptr_disp32(Reg::V10, 0, Reg::V0); // region_top[cur] = heap_top
    // Cache committed bound (48), reset region index (40), seed the
    // free-region accumulator (r9) with the EXISTING pool head, not zero,
    // so already-free regions stay linked.
    out.mov_data_addr(Reg::V10, t.committed_count);
    out.load_ptr_disp32(Reg::V0, Reg::V10, 0);
    out.store_rbp_slot(48, Reg::V0);
    out.mov_imm64(Reg::V0, 0);
    out.store_rbp_slot(40, Reg::V0);
    out.mov_data_addr(Reg::V10, free_region_head);
    out.load_ptr_disp32(Reg::V9, Reg::V10, 0);
    // Cache the current-parity header mark bit in slot 8.
    out.mov_data_addr(Reg::V10, header_mark);
    out.load_ptr_disp32(Reg::V0, Reg::V10, 0);
    out.store_rbp_slot(8, Reg::V0);

    let region_loop = out.create_text_label();
    let region_done = out.create_text_label();
    let inner_loop = out.create_text_label();
    let inner_done = out.create_text_label();
    let block_dead = out.create_text_label();
    let next_region = out.create_text_label();
    let reclaim_step = out.create_text_label();

    out.bind_text_label(region_loop);
    out.load_rbp_slot(Reg::V0, 40);
    out.load_rbp_slot(Reg::V1, 48);
    out.cmp_reg_reg(Reg::V0, Reg::V1);
    out.jcc_label(Condition::AboveOrEqual, region_done);

    // rsi = region_base + (i << REGION_SHIFT); save it.
    out.mov_reg_reg(Reg::V1, Reg::V0);
    out.shl_reg_imm8(Reg::V1, GC_REGION_SHIFT);
    out.mov_data_addr(Reg::V10, t.region_base);
    out.load_ptr_disp32(Reg::V10, Reg::V10, 0);
    out.add_reg_reg(Reg::V10, Reg::V1);
    out.mov_reg_reg(Reg::V6, Reg::V10);
    out.store_rbp_slot(56, Reg::V6);
    // r8 = region_top[i]
    out.mov_reg_reg(Reg::V1, Reg::V0);
    out.shl_reg_imm8(Reg::V1, 3);
    out.mov_data_addr(Reg::V10, t.region_top);
    out.add_reg_reg(Reg::V10, Reg::V1);
    out.load_ptr_disp32(Reg::V8, Reg::V10, 0);
    // Empty region (top == base): skip; it is already free.
    out.cmp_reg_reg(Reg::V8, Reg::V6);
    out.jcc_label(Condition::Equal, next_region);
    // M7: skip from-space (ghost) regions. Inert while evac is off.
    out.load_rbp_slot(Reg::V0, 40);
    out.shl_reg_imm8(Reg::V0, 3);
    out.mov_data_addr(Reg::V10, t.region_fromspace);
    out.add_reg_reg(Reg::V10, Reg::V0);
    out.load_ptr_disp32(Reg::V0, Reg::V10, 0);
    out.test_reg_reg(Reg::V0, Reg::V0);
    out.jcc_label(Condition::NotEqual, next_region);

    // Inner block walk: r11 = cursor, rdx = any_live flag, slot 16 =
    // live-byte accumulator for this region.
    out.mov_reg_reg(Reg::V11, Reg::V6);
    out.xor_reg_reg(Reg::V2, Reg::V2);
    out.mov_imm64(Reg::V0, 0);
    out.store_rbp_slot(16, Reg::V0);
    out.bind_text_label(inner_loop);
    out.cmp_reg_reg(Reg::V11, Reg::V8);
    out.jcc_label(Condition::AboveOrEqual, inner_done);
    out.load_ptr_disp32(Reg::V0, Reg::V11, 0); // header word0
    out.mov_reg_reg(Reg::V1, Reg::V0);
    out.and_reg_imm32(Reg::V1, -16); // size (clears FWD + mark bits)
    // Live iff the current-parity mark bit is set (slot 8).
    out.load_rbp_slot(Reg::V7, 8);
    out.test_reg_reg(Reg::V0, Reg::V7);
    out.jcc_label(Condition::Equal, block_dead);
    // Live: rewrite the header as size | current mark -- KEEPS the current
    // mark (so the relocate walk can still read liveness after the sweep)
    // and clears the stale (previous-cycle) mark and the FWD bit.
    out.mov_reg_reg(Reg::V0, Reg::V1);
    out.or_reg_reg(Reg::V0, Reg::V7);
    out.store_ptr_disp32(Reg::V11, 0, Reg::V0);
    out.mov_imm64(Reg::V2, 1);
    out.load_rbp_slot(Reg::V0, 16);
    out.add_reg_reg(Reg::V0, Reg::V1);
    out.store_rbp_slot(16, Reg::V0);
    out.add_reg_reg(Reg::V11, Reg::V1);
    out.jmp_label(inner_loop);
    out.bind_text_label(block_dead);
    // Dead: skip; whole-region reclamation is decided after the walk.
    out.add_reg_reg(Reg::V11, Reg::V1);
    out.jmp_label(inner_loop);

    out.bind_text_label(inner_done);
    // Record this region's live bytes for M7 sparsest-first selection.
    out.load_rbp_slot(Reg::V0, 40);
    out.shl_reg_imm8(Reg::V0, 3);
    out.mov_data_addr(Reg::V10, region_live);
    out.add_reg_reg(Reg::V10, Reg::V0);
    out.load_rbp_slot(Reg::V0, 16);
    out.store_ptr_disp32(Reg::V10, 0, Reg::V0);
    out.test_reg_reg(Reg::V2, Reg::V2);
    out.jcc_label(Condition::NotEqual, next_region); // any live: keep
    out.load_rbp_slot(Reg::V6, 56);
    // Never reclaim the current bump region.
    out.mov_data_addr(Reg::V10, t.heap_base);
    out.load_ptr_disp32(Reg::V10, Reg::V10, 0);
    out.cmp_reg_reg(Reg::V6, Reg::V10);
    out.jcc_label(Condition::Equal, next_region);
    // Fully dead: reclaim the run. rax = first block size (== the object
    // size; a large run spans multiple regions, a normal region is one).
    out.load_ptr_disp32(Reg::V0, Reg::V6, 0);
    out.and_reg_imm32(Reg::V0, -16); // size (clears FWD + marks)
    out.mov_reg_reg(Reg::V7, Reg::V6); // rdi = m = run base
    out.bind_text_label(reclaim_step);
    // Link this region into the free pool and mark it empty.
    out.store_ptr_disp32(Reg::V7, 0, Reg::V9);
    out.mov_reg_reg(Reg::V9, Reg::V7);
    out.mov_reg_reg(Reg::V1, Reg::V7);
    out.mov_data_addr(Reg::V10, t.region_base);
    out.load_ptr_disp32(Reg::V10, Reg::V10, 0);
    out.sub_reg_reg(Reg::V1, Reg::V10);
    out.shr_reg_imm8(Reg::V1, GC_REGION_SHIFT);
    out.shl_reg_imm8(Reg::V1, 3);
    out.mov_data_addr(Reg::V10, t.region_top);
    out.add_reg_reg(Reg::V10, Reg::V1);
    out.store_ptr_disp32(Reg::V10, 0, Reg::V7); // region_top[idx] = base
    out.sub_reg_imm32(Reg::V0, GC_REGION_SIZE as i32);
    out.add_reg_imm32(Reg::V7, GC_REGION_SIZE as i32);
    out.cmp_reg_imm8(Reg::V0, 0);
    out.jcc_label(Condition::Greater, reclaim_step);

    out.bind_text_label(next_region);
    out.load_rbp_slot(Reg::V0, 40);
    out.add_reg_imm32(Reg::V0, 1);
    out.store_rbp_slot(40, Reg::V0);
    out.jmp_label(region_loop);

    out.bind_text_label(region_done);
    out.mov_data_addr(Reg::V10, free_region_head);
    out.store_ptr_disp32(Reg::V10, 0, Reg::V9);
    out.leave();
    out.ret();
}

/// Portable version of `gc_load_barrier_slow` (V0/rax = bad-colored heap
/// pointer, V10/r10 = the field address it was loaded from; out: V0 = raw
/// remapped pointer). Strips the color, follows an M7 forwarding word
/// (via `bt` so no scratch register is needed), evacuates on demand
/// during Relocate, self-heals the slot to the good-colored pointer, and
/// marks the result during Mark. Preserves the caller-saved registers a
/// pointer-typed barriered read must keep live across the evacuate/mark
/// calls. Not a framed routine -- bind ... ret. Uses ColorStrip (R13) and
/// GoodColor (R14).
pub fn emit_gc_load_barrier_slow<E: PortableAsm>(
    out: &mut E,
    entry: E::TextLabel,
    phase: E::DataLabel,
    region_base: E::DataLabel,
    region_fromspace: E::DataLabel,
    evacuate: E::TextLabel,
    mark_visit: E::TextLabel,
) {
    use crate::gc_layout::{GC_PHASE_MARK, GC_PHASE_RELOCATE, GC_REGION_SHIFT};

    out.bind_text_label(entry);
    out.and_reg_reg(Reg::V0, Reg::ColorStrip); // strip -> raw P
    let healed = out.create_text_label();
    let not_fwd = out.create_text_label();
    let done = out.create_text_label();
    // Follow forwarding: if P's header is a forwarding word, P is a ghost
    // -- redirect rax to the to-space copy. Inert while evac is off. Uses
    // `bt` so the FWD test needs no scratch register -- the slow path must
    // preserve every caller-saved register in the Idle path (its M5/M6
    // clobber set was rax/r10/r11 only).
    out.load_ptr_disp32(Reg::V11, Reg::V0, -16);
    out.bt_reg_imm8(Reg::V11, 0); // CF = FWD bit
    out.jcc_label(Condition::AboveOrEqual, not_fwd); // CF clear
    out.and_reg_imm32(Reg::V11, -16); // to-space user ptr
    out.mov_reg_reg(Reg::V0, Reg::V11);
    out.jmp_label(healed);
    out.bind_text_label(not_fwd);
    // Relocate: evacuate-on-demand. r10 (the field address) must be
    // preserved through the self-heal, so the phase is read via r11.
    out.mov_data_addr(Reg::V11, phase);
    out.load_ptr_disp32(Reg::V11, Reg::V11, 0);
    out.cmp_reg_imm8(Reg::V11, GC_PHASE_RELOCATE as i8);
    out.jcc_label(Condition::NotEqual, healed);
    // idx = (P - region_base) >> SHIFT ; from-space?
    out.mov_reg_reg(Reg::V1, Reg::V0);
    out.mov_data_addr(Reg::V11, region_base);
    out.load_ptr_disp32(Reg::V11, Reg::V11, 0);
    out.sub_reg_reg(Reg::V1, Reg::V11);
    out.shr_reg_imm8(Reg::V1, GC_REGION_SHIFT);
    out.shl_reg_imm8(Reg::V1, 3);
    out.mov_data_addr(Reg::V11, region_fromspace);
    out.add_reg_reg(Reg::V11, Reg::V1);
    out.load_ptr_disp32(Reg::V11, Reg::V11, 0);
    out.test_reg_reg(Reg::V11, Reg::V11);
    out.jcc_label(Condition::Equal, healed);
    // Preserve across the evacuation call the registers the Idle/Mark slow
    // path leaves untouched but gc_evacuate clobbers: r10 (field address),
    // and rdx/rsi (a pointer-typed barriered read preserves them in every
    // other phase). Four pushes keep rsp 16-aligned; r11 is a throwaway
    // pad (recomputed at the self-heal).
    out.push_reg(Reg::V10);
    out.push_reg(Reg::V2);
    out.push_reg(Reg::V6);
    out.push_reg(Reg::V11);
    out.mov_reg_reg(Reg::V7, Reg::V0);
    out.call_label(evacuate); // rax = to-space
    out.pop_reg(Reg::V11);
    out.pop_reg(Reg::V6);
    out.pop_reg(Reg::V2);
    out.pop_reg(Reg::V10);
    out.bind_text_label(healed);
    // Self-heal: write the good-colored (remapped) pointer back to the
    // slot. r10 = the field address, carried from the barrier fast path
    // (and preserved across gc_evacuate above).
    out.mov_reg_reg(Reg::V11, Reg::V0);
    out.or_reg_reg(Reg::V11, Reg::GoodColor); // recolor to good
    out.store_ptr_disp32(Reg::V10, 0, Reg::V11); // self-heal
    // During Mark, the mutator has just loaded a raw pointer into a
    // register, so it must be marked to keep the strong tricolor invariant
    // (load-barrier-driven incremental update). rax (the raw result) is
    // preserved across the mark call.
    out.mov_data_addr(Reg::V10, phase);
    out.load_ptr_disp32(Reg::V11, Reg::V10, 0);
    out.cmp_reg_imm8(Reg::V11, GC_PHASE_MARK as i8);
    out.jcc_label(Condition::NotEqual, done);
    out.push_reg(Reg::V0);
    out.mov_reg_reg(Reg::V7, Reg::V0);
    out.call_label(mark_visit);
    out.pop_reg(Reg::V0);
    out.bind_text_label(done);
    out.ret();
}

/// Portable version of `gc_deep_equal(V7/rdi = a, V6/rsi = b -> V0/rax =
/// 1 if structurally equal, else 0)`. Follows an M7 forwarding word on
/// each operand, checks the type tags match, then compares only the
/// shared-prefix payload: raw-bytes qword by qword, pointer records/lists
/// slot by slot by recursing (the list tag first checking the two leading
/// lengths). Recursive -- calls itself via `entry`. Frame slots: 8 =
/// cursor_a, 16 = end_a, 24 = cursor_b. Uses ColorStrip (R13).
pub fn emit_gc_deep_equal<E: PortableAsm>(out: &mut E, entry: E::TextLabel) {
    use crate::gc_layout::{
        GC_FWD, GC_TYPE_POINTER_LIST, GC_TYPE_POINTER_RECORD, GC_TYPE_RAW_BYTES,
    };

    out.bind_text_label(entry);
    out.push_reg(Reg::V5); // rbp
    out.mov_reg_reg(Reg::V5, Reg::V4); // rbp = rsp
    // Locals: [rbp-8] = cursor_a, [rbp-16] = end_a, [rbp-24] = cursor_b.
    out.sub_reg_imm8(Reg::V4, 32);

    let equal = out.create_text_label();
    let not_equal = out.create_text_label();
    let done = out.create_text_label();
    let raw_bytes = out.create_text_label();
    let raw_loop = out.create_text_label();
    let ptr_setup = out.create_text_label();
    let slot_loop = out.create_text_label();
    let raw_have_min = out.create_text_label();
    let ptr_have_min = out.create_text_label();

    // Identical pointers (including both null) are trivially equal.
    out.cmp_reg_reg(Reg::V7, Reg::V6);
    out.jcc_label(Condition::Equal, equal);
    // A null on exactly one side has no header to dereference.
    out.test_reg_reg(Reg::V7, Reg::V7);
    out.jcc_label(Condition::Equal, not_equal);
    out.test_reg_reg(Reg::V6, Reg::V6);
    out.jcc_label(Condition::Equal, not_equal);
    // M7: follow forwarding on each operand before dereferencing its
    // header. Inert while evac is off. rax is scratch.
    let deq_a_not_fwd = out.create_text_label();
    out.load_ptr_disp32(Reg::V0, Reg::V7, -16);
    out.and_reg_imm32(Reg::V0, GC_FWD as i32);
    out.test_reg_reg(Reg::V0, Reg::V0);
    out.jcc_label(Condition::Equal, deq_a_not_fwd);
    out.load_ptr_disp32(Reg::V0, Reg::V7, -16);
    out.and_reg_imm32(Reg::V0, -16);
    out.mov_reg_reg(Reg::V7, Reg::V0);
    out.bind_text_label(deq_a_not_fwd);
    let deq_b_not_fwd = out.create_text_label();
    out.load_ptr_disp32(Reg::V0, Reg::V6, -16);
    out.and_reg_imm32(Reg::V0, GC_FWD as i32);
    out.test_reg_reg(Reg::V0, Reg::V0);
    out.jcc_label(Condition::Equal, deq_b_not_fwd);
    out.load_ptr_disp32(Reg::V0, Reg::V6, -16);
    out.and_reg_imm32(Reg::V0, -16);
    out.mov_reg_reg(Reg::V6, Reg::V0);
    out.bind_text_label(deq_b_not_fwd);
    // Type tags must match: type_a = [rdi-8], type_b = [rsi-8].
    out.load_ptr_disp32(Reg::V0, Reg::V7, -8);
    out.load_ptr_disp32(Reg::V1, Reg::V6, -8);
    out.cmp_reg_reg(Reg::V0, Reg::V1);
    out.jcc_label(Condition::NotEqual, not_equal);
    // payload_a = ([rdi-16] & -16) - 16 in r8, payload_b in r9.
    out.load_ptr_disp32(Reg::V8, Reg::V7, -16);
    out.load_ptr_disp32(Reg::V9, Reg::V6, -16);
    out.and_reg_imm32(Reg::V8, -16);
    out.and_reg_imm32(Reg::V9, -16);
    out.sub_reg_imm8(Reg::V8, 16);
    out.sub_reg_imm8(Reg::V9, 16);
    // rax still holds the shared type tag.
    out.cmp_reg_imm8(Reg::V0, GC_TYPE_RAW_BYTES as i8);
    out.jcc_label(Condition::Equal, raw_bytes);
    out.cmp_reg_imm8(Reg::V0, GC_TYPE_POINTER_RECORD as i8);
    out.jcc_label(Condition::Below, not_equal);
    // Pointer record / array / list: a packed run of heap-pointer slots,
    // optionally led by an integer length for the list tag.
    out.cmp_reg_imm8(Reg::V0, GC_TYPE_POINTER_LIST as i8);
    out.jcc_label(Condition::NotEqual, ptr_setup);
    // List: the two lengths must match, then skip past the length word.
    out.load_ptr_disp32(Reg::V10, Reg::V7, 0);
    out.load_ptr_disp32(Reg::V11, Reg::V6, 0);
    out.cmp_reg_reg(Reg::V10, Reg::V11);
    out.jcc_label(Condition::NotEqual, not_equal);
    out.add_reg_imm32(Reg::V7, 8);
    out.add_reg_imm32(Reg::V6, 8);
    out.sub_reg_imm8(Reg::V8, 8);
    out.sub_reg_imm8(Reg::V9, 8);
    out.bind_text_label(ptr_setup);
    // r8 = min(payload_a, payload_b): compare only the shared prefix.
    out.cmp_reg_reg(Reg::V8, Reg::V9);
    out.jcc_label(Condition::Below, ptr_have_min);
    out.mov_reg_reg(Reg::V8, Reg::V9);
    out.bind_text_label(ptr_have_min);
    // cursor_a = rdi, end_a = rdi + min, cursor_b = rsi.
    out.mov_reg_reg(Reg::V10, Reg::V7);
    out.add_reg_reg(Reg::V10, Reg::V8);
    out.store_rbp_slot(8, Reg::V7);
    out.store_rbp_slot(16, Reg::V10);
    out.store_rbp_slot(24, Reg::V6);
    out.bind_text_label(slot_loop);
    out.load_rbp_slot(Reg::V10, 8);
    out.load_rbp_slot(Reg::V11, 16);
    out.cmp_reg_reg(Reg::V10, Reg::V11);
    out.jcc_label(Condition::AboveOrEqual, equal);
    out.load_rbp_slot(Reg::V11, 24);
    // rdi = a_slot = [cursor_a], rsi = b_slot = [cursor_b], recurse. Both
    // slots hold colored pointers; strip before the recursive deep-equal
    // dereferences them.
    out.load_ptr_disp32(Reg::V7, Reg::V10, 0);
    out.and_reg_reg(Reg::V7, Reg::ColorStrip);
    out.load_ptr_disp32(Reg::V6, Reg::V11, 0);
    out.and_reg_reg(Reg::V6, Reg::ColorStrip);
    out.call_label(entry);
    out.test_reg_reg(Reg::V0, Reg::V0);
    out.jcc_label(Condition::Equal, not_equal);
    // Advance both cursors one slot and continue.
    out.load_rbp_slot(Reg::V10, 8);
    out.add_reg_imm32(Reg::V10, 8);
    out.store_rbp_slot(8, Reg::V10);
    out.load_rbp_slot(Reg::V10, 24);
    out.add_reg_imm32(Reg::V10, 8);
    out.store_rbp_slot(24, Reg::V10);
    out.jmp_label(slot_loop);
    // Raw bytes: compare the shared-prefix payload qword by qword.
    out.bind_text_label(raw_bytes);
    out.cmp_reg_reg(Reg::V8, Reg::V9);
    out.jcc_label(Condition::Below, raw_have_min);
    out.mov_reg_reg(Reg::V8, Reg::V9);
    out.bind_text_label(raw_have_min);
    out.bind_text_label(raw_loop);
    out.test_reg_reg(Reg::V8, Reg::V8);
    out.jcc_label(Condition::Equal, equal);
    out.load_ptr_disp32(Reg::V10, Reg::V7, 0);
    out.load_ptr_disp32(Reg::V11, Reg::V6, 0);
    out.cmp_reg_reg(Reg::V10, Reg::V11);
    out.jcc_label(Condition::NotEqual, not_equal);
    out.add_reg_imm32(Reg::V7, 8);
    out.add_reg_imm32(Reg::V6, 8);
    out.sub_reg_imm8(Reg::V8, 8);
    out.jmp_label(raw_loop);
    out.bind_text_label(equal);
    out.mov_imm64(Reg::V0, 1);
    out.jmp_label(done);
    out.bind_text_label(not_equal);
    out.mov_imm64(Reg::V0, 0);
    out.bind_text_label(done);
    out.leave();
    out.ret();
}

/// Portable version of `gc_mark_roots`: walk every shadow-stack root
/// slot and, for each non-null pointer, call `gc_mark_visit` to mark it
/// and push it onto the trace worklist. The sole root source for the
/// mark. Uses two frame slots (`[rbp-8]` = cursor, `[rbp-16]` = end).
pub fn emit_gc_mark_roots<E: PortableAsm>(
    out: &mut E,
    entry: E::TextLabel,
    shadow_stack: E::DataLabel,
    shadow_stack_top: E::DataLabel,
    mark_visit: E::TextLabel,
) {
    out.bind_text_label(entry);
    out.push_reg(Reg::V5); // rbp
    out.mov_reg_reg(Reg::V5, Reg::V4); // rbp = rsp
    out.sub_reg_imm8(Reg::V4, 16);
    out.mov_data_addr(Reg::V10, shadow_stack);
    out.load_ptr_disp32(Reg::V10, Reg::V10, 0);
    out.store_rbp_slot(8, Reg::V10);
    // end = base + top * 8
    out.mov_data_addr(Reg::V8, shadow_stack_top);
    out.load_ptr_disp32(Reg::V1, Reg::V8, 0);
    out.shl_reg_imm8(Reg::V1, 3);
    out.add_reg_reg(Reg::V10, Reg::V1);
    out.store_rbp_slot(16, Reg::V10);

    let shadow_loop = out.create_text_label();
    let shadow_done = out.create_text_label();
    let shadow_skip = out.create_text_label();
    out.bind_text_label(shadow_loop);
    out.load_rbp_slot(Reg::V10, 8);
    out.load_rbp_slot(Reg::V11, 16);
    out.cmp_reg_reg(Reg::V10, Reg::V11);
    out.jcc_label(Condition::AboveOrEqual, shadow_done);
    // entry = [r10] is the address of a slot; deref to read the pointer.
    out.load_ptr_disp32(Reg::V0, Reg::V10, 0);
    out.load_ptr_disp32(Reg::V7, Reg::V0, 0);
    out.test_reg_reg(Reg::V7, Reg::V7);
    out.jcc_label(Condition::Equal, shadow_skip);
    out.call_label(mark_visit);
    out.bind_text_label(shadow_skip);
    out.load_rbp_slot(Reg::V10, 8);
    out.add_reg_imm32(Reg::V10, 8);
    out.store_rbp_slot(8, Reg::V10);
    out.jmp_label(shadow_loop);
    out.bind_text_label(shadow_done);
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

/// Text labels for the subroutines the synchronous `gc_collect` entry
/// dispatches to.
#[derive(Clone, Copy)]
pub struct CollectTargets<T: Copy> {
    pub stw_mark_complete: T,
    pub free_ghost_regions: T,
    pub sweep: T,
    pub relocate_quantum: T,
    pub mark_end: T,
}

/// Portable version of `gc_collect`: the synchronous collection entry
/// (do_collect / --gc-stress). If a Mark cycle is in progress finish it
/// via mark_end (self-bracketing its pause); if Relocate is in progress
/// drain it synchronously with unbounded quanta; otherwise run a full
/// from-scratch STW mark + free-ghosts + sweep, bracketed as one pause.
/// Exactly one sweep runs either way.
pub fn emit_gc_collect<E: PortableAsm>(
    out: &mut E,
    entry: E::TextLabel,
    timing: bool,
    pause: PauseCells<E::DataLabel>,
    phase: E::DataLabel,
    targets: CollectTargets<E::TextLabel>,
) {
    use crate::gc_layout::{GC_PHASE_MARK, GC_PHASE_RELOCATE};

    out.bind_text_label(entry);
    out.push_reg(Reg::V5); // rbp
    out.mov_reg_reg(Reg::V5, Reg::V4); // rbp = rsp
    let collect_mark = out.create_text_label();
    let collect_reloc = out.create_text_label();
    let collect_done = out.create_text_label();
    out.mov_data_addr(Reg::V10, phase);
    out.load_ptr_disp32(Reg::V0, Reg::V10, 0);
    out.cmp_reg_imm8(Reg::V0, GC_PHASE_MARK as i8);
    out.jcc_label(Condition::Equal, collect_mark);
    out.cmp_reg_imm8(Reg::V0, GC_PHASE_RELOCATE as i8);
    out.jcc_label(Condition::Equal, collect_reloc);
    // Idle: a full from-scratch STW mark + sweep, bracketed as one pause.
    // Free the previous cycle's ghosts first (no-op until evac runs).
    emit_gc_pause_start(out, timing, pause);
    out.call_label(targets.stw_mark_complete);
    out.call_label(targets.free_ghost_regions);
    out.call_label(targets.sweep);
    emit_gc_pause_end(out, timing, pause);
    out.jmp_label(collect_done);
    // Relocate in progress: drain the remaining evacuation synchronously
    // (unbounded quanta) until the phase returns to Idle. Unreachable
    // until the activation step.
    out.bind_text_label(collect_reloc);
    let reloc_loop = out.create_text_label();
    out.bind_text_label(reloc_loop);
    out.mov_imm64(Reg::V7, i64::MAX as u64);
    out.call_label(targets.relocate_quantum);
    out.mov_data_addr(Reg::V10, phase);
    out.load_ptr_disp32(Reg::V0, Reg::V10, 0);
    out.cmp_reg_imm8(Reg::V0, GC_PHASE_RELOCATE as i8);
    out.jcc_label(Condition::Equal, reloc_loop);
    out.jmp_label(collect_done);
    out.bind_text_label(collect_mark);
    out.call_label(targets.mark_end);
    out.bind_text_label(collect_done);
    out.leave();
    out.ret();
}

/// Text labels for the subroutines `gc_mark_end` chains through to close
/// a cycle.
#[derive(Clone, Copy)]
pub struct MarkEndTargets<T: Copy> {
    pub drain: T,
    pub stw_mark_complete: T,
    pub free_ghost_regions: T,
    pub sweep: T,
    pub relocate_start: T,
}

/// Portable version of `gc_mark_end` (short STW pause): finish the cycle.
/// If the worklist overflowed during the incremental quanta, degrade to a
/// from-scratch STW re-mark; otherwise drain the remaining frontier to
/// fixpoint (a straggler may have been added by the barrier), and if THAT
/// overflows, degrade. Then free ghosts, sweep, and close via
/// `relocate_start`. Wraps the body in the portable pause-timing helpers.
pub fn emit_gc_mark_end<E: PortableAsm>(
    out: &mut E,
    entry: E::TextLabel,
    timing: bool,
    pause: PauseCells<E::DataLabel>,
    targets: MarkEndTargets<E::TextLabel>,
    stw_fallback_pending: E::DataLabel,
    stw_fallbacks: E::DataLabel,
) {
    out.bind_text_label(entry);
    out.push_reg(Reg::V5); // rbp
    out.mov_reg_reg(Reg::V5, Reg::V4); // rbp = rsp
    emit_gc_pause_start(out, timing, pause); // MarkEnd STW pause (final drain + sweep).
    let do_degrade = out.create_text_label();
    let after_mark = out.create_text_label();
    out.mov_data_addr(Reg::V10, stw_fallback_pending);
    out.load_ptr_disp32(Reg::V0, Reg::V10, 0);
    out.test_reg_reg(Reg::V0, Reg::V0);
    out.jcc_label(Condition::NotEqual, do_degrade);
    out.call_label(targets.drain);
    out.mov_data_addr(Reg::V10, stw_fallback_pending);
    out.load_ptr_disp32(Reg::V0, Reg::V10, 0);
    out.test_reg_reg(Reg::V0, Reg::V0);
    out.jcc_label(Condition::Equal, after_mark);
    out.bind_text_label(do_degrade);
    out.mov_data_addr(Reg::V10, stw_fallbacks);
    out.load_ptr_disp32(Reg::V0, Reg::V10, 0);
    out.add_reg_imm32(Reg::V0, 1);
    out.store_ptr_disp32(Reg::V10, 0, Reg::V0);
    out.call_label(targets.stw_mark_complete);
    out.bind_text_label(after_mark);
    // Free the previous cycle's ghost (from-space) regions: the mark just
    // reached fixpoint, so no live slot references them (soundness
    // invariant III). A no-op until evacuation runs.
    out.call_label(targets.free_ghost_regions);
    out.call_label(targets.sweep);
    // Close the cycle. gc_relocate_start selects from-space regions, fixes
    // roots, and enters the Relocate phase when evacuation is active;
    // until the activation step it just returns to Idle and resets the
    // proactive-trigger counter.
    out.call_label(targets.relocate_start);
    emit_gc_pause_end(out, timing, pause);
    out.leave();
    out.ret();
}

/// Portable version of `gc_relocate_finish`: flush the final to-space
/// region's watermark back into `region_top`, clear the evac cursor, then
/// return the collector to Idle and reset the proactive-cycle byte
/// counter. Reuses `RegionTables` for region_base / region_top; the four
/// evac/phase labels beyond that set are passed individually.
pub fn emit_gc_relocate_finish<E: PortableAsm>(
    out: &mut E,
    entry: E::TextLabel,
    t: RegionTables<E::DataLabel>,
    evac_region_base: E::DataLabel,
    evac_top: E::DataLabel,
    phase: E::DataLabel,
    bytes_since_cycle: E::DataLabel,
) {
    use crate::gc_layout::{GC_PHASE_IDLE, GC_REGION_SHIFT};

    out.bind_text_label(entry);
    out.push_reg(Reg::V5); // rbp
    out.mov_reg_reg(Reg::V5, Reg::V4); // rbp = rsp
    // Flush the final to-space region's watermark.
    out.mov_data_addr(Reg::V10, evac_region_base);
    out.load_ptr_disp32(Reg::V0, Reg::V10, 0);
    let no_flush = out.create_text_label();
    out.test_reg_reg(Reg::V0, Reg::V0);
    out.jcc_label(Condition::Equal, no_flush);
    out.mov_data_addr(Reg::V10, t.region_base);
    out.load_ptr_disp32(Reg::V11, Reg::V10, 0);
    out.sub_reg_reg(Reg::V0, Reg::V11);
    out.shr_reg_imm8(Reg::V0, GC_REGION_SHIFT);
    out.shl_reg_imm8(Reg::V0, 3);
    out.mov_data_addr(Reg::V10, t.region_top);
    out.add_reg_reg(Reg::V10, Reg::V0);
    out.mov_data_addr(Reg::V8, evac_top);
    out.load_ptr_disp32(Reg::V2, Reg::V8, 0);
    out.store_ptr_disp32(Reg::V10, 0, Reg::V2);
    out.mov_imm64(Reg::V0, 0);
    out.mov_data_addr(Reg::V10, evac_region_base);
    out.store_ptr_disp32(Reg::V10, 0, Reg::V0);
    out.bind_text_label(no_flush);
    out.mov_data_addr(Reg::V10, phase);
    out.mov_imm64(Reg::V0, GC_PHASE_IDLE);
    out.store_ptr_disp32(Reg::V10, 0, Reg::V0);
    out.mov_data_addr(Reg::V10, bytes_since_cycle);
    out.mov_imm64(Reg::V0, 0);
    out.store_ptr_disp32(Reg::V10, 0, Reg::V0);
    out.leave();
    out.ret();
}

/// Data cells the mutator allocation entry reads/writes.
#[derive(Clone, Copy)]
pub struct AllocCells<D: Copy> {
    pub phase: D,
    pub bytes_since_cycle: D,
    pub budget_regions: D,
    pub bytes_since_quantum: D,
    pub mark_worklist_top: D,
    pub header_mark: D,
    pub alloc_count: D,
    pub bytes_allocated: D,
    pub heap_top: D,
    pub heap_end: D,
    pub oom_text: D,
}

/// Text labels the mutator allocation entry dispatches to.
#[derive(Clone, Copy)]
pub struct AllocTargets<T: Copy> {
    pub mark_start: T,
    pub trace: T,
    pub mark_end: T,
    pub relocate_quantum: T,
    pub collect: T,
    pub grow_budget: T,
    pub acquire_region: T,
    pub alloc_large: T,
}

/// Host-config flags that gate mutator-allocation codegen.
#[derive(Clone, Copy)]
pub struct AllocFlags {
    /// `--gc-stress`: collect before every allocation attempt.
    pub stress: bool,
    /// `--gc-log`: count allocations and bytes.
    pub log: bool,
}

/// One allocation attempt: bump within the current region (acquiring a
/// fresh one on the boundary), or the large-object run for requests
/// bigger than one region. Jumps to `fail` when no region can satisfy the
/// request and to `success` (V0 = user pointer) on success. Reads the
/// aligned total from frame slot 16 and the type tag from slot 24 (set by
/// the caller). Emitted inline three times by `emit_gc_alloc`.
pub fn emit_gc_alloc_attempt<E: PortableAsm>(
    out: &mut E,
    fail: E::TextLabel,
    success: E::TextLabel,
    heap_top: E::DataLabel,
    heap_end: E::DataLabel,
    acquire_region: E::TextLabel,
    alloc_large: E::TextLabel,
) {
    use crate::gc_layout::GC_REGION_SIZE;

    let large = out.create_text_label();
    let need_region = out.create_text_label();
    let do_bump = out.create_text_label();

    // total = [rbp - 16]; requests larger than a region go the large path.
    out.load_rbp_slot(Reg::V7, 16);
    out.mov_imm64(Reg::V1, GC_REGION_SIZE);
    out.cmp_reg_reg(Reg::V7, Reg::V1);
    out.jcc_label(Condition::Above, large);

    // ---- Bump within the current region ----
    out.bind_text_label(do_bump);
    out.mov_data_addr(Reg::V10, heap_top);
    out.load_ptr_disp32(Reg::V11, Reg::V10, 0);
    out.load_rbp_slot(Reg::V7, 16);
    out.mov_reg_reg(Reg::V1, Reg::V11);
    out.add_reg_reg(Reg::V1, Reg::V7);
    out.mov_data_addr(Reg::V8, heap_end);
    out.load_ptr_disp32(Reg::V8, Reg::V8, 0);
    out.cmp_reg_reg(Reg::V1, Reg::V8);
    out.jcc_label(Condition::Above, need_region);
    out.store_ptr_disp32(Reg::V10, 0, Reg::V1);
    out.store_ptr_disp32(Reg::V11, 0, Reg::V7);
    out.load_rbp_slot(Reg::V6, 24);
    out.store_ptr_disp32(Reg::V11, 8, Reg::V6);
    out.mov_reg_reg(Reg::V0, Reg::V11);
    out.add_reg_imm32(Reg::V0, 16);
    out.jmp_label(success);

    // Current region is full: acquire a fresh empty one and retry the bump
    // (guaranteed to fit, since total <= region size).
    out.bind_text_label(need_region);
    out.call_label(acquire_region);
    out.test_reg_reg(Reg::V0, Reg::V0);
    out.jcc_label(Condition::Equal, fail);
    out.jmp_label(do_bump);

    // ---- Large object (> one region): contiguous region run. ----
    out.bind_text_label(large);
    out.load_rbp_slot(Reg::V7, 16);
    out.load_rbp_slot(Reg::V6, 24);
    out.call_label(alloc_large);
    out.test_reg_reg(Reg::V0, Reg::V0);
    out.jcc_label(Condition::Equal, fail);
    out.jmp_label(success);
}

/// Portable version of `gc_alloc` (V7/rdi = payload size, V6/rsi = type
/// tag -> V0/rax = user pointer). The mutator's allocation entry: runs the
/// incremental-mark driver (proactive cycle start / one mark or relocate
/// quantum), optionally `--gc-stress`-collects, then attempts the bump,
/// retrying after a collect and a budget grow before OOM. Sets the
/// allocate-black mark during Mark and bumps the `--gc-log` counters.
/// Frame: pushes rbx, slot 16 = aligned total, slot 24 = type tag.
pub fn emit_gc_alloc<E: PortableAsm>(
    out: &mut E,
    entry: E::TextLabel,
    c: AllocCells<E::DataLabel>,
    t: AllocTargets<E::TextLabel>,
    flags: AllocFlags,
    oom_len: usize,
    stderr_fd: u64,
) {
    use crate::gc_layout::{
        GC_PHASE_MARK, GC_PHASE_RELOCATE, GC_QUANTUM_BYTES, GC_QUANTUM_POPS, GC_REGION_SHIFT,
    };

    out.bind_text_label(entry);
    out.push_reg(Reg::V5); // rbp
    out.mov_reg_reg(Reg::V5, Reg::V4); // rbp = rsp
    out.push_reg(Reg::V3); // rbx
    // Reserve two qwords for locals.
    out.sub_reg_imm8(Reg::V4, 16);
    // total = align_up(rdi + 16, 16)
    out.add_reg_imm32(Reg::V7, 16 + 15);
    out.and_reg_imm32(Reg::V7, -16);
    out.store_rbp_slot(16, Reg::V7);
    out.store_rbp_slot(24, Reg::V6);

    let success = out.create_text_label();
    let do_collect = out.create_text_label();
    let do_grow = out.create_text_label();
    let oom = out.create_text_label();
    let after_collect = out.create_text_label();

    // ---- M6 incremental-mark driver (before the attempt, so it observes
    // only the mutator's already-rooted state). None of the routines here
    // allocate, so gc_alloc is not re-entered.
    let driver_mark = out.create_text_label();
    let driver_reloc = out.create_text_label();
    let driver_done = out.create_text_label();
    out.mov_data_addr(Reg::V10, c.phase);
    out.load_ptr_disp32(Reg::V11, Reg::V10, 0);
    out.cmp_reg_imm8(Reg::V11, GC_PHASE_MARK as i8);
    out.jcc_label(Condition::Equal, driver_mark);
    out.cmp_reg_imm8(Reg::V11, GC_PHASE_RELOCATE as i8);
    out.jcc_label(Condition::Equal, driver_reloc);
    // Idle: accumulate allocation pressure and start a cycle proactively
    // once it crosses half the soft budget.
    out.mov_data_addr(Reg::V10, c.bytes_since_cycle);
    out.load_ptr_disp32(Reg::V0, Reg::V10, 0);
    out.load_rbp_slot(Reg::V11, 16); // total block bytes
    out.add_reg_reg(Reg::V0, Reg::V11);
    out.store_ptr_disp32(Reg::V10, 0, Reg::V0);
    // threshold = budget_regions * REGION_SIZE / 2 = budget << (SHIFT-1).
    out.mov_data_addr(Reg::V10, c.budget_regions);
    out.load_ptr_disp32(Reg::V1, Reg::V10, 0);
    out.shl_reg_imm8(Reg::V1, GC_REGION_SHIFT - 1);
    out.cmp_reg_reg(Reg::V0, Reg::V1);
    out.jcc_label(Condition::Below, driver_done);
    out.call_label(t.mark_start);
    out.jmp_label(driver_done);
    // Mark: run one bounded quantum per GC_QUANTUM_BYTES allocated; when
    // the worklist empties finish the cycle via MarkEnd.
    out.bind_text_label(driver_mark);
    out.mov_data_addr(Reg::V10, c.bytes_since_quantum);
    out.load_ptr_disp32(Reg::V0, Reg::V10, 0);
    out.load_rbp_slot(Reg::V11, 16);
    out.add_reg_reg(Reg::V0, Reg::V11);
    out.store_ptr_disp32(Reg::V10, 0, Reg::V0);
    out.cmp_reg_imm32(Reg::V0, GC_QUANTUM_BYTES as i32);
    out.jcc_label(Condition::Below, driver_done);
    out.mov_imm64(Reg::V0, 0);
    out.store_ptr_disp32(Reg::V10, 0, Reg::V0); // reset counter
    out.mov_imm64(Reg::V7, GC_QUANTUM_POPS);
    out.call_label(t.trace);
    out.mov_data_addr(Reg::V10, c.mark_worklist_top);
    out.load_ptr_disp32(Reg::V0, Reg::V10, 0);
    out.test_reg_reg(Reg::V0, Reg::V0);
    out.jcc_label(Condition::NotEqual, driver_done);
    out.call_label(t.mark_end);
    out.jmp_label(driver_done);
    // Relocate: run one bounded evacuation quantum per GC_QUANTUM_BYTES.
    out.bind_text_label(driver_reloc);
    out.mov_data_addr(Reg::V10, c.bytes_since_quantum);
    out.load_ptr_disp32(Reg::V0, Reg::V10, 0);
    out.load_rbp_slot(Reg::V11, 16);
    out.add_reg_reg(Reg::V0, Reg::V11);
    out.store_ptr_disp32(Reg::V10, 0, Reg::V0);
    out.cmp_reg_imm32(Reg::V0, GC_QUANTUM_BYTES as i32);
    out.jcc_label(Condition::Below, driver_done);
    out.mov_imm64(Reg::V0, 0);
    out.store_ptr_disp32(Reg::V10, 0, Reg::V0);
    out.mov_imm64(Reg::V7, GC_QUANTUM_BYTES);
    out.call_label(t.relocate_quantum);
    out.bind_text_label(driver_done);

    // --gc-stress: collect before every allocation attempt.
    if flags.stress {
        out.call_label(t.collect);
    }

    emit_gc_alloc_attempt(
        out,
        do_collect,
        success,
        c.heap_top,
        c.heap_end,
        t.acquire_region,
        t.alloc_large,
    );
    // First attempt failed; run collector then retry.
    out.bind_text_label(do_collect);
    out.call_label(t.collect);
    out.bind_text_label(after_collect);
    emit_gc_alloc_attempt(
        out,
        do_grow,
        success,
        c.heap_top,
        c.heap_end,
        t.acquire_region,
        t.alloc_large,
    );

    // Collector freed no usable region -- raise the soft budget and retry
    // once more. gc_grow_budget returns 1 on success, 0 when even the full
    // reservation cannot hold the request.
    out.bind_text_label(do_grow);
    out.load_rbp_slot(Reg::V7, 16);
    out.call_label(t.grow_budget);
    out.test_reg_reg(Reg::V0, Reg::V0);
    out.jcc_label(Condition::Equal, oom);
    emit_gc_alloc_attempt(
        out,
        oom,
        success,
        c.heap_top,
        c.heap_end,
        t.acquire_region,
        t.alloc_large,
    );

    out.bind_text_label(oom);
    out.plat_write_data(stderr_fd, c.oom_text, oom_len);
    out.plat_exit(1);

    out.bind_text_label(success);
    // Allocate-black: an object allocated during Mark is born live this
    // cycle (its current-parity header mark bit is set). rax = user
    // pointer must survive, so this uses r10/r11 only. Only during Mark.
    let skip_black = out.create_text_label();
    out.mov_data_addr(Reg::V10, c.phase);
    out.load_ptr_disp32(Reg::V11, Reg::V10, 0);
    out.cmp_reg_imm8(Reg::V11, GC_PHASE_MARK as i8);
    out.jcc_label(Condition::NotEqual, skip_black);
    out.load_ptr_disp32(Reg::V11, Reg::V0, -16);
    out.mov_data_addr(Reg::V10, c.header_mark);
    out.load_ptr_disp32(Reg::V10, Reg::V10, 0);
    out.or_reg_reg(Reg::V11, Reg::V10);
    out.store_ptr_disp32(Reg::V0, -16, Reg::V11);
    out.bind_text_label(skip_black);
    // --gc-log: count this allocation and its byte size. rax holds the
    // user pointer and must survive, so the counter bumps use r10/r11/rbx.
    if flags.log {
        out.mov_data_addr(Reg::V10, c.alloc_count);
        out.load_ptr_disp32(Reg::V11, Reg::V10, 0);
        out.add_reg_imm32(Reg::V11, 1);
        out.store_ptr_disp32(Reg::V10, 0, Reg::V11);
        out.load_rbp_slot(Reg::V11, 16); // total block bytes
        out.mov_data_addr(Reg::V10, c.bytes_allocated);
        out.load_ptr_disp32(Reg::V3, Reg::V10, 0);
        out.add_reg_reg(Reg::V3, Reg::V11);
        out.store_ptr_disp32(Reg::V10, 0, Reg::V3);
    }
    // Tear down locals and return.
    out.add_reg_imm32(Reg::V4, 16);
    out.pop_reg(Reg::V3);
    out.leave();
    out.ret();
}

/// Portable version of `gc_alloc_large` (V7/rdi = total size, V6/rsi =
/// type tag -> V0/rax = user pointer, or 0 on budget exhaustion). Commits
/// a contiguous run of `N = ceil(total / REGION_SIZE)` tail regions, lays
/// the object's header into the first, sets the first region's watermark
/// to span the whole object, marks the member regions empty, and returns
/// `base + 16`. Leaf (no frame). Reuses `RegionTables` for committed_count
/// / region_base / region_top; budget_regions is the extra label.
pub fn emit_gc_alloc_large<E: PortableAsm>(
    out: &mut E,
    entry: E::TextLabel,
    t: RegionTables<E::DataLabel>,
    budget_regions: E::DataLabel,
) {
    use crate::gc_layout::{GC_REGION_SHIFT, GC_REGION_SIZE};

    out.bind_text_label(entry);
    let member_loop = out.create_text_label();
    let member_done = out.create_text_label();
    let fail = out.create_text_label();

    // rcx = N = ceil(total / REGION_SIZE) = (total + REGION_SIZE-1) >> SHIFT
    out.mov_reg_reg(Reg::V1, Reg::V7);
    out.add_reg_imm32(Reg::V1, (GC_REGION_SIZE - 1) as i32);
    out.shr_reg_imm8(Reg::V1, GC_REGION_SHIFT);
    // r8 = committed, r9 = committed + N (new committed). Check budget.
    out.mov_data_addr(Reg::V10, t.committed_count);
    out.load_ptr_disp32(Reg::V8, Reg::V10, 0);
    out.mov_reg_reg(Reg::V9, Reg::V8);
    out.add_reg_reg(Reg::V9, Reg::V1);
    out.mov_data_addr(Reg::V11, budget_regions);
    out.load_ptr_disp32(Reg::V11, Reg::V11, 0);
    out.cmp_reg_reg(Reg::V9, Reg::V11);
    out.jcc_label(Condition::Above, fail);
    // base_f = region_base + (committed << REGION_SHIFT).
    out.mov_reg_reg(Reg::V0, Reg::V8);
    out.shl_reg_imm8(Reg::V0, GC_REGION_SHIFT);
    out.mov_data_addr(Reg::V10, t.region_base);
    out.load_ptr_disp32(Reg::V10, Reg::V10, 0);
    out.add_reg_reg(Reg::V0, Reg::V10); // rax = base_f
    // committed_count = committed + N.
    out.mov_data_addr(Reg::V10, t.committed_count);
    out.store_ptr_disp32(Reg::V10, 0, Reg::V9);
    // Header at base_f: [base_f] = total, [base_f+8] = tag.
    out.store_ptr_disp32(Reg::V0, 0, Reg::V7);
    out.store_ptr_disp32(Reg::V0, 8, Reg::V6);
    // region_top[f] = base_f + total (spans the whole run's object).
    out.mov_reg_reg(Reg::V11, Reg::V8);
    out.shl_reg_imm8(Reg::V11, 3);
    out.mov_data_addr(Reg::V10, t.region_top);
    out.add_reg_reg(Reg::V10, Reg::V11);
    out.mov_reg_reg(Reg::V11, Reg::V0);
    out.add_reg_reg(Reg::V11, Reg::V7);
    out.store_ptr_disp32(Reg::V10, 0, Reg::V11);
    // Member regions f+1 .. f+N-1: region_top[m] = its base (empty).
    // r8 = m index (start f+1), r9 = f+N bound.
    out.add_reg_imm32(Reg::V8, 1);
    out.bind_text_label(member_loop);
    out.cmp_reg_reg(Reg::V8, Reg::V9);
    out.jcc_label(Condition::AboveOrEqual, member_done);
    // addr = region_base + (m << REGION_SHIFT).
    out.mov_reg_reg(Reg::V11, Reg::V8);
    out.shl_reg_imm8(Reg::V11, GC_REGION_SHIFT);
    out.mov_data_addr(Reg::V10, t.region_base);
    out.load_ptr_disp32(Reg::V10, Reg::V10, 0);
    out.add_reg_reg(Reg::V11, Reg::V10); // r11 = member base
    // region_top[m] = member base.
    out.mov_reg_reg(Reg::V1, Reg::V8);
    out.shl_reg_imm8(Reg::V1, 3);
    out.mov_data_addr(Reg::V10, t.region_top);
    out.add_reg_reg(Reg::V10, Reg::V1);
    out.store_ptr_disp32(Reg::V10, 0, Reg::V11);
    out.add_reg_imm32(Reg::V8, 1);
    out.jmp_label(member_loop);
    out.bind_text_label(member_done);
    // return rax = base_f + 16.
    out.add_reg_imm32(Reg::V0, 16);
    out.ret();

    out.bind_text_label(fail);
    out.mov_imm64(Reg::V0, 0);
    out.ret();
}

/// Portable version of `gc_acquire_region`: obtain a fresh mutator
/// region -- first flushing the current region's watermark, then popping
/// the free-region pool (zeroing the reused region so stale bytes never
/// read as bogus pointers), else committing the next tail region if
/// within budget. Installs the region into the bump globals and returns
/// `V0` = 1 on success, 0 if the budget is exhausted. Leaf (no frame).
/// Reuses `RegionTables` for heap_base / heap_top / region_base /
/// region_top / committed_count; free_region_head / budget_regions /
/// heap_end are the labels beyond that set.
pub fn emit_gc_acquire_region<E: PortableAsm>(
    out: &mut E,
    entry: E::TextLabel,
    t: RegionTables<E::DataLabel>,
    free_region_head: E::DataLabel,
    budget_regions: E::DataLabel,
    heap_end: E::DataLabel,
) {
    use crate::gc_layout::{GC_REGION_SHIFT, GC_REGION_SIZE};

    out.bind_text_label(entry);
    let from_tail = out.create_text_label();
    let install = out.create_text_label();
    let fail = out.create_text_label();

    // region_top[cur] = gc_heap_top, where cur = (heap_base -
    // region_base) >> REGION_SHIFT.
    out.mov_data_addr(Reg::V10, t.heap_base);
    out.load_ptr_disp32(Reg::V1, Reg::V10, 0);
    out.mov_data_addr(Reg::V10, t.region_base);
    out.load_ptr_disp32(Reg::V11, Reg::V10, 0);
    out.sub_reg_reg(Reg::V1, Reg::V11);
    out.shr_reg_imm8(Reg::V1, GC_REGION_SHIFT);
    out.shl_reg_imm8(Reg::V1, 3);
    out.mov_data_addr(Reg::V10, t.region_top);
    out.add_reg_reg(Reg::V10, Reg::V1);
    out.mov_data_addr(Reg::V8, t.heap_top);
    out.load_ptr_disp32(Reg::V0, Reg::V8, 0);
    out.store_ptr_disp32(Reg::V10, 0, Reg::V0);

    // (1) Free-region pool: rax = free_region_head; if non-null pop it.
    out.mov_data_addr(Reg::V10, free_region_head);
    out.load_ptr_disp32(Reg::V0, Reg::V10, 0);
    out.test_reg_reg(Reg::V0, Reg::V0);
    out.jcc_label(Condition::Equal, from_tail);
    // free_region_head = [rax] (link stored at the region base).
    out.load_ptr_disp32(Reg::V1, Reg::V0, 0);
    out.store_ptr_disp32(Reg::V10, 0, Reg::V1);
    // Zero the reused region: unlike a fresh mmap'd tail region, a
    // reclaimed region still holds its previous objects' bytes. The bump
    // path does not clear an object's padding qwords (those within the
    // 16-aligned block size that the constructor doesn't write), and the
    // mark trace walks every payload qword of a pointer object -- so a
    // stale non-null value there would be chased as a bogus pointer.
    // Clearing the whole region once on reuse restores the "all bump
    // memory reads as zero" invariant.
    let zero_loop = out.create_text_label();
    let zero_done = out.create_text_label();
    out.mov_reg_reg(Reg::V8, Reg::V0); // cursor
    out.mov_reg_reg(Reg::V11, Reg::V0);
    out.add_reg_imm32(Reg::V11, GC_REGION_SIZE as i32); // end
    out.mov_imm64(Reg::V9, 0);
    out.bind_text_label(zero_loop);
    out.cmp_reg_reg(Reg::V8, Reg::V11);
    out.jcc_label(Condition::AboveOrEqual, zero_done);
    out.store_ptr_disp32(Reg::V8, 0, Reg::V9);
    out.add_reg_imm32(Reg::V8, 8);
    out.jmp_label(zero_loop);
    out.bind_text_label(zero_done);
    out.jmp_label(install);

    // (2) Commit the next tail region if within budget.
    out.bind_text_label(from_tail);
    out.mov_data_addr(Reg::V10, t.committed_count);
    out.load_ptr_disp32(Reg::V0, Reg::V10, 0);
    out.mov_data_addr(Reg::V8, budget_regions);
    out.load_ptr_disp32(Reg::V11, Reg::V8, 0);
    out.cmp_reg_reg(Reg::V0, Reg::V11);
    out.jcc_label(Condition::AboveOrEqual, fail);
    // new_base = region_base + (committed << REGION_SHIFT).
    out.mov_reg_reg(Reg::V1, Reg::V0);
    out.shl_reg_imm8(Reg::V1, GC_REGION_SHIFT);
    out.mov_data_addr(Reg::V8, t.region_base);
    out.load_ptr_disp32(Reg::V8, Reg::V8, 0);
    out.add_reg_reg(Reg::V8, Reg::V1);
    // committed_count = committed + 1.
    out.add_reg_imm32(Reg::V0, 1);
    out.store_ptr_disp32(Reg::V10, 0, Reg::V0);
    out.mov_reg_reg(Reg::V0, Reg::V8);

    // install: rax = new region base. Set the bump globals.
    out.bind_text_label(install);
    out.mov_data_addr(Reg::V10, t.heap_base);
    out.store_ptr_disp32(Reg::V10, 0, Reg::V0);
    out.mov_data_addr(Reg::V10, t.heap_top);
    out.store_ptr_disp32(Reg::V10, 0, Reg::V0);
    out.mov_reg_reg(Reg::V1, Reg::V0);
    out.add_reg_imm32(Reg::V1, GC_REGION_SIZE as i32);
    out.mov_data_addr(Reg::V10, heap_end);
    out.store_ptr_disp32(Reg::V10, 0, Reg::V1);
    out.mov_imm64(Reg::V0, 1);
    out.ret();

    out.bind_text_label(fail);
    out.mov_imm64(Reg::V0, 0);
    out.ret();
}

/// All the data cells `gc_relocate_start` reads/writes across its color
/// setup, sparsest-first selection, and Relocate/Idle entry. Grouped into
/// one parameter because the routine touches ~18 globals.
#[derive(Clone, Copy)]
pub struct RelocateStartCells<D: Copy> {
    pub good_color: D,
    pub bad_mask: D,
    pub free_region_head: D,
    pub budget_regions: D,
    pub committed_count: D,
    pub heap_base: D,
    pub region_base: D,
    pub region_top: D,
    pub region_live: D,
    pub region_fromspace: D,
    pub evac_region_base: D,
    pub evac_top: D,
    pub evac_end: D,
    pub reloc_scan_idx: D,
    pub reloc_block: D,
    pub bytes_since_quantum: D,
    pub phase: D,
    pub bytes_since_cycle: D,
}

/// Portable version of `gc_relocate_start` (fused into the MarkEnd pause).
/// When evacuation is active, switch to the Idle/Relocate color config
/// (good = R), compute the evacuation byte cap from the free-region count,
/// select the sparsest non-current, non-large from-space regions within
/// the headroom cap, fix up the roots to their to-space copies, reset the
/// relocate cursor + quantum pacer, and enter the Relocate phase. If
/// evacuation is compiled off (`evac_off`), or nothing worth evacuating is
/// selected, close the cycle straight to Idle. `poison` forces
/// `bad = COLOR_MASK` (the barrier-coverage canary). Both flags gate
/// codegen and are passed in.
pub fn emit_gc_relocate_start<E: PortableAsm>(
    out: &mut E,
    entry: E::TextLabel,
    c: RelocateStartCells<E::DataLabel>,
    fix_roots: E::TextLabel,
    evac_off: bool,
    poison: bool,
) {
    use crate::gc_layout::{
        GC_COLOR_M0, GC_COLOR_M1, GC_COLOR_MASK, GC_COLOR_R, GC_PHASE_IDLE, GC_PHASE_RELOCATE,
        GC_REGION_SHIFT, GC_REGION_SIZE,
    };

    out.bind_text_label(entry);
    out.push_reg(Reg::V5); // rbp
    out.mov_reg_reg(Reg::V5, Reg::V4); // rbp = rsp
    let mark_only = out.create_text_label();
    if !evac_off {
        // slots: 8=idx, 16=committed, 24=cap_bytes, 32=selected_live,
        // 40=cur_bump_idx.
        out.sub_reg_imm8(Reg::V4, 48);
        // Idle/Relocate color config: good = R, bad = M0|M1 (or COLOR_MASK
        // under poison). Applies to both the Relocate entry and the
        // mark-only fallback (both end at good = R).
        out.mov_imm64(Reg::V0, GC_COLOR_R);
        out.mov_data_addr(Reg::V10, c.good_color);
        out.store_ptr_disp32(Reg::V10, 0, Reg::V0);
        if poison {
            out.mov_imm64(Reg::V0, GC_COLOR_MASK);
        } else {
            out.mov_imm64(Reg::V0, GC_COLOR_M0 | GC_COLOR_M1);
        }
        out.mov_data_addr(Reg::V10, c.bad_mask);
        out.store_ptr_disp32(Reg::V10, 0, Reg::V0);
        out.mov_data_addr(Reg::GoodColor, c.good_color);
        out.load_ptr_disp32(Reg::GoodColor, Reg::GoodColor, 0);
        out.mov_data_addr(Reg::BadMask, c.bad_mask);
        out.load_ptr_disp32(Reg::BadMask, Reg::BadMask, 0);
        // free_regions = (budget - committed) + free-pool count.
        out.mov_imm64(Reg::V1, 0);
        out.mov_data_addr(Reg::V10, c.free_region_head);
        out.load_ptr_disp32(Reg::V0, Reg::V10, 0);
        let fp_loop = out.create_text_label();
        let fp_done = out.create_text_label();
        out.bind_text_label(fp_loop);
        out.test_reg_reg(Reg::V0, Reg::V0);
        out.jcc_label(Condition::Equal, fp_done);
        out.add_reg_imm32(Reg::V1, 1);
        out.load_ptr_disp32(Reg::V0, Reg::V0, 0);
        out.jmp_label(fp_loop);
        out.bind_text_label(fp_done);
        out.mov_data_addr(Reg::V10, c.budget_regions);
        out.load_ptr_disp32(Reg::V0, Reg::V10, 0);
        out.mov_data_addr(Reg::V10, c.committed_count);
        out.load_ptr_disp32(Reg::V11, Reg::V10, 0);
        out.sub_reg_reg(Reg::V0, Reg::V11);
        out.add_reg_reg(Reg::V0, Reg::V1); // rax = free_regions
        // cap_bytes = (free_regions >= 2) ? (free_regions-2)<<(SHIFT-1) : 0.
        // Half the free bytes keeps the region count within free_regions-2
        // even at ~50% packing, so acquire_evac_region never runs past the
        // reservation.
        let have_cap = out.create_text_label();
        let store_cap = out.create_text_label();
        out.cmp_reg_imm8(Reg::V0, 2);
        out.jcc_label(Condition::AboveOrEqual, have_cap);
        out.mov_imm64(Reg::V0, 0);
        out.jmp_label(store_cap);
        out.bind_text_label(have_cap);
        out.sub_reg_imm8(Reg::V0, 2);
        out.shl_reg_imm8(Reg::V0, GC_REGION_SHIFT - 1);
        out.bind_text_label(store_cap);
        out.store_rbp_slot(24, Reg::V0); // cap_bytes
        // cur_bump_idx = (heap_base - region_base) >> SHIFT.
        out.mov_data_addr(Reg::V10, c.heap_base);
        out.load_ptr_disp32(Reg::V0, Reg::V10, 0);
        out.mov_data_addr(Reg::V10, c.region_base);
        out.load_ptr_disp32(Reg::V11, Reg::V10, 0);
        out.sub_reg_reg(Reg::V0, Reg::V11);
        out.shr_reg_imm8(Reg::V0, GC_REGION_SHIFT);
        out.store_rbp_slot(40, Reg::V0);
        out.mov_data_addr(Reg::V10, c.committed_count);
        out.load_ptr_disp32(Reg::V0, Reg::V10, 0);
        out.store_rbp_slot(16, Reg::V0); // committed bound
        out.mov_imm64(Reg::V0, 0);
        out.store_rbp_slot(8, Reg::V0); // idx
        out.store_rbp_slot(32, Reg::V0); // selected_live
        let sel_loop = out.create_text_label();
        let sel_next = out.create_text_label();
        let sel_done = out.create_text_label();
        out.bind_text_label(sel_loop);
        out.load_rbp_slot(Reg::V0, 8);
        out.load_rbp_slot(Reg::V1, 16);
        out.cmp_reg_reg(Reg::V0, Reg::V1);
        out.jcc_label(Condition::AboveOrEqual, sel_done);
        // skip the current bump region.
        out.load_rbp_slot(Reg::V1, 40);
        out.cmp_reg_reg(Reg::V0, Reg::V1);
        out.jcc_label(Condition::Equal, sel_next);
        // base = region_base + idx<<SHIFT ; top = region_top[idx].
        out.mov_reg_reg(Reg::V1, Reg::V0);
        out.shl_reg_imm8(Reg::V1, GC_REGION_SHIFT);
        out.mov_data_addr(Reg::V10, c.region_base);
        out.load_ptr_disp32(Reg::V10, Reg::V10, 0);
        out.add_reg_reg(Reg::V1, Reg::V10); // base (rcx)
        out.load_rbp_slot(Reg::V8, 8);
        out.shl_reg_imm8(Reg::V8, 3);
        out.mov_data_addr(Reg::V10, c.region_top);
        out.add_reg_reg(Reg::V10, Reg::V8);
        out.load_ptr_disp32(Reg::V8, Reg::V10, 0); // top (r8)
        // empty (top == base)?
        out.cmp_reg_reg(Reg::V8, Reg::V1);
        out.jcc_label(Condition::Equal, sel_next);
        // large (top - base > REGION_SIZE)?
        out.sub_reg_reg(Reg::V8, Reg::V1); // used bytes
        out.cmp_reg_imm32(Reg::V8, GC_REGION_SIZE as i32);
        out.jcc_label(Condition::Above, sel_next);
        // live = gc_region_live[idx].
        out.load_rbp_slot(Reg::V8, 8);
        out.shl_reg_imm8(Reg::V8, 3);
        out.mov_data_addr(Reg::V10, c.region_live);
        out.add_reg_reg(Reg::V10, Reg::V8);
        out.load_ptr_disp32(Reg::V11, Reg::V10, 0); // live (r11)
        // not sparse (live >= REGION_SIZE/2)?
        out.cmp_reg_imm32(Reg::V11, (GC_REGION_SIZE / 2) as i32);
        out.jcc_label(Condition::AboveOrEqual, sel_next);
        // headroom (selected_live + live > cap)?
        out.load_rbp_slot(Reg::V0, 32);
        out.add_reg_reg(Reg::V0, Reg::V11);
        out.load_rbp_slot(Reg::V1, 24);
        out.cmp_reg_reg(Reg::V0, Reg::V1);
        out.jcc_label(Condition::Above, sel_next);
        // select: fromspace[idx] = 1 ; selected_live += live.
        out.store_rbp_slot(32, Reg::V0); // selected_live += live
        out.load_rbp_slot(Reg::V8, 8);
        out.shl_reg_imm8(Reg::V8, 3);
        out.mov_data_addr(Reg::V10, c.region_fromspace);
        out.add_reg_reg(Reg::V10, Reg::V8);
        out.mov_imm64(Reg::V0, 1);
        out.store_ptr_disp32(Reg::V10, 0, Reg::V0);
        out.bind_text_label(sel_next);
        out.load_rbp_slot(Reg::V0, 8);
        out.add_reg_imm32(Reg::V0, 1);
        out.store_rbp_slot(8, Reg::V0);
        out.jmp_label(sel_loop);
        out.bind_text_label(sel_done);
        // Nothing selected -> mark-only close.
        out.load_rbp_slot(Reg::V0, 32);
        out.test_reg_reg(Reg::V0, Reg::V0);
        out.jcc_label(Condition::Equal, mark_only);
        // Reset the evacuation bump so the first copy acquires a fresh
        // to-space region (relocate_finish only clears evac_region_base).
        out.mov_imm64(Reg::V0, 0);
        out.mov_data_addr(Reg::V10, c.evac_region_base);
        out.store_ptr_disp32(Reg::V10, 0, Reg::V0);
        out.mov_data_addr(Reg::V10, c.evac_top);
        out.store_ptr_disp32(Reg::V10, 0, Reg::V0);
        out.mov_data_addr(Reg::V10, c.evac_end);
        out.store_ptr_disp32(Reg::V10, 0, Reg::V0);
        // Fix roots, reset the relocate cursor + quantum pacer, enter the
        // Relocate phase.
        out.call_label(fix_roots);
        out.mov_imm64(Reg::V0, 0);
        out.mov_data_addr(Reg::V10, c.reloc_scan_idx);
        out.store_ptr_disp32(Reg::V10, 0, Reg::V0);
        out.mov_data_addr(Reg::V10, c.reloc_block);
        out.store_ptr_disp32(Reg::V10, 0, Reg::V0);
        out.mov_data_addr(Reg::V10, c.bytes_since_quantum);
        out.store_ptr_disp32(Reg::V10, 0, Reg::V0);
        out.mov_data_addr(Reg::V10, c.phase);
        out.mov_imm64(Reg::V0, GC_PHASE_RELOCATE);
        out.store_ptr_disp32(Reg::V10, 0, Reg::V0);
        out.leave();
        out.ret();
    }
    // Mark-only close (evac off, or nothing selected): back to Idle, reset
    // the proactive counter.
    out.bind_text_label(mark_only);
    out.mov_data_addr(Reg::V10, c.phase);
    out.mov_imm64(Reg::V0, GC_PHASE_IDLE);
    out.store_ptr_disp32(Reg::V10, 0, Reg::V0);
    out.mov_data_addr(Reg::V10, c.bytes_since_cycle);
    out.mov_imm64(Reg::V0, 0);
    out.store_ptr_disp32(Reg::V10, 0, Reg::V0);
    out.leave();
    out.ret();
}

/// The resumable-cursor and mark cells the relocate quantum reads.
#[derive(Clone, Copy)]
pub struct RelocQuantumCells<D: Copy> {
    pub reloc_scan_idx: D,
    pub reloc_block: D,
    pub header_mark: D,
}

/// Portable version of `gc_relocate_quantum` (V7/rdi = byte budget):
/// linearly walk the from-space regions (resumable cursor in
/// reloc_scan_idx / reloc_block), evacuating each live block until the
/// budget is spent or all from-space is drained. On drain, call
/// relocate_finish; otherwise save the block cursor for the next quantum.
/// Frame: 8 = budget, 16 = cur, 24 = top. Reuses `RegionTables` for
/// committed_count / region_base / region_top / region_fromspace.
pub fn emit_gc_relocate_quantum<E: PortableAsm>(
    out: &mut E,
    entry: E::TextLabel,
    t: RegionTables<E::DataLabel>,
    c: RelocQuantumCells<E::DataLabel>,
    evacuate: E::TextLabel,
    relocate_finish: E::TextLabel,
) {
    use crate::gc_layout::{GC_FWD, GC_REGION_SHIFT};

    out.bind_text_label(entry);
    out.push_reg(Reg::V5); // rbp
    out.mov_reg_reg(Reg::V5, Reg::V4); // rbp = rsp
    out.sub_reg_imm8(Reg::V4, 32); // 8=budget,16=cur,24=top
    out.store_rbp_slot(8, Reg::V7);
    let region_scan = out.create_text_label();
    let block_loop = out.create_text_label();
    let advance_region = out.create_text_label();
    let not_fwd = out.create_text_label();
    let after_block = out.create_text_label();
    let saved_cur = out.create_text_label();
    out.bind_text_label(region_scan);
    // idx = gc_reloc_scan_idx; if idx >= committed -> finish.
    out.mov_data_addr(Reg::V10, c.reloc_scan_idx);
    out.load_ptr_disp32(Reg::V0, Reg::V10, 0);
    out.mov_data_addr(Reg::V10, t.committed_count);
    out.load_ptr_disp32(Reg::V11, Reg::V10, 0);
    out.cmp_reg_reg(Reg::V0, Reg::V11);
    out.jcc_label(Condition::AboveOrEqual, saved_cur);
    // fromspace[idx]? if not, advance to next region.
    out.mov_reg_reg(Reg::V1, Reg::V0);
    out.shl_reg_imm8(Reg::V1, 3);
    out.mov_data_addr(Reg::V10, t.region_fromspace);
    out.add_reg_reg(Reg::V10, Reg::V1);
    out.load_ptr_disp32(Reg::V11, Reg::V10, 0);
    out.test_reg_reg(Reg::V11, Reg::V11);
    out.jcc_label(Condition::Equal, advance_region);
    // base = region_base + (idx << SHIFT) ; top = region_top[idx].
    out.mov_reg_reg(Reg::V1, Reg::V0);
    out.shl_reg_imm8(Reg::V1, GC_REGION_SHIFT);
    out.mov_data_addr(Reg::V10, t.region_base);
    out.load_ptr_disp32(Reg::V10, Reg::V10, 0);
    out.add_reg_reg(Reg::V1, Reg::V10); // base
    out.mov_reg_reg(Reg::V8, Reg::V0);
    out.shl_reg_imm8(Reg::V8, 3);
    out.mov_data_addr(Reg::V10, t.region_top);
    out.add_reg_reg(Reg::V10, Reg::V8);
    out.load_ptr_disp32(Reg::V8, Reg::V10, 0); // top
    out.store_rbp_slot(24, Reg::V8);
    // cur = gc_reloc_block; if 0 use base.
    out.mov_data_addr(Reg::V10, c.reloc_block);
    out.load_ptr_disp32(Reg::V0, Reg::V10, 0);
    out.test_reg_reg(Reg::V0, Reg::V0);
    let have_cur = out.create_text_label();
    out.jcc_label(Condition::NotEqual, have_cur);
    out.mov_reg_reg(Reg::V0, Reg::V1); // cur = base
    out.bind_text_label(have_cur);
    out.store_rbp_slot(16, Reg::V0);
    out.bind_text_label(block_loop);
    out.load_rbp_slot(Reg::V0, 16); // cur
    out.load_rbp_slot(Reg::V8, 24); // top
    out.cmp_reg_reg(Reg::V0, Reg::V8);
    out.jcc_label(Condition::AboveOrEqual, advance_region);
    // budget <= 0 ? save cur and return.
    out.load_rbp_slot(Reg::V1, 8);
    out.test_reg_reg(Reg::V1, Reg::V1);
    out.jcc_label(Condition::Equal, saved_cur);
    out.cmp_reg_imm8(Reg::V1, 0);
    out.jcc_label(Condition::Less, saved_cur);
    // word0 = [cur].
    out.load_ptr_disp32(Reg::V11, Reg::V0, 0);
    out.mov_reg_reg(Reg::V1, Reg::V11);
    out.and_reg_imm32(Reg::V1, GC_FWD as i32);
    out.test_reg_reg(Reg::V1, Reg::V1);
    out.jcc_label(Condition::Equal, not_fwd);
    // Forwarded: size from word1 = [cur+8].
    out.load_ptr_disp32(Reg::V1, Reg::V0, 8);
    out.jmp_label(after_block);
    out.bind_text_label(not_fwd);
    // size = word0 & -16.
    out.mov_reg_reg(Reg::V1, Reg::V11);
    out.and_reg_imm32(Reg::V1, -16);
    // live? (word0 & current mark bit)
    out.mov_data_addr(Reg::V10, c.header_mark);
    out.load_ptr_disp32(Reg::V8, Reg::V10, 0);
    out.test_reg_reg(Reg::V11, Reg::V8);
    out.jcc_label(Condition::Equal, after_block);
    // Live: evacuate(cur+16). Preserve cur/size in slots; the call
    // clobbers rax/rcx/etc. budget -= size.
    out.store_rbp_slot(16, Reg::V0); // (cur unchanged, keep)
    out.mov_reg_reg(Reg::V7, Reg::V0);
    out.add_reg_imm32(Reg::V7, 16); // user ptr
    // stash size in a callee-preserved-ish slot before the call.
    out.push_reg(Reg::V1); // size (one push, then call — realign)
    out.push_reg(Reg::V1); // pad to keep rsp 16-aligned
    out.call_label(evacuate);
    out.pop_reg(Reg::V1);
    out.pop_reg(Reg::V1); // rcx = size
    // budget -= size
    out.load_rbp_slot(Reg::V0, 8);
    out.sub_reg_reg(Reg::V0, Reg::V1);
    out.store_rbp_slot(8, Reg::V0);
    out.bind_text_label(after_block);
    // cur += size (rcx). Reload cur, advance, store.
    out.load_rbp_slot(Reg::V0, 16);
    out.add_reg_reg(Reg::V0, Reg::V1);
    out.store_rbp_slot(16, Reg::V0);
    out.jmp_label(block_loop);
    out.bind_text_label(advance_region);
    // gc_reloc_scan_idx++ ; gc_reloc_block = 0 ; rescan.
    out.mov_data_addr(Reg::V10, c.reloc_scan_idx);
    out.load_ptr_disp32(Reg::V0, Reg::V10, 0);
    out.add_reg_imm32(Reg::V0, 1);
    out.store_ptr_disp32(Reg::V10, 0, Reg::V0);
    out.mov_data_addr(Reg::V10, c.reloc_block);
    out.mov_imm64(Reg::V0, 0);
    out.store_ptr_disp32(Reg::V10, 0, Reg::V0);
    out.jmp_label(region_scan);
    out.bind_text_label(saved_cur);
    // Save the block cursor for resumption. gc_relocate_finish is called
    // only when idx >= committed.
    out.mov_data_addr(Reg::V10, c.reloc_scan_idx);
    out.load_ptr_disp32(Reg::V0, Reg::V10, 0);
    out.mov_data_addr(Reg::V10, t.committed_count);
    out.load_ptr_disp32(Reg::V11, Reg::V10, 0);
    out.cmp_reg_reg(Reg::V0, Reg::V11);
    let just_save = out.create_text_label();
    out.jcc_label(Condition::Below, just_save);
    out.call_label(relocate_finish);
    out.leave();
    out.ret();
    out.bind_text_label(just_save);
    out.load_rbp_slot(Reg::V0, 16);
    out.mov_data_addr(Reg::V10, c.reloc_block);
    out.store_ptr_disp32(Reg::V10, 0, Reg::V0);
    out.leave();
    out.ret();
}

/// The evacuation bump cursor cells plus the relocated-object counter.
#[derive(Clone, Copy)]
pub struct EvacBumpCells<D: Copy> {
    pub evac_top: D,
    pub evac_end: D,
    pub relocated_count: D,
}

/// Portable version of `gc_evacuate` (V7/rdi = user pointer -> V0/rax =
/// to-space user pointer). Idempotent: follows an existing forwarding
/// word. Otherwise copies the whole block into the evacuation bump
/// (refilling the to-space region as needed, aborting on an oversized
/// object that selection should have excluded), installs a forwarding
/// word `new_user | FWD` in the old header, stashes the size in the old
/// word1 for the relocate walk, and bumps the --gc-log relocated counter.
/// Non-recursive (only copies). Frame: [rbp-8] = oldP, [rbp-16] = size.
pub fn emit_gc_evacuate<E: PortableAsm>(
    out: &mut E,
    entry: E::TextLabel,
    c: EvacBumpCells<E::DataLabel>,
    acquire_evac_region: E::TextLabel,
    oversized_msg: E::DataLabel,
    oversized_len: usize,
    stderr_fd: u64,
) {
    use crate::gc_layout::{GC_FWD, GC_REGION_SIZE};

    out.bind_text_label(entry);
    out.push_reg(Reg::V5); // rbp
    out.mov_reg_reg(Reg::V5, Reg::V4); // rbp = rsp
    out.sub_reg_imm8(Reg::V4, 16); // [rbp-8]=oldP, [rbp-16]=size
    let copy = out.create_text_label();
    let epilogue = out.create_text_label();
    let retry = out.create_text_label();
    let fits = out.create_text_label();
    out.load_ptr_disp32(Reg::V0, Reg::V7, -16);
    // Already forwarded? rax & FWD.
    out.mov_reg_reg(Reg::V1, Reg::V0);
    out.and_reg_imm32(Reg::V1, GC_FWD as i32);
    out.test_reg_reg(Reg::V1, Reg::V1);
    out.jcc_label(Condition::Equal, copy);
    out.and_reg_imm32(Reg::V0, -16); // to-space user ptr
    out.jmp_label(epilogue);
    out.bind_text_label(copy);
    out.store_rbp_slot(8, Reg::V7); // oldP
    out.and_reg_imm32(Reg::V0, -16); // size S
    out.store_rbp_slot(16, Reg::V0);
    // An object that does not fit one to-space region can never be
    // evacuated (the refill loop would spin forever). Selection guarantees
    // this never happens; treat it as an invariant violation and abort.
    let evac_size_ok = out.create_text_label();
    let evac_oversized = out.create_text_label();
    out.cmp_reg_imm32(Reg::V0, GC_REGION_SIZE as i32);
    out.jcc_label(Condition::Above, evac_oversized);
    out.jmp_label(evac_size_ok);
    out.bind_text_label(evac_oversized);
    out.plat_write_data(stderr_fd, oversized_msg, oversized_len);
    out.plat_exit(1);
    out.bind_text_label(evac_size_ok);
    let need_refill = out.create_text_label();
    out.bind_text_label(retry);
    out.mov_data_addr(Reg::V10, c.evac_top);
    out.load_ptr_disp32(Reg::V8, Reg::V10, 0); // dst = evac_top
    out.load_rbp_slot(Reg::V2, 16); // S
    out.mov_reg_reg(Reg::V9, Reg::V8);
    out.add_reg_reg(Reg::V9, Reg::V2); // new_top = dst + S
    out.mov_data_addr(Reg::V10, c.evac_end);
    out.load_ptr_disp32(Reg::V11, Reg::V10, 0);
    out.cmp_reg_reg(Reg::V9, Reg::V11);
    out.jcc_label(Condition::Above, need_refill); // new_top > end
    out.jmp_label(fits);
    // Refill to-space (never fails: headroom invariant).
    out.bind_text_label(need_refill);
    out.load_rbp_slot(Reg::V7, 16); // size
    out.call_label(acquire_evac_region);
    out.jmp_label(retry);
    out.bind_text_label(fits);
    out.mov_data_addr(Reg::V10, c.evac_top);
    out.store_ptr_disp32(Reg::V10, 0, Reg::V9); // bump
    // memcpy S bytes: rsi = oldP-16, rdi = dst (r8), rcx = S.
    out.load_rbp_slot(Reg::V6, 8);
    out.sub_reg_imm8(Reg::V6, 16);
    out.mov_reg_reg(Reg::V7, Reg::V8);
    out.mov_reg_reg(Reg::V1, Reg::V2);
    out.rep_movsb(); // clobbers rsi/rdi/rcx; r8 survives
    // n_user = dst + 16.
    out.mov_reg_reg(Reg::V0, Reg::V8);
    out.add_reg_imm32(Reg::V0, 16);
    // Install forwarding in the OLD header: [oldP-16] = n_user | FWD.
    out.load_rbp_slot(Reg::V11, 8); // oldP
    out.mov_reg_reg(Reg::V1, Reg::V0);
    out.mov_imm64(Reg::V2, GC_FWD);
    out.or_reg_reg(Reg::V1, Reg::V2);
    out.store_ptr_disp32(Reg::V11, -16, Reg::V1);
    // [oldP-8] = S (word1, size for the relocate walk).
    out.load_rbp_slot(Reg::V1, 16);
    out.store_ptr_disp32(Reg::V11, -8, Reg::V1);
    // Count this evacuation (--gc-log). rax = n_user must survive.
    out.mov_data_addr(Reg::V10, c.relocated_count);
    out.load_ptr_disp32(Reg::V11, Reg::V10, 0);
    out.add_reg_imm32(Reg::V11, 1);
    out.store_ptr_disp32(Reg::V10, 0, Reg::V11);
    out.bind_text_label(epilogue);
    out.leave();
    out.ret();
}

/// Data cells the evacuation-region acquire path reads and writes: the
/// dedicated to-space bump (`evac_*`), the region arrays, and the free
/// pool. Distinct from the mutator's `gc_heap_*` bump.
#[derive(Clone, Copy)]
pub struct EvacRegionCells<D: Copy> {
    pub evac_region_base: D,
    pub evac_top: D,
    pub evac_end: D,
    pub region_base: D,
    pub region_top: D,
    pub committed_count: D,
    pub free_region_head: D,
}

/// Portable version of `gc_acquire_evac_region`: obtain a fresh to-space
/// region into the dedicated evacuation bump (NOT the mutator's bump).
/// Flushes the previous evac region's watermark, then pops the free pool
/// (zeroing the reused region) or commits the next tail region -- aborting
/// via the exhaustion diagnostic if that would pass the reservation (a
/// safety net; the headroom cap makes it unreachable). Installs the evac
/// bump globals and sets the new region's watermark to base. Leaf (no
/// frame). `exhausted_msg` / `exhausted_len` / `stderr_fd` describe the
/// abort diagnostic; the message label is built by the caller.
pub fn emit_gc_acquire_evac_region<E: PortableAsm>(
    out: &mut E,
    entry: E::TextLabel,
    c: EvacRegionCells<E::DataLabel>,
    exhausted_msg: E::DataLabel,
    exhausted_len: usize,
    stderr_fd: u64,
) {
    use crate::gc_layout::{GC_REGION_SHIFT, GC_REGION_SIZE, GC_RESERVE_REGIONS};

    out.bind_text_label(entry);
    let no_flush = out.create_text_label();
    let from_tail = out.create_text_label();
    let install = out.create_text_label();
    // Flush the previous evac region's watermark, if any.
    out.mov_data_addr(Reg::V10, c.evac_region_base);
    out.load_ptr_disp32(Reg::V0, Reg::V10, 0);
    out.test_reg_reg(Reg::V0, Reg::V0);
    out.jcc_label(Condition::Equal, no_flush);
    out.mov_data_addr(Reg::V10, c.region_base);
    out.load_ptr_disp32(Reg::V11, Reg::V10, 0);
    out.sub_reg_reg(Reg::V0, Reg::V11);
    out.shr_reg_imm8(Reg::V0, GC_REGION_SHIFT);
    out.shl_reg_imm8(Reg::V0, 3);
    out.mov_data_addr(Reg::V10, c.region_top);
    out.add_reg_reg(Reg::V10, Reg::V0);
    out.mov_data_addr(Reg::V8, c.evac_top);
    out.load_ptr_disp32(Reg::V2, Reg::V8, 0);
    out.store_ptr_disp32(Reg::V10, 0, Reg::V2);
    out.bind_text_label(no_flush);
    // (1) Free-region pool.
    out.mov_data_addr(Reg::V10, c.free_region_head);
    out.load_ptr_disp32(Reg::V0, Reg::V10, 0);
    out.test_reg_reg(Reg::V0, Reg::V0);
    out.jcc_label(Condition::Equal, from_tail);
    out.load_ptr_disp32(Reg::V1, Reg::V0, 0);
    out.store_ptr_disp32(Reg::V10, 0, Reg::V1); // pop
    // Zero the reused region.
    let zero_loop = out.create_text_label();
    let zero_done = out.create_text_label();
    out.mov_reg_reg(Reg::V8, Reg::V0);
    out.mov_reg_reg(Reg::V11, Reg::V0);
    out.add_reg_imm32(Reg::V11, GC_REGION_SIZE as i32);
    out.mov_imm64(Reg::V9, 0);
    out.bind_text_label(zero_loop);
    out.cmp_reg_reg(Reg::V8, Reg::V11);
    out.jcc_label(Condition::AboveOrEqual, zero_done);
    out.store_ptr_disp32(Reg::V8, 0, Reg::V9);
    out.add_reg_imm32(Reg::V8, 8);
    out.jmp_label(zero_loop);
    out.bind_text_label(zero_done);
    out.jmp_label(install);
    // (2) Commit the next tail region. Hard safety net: never commit past
    // the reservation. The headroom cap in gc_relocate_start makes this
    // unreachable, but an out-of-bounds to-space write would be silent
    // corruption, so abort cleanly if the accounting is ever wrong.
    out.bind_text_label(from_tail);
    out.mov_data_addr(Reg::V10, c.committed_count);
    out.load_ptr_disp32(Reg::V0, Reg::V10, 0);
    let evac_ok = out.create_text_label();
    out.cmp_reg_imm32(Reg::V0, GC_RESERVE_REGIONS as i32);
    out.jcc_label(Condition::Below, evac_ok);
    out.plat_write_data(stderr_fd, exhausted_msg, exhausted_len);
    out.plat_exit(1);
    out.bind_text_label(evac_ok);
    out.mov_reg_reg(Reg::V1, Reg::V0);
    out.shl_reg_imm8(Reg::V1, GC_REGION_SHIFT);
    out.mov_data_addr(Reg::V8, c.region_base);
    out.load_ptr_disp32(Reg::V8, Reg::V8, 0);
    out.add_reg_reg(Reg::V8, Reg::V1);
    out.add_reg_imm32(Reg::V0, 1);
    out.store_ptr_disp32(Reg::V10, 0, Reg::V0);
    out.mov_reg_reg(Reg::V0, Reg::V8); // rax = new base
    // install: set the evac bump globals (NOT the mutator's).
    out.bind_text_label(install);
    out.mov_data_addr(Reg::V10, c.evac_region_base);
    out.store_ptr_disp32(Reg::V10, 0, Reg::V0);
    out.mov_data_addr(Reg::V10, c.evac_top);
    out.store_ptr_disp32(Reg::V10, 0, Reg::V0);
    out.mov_reg_reg(Reg::V1, Reg::V0);
    out.add_reg_imm32(Reg::V1, GC_REGION_SIZE as i32);
    out.mov_data_addr(Reg::V10, c.evac_end);
    out.store_ptr_disp32(Reg::V10, 0, Reg::V1);
    // region_top[newidx] = base (empty until the watermark is flushed).
    out.mov_reg_reg(Reg::V1, Reg::V0);
    out.mov_data_addr(Reg::V10, c.region_base);
    out.load_ptr_disp32(Reg::V10, Reg::V10, 0);
    out.sub_reg_reg(Reg::V1, Reg::V10);
    out.shr_reg_imm8(Reg::V1, GC_REGION_SHIFT);
    out.shl_reg_imm8(Reg::V1, 3);
    out.mov_data_addr(Reg::V10, c.region_top);
    out.add_reg_reg(Reg::V10, Reg::V1);
    out.store_ptr_disp32(Reg::V10, 0, Reg::V0);
    out.ret();
}

/// Portable version of `gc_relocate_fix_roots`: at RelocateStart, walk
/// every shadow-stack root slot and, for each root pointing into a
/// from-space region, redirect it to the object's to-space copy --
/// following an existing forwarding word or evacuating on demand. Uses
/// two frame slots (`[rbp-8]` = cursor, `[rbp-16]` = end). Needs no new
/// trait methods.
pub fn emit_gc_relocate_fix_roots<E: PortableAsm>(
    out: &mut E,
    entry: E::TextLabel,
    shadow_stack: E::DataLabel,
    shadow_stack_top: E::DataLabel,
    region_base: E::DataLabel,
    region_fromspace: E::DataLabel,
    evacuate: E::TextLabel,
) {
    use crate::gc_layout::{GC_FWD, GC_REGION_SHIFT};

    out.bind_text_label(entry);
    out.push_reg(Reg::V5); // rbp
    out.mov_reg_reg(Reg::V5, Reg::V4); // rbp = rsp
    out.sub_reg_imm8(Reg::V4, 16); // [rbp-8]=cursor, [rbp-16]=end
    out.mov_data_addr(Reg::V10, shadow_stack);
    out.load_ptr_disp32(Reg::V10, Reg::V10, 0);
    out.store_rbp_slot(8, Reg::V10);
    out.mov_data_addr(Reg::V8, shadow_stack_top);
    out.load_ptr_disp32(Reg::V1, Reg::V8, 0);
    out.shl_reg_imm8(Reg::V1, 3);
    out.add_reg_reg(Reg::V10, Reg::V1);
    out.store_rbp_slot(16, Reg::V10);
    let loop_l = out.create_text_label();
    let done_l = out.create_text_label();
    let next_l = out.create_text_label();
    let do_evac = out.create_text_label();
    let store_root = out.create_text_label();
    out.bind_text_label(loop_l);
    out.load_rbp_slot(Reg::V10, 8);
    out.load_rbp_slot(Reg::V11, 16);
    out.cmp_reg_reg(Reg::V10, Reg::V11);
    out.jcc_label(Condition::AboveOrEqual, done_l);
    // slotaddr = [cursor]; val = [slotaddr].
    out.load_ptr_disp32(Reg::V0, Reg::V10, 0);
    out.load_ptr_disp32(Reg::V7, Reg::V0, 0);
    out.test_reg_reg(Reg::V7, Reg::V7);
    out.jcc_label(Condition::Equal, next_l);
    // idx = (val - region_base) >> SHIFT ; from-space?
    out.mov_reg_reg(Reg::V1, Reg::V7);
    out.mov_data_addr(Reg::V10, region_base);
    out.load_ptr_disp32(Reg::V10, Reg::V10, 0);
    out.sub_reg_reg(Reg::V1, Reg::V10);
    out.shr_reg_imm8(Reg::V1, GC_REGION_SHIFT);
    out.shl_reg_imm8(Reg::V1, 3);
    out.mov_data_addr(Reg::V10, region_fromspace);
    out.add_reg_reg(Reg::V10, Reg::V1);
    out.load_ptr_disp32(Reg::V11, Reg::V10, 0);
    out.test_reg_reg(Reg::V11, Reg::V11);
    out.jcc_label(Condition::Equal, next_l);
    // val in from-space: new = (word0 & FWD) ? word0 & -16 : evacuate(val)
    out.load_ptr_disp32(Reg::V11, Reg::V7, -16);
    out.mov_reg_reg(Reg::V1, Reg::V11);
    out.and_reg_imm32(Reg::V1, GC_FWD as i32);
    out.test_reg_reg(Reg::V1, Reg::V1);
    out.jcc_label(Condition::Equal, do_evac);
    out.and_reg_imm32(Reg::V11, -16);
    out.mov_reg_reg(Reg::V0, Reg::V11); // new
    out.jmp_label(store_root);
    out.bind_text_label(do_evac);
    out.call_label(evacuate); // rdi = val -> rax = to-space
    out.bind_text_label(store_root);
    // [slotaddr] = new. slotaddr was clobbered by the call; reload it.
    out.load_rbp_slot(Reg::V10, 8);
    out.load_ptr_disp32(Reg::V10, Reg::V10, 0); // slotaddr
    out.store_ptr_disp32(Reg::V10, 0, Reg::V0);
    out.bind_text_label(next_l);
    out.load_rbp_slot(Reg::V10, 8);
    out.add_reg_imm32(Reg::V10, 8);
    out.store_rbp_slot(8, Reg::V10);
    out.jmp_label(loop_l);
    out.bind_text_label(done_l);
    out.leave();
    out.ret();
}

/// Data cells the MarkStart color/phase setup writes and reloads.
#[derive(Clone, Copy)]
pub struct MarkStartCells<D: Copy> {
    pub header_mark: D,
    pub good_color: D,
    pub bad_mask: D,
    pub mark_color: D,
    pub worklist_top: D,
    pub phase: D,
}

/// Host-config flags that select which MarkStart color scheme is emitted.
/// Both are compile-time constants for a given build, so they pick a code
/// path exactly as the original inline `if`s did.
#[derive(Clone, Copy)]
pub struct MarkColorMode {
    /// M6 scheme (good flips M0<->M1 directly) instead of the R scheme.
    pub evac_off: bool,
    /// `--gc-poison`: force `bad = COLOR_MASK` so every non-good pointer
    /// slow-paths (the barrier-coverage canary).
    pub poison: bool,
}

/// Portable version of `gc_mark_start` (short STW pause): flip the header
/// mark parity and the collector's color config to the new cycle, reload
/// the reserved color-register caches (GoodColor / BadMask), reset the
/// worklist, scan roots, and enter the Mark phase. O(roots).
///
/// `evac_off` selects the M6 scheme (good flips M0<->M1 directly) vs the
/// R scheme (a persistent mark color toggles and good/bad derive from
/// it). `poison` forces `bad = COLOR_MASK` so every non-good pointer
/// slow-paths (the `--gc-poison` barrier-coverage canary). Both are host
/// config, passed in as flags so the emitted code is chosen the same way
/// the original inline `if` did.
pub fn emit_gc_mark_start<E: PortableAsm>(
    out: &mut E,
    entry: E::TextLabel,
    timing: bool,
    pause: PauseCells<E::DataLabel>,
    cells: MarkStartCells<E::DataLabel>,
    mark_roots: E::TextLabel,
    mode: MarkColorMode,
) {
    use crate::gc_layout::{
        GC_COLOR_M0, GC_COLOR_M1, GC_COLOR_MASK, GC_COLOR_R, GC_HMARK_BOTH, GC_PHASE_MARK,
    };
    let MarkColorMode { evac_off, poison } = mode;

    out.bind_text_label(entry);
    out.push_reg(Reg::V5); // rbp
    out.mov_reg_reg(Reg::V5, Reg::V4); // rbp = rsp
    emit_gc_pause_start(out, timing, pause); // MarkStart is a short STW pause (O(roots)).
    // M7: toggle the header mark parity (bits 1-2) so this cycle marks
    // with the OTHER bit than the last cycle -- a live object surviving
    // multiple cycles is thus re-marked each cycle, and the sweep clears
    // the now-stale parity. Must precede gc_mark_roots (which sets the
    // current mark bit on roots).
    out.mov_data_addr(Reg::V10, cells.header_mark);
    out.load_ptr_disp32(Reg::V0, Reg::V10, 0);
    out.mov_imm64(Reg::V11, GC_HMARK_BOTH);
    out.xor_reg_reg(Reg::V0, Reg::V11);
    out.store_ptr_disp32(Reg::V10, 0, Reg::V0);
    // Enter the Mark phase's color config. The mark color toggles M0<->M1
    // each cycle. Good becomes the new mark color; bad catches the other
    // two colors so any pointer NOT at the new mark color (an R-colored
    // Idle pointer, or a stale old-mark-color pointer) slow-paths and is
    // remapped + marked (incremental update).
    if evac_off {
        // M6 scheme: good flips M0<->M1 directly via gc_good_color.
        out.mov_data_addr(Reg::V10, cells.good_color);
        out.load_ptr_disp32(Reg::V0, Reg::V10, 0); // old good
        if poison {
            out.mov_imm64(Reg::V1, GC_COLOR_MASK);
        } else {
            out.mov_reg_reg(Reg::V1, Reg::V0);
            out.mov_imm64(Reg::V11, GC_COLOR_R);
            out.or_reg_reg(Reg::V1, Reg::V11); // old_good | R
        }
        out.mov_data_addr(Reg::V11, cells.bad_mask);
        out.store_ptr_disp32(Reg::V11, 0, Reg::V1);
        out.mov_imm64(Reg::V1, GC_COLOR_M0 | GC_COLOR_M1);
        out.xor_reg_reg(Reg::V0, Reg::V1);
        out.store_ptr_disp32(Reg::V10, 0, Reg::V0);
    } else {
        // R scheme: toggle the persistent mark color, then good = mark
        // color, bad = (M0|M1|R) ^ mark color.
        out.mov_data_addr(Reg::V10, cells.mark_color);
        out.load_ptr_disp32(Reg::V0, Reg::V10, 0);
        out.mov_imm64(Reg::V1, GC_COLOR_M0 | GC_COLOR_M1);
        out.xor_reg_reg(Reg::V0, Reg::V1); // new mark color
        out.store_ptr_disp32(Reg::V10, 0, Reg::V0);
        out.mov_data_addr(Reg::V10, cells.good_color);
        out.store_ptr_disp32(Reg::V10, 0, Reg::V0); // good = mark color
        if poison {
            out.mov_imm64(Reg::V1, GC_COLOR_MASK);
        } else {
            out.mov_imm64(Reg::V1, GC_COLOR_M0 | GC_COLOR_M1 | GC_COLOR_R);
            out.xor_reg_reg(Reg::V1, Reg::V0); // (M0|M1|R) ^ mark
        }
        out.mov_data_addr(Reg::V11, cells.bad_mask);
        out.store_ptr_disp32(Reg::V11, 0, Reg::V1);
    }
    // Reload the caches from the cells.
    out.mov_data_addr(Reg::GoodColor, cells.good_color);
    out.load_ptr_disp32(Reg::GoodColor, Reg::GoodColor, 0);
    out.mov_data_addr(Reg::BadMask, cells.bad_mask);
    out.load_ptr_disp32(Reg::BadMask, Reg::BadMask, 0);
    // Reset the worklist and scan roots into it (marked new-color).
    out.mov_data_addr(Reg::V10, cells.worklist_top);
    out.mov_imm64(Reg::V0, 0);
    out.store_ptr_disp32(Reg::V10, 0, Reg::V0);
    out.call_label(mark_roots);
    // Enter the Mark phase.
    out.mov_data_addr(Reg::V10, cells.phase);
    out.mov_imm64(Reg::V0, GC_PHASE_MARK);
    out.store_ptr_disp32(Reg::V10, 0, Reg::V0);
    emit_gc_pause_end(out, timing, pause);
    out.leave();
    out.ret();
}
