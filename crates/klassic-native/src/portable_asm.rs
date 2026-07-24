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
// Both enums below declare their full architectural budget -- every
// x86-64 register/comparison outcome this backend has ever used, not
// just what the currently-migrated functions happen to exercise
// (`emit_print_i64`/`emit_write_data`/`emit_exit_code` don't touch
// r9/r11 or most comparison outcomes). That's a deliberate design
// choice (see the design spec), not an oversight, so the handful of
// variants unused by this first migration slice are allowed rather
// than trimmed away and re-added piecemeal as each future migration
// happens to need one.
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
}

/// Semantic comparison outcomes for conditional jumps -- these describe
/// *what was compared*, not any architecture's condition-code
/// mnemonics, so they carry over unchanged from the x86-64 backend's
/// existing `Condition` enum.
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
