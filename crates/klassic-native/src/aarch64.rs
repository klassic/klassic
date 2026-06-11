//! Direct AArch64 backend for `aarch64-apple-darwin` (Apple Silicon
//! macOS). Like the x86_64 ELF backend it emits machine code and the
//! executable container directly — no `cc` / `as` / `ld` / `codesign`
//! — and like that backend's early history it starts from a small
//! vertical slice and grows. The current subset covers top-level
//! `Int` / `Bool` expressions (arithmetic, comparisons, short-circuit
//! logic), `val` / `mutable` locals with assignment, `if` / `while`,
//! and `println` of runtime `Int` / `Bool` values plus string and
//! double literals. Everything else fails with a source-located
//! diagnostic, never wrong code. Generated code is
//! position-independent (`adrp`+`add` for data, relative branches)
//! because the kernel slides `MH_PIE` images.
//!
//! Darwin arm64 syscall convention: number in `x16`, arguments in
//! `x0..`, trap via `svc #0x80`. BSD numbers: `exit` = 1, `write` = 4.

use std::collections::HashMap;

use klassic_span::{Diagnostic, Span};
use klassic_syntax::{BinaryOp, Expr};

use crate::macho::{self, DataFixup};

const SYS_EXIT: u16 = 1;
const SYS_WRITE: u16 = 4;
/// Darwin `mmap`. Heap segments come straight from the kernel.
const SYS_MMAP: u16 = 197;
const STDOUT_FD: u64 = 1;
const STDERR_FD: u64 = 2;
/// One bump-allocator segment. Exhaustion mmaps a fresh segment (the
/// old one leaks until the backend grows a collector — same place the
/// x86_64 backend started).
const HEAP_SEGMENT_BYTES: u64 = 64 * 1024 * 1024;
/// Darwin MAP_ANON | MAP_PRIVATE.
const MMAP_ANON_PRIVATE: u64 = 0x1002;
const PROT_READ_WRITE: u64 = 3;

/// AArch64 general-purpose registers used by the subset. The numeric
/// value is the register number in instruction encodings; 31 encodes
/// `xzr` or `sp` depending on position and is handled inside the
/// emission helpers rather than exposed here.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Reg {
    X0 = 0,
    X1 = 1,
    X2 = 2,
    X3 = 3,
    X4 = 4,
    X5 = 5,
    X6 = 6,
    X7 = 7,
    /// Darwin syscall number register.
    X16 = 16,
    /// Callee-saved: bump-allocator next pointer. The generated code
    /// never spills it, which is exactly why a heap exists without a
    /// writable data segment.
    X19 = 19,
    /// Callee-saved: bump-allocator end pointer.
    X20 = 20,
}

/// AAPCS64 integer argument registers, in order.
const ARG_REGS: [Reg; 8] = [
    Reg::X0,
    Reg::X1,
    Reg::X2,
    Reg::X3,
    Reg::X4,
    Reg::X5,
    Reg::X6,
    Reg::X7,
];

/// Frame pointer register number: locals live at `[x29, #offset]` so
/// the operand stack can move `sp` freely. Only ever a base register,
/// so it stays out of the `Reg` enum.
const FP: u32 = 29;

/// AArch64 condition codes (B.cond / CSINC encodings).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Cond {
    Eq = 0,
    Ne = 1,
    /// After `fcmp`: strictly less, unordered fails — the float `<`.
    Mi = 4,
    /// After `fcmp`: less-or-equal, unordered fails — the float `<=`.
    Ls = 9,
    Ge = 10,
    Lt = 11,
    Gt = 12,
    Le = 13,
}

impl Cond {
    /// The AArch64 condition encoding flips the polarity in the low
    /// bit, which `cset` (alias of `csinc` with the inverted
    /// condition) relies on.
    fn inverted_bits(self) -> u32 {
        (self as u32) ^ 1
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct Label(usize);

enum BranchKind {
    /// `b label` — 26-bit signed word offset.
    Unconditional,
    /// `bl label` — 26-bit signed word offset, link register.
    Link,
    /// `b.cond label` — 19-bit signed word offset.
    Conditional(Cond),
    /// `cbz xreg, label`.
    CompareZero(Reg),
    /// `cbnz xreg, label`.
    CompareNonZero(Reg),
}

struct BranchFixup {
    code_offset: usize,
    label: Label,
    kind: BranchKind,
}

#[derive(Default)]
struct Assembler {
    code: Vec<u8>,
    rodata: Vec<u8>,
    fixups: Vec<DataFixup>,
    labels: Vec<Option<usize>>,
    branches: Vec<BranchFixup>,
}

impl Assembler {
    fn word(&mut self, instruction: u32) {
        self.code.extend_from_slice(&instruction.to_le_bytes());
    }

    fn new_label(&mut self) -> Label {
        self.labels.push(None);
        Label(self.labels.len() - 1)
    }

    fn bind(&mut self, label: Label) {
        debug_assert!(self.labels[label.0].is_none(), "label bound twice");
        self.labels[label.0] = Some(self.code.len());
    }

    fn branch(&mut self, label: Label, kind: BranchKind) {
        self.branches.push(BranchFixup {
            code_offset: self.code.len(),
            label,
            kind,
        });
        // Placeholder opcodes; offsets are patched in `finish`.
        match self.branches.last().expect("just pushed").kind {
            BranchKind::Unconditional => self.word(0x1400_0000),
            BranchKind::Link => self.word(0x9400_0000),
            BranchKind::Conditional(cond) => self.word(0x5400_0000 | cond as u32),
            BranchKind::CompareZero(reg) => self.word(0xb400_0000 | reg as u32),
            BranchKind::CompareNonZero(reg) => self.word(0xb500_0000 | reg as u32),
        }
    }

    /// Resolve every recorded branch against its bound label.
    fn finish(&mut self) {
        for fixup in &self.branches {
            let target = self.labels[fixup.label.0].expect("branch target label never bound");
            let delta_words = (target as i64 - fixup.code_offset as i64) / 4;
            let mut word_bytes = [0u8; 4];
            word_bytes.copy_from_slice(&self.code[fixup.code_offset..fixup.code_offset + 4]);
            let word = u32::from_le_bytes(word_bytes);
            let patched = match fixup.kind {
                BranchKind::Unconditional | BranchKind::Link => {
                    word | ((delta_words as u32) & 0x03ff_ffff)
                }
                BranchKind::Conditional(_)
                | BranchKind::CompareZero(_)
                | BranchKind::CompareNonZero(_) => word | (((delta_words as u32) & 0x7_ffff) << 5),
            };
            self.code[fixup.code_offset..fixup.code_offset + 4]
                .copy_from_slice(&patched.to_le_bytes());
        }
        self.branches.clear();
    }

    /// `movz xd, #imm16, lsl #(16 * shift)`
    fn movz(&mut self, reg: Reg, imm16: u16, shift: u32) {
        self.word(0xd280_0000 | (shift << 21) | (u32::from(imm16) << 5) | reg as u32);
    }

    /// `movk xd, #imm16, lsl #(16 * shift)`
    fn movk(&mut self, reg: Reg, imm16: u16, shift: u32) {
        self.word(0xf280_0000 | (shift << 21) | (u32::from(imm16) << 5) | reg as u32);
    }

    /// Materialize an arbitrary 64-bit constant. Negative values get
    /// the movn-free simple form: movz/movk all four halfwords.
    fn mov_imm64(&mut self, reg: Reg, value: u64) {
        self.movz(reg, value as u16, 0);
        for shift in 1..4u32 {
            let part = (value >> (16 * shift)) as u16;
            if part != 0 {
                self.movk(reg, part, shift);
            }
        }
    }

    /// `mov xd, xm` (orr xd, xzr, xm)
    fn mov_reg(&mut self, dst: Reg, src: Reg) {
        self.word(0xaa00_03e0 | ((src as u32) << 16) | dst as u32);
    }

    fn add_reg(&mut self, dst: Reg, lhs: Reg, rhs: Reg) {
        self.word(0x8b00_0000 | ((rhs as u32) << 16) | ((lhs as u32) << 5) | dst as u32);
    }

    fn sub_reg(&mut self, dst: Reg, lhs: Reg, rhs: Reg) {
        self.word(0xcb00_0000 | ((rhs as u32) << 16) | ((lhs as u32) << 5) | dst as u32);
    }

    /// `mul xd, xn, xm` (madd with xzr accumulator)
    fn mul_reg(&mut self, dst: Reg, lhs: Reg, rhs: Reg) {
        self.word(0x9b00_7c00 | ((rhs as u32) << 16) | ((lhs as u32) << 5) | dst as u32);
    }

    fn sdiv_reg(&mut self, dst: Reg, lhs: Reg, rhs: Reg) {
        self.word(0x9ac0_0c00 | ((rhs as u32) << 16) | ((lhs as u32) << 5) | dst as u32);
    }

    /// `msub xd, xn, xm, xa` — xd = xa - xn * xm (remainder helper).
    fn msub_reg(&mut self, dst: Reg, lhs: Reg, rhs: Reg, acc: Reg) {
        self.word(
            0x9b00_8000
                | ((rhs as u32) << 16)
                | ((acc as u32) << 10)
                | ((lhs as u32) << 5)
                | dst as u32,
        );
    }

    /// `cmp xn, xm` (subs xzr, xn, xm)
    fn cmp_reg(&mut self, lhs: Reg, rhs: Reg) {
        self.word(0xeb00_001f | ((rhs as u32) << 16) | ((lhs as u32) << 5));
    }

    /// `cset xd, cond`
    fn cset(&mut self, dst: Reg, cond: Cond) {
        self.word(0x9a9f_07e0 | (cond.inverted_bits() << 12) | dst as u32);
    }

    /// `sub sp, sp, #imm12`
    fn sub_sp_imm(&mut self, imm: u32) {
        debug_assert!(imm < 4096);
        self.word(0xd100_03ff | (imm << 10));
    }

    /// `add sp, sp, #imm12`
    fn add_sp_imm(&mut self, imm: u32) {
        debug_assert!(imm < 4096);
        self.word(0x9100_03ff | (imm << 10));
    }

    /// `add xd, sp, #imm12`
    fn add_reg_sp_imm(&mut self, dst: Reg, imm: u32) {
        debug_assert!(imm < 4096);
        self.word(0x9100_0000 | (imm << 10) | (31 << 5) | dst as u32);
    }

    /// `add xd, xn, #imm12`
    fn add_reg_imm(&mut self, dst: Reg, src: Reg, imm: u32) {
        debug_assert!(imm < 4096);
        self.word(0x9100_0000 | (imm << 10) | ((src as u32) << 5) | dst as u32);
    }

    /// `mov x29, sp`
    fn mov_fp_sp(&mut self) {
        self.word(0x9100_03fd);
    }

    /// `str xt, [sp, #-16]!` — 16-byte stride keeps sp aligned, which
    /// Darwin arm64 enforces in hardware on sp-based accesses.
    fn push(&mut self, reg: Reg) {
        self.word(0xf81f_0fe0 | reg as u32);
    }

    /// `ldr xt, [sp], #16`
    fn pop(&mut self, reg: Reg) {
        self.word(0xf841_07e0 | reg as u32);
    }

    /// `str xt, [x29, #offset]` (unsigned, 8-byte scaled)
    fn store_local(&mut self, reg: Reg, offset: u32) {
        debug_assert!(offset.is_multiple_of(8) && offset / 8 < 4096);
        self.word(0xf900_0000 | ((offset / 8) << 10) | (FP << 5) | reg as u32);
    }

    /// `ldr xt, [x29, #offset]`
    fn load_local(&mut self, reg: Reg, offset: u32) {
        debug_assert!(offset.is_multiple_of(8) && offset / 8 < 4096);
        self.word(0xf940_0000 | ((offset / 8) << 10) | (FP << 5) | reg as u32);
    }

    /// `strb wt, [xn, #-1]!`
    fn store_byte_pre_decrement(&mut self, reg: Reg, base: Reg) {
        self.word(0x3800_0c00 | (0x1ff << 12) | ((base as u32) << 5) | reg as u32);
    }

    /// `adrp xd, <page>` + `add xd, xd, #<pageoff>` addressing a byte
    /// in rodata; both immediates are zero placeholders patched by the
    /// Mach-O writer once the image layout is final.
    fn load_rodata_address(&mut self, reg: Reg, data_offset: usize) {
        let adrp_offset = self.code.len();
        self.word(0x9000_0000 | reg as u32);
        let add_offset = self.code.len();
        let rn = reg as u32;
        self.word(0x9100_0000 | (rn << 5) | rn);
        self.fixups.push(DataFixup {
            adrp_offset,
            add_offset,
            data_offset,
        });
    }

    /// `svc #0x80` — Darwin syscall trap.
    fn svc_0x80(&mut self) {
        self.word(0xd400_1001);
    }

    fn intern_rodata(&mut self, bytes: &[u8]) -> usize {
        let offset = self.rodata.len();
        self.rodata.extend_from_slice(bytes);
        offset
    }

    /// Intern a string literal as a `[len: u64][bytes]` object —
    /// byte-identical to what the bump allocator produces, so every
    /// string value is one pointer regardless of where it lives.
    fn intern_string_object(&mut self, text: &str) -> usize {
        while !self.rodata.len().is_multiple_of(8) {
            self.rodata.push(0);
        }
        let offset = self.rodata.len();
        self.rodata
            .extend_from_slice(&(text.len() as u64).to_le_bytes());
        self.rodata.extend_from_slice(text.as_bytes());
        offset
    }

    /// write(fd, <rodata bytes>, len) — clobbers x0/x1/x2/x16.
    fn emit_write_rodata(&mut self, fd: u64, bytes: &[u8]) {
        let data_offset = self.intern_rodata(bytes);
        self.mov_imm64(Reg::X0, fd);
        self.load_rodata_address(Reg::X1, data_offset);
        self.mov_imm64(Reg::X2, bytes.len() as u64);
        self.mov_imm64(Reg::X16, u64::from(SYS_WRITE));
        self.svc_0x80();
    }

    fn emit_exit(&mut self, status: u64) {
        self.mov_imm64(Reg::X0, status);
        self.mov_imm64(Reg::X16, u64::from(SYS_EXIT));
        self.svc_0x80();
    }

    /// Decompose the signed integer in x0 into decimal digits at the
    /// tail of a 32-byte stack buffer the caller has reserved
    /// (`sub sp, #32`). Leaves x1 = first byte, sp+32 = one past the
    /// end. Clobbers x0/x2-x5.
    fn emit_int_digits(&mut self, include_newline: bool) {
        self.add_reg_sp_imm(Reg::X1, 32); // one past the buffer end
        if include_newline {
            self.mov_imm64(Reg::X3, b'\n' as u64);
            self.store_byte_pre_decrement(Reg::X3, Reg::X1);
        }
        // x5 = 1 when negative, then continue with |value|.
        self.cmp_x0_zero();
        self.cset(Reg::X5, Cond::Lt);
        let non_negative = self.new_label();
        self.branch(non_negative, BranchKind::Conditional(Cond::Ge));
        self.neg_x0();
        self.bind(non_negative);
        // Digit loop: do { x4 = x0 / 10; digit = x0 - x4*10; } while x0.
        let digit_loop = self.new_label();
        self.bind(digit_loop);
        self.mov_imm64(Reg::X3, 10);
        self.sdiv_reg(Reg::X4, Reg::X0, Reg::X3);
        self.msub_reg(Reg::X2, Reg::X4, Reg::X3, Reg::X0);
        self.add_reg_imm(Reg::X2, Reg::X2, u32::from(b'0'));
        self.store_byte_pre_decrement(Reg::X2, Reg::X1);
        self.mov_reg(Reg::X0, Reg::X4);
        self.branch(digit_loop, BranchKind::CompareNonZero(Reg::X0));
        let no_sign = self.new_label();
        self.branch(no_sign, BranchKind::CompareZero(Reg::X5));
        self.mov_imm64(Reg::X3, b'-' as u64);
        self.store_byte_pre_decrement(Reg::X3, Reg::X1);
        self.bind(no_sign);
    }

    /// Print the signed integer in x0 followed by a newline with one
    /// write syscall. Clobbers x0-x5/x16.
    fn emit_print_int_line(&mut self) {
        self.sub_sp_imm(32);
        self.emit_int_digits(true);
        // write(1, x1, (sp+32) - x1)
        self.add_reg_sp_imm(Reg::X2, 32);
        self.sub_reg(Reg::X2, Reg::X2, Reg::X1);
        self.mov_imm64(Reg::X0, STDOUT_FD);
        self.mov_imm64(Reg::X16, u64::from(SYS_WRITE));
        self.svc_0x80();
        self.add_sp_imm(32);
    }

    /// `cmp x0, #0`
    fn cmp_x0_zero(&mut self) {
        self.word(0xf100_001f);
    }

    /// `neg x0, x0` (sub x0, xzr, x0)
    fn neg_x0(&mut self) {
        self.word(0xcb00_03e0);
    }

    // --- scalar double FP (the value lives in a GP register as raw
    // bits; these move it through d0/d1 for the actual operation) ---

    /// `fmov dd, xn` — reinterpret the GP bits as a double in dN.
    fn fmov_d_from_x(&mut self, d: u32, x: Reg) {
        self.word(0x9e67_0000 | ((x as u32) << 5) | d);
    }

    /// `fmov xd, dn` — reinterpret the double bits back into a GP reg.
    fn fmov_x_from_d(&mut self, x: Reg, d: u32) {
        self.word(0x9e66_0000 | (d << 5) | x as u32);
    }

    fn fadd_d(&mut self, dd: u32, dn: u32, dm: u32) {
        self.word(0x1e60_2800 | (dm << 16) | (dn << 5) | dd);
    }

    fn fsub_d(&mut self, dd: u32, dn: u32, dm: u32) {
        self.word(0x1e60_3800 | (dm << 16) | (dn << 5) | dd);
    }

    fn fmul_d(&mut self, dd: u32, dn: u32, dm: u32) {
        self.word(0x1e60_0800 | (dm << 16) | (dn << 5) | dd);
    }

    fn fdiv_d(&mut self, dd: u32, dn: u32, dm: u32) {
        self.word(0x1e60_1800 | (dm << 16) | (dn << 5) | dd);
    }

    /// `fcmp dn, dm` — sets NZCV for a conditional-set.
    fn fcmp_d(&mut self, dn: u32, dm: u32) {
        self.word(0x1e60_2000 | (dm << 16) | (dn << 5));
    }

    /// `sub xd, xn, #imm12`
    fn sub_reg_imm(&mut self, dst: Reg, src: Reg, imm: u32) {
        debug_assert!(imm < 4096);
        self.word(0xd100_0000 | (imm << 10) | ((src as u32) << 5) | dst as u32);
    }

    /// `cmp xn, #imm12` (subs xzr, xn, #imm)
    fn cmp_imm(&mut self, reg: Reg, imm: u32) {
        debug_assert!(imm < 4096);
        self.word(0xf100_001f | (imm << 10) | ((reg as u32) << 5));
    }

    /// `lsr xd, xn, #shift`
    fn lsr_imm(&mut self, dst: Reg, src: Reg, shift: u32) {
        debug_assert!(shift < 64);
        self.word(0xd340_0000 | (shift << 16) | (63 << 10) | ((src as u32) << 5) | dst as u32);
    }

    /// `lsl xd, xn, #shift`
    fn lsl_imm(&mut self, dst: Reg, src: Reg, shift: u32) {
        debug_assert!(0 < shift && shift < 64);
        let immr = 64 - shift;
        let imms = 63 - shift;
        self.word(0xd340_0000 | (immr << 16) | (imms << 10) | ((src as u32) << 5) | dst as u32);
    }

    /// `ldr xt, [xn, #imm]` (unsigned, 8-byte scaled)
    fn ldr_imm(&mut self, dst: Reg, base: Reg, imm: u32) {
        debug_assert!(imm.is_multiple_of(8) && imm / 8 < 4096);
        self.word(0xf940_0000 | ((imm / 8) << 10) | ((base as u32) << 5) | dst as u32);
    }

    /// `str xt, [xn, #imm]` (unsigned, 8-byte scaled)
    fn str_imm(&mut self, src: Reg, base: Reg, imm: u32) {
        debug_assert!(imm.is_multiple_of(8) && imm / 8 < 4096);
        self.word(0xf900_0000 | ((imm / 8) << 10) | ((base as u32) << 5) | src as u32);
    }

    /// `ldrb wt, [xn]` — peek without advancing.
    fn ldrb(&mut self, dst: Reg, base: Reg) {
        self.word(0x3940_0000 | ((base as u32) << 5) | dst as u32);
    }

    /// `ldr xt, [xn, xm]` — register-offset load.
    fn ldr_reg_offset(&mut self, dst: Reg, base: Reg, offset: Reg) {
        self.word(0xf860_6800 | ((offset as u32) << 16) | ((base as u32) << 5) | dst as u32);
    }

    /// `str xt, [xn, xm]` — register-offset store.
    fn str_reg_offset(&mut self, src: Reg, base: Reg, offset: Reg) {
        self.word(0xf820_6800 | ((offset as u32) << 16) | ((base as u32) << 5) | src as u32);
    }

    /// `ldrb wt, [xn], #1`
    fn ldrb_post_increment(&mut self, dst: Reg, base: Reg) {
        self.word(0x3840_1400 | ((base as u32) << 5) | dst as u32);
    }

    /// `strb wt, [xn], #1`
    fn strb_post_increment(&mut self, src: Reg, base: Reg) {
        self.word(0x3800_1400 | ((base as u32) << 5) | src as u32);
    }

    /// `stp x29, x30, [sp, #-16]!` — function prologue link save.
    fn push_frame_record(&mut self) {
        self.word(0xa9bf_7bfd);
    }

    /// `ldp x29, x30, [sp], #16` — function epilogue link restore.
    fn pop_frame_record(&mut self) {
        self.word(0xa8c1_7bfd);
    }

    /// `ret`
    fn ret(&mut self) {
        self.word(0xd65f_03c0);
    }
}

fn unsupported(span: Span, feature: &str) -> Diagnostic {
    Diagnostic::compile(
        span,
        format!("{feature} is not supported by the aarch64-apple-darwin backend yet"),
    )
}

/// Element types a list can carry in the current subset.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ListElem {
    Int,
    Bool,
    Str,
}

/// The value types the current subset can hold in a register / local.
/// `Str` is a pointer to a `[len: u64][bytes]` object in rodata or on
/// the bump heap; `List` is a pointer to a `[value: u64][next: ptr]`
/// cons cell (nil is the null pointer).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ValueType {
    Int,
    /// An IEEE 754 double held as raw bits in a GP register / frame
    /// slot, exactly like an Int; arithmetic and comparison bounce
    /// through the FP registers, everything else (storage, calls,
    /// locals) treats it as an opaque qword.
    Double,
    Bool,
    Str,
    List(ListElem),
    /// The type of `[]` before any element pins it down; assignable
    /// to every list type (it is the null pointer).
    EmptyList,
    /// An insertion-ordered, de-duplicated set, stored as the same
    /// cons-cell list as `List` (membership and dedup are linear
    /// scans).
    Set(ListElem),
    /// The type of `%()` before any element pins it down.
    EmptySet,
    /// A record value: index into the emitter's record table (nominal
    /// declarations and interned structural shapes).
    Record(u32),
    /// An untyped heap pointer: lowered enum values and the box cells
    /// the enum desugaring builds around scalars.
    Ptr,
    Unit,
    /// The "type" of diverging expressions (`__match_fail()`): merges
    /// with anything in an if join.
    Never,
}

/// Merge the types of two branches of an `if` expression: a diverging
/// branch adopts the other branch's type, and an empty-list branch
/// adopts the other branch's list type.
fn merge_branch_types(then_ty: ValueType, else_ty: ValueType) -> Option<ValueType> {
    match (then_ty, else_ty) {
        (ValueType::Never, other) | (other, ValueType::Never) => Some(other),
        (ValueType::EmptyList, other @ ValueType::List(_))
        | (other @ ValueType::List(_), ValueType::EmptyList) => Some(other),
        (ValueType::EmptySet, other @ ValueType::Set(_))
        | (other @ ValueType::Set(_), ValueType::EmptySet) => Some(other),
        (left, right) if left == right => Some(left),
        _ => None,
    }
}

/// Whether a value of type `actual` can flow into a slot of type
/// `expected` (locals, arguments, returns): exact match, or the
/// polymorphic empty list into any list slot.
fn assignable(actual: ValueType, expected: ValueType) -> bool {
    actual == expected
        || (actual == ValueType::EmptyList
            && matches!(expected, ValueType::List(_) | ValueType::EmptyList))
        || (actual == ValueType::EmptySet
            && matches!(expected, ValueType::Set(_) | ValueType::EmptySet))
}

/// One nominal record declaration or interned structural shape:
/// values are heap objects with one qword per field in declaration
/// order. A structural shape has an empty name.
struct RecordInfo {
    name: String,
    fields: Vec<(String, ValueType)>,
    /// False when the declaration could not be typed (generics,
    /// function-typed fields): the entry stays so `Record` indices
    /// remain stable, but every lookup skips it.
    usable: bool,
}

/// The value type of one list element of element type `elem`.
fn elem_value_type(elem: ListElem) -> ValueType {
    match elem {
        ListElem::Int => ValueType::Int,
        ListElem::Bool => ValueType::Bool,
        ListElem::Str => ValueType::Str,
    }
}

/// The element type a value of type `ty` can be a list element of.
fn list_elem_of(ty: ValueType) -> Option<ListElem> {
    match ty {
        ValueType::Int => Some(ListElem::Int),
        ValueType::Bool => Some(ListElem::Bool),
        ValueType::Str => Some(ListElem::Str),
        _ => None,
    }
}

/// A top-level annotated `def` compiled as a real AAPCS64 function:
/// arguments in x0..x7, result in x0, frame record saved by the
/// callee prologue (which is what makes recursion per-frame safe).
struct FunctionInfo {
    label: Label,
    params: Vec<(String, ValueType)>,
    ret: ValueType,
    body: Expr,
}

#[derive(Default)]
struct Emitter {
    asm: Assembler,
    functions: Vec<(String, FunctionInfo)>,
    /// Indices of functions reached from compiled code; only these
    /// are emitted (the stdlib prelude declares many functions the
    /// subset cannot compile — they only fail if actually called).
    pending: Vec<usize>,
    /// Nominal record declarations plus interned structural shapes;
    /// `ValueType::Record` carries an index into this table.
    records: Vec<RecordInfo>,
    /// Lazily created label of the shared heap-segment mmap routine;
    /// emitted at the end only when some allocation referenced it.
    heap_grow_label: Option<Label>,
    /// Lazily emitted set helpers (called via `bl`): scalar / string
    /// membership scans and a cons-list reverse.
    member_scalar_label: Option<Label>,
    member_string_label: Option<Label>,
    list_reverse_label: Option<Label>,
    /// Names of enums `desugar_enums` lowered to `__gc_record` shape;
    /// annotations naming them type as plain heap pointers.
    lowered_enums: std::collections::HashSet<String>,
    scopes: Vec<HashMap<String, (u32, ValueType)>>,
    next_local_offset: u32,
}

impl Emitter {
    fn lookup(&self, name: &str) -> Option<(u32, ValueType)> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.get(name).copied())
    }

    /// Resolve a type annotation against scalars, list types, lowered
    /// enums, and declared records (`#Point` or `Point`).
    fn annotation_type(&self, text: &str, span: Span) -> Result<ValueType, Diagnostic> {
        let trimmed = text.trim();
        let bare = trimmed.trim_start_matches('#');
        // Lowered monomorphic enum values travel as plain heap
        // pointers.
        if self.lowered_enums.contains(bare) {
            return Ok(ValueType::Ptr);
        }
        if let Some(index) = self
            .records
            .iter()
            .position(|record| record.usable && !record.name.is_empty() && record.name == bare)
        {
            return Ok(ValueType::Record(index as u32));
        }
        match trimmed {
            "Int" | "Long" | "Short" | "Byte" => Ok(ValueType::Int),
            "Double" | "Float" => Ok(ValueType::Double),
            "Bool" | "Boolean" => Ok(ValueType::Bool),
            "String" => Ok(ValueType::Str),
            "List<Int>" => Ok(ValueType::List(ListElem::Int)),
            "List<Bool>" => Ok(ValueType::List(ListElem::Bool)),
            "List<String>" => Ok(ValueType::List(ListElem::Str)),
            "Set<Int>" => Ok(ValueType::Set(ListElem::Int)),
            "Set<Bool>" => Ok(ValueType::Set(ListElem::Bool)),
            "Set<String>" => Ok(ValueType::Set(ListElem::Str)),
            "Unit" => Ok(ValueType::Unit),
            other => Err(unsupported(span, &format!("type annotation `{other}`"))),
        }
    }

    /// Intern a structural record shape, reusing an existing entry so
    /// equal shapes share one `ValueType::Record` index.
    fn intern_structural_record(&mut self, fields: Vec<(String, ValueType)>) -> u32 {
        if let Some(index) = self
            .records
            .iter()
            .position(|record| record.usable && record.name.is_empty() && record.fields == fields)
        {
            return index as u32;
        }
        self.records.push(RecordInfo {
            name: String::new(),
            fields,
            usable: true,
        });
        (self.records.len() - 1) as u32
    }

    fn declare_local(&mut self, name: &str, ty: ValueType) -> u32 {
        let offset = self.next_local_offset;
        self.next_local_offset += 8;
        self.scopes
            .last_mut()
            .expect("emitter scope")
            .insert(name.to_string(), (offset, ty));
        offset
    }

    /// Compile `expr`, leaving its value in x0. Binary operands use
    /// the machine stack (16-byte strides keep sp aligned).
    fn expression(&mut self, expr: &Expr) -> Result<ValueType, Diagnostic> {
        match expr {
            Expr::Int { value, .. } => {
                self.asm.mov_imm64(Reg::X0, *value as u64);
                Ok(ValueType::Int)
            }
            Expr::Double { value, .. } => {
                // The double travels as its raw IEEE 754 bits in x0.
                self.asm.mov_imm64(Reg::X0, value.to_bits());
                Ok(ValueType::Double)
            }
            Expr::Bool { value, .. } => {
                self.asm.mov_imm64(Reg::X0, u64::from(*value));
                Ok(ValueType::Bool)
            }
            Expr::String { value, span } => {
                if value.contains("#{") {
                    return Err(unsupported(*span, "string interpolation"));
                }
                let offset = self.asm.intern_string_object(value);
                self.asm.load_rodata_address(Reg::X0, offset);
                Ok(ValueType::Str)
            }
            Expr::Identifier { name, span } => {
                let Some((offset, ty)) = self.lookup(name) else {
                    return Err(unsupported(*span, &format!("identifier `{name}`")));
                };
                self.asm.load_local(Reg::X0, offset);
                Ok(ty)
            }
            Expr::Binary { lhs, op, rhs, span } => self.binary(lhs, *op, rhs, *span),
            Expr::ListLiteral { elements, span } => {
                // Build the cons chain back to front: all elements go
                // onto the machine stack first, then each pop becomes
                // the head of a fresh cell whose tail is in x0.
                let mut elem = None;
                for element in elements {
                    let ty = self.expression(element)?;
                    let Some(this_elem) = list_elem_of(ty) else {
                        return Err(unsupported(element.span(), "a list element of this type"));
                    };
                    if *elem.get_or_insert(this_elem) != this_elem {
                        return Err(unsupported(*span, "mixed list element types"));
                    }
                    self.asm.push(Reg::X0);
                }
                self.asm.mov_imm64(Reg::X0, 0); // nil
                for _ in elements {
                    self.emit_cons_cell();
                }
                Ok(match elem {
                    Some(elem) => ValueType::List(elem),
                    None => ValueType::EmptyList,
                })
            }
            Expr::SetLiteral { elements, span } => self.set_literal(elements, *span),
            // Nominal construction `#Point(3, 4)`.
            Expr::RecordConstructor {
                name,
                arguments,
                span,
            } => {
                let Some(index) = self
                    .records
                    .iter()
                    .position(|record| record.usable && record.name == *name)
                else {
                    return Err(unsupported(*span, &format!("record `{name}`")));
                };
                let fields = self.records[index].fields.clone();
                if fields.len() != arguments.len() {
                    return Err(Diagnostic::compile(
                        *span,
                        format!(
                            "{name} expects {} fields but got {}",
                            fields.len(),
                            arguments.len()
                        ),
                    ));
                }
                for (argument, (_, expected)) in arguments.iter().zip(fields.iter()) {
                    let ty = self.expression(argument)?;
                    if !assignable(ty, *expected) {
                        return Err(unsupported(argument.span(), "a record field of this type"));
                    }
                    self.asm.push(Reg::X0);
                }
                self.emit_record_object(arguments.len());
                Ok(ValueType::Record(index as u32))
            }
            // Structural literal `record { x: 1; y: 2 }`: the shape
            // is interned so equal shapes share one type.
            Expr::RecordLiteral { fields, .. } => {
                let mut typed = Vec::with_capacity(fields.len());
                for (field_name, value) in fields {
                    let ty = self.expression(value)?;
                    if ty == ValueType::Unit || ty == ValueType::Never {
                        return Err(unsupported(value.span(), "a record field of this type"));
                    }
                    typed.push((field_name.clone(), ty));
                    self.asm.push(Reg::X0);
                }
                let count = fields.len();
                let index = self.intern_structural_record(typed);
                self.emit_record_object(count);
                Ok(ValueType::Record(index))
            }
            Expr::FieldAccess {
                target,
                field,
                span,
            } => {
                let target_ty = self.expression(target)?;
                let ValueType::Record(index) = target_ty else {
                    return Err(unsupported(*span, "field access on this value"));
                };
                let info = &self.records[index as usize];
                let Some(position) = info
                    .fields
                    .iter()
                    .position(|(field_name, _)| field_name == field)
                else {
                    return Err(unsupported(*span, &format!("field `{field}`")));
                };
                let ty = info.fields[position].1;
                self.asm.ldr_imm(Reg::X0, Reg::X0, (position * 8) as u32);
                Ok(ty)
            }
            Expr::Call {
                callee,
                arguments,
                span,
            } => {
                // Curried `cons(head)(tail)` — the evaluator's list
                // prepend builtin.
                if arguments.len() == 1
                    && let Expr::Call {
                        callee: inner,
                        arguments: head_args,
                        ..
                    } = callee.as_ref()
                    && head_args.len() == 1
                    && matches!(inner.as_ref(), Expr::Identifier { name, .. } if name == "cons")
                {
                    let head_ty = self.expression(&head_args[0])?;
                    let Some(elem) = list_elem_of(head_ty) else {
                        return Err(unsupported(
                            head_args[0].span(),
                            "a list element of this type",
                        ));
                    };
                    self.asm.push(Reg::X0);
                    let tail_ty = self.expression(&arguments[0])?;
                    if !assignable(tail_ty, ValueType::List(elem)) {
                        return Err(unsupported(arguments[0].span(), "consing onto this value"));
                    }
                    self.emit_cons_cell();
                    return Ok(ValueType::List(elem));
                }
                // Method-style builtin call `target.method(args)`:
                // dispatch as `method(target, args)`, the same way the
                // evaluator and C backend resolve value methods.
                if let Expr::FieldAccess { target, field, .. } = callee.as_ref() {
                    let mut all = Vec::with_capacity(arguments.len() + 1);
                    all.push((**target).clone());
                    all.extend(arguments.iter().cloned());
                    if let Some(ty) = self.builtin_call(field, &all, *span)? {
                        return Ok(ty);
                    }
                    return Err(unsupported(*span, &format!("method `{field}`")));
                }
                let Expr::Identifier { name, .. } = callee.as_ref() else {
                    return Err(unsupported(*span, "calling a non-identifier"));
                };
                // Builtins first, mirroring the C backend's dispatch.
                if let Some(ty) = self.builtin_call(name, arguments, *span)? {
                    return Ok(ty);
                }
                self.function_call(name, arguments, *span)
            }
            Expr::If {
                condition,
                then_branch,
                else_branch,
                span,
            } => {
                let Some(else_branch) = else_branch else {
                    return Err(unsupported(*span, "if without else as an expression"));
                };
                let condition_ty = self.expression(condition)?;
                if condition_ty != ValueType::Bool {
                    return Err(unsupported(condition.span(), "a non-Bool condition"));
                }
                let else_label = self.asm.new_label();
                let end_label = self.asm.new_label();
                self.asm
                    .branch(else_label, BranchKind::CompareZero(Reg::X0));
                let then_ty = self.expression(then_branch)?;
                self.asm.branch(end_label, BranchKind::Unconditional);
                self.asm.bind(else_label);
                let else_ty = self.expression(else_branch)?;
                self.asm.bind(end_label);
                merge_branch_types(then_ty, else_ty)
                    .ok_or_else(|| unsupported(*span, "if branches with different types"))
            }
            // A block in expression position: statements, then the
            // value of the final expression (the enum lowering leans
            // on this shape heavily).
            Expr::Block { expressions, .. } => {
                let Some((last, init)) = expressions.split_last() else {
                    self.asm.mov_imm64(Reg::X0, 0);
                    return Ok(ValueType::Unit);
                };
                self.scopes.push(HashMap::new());
                for expression in init {
                    self.statement(expression)?;
                }
                let ty = self.expression(last)?;
                self.scopes.pop();
                Ok(ty)
            }
            other => Err(unsupported(other.span(), "this expression")),
        }
    }

    fn binary(
        &mut self,
        lhs: &Expr,
        op: BinaryOp,
        rhs: &Expr,
        span: Span,
    ) -> Result<ValueType, Diagnostic> {
        // Short-circuit logic first: rhs must not evaluate eagerly.
        if matches!(op, BinaryOp::LogicalAnd | BinaryOp::LogicalOr) {
            let lhs_ty = self.expression(lhs)?;
            let end_label = self.asm.new_label();
            match op {
                BinaryOp::LogicalAnd => {
                    self.asm.branch(end_label, BranchKind::CompareZero(Reg::X0))
                }
                _ => self
                    .asm
                    .branch(end_label, BranchKind::CompareNonZero(Reg::X0)),
            }
            let rhs_ty = self.expression(rhs)?;
            self.asm.bind(end_label);
            if lhs_ty != ValueType::Bool || rhs_ty != ValueType::Bool {
                return Err(unsupported(span, "logical operator on non-Bool operands"));
            }
            return Ok(ValueType::Bool);
        }

        let lhs_ty = self.expression(lhs)?;
        self.asm.push(Reg::X0);
        let rhs_ty = self.expression(rhs)?;
        self.asm.mov_reg(Reg::X1, Reg::X0);
        self.asm.pop(Reg::X0);
        if lhs_ty != rhs_ty {
            return Err(unsupported(span, "mixed operand types"));
        }
        if lhs_ty == ValueType::Str {
            return match op {
                BinaryOp::Add => {
                    self.emit_str_concat();
                    Ok(ValueType::Str)
                }
                BinaryOp::Equal => {
                    self.emit_str_eq();
                    Ok(ValueType::Bool)
                }
                BinaryOp::NotEqual => {
                    self.emit_str_eq();
                    self.asm.cmp_imm(Reg::X0, 0);
                    self.asm.cset(Reg::X0, Cond::Eq);
                    Ok(ValueType::Bool)
                }
                _ => Err(unsupported(span, "this string operator")),
            };
        }
        if lhs_ty == ValueType::Double {
            // Both operands are raw double bits in x0 / x1; move them
            // into d0 / d1 for the FP unit. IEEE division by zero is
            // defined (it yields an infinity), so no zero guard.
            self.asm.fmov_d_from_x(0, Reg::X0);
            self.asm.fmov_d_from_x(1, Reg::X1);
            return match op {
                BinaryOp::Add | BinaryOp::Subtract | BinaryOp::Multiply | BinaryOp::Divide => {
                    match op {
                        BinaryOp::Add => self.asm.fadd_d(0, 0, 1),
                        BinaryOp::Subtract => self.asm.fsub_d(0, 0, 1),
                        BinaryOp::Multiply => self.asm.fmul_d(0, 0, 1),
                        _ => self.asm.fdiv_d(0, 0, 1),
                    }
                    self.asm.fmov_x_from_d(Reg::X0, 0);
                    Ok(ValueType::Double)
                }
                BinaryOp::Less
                | BinaryOp::LessEqual
                | BinaryOp::Greater
                | BinaryOp::GreaterEqual
                | BinaryOp::Equal
                | BinaryOp::NotEqual => {
                    let cond = match op {
                        BinaryOp::Less => Cond::Mi,
                        BinaryOp::LessEqual => Cond::Ls,
                        BinaryOp::Greater => Cond::Gt,
                        BinaryOp::GreaterEqual => Cond::Ge,
                        BinaryOp::Equal => Cond::Eq,
                        _ => Cond::Ne,
                    };
                    self.asm.fcmp_d(0, 1);
                    self.asm.cset(Reg::X0, cond);
                    Ok(ValueType::Bool)
                }
                _ => Err(unsupported(span, "this double operator")),
            };
        }
        match op {
            BinaryOp::Add | BinaryOp::Subtract | BinaryOp::Multiply | BinaryOp::Divide => {
                if lhs_ty != ValueType::Int {
                    return Err(unsupported(span, "arithmetic on non-Int operands"));
                }
                match op {
                    BinaryOp::Add => self.asm.add_reg(Reg::X0, Reg::X0, Reg::X1),
                    BinaryOp::Subtract => self.asm.sub_reg(Reg::X0, Reg::X0, Reg::X1),
                    BinaryOp::Multiply => self.asm.mul_reg(Reg::X0, Reg::X0, Reg::X1),
                    _ => {
                        // Match the evaluator: division by zero is a
                        // runtime error, not an arm64 zero result.
                        let ok_label = self.asm.new_label();
                        self.asm
                            .branch(ok_label, BranchKind::CompareNonZero(Reg::X1));
                        self.asm
                            .emit_write_rodata(STDERR_FD, b"klassic: division by zero\n");
                        self.asm.emit_exit(1);
                        self.asm.bind(ok_label);
                        self.asm.sdiv_reg(Reg::X0, Reg::X0, Reg::X1);
                    }
                }
                Ok(ValueType::Int)
            }
            BinaryOp::Less | BinaryOp::LessEqual | BinaryOp::Greater | BinaryOp::GreaterEqual => {
                if lhs_ty != ValueType::Int {
                    return Err(unsupported(span, "comparison on non-Int operands"));
                }
                let cond = match op {
                    BinaryOp::Less => Cond::Lt,
                    BinaryOp::LessEqual => Cond::Le,
                    BinaryOp::Greater => Cond::Gt,
                    _ => Cond::Ge,
                };
                self.asm.cmp_reg(Reg::X0, Reg::X1);
                self.asm.cset(Reg::X0, cond);
                Ok(ValueType::Bool)
            }
            BinaryOp::Equal | BinaryOp::NotEqual => {
                let cond = if op == BinaryOp::Equal {
                    Cond::Eq
                } else {
                    Cond::Ne
                };
                self.asm.cmp_reg(Reg::X0, Reg::X1);
                self.asm.cset(Reg::X0, cond);
                Ok(ValueType::Bool)
            }
            _ => Err(unsupported(span, "this binary operator")),
        }
    }

    /// Bump-allocate `x4` bytes (already 8-aligned): result pointer in
    /// x5, x19 advanced. Calls the shared mmap routine when the
    /// current segment is too small; that routine preserves x0-x5.
    fn emit_alloc(&mut self) {
        let fits = self.asm.new_label();
        self.asm.sub_reg(Reg::X6, Reg::X20, Reg::X19);
        self.asm.cmp_reg(Reg::X6, Reg::X4);
        self.asm.branch(fits, BranchKind::Conditional(Cond::Ge));
        let grow = match self.heap_grow_label {
            Some(label) => label,
            None => {
                let label = self.asm.new_label();
                self.heap_grow_label = Some(label);
                label
            }
        };
        self.asm.branch(grow, BranchKind::Link);
        self.asm.bind(fits);
        self.asm.mov_reg(Reg::X5, Reg::X19);
        self.asm.add_reg(Reg::X19, Reg::X19, Reg::X4);
    }

    /// Copy `[count]` bytes between the byte pointers in `src`/`dst`;
    /// `count` reaches zero, `src`/`dst` advance, `scratch` clobbered.
    fn emit_copy_bytes(&mut self, count: Reg, src: Reg, dst: Reg, scratch: Reg) {
        let copy_loop = self.asm.new_label();
        let done = self.asm.new_label();
        self.asm.bind(copy_loop);
        self.asm.branch(done, BranchKind::CompareZero(count));
        self.asm.ldrb_post_increment(scratch, src);
        self.asm.strb_post_increment(scratch, dst);
        self.asm.sub_reg_imm(count, count, 1);
        self.asm.branch(copy_loop, BranchKind::Unconditional);
        self.asm.bind(done);
    }

    /// String concatenation: a in x0, b in x1 → fresh heap object in
    /// x0. Layout is `[len][a bytes][b bytes]`.
    fn emit_str_concat(&mut self) {
        self.asm.ldr_imm(Reg::X2, Reg::X0, 0);
        self.asm.ldr_imm(Reg::X3, Reg::X1, 0);
        self.asm.add_reg(Reg::X2, Reg::X2, Reg::X3);
        // size = align8(total + 8 header)
        self.asm.add_reg_imm(Reg::X4, Reg::X2, 15);
        self.asm.lsr_imm(Reg::X4, Reg::X4, 3);
        self.asm.lsl_imm(Reg::X4, Reg::X4, 3);
        self.emit_alloc();
        self.asm.str_imm(Reg::X2, Reg::X5, 0);
        self.asm.add_reg_imm(Reg::X7, Reg::X5, 8);
        self.asm.ldr_imm(Reg::X2, Reg::X0, 0);
        self.asm.add_reg_imm(Reg::X6, Reg::X0, 8);
        self.emit_copy_bytes(Reg::X2, Reg::X6, Reg::X7, Reg::X3);
        self.asm.ldr_imm(Reg::X2, Reg::X1, 0);
        self.asm.add_reg_imm(Reg::X6, Reg::X1, 8);
        self.emit_copy_bytes(Reg::X2, Reg::X6, Reg::X7, Reg::X3);
        self.asm.mov_reg(Reg::X0, Reg::X5);
    }

    /// String equality: a in x0, b in x1 → Bool in x0.
    fn emit_str_eq(&mut self) {
        let differ = self.asm.new_label();
        let same = self.asm.new_label();
        let end = self.asm.new_label();
        self.asm.ldr_imm(Reg::X2, Reg::X0, 0);
        self.asm.ldr_imm(Reg::X3, Reg::X1, 0);
        self.asm.cmp_reg(Reg::X2, Reg::X3);
        self.asm.branch(differ, BranchKind::Conditional(Cond::Ne));
        self.asm.add_reg_imm(Reg::X6, Reg::X0, 8);
        self.asm.add_reg_imm(Reg::X7, Reg::X1, 8);
        let byte_loop = self.asm.new_label();
        self.asm.bind(byte_loop);
        self.asm.branch(same, BranchKind::CompareZero(Reg::X2));
        self.asm.ldrb_post_increment(Reg::X3, Reg::X6);
        self.asm.ldrb_post_increment(Reg::X4, Reg::X7);
        self.asm.cmp_reg(Reg::X3, Reg::X4);
        self.asm.branch(differ, BranchKind::Conditional(Cond::Ne));
        self.asm.sub_reg_imm(Reg::X2, Reg::X2, 1);
        self.asm.branch(byte_loop, BranchKind::Unconditional);
        self.asm.bind(same);
        self.asm.mov_imm64(Reg::X0, 1);
        self.asm.branch(end, BranchKind::Unconditional);
        self.asm.bind(differ);
        self.asm.mov_imm64(Reg::X0, 0);
        self.asm.bind(end);
    }

    /// `length(s)`: UTF-8 character count, matching the evaluator —
    /// bytes whose top two bits are not `10` start a character.
    fn emit_str_char_count(&mut self) {
        self.asm.ldr_imm(Reg::X2, Reg::X0, 0);
        self.asm.add_reg_imm(Reg::X3, Reg::X0, 8);
        self.asm.mov_imm64(Reg::X4, 0);
        let count_loop = self.asm.new_label();
        let done = self.asm.new_label();
        let continuation = self.asm.new_label();
        self.asm.bind(count_loop);
        self.asm.branch(done, BranchKind::CompareZero(Reg::X2));
        self.asm.ldrb_post_increment(Reg::X5, Reg::X3);
        self.asm.lsr_imm(Reg::X5, Reg::X5, 6);
        self.asm.cmp_imm(Reg::X5, 2);
        self.asm
            .branch(continuation, BranchKind::Conditional(Cond::Eq));
        self.asm.add_reg_imm(Reg::X4, Reg::X4, 1);
        self.asm.bind(continuation);
        self.asm.sub_reg_imm(Reg::X2, Reg::X2, 1);
        self.asm.branch(count_loop, BranchKind::Unconditional);
        self.asm.bind(done);
        self.asm.mov_reg(Reg::X0, Reg::X4);
    }

    /// Advance over up to x5 UTF-8 characters: x3 = byte pointer
    /// (advanced), x4 = remaining bytes (decremented), x5 destroyed,
    /// x6 clobbered. Stops early when the bytes run out, which is
    /// what clamps out-of-range indices.
    fn emit_skip_chars(&mut self) {
        let scan_loop = self.asm.new_label();
        let done = self.asm.new_label();
        let char_done = self.asm.new_label();
        let continuation_loop = self.asm.new_label();
        self.asm.bind(scan_loop);
        self.asm.branch(done, BranchKind::CompareZero(Reg::X5));
        self.asm.branch(done, BranchKind::CompareZero(Reg::X4));
        // Consume the lead byte, then any continuation bytes (top
        // bits `10`).
        self.asm.ldrb_post_increment(Reg::X6, Reg::X3);
        self.asm.sub_reg_imm(Reg::X4, Reg::X4, 1);
        self.asm.bind(continuation_loop);
        self.asm.branch(char_done, BranchKind::CompareZero(Reg::X4));
        self.asm.ldrb(Reg::X6, Reg::X3);
        self.asm.lsr_imm(Reg::X6, Reg::X6, 6);
        self.asm.cmp_imm(Reg::X6, 2);
        self.asm
            .branch(char_done, BranchKind::Conditional(Cond::Ne));
        self.asm.add_reg_imm(Reg::X3, Reg::X3, 1);
        self.asm.sub_reg_imm(Reg::X4, Reg::X4, 1);
        self.asm
            .branch(continuation_loop, BranchKind::Unconditional);
        self.asm.bind(char_done);
        self.asm.sub_reg_imm(Reg::X5, Reg::X5, 1);
        self.asm.branch(scan_loop, BranchKind::Unconditional);
        self.asm.bind(done);
    }

    /// `substring(s, start, end)` with the evaluator's semantics:
    /// char-indexed, negatives clamp to 0, everything clamps to the
    /// string, `end < start` yields the empty string. In: s = x0,
    /// start = x1, end = x2; out: fresh object pointer in x0.
    fn emit_substring(&mut self) {
        // Clamp negative indices to zero.
        for reg in [Reg::X1, Reg::X2] {
            let non_negative = self.asm.new_label();
            self.asm.cmp_imm(reg, 0);
            self.asm
                .branch(non_negative, BranchKind::Conditional(Cond::Ge));
            self.asm.mov_imm64(reg, 0);
            self.asm.bind(non_negative);
        }
        // First scan: skip `start` characters from the payload.
        self.asm.ldr_imm(Reg::X4, Reg::X0, 0);
        self.asm.add_reg_imm(Reg::X3, Reg::X0, 8);
        self.asm.mov_reg(Reg::X5, Reg::X1);
        // chars to take = max(end - start, 0); computed before the
        // scans clobber x1/x2.
        self.asm.sub_reg(Reg::X2, Reg::X2, Reg::X1);
        self.emit_skip_chars();
        // x0 is free now: keep the slice's first byte there.
        self.asm.mov_reg(Reg::X0, Reg::X3);
        let non_negative_take = self.asm.new_label();
        self.asm.cmp_imm(Reg::X2, 0);
        self.asm
            .branch(non_negative_take, BranchKind::Conditional(Cond::Ge));
        self.asm.mov_imm64(Reg::X2, 0);
        self.asm.bind(non_negative_take);
        // Second scan: advance over the characters being taken.
        self.asm.mov_reg(Reg::X5, Reg::X2);
        self.emit_skip_chars();
        // Slice byte length, then allocate and copy.
        self.asm.sub_reg(Reg::X2, Reg::X3, Reg::X0);
        self.asm.add_reg_imm(Reg::X4, Reg::X2, 15);
        self.asm.lsr_imm(Reg::X4, Reg::X4, 3);
        self.asm.lsl_imm(Reg::X4, Reg::X4, 3);
        self.emit_alloc();
        self.asm.str_imm(Reg::X2, Reg::X5, 0);
        self.asm.add_reg_imm(Reg::X7, Reg::X5, 8);
        self.emit_copy_bytes(Reg::X2, Reg::X0, Reg::X7, Reg::X3);
        self.asm.mov_reg(Reg::X0, Reg::X5);
    }

    /// `toString` of the integer in x0 → fresh string object in x0.
    fn emit_int_to_str(&mut self) {
        self.asm.sub_sp_imm(32);
        self.asm.emit_int_digits(false);
        self.asm.add_reg_sp_imm(Reg::X2, 32);
        self.asm.sub_reg(Reg::X2, Reg::X2, Reg::X1);
        self.asm.add_reg_imm(Reg::X4, Reg::X2, 15);
        self.asm.lsr_imm(Reg::X4, Reg::X4, 3);
        self.asm.lsl_imm(Reg::X4, Reg::X4, 3);
        self.emit_alloc();
        self.asm.str_imm(Reg::X2, Reg::X5, 0);
        self.asm.add_reg_imm(Reg::X7, Reg::X5, 8);
        self.emit_copy_bytes(Reg::X2, Reg::X1, Reg::X7, Reg::X3);
        self.asm.mov_reg(Reg::X0, Reg::X5);
        self.asm.add_sp_imm(32);
    }

    /// `toString` of the Bool in x0 → interned "true"/"false" object.
    fn emit_bool_to_str(&mut self) {
        let false_label = self.asm.new_label();
        let end_label = self.asm.new_label();
        let true_offset = self.asm.intern_string_object("true");
        let false_offset = self.asm.intern_string_object("false");
        self.asm
            .branch(false_label, BranchKind::CompareZero(Reg::X0));
        self.asm.load_rodata_address(Reg::X0, true_offset);
        self.asm.branch(end_label, BranchKind::Unconditional);
        self.asm.bind(false_label);
        self.asm.load_rodata_address(Reg::X0, false_offset);
        self.asm.bind(end_label);
    }

    /// Build a record object from `count` field values pushed onto
    /// the machine stack in declaration order → object pointer in x0.
    fn emit_record_object(&mut self, count: usize) {
        self.asm.mov_imm64(Reg::X4, (count * 8) as u64);
        self.emit_alloc();
        for position in (0..count).rev() {
            self.asm.pop(Reg::X1);
            self.asm.str_imm(Reg::X1, Reg::X5, (position * 8) as u32);
        }
        self.asm.mov_reg(Reg::X0, Reg::X5);
    }

    /// Prepend a cons cell: head value on top of the machine stack,
    /// tail pointer in x0 → fresh cell pointer in x0.
    fn emit_cons_cell(&mut self) {
        self.asm.mov_imm64(Reg::X4, 16);
        self.emit_alloc();
        self.asm.pop(Reg::X1);
        self.asm.str_imm(Reg::X1, Reg::X5, 0);
        self.asm.str_imm(Reg::X0, Reg::X5, 8);
        self.asm.mov_reg(Reg::X0, Reg::X5);
    }

    /// Get/create the lazily-emitted membership-scan routine label for
    /// an element type. Both the routines take `x0` = list head and
    /// `x1` = candidate and return a Bool in `x0`.
    fn member_label(&mut self, elem: ListElem) -> Label {
        let slot = match elem {
            ListElem::Str => &mut self.member_string_label,
            _ => &mut self.member_scalar_label,
        };
        match slot {
            Some(label) => *label,
            None => {
                let label = self.asm.new_label();
                *slot = Some(label);
                label
            }
        }
    }

    fn reverse_label(&mut self) -> Label {
        match self.list_reverse_label {
            Some(label) => label,
            None => {
                let label = self.asm.new_label();
                self.list_reverse_label = Some(label);
                label
            }
        }
    }

    /// Compile a `%(...)` set literal: each element is added to a
    /// cons-list accumulator only if a membership scan does not find
    /// it, so duplicates collapse to their first occurrence. The
    /// accumulator ends up in reverse-insertion order, so a final
    /// reverse restores insertion order for printing.
    fn set_literal(&mut self, elements: &[Expr], span: Span) -> Result<ValueType, Diagnostic> {
        // Frame slot for the partial set; survives the bl calls below.
        let acc = self.next_local_offset;
        self.next_local_offset += 8;
        self.asm.mov_imm64(Reg::X0, 0); // nil
        self.asm.store_local(Reg::X0, acc);

        let mut elem_ty = None;
        for element in elements {
            let ty = self.expression(element)?;
            let Some(this) = list_elem_of(ty) else {
                return Err(unsupported(element.span(), "a set element of this type"));
            };
            if *elem_ty.get_or_insert(this) != this {
                return Err(unsupported(span, "mixed set element types"));
            }
            // Is the candidate (x0) already in the partial set?
            self.asm.mov_reg(Reg::X1, Reg::X0);
            self.asm.push(Reg::X1); // candidate survives the bl
            self.asm.load_local(Reg::X0, acc);
            let member = self.member_label(this);
            self.asm.branch(member, BranchKind::Link);
            let skip = self.asm.new_label();
            self.asm.branch(skip, BranchKind::CompareNonZero(Reg::X0));
            // Absent: prepend the candidate to the accumulator.
            self.asm.pop(Reg::X1);
            self.asm.push(Reg::X1); // head for emit_cons_cell
            self.asm.load_local(Reg::X0, acc);
            self.emit_cons_cell();
            self.asm.store_local(Reg::X0, acc);
            let after = self.asm.new_label();
            self.asm.branch(after, BranchKind::Unconditional);
            self.asm.bind(skip);
            self.asm.pop(Reg::X1); // discard the candidate
            self.asm.bind(after);
        }

        self.asm.load_local(Reg::X0, acc);
        let reverse = self.reverse_label();
        self.asm.branch(reverse, BranchKind::Link);
        Ok(match elem_ty {
            Some(elem) => ValueType::Set(elem),
            None => ValueType::EmptySet,
        })
    }

    /// println of the set in x0 in the evaluator's format: `%(e1, e2)`.
    fn emit_println_set(&mut self, elem: ListElem) {
        let loop_start = self.asm.new_label();
        let close = self.asm.new_label();
        self.asm.push(Reg::X0);
        self.asm.emit_write_rodata(STDOUT_FD, b"%(");
        self.asm.pop(Reg::X3);
        self.asm.push(Reg::X3);
        self.asm.branch(close, BranchKind::CompareZero(Reg::X3));
        self.asm.bind(loop_start);
        self.asm.pop(Reg::X3);
        self.asm.push(Reg::X3);
        self.asm.ldr_imm(Reg::X0, Reg::X3, 0);
        self.emit_print_elem(elem);
        self.asm.pop(Reg::X3);
        self.asm.ldr_imm(Reg::X3, Reg::X3, 8);
        self.asm.push(Reg::X3);
        self.asm.branch(close, BranchKind::CompareZero(Reg::X3));
        self.asm.emit_write_rodata(STDOUT_FD, b", ");
        self.asm.branch(loop_start, BranchKind::Unconditional);
        self.asm.bind(close);
        self.asm.pop(Reg::X3);
        self.asm.emit_write_rodata(STDOUT_FD, b")\n");
    }

    /// `bl`-called scalar membership scan: x0 = list, x1 = candidate
    /// (a raw Int/Bool qword) → x0 = Bool. No frame: it calls nothing.
    fn emit_member_scalar_routine(&mut self, label: Label) {
        self.asm.bind(label);
        self.asm.mov_reg(Reg::X2, Reg::X0); // cursor
        let loop_start = self.asm.new_label();
        let found = self.asm.new_label();
        let not_found = self.asm.new_label();
        self.asm.bind(loop_start);
        self.asm.branch(not_found, BranchKind::CompareZero(Reg::X2));
        self.asm.ldr_imm(Reg::X3, Reg::X2, 0);
        self.asm.cmp_reg(Reg::X3, Reg::X1);
        self.asm.branch(found, BranchKind::Conditional(Cond::Eq));
        self.asm.ldr_imm(Reg::X2, Reg::X2, 8);
        self.asm.branch(loop_start, BranchKind::Unconditional);
        self.asm.bind(found);
        self.asm.mov_imm64(Reg::X0, 1);
        self.asm.ret();
        self.asm.bind(not_found);
        self.asm.mov_imm64(Reg::X0, 0);
        self.asm.ret();
    }

    /// `bl`-called string membership scan: x0 = list, x1 = candidate
    /// (a string pointer) → x0 = Bool. The candidate and cursor live
    /// in a 16-byte stack scratch addressed off x5, which the inlined
    /// `str_eq` (using x0-x4/x6/x7) never touches.
    fn emit_member_string_routine(&mut self, label: Label) {
        self.asm.bind(label);
        self.asm.sub_sp_imm(16);
        self.asm.add_reg_sp_imm(Reg::X5, 0); // scratch base
        self.asm.str_imm(Reg::X1, Reg::X5, 0); // candidate
        self.asm.mov_reg(Reg::X2, Reg::X0); // cursor
        let loop_start = self.asm.new_label();
        let found = self.asm.new_label();
        let not_found = self.asm.new_label();
        self.asm.bind(loop_start);
        self.asm.branch(not_found, BranchKind::CompareZero(Reg::X2));
        self.asm.str_imm(Reg::X2, Reg::X5, 8); // save cursor
        self.asm.ldr_imm(Reg::X0, Reg::X2, 0); // element
        self.asm.ldr_imm(Reg::X1, Reg::X5, 0); // candidate
        self.emit_str_eq();
        self.asm.branch(found, BranchKind::CompareNonZero(Reg::X0));
        self.asm.ldr_imm(Reg::X2, Reg::X5, 8); // restore cursor
        self.asm.ldr_imm(Reg::X2, Reg::X2, 8); // next
        self.asm.branch(loop_start, BranchKind::Unconditional);
        self.asm.bind(found);
        self.asm.add_sp_imm(16);
        self.asm.mov_imm64(Reg::X0, 1);
        self.asm.ret();
        self.asm.bind(not_found);
        self.asm.add_sp_imm(16);
        self.asm.mov_imm64(Reg::X0, 0);
        self.asm.ret();
    }

    /// `bl`-called cons-list reverse: x0 = list → x0 = fresh reversed
    /// list. Needs a frame record because it allocates (which can
    /// `bl` the heap-grow routine).
    fn emit_list_reverse_routine(&mut self, label: Label) {
        self.asm.bind(label);
        self.asm.push_frame_record();
        self.asm.mov_imm64(Reg::X1, 0); // acc = nil
        let loop_start = self.asm.new_label();
        let done = self.asm.new_label();
        self.asm.bind(loop_start);
        self.asm.branch(done, BranchKind::CompareZero(Reg::X0));
        self.asm.ldr_imm(Reg::X2, Reg::X0, 0); // head
        self.asm.ldr_imm(Reg::X0, Reg::X0, 8); // advance input
        // Build [head][acc]; emit_alloc preserves x0-x5 across grow.
        self.asm.mov_imm64(Reg::X4, 16);
        self.emit_alloc();
        self.asm.str_imm(Reg::X2, Reg::X5, 0); // head
        self.asm.str_imm(Reg::X1, Reg::X5, 8); // next = acc
        self.asm.mov_reg(Reg::X1, Reg::X5); // acc = new cell
        self.asm.branch(loop_start, BranchKind::Unconditional);
        self.asm.bind(done);
        self.asm.mov_reg(Reg::X0, Reg::X1);
        self.asm.pop_frame_record();
        self.asm.ret();
    }

    /// Print one element value in x0 without a newline; clobbers
    /// x0-x5/x16.
    fn emit_print_elem(&mut self, elem: ListElem) {
        match elem {
            ListElem::Int => {
                self.asm.sub_sp_imm(32);
                self.asm.emit_int_digits(false);
                self.asm.add_reg_sp_imm(Reg::X2, 32);
                self.asm.sub_reg(Reg::X2, Reg::X2, Reg::X1);
                self.asm.mov_imm64(Reg::X0, STDOUT_FD);
                self.asm.mov_imm64(Reg::X16, u64::from(SYS_WRITE));
                self.asm.svc_0x80();
                self.asm.add_sp_imm(32);
            }
            ListElem::Str => {
                self.asm.ldr_imm(Reg::X2, Reg::X0, 0);
                self.asm.add_reg_imm(Reg::X1, Reg::X0, 8);
                self.asm.mov_imm64(Reg::X0, STDOUT_FD);
                self.asm.mov_imm64(Reg::X16, u64::from(SYS_WRITE));
                self.asm.svc_0x80();
            }
            ListElem::Bool => {
                let false_label = self.asm.new_label();
                let end_label = self.asm.new_label();
                self.asm
                    .branch(false_label, BranchKind::CompareZero(Reg::X0));
                self.asm.emit_write_rodata(STDOUT_FD, b"true");
                self.asm.branch(end_label, BranchKind::Unconditional);
                self.asm.bind(false_label);
                self.asm.emit_write_rodata(STDOUT_FD, b"false");
                self.asm.bind(end_label);
            }
        }
    }

    /// println of the record object in x0 in the evaluator's format:
    /// `#Name(v1, v2)` for nominal records, `#(v1, v2)` for
    /// structural shapes. Scalar and string fields only for now.
    fn emit_println_record(&mut self, index: u32, span: Span) -> Result<(), Diagnostic> {
        let info = &self.records[index as usize];
        let opener = format!("#{}(", info.name);
        let fields = info.fields.clone();
        for (_, ty) in &fields {
            if list_elem_of(*ty).is_none() {
                return Err(unsupported(span, "printing a record with this field type"));
            }
        }
        self.asm.push(Reg::X0); // the object survives the writes
        self.asm.emit_write_rodata(STDOUT_FD, opener.as_bytes());
        for (position, (_, ty)) in fields.iter().enumerate() {
            if position > 0 {
                self.asm.emit_write_rodata(STDOUT_FD, b", ");
            }
            self.asm.pop(Reg::X0);
            self.asm.push(Reg::X0);
            self.asm.ldr_imm(Reg::X0, Reg::X0, (position * 8) as u32);
            self.emit_print_elem(list_elem_of(*ty).expect("checked above"));
        }
        self.asm.pop(Reg::X0); // discard the saved object
        self.asm.emit_write_rodata(STDOUT_FD, b")\n");
        Ok(())
    }

    /// println of the list in x0 in the evaluator's format:
    /// `[e1, e2, ...]` (strings unquoted).
    fn emit_println_list(&mut self, elem: ListElem) {
        let loop_start = self.asm.new_label();
        let close = self.asm.new_label();
        self.asm.push(Reg::X0); // cursor survives the writes below
        self.asm.emit_write_rodata(STDOUT_FD, b"[");
        self.asm.pop(Reg::X3);
        self.asm.push(Reg::X3);
        self.asm.branch(close, BranchKind::CompareZero(Reg::X3));
        self.asm.bind(loop_start);
        self.asm.pop(Reg::X3);
        self.asm.push(Reg::X3);
        self.asm.ldr_imm(Reg::X0, Reg::X3, 0);
        self.emit_print_elem(elem);
        self.asm.pop(Reg::X3);
        self.asm.ldr_imm(Reg::X3, Reg::X3, 8);
        self.asm.push(Reg::X3);
        self.asm.branch(close, BranchKind::CompareZero(Reg::X3));
        self.asm.emit_write_rodata(STDOUT_FD, b", ");
        self.asm.branch(loop_start, BranchKind::Unconditional);
        self.asm.bind(close);
        self.asm.pop(Reg::X3); // discard the cursor slot
        self.asm.emit_write_rodata(STDOUT_FD, b"]\n");
    }

    /// println of the string object in x0: one write for the payload,
    /// one for the newline.
    fn emit_println_str(&mut self) {
        self.asm.ldr_imm(Reg::X2, Reg::X0, 0);
        self.asm.add_reg_imm(Reg::X1, Reg::X0, 8);
        self.asm.mov_imm64(Reg::X0, STDOUT_FD);
        self.asm.mov_imm64(Reg::X16, u64::from(SYS_WRITE));
        self.asm.svc_0x80();
        self.asm.emit_write_rodata(STDOUT_FD, b"\n");
    }

    /// The shared heap-grow routine: mmap a fresh segment into
    /// x19/x20. Preserves x0-x5 because allocation sites have live
    /// operands in them.
    fn emit_heap_grow_routine(&mut self, label: Label) {
        self.asm.bind(label);
        self.asm.push_frame_record();
        for reg in [Reg::X0, Reg::X1, Reg::X2, Reg::X3, Reg::X4, Reg::X5] {
            self.asm.push(reg);
        }
        self.asm.mov_imm64(Reg::X0, 0);
        self.asm.mov_imm64(Reg::X1, HEAP_SEGMENT_BYTES);
        self.asm.mov_imm64(Reg::X2, PROT_READ_WRITE);
        self.asm.mov_imm64(Reg::X3, MMAP_ANON_PRIVATE);
        self.asm.mov_imm64(Reg::X4, u64::MAX); // fd = -1
        self.asm.mov_imm64(Reg::X5, 0);
        self.asm.mov_imm64(Reg::X16, u64::from(SYS_MMAP));
        self.asm.svc_0x80();
        self.asm.mov_reg(Reg::X19, Reg::X0);
        self.asm.mov_imm64(Reg::X1, HEAP_SEGMENT_BYTES);
        self.asm.add_reg(Reg::X20, Reg::X19, Reg::X1);
        for reg in [Reg::X5, Reg::X4, Reg::X3, Reg::X2, Reg::X1, Reg::X0] {
            self.asm.pop(reg);
        }
        self.asm.pop_frame_record();
        self.asm.ret();
    }

    /// String / display builtins, mirroring the C backend's surface.
    /// Returns Ok(None) when `name` is not a builtin.
    fn builtin_call(
        &mut self,
        name: &str,
        arguments: &[Expr],
        span: Span,
    ) -> Result<Option<ValueType>, Diagnostic> {
        match (name, arguments.len()) {
            ("length", 1) => {
                if self.expression(&arguments[0])? != ValueType::Str {
                    return Err(unsupported(span, "length of a non-string"));
                }
                self.emit_str_char_count();
                Ok(Some(ValueType::Int))
            }
            ("isEmptyString", 1) => {
                if self.expression(&arguments[0])? != ValueType::Str {
                    return Err(unsupported(span, "isEmptyString of a non-string"));
                }
                self.asm.ldr_imm(Reg::X1, Reg::X0, 0);
                self.asm.cmp_imm(Reg::X1, 0);
                // Reuse the comparison: x0 = (byte length == 0).
                self.asm.cset(Reg::X0, Cond::Eq);
                Ok(Some(ValueType::Bool))
            }
            ("toString", 1) => {
                match self.expression(&arguments[0])? {
                    ValueType::Int => self.emit_int_to_str(),
                    ValueType::Bool => self.emit_bool_to_str(),
                    ValueType::Str => {}
                    other => {
                        return Err(unsupported(span, &format!("toString of {other:?}")));
                    }
                }
                Ok(Some(ValueType::Str))
            }
            ("substring", 3) => {
                if self.expression(&arguments[0])? != ValueType::Str {
                    return Err(unsupported(span, "substring of a non-string"));
                }
                self.asm.push(Reg::X0);
                if self.expression(&arguments[1])? != ValueType::Int {
                    return Err(unsupported(span, "substring with a non-Int start"));
                }
                self.asm.push(Reg::X0);
                if self.expression(&arguments[2])? != ValueType::Int {
                    return Err(unsupported(span, "substring with a non-Int end"));
                }
                self.asm.mov_reg(Reg::X2, Reg::X0);
                self.asm.pop(Reg::X1);
                self.asm.pop(Reg::X0);
                self.emit_substring();
                Ok(Some(ValueType::Str))
            }
            ("at", 2) => {
                if self.expression(&arguments[0])? != ValueType::Str {
                    return Err(unsupported(span, "at of a non-string"));
                }
                self.asm.push(Reg::X0);
                if self.expression(&arguments[1])? != ValueType::Int {
                    return Err(unsupported(span, "at with a non-Int index"));
                }
                self.asm.mov_reg(Reg::X1, Reg::X0);
                self.asm.pop(Reg::X0);
                // at(s, i) = substring(s, i, i + 1)
                self.asm.add_reg_imm(Reg::X2, Reg::X1, 1);
                self.emit_substring();
                Ok(Some(ValueType::Str))
            }
            ("head", 1) => {
                let ty = self.expression(&arguments[0])?;
                let ValueType::List(elem) = ty else {
                    return Err(unsupported(span, "head of a non-list"));
                };
                let non_empty = self.asm.new_label();
                self.asm
                    .branch(non_empty, BranchKind::CompareNonZero(Reg::X0));
                self.asm
                    .emit_write_rodata(STDERR_FD, b"klassic: head expects a non-empty list\n");
                self.asm.emit_exit(1);
                self.asm.bind(non_empty);
                self.asm.ldr_imm(Reg::X0, Reg::X0, 0);
                Ok(Some(elem_value_type(elem)))
            }
            ("tail", 1) => {
                let ty = self.expression(&arguments[0])?;
                if !matches!(ty, ValueType::List(_) | ValueType::EmptyList) {
                    return Err(unsupported(span, "tail of a non-list"));
                }
                // The evaluator's tail([]) is [] — nil stays nil.
                let end = self.asm.new_label();
                self.asm.branch(end, BranchKind::CompareZero(Reg::X0));
                self.asm.ldr_imm(Reg::X0, Reg::X0, 8);
                self.asm.bind(end);
                Ok(Some(ty))
            }
            ("isEmpty", 1) => {
                let ty = self.expression(&arguments[0])?;
                if !matches!(ty, ValueType::List(_) | ValueType::EmptyList) {
                    return Err(unsupported(span, "isEmpty of a non-list"));
                }
                self.asm.cmp_imm(Reg::X0, 0);
                self.asm.cset(Reg::X0, Cond::Eq);
                Ok(Some(ValueType::Bool))
            }
            ("size", 1) => {
                let ty = self.expression(&arguments[0])?;
                if !matches!(
                    ty,
                    ValueType::List(_)
                        | ValueType::EmptyList
                        | ValueType::Set(_)
                        | ValueType::EmptySet
                ) {
                    return Err(unsupported(span, "size of a non-collection"));
                }
                // Both lists and sets are cons-cell chains, so the
                // length walk is identical.
                let count_loop = self.asm.new_label();
                let done = self.asm.new_label();
                self.asm.mov_reg(Reg::X1, Reg::X0);
                self.asm.mov_imm64(Reg::X0, 0);
                self.asm.bind(count_loop);
                self.asm.branch(done, BranchKind::CompareZero(Reg::X1));
                self.asm.ldr_imm(Reg::X1, Reg::X1, 8);
                self.asm.add_reg_imm(Reg::X0, Reg::X0, 1);
                self.asm.branch(count_loop, BranchKind::Unconditional);
                self.asm.bind(done);
                Ok(Some(ValueType::Int))
            }
            ("contains", 2) => {
                let set_ty = self.expression(&arguments[0])?;
                let elem = match set_ty {
                    ValueType::Set(elem) => Some(elem),
                    ValueType::EmptySet => None,
                    _ => return Err(unsupported(span, "contains on a non-set")),
                };
                self.asm.push(Reg::X0); // the set survives the candidate eval
                let candidate_ty = self.expression(&arguments[1])?;
                self.asm.mov_reg(Reg::X1, Reg::X0); // candidate
                self.asm.pop(Reg::X0); // set head
                match elem {
                    Some(elem) => {
                        if list_elem_of(candidate_ty) != Some(elem) {
                            return Err(unsupported(span, "contains with a mismatched element"));
                        }
                        let member = self.member_label(elem);
                        self.asm.branch(member, BranchKind::Link);
                    }
                    // Nothing is a member of the empty set.
                    None => self.asm.mov_imm64(Reg::X0, 0),
                }
                Ok(Some(ValueType::Bool))
            }
            // ---- enum-lowering primitives (`desugar_enums`) ----
            // `__gc_record(n)` / `__gc_alloc(bytes)`: zeroed heap
            // memory from the bump allocator (segments are fresh mmap
            // pages, so zeroing is free by construction).
            ("__gc_record", 1) | ("__gc_alloc", 1) => {
                if self.expression(&arguments[0])? != ValueType::Int {
                    return Err(unsupported(span, &format!("{name} with a non-Int size")));
                }
                if name == "__gc_record" {
                    // n pointer slots → n * 8 bytes.
                    self.asm.lsl_imm(Reg::X4, Reg::X0, 3);
                } else {
                    self.asm.add_reg_imm(Reg::X4, Reg::X0, 7);
                    self.asm.lsr_imm(Reg::X4, Reg::X4, 3);
                    self.asm.lsl_imm(Reg::X4, Reg::X4, 3);
                }
                self.emit_alloc();
                self.asm.mov_reg(Reg::X0, Reg::X5);
                Ok(Some(ValueType::Ptr))
            }
            ("__gc_write", 3) => {
                if self.expression(&arguments[0])? != ValueType::Ptr {
                    return Err(unsupported(span, "__gc_write to a non-pointer"));
                }
                self.asm.push(Reg::X0);
                if self.expression(&arguments[1])? != ValueType::Int {
                    return Err(unsupported(span, "__gc_write with a non-Int offset"));
                }
                self.asm.push(Reg::X0);
                if self.expression(&arguments[2])? == ValueType::Unit {
                    return Err(unsupported(span, "__gc_write of unit"));
                }
                self.asm.mov_reg(Reg::X2, Reg::X0);
                self.asm.pop(Reg::X1);
                self.asm.pop(Reg::X0);
                self.asm.str_reg_offset(Reg::X2, Reg::X0, Reg::X1);
                Ok(Some(ValueType::Unit))
            }
            ("__gc_read", 2) | ("__gc_read_ptr", 2) | ("__gc_read_string", 2) => {
                if self.expression(&arguments[0])? != ValueType::Ptr {
                    return Err(unsupported(span, &format!("{name} of a non-pointer")));
                }
                self.asm.push(Reg::X0);
                if self.expression(&arguments[1])? != ValueType::Int {
                    return Err(unsupported(span, &format!("{name} with a non-Int offset")));
                }
                self.asm.mov_reg(Reg::X1, Reg::X0);
                self.asm.pop(Reg::X0);
                self.asm.ldr_reg_offset(Reg::X0, Reg::X0, Reg::X1);
                Ok(Some(match name {
                    "__gc_read" => ValueType::Int,
                    "__gc_read_string" => ValueType::Str,
                    _ => ValueType::Ptr,
                }))
            }
            // On this backend rodata literals and heap strings are
            // already the same one-pointer layout; normalization is
            // the identity until a tracing collector exists.
            ("__gc_string", 1) => {
                if self.expression(&arguments[0])? != ValueType::Str {
                    return Err(unsupported(span, "__gc_string of a non-string"));
                }
                Ok(Some(ValueType::Str))
            }
            ("__match_fail", 0) => {
                self.asm
                    .emit_write_rodata(STDERR_FD, b"klassic: match: no pattern matched\n");
                self.asm.emit_exit(1);
                Ok(Some(ValueType::Never))
            }
            _ => Ok(None),
        }
    }

    /// Call a top-level annotated function: arguments are evaluated
    /// left to right onto the machine stack, then popped into the
    /// AAPCS64 argument registers right to left.
    fn function_call(
        &mut self,
        name: &str,
        arguments: &[Expr],
        span: Span,
    ) -> Result<ValueType, Diagnostic> {
        // rposition: a later (user) definition shadows an earlier
        // (prelude) one, matching evaluator scoping.
        let Some(index) = self.functions.iter().rposition(|(n, _)| n == name) else {
            return Err(unsupported(span, &format!("function `{name}`")));
        };
        let (label, params, ret) = {
            let info = &self.functions[index].1;
            (info.label, info.params.clone(), info.ret)
        };
        if arguments.len() != params.len() {
            return Err(Diagnostic::compile(
                span,
                format!(
                    "{name} expects {} arguments but got {}",
                    params.len(),
                    arguments.len()
                ),
            ));
        }
        if arguments.len() > ARG_REGS.len() {
            return Err(unsupported(span, "calls with more than 8 arguments"));
        }
        for (argument, (_, expected)) in arguments.iter().zip(params.iter()) {
            let ty = self.expression(argument)?;
            if !assignable(ty, *expected) {
                return Err(unsupported(argument.span(), "an argument of this type"));
            }
            self.asm.push(Reg::X0);
        }
        for register in ARG_REGS.iter().take(arguments.len()).rev() {
            self.asm.pop(*register);
        }
        self.asm.branch(label, BranchKind::Link);
        self.pending.push(index);
        Ok(ret)
    }

    /// Emit one collected function: prologue saves the frame record
    /// and binds parameters to frame-pointer slots, the body is a
    /// single expression whose value stays in x0.
    fn emit_function(&mut self, index: usize) -> Result<(), Diagnostic> {
        let (label, params, ret, body) = {
            let info = &self.functions[index].1;
            (info.label, info.params.clone(), info.ret, info.body.clone())
        };
        self.asm.bind(label);
        self.asm.push_frame_record();
        let frame_size = ((params.len() as u32 + count_var_decls(&body)) * 8).div_ceil(16) * 16;
        if frame_size >= 4096 {
            return Err(unsupported(body.span(), "this many local variables"));
        }
        if frame_size > 0 {
            self.asm.sub_sp_imm(frame_size);
        }
        self.asm.mov_fp_sp();

        self.scopes.push(HashMap::new());
        let saved_offset = self.next_local_offset;
        self.next_local_offset = 0;
        for (position, (param, ty)) in params.iter().enumerate() {
            let offset = self.declare_local(param, *ty);
            self.asm.store_local(ARG_REGS[position], offset);
        }
        let body_ty = self.expression(&body)?;
        self.scopes.pop();
        self.next_local_offset = saved_offset;
        if !assignable(body_ty, ret) {
            return Err(unsupported(
                body.span(),
                "a function body of a different type than its return annotation",
            ));
        }

        if frame_size > 0 {
            self.asm.add_sp_imm(frame_size);
        }
        self.asm.pop_frame_record();
        self.asm.ret();
        Ok(())
    }

    /// Render a `println` argument that is a compile-time literal to
    /// its evaluator-identical line. Doubles go through Rust's `f64`
    /// Display, the same formatter the evaluator and klassic_rt use.
    fn literal_line(argument: &Expr) -> Result<Option<String>, Diagnostic> {
        match argument {
            Expr::String { value, span } => {
                if value.contains("#{") {
                    return Err(unsupported(*span, "string interpolation"));
                }
                Ok(Some(format!("{value}\n")))
            }
            Expr::Double { value, .. } => Ok(Some(format!("{value}\n"))),
            _ => Ok(None),
        }
    }

    fn println_call(&mut self, arguments: &[Expr], span: Span) -> Result<(), Diagnostic> {
        if arguments.len() != 1 {
            return Err(Diagnostic::compile(
                span,
                format!("println expects 1 argument but got {}", arguments.len()),
            ));
        }
        let argument = &arguments[0];
        if let Some(line) = Self::literal_line(argument)? {
            self.asm.emit_write_rodata(STDOUT_FD, line.as_bytes());
            return Ok(());
        }
        match self.expression(argument)? {
            ValueType::Int => {
                self.asm.emit_print_int_line();
                Ok(())
            }
            ValueType::Str => {
                self.emit_println_str();
                Ok(())
            }
            ValueType::List(elem) => {
                self.emit_println_list(elem);
                Ok(())
            }
            ValueType::EmptyList => {
                self.asm.emit_write_rodata(STDOUT_FD, b"[]\n");
                Ok(())
            }
            ValueType::Set(elem) => {
                self.emit_println_set(elem);
                Ok(())
            }
            ValueType::EmptySet => {
                self.asm.emit_write_rodata(STDOUT_FD, b"%()\n");
                Ok(())
            }
            ValueType::Record(index) => {
                self.emit_println_record(index, argument.span())?;
                Ok(())
            }
            ValueType::Bool => {
                let false_label = self.asm.new_label();
                let end_label = self.asm.new_label();
                self.asm
                    .branch(false_label, BranchKind::CompareZero(Reg::X0));
                self.asm.emit_write_rodata(STDOUT_FD, b"true\n");
                self.asm.branch(end_label, BranchKind::Unconditional);
                self.asm.bind(false_label);
                self.asm.emit_write_rodata(STDOUT_FD, b"false\n");
                self.asm.bind(end_label);
                Ok(())
            }
            other => Err(unsupported(
                argument.span(),
                &format!("printing a {other:?} value"),
            )),
        }
    }

    fn statement(&mut self, expr: &Expr) -> Result<(), Diagnostic> {
        match expr {
            Expr::Block { expressions, .. } => {
                self.scopes.push(HashMap::new());
                for expression in expressions {
                    self.statement(expression)?;
                }
                self.scopes.pop();
                Ok(())
            }
            // Declarations have no runtime effect in the current
            // subset; calling a declared function is rejected at the
            // call site.
            Expr::ModuleHeader { .. }
            | Expr::Import { .. }
            | Expr::DefDecl { .. }
            | Expr::RecordDeclaration { .. } => Ok(()),
            Expr::VarDecl { name, value, .. } => {
                let ty = self.expression(value)?;
                if ty == ValueType::Unit {
                    return Err(unsupported(value.span(), "a unit-typed binding"));
                }
                let offset = self.declare_local(name, ty);
                self.asm.store_local(Reg::X0, offset);
                Ok(())
            }
            Expr::Assign { name, value, span } => {
                let Some((offset, expected)) = self.lookup(name) else {
                    return Err(unsupported(*span, &format!("assignment to `{name}`")));
                };
                let ty = self.expression(value)?;
                if !assignable(ty, expected) {
                    return Err(unsupported(*span, "assignment changing a type"));
                }
                self.asm.store_local(Reg::X0, offset);
                Ok(())
            }
            Expr::While {
                condition, body, ..
            } => {
                let loop_label = self.asm.new_label();
                let end_label = self.asm.new_label();
                self.asm.bind(loop_label);
                let condition_ty = self.expression(condition)?;
                if condition_ty != ValueType::Bool {
                    return Err(unsupported(condition.span(), "a non-Bool condition"));
                }
                self.asm.branch(end_label, BranchKind::CompareZero(Reg::X0));
                self.statement(body)?;
                self.asm.branch(loop_label, BranchKind::Unconditional);
                self.asm.bind(end_label);
                Ok(())
            }
            Expr::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                let condition_ty = self.expression(condition)?;
                if condition_ty != ValueType::Bool {
                    return Err(unsupported(condition.span(), "a non-Bool condition"));
                }
                let else_label = self.asm.new_label();
                let end_label = self.asm.new_label();
                self.asm
                    .branch(else_label, BranchKind::CompareZero(Reg::X0));
                self.statement(then_branch)?;
                self.asm.branch(end_label, BranchKind::Unconditional);
                self.asm.bind(else_label);
                if let Some(else_branch) = else_branch {
                    self.statement(else_branch)?;
                }
                self.asm.bind(end_label);
                Ok(())
            }
            Expr::Call {
                callee, arguments, ..
            } if matches!(callee.as_ref(), Expr::Identifier { name, .. } if name == "println") => {
                self.println_call(arguments, expr.span())
            }
            other => {
                // An expression in statement position: evaluate for
                // effect (the subset's expressions are pure, but a
                // discarded value is harmless and keeps parity with
                // the C backend).
                self.expression(other)?;
                Ok(())
            }
        }
    }
}

/// Count every local the program can declare so the frame can be
/// reserved once up front (slots are never reused; fine at this
/// scale). The recursion mirrors exactly the expression shapes the
/// code generator walks — the enum lowering plants `val`s inside
/// expression-position blocks — so it never undercounts a compilable
/// program; anything it cannot see fails compilation before a slot
/// is touched.
fn count_var_decls(expr: &Expr) -> u32 {
    match expr {
        Expr::VarDecl { value, .. } => 1 + count_var_decls(value),
        Expr::Assign { value, .. } => count_var_decls(value),
        Expr::Block { expressions, .. } => expressions.iter().map(count_var_decls).sum(),
        Expr::While {
            condition, body, ..
        } => count_var_decls(condition) + count_var_decls(body),
        Expr::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            count_var_decls(condition)
                + count_var_decls(then_branch)
                + else_branch
                    .as_ref()
                    .map_or(0, |branch| count_var_decls(branch))
        }
        Expr::Binary { lhs, rhs, .. } => count_var_decls(lhs) + count_var_decls(rhs),
        Expr::Call {
            callee, arguments, ..
        } => count_var_decls(callee) + arguments.iter().map(count_var_decls).sum::<u32>(),
        Expr::ListLiteral { elements, .. } => elements.iter().map(count_var_decls).sum(),
        // A set literal reserves one frame slot for its accumulator,
        // plus whatever its elements declare.
        Expr::SetLiteral { elements, .. } => 1 + elements.iter().map(count_var_decls).sum::<u32>(),
        Expr::RecordConstructor { arguments, .. } => arguments.iter().map(count_var_decls).sum(),
        Expr::RecordLiteral { fields, .. } => {
            fields.iter().map(|(_, value)| count_var_decls(value)).sum()
        }
        Expr::FieldAccess { target, .. } => count_var_decls(target),
        _ => 0,
    }
}

/// Register every monomorphic, fully annotated top-level record
/// declaration. Two passes so records can reference each other in any
/// order; declarations the subset cannot type (generics, function
/// fields) are skipped and produce a use-site diagnostic instead.
fn collect_records(expr: &Expr, emitter: &mut Emitter) {
    let Expr::Block { expressions, .. } = expr else {
        return;
    };
    // Pass 1: names, so field annotations can reference any record.
    for expression in expressions {
        if let Expr::RecordDeclaration {
            name, type_params, ..
        } = expression
            && type_params.is_empty()
        {
            // Provisionally usable so pass 2 can resolve mutual
            // references; refined below.
            emitter.records.push(RecordInfo {
                name: name.clone(),
                fields: Vec::new(),
                usable: true,
            });
        }
    }
    // Pass 2: field types; undecodable declarations are marked
    // unusable (indices must stay stable).
    for expression in expressions {
        let Expr::RecordDeclaration {
            name,
            type_params,
            fields,
            span,
        } = expression
        else {
            continue;
        };
        if !type_params.is_empty() {
            continue;
        }
        let mut typed = Vec::with_capacity(fields.len());
        let mut usable = true;
        for field in fields {
            let Some(annotation) = &field.annotation else {
                usable = false;
                break;
            };
            let Ok(ty) = emitter.annotation_type(&annotation.text, *span) else {
                usable = false;
                break;
            };
            typed.push((field.name.clone(), ty));
        }
        let index = emitter
            .records
            .iter()
            .position(|record| record.name == *name)
            .expect("registered in pass 1");
        emitter.records[index].fields = typed;
        emitter.records[index].usable = usable;
    }
}

/// Register every fully annotated top-level `def` as a callable
/// function. Defs with missing or unsupported annotations are skipped
/// silently — the stdlib prelude is full of them — and calling one
/// produces the call-site diagnostic instead.
fn collect_functions(expr: &Expr, emitter: &mut Emitter) {
    let Expr::Block { expressions, .. } = expr else {
        return;
    };
    for expression in expressions {
        let Expr::DefDecl {
            name,
            params,
            param_annotations,
            return_annotation,
            body,
            span,
            ..
        } = expression
        else {
            continue;
        };
        let mut signature = Vec::with_capacity(params.len());
        for (param, annotation) in params.iter().zip(param_annotations.iter()) {
            let Some(annotation) = annotation else {
                signature.clear();
                break;
            };
            let Ok(ty) = emitter.annotation_type(&annotation.text, *span) else {
                signature.clear();
                break;
            };
            signature.push((param.clone(), ty));
        }
        if signature.len() != params.len() {
            continue;
        }
        let Some(return_annotation) = return_annotation else {
            continue;
        };
        let Ok(ret) = emitter.annotation_type(&return_annotation.text, *span) else {
            continue;
        };
        let label = emitter.asm.new_label();
        emitter.functions.push((
            name.clone(),
            FunctionInfo {
                label,
                params: signature,
                ret,
                body: body.as_ref().clone(),
            },
        ));
    }
}

/// Compile the whole program to a signed Mach-O arm64 executable.
/// `lowered_enums` names the enums `desugar_enums` already lowered to
/// `__gc_record` shape.
pub(crate) fn emit_macho_program(
    expr: &Expr,
    lowered_enums: std::collections::HashSet<String>,
) -> Result<Vec<u8>, Diagnostic> {
    let mut emitter = Emitter {
        lowered_enums,
        ..Emitter::default()
    };
    emitter.scopes.push(HashMap::new());
    collect_records(expr, &mut emitter);
    collect_functions(expr, &mut emitter);

    // Empty heap: the first allocation's capacity check fails and
    // mmaps the first segment.
    emitter.asm.mov_imm64(Reg::X19, 0);
    emitter.asm.mov_imm64(Reg::X20, 0);

    let frame_size = (count_var_decls(expr) * 8).div_ceil(16) * 16;
    if frame_size >= 4096 {
        return Err(unsupported(expr.span(), "this many local variables"));
    }
    if frame_size > 0 {
        emitter.asm.sub_sp_imm(frame_size);
    }
    emitter.asm.mov_fp_sp();

    emitter.statement(expr)?;
    emitter.asm.emit_exit(0);

    // Emit reached functions; their bodies may reach more.
    let mut emitted = vec![false; emitter.functions.len()];
    while let Some(index) = emitter.pending.pop() {
        if !emitted[index] {
            emitted[index] = true;
            emitter.emit_function(index)?;
        }
    }
    // Set helpers before heap-grow: the reverse routine allocates, so
    // it can be what first references the heap-grow routine.
    if let Some(label) = emitter.member_scalar_label {
        emitter.emit_member_scalar_routine(label);
    }
    if let Some(label) = emitter.member_string_label {
        emitter.emit_member_string_routine(label);
    }
    if let Some(label) = emitter.list_reverse_label {
        emitter.emit_list_reverse_routine(label);
    }
    if let Some(label) = emitter.heap_grow_label {
        emitter.emit_heap_grow_routine(label);
    }

    emitter.asm.finish();
    Ok(macho::write_executable(
        emitter.asm.code,
        emitter.asm.rodata,
        &emitter.asm.fixups,
        "klassic",
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn words(asm: &Assembler) -> Vec<u32> {
        asm.code
            .chunks_exact(4)
            .map(|chunk| u32::from_le_bytes(chunk.try_into().unwrap()))
            .collect()
    }

    #[test]
    fn encodes_reference_instructions() {
        let mut asm = Assembler::default();
        asm.movz(Reg::X0, 1, 0);
        asm.movz(Reg::X16, 4, 0);
        asm.movk(Reg::X2, 0xbeef, 1);
        asm.svc_0x80();
        asm.add_reg(Reg::X0, Reg::X0, Reg::X1);
        asm.sub_reg(Reg::X2, Reg::X3, Reg::X4);
        asm.mul_reg(Reg::X0, Reg::X0, Reg::X1);
        asm.sdiv_reg(Reg::X0, Reg::X0, Reg::X1);
        asm.cmp_reg(Reg::X0, Reg::X1);
        asm.cset(Reg::X0, Cond::Lt);
        asm.mov_reg(Reg::X1, Reg::X0);
        asm.push(Reg::X0);
        asm.pop(Reg::X1);
        asm.store_local(Reg::X0, 16);
        asm.load_local(Reg::X2, 8);
        assert_eq!(
            words(&asm),
            vec![
                0xd280_0020, // movz x0, #1
                0xd280_0090, // movz x16, #4
                0xf2b7_dde2, // movk x2, #0xbeef, lsl #16
                0xd400_1001, // svc #0x80
                0x8b01_0000, // add x0, x0, x1
                0xcb04_0062, // sub x2, x3, x4
                0x9b01_7c00, // mul x0, x0, x1
                0x9ac1_0c00, // sdiv x0, x0, x1
                0xeb01_001f, // cmp x0, x1
                0x9a9f_a7e0, // cset x0, lt
                0xaa00_03e1, // mov x1, x0
                0xf81f_0fe0, // str x0, [sp, #-16]!
                0xf841_07e1, // ldr x1, [sp], #16
                0xf900_0ba0, // str x0, [x29, #16]
                0xf940_07a2, // ldr x2, [x29, #8]
            ]
        );
    }

    #[test]
    fn branches_resolve_forward_and_backward() {
        let mut asm = Assembler::default();
        let back = asm.new_label();
        let forward = asm.new_label();
        asm.bind(back);
        asm.movz(Reg::X0, 0, 0);
        asm.branch(forward, BranchKind::CompareZero(Reg::X0));
        asm.branch(back, BranchKind::Unconditional);
        asm.bind(forward);
        asm.movz(Reg::X0, 1, 0);
        asm.finish();
        let words = words(&asm);
        // cbz x0, +2 words
        assert_eq!(words[1], 0xb400_0000 | (2 << 5));
        // b -2 words (back to offset 0 from offset 8)
        assert_eq!(words[2], 0x1400_0000 | (0x03ff_ffff & (-2i32 as u32)));
    }

    #[test]
    fn mov_imm64_skips_zero_halfwords() {
        let mut asm = Assembler::default();
        asm.mov_imm64(Reg::X2, 13);
        assert_eq!(words(&asm), vec![0xd280_01a2]); // movz x2, #13 only

        let mut asm = Assembler::default();
        asm.mov_imm64(Reg::X0, 0x1_0000);
        assert_eq!(
            words(&asm),
            vec![0xd280_0000, 0xf2a0_0020] // movz x0, #0 ; movk x0, #1, lsl #16
        );
    }

    #[test]
    fn write_sequence_records_one_fixup_per_string() {
        let mut asm = Assembler::default();
        asm.emit_write_rodata(STDOUT_FD, b"hello\n");
        asm.emit_write_rodata(STDOUT_FD, b"bye\n");
        assert_eq!(asm.fixups.len(), 2);
        assert_eq!(asm.fixups[0].data_offset, 0);
        assert_eq!(asm.fixups[1].data_offset, 6);
        assert_eq!(asm.rodata, b"hello\nbye\n");
        // Each fixup names an adrp immediately followed by its add.
        for fixup in &asm.fixups {
            assert_eq!(fixup.add_offset, fixup.adrp_offset + 4);
            let adrp = u32::from_le_bytes(
                asm.code[fixup.adrp_offset..fixup.adrp_offset + 4]
                    .try_into()
                    .unwrap(),
            );
            assert_eq!(adrp & 0x9f00_001f, 0x9000_0001); // adrp x1, 0
        }
    }
}
