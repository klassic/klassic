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
const STDOUT_FD: u64 = 1;
const STDERR_FD: u64 = 2;

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

    /// Print the signed integer in x0 followed by a newline: digits
    /// are decomposed into a 32-byte stack buffer back to front, then
    /// written with one syscall. Clobbers x0-x5/x16.
    fn emit_print_int_line(&mut self) {
        self.sub_sp_imm(32);
        self.add_reg_sp_imm(Reg::X1, 32); // one past the buffer end
        self.mov_imm64(Reg::X3, b'\n' as u64);
        self.store_byte_pre_decrement(Reg::X3, Reg::X1);
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

/// The value types the current subset can hold in a register / local.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ValueType {
    Int,
    Bool,
    Unit,
}

fn value_type_from_annotation(text: &str, span: Span) -> Result<ValueType, Diagnostic> {
    match text.trim() {
        "Int" | "Long" | "Short" | "Byte" => Ok(ValueType::Int),
        "Bool" | "Boolean" => Ok(ValueType::Bool),
        "Unit" => Ok(ValueType::Unit),
        other => Err(unsupported(span, &format!("type annotation `{other}`"))),
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
            Expr::Bool { value, .. } => {
                self.asm.mov_imm64(Reg::X0, u64::from(*value));
                Ok(ValueType::Bool)
            }
            Expr::Identifier { name, span } => {
                let Some((offset, ty)) = self.lookup(name) else {
                    return Err(unsupported(*span, &format!("identifier `{name}`")));
                };
                self.asm.load_local(Reg::X0, offset);
                Ok(ty)
            }
            Expr::Binary { lhs, op, rhs, span } => self.binary(lhs, *op, rhs, *span),
            Expr::Call {
                callee,
                arguments,
                span,
            } => {
                let Expr::Identifier { name, .. } = callee.as_ref() else {
                    return Err(unsupported(*span, "calling a non-identifier"));
                };
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
                if then_ty != else_ty {
                    return Err(unsupported(*span, "if branches with different types"));
                }
                Ok(then_ty)
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
            if ty != *expected {
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
        if body_ty != ret {
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
            ValueType::Unit => Err(unsupported(argument.span(), "printing unit")),
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
            Expr::ModuleHeader { .. } | Expr::Import { .. } | Expr::DefDecl { .. } => Ok(()),
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
                if ty != expected {
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
/// scale). The recursion mirrors exactly the statement positions the
/// code generator walks, so it never undercounts a compilable
/// program.
fn count_var_decls(expr: &Expr) -> u32 {
    match expr {
        Expr::VarDecl { .. } => 1,
        Expr::Block { expressions, .. } => expressions.iter().map(count_var_decls).sum(),
        Expr::While { body, .. } => count_var_decls(body),
        Expr::If {
            then_branch,
            else_branch,
            ..
        } => {
            count_var_decls(then_branch)
                + else_branch
                    .as_ref()
                    .map_or(0, |branch| count_var_decls(branch))
        }
        _ => 0,
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
            let Ok(ty) = value_type_from_annotation(&annotation.text, *span) else {
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
        let Ok(ret) = value_type_from_annotation(&return_annotation.text, *span) else {
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
pub(crate) fn emit_macho_program(expr: &Expr) -> Result<Vec<u8>, Diagnostic> {
    let mut emitter = Emitter::default();
    emitter.scopes.push(HashMap::new());
    collect_functions(expr, &mut emitter);

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
