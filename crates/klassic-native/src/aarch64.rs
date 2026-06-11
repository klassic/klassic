//! Direct AArch64 backend for `aarch64-apple-darwin` (Apple Silicon
//! macOS). Like the x86_64 ELF backend it emits machine code and the
//! executable container directly — no `cc` / `as` / `ld` / `codesign`
//! — and like that backend's early history it starts from a small
//! vertical slice and grows: the current subset compiles top-level
//! `println` of literals (strings without interpolation, integers,
//! booleans, doubles) to Darwin `write` syscalls and exits cleanly.
//! Everything else fails with a source-located diagnostic, never
//! wrong code. The code is position-independent (`adrp`+`add` for
//! data, no absolute addresses) because the kernel slides `MH_PIE`
//! images.
//!
//! Darwin arm64 syscall convention: number in `x16`, arguments in
//! `x0..`, trap via `svc #0x80`. BSD numbers: `exit` = 1, `write` = 4.

use klassic_span::{Diagnostic, Span};
use klassic_syntax::Expr;

use crate::macho::{self, DataFixup};

const SYS_EXIT: u16 = 1;
const SYS_WRITE: u16 = 4;
const STDOUT_FD: u16 = 1;

/// AArch64 general-purpose registers used by the subset. The numeric
/// value is the register number in instruction encodings.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Reg {
    X0 = 0,
    X1 = 1,
    X2 = 2,
    /// Darwin syscall number register.
    X16 = 16,
}

#[derive(Default)]
struct Assembler {
    code: Vec<u8>,
    rodata: Vec<u8>,
    fixups: Vec<DataFixup>,
}

impl Assembler {
    fn word(&mut self, instruction: u32) {
        self.code.extend_from_slice(&instruction.to_le_bytes());
    }

    /// `movz xd, #imm16, lsl #(16 * shift)`
    fn movz(&mut self, reg: Reg, imm16: u16, shift: u32) {
        self.word(0xd280_0000 | (shift << 21) | (u32::from(imm16) << 5) | reg as u32);
    }

    /// `movk xd, #imm16, lsl #(16 * shift)`
    fn movk(&mut self, reg: Reg, imm16: u16, shift: u32) {
        self.word(0xf280_0000 | (shift << 21) | (u32::from(imm16) << 5) | reg as u32);
    }

    /// Materialize an arbitrary 64-bit constant.
    fn mov_imm64(&mut self, reg: Reg, value: u64) {
        self.movz(reg, value as u16, 0);
        for shift in 1..4u32 {
            let part = (value >> (16 * shift)) as u16;
            if part != 0 {
                self.movk(reg, part, shift);
            }
        }
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

    /// write(1, <rodata bytes>, len)
    fn emit_write_rodata(&mut self, bytes: &[u8]) {
        let data_offset = self.intern_rodata(bytes);
        self.mov_imm64(Reg::X0, u64::from(STDOUT_FD));
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
}

fn unsupported(span: Span, feature: &str) -> Diagnostic {
    Diagnostic::compile(
        span,
        format!("{feature} is not supported by the aarch64-apple-darwin backend yet"),
    )
}

/// Render a `println` argument that is a compile-time literal to its
/// evaluator-identical line. Doubles go through Rust's `f64` Display,
/// the same formatter the evaluator and klassic_rt use.
fn literal_line(argument: &Expr) -> Result<Option<String>, Diagnostic> {
    match argument {
        Expr::String { value, span } => {
            if value.contains("#{") {
                return Err(unsupported(*span, "string interpolation"));
            }
            Ok(Some(format!("{value}\n")))
        }
        Expr::Int { value, .. } => Ok(Some(format!("{value}\n"))),
        Expr::Bool { value, .. } => Ok(Some(format!("{value}\n"))),
        Expr::Double { value, .. } => Ok(Some(format!("{value}\n"))),
        _ => Ok(None),
    }
}

fn emit_statement(expr: &Expr, asm: &mut Assembler) -> Result<(), Diagnostic> {
    match expr {
        Expr::Block { expressions, .. } => {
            for expression in expressions {
                emit_statement(expression, asm)?;
            }
            Ok(())
        }
        // Declarations have no runtime effect in the current subset;
        // calling a declared function is rejected at the call site.
        Expr::ModuleHeader { .. } | Expr::Import { .. } | Expr::DefDecl { .. } => Ok(()),
        Expr::Call {
            callee, arguments, ..
        } if matches!(callee.as_ref(), Expr::Identifier { name, .. } if name == "println") => {
            if arguments.len() != 1 {
                return Err(Diagnostic::compile(
                    expr.span(),
                    format!("println expects 1 argument but got {}", arguments.len()),
                ));
            }
            match literal_line(&arguments[0])? {
                Some(line) => {
                    asm.emit_write_rodata(line.as_bytes());
                    Ok(())
                }
                None => Err(unsupported(
                    arguments[0].span(),
                    "printing a non-literal expression",
                )),
            }
        }
        other => Err(unsupported(other.span(), "this expression")),
    }
}

/// Compile the whole program to a signed Mach-O arm64 executable.
pub(crate) fn emit_macho_program(expr: &Expr) -> Result<Vec<u8>, Diagnostic> {
    let mut asm = Assembler::default();
    emit_statement(expr, &mut asm)?;
    asm.emit_exit(0);
    Ok(macho::write_executable(
        asm.code,
        asm.rodata,
        &asm.fixups,
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
        assert_eq!(
            words(&asm),
            vec![
                0xd280_0020, // movz x0, #1
                0xd280_0090, // movz x16, #4
                0xf2b7_dde2, // movk x2, #0xbeef, lsl #16
                0xd400_1001, // svc #0x80
            ]
        );
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
        asm.emit_write_rodata(b"hello\n");
        asm.emit_write_rodata(b"bye\n");
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
