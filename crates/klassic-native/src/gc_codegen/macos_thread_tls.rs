//! Actual pthread TLS key ownership, with normalized C-int status returns.
use super::{Assembler, BranchKind as B, Cond, Label, Reg as R};
use crate::gc_codegen::production::ThreadTlsOsLabels;
use crate::macho::ImportSymbol as I;
use crate::portable_asm::PortableAsm;

pub(super) fn emit(a: &mut Assembler, l: &ThreadTlsOsLabels<Label>) {
    for (op, label) in [
        (0, l.create),
        (1, l.get),
        (2, l.set),
        (3, l.delete),
        (4, l.identity),
    ] {
        a.bind(label);
        a.native_entry_begin(64);
        a.mov_reg(R::X20, R::X5); // request, retained through pthread calls
        if op == 4 {
            a.call_import(I::PthreadSelf);
            a.str_imm(R::X0, R::X20, 16);
            let valid = a.new_label();
            a.cmp_imm(R::X0, 0);
            a.branch(valid, B::Conditional(Cond::Ne));
            a.mov_imm64(R::X0, 6);
            let done = a.new_label();
            a.branch(done, B::Unconditional);
            a.bind(valid);
            a.mov_imm64(R::X0, 0);
            a.bind(done);
            a.native_entry_end();
            a.ret();
            continue;
        }
        if op == 0 {
            a.mov_reg(R::X0, R::X20);
            a.mov_imm64(R::X1, 0); // explicit detach; no unsafe async destructor
        } else {
            a.ldr_imm(R::X0, R::X20, 0);
            if op == 2 {
                a.ldr_imm(R::X1, R::X20, 8);
            }
        }
        a.call_import(match op {
            0 => I::PthreadKeyCreate,
            1 => I::PthreadGetSpecific,
            2 => I::PthreadSetSpecific,
            _ => I::PthreadKeyDelete,
        });
        if op == 1 {
            a.str_imm(R::X0, R::X20, 8);
            a.call_import(I::PthreadSelf);
            a.str_imm(R::X0, R::X20, 16);
            a.mov_imm64(R::X0, 0);
        } else {
            a.word(0x2a00_03e0); // mov w0,w0: ignore unspecified high int bits
            let done = a.new_label();
            a.cmp_imm(R::X0, 0);
            a.branch(done, B::Conditional(Cond::Eq));
            a.mov_imm64(R::X0, 6);
            a.bind(done);
        }
        a.native_entry_end();
        a.ret();
    }
}
