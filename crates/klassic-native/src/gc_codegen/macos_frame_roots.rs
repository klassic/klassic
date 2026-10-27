//! Precise AArch64 compiler roots. Managed frame/scratch slots contain real
//! RootCell pointers; every read/store goes through the production root ABI.
//! Wrappers preserve backend temporaries and NZCV, like the replaced ldr/str.
use super::{Assembler, Emitter, Label, Reg};
use crate::portable_asm::PortableAsm;

#[derive(Clone, Copy)]
pub(super) struct Labels {
    adopt: Label,
    load: Label,
    store: Label,
    pub(super) pop: Label,
}
impl Labels {
    pub(super) fn new(a: &mut Assembler) -> Self {
        Self {
            adopt: a.new_label(),
            load: a.new_label(),
            store: a.new_label(),
            pop: a.new_label(),
        }
    }
}

pub(super) fn emit(
    a: &mut Assembler,
    entries: Labels,
    l: &crate::gc_codegen::production::Labels<Label>,
) {
    for (kind, label, entry) in [
        (0, entries.adopt, l.root_adopt),
        (1, entries.load, l.root_load),
        (2, entries.store, l.root_store),
        (3, entries.pop, l.root_pop),
    ] {
        a.bind(label);
        if kind == 3 {
            a.push(Reg::X16);
        }
        for reg in [Reg::X0, Reg::X1, Reg::X10, Reg::X11, Reg::X12, Reg::X17] {
            a.push(reg);
        }
        match kind {
            0 => a.mov_reg(Reg::X0, Reg::X16),
            1 => a.ldr_imm(Reg::X0, Reg::X16, 0),
            2 => {
                a.mov_reg(Reg::X0, Reg::X17);
                a.mov_reg(Reg::X1, Reg::X16);
            }
            _ => {}
        }
        a.word(0xd53b_4200 | 17); // mrs x17, NZCV
        a.push(Reg::X17);
        PortableAsm::call_label(a, entry);
        if kind == 1 {
            a.mov_reg(Reg::X16, Reg::X0);
        }
        a.pop(Reg::X17);
        a.word(0xd51b_4200 | 17); // msr NZCV, x17
        for reg in [Reg::X17, Reg::X12, Reg::X11, Reg::X10, Reg::X1, Reg::X0] {
            a.pop(reg);
        }
        if kind == 3 {
            a.pop(Reg::X16);
        }
        a.ret();
    }
}

impl Emitter {
    pub(super) fn frame_slot_is_root(&self, offset: u32) -> bool {
        self.scope_root_slots[self.root_frame_base..]
            .iter()
            .any(|slots| slots.contains(&offset))
    }
    pub(super) fn close_root_scope(&mut self) {
        let roots = self.scope_root_slots.pop().expect("emitter root scope");
        self.emit_shadow_pop(roots.len());
    }
    pub(super) fn load_local(&mut self, reg: Reg, offset: u32) {
        if self.gc_gen.is_some() && self.frame_slot_is_root(offset) {
            self.load_root_address(reg, Reg::X29, offset);
        } else {
            self.asm.load_local(reg, offset);
        }
    }
    pub(super) fn store_local(&mut self, reg: Reg, offset: u32) {
        if let Some(runtime) = &self.gc_gen
            && self.frame_slot_is_root(offset)
        {
            let entry = runtime.frame_roots.store;
            self.asm.push(Reg::X16);
            self.asm.push(Reg::X17);
            self.asm.mov_reg(Reg::X17, reg);
            self.asm.add_reg_imm(Reg::X16, Reg::X29, offset);
            PortableAsm::call_label(&mut self.asm, entry);
            self.asm.pop(Reg::X17);
            self.asm.pop(Reg::X16);
        } else {
            self.asm.store_local(reg, offset);
        }
    }
    pub(super) fn adopt_root_address(&mut self, base: Reg, offset: u32) {
        let entry = self.gc_gen.as_ref().unwrap().frame_roots.adopt;
        self.asm.push(Reg::X16);
        self.asm.add_reg_imm(Reg::X16, base, offset);
        PortableAsm::call_label(&mut self.asm, entry);
        self.asm.pop(Reg::X16);
    }
    pub(super) fn load_root_address(&mut self, reg: Reg, base: Reg, offset: u32) {
        if let Some(runtime) = &self.gc_gen {
            let entry = runtime.frame_roots.load;
            if reg != Reg::X16 {
                self.asm.push(Reg::X16);
            }
            self.asm.add_reg_imm(Reg::X16, base, offset);
            PortableAsm::call_label(&mut self.asm, entry);
            self.asm.mov_reg(reg, Reg::X16);
            if reg != Reg::X16 {
                self.asm.pop(Reg::X16);
            }
        } else {
            self.asm.ldr_imm(reg, base, offset);
        }
    }
    pub(super) fn push_cell_root(&mut self, reg: Reg) {
        let entry = self.gc_gen.as_ref().unwrap().frame_roots.adopt;
        self.asm.push(reg);
        self.asm.push(Reg::X16);
        self.asm.add_reg_sp_imm(Reg::X16, 16);
        PortableAsm::call_label(&mut self.asm, entry);
        self.asm.pop(Reg::X16);
    }
    pub(super) fn peek_cell_root(&mut self, reg: Reg) {
        let entry = self.gc_gen.as_ref().unwrap().frame_roots.load;
        self.asm.push(Reg::X16);
        self.asm.add_reg_sp_imm(Reg::X16, 16);
        PortableAsm::call_label(&mut self.asm, entry);
        self.asm.mov_reg(reg, Reg::X16);
        if reg == Reg::X16 {
            self.asm.add_sp_imm(16);
        } else {
            self.asm.pop(Reg::X16);
        }
    }
}
