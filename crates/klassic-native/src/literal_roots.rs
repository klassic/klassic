//! Permanent data slots share the production RootCell/barrier contract.
use super::{DataLabel, NativeCodeGenerator, NativeValue, Reg, TextLabel};

impl NativeCodeGenerator {
    /// Typed scratch output: heap values get permanent cells before generated
    /// user code runs. Scalar bits never decide whether this is a root.
    pub(super) fn new_literal_scalar_slot(&mut self, value: NativeValue) -> DataLabel {
        let slot = self.asm.data_label_with_i64s(&[0]);
        if self.gc_gen.is_some()
            && matches!(value, NativeValue::HeapPointer | NativeValue::HeapString)
        {
            self.literal_root_cells.push(slot);
        }
        slot
    }

    /// The old data load preserved flags. Root barriers must preserve that
    /// contract as well as the other registers, including a live Rax operand.
    pub(super) fn emit_load_literal_slot(&mut self, dst: Reg, slot: DataLabel) {
        let load = self.gc_gen.as_ref().and_then(|runtime| {
            self.literal_root_cells
                .contains(&slot)
                .then_some(runtime.labels.root_load)
        });
        let Some(load) = load else {
            self.asm.mov_data_addr(Reg::R10, slot);
            self.asm.load_ptr_disp32(dst, Reg::R10, 0);
            return;
        };
        self.asm.bytes(&[0x9c]); // pushfq
        if dst != Reg::Rax {
            self.asm.push_reg(Reg::Rax);
        }
        self.asm.mov_data_addr(Reg::R10, slot);
        self.asm.load_ptr_disp32(Reg::Rax, Reg::R10, 0);
        self.asm.call_label(load);
        if dst != Reg::Rax {
            self.asm.mov_reg_reg(dst, Reg::Rax);
            self.asm.pop_reg(Reg::Rax);
        }
        self.asm.bytes(&[0x9d]); // popfq
    }

    /// Rax = raw payload, R10 = data slot address. Return true when the
    /// permanent cell owns this store; SATB and resolution run before publish.
    pub(super) fn emit_store_literal_root(&mut self, slot: DataLabel) -> bool {
        let store = self.gc_gen.as_ref().and_then(|runtime| {
            self.literal_root_cells
                .contains(&slot)
                .then_some(runtime.labels.root_store)
        });
        let Some(store) = store else {
            return false;
        };
        self.asm.bytes(&[0x9c]);
        self.asm.push_reg(Reg::Rcx);
        self.asm.mov_reg_reg(Reg::Rcx, Reg::R10);
        self.asm.call_label(store);
        self.asm.pop_reg(Reg::Rcx);
        self.asm.bytes(&[0x9d]);
        true
    }

    pub(super) fn emit_generational_literal_seed(&mut self, entry: TextLabel) -> bool {
        let Some(runtime) = &self.gc_gen else {
            return false;
        };
        let (adopt, literals) = (runtime.labels.root_adopt, runtime.labels.literals);
        self.asm.bind_text_label(entry);
        for cell in self.literal_root_cells.clone() {
            self.asm.mov_data_addr(Reg::Rax, cell);
            self.asm.call_label(adopt);
        }
        self.asm.ret();
        // The common permanent segment includes literal cells in its captured
        // mark/relocation bounds. No x86 literal walk runs in either pause.
        self.asm.bind_text_label(literals);
        self.asm.ret();
        true
    }
}
