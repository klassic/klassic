//! `--gc=generational` on the direct AArch64 Mach-O backend.
//!
//! The generational runtime (`gc_codegen::production`) is written over
//! `PortableAsm`, so it emits into this backend's `Assembler` unchanged --
//! with one exception handled by `FlagAsm` below: the routines were written
//! against x86-64, where `sub`/`and`/`or`/shifts set the flags a following
//! `jcc` reads. AArch64's plain forms do not, so `FlagAsm` restores the x86
//! flag contract for exactly this emission (the legacy collector keeps the
//! plain `Assembler` impl, so default output is unchanged).
//!
//! Roots differ from x86 on purpose. The x86 backend routes every frame slot
//! through a RootCell (`root_adopt`/`root_load`/`root_store`). This backend
//! already roots frame slots by *address* on its shadow stack and re-reads a
//! slot after every allocation, because the legacy collector moves objects.
//! The generational collector reaches those slots through the backend
//! `literals` hook: it visits every shadow-stack entry with
//! `forward_cell_entry`, which evacuates the referent and rewrites the slot
//! in place. Heap fields go through `field_store` (bound at the legacy
//! `gc_store_pointer` label) and `field_load` (the barrier trampoline).
use super::{Assembler, Emitter, Label, PortDataAddr, Reg};
use crate::gc_codegen::production::{self, Backend, Data, Labels};
use crate::portable_asm::{Condition, PortableAsm, Reg as P};

/// Darwin `munmap`.
const SYS_MUNMAP: u64 = 73;

/// The generational routines' labels and data, present under the flag.
pub(super) struct GenRuntime {
    labels: Labels<Label>,
    data: Data<PortDataAddr>,
    /// (legacy label every call site uses, generational routine) pairs:
    /// call sites rely on the legacy routines keeping x1-x12, which the
    /// generational ones do not promise, so a wrapper keeps them.
    wrapped: [(Label, Label); 3],
}

impl Emitter {
    /// Create the labels and data. `alloc`, `field_store`, `deep_equal` are
    /// the legacy labels every call site already uses.
    pub(super) fn prepare_generational(&mut self) {
        use production::{FAIL_TEXT, OLD_FULL_TEXT, OOM_TEXT, TOO_LARGE_TEXT};
        let map = self.asm.new_label();
        let legacy = [
            self.gc_alloc_entry_label(),
            self.gc_store_pointer_entry(),
            self.gc_deep_equal_label(),
        ];
        let inner = [
            self.asm.new_label(),
            self.asm.new_label(),
            self.asm.new_label(),
        ];
        let alloc = inner[0];
        let backend = Backend {
            field_store: inner[1],
            deep_equal: inner[2],
            literals: self.asm.new_label(),
        };
        let asm = &mut self.asm;
        let labels = Labels::new(|| asm.new_label(), map, alloc, backend);
        let runtime = PortDataAddr::Bss(self.asm.reserve_data_cells(1));
        let mut text = |bytes: &[u8]| PortDataAddr::Rodata(self.asm.intern_rodata(bytes));
        let data = Data {
            runtime,
            fail_text: text(FAIL_TEXT),
            oom_text: text(OOM_TEXT),
            too_large_text: text(TOO_LARGE_TEXT),
            old_full_text: text(OLD_FULL_TEXT),
        };
        let wrapped = [0, 1, 2].map(|i| (legacy[i], inner[i]));
        self.gc_gen = Some(GenRuntime {
            labels,
            data,
            wrapped,
        });
    }

    /// Startup under the flag: map the generational runtime and the shadow
    /// stack (the only legacy table still in use).
    pub(super) fn emit_generational_startup(&mut self, shadow_stack: PortDataAddr) {
        let startup = self.gen_labels().startup;
        PortableAsm::call_label(&mut self.asm, startup);
        let bytes = (crate::gc_layout::GC_SHADOW_STACK_LEN * 8) as u64;
        self.asm.mov_imm64(Reg::X0, 0);
        self.asm.mov_imm64(Reg::X1, bytes);
        self.asm.mov_imm64(Reg::X2, super::PROT_READ_WRITE);
        self.asm.mov_imm64(Reg::X3, super::MMAP_ANON_PRIVATE);
        self.asm.mov_imm64(Reg::X4, u64::MAX);
        self.asm.mov_imm64(Reg::X5, 0);
        self.asm.mov_imm64(Reg::X16, u64::from(super::SYS_MMAP));
        self.asm.svc_0x80();
        self.asm
            .emit_abort_if_syscall_failed(b"klassic gc: mmap failed\n");
        let PortDataAddr::Bss(cell) = shadow_stack else {
            unreachable!("GC cells live in __bss");
        };
        self.asm.load_data_address(Reg::X10, cell);
        self.asm.str_imm(Reg::X0, Reg::X10, 0);
    }

    pub(super) fn gen_labels(&self) -> &Labels<Label> {
        &self
            .gc_gen
            .as_ref()
            .expect("generational runtime prepared")
            .labels
    }

    /// The heap-field load under the flag: x8 = field address, x0 = the
    /// loaded word on entry and the payload address on exit.
    pub(super) fn emit_generational_field_load(&mut self) {
        self.asm.ldr_imm(Reg::X0, Reg::X8, 0);
        let wrapper = self.barrier_trampoline_label();
        self.asm.push_frame_record();
        self.asm.branch(wrapper, super::BranchKind::Link);
        self.asm.pop_frame_record();
    }

    /// `emit_read_fd_to_string` under the flag (x0 = fd -> x0 = string).
    /// The kernel reads into a native mapping rather than a heap object --
    /// a megabyte buffer is larger than a generational page, and no managed
    /// address should reach the kernel -- then only the bytes read are
    /// copied into the result string and the mapping is released.
    pub(super) fn emit_generational_read_fd_to_string(&mut self, cap: u64) {
        let map = self.gen_labels().map;
        self.asm.push(Reg::X0); // fd
        self.asm.mov_imm64(Reg::X5, cap);
        PortableAsm::call_label(&mut self.asm, map);
        let mapped = self.asm.new_label();
        self.asm
            .branch(mapped, super::BranchKind::CompareNonZero(Reg::X0));
        self.asm
            .emit_write_rodata(super::STDERR_FD, b"klassic gc: mmap failed\n");
        self.asm.emit_exit(1);
        self.asm.bind(mapped);
        self.asm.pop(Reg::X3); // fd
        self.asm.push(Reg::X0); // buffer
        self.asm.push(Reg::X3);
        self.asm.mov_reg(Reg::X1, Reg::X0);
        self.asm.mov_reg(Reg::X0, Reg::X3);
        self.asm.mov_imm64(Reg::X2, cap);
        self.asm.mov_imm64(Reg::X16, u64::from(super::SYS_READ));
        self.asm.svc_0x80();
        self.asm
            .emit_abort_if_syscall_failed(b"klassic: FileInput#all failed to read file\n");
        self.asm.pop(Reg::X3); // fd
        self.asm.push(Reg::X0); // bytes read
        self.asm.mov_reg(Reg::X0, Reg::X3);
        self.asm.mov_imm64(Reg::X16, u64::from(super::SYS_CLOSE));
        self.asm.svc_0x80();
        self.asm
            .emit_abort_if_syscall_failed(b"klassic: FileInput#all failed to close file\n");
        self.asm.pop(Reg::X2);
        self.asm.push(Reg::X2);
        self.emit_alloc_raw_string(Reg::X2); // x5 = result, x2 kept
        self.asm.pop(Reg::X2); // bytes read
        self.asm.pop(Reg::X6); // buffer
        self.asm.push(Reg::X6);
        self.asm.str_imm(Reg::X2, Reg::X5, 0);
        self.asm.add_reg_imm(Reg::X7, Reg::X5, 8);
        self.emit_copy_bytes(Reg::X2, Reg::X6, Reg::X7, Reg::X3);
        self.asm.pop(Reg::X0); // buffer
        self.asm.mov_imm64(Reg::X1, cap);
        self.asm.mov_imm64(Reg::X16, SYS_MUNMAP);
        self.asm.svc_0x80();
        self.asm.mov_reg(Reg::X0, Reg::X5);
    }

    /// `--gc-log`: publish/verify, then one line of runtime statistics.
    pub(super) fn emit_generational_log_report(&mut self) {
        let verify = self.gen_labels().verify;
        let PortDataAddr::Bss(cell) = self.gc_gen.as_ref().expect("prepared").data.runtime else {
            unreachable!("the runtime cell lives in __bss");
        };
        PortableAsm::call_label(&mut self.asm, verify);
        let stats = production::stat_offsets();
        self.asm
            .emit_write_rodata(super::STDERR_FD, b"gc[generational]:");
        for (name, offset) in [
            (&b" collections="[..], stats.collections),
            (&b" full_collections="[..], stats.full_collections),
            (&b" healed="[..], stats.healed),
            (&b" promoted_bytes="[..], stats.promoted),
            (&b" allocated_bytes="[..], stats.allocated_bytes),
            (&b" pages_used="[..], stats.pages_used),
            (&b" objects="[..], stats.objects),
            (&b" published="[..], stats.published),
        ] {
            self.asm.emit_write_rodata(super::STDERR_FD, name);
            self.asm.load_data_address(Reg::X0, cell);
            self.asm.ldr_imm(Reg::X0, Reg::X0, 0);
            PortableAsm::load_ptr_disp32(&mut self.asm, P::V0, P::V0, offset);
            self.asm.sub_sp_imm(32);
            self.asm.emit_int_digits(false);
            self.asm.add_reg_sp_imm(Reg::X2, 32);
            self.asm.sub_reg(Reg::X2, Reg::X2, Reg::X1);
            self.asm.mov_imm64(Reg::X0, super::STDERR_FD);
            self.asm.mov_imm64(Reg::X16, u64::from(super::SYS_WRITE));
            self.asm.svc_0x80();
            self.asm.add_sp_imm(32);
        }
        self.asm.emit_write_rodata(super::STDERR_FD, b"\n");
    }

    /// Emit the runtime, the Darwin mapping leaf and the root hook.
    pub(super) fn emit_generational_runtime(
        &mut self,
        shadow_stack: PortDataAddr,
        shadow_top: PortDataAddr,
    ) {
        let runtime = self.gc_gen.take().expect("generational runtime prepared");
        let l = &runtime.labels;
        emit_map(&mut self.asm, l.map);
        for (legacy, inner) in runtime.wrapped {
            emit_keep_registers(&mut self.asm, legacy, inner);
        }
        emit_root_hook(&mut self.asm, l, shadow_stack, shadow_top);
        let mut flags = FlagAsm(&mut self.asm);
        production::emit(
            &mut flags,
            l,
            &runtime.data,
            super::STDERR_FD,
            self.gc_stress,
        );
        self.gc_gen = Some(runtime);
    }
}

/// `entry`: call `target` keeping x1-x12 (x0 carries the result, or is
/// kept by the target).
fn emit_keep_registers(asm: &mut Assembler, entry: Label, target: Label) {
    const KEPT: [Reg; 12] = [
        Reg::X1,
        Reg::X2,
        Reg::X3,
        Reg::X4,
        Reg::X5,
        Reg::X6,
        Reg::X7,
        Reg::X8,
        Reg::X9,
        Reg::X10,
        Reg::X11,
        Reg::X12,
    ];
    asm.bind(entry);
    asm.push_frame_record();
    for reg in KEPT {
        asm.push(reg);
    }
    asm.branch(target, super::BranchKind::Link);
    for reg in KEPT.into_iter().rev() {
        asm.pop(reg);
    }
    asm.pop_frame_record();
    asm.ret();
}

/// `(x5 = bytes) -> x0 = zeroed anonymous mapping, or 0`. Darwin reports a
/// failed `mmap` with the carry flag. Preserves x3 (the runtime pointer the
/// caller may hold).
fn emit_map(asm: &mut Assembler, entry: Label) {
    let ok = asm.new_label();
    asm.bind(entry);
    asm.push(Reg::X3);
    asm.mov_reg(Reg::X1, Reg::X5);
    asm.mov_imm64(Reg::X0, 0);
    asm.mov_imm64(Reg::X2, super::PROT_READ_WRITE);
    asm.mov_imm64(Reg::X3, super::MMAP_ANON_PRIVATE);
    asm.mov_imm64(Reg::X4, u64::MAX);
    asm.mov_imm64(Reg::X5, 0);
    asm.mov_imm64(Reg::X16, u64::from(super::SYS_MMAP));
    asm.svc_0x80();
    asm.branch(ok, super::BranchKind::Conditional(super::Cond::Cc));
    asm.mov_imm64(Reg::X0, 0);
    asm.bind(ok);
    asm.pop(Reg::X3);
    asm.ret();
}

/// `literals`: every shadow-stack entry is the address of a slot holding a
/// payload address (or null); forward each referent in place. Called from
/// `collect` with x3 = runtime, which `forward_cell_entry` preserves.
fn emit_root_hook(
    asm: &mut Assembler,
    l: &Labels<Label>,
    shadow_stack: PortDataAddr,
    shadow_top: PortDataAddr,
) {
    let a = asm;
    let lp = a.create_text_label();
    let done = a.create_text_label();
    a.bind_text_label(l.literals);
    PortableAsm::mov_imm64(a, P::V0, 0);
    a.bind_text_label(lp);
    a.mov_data_addr(P::V1, shadow_top);
    a.load_ptr_disp32(P::V1, P::V1, 0);
    a.cmp_reg_reg(P::V0, P::V1);
    a.jcc_label(Condition::AboveOrEqual, done);
    a.push_reg(P::V0);
    a.mov_data_addr(P::V1, shadow_stack);
    a.load_ptr_disp32(P::V1, P::V1, 0);
    a.shl_reg_imm8(P::V0, 3);
    a.add_reg_reg(P::V1, P::V0);
    a.load_ptr_disp32(P::V0, P::V1, 0);
    a.call_label(l.forward_cell_entry);
    a.pop_reg(P::V0);
    a.add_reg_imm32(P::V0, 1);
    a.jmp_label(lp);
    a.bind_text_label(done);
    a.ret();
}

/// `PortableAsm` with x86 flag semantics after arithmetic, for the
/// generational routines (see the module comment). `sub` is preceded by the
/// equivalent `cmp`, so every condition matches x86; `add`, logic ops and
/// shifts are followed by `tst d, d`, which gives x86's ZF/SF (and the
/// cleared CF/OF of the logic ops).
struct FlagAsm<'a>(&'a mut Assembler);

impl FlagAsm<'_> {
    fn is_frame(r: P) -> bool {
        matches!(r, P::V4 | P::V5)
    }
    fn settle(&mut self, r: P) {
        if !Self::is_frame(r) {
            self.0.test_reg_reg(r, r);
        }
    }
}

impl PortableAsm for FlagAsm<'_> {
    type TextLabel = Label;
    type DataLabel = PortDataAddr;

    fn create_text_label(&mut self) -> Label {
        self.0.create_text_label()
    }
    fn bind_text_label(&mut self, label: Label) {
        self.0.bind_text_label(label);
    }
    fn jmp_label(&mut self, label: Label) {
        self.0.jmp_label(label);
    }
    fn jcc_label(&mut self, cond: Condition, label: Label) {
        self.0.jcc_label(cond, label);
    }
    fn call_label(&mut self, label: Label) {
        self.0.call_label(label);
    }
    fn call_native_unary(&mut self, target: P) {
        self.0.call_native_unary(target);
    }
    fn call_native_binary(&mut self, target: P) {
        self.0.call_native_binary(target);
    }
    fn ret(&mut self) {
        PortableAsm::ret(self.0);
    }
    fn leave(&mut self) {
        self.0.leave();
    }
    fn push_reg(&mut self, r: P) {
        self.0.push_reg(r);
    }
    fn pop_reg(&mut self, r: P) {
        self.0.pop_reg(r);
    }
    fn mov_reg_reg(&mut self, dst: P, src: P) {
        self.0.mov_reg_reg(dst, src);
    }
    fn mov_imm64(&mut self, dst: P, imm: u64) {
        PortableAsm::mov_imm64(self.0, dst, imm);
    }
    fn mov_data_addr(&mut self, dst: P, label: PortDataAddr) {
        self.0.mov_data_addr(dst, label);
    }
    fn load_ptr_disp32(&mut self, dst: P, base: P, disp: i32) {
        self.0.load_ptr_disp32(dst, base, disp);
    }
    fn store_ptr_disp32(&mut self, base: P, disp: i32, src: P) {
        self.0.store_ptr_disp32(base, disp, src);
    }
    fn atomic_load_acquire(&mut self, dst: P, address: P) {
        self.0.atomic_load_acquire(dst, address);
    }
    fn atomic_store_release(&mut self, address: P, value: P) {
        self.0.atomic_store_release(address, value);
    }
    fn atomic_compare_exchange(&mut self, address: P, desired: P) {
        self.0.atomic_compare_exchange(address, desired);
    }
    fn atomic_load_acquire_u32(&mut self, dst: P, address: P) {
        self.0.atomic_load_acquire_u32(dst, address);
    }
    fn atomic_store_release_u32(&mut self, address: P, value: P) {
        self.0.atomic_store_release_u32(address, value);
    }
    fn atomic_compare_exchange_u32(&mut self, address: P, desired: P) {
        self.0.atomic_compare_exchange_u32(address, desired);
    }
    fn atomic_fence(&mut self) {
        self.0.atomic_fence();
    }
    fn load_rbp_slot(&mut self, dst: P, offset: i32) {
        self.0.load_rbp_slot(dst, offset);
    }
    fn store_rbp_slot(&mut self, offset: i32, src: P) {
        self.0.store_rbp_slot(offset, src);
    }
    fn add_reg_reg(&mut self, dst: P, src: P) {
        self.0.add_reg_reg(dst, src);
        self.settle(dst);
    }
    fn sub_reg_reg(&mut self, dst: P, src: P) {
        self.0.cmp_reg_reg(dst, src);
        self.0.sub_reg_reg(dst, src);
    }
    fn and_reg_reg(&mut self, dst: P, src: P) {
        self.0.and_reg_reg(dst, src);
        self.settle(dst);
    }
    fn or_reg_reg(&mut self, dst: P, src: P) {
        self.0.or_reg_reg(dst, src);
        self.settle(dst);
    }
    fn xor_reg_reg(&mut self, dst: P, src: P) {
        self.0.xor_reg_reg(dst, src);
        self.settle(dst);
    }
    fn cmp_reg_reg(&mut self, a: P, b: P) {
        self.0.cmp_reg_reg(a, b);
    }
    fn test_reg_reg(&mut self, a: P, b: P) {
        self.0.test_reg_reg(a, b);
    }
    fn add_reg_imm32(&mut self, dst: P, imm: i32) {
        self.0.add_reg_imm32(dst, imm);
        self.settle(dst);
    }
    fn sub_reg_imm8(&mut self, r: P, imm: i8) {
        if !Self::is_frame(r) {
            self.0.cmp_reg_imm32(r, i32::from(imm));
        }
        self.0.sub_reg_imm8(r, imm);
    }
    fn sub_reg_imm32(&mut self, r: P, imm: i32) {
        if !Self::is_frame(r) {
            self.0.cmp_reg_imm32(r, imm);
        }
        self.0.sub_reg_imm32(r, imm);
    }
    fn and_reg_imm32(&mut self, r: P, imm: i32) {
        self.0.and_reg_imm32(r, imm);
        self.settle(r);
    }
    fn cmp_reg_imm8(&mut self, r: P, imm: i8) {
        self.0.cmp_reg_imm8(r, imm);
    }
    fn cmp_reg_imm32(&mut self, r: P, imm: i32) {
        self.0.cmp_reg_imm32(r, imm);
    }
    fn shl_reg_imm8(&mut self, r: P, imm: u8) {
        self.0.shl_reg_imm8(r, imm);
        self.settle(r);
    }
    fn shr_reg_imm8(&mut self, r: P, imm: u8) {
        self.0.shr_reg_imm8(r, imm);
        self.settle(r);
    }
    fn shl_reg_count(&mut self, r: P) {
        self.0.shl_reg_count(r);
        self.settle(r);
    }
    fn shr_reg_count(&mut self, r: P) {
        self.0.shr_reg_count(r);
        self.settle(r);
    }
    fn mul_unsigned_wide(&mut self, rhs: P) {
        self.0.mul_unsigned_wide(rhs);
    }
    fn div_unsigned(&mut self, rhs: P) {
        self.0.div_unsigned(rhs);
    }
    fn bt_reg_imm8(&mut self, r: P, bit: u8) {
        self.0.bt_reg_imm8(r, bit);
    }
    fn rep_movsb(&mut self) {
        self.0.rep_movsb();
    }
    fn plat_write_data(&mut self, fd: u64, data: PortDataAddr, len: usize) {
        self.0.plat_write_data(fd, data, len);
    }
    fn plat_exit(&mut self, code: u64) {
        self.0.plat_exit(code);
    }
    fn plat_read_monotonic_ns(&mut self) {
        self.0.plat_read_monotonic_ns();
    }
}
