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
//! Frame/scratch roots and rooted temporaries use the shared RootCell ABI.
//! Backend-local wrappers preserve NZCV and registers across its entries.
//! Registered roots are discovered by budgeted worker frontiers; the literals
//! hook is empty and no raw shadow table is allocated under the flag.
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
    worker_storage: super::DataLabel,
    pub(super) frame_roots: super::gc_frame_roots::Labels,
}

impl Emitter {
    /// Keep backend temporaries and Darwin's syscall error carry across the
    /// shared state transition. No owner poll/adoption occurs in this scope.
    pub(super) fn emit_generational_native_transition(&mut self, entering: bool) {
        if self.gc_gen.is_none() {
            return;
        }
        for r in [Reg::X10, Reg::X11, Reg::X12] {
            self.asm.push(r);
        }
        if !entering {
            self.asm.cset(Reg::X12, super::Cond::Hs);
            self.asm.push(Reg::X12);
        }
        let labels = self.gen_labels();
        let entry = if entering {
            labels.native_enter
        } else {
            labels.native_exit
        };
        PortableAsm::call_label(&mut self.asm, entry);
        if !entering {
            self.asm.pop(Reg::X12);
            self.asm.cmp_imm(Reg::X12, 1); // restore syscall carry, not GC flags
        }
        for r in [Reg::X12, Reg::X11, Reg::X10] {
            self.asm.pop(r);
        }
    }

    /// FileOutput receives a native path and rooted content. After open, copy
    /// to a temporary native mapping; only that mapping reaches write(2).
    /// The sole owner cannot retire the borrowed source before this returns.
    pub(super) fn emit_generational_file_content(&mut self) {
        self.asm.push(Reg::X3); // fd
        self.asm.push(Reg::X4); // borrowed content; no managed allocation/poll
        self.asm.ldr_imm(Reg::X5, Reg::X4, 0);
        let sized = self.asm.new_label();
        self.asm
            .branch(sized, super::BranchKind::CompareNonZero(Reg::X5));
        self.asm.mov_imm64(Reg::X5, 1); // mmap must also support an empty string
        self.asm.bind(sized);
        self.asm.push(Reg::X5);
        let map = self.gen_labels().map;
        PortableAsm::call_label(&mut self.asm, map);
        self.asm.pop(Reg::X2); // mapping length
        self.asm.pop(Reg::X4);
        self.asm.pop(Reg::X3);
        let mapped = self.asm.new_label();
        self.asm
            .branch(mapped, super::BranchKind::CompareNonZero(Reg::X0));
        self.asm
            .emit_write_rodata(super::STDERR_FD, b"klassic gc: mmap failed\n");
        self.asm.emit_exit(1);
        self.asm.bind(mapped);
        self.asm.push(Reg::X0); // native buffer
        self.asm.push(Reg::X2); // mapping length
        self.asm.push(Reg::X3); // fd
        self.asm.ldr_imm(Reg::X2, Reg::X4, 0);
        self.asm.push(Reg::X2); // content length
        self.asm.add_reg_imm(Reg::X6, Reg::X4, 8);
        self.asm.mov_reg(Reg::X7, Reg::X0);
        self.emit_copy_bytes(Reg::X2, Reg::X6, Reg::X7, Reg::X3);
        self.asm.pop(Reg::X2);
        self.asm.pop(Reg::X3);
        self.asm.mov_reg(Reg::X1, Reg::X0);
        self.asm.mov_reg(Reg::X0, Reg::X3);
        self.asm.push(Reg::X3);
        self.emit_generational_native_transition(true);
        self.asm.mov_imm64(Reg::X16, u64::from(super::SYS_WRITE));
        self.asm.svc_0x80();
        self.emit_generational_native_transition(false);
        self.asm
            .emit_abort_if_syscall_failed(b"klassic: FileOutput#write failed to write file\n");
        self.asm.pop(Reg::X0); // fd
        self.emit_generational_native_transition(true);
        self.asm.mov_imm64(Reg::X16, u64::from(super::SYS_CLOSE));
        self.asm.svc_0x80();
        self.emit_generational_native_transition(false);
        self.asm
            .emit_abort_if_syscall_failed(b"klassic: FileOutput#write failed to close file\n");
        self.asm.pop(Reg::X1); // mapping length
        self.asm.pop(Reg::X0); // native buffer
        self.asm.mov_imm64(Reg::X16, SYS_MUNMAP);
        self.asm.svc_0x80();
        self.asm.mov_imm64(Reg::X0, 0);
    }

    /// Create the labels and data. `alloc`, `field_store`, `deep_equal` are
    /// the legacy labels every call site already uses.
    pub(super) fn prepare_generational(&mut self) {
        use production::{
            FAIL_TEXT, METADATA_OOM_TEXT, NATIVE_OOM_TEXT, OLD_FULL_TEXT, OOM_TEXT,
            RELOCATION_OOM_TEXT, TOO_LARGE_TEXT,
        };
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
            raw_roots: false,
        };
        let asm = &mut self.asm;
        let hooks = production::WorkerHooks {
            spawn: asm.new_label(),
            park: asm.new_label(),
            unpark: asm.new_label(),
            relax: asm.new_label(),
            clock: asm.new_label(),
            join: Some(asm.new_label()),
        };
        let labels = Labels::new(|| asm.new_label(), map, alloc, backend)
            .with_worker(hooks, || asm.new_label());
        let frame_roots = super::gc_frame_roots::Labels::new(&mut self.asm);
        let worker_storage = self.asm.reserve_data_cells(17);
        let runtime = PortDataAddr::Bss(self.asm.reserve_data_cells(1));
        let mut text = |bytes: &[u8]| PortDataAddr::Rodata(self.asm.intern_rodata(bytes));
        let data = Data {
            runtime,
            fail_text: text(FAIL_TEXT),
            oom_text: text(OOM_TEXT),
            native_oom_text: text(NATIVE_OOM_TEXT),
            heap_commit_failed_text: text(production::HEAP_COMMIT_FAILED_TEXT),
            large_pages_failed_text: text(production::LARGE_PAGES_FAILED_TEXT),
            heap_decommit_failed_text: text(production::HEAP_DECOMMIT_FAILED_TEXT),
            metadata_release_failed_text: text(production::METADATA_RELEASE_FAILED_TEXT),
            metadata_oom_text: text(METADATA_OOM_TEXT),
            metadata_budget_oom_text: text(production::METADATA_BUDGET_OOM_TEXT),
            relocation_oom_text: text(RELOCATION_OOM_TEXT),
            too_large_text: text(TOO_LARGE_TEXT),
            old_full_text: text(OLD_FULL_TEXT),
        };
        let wrapped = [0, 1, 2].map(|i| (legacy[i], inner[i]));
        self.gc_gen = Some(GenRuntime {
            labels,
            data,
            wrapped,
            worker_storage,
            frame_roots,
        });
    }

    /// Startup uses the shared RootCell storage; no raw shadow table is mapped.
    pub(super) fn emit_generational_startup(&mut self, _shadow_stack: PortDataAddr) {
        let startup = self.gen_labels().startup;
        PortableAsm::call_label(&mut self.asm, startup);
        let runtime = self.gc_gen.as_ref().unwrap().data.runtime;
        PortableAsm::mov_data_addr(&mut self.asm, P::V3, runtime);
        PortableAsm::load_ptr_disp32(&mut self.asm, P::V3, P::V3, 0);
        production::metadata::seal_initial(&mut self.asm);
    }

    pub(super) fn gen_labels(&self) -> &Labels<Label> {
        &self
            .gc_gen
            .as_ref()
            .expect("generational runtime prepared")
            .labels
    }

    pub(super) fn emit_generational_worker_shutdown(&mut self) {
        if let Some(runtime) = self.gc_gen.take() {
            production::shutdown_worker(
                &mut FlagAsm(&mut self.asm),
                &runtime.labels,
                runtime.data.runtime,
            );
            self.gc_gen = Some(runtime);
        }
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
    pub(super) fn emit_generational_read_fd_to_string(&mut self, cap: u64, close: bool) {
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
        if !close {
            self.asm.mov_reg(Reg::X9, Reg::X1); // native mapping base
            self.asm.mov_imm64(Reg::X8, 0); // completed bytes
        }
        let again = self.asm.new_label();
        let finished = self.asm.new_label();
        self.asm.bind(again);
        self.emit_generational_native_transition(true);
        self.asm.mov_imm64(Reg::X16, u64::from(super::SYS_READ));
        self.asm.svc_0x80();
        self.emit_generational_native_transition(false);
        self.asm.emit_abort_if_syscall_failed(if close {
            b"klassic: FileInput#all failed to read file\n"
        } else {
            b"klassic: StandardInput failed to read stdin\n"
        });
        if !close {
            self.asm
                .branch(finished, super::BranchKind::CompareZero(Reg::X0));
            self.asm.add_reg(Reg::X8, Reg::X8, Reg::X0);
            self.asm.mov_imm64(Reg::X2, cap);
            self.asm.cmp_reg(Reg::X8, Reg::X2);
            self.asm
                .branch(finished, super::BranchKind::Conditional(super::Cond::Hs));
            self.asm.mov_reg(Reg::X0, Reg::X3);
            self.asm.add_reg(Reg::X1, Reg::X9, Reg::X8);
            self.asm.sub_reg(Reg::X2, Reg::X2, Reg::X8);
            self.asm.branch(again, super::BranchKind::Unconditional);
            self.asm.bind(finished);
            self.asm.mov_reg(Reg::X0, Reg::X8);
        } else {
            self.asm.bind(finished);
        }
        self.asm.pop(Reg::X3); // fd
        self.asm.push(Reg::X0); // bytes read
        if close {
            self.asm.mov_reg(Reg::X0, Reg::X3);
            self.emit_generational_native_transition(true);
            self.asm.mov_imm64(Reg::X16, u64::from(super::SYS_CLOSE));
            self.asm.svc_0x80();
            self.emit_generational_native_transition(false);
            self.asm
                .emit_abort_if_syscall_failed(b"klassic: FileInput#all failed to close file\n");
        }
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

    /// Write one retained record using the cursor in X19.
    fn emit_generational_trace_fields(&mut self, header_bytes: &[u8], fields: &[(&[u8], i32)]) {
        self.asm.emit_write_rodata(super::STDERR_FD, header_bytes);
        for &(name, offset) in fields {
            self.asm.emit_write_rodata(super::STDERR_FD, name);
            self.asm.ldr_imm(Reg::X0, Reg::X19, offset as u32);
            self.asm.sub_sp_imm(32);
            self.asm.emit_int_digits(false);
            self.asm.add_reg_sp_imm(Reg::X2, 32);
            self.asm.sub_reg(Reg::X2, Reg::X2, Reg::X1);
            self.asm.mov_imm64(Reg::X0, super::STDERR_FD);
            self.asm.mov_imm64(Reg::X16, u64::from(super::SYS_WRITE));
            self.asm.svc_0x80();
            self.asm.add_sp_imm(32);
        }
    }

    fn emit_generational_trace_report(&mut self, cell: super::DataLabel) {
        use production::trace;
        if self.gc_log_format != crate::GcLogFormat::Json {
            return;
        }
        self.asm.push(Reg::X19);
        self.asm.push(Reg::X20);
        self.asm.load_data_address(Reg::X19, cell);
        self.asm.ldr_imm(Reg::X19, Reg::X19, 0);
        self.asm.ldr_imm(Reg::X20, Reg::X19, trace::COUNT as u32);
        self.asm
            .mov_imm64(Reg::X0, trace::buffer_offset(&self.asm) as u64);
        self.asm.add_reg(Reg::X19, Reg::X19, Reg::X0);
        let again = self.asm.new_label();
        let done = self.asm.new_label();
        self.asm.bind(again);
        self.asm
            .branch(done, super::BranchKind::CompareZero(Reg::X20));
        let timer = self.asm.new_label();
        let rendezvous = self.asm.new_label();
        let park = self.asm.new_label();
        let written = self.asm.new_label();
        self.asm.ldr_imm(Reg::X0, Reg::X19, 24);
        self.asm.mov_imm64(Reg::X1, production::mutator_time::TAG);
        self.asm.cmp_reg(Reg::X0, Reg::X1);
        self.asm
            .branch(timer, super::BranchKind::Conditional(super::Cond::Eq));
        self.asm
            .mov_imm64(Reg::X1, production::rendezvous_time::TAG);
        self.asm.cmp_reg(Reg::X0, Reg::X1);
        self.asm
            .branch(rendezvous, super::BranchKind::Conditional(super::Cond::Eq));
        self.asm
            .mov_imm64(Reg::X1, production::rendezvous_time::PARK_TAG);
        self.asm.cmp_reg(Reg::X0, Reg::X1);
        self.asm
            .branch(park, super::BranchKind::Conditional(super::Cond::Eq));
        self.emit_generational_trace_fields(trace::HEADER, &trace::FIELDS);
        self.asm.branch(written, super::BranchKind::Unconditional);
        self.asm.bind(timer);
        self.emit_generational_trace_fields(
            production::mutator_time::HEADER,
            &production::mutator_time::FIELDS,
        );
        self.asm.branch(written, super::BranchKind::Unconditional);
        self.asm.bind(rendezvous);
        self.emit_generational_trace_fields(
            production::rendezvous_time::HEADER,
            &production::rendezvous_time::FIELDS,
        );
        self.asm.branch(written, super::BranchKind::Unconditional);
        self.asm.bind(park);
        self.emit_generational_trace_fields(
            production::rendezvous_time::PARK_HEADER,
            &production::rendezvous_time::PARK_FIELDS,
        );
        self.asm.bind(written);
        self.asm.emit_write_rodata(super::STDERR_FD, b"}\n");
        self.asm
            .add_reg_imm(Reg::X19, Reg::X19, trace::RECORD_BYTES as u32);
        self.asm.sub_reg_imm(Reg::X20, Reg::X20, 1);
        self.asm.branch(again, super::BranchKind::Unconditional);
        self.asm.bind(done);
        self.asm.pop(Reg::X20);
        self.asm.pop(Reg::X19);
    }

    pub(super) fn emit_generational_log_report(&mut self) {
        let verify = self.gen_labels().verify;
        let PortDataAddr::Bss(cell) = self.gc_gen.as_ref().expect("prepared").data.runtime else {
            unreachable!("the runtime cell lives in __bss");
        };
        PortableAsm::call_label(&mut self.asm, verify);
        let stats = production::stat_offsets();
        self.emit_generational_trace_report(cell);
        self.asm
            .emit_write_rodata(super::STDERR_FD, self.gc_log_format.header());
        for (name, offset) in [
            (&b" trace_events="[..], stats.trace[0]),
            (&b" trace_dropped="[..], stats.trace[1]),
            (&b" trace_clock_invalid="[..], stats.trace[2]),
            (&b" trace_capacity="[..], stats.trace[3]),
            (&b" native_io_entries="[..], stats.native_io[0]),
            (&b" native_io_exits="[..], stats.native_io[1]),
            (&b" native_copyout_chunks="[..], stats.native_io[2]),
            (&b" native_buffer_writes="[..], stats.native_io[3]),
            (&b" mutator_collect_calls="[..], stats.mutator_time[0]),
            (&b" mutator_collect_valid="[..], stats.mutator_time[1]),
            (&b" mutator_collect_invalid="[..], stats.mutator_time[2]),
            (&b" mutator_collect_total_ns="[..], stats.mutator_time[3]),
            (&b" mutator_collect_max_ns="[..], stats.mutator_time[4]),
            (&b" mutator_collect_last_ns="[..], stats.mutator_time[5]),
            (
                &b" mutator_collect_clock_invalid="[..],
                stats.mutator_time[6],
            ),
            (&b" mutator_collect_last_status="[..], stats.mutator_time[7]),
            (&b" large_allocations="[..], stats.large_object[0]),
            (&b" large_allocated_bytes="[..], stats.large_object[1]),
            (&b" large_allocation_retries="[..], stats.large_object[2]),
            (&b" large_live_pages="[..], stats.large_object[3]),
            (&b" large_reclaimed_pages="[..], stats.large_object[4]),
            (&b" large_scanned_fields="[..], stats.large_object[5]),
            (&b" large_full_cycles="[..], stats.large_object[6]),
            (&b" collections="[..], stats.collections),
            (&b" full_collections="[..], stats.full_collections),
            (&b" healed="[..], stats.healed),
            (&b" promoted_bytes="[..], stats.promoted),
            (&b" allocated_bytes="[..], stats.allocated_bytes),
            (&b" fresh_allocated_bytes="[..], stats.fresh_allocated_bytes),
            (&b" old_prediction_clamps="[..], stats.old_prediction_clamps),
            (&b" young_mark_elapsed_ns="[..], stats.young_mark_elapsed_ns),
            (&b" heap_min_committed_bytes="[..], stats.heap_minimum),
            (&b" heap_initial_committed_bytes="[..], stats.heap_minimum),
            (
                &b" heap_uncommit_delay_ns="[..],
                stats.heap_uncommit_delay_ns,
            ),
            (&b" heap_committed_bytes="[..], stats.heap_committed),
            (&b" heap_peak_committed_bytes="[..], stats.heap_peak),
            (&b" heap_commit_pages="[..], stats.heap_commits),
            (&b" heap_recommit_pages="[..], stats.heap_recommits),
            (&b" heap_uncommit_pages="[..], stats.heap_uncommits),
            (&b" heap_pretouched_bytes="[..], stats.heap_pretouched),
            (&b" heap_large_page_mode="[..], stats.large_pages[0]),
            (&b" heap_reservation_base="[..], stats.large_pages[1]),
            (&b" heap_reservation_bytes="[..], stats.large_pages[2]),
            (
                &b" heap_large_page_advised_bytes="[..],
                stats.large_pages[3],
            ),
            (
                &b" heap_large_page_advice_failures="[..],
                stats.large_pages[4],
            ),
            (
                &b" heap_large_page_collapsed_bytes="[..],
                stats.large_pages[5],
            ),
            (
                &b" heap_large_page_collapse_failures="[..],
                stats.large_pages[6],
            ),
            (&b" heap_capacity_bytes="[..], stats.heap_capacity_bytes),
            (&b" heap_soft_max_bytes="[..], stats.heap_soft_maximum),
            (&b" metadata_limit_bytes="[..], stats.metadata[0]),
            (&b" metadata_initial_bytes="[..], stats.metadata[1]),
            (&b" metadata_charged_bytes="[..], stats.metadata[2]),
            (&b" metadata_committed_bytes="[..], stats.metadata[3]),
            (&b" metadata_pending_bytes="[..], stats.metadata[4]),
            (&b" metadata_peak_committed_bytes="[..], stats.metadata[5]),
            (&b" metadata_maps="[..], stats.metadata[6]),
            (&b" metadata_map_failures="[..], stats.metadata[7]),
            (&b" metadata_unmaps="[..], stats.metadata[8]),
            (&b" metadata_unmap_failures="[..], stats.metadata[9]),
            (&b" metadata_retire_pending="[..], stats.metadata[10]),
            (&b" old_limit_bytes="[..], stats.old_limit_bytes),
            (&b" old_charged_bytes="[..], stats.old_charged_bytes),
            (&b" young_limit_pages="[..], stats.young_limit_pages),
            (&b" old_run_count="[..], stats.old_run_count),
            (&b" old_run_switches="[..], stats.old_run_switches),
            (&b" old_capacity_pages="[..], stats.old_capacity_pages),
            (&b" pool_reduced_old_runs="[..], stats.pool_reduced_old_runs),
            (&b" pool_runs="[..], stats.pool_runs),
            (&b" pool_released_pages="[..], stats.pool_released_pages),
            (&b" pool_role_transfers="[..], stats.pool_role_transfers),
            (&b" young_capacity_pages="[..], stats.young_capacity_pages),
            (&b" young_requested_pages="[..], stats.young_requested_pages),
            (&b" young_protected_bytes="[..], stats.young_protected_bytes),
            (
                &b" young_copy_reserve_pages="[..],
                stats.young_copy_reserve_pages,
            ),
            (&b" young_capacity_clamps="[..], stats.young_capacity_clamps),
            (&b" young_capacity_grows="[..], stats.young_capacity_grows),
            (
                &b" young_capacity_shrinks="[..],
                stats.young_capacity_shrinks,
            ),
            (
                &b" young_capacity_samples="[..],
                stats.young_capacity_samples,
            ),
            (&b" start_target_pages="[..], stats.start_target_pages),
            (
                &b" allocation_bytes_per_ms="[..],
                stats.allocation_bytes_per_ms,
            ),
            (
                &b" young_mark_units_per_ms="[..],
                stats.young_mark_units_per_ms,
            ),
            (&b" start_rate_samples="[..], stats.start_rate_samples),
            (&b" start_mark_samples="[..], stats.start_mark_samples),
            (&b" start_target_updates="[..], stats.start_target_updates),
            (&b" start_invalid_samples="[..], stats.start_invalid_samples),
            (&b" last_mark_work_units="[..], stats.last_mark_work_units),
            (&b" tenuring_threshold="[..], stats.tenuring_threshold),
            (&b" tenuring_frozen="[..], stats.tenuring_frozen),
            (&b" tenuring_updates="[..], stats.tenuring_updates),
            (
                &b" tenuring_young_samples="[..],
                stats.tenuring_young_samples,
            ),
            (&b" tenuring_full_samples="[..], stats.tenuring_full_samples),
            (&b" young_survivor_bytes="[..], stats.young_survivor_bytes),
            (
                &b" young_promoted_winner_bytes="[..],
                stats.young_promoted_winner_bytes,
            ),
            (&b" old_target_bytes="[..], stats.old_target_bytes),
            (&b" old_retained_bytes="[..], stats.old_retained_bytes),
            (&b" old_promotion_ema="[..], stats.old_promotion_ema),
            (&b" old_allocation_budget="[..], stats.old_allocation_budget),
            (&b" old_full_samples="[..], stats.old_full_samples),
            (&b" old_young_samples="[..], stats.old_young_samples),
            (&b" old_target_updates="[..], stats.old_target_updates),
            (&b" old_deferred="[..], stats.old_deferred),
            (&b" old_periodic_requests="[..], stats.old_periodic_requests),
            (&b" old_pressure_requests="[..], stats.old_pressure_requests),
            (&b" pages_used="[..], stats.pages_used),
            (&b" worker_scanned="[..], stats.worker_scanned),
            (&b" worker_copies="[..], stats.worker_copies),
            (&b" mutator_copies="[..], stats.mutator_copies),
            (&b" marked="[..], stats.marked),
            (&b" unmarked_copied="[..], stats.unmarked_copied),
            (&b" young_worker_units="[..], stats.young_worker_units),
            (&b" young_unmarked_copied="[..], stats.young_unmarked_copied),
            (&b" objects="[..], stats.objects),
            (&b" published="[..], stats.published),
            (&b" allocation_stalls="[..], stats.allocation_stalls),
        ] {
            let name = self.gc_log_format.field(name);
            self.asm.emit_write_rodata(super::STDERR_FD, &name);
            if offset == stats.heap_uncommit_delay_ns {
                let digits = PortableAsm::gc_uncommit_delay_ns(&self.asm).to_string();
                self.asm
                    .emit_write_rodata(super::STDERR_FD, digits.as_bytes());
                continue;
            }
            self.asm.load_data_address(Reg::X0, cell);
            self.asm.ldr_imm(Reg::X0, Reg::X0, 0);
            production::load_diagnostic(&mut self.asm, P::V0, P::V0, offset);
            self.asm.sub_sp_imm(32);
            self.asm.emit_int_digits(false);
            self.asm.add_reg_sp_imm(Reg::X2, 32);
            self.asm.sub_reg(Reg::X2, Reg::X2, Reg::X1);
            self.asm.mov_imm64(Reg::X0, super::STDERR_FD);
            self.asm.mov_imm64(Reg::X16, u64::from(super::SYS_WRITE));
            self.asm.svc_0x80();
            self.asm.add_sp_imm(32);
        }
        self.asm
            .emit_write_rodata(super::STDERR_FD, self.gc_log_format.footer());
    }

    /// Emit the runtime, the Darwin mapping leaf and the root hook.
    pub(super) fn emit_generational_runtime(
        &mut self,
        _shadow_stack: PortDataAddr,
        _shadow_top: PortDataAddr,
    ) {
        let runtime = self.gc_gen.take().expect("generational runtime prepared");
        let l = &runtime.labels;
        emit_map(&mut self.asm, l.map);
        emit_heap_memory(&mut self.asm, &l.memory);
        super::gc_macos_thread_tls::emit(&mut self.asm, &l.tls_os);
        for (legacy, inner) in runtime.wrapped {
            emit_keep_registers(&mut self.asm, legacy, inner);
        }
        self.asm.bind(l.literals);
        self.asm.ret();
        super::gc_frame_roots::emit(&mut self.asm, runtime.frame_roots, l);
        super::gc_macos_production::emit(
            &mut self.asm,
            l.worker_hooks().expect("worker hooks"),
            l.worker_entry().expect("worker entry"),
            runtime.worker_storage,
        );
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

/// `PortableAsm` with x86 flag semantics after arithmetic, for the
/// generational routines (see the module comment). SUBS supplies subtraction
/// flags directly. ADDS retains result/overflow flags and its carry is
/// inverted to the no-borrow convention used by portable comparisons.
/// Logical tests set no-borrow (x86 CF=0); shifts supply result ZF/SF for
/// the GC's zero/sign branches. The legacy Assembler stays unchanged.
struct FlagAsm<'a>(&'a mut Assembler);

impl FlagAsm<'_> {
    fn is_frame(r: P) -> bool {
        matches!(r, P::V4 | P::V5)
    }
    fn settle(&mut self, r: P) {
        if !Self::is_frame(r) {
            self.0.test_reg_reg(r, r);
            self.normalize_carry(false);
        }
    }
    // Portable unsigned comparisons use ARM's no-borrow convention. ADDS
    // produces carry instead, while logical instructions clear x86 carry.
    fn normalize_carry(&mut self, invert: bool) {
        self.0.word(0xd53b_420a); // mrs x10,nzcv
        if invert {
            self.0.word(0xd263_014a); // eor x10,x10,#0x20000000
        } else {
            self.0.word(0xb263_014a); // orr x10,x10,#0x20000000
        }
        self.0.word(0xd51b_420a); // msr nzcv,x10
        self.0.bt_pending = false;
    }
}

impl PortableAsm for FlagAsm<'_> {
    type TextLabel = Label;
    type DataLabel = PortDataAddr;

    fn gc_metadata_limit(&self) -> Option<u64> {
        self.0.gc_metadata_limit()
    }
    fn gc_metadata_alignment(&self) -> u64 {
        self.0.gc_metadata_alignment()
    }
    fn gc_external_metadata_bytes(&self) -> u64 {
        self.0.gc_external_metadata_bytes()
    }
    fn gc_timing(&self) -> bool {
        self.0.gc_timing()
    }
    fn gc_json_events(&self) -> bool {
        self.0.gc_json_events()
    }
    fn gc_pre_touch(&self) -> bool {
        self.0.gc_pre_touch()
    }
    fn gc_uncommit_delay_ns(&self) -> u64 {
        self.0.gc_uncommit_delay_ns()
    }
    fn gc_heap_soft_maximum(&self) -> u64 {
        self.0.gc_heap_soft_maximum()
    }
    fn gc_heap_minimum(&self) -> u64 {
        self.0.gc_heap_minimum()
    }
    fn gc_heap_capacity(&self) -> u64 {
        self.0.gc_heap_capacity()
    }
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
    fn native_entry_begin(&mut self, locals: u32) {
        self.0.native_entry_begin(locals);
    }
    fn native_entry_end(&mut self) {
        self.0.native_entry_end();
    }
    fn mov_text_addr(&mut self, dst: P, label: Label) {
        self.0.mov_text_addr(dst, label);
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
        if Self::is_frame(dst) || Self::is_frame(src) {
            self.0.add_reg_reg(dst, src);
        } else {
            let d = super::port_reg(dst) as u32;
            let s = super::port_reg(src) as u32;
            self.0.word(0xab00_0000 | (s << 16) | (d << 5) | d); // adds
            self.normalize_carry(true);
        }
    }
    fn sub_reg_reg(&mut self, dst: P, src: P) {
        let d = super::port_reg(dst) as u32;
        let s = super::port_reg(src) as u32;
        self.0.word(0xeb00_0000 | (s << 16) | (d << 5) | d); // subs
        self.0.bt_pending = false;
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
        self.normalize_carry(false);
    }
    fn add_reg_imm32(&mut self, dst: P, imm: i32) {
        if Self::is_frame(dst) {
            self.0.add_reg_imm32(dst, imm);
        } else {
            let d = super::port_reg(dst) as u32;
            if (0..4096).contains(&imm) {
                self.0
                    .word(0xb100_0000 | ((imm as u32) << 10) | (d << 5) | d);
            } else {
                self.0.mov_imm64(Reg::X10, imm as i64 as u64);
                self.0.word(0xab0a_0000 | (d << 5) | d); // adds dst,dst,x10
            }
            self.normalize_carry(true);
        }
    }
    fn sub_reg_imm8(&mut self, r: P, imm: i8) {
        if r == P::V4 {
            self.0.sub_reg_imm8(r, imm);
        } else {
            self.sub_reg_imm32(r, i32::from(imm));
        }
    }
    fn sub_reg_imm32(&mut self, r: P, imm: i32) {
        if r == P::V4 {
            self.0.sub_sp_imm(imm as u32);
        } else {
            let d = if r == P::V5 {
                29
            } else {
                super::port_reg(r) as u32
            };
            if (0..4096).contains(&imm) {
                self.0
                    .word(0xf100_0000 | ((imm as u32) << 10) | (d << 5) | d);
            } else {
                self.0.mov_imm64(Reg::X10, imm as i64 as u64);
                self.0.word(0xeb0a_0000 | (d << 5) | d); // subs dst,dst,x10
            }
            self.0.bt_pending = false;
        }
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
    fn plat_write_buffer(&mut self) {
        self.0.plat_write_buffer();
    }
    fn plat_sleep_millis(&mut self) {
        self.0.plat_sleep_millis();
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

// Darwin syscall numbers: Apple xnu bsd/kern/syscalls.master (mprotect74,
// madvise75); https://github.com/apple-oss-distributions/xnu/blob/main/bsd/kern/syscalls.master
fn emit_heap_memory(asm: &mut Assembler, l: &production::MemoryLabels<Label>) {
    asm.bind(l.reserve);
    asm.push(Reg::X3);
    asm.mov_reg(Reg::X1, Reg::X5);
    asm.mov_imm64(Reg::X0, 0);
    asm.mov_imm64(Reg::X2, 0);
    asm.mov_imm64(Reg::X3, super::MMAP_ANON_PRIVATE);
    asm.mov_imm64(Reg::X4, u64::MAX);
    asm.mov_imm64(Reg::X5, 0);
    asm.mov_imm64(Reg::X16, u64::from(super::SYS_MMAP));
    asm.svc_0x80();
    let ok = asm.new_label();
    asm.branch(ok, super::BranchKind::Conditional(super::Cond::Cc));
    asm.mov_imm64(Reg::X0, 0);
    asm.bind(ok);
    asm.pop(Reg::X3);
    asm.ret();
    asm.bind(l.commit);
    asm.mov_reg(Reg::X0, Reg::X5);
    asm.mov_reg(Reg::X1, Reg::X4);
    asm.mov_imm64(Reg::X2, 3);
    asm.mov_imm64(Reg::X16, 74);
    asm.svc_0x80();
    let failed = asm.new_label();
    asm.branch(failed, super::BranchKind::Conditional(super::Cond::Hs));
    asm.mov_imm64(Reg::X0, 1);
    asm.ret();
    asm.bind(failed);
    asm.mov_imm64(Reg::X0, 0);
    asm.ret();
    asm.bind(l.decommit);
    asm.push(Reg::X19);
    asm.push(Reg::X20);
    asm.mov_reg(Reg::X19, Reg::X5);
    asm.mov_reg(Reg::X20, Reg::X4);
    asm.mov_reg(Reg::X0, Reg::X19);
    asm.mov_reg(Reg::X1, Reg::X20);
    asm.mov_imm64(Reg::X2, 0);
    asm.mov_imm64(Reg::X16, 74);
    asm.svc_0x80();
    let failed = asm.new_label();
    let done = asm.new_label();
    asm.branch(failed, super::BranchKind::Conditional(super::Cond::Hs));
    asm.mov_reg(Reg::X0, Reg::X19);
    asm.mov_reg(Reg::X1, Reg::X20);
    asm.mov_imm64(Reg::X2, 5); // MADV_FREE; Free-role cleanup already zeroed the page.
    asm.mov_imm64(Reg::X16, 75);
    asm.svc_0x80();
    asm.branch(failed, super::BranchKind::Conditional(super::Cond::Hs));
    asm.mov_imm64(Reg::X0, 1);
    asm.branch(done, super::BranchKind::Unconditional);
    asm.bind(failed);
    asm.mov_imm64(Reg::X0, 0);
    asm.bind(done);
    asm.pop(Reg::X20);
    asm.pop(Reg::X19);
    asm.ret();
    asm.bind(l.release);
    asm.mov_reg(Reg::X0, Reg::X5);
    asm.mov_reg(Reg::X1, Reg::X4);
    asm.mov_imm64(Reg::X16, 73); // munmap, checked carry result.
    asm.svc_0x80();
    let failed = asm.new_label();
    asm.branch(failed, super::BranchKind::Conditional(super::Cond::Hs));
    asm.mov_imm64(Reg::X0, 1);
    asm.ret();
    asm.bind(failed);
    asm.mov_imm64(Reg::X0, 0);
    asm.ret();
}

#[cfg(test)]
#[path = "macos_flag_tests.rs"]
mod flag_tests;
