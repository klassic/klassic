//! Darwin native OS leaves for the production single-worker collector.
//! Nonmoving event storage is destroyed only after the worker has joined.
use super::{Assembler, BranchKind as B, Cond, DataLabel, Label, Reg as R, gc_macos};
use crate::gc_codegen::production::WorkerHooks;
use crate::macho::ImportSymbol as I;

fn begin(a: &mut Assembler) {
    a.push_frame_record();
    a.push(R::X3); // common runtime pointer is volatile in the native ABI
}
fn end(a: &mut Assembler) {
    a.pop(R::X3);
    a.pop_frame_record();
    a.ret();
}
fn status(a: &mut Assembler, symbol: I) {
    a.call_import(symbol);
    a.word(0x2a00_03e0); // mov w0,w0: C int's upper bits are unspecified
}

pub(super) fn emit(a: &mut Assembler, h: &WorkerHooks<Label>, entry: Label, storage: DataLabel) {
    let init = a.new_label();
    let destroy = a.new_label();
    let park = a.new_label();
    let wake = a.new_label();
    let failed = a.new_label();
    let invalid = a.new_label();
    let cleanup = a.new_label();
    let callback = a.new_label();
    let lifecycle_failed = a.new_label();
    a.bind(h.spawn);
    begin(a);
    a.load_data_address(R::X0, storage);
    a.branch(init, B::Link);
    a.branch(cleanup, B::CompareNonZero(R::X0));
    a.load_data_address(R::X0, storage);
    a.add_reg_imm(R::X0, R::X0, 128); // pthread_t after the event
    a.mov_imm64(R::X1, 0);
    a.branch(callback, B::Address(R::X2));
    a.mov_imm64(R::X3, 0);
    status(a, I::PthreadCreate);
    a.branch(cleanup, B::CompareNonZero(R::X0));
    a.mov_imm64(R::X0, 1);
    end(a);
    a.bind(cleanup);
    a.load_data_address(R::X0, storage);
    a.branch(destroy, B::Link);
    a.branch(invalid, B::CompareNonZero(R::X0));
    a.bind(failed);
    a.mov_imm64(R::X0, 0);
    end(a);

    if let Some(join) = h.join {
        a.bind(join);
        begin(a);
        a.load_data_address(R::X0, storage);
        a.ldr_imm(R::X0, R::X0, 128);
        a.branch(lifecycle_failed, B::CompareZero(R::X0));
        // pthread_join writes the callback's pointer result, not a C int.
        a.sub_sp_imm(16);
        a.mov_imm64(R::X1, u64::MAX);
        a.word(0xf900_03e1); // str x1,[sp]: poison the output until join writes it
        a.add_reg_sp_imm(R::X1, 0);
        status(a, I::PthreadJoin);
        a.branch(lifecycle_failed, B::CompareNonZero(R::X0));
        a.word(0xf940_03e0); // ldr x0,[sp]
        a.add_sp_imm(16);
        a.branch(lifecycle_failed, B::CompareNonZero(R::X0));
        a.load_data_address(R::X0, storage);
        a.mov_imm64(R::X1, 0);
        a.str_imm(R::X1, R::X0, 128);
        a.branch(destroy, B::Link);
        a.branch(lifecycle_failed, B::CompareNonZero(R::X0));
        end(a);
    }
    a.bind(callback);
    emit_callback(a, entry);

    a.bind(h.park);
    begin(a);
    let awake = a.new_label();
    a.ldr_imm32(R::X0, R::X5, 0);
    a.word(0x2a04_03e2); // mov w2,w4: compare the low sequence word
    a.cmp_reg(R::X0, R::X2);
    a.branch(awake, B::Conditional(Cond::Ne));
    a.load_data_address(R::X0, storage);
    a.branch(park, B::Link);
    a.branch(invalid, B::CompareNonZero(R::X0));
    a.bind(awake);
    end(a);

    a.bind(h.unpark);
    begin(a);
    let absent = a.new_label();
    a.load_data_address(R::X0, storage);
    a.ldr_imm(R::X1, R::X0, 120); // initialized ownership mask
    a.branch(absent, B::CompareZero(R::X1));
    a.branch(wake, B::Link);
    a.branch(invalid, B::CompareNonZero(R::X0));
    a.bind(absent);
    end(a);

    a.bind(h.relax);
    a.word(0xd503_203f); // YIELD hint; preserves all registers and NZCV
    a.ret();
    a.bind(h.clock);
    a.emit_gc_monotonic_ns();
    a.ret();
    a.bind(init);
    gc_macos::emit_event_init(a);
    a.bind(destroy);
    gc_macos::emit_event_destroy(a);
    a.bind(park);
    gc_macos::emit_timed_park(a);
    a.bind(wake);
    gc_macos::emit_wake(a);
    a.bind(invalid);
    a.emit_write_rodata(2, b"GC worker notification failed\n");
    a.emit_exit(1);
    a.bind(lifecycle_failed);
    a.emit_write_rodata(2, b"GC worker lifecycle failed\n");
    a.emit_exit(1);
}

/// Returning to libSystem must obey AAPCS64 even though the internal collector
/// uses its own register convention. Keep x19-x29 and d8-d15, and return NULL.
pub(super) fn emit_callback(a: &mut Assembler, entry: Label) {
    a.push_frame_record();
    for n in 19..=28 {
        a.word(0xf81f_0fe0 | n); // str xn,[sp,#-16]!
    }
    a.sub_sp_imm(64);
    for n in 8..=15 {
        a.word(0xfd00_03e0 | ((n - 8) << 10) | n); // str dn,[sp,#offset]
    }
    a.branch(entry, B::Link);
    for n in 8..=15 {
        a.word(0xfd40_03e0 | ((n - 8) << 10) | n); // ldr dn,[sp,#offset]
    }
    a.add_sp_imm(64);
    for n in (19..=28).rev() {
        a.word(0xf841_07e0 | n); // ldr xn,[sp],#16
    }
    a.pop_frame_record();
    a.mov_imm64(R::X0, 0);
    a.ret();
}
