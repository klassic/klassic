//! Phase-protected barrier jobs with deferred consumption, matching gc_buffer_job.h.
use super::*;
use crate::gc_abi::{BarrierJob, BufferConsumer, BufferCursor};
fn bytes(e: E) -> E {
    add(shl(e.clone(), 5), shl(e, 4))
}
#[allow(dead_code)]
pub(crate) fn emit_buffer_job<A: PortableAsm>(a: &mut A) {
    let status: [_; 5] = std::array::from_fn(|_| a.create_text_label());
    let invalid = status[1];
    let selected = a.create_text_label();
    let claim = a.create_text_label();
    let copy = a.create_text_label();
    let capped = a.create_text_label();
    let loop_top = a.create_text_label();
    let copied = a.create_text_label();
    let done = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(CTX, R::V7);
    a.store_rbp_slot(Q, R::V6);
    a.store_rbp_slot(OP, R::V2);
    let mut b = Emit { a };
    macro_rules! q {
        ($f:ident) => {
            field(Q, offset_of!(BarrierJob, $f), 0)
        };
    }
    macro_rules! c {
        ($f:ident) => {
            field(REG, offset_of!(BufferConsumer, $f), 0)
        };
    }
    macro_rules! buf {
        ($f:ident) => {
            field(SEG, offset_of!(BufferCursor, $f), 0)
        };
    }
    b.branch(local(OP), n(2), C::Above, invalid);
    b.branch(
        field(CTX, offset_of!(MutatorContext, abi_version), 4),
        n(crate::gc_abi::ABI_VERSION as u64),
        C::NotEqual,
        invalid,
    );
    b.branch(q!(identity), n(0), C::Equal, invalid);
    b.branch(
        q!(identity),
        field(CTX, offset_of!(MutatorContext, thread_id), 0),
        C::NotEqual,
        invalid,
    );
    b.branch(q!(selector), n(1), C::Above, invalid);
    b.branch(
        field(CTX, offset_of!(MutatorContext, state), 4),
        n(3),
        C::Above,
        invalid,
    );
    b.set(
        local(SEG),
        add(local(CTX), n(offset_of!(MutatorContext, satb) as u64)),
    );
    b.set(
        local(REG),
        add(
            local(CTX),
            n(offset_of!(MutatorContext, satb_consumer) as u64),
        ),
    );
    b.branch(q!(selector), n(0), C::Equal, selected);
    b.set(
        local(SEG),
        add(local(CTX), n(offset_of!(MutatorContext, remembered) as u64)),
    );
    b.set(
        local(REG),
        add(
            local(CTX),
            n(offset_of!(MutatorContext, remembered_consumer) as u64),
        ),
    );
    b.a.bind_text_label(selected);
    b.pointer(buf!(entries), invalid);
    b.branch(buf!(capacity), n(0), C::Equal, invalid);
    b.branch(buf!(capacity), n(u32::MAX as u64), C::Above, invalid);
    b.branch(
        add(buf!(entries), bytes(buf!(capacity))),
        buf!(entries),
        C::Below,
        invalid,
    );
    b.set(
        local(PREV),
        field(SEG, offset_of!(BufferCursor, consumed), 8),
    );
    b.set(
        local(NEXT),
        field(SEG, offset_of!(BufferCursor, published), 8),
    );
    b.branch(local(PREV), local(NEXT), C::Above, invalid);
    b.branch(
        sub(local(NEXT), local(PREV)),
        buf!(capacity),
        C::Above,
        invalid,
    );
    b.branch(local(OP), n(0), C::Equal, claim);
    b.branch(q!(serial), n(0), C::Equal, invalid);
    b.branch(q!(serial), c!(serial), C::NotEqual, invalid);
    b.branch(c!(last_serial), n(0), C::Equal, invalid);
    b.branch(c!(last_serial), n(u64::MAX / 2), C::Above, invalid);
    b.branch(
        c!(serial),
        add(shl(c!(last_serial), 1), q!(selector)),
        C::NotEqual,
        invalid,
    );
    b.branch(q!(begin), local(PREV), C::NotEqual, invalid);
    b.branch(q!(end), c!(end), C::NotEqual, invalid);
    b.branch(q!(end), local(PREV), C::Below, invalid);
    b.branch(q!(end), local(PREV), C::Equal, invalid);
    b.branch(q!(end), local(NEXT), C::Above, invalid);
    b.set(local(USED), sub(q!(end), local(PREV)));
    b.branch(local(USED), n(256), C::Above, invalid);
    b.branch(local(OP), n(1), C::Equal, copy);
    b.branch(c!(cursor), q!(end), C::NotEqual, status[3]);
    b.branch(q!(written), local(USED), C::NotEqual, status[3]);
    b.set(field(SEG, offset_of!(BufferCursor, consumed), 8), q!(end));
    for off in [
        offset_of!(BufferConsumer, serial),
        offset_of!(BufferConsumer, end),
        offset_of!(BufferConsumer, cursor),
    ] {
        b.set(field(REG, off, 0), n(0));
    }
    b.a.jmp_label(status[0]);
    b.a.bind_text_label(claim);
    b.branch(c!(serial), n(0), C::NotEqual, status[3]);
    b.branch(c!(end), n(0), C::NotEqual, invalid);
    b.branch(c!(cursor), n(0), C::NotEqual, invalid);
    b.branch(q!(capacity), n(0), C::Equal, invalid);
    b.branch(q!(capacity), n(256), C::Above, invalid);
    b.branch(local(PREV), local(NEXT), C::Equal, status[2]);
    b.branch(c!(last_serial), n(u64::MAX / 2), C::AboveOrEqual, status[4]);
    b.set(local(USED), sub(local(NEXT), local(PREV)));
    b.branch(local(USED), q!(capacity), C::Below, capped);
    b.set(local(USED), q!(capacity));
    b.a.bind_text_label(capped);
    b.set(c!(last_serial), add(c!(last_serial), n(1)));
    b.set(c!(serial), add(shl(c!(last_serial), 1), q!(selector)));
    b.set(c!(end), add(local(PREV), local(USED)));
    b.set(c!(cursor), local(PREV));
    b.set(q!(serial), c!(serial));
    b.set(q!(begin), local(PREV));
    b.set(q!(end), c!(end));
    b.set(q!(written), n(0));
    b.a.jmp_label(status[0]);
    b.a.bind_text_label(copy);
    b.branch(c!(cursor), local(PREV), C::NotEqual, invalid);
    b.branch(q!(written), n(0), C::NotEqual, invalid);
    b.pointer(q!(output), invalid);
    b.branch(q!(capacity), local(USED), C::Below, invalid);
    b.branch(q!(capacity), n(256), C::Above, invalid);
    b.branch(
        add(q!(output), bytes(q!(capacity))),
        q!(output),
        C::Below,
        invalid,
    );
    b.set(local(STATE), n(0));
    b.a.bind_text_label(loop_top);
    b.branch(local(STATE), local(USED), C::Equal, copied);
    b.value(add(local(PREV), local(STATE)));
    b.a.load_rbp_slot(R::V11, SEG);
    b.a.load_ptr_disp32(R::V1, R::V11, offset_of!(BufferCursor, capacity) as i32);
    b.a.div_unsigned(R::V1);
    // Keep the two record addresses in volatile registers through the copy.
    // Re-evaluating six field expressions needlessly multiplies stack traffic
    // in a full 256-entry slice on both targets.
    b.a.mov_reg_reg(R::V8, R::V2);
    b.a.shl_reg_imm8(R::V8, 5);
    b.a.shl_reg_imm8(R::V2, 4);
    b.a.add_reg_reg(R::V8, R::V2);
    b.a.load_rbp_slot(R::V9, SEG);
    b.a.load_ptr_disp32(R::V9, R::V9, offset_of!(BufferCursor, entries) as i32);
    b.a.add_reg_reg(R::V9, R::V8);
    b.a.load_rbp_slot(R::V10, STATE);
    b.a.mov_reg_reg(R::V8, R::V10);
    b.a.shl_reg_imm8(R::V8, 5);
    b.a.shl_reg_imm8(R::V10, 4);
    b.a.add_reg_reg(R::V8, R::V10);
    b.a.load_rbp_slot(R::V11, Q);
    b.a.load_ptr_disp32(R::V11, R::V11, offset_of!(BarrierJob, output) as i32);
    b.a.add_reg_reg(R::V11, R::V8);
    for off in (0..48).step_by(8) {
        b.a.load_ptr_disp32(R::V0, R::V9, off);
        b.a.store_ptr_disp32(R::V11, off, R::V0);
    }
    b.set(local(STATE), add(local(STATE), n(1)));
    b.a.jmp_label(loop_top);
    b.a.bind_text_label(copied);
    b.set(q!(written), local(USED));
    b.set(c!(cursor), q!(end));
    b.a.jmp_label(status[0]);
    for (code, label) in status.into_iter().enumerate() {
        b.a.bind_text_label(label);
        b.a.mov_imm64(R::V0, code as u64);
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(done);
    b.a.leave();
}
