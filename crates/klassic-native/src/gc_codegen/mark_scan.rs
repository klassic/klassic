//! Bounded atomic slot loading from a protected canonical mark claim.
use super::*;
use crate::gc_abi::{MarkRecord, MarkRequest, MarkScan, ObjectHeader};
macro_rules! scan {
    ($f:ident) => {
        field(G, offset_of!(MarkScan, $f), 0)
    };
}
macro_rules! job {
    ($f:ident) => {
        field(SEG, offset_of!(MarkRequest, $f), 0)
    };
}
macro_rules! header {
    ($f:ident) => {
        field(REG, offset_of!(ObjectHeader, $f), 0)
    };
}
const DESC: i32 = 192;
/// V7=MarkScan, finite guard/no-poll leaf. Does not advance the claim cursor.
#[allow(dead_code)]
pub(crate) fn emit_mark_scan<A: PortableAsm>(a: &mut A) {
    let after = a.create_text_label();
    let invalid = a.create_text_label();
    let done = a.create_text_label();
    let descriptor = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(G, R::V7);
    let mut b = Emit { a };
    b.pointer(scan!(job), invalid);
    b.pointer(scan!(registry), invalid);
    b.branch(and(scan!(heap_base), n(15)), n(0), C::NotEqual, invalid);
    let locations_valid = b.a.create_text_label();
    b.branch(scan!(locations), n(0), C::Equal, locations_valid);
    b.pointer(scan!(locations), invalid);
    b.branch(scan!(capacity), n(1), C::Below, invalid);
    b.branch(scan!(capacity), n(256), C::Above, invalid);
    b.set(local(USED), shl(scan!(capacity), 3));
    b.branch(
        add(scan!(locations), local(USED)),
        scan!(locations),
        C::Below,
        invalid,
    );
    b.branch(
        add(scan!(output), local(USED)),
        scan!(output),
        C::Below,
        invalid,
    );
    b.branch(
        scan!(locations),
        add(scan!(output), local(USED)),
        C::AboveOrEqual,
        locations_valid,
    );
    b.branch(
        scan!(output),
        add(scan!(locations), local(USED)),
        C::Below,
        invalid,
    );
    b.a.bind_text_label(locations_valid);
    b.set(local(SEG), scan!(job));
    for v in [
        job!(index),
        job!(serial),
        job!(incarnation),
        job!(metadata_lease),
    ] {
        b.branch(v, n(0), C::Equal, invalid);
    }
    b.pointer(job!(record), invalid);
    b.branch(shr(job!(reference), 16), n(32), C::Below, invalid);
    b.branch(
        and(job!(reference), n(0xf0000 | 0xcf)),
        n(0),
        C::NotEqual,
        invalid,
    );
    b.branch(and(job!(reference), n(0x30)), n(0), C::Equal, invalid);
    for mask in [0x300, 0xc00, 0xf000] {
        b.set(local(USED), and(job!(reference), n(mask)));
        b.branch(local(USED), n(0), C::Equal, invalid);
        b.branch(
            and(local(USED), sub(local(USED), n(1))),
            n(0),
            C::NotEqual,
            invalid,
        );
    }
    b.set(local(Q), add(scan!(heap_base), shr(job!(reference), 16)));
    b.branch(local(Q), scan!(heap_base), C::Below, invalid);
    b.set(local(REG), sub(local(Q), n(32)));
    b.branch(
        field(REG, offset_of!(ObjectHeader, state), 8),
        n(2),
        C::NotEqual,
        invalid,
    );
    b.set(local(PREV), job!(record));
    b.branch(
        field(PREV, offset_of!(MarkRecord, object_id), 0),
        n(0),
        C::Equal,
        invalid,
    );
    b.branch(header!(mark_record), job!(record), C::NotEqual, invalid);
    b.branch(
        add(local(Q), header!(payload_bytes)),
        local(Q),
        C::Below,
        invalid,
    );
    for (i, v) in [
        header!(descriptor_id),
        header!(payload_bytes),
        job!(cursor),
        scan!(capacity),
        scan!(output),
        scan!(capacity),
        n(0),
        n(0),
    ]
    .into_iter()
    .enumerate()
    {
        b.set(local(DESC - i as i32 * 8), v);
    }
    b.value(scan!(registry));
    b.a.mov_reg_reg(R::V7, R::V0);
    b.a.mov_reg_reg(R::V6, R::V5);
    b.a.sub_reg_imm32(R::V6, DESC);
    b.a.call_label(descriptor);
    b.a.store_rbp_slot(OP, R::V0);
    b.a.cmp_reg_imm32(R::V0, 2);
    b.a.jcc_label(C::Equal, done);
    b.value(scan!(output));
    b.a.mov_reg_reg(R::V6, R::V0);
    b.a.load_rbp_slot(R::V7, Q);
    b.a.load_rbp_slot(R::V8, DESC - 48);
    b.value(scan!(locations));
    b.a.mov_reg_reg(R::V9, R::V0);
    let loop_label = b.a.create_text_label();
    let scanned = b.a.create_text_label();
    b.a.bind_text_label(loop_label);
    b.a.test_reg_reg(R::V8, R::V8);
    b.a.jcc_label(C::Equal, scanned);
    b.a.load_ptr_disp32(R::V0, R::V6, 0);
    let no_location = b.a.create_text_label();
    b.a.test_reg_reg(R::V9, R::V9);
    b.a.jcc_label(C::Equal, no_location);
    b.a.store_ptr_disp32(R::V9, 0, R::V0);
    b.a.add_reg_imm32(R::V9, 8);
    b.a.bind_text_label(no_location);
    b.a.add_reg_reg(R::V0, R::V7);
    b.a.atomic_load_acquire(R::V0, R::V0);
    b.a.store_ptr_disp32(R::V6, 0, R::V0);
    b.a.add_reg_imm32(R::V6, 8);
    b.a.sub_reg_imm32(R::V8, 1);
    b.a.jmp_label(loop_label);
    b.a.bind_text_label(scanned);
    b.set(scan!(next_cursor), local(DESC - 16));
    b.set(scan!(written), local(DESC - 48));
    b.set(scan!(examined), local(DESC - 56));
    b.value(local(OP));
    b.a.jmp_label(done);
    b.a.bind_text_label(invalid);
    b.a.mov_imm64(R::V0, 2);
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.jmp_label(after);
    b.a.bind_text_label(descriptor);
    crate::gc_codegen::descriptors::emit_scan(b.a);
    b.a.ret();
    b.a.bind_text_label(after);
}
