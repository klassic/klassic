//! Claimed moved slot -> destination pending work; gc_remembered_transfer.h.
use super::*;
use crate::gc_abi::{
    RememberedInventory as I, RememberedInventoryRequest as Check, RememberedJob as J,
    RememberedRegistration as N, RememberedSet as S, RememberedTransfer as T,
};
const SC: i32 = 104;
const SP: i32 = 112;
const DC: i32 = 120;
const DP: i32 = 128;
const BEFORE: i32 = 136;
const AFTER: i32 = 144;
const TMP: i32 = 200;
fn boolean<A: PortableAsm>(b: &mut Emit<'_, A>, slot: i32, v: E) {
    let done = b.a.create_text_label();
    b.set(local(slot), n(0));
    b.branch(v, n(0), C::Equal, done);
    b.set(local(slot), n(1));
    b.a.bind_text_label(done);
}
fn bitmap<A: PortableAsm>(
    b: &mut Emit<'_, A>,
    set: i32,
    offset: E,
    mask: i32,
    current: i32,
    previous: i32,
) {
    b.set(
        local(OP),
        shr(sub(offset, field(set, offset_of!(S, start), 0)), 3),
    );
    b.set(local(mask), n(1));
    b.set(local(TMP), and(local(OP), n(63)));
    let loop_ = b.a.create_text_label();
    let done = b.a.create_text_label();
    b.a.bind_text_label(loop_);
    b.branch(local(TMP), n(0), C::Equal, done);
    b.set(local(mask), shl(local(mask), 1));
    b.set(local(TMP), sub(local(TMP), n(1)));
    b.a.jmp_label(loop_);
    b.a.bind_text_label(done);
    for (slot, bank) in [
        (current, field(set, offset_of!(S, current), 0)),
        (previous, sub(n(1), field(set, offset_of!(S, current), 0))),
    ] {
        let base = E::Mem(
            Box::new(add(
                add(local(set), n(offset_of!(S, bits) as u64)),
                shl(bank, 3),
            )),
            0,
            0,
        );
        b.set(local(slot), add(base, shl(shr(local(OP), 6), 3)));
    }
}
#[allow(dead_code)]
pub(crate) fn emit_transfer<A: PortableAsm>(a: &mut A) {
    let status: [_; 3] = std::array::from_fn(|_| a.create_text_label());
    let bad = status[1];
    let done = a.create_text_label();
    let core = a.create_text_label();
    let after = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    let mut b = Emit { a };
    macro_rules! q {
        ($f:ident) => {
            field(Q, offset_of!(T, $f), 0)
        };
    }
    macro_rules! s {
        ($f:ident) => {
            field(REG, offset_of!(S, $f), 0)
        };
    }
    macro_rules! d {
        ($f:ident) => {
            field(SEG, offset_of!(S, $f), 0)
        };
    }
    macro_rules! g {
        ($f:ident) => {
            field(G, offset_of!(I, $f), 0)
        };
    }
    macro_rules! j {
        ($f:ident) => {
            field(LEASE, offset_of!(J, $f), 0)
        };
    }
    b.pointer(local(Q), bad);
    b.a.mov_reg_reg(R::V0, R::V5);
    b.a.sub_reg_imm32(R::V0, 176);
    b.a.store_rbp_slot(OP, R::V0);
    b.set(field(OP, offset_of!(Check, job), 0), q!(claim));
    b.set(field(OP, offset_of!(Check, phase), 0), n(0));
    for reg in [q!(source), q!(destination)] {
        b.set(field(OP, offset_of!(Check, registration), 0), reg);
        b.a.load_rbp_slot(R::V7, G);
        b.a.load_rbp_slot(R::V6, OP);
        b.a.mov_imm64(R::V2, 8);
        b.a.call_label(core);
        b.a.store_rbp_slot(TMP, R::V0);
        b.branch(local(TMP), n(0), C::NotEqual, bad);
    }
    b.set(local(REG), q!(source));
    b.set(local(REG), field(REG, offset_of!(N, set), 0));
    b.set(local(SEG), q!(destination));
    b.set(local(SEG), field(SEG, offset_of!(N, set), 0));
    b.set(local(LEASE), q!(claim));
    b.branch(local(REG), local(SEG), C::Equal, bad);
    let separate = b.a.create_text_label();
    b.branch(s!(start), d!(limit), C::Above, separate);
    b.branch(s!(start), d!(limit), C::Equal, separate);
    b.branch(d!(start), s!(limit), C::Below, bad);
    b.a.bind_text_label(separate);
    b.branch(j!(incarnation), s!(incarnation), C::NotEqual, bad);
    b.branch(j!(serial), n(0), C::Equal, bad);
    b.branch(j!(serial), s!(active_serial), C::NotEqual, bad);
    b.branch(j!(offset), s!(active_offset), C::NotEqual, bad);
    b.branch(j!(phase), s!(epoch), C::NotEqual, bad);
    b.branch(j!(keep), n(0), C::NotEqual, bad);
    b.branch(q!(incarnation), d!(incarnation), C::NotEqual, bad);
    b.branch(q!(offset), d!(start), C::Below, bad);
    b.branch(q!(offset), d!(limit), C::Above, bad);
    b.branch(q!(offset), d!(limit), C::Equal, bad);
    b.branch(and(q!(offset), n(7)), n(0), C::NotEqual, bad);
    b.branch(s!(last_phase), g!(last_phase), C::Above, bad);
    b.branch(d!(last_phase), g!(last_phase), C::Above, bad);
    b.branch(s!(epoch), g!(epoch), C::NotEqual, status[2]);
    b.branch(d!(epoch), g!(epoch), C::NotEqual, status[2]);
    bitmap(&mut b, REG, j!(offset), STATE, CTX, PREV);
    bitmap(&mut b, SEG, q!(offset), USED, NEXT, KIND);
    boolean(&mut b, SC, and(field(CTX, 0, 0), local(STATE)));
    boolean(&mut b, SP, and(field(PREV, 0, 0), local(STATE)));
    boolean(&mut b, DC, and(field(NEXT, 0, 0), local(USED)));
    boolean(&mut b, DP, and(field(KIND, 0, 0), local(USED)));
    boolean(&mut b, BEFORE, s!(pending));
    boolean(&mut b, TMP, d!(pending));
    b.set(local(BEFORE), add(local(BEFORE), local(TMP)));
    for (count, flag) in [
        (s!(current_count), SC),
        (s!(pending), SP),
        (d!(current_count), DC),
        (d!(pending), DP),
    ] {
        b.branch(count, local(flag), C::Below, bad);
    }
    let dc_ok = b.a.create_text_label();
    let dp_ok = b.a.create_text_label();
    b.branch(local(SC), n(0), C::Equal, dc_ok);
    b.branch(local(DC), n(0), C::NotEqual, dc_ok);
    b.branch(
        d!(current_count),
        shr(sub(d!(limit), d!(start)), 3),
        C::Equal,
        bad,
    );
    b.a.bind_text_label(dc_ok);
    b.branch(local(DP), n(0), C::NotEqual, dp_ok);
    b.branch(
        d!(pending),
        shr(sub(d!(limit), d!(start)), 3),
        C::Equal,
        bad,
    );
    b.a.bind_text_label(dp_ok);
    b.branch(g!(pending), local(BEFORE), C::Below, bad);
    boolean(&mut b, TMP, d!(active_serial));
    b.branch(g!(claims), add(n(1), local(TMP)), C::Below, bad);
    boolean(&mut b, AFTER, sub(s!(pending), local(SP)));
    b.set(
        local(AFTER),
        add(add(sub(g!(pending), local(BEFORE)), local(AFTER)), n(1)),
    );
    b.branch(local(AFTER), g!(members), C::Above, bad);
    // All rejection checks precede this locked transaction's first mutation.
    let current_done = b.a.create_text_label();
    let previous_done = b.a.create_text_label();
    b.branch(local(SC), n(0), C::Equal, current_done);
    b.branch(local(DC), n(0), C::NotEqual, current_done);
    b.set(field(NEXT, 0, 0), add(field(NEXT, 0, 0), local(USED)));
    b.set(d!(current_count), add(d!(current_count), n(1)));
    b.a.bind_text_label(current_done);
    b.branch(local(DP), n(0), C::NotEqual, previous_done);
    b.set(field(KIND, 0, 0), add(field(KIND, 0, 0), local(USED)));
    b.set(d!(pending), add(d!(pending), n(1)));
    b.a.bind_text_label(previous_done);
    b.set(
        field(CTX, 0, 0),
        and(field(CTX, 0, 0), sub(n(u64::MAX), local(STATE))),
    );
    b.set(
        field(PREV, 0, 0),
        and(field(PREV, 0, 0), sub(n(u64::MAX), local(STATE))),
    );
    b.set(s!(current_count), sub(s!(current_count), local(SC)));
    b.set(s!(pending), sub(s!(pending), local(SP)));
    b.set(s!(active_serial), n(0));
    b.set(s!(active_offset), n(0));
    b.set(g!(claims), sub(g!(claims), n(1)));
    b.set(g!(pending), local(AFTER));
    b.branch(s!(last_phase), d!(last_phase), C::Below, status[0]);
    b.set(d!(last_phase), s!(last_phase));
    b.a.jmp_label(status[0]);
    for (i, label) in status.into_iter().enumerate() {
        b.a.bind_text_label(label);
        b.a.mov_imm64(R::V0, i as u64);
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.jmp_label(after);
    b.a.bind_text_label(core);
    super::remembered_inventory::emit_inventory(b.a);
    b.a.ret();
    b.a.bind_text_label(after);
}
