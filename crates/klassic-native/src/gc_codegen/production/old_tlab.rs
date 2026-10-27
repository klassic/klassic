//! Affine TLAB suspension for enrolled mark and moving-end/Idle rounds. The actual
//! page remains allocated to this owner; no bank/page/metadata reuse is allowed.
//! Parked contexts expose zero TLAB fields. Resume revalidates the same page,
//! owner, incarnation and published allocation frontier before restoring it.
use super::*;
#[repr(C)]
pub(super) struct State {
    active: u64,
    page: u64,
    incarnation: u64,
    span: [u64; 3],
    pub(super) suspends: u64,
    pub(super) restores: u64,
}
const BASE: usize =
    owner_state::offset(offset_of!(OwnerState, phase_round) + offset_of!(phase_round::Owner, tlab));
const ACTIVE: usize = BASE + offset_of!(State, active);
const PAGE: usize = BASE + offset_of!(State, page);
const INCARNATION: usize = BASE + offset_of!(State, incarnation);
const SPAN: usize = BASE + offset_of!(State, span);
const FIELDS: [usize; 3] = [
    offset_of!(MutatorContext, tlab_start),
    offset_of!(MutatorContext, tlab_top),
    offset_of!(MutatorContext, tlab_end),
];

fn validate<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    journal_snapshot::lifetime(a, l);
    load_rt(a, R::V2, PAGE);
    load_rt(a, R::V0, RT_REQUEST + offset_of!(AllocationRequest, page));
    a.cmp_reg_reg(R::V0, R::V2);
    a.jcc_label(C::NotEqual, l.fail);
    let absent = a.create_text_label();
    let done = a.create_text_label();
    a.test_reg_reg(R::V2, R::V2);
    a.jcc_label(C::Equal, absent);
    for (field, expected) in [
        (
            offset_of!(AllocationPage, owner),
            RT_CTX + offset_of!(MutatorContext, thread_id),
        ),
        (offset_of!(AllocationPage, incarnation), INCARNATION),
    ] {
        a.load_ptr_disp32(R::V0, R::V2, d(field));
        load_rt(a, R::V1, expected);
        a.cmp_reg_reg(R::V0, R::V1);
        a.jcc_label(C::NotEqual, l.fail);
    }
    a.load_ptr_disp32(R::V0, R::V2, d(offset_of!(AllocationPage, state)));
    a.cmp_reg_imm32(R::V0, crate::gc_abi::PAGE_ALLOCATING as i32);
    a.jcc_label(C::NotEqual, l.fail);
    for (i, field) in [
        offset_of!(AllocationPage, start),
        offset_of!(AllocationPage, allocated_top),
        offset_of!(AllocationPage, limit),
    ]
    .into_iter()
    .enumerate()
    {
        a.load_ptr_disp32(R::V0, R::V2, d(field));
        load_rt(a, R::V1, RT_HEAP_BASE);
        a.add_reg_reg(R::V0, R::V1);
        load_rt(a, R::V1, SPAN + i * 8);
        a.cmp_reg_reg(R::V0, R::V1);
        a.jcc_label(C::NotEqual, l.fail);
    }
    a.jmp_label(done);
    a.bind_text_label(absent);
    for i in 0..3 {
        load_rt(a, R::V0, SPAN + i * 8);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, l.fail);
    }
    a.bind_text_label(done);
}
pub(super) fn suspend<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    load_rt(a, R::V0, ACTIVE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    for (i, field) in FIELDS.into_iter().enumerate() {
        load_rt(a, R::V0, RT_CTX + field);
        store_rt(a, SPAN + i * 8, R::V0);
    }
    load_rt(a, R::V2, RT_REQUEST + offset_of!(AllocationRequest, page));
    store_rt(a, PAGE, R::V2);
    let absent = a.create_text_label();
    a.test_reg_reg(R::V2, R::V2);
    a.jcc_label(C::Equal, absent);
    a.load_ptr_disp32(R::V0, R::V2, d(offset_of!(AllocationPage, incarnation)));
    store_rt(a, INCARNATION, R::V0);
    a.bind_text_label(absent);
    validate(a, l);
    for field in FIELDS {
        store_rt_imm(a, RT_CTX + field, 0);
    }
    store_rt_imm(a, ACTIVE, 1);
    copy::bump(a, BASE + offset_of!(State, suspends), R::V0);
}
pub(super) fn restore<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let done = a.create_text_label();
    load_rt(a, R::V0, ACTIVE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    for field in FIELDS {
        load_rt(a, R::V0, RT_CTX + field);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, l.fail);
    }
    validate(a, l);
    for (i, field) in FIELDS.into_iter().enumerate() {
        load_rt(a, R::V0, SPAN + i * 8);
        store_rt(a, RT_CTX + field, R::V0);
    }
    store_rt_imm(a, ACTIVE, 0);
    copy::bump(a, BASE + offset_of!(State, restores), R::V0);
    a.bind_text_label(done);
}
