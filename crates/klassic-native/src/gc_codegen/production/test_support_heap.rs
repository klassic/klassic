//! Fault sentinels immediately outside the configured generation directory.
use super::super::*;
use crate::gc_codegen::production::lea_rt;
pub(crate) fn poison_next_generation<E: PortableAsm>(a: &mut E) {
    let offset = layout(a).generations + (geometry(a).pages + 1) * 8;
    assert!(offset + 8 <= layout(a).owner_cards);
    lea_rt(a, R::V1, offset as usize);
    a.mov_imm64(R::V0, generation::YOUNG);
    a.atomic_store_release(R::V1, R::V0);
}
