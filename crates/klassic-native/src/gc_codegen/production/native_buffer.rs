//! Unmanaged console buffers: actual owner Native/Running transitions, no
//! borrowed heap cursor or synchronous whole-heap pin. Formatter buffers are
//! native stack/image storage; callers retain all managed state in real roots.
use super::*;

pub(super) fn emit<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, runtime: E::DataLabel) {
    a.bind_text_label(l.write_native_buffer);
    let saved = native_io::SAVED.into_iter().filter(|r| *r != R::V0);
    for r in saved.clone() {
        a.push_reg(r);
    }
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    for _ in 0..4 {
        a.push_reg(R::V0);
    }
    a.store_rbp_slot(8, R::V7);
    a.store_rbp_slot(16, R::V6);
    a.store_rbp_slot(24, R::V2);
    load_runtime(a, runtime);
    native_api::compiler_bootstrap(a, l);
    native_api::compiler_transition(a, l, true);
    a.load_rbp_slot(R::V7, 8);
    a.load_rbp_slot(R::V6, 16);
    a.load_rbp_slot(R::V2, 24);
    a.push_reg(R::V3);
    a.push_reg(R::V3);
    a.plat_write_buffer();
    a.pop_reg(R::V3);
    a.pop_reg(R::V3);
    a.store_rbp_slot(32, R::V0);
    native_api::compiler_transition(a, l, false);
    copy::bump(a, native_io::BUFFER_WRITES, R::V0);
    a.load_rbp_slot(R::V0, 32);
    a.leave();
    for r in saved.rev() {
        a.pop_reg(r);
    }
    a.ret();
}
