//! An actual ACK/Native handoff closes owner remembered admission.
use super::{H, Src, slot_offset};
use crate::gc_codegen::production::test_support::*;
use crate::portable_asm::{PortableAsm, Reg as R};

fn sealed_store(g: &mut crate::NativeCodeGenerator, mode: u64) {
    let mut h = H::begin(g);
    h.alloc(2, 32, 0);
    h.root(0);
    remembered_buffer_enable_moving(h.g);
    remembered_buffer_cycle(h.g, false);
    remembered_buffer_cycle(h.g, false);
    h.value(Src::Root(0));
    h.keep(4);
    h.alloc(1, 16, 3);
    h.set_scalar(Src::Raw(3), 0, 1776);
    h.store(Src::Raw(4), 0, Src::Raw(3), true);
    h.report("before", |h| remembered_buffer_metric(h.g, 0));
    if mode == 0 {
        remembered_owner_ack(h.g);
    } else {
        h.g.call_label(h.l.native_enter);
    }
    h.report("sealed", |h| remembered_buffer_metric(h.g, 0));
    h.report("state", |h| native_context_state(h.g));
    // Existing registration must not bypass admission either. No movement
    // runs in this deterministic fixture, so these raw values remain valid.
    h.g.load_rbp_slot(R::V7, slot_offset(4));
    remembered_record_probe(h.g);
}

#[test]
fn production_remembered_store_rejects_acked_and_native_owners() {
    for mode in [0, 1] {
        let o = super::execute(sealed_store, mode, true, false);
        assert_eq!(o.status.code(), Some(1));
        let stdout = String::from_utf8_lossy(&o.stdout);
        assert!(stdout.contains("before=1\n"), "{stdout}");
        assert!(stdout.contains("sealed=0\n"), "{stdout}");
        assert!(
            stdout.contains(&format!("state={}\n", mode + 1)),
            "{stdout}"
        );
        assert!(String::from_utf8_lossy(&o.stderr).contains("runtime invariant violated"));
    }
}
