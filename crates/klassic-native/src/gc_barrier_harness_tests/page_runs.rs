//! Actual Old/Large geometry consumed by the shared reference lease leaf.
use super::{H, Src, run, slot_offset};
use crate::gc_codegen::production::{MAX_PAYLOAD, test_support::*};
use crate::portable_asm::{PortableAsm, Reg as R};
fn probe(h: &mut H<'_>, request: crate::DataLabel, slot: u32, delta: u64) {
    h.value(Src::Root(slot));
    emit_encode_reference(h.g, R::V0, R::V1);
    h.g.mov_imm64(R::V1, delta << 16);
    h.g.add_reg_reg(R::V0, R::V1);
    h.g.mov_data_addr(R::V1, request);
    h.g.store_ptr_disp32(R::V1, 0, R::V0);
    reference_probe(h.g, request);
}
fn geometry(g: &mut crate::NativeCodeGenerator, arg: u64) {
    let large = arg & 256 != 0;
    let bytes = if large { 400_000 } else { arg };
    let mut h = H::begin(g);
    if !large {
        // The first real Full copy leaves exactly one header at the end of
        // page one. The next object's payload begins in the following page.
        h.alloc(1, MAX_PAYLOAD - 32, 0);
        h.root(0);
    } else {
        h.g.mov_imm64(R::V0, 0);
        h.keep(0);
        h.root(0);
    }
    h.alloc(1, bytes, 1);
    h.root(1);
    if bytes > 0 {
        h.set_scalar(Src::Root(1), 0, 77);
    }
    request_full_collection(h.g);
    h.collect();
    h.finish_collection();
    for (i, key) in [
        "payload_mod",
        "tail_state",
        "physical_inc",
        "start",
        "limit",
        "top",
        "words",
        "head_inc",
    ]
    .iter()
    .enumerate()
    {
        h.report(key, |h| {
            h.value(Src::Root(1));
            reference_geometry(h.g, i);
        });
    }
    let r = reference_lease(h.g);
    probe(&mut h, r, 1, 0);
    h.keep(9);
    h.report("shared", |h| h.value(Src::Raw(9)));
    h.report("active", |h| lease_metric(h.g, 0));
    // A retained metadata token is not permission to call an interior word
    // an object. The real source bitmap rejects it without partial ownership.
    let bad = reference_lease(h.g);
    probe(&mut h, bad, 1, 16);
    h.keep(9);
    h.report("interior", |h| h.value(Src::Raw(9)));
    h.report("bad_token", |h| lease_request_word(h.g, bad, 3));
    reference_transfer(h.g, r, true);
    lease_require_ok(h.g);
    let owned = lease_request(h.g);
    h.g.load_rbp_slot(R::V0, slot_offset(1));
    lease_call(h.g, owned, 0);
    lease_require_ok(h.g);
    h.report("public_head", |h| lease_request_word(h.g, owned, 0));
    lease_call(h.g, owned, 1);
    lease_require_ok(h.g);
    // Repeated actual Full relocation and cleanup must leave the descriptor
    // span, start bitmap and canonical root consistent on its new owner.
    request_full_collection(h.g);
    h.collect();
    h.finish_collection();
    let after = reference_lease(h.g);
    probe(&mut h, after, 1, 0);
    lease_require_ok(h.g);
    if bytes > 0 {
        h.report("value", |h| h.load(Src::Root(1), 0));
    }
    reference_transfer(h.g, after, true);
    lease_require_ok(h.g);
    h.report("empty", |h| lease_metric(h.g, 0));
}
#[test]
fn production_shared_reference_leases_validate_old_boundary_and_large_run_geometry() {
    for mode in [0, 16, 128, 256] {
        let o = run(geometry, mode, false);
        assert_eq!(o.get("shared"), 0, "{}", o.text());
        assert_eq!(o.get("active"), 1);
        assert_eq!(o.get("interior"), 1);
        assert_eq!(o.get("bad_token"), 0);
        assert_eq!(o.get("physical_inc"), o.get("head_inc"));
        let pages = (o.get("limit") - o.get("start")) / 131072;
        assert!(pages >= 2);
        assert_eq!(o.get("words"), pages * 128);
        assert!(o.get("top") > o.get("start"));
        assert!(o.get("top") <= o.get("limit"));
        assert_eq!(o.get("empty"), 0);
        if mode != 0 {
            assert_eq!(o.get("value"), 77);
        }
        if mode != 256 {
            assert_eq!(o.get("payload_mod"), 0);
            assert_eq!(o.get("tail_state"), 4);
        }
    }
}
