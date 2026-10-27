//! Budget reservations precede real OS maps and remain separate from payload.
use super::{H, Src, execute, run};
use crate::gc_codegen::production::metadata;
use crate::portable_asm::{PortableAsm, Reg as R};

fn map(h: &mut H<'_>, bytes: u64) {
    h.g.mov_imm64(R::V7, bytes);
    h.g.call_label(h.g.gc_gen.as_ref().unwrap().labels.metadata_map);
}
fn stats(h: &mut H<'_>) {
    for (name, offset) in [
        "limit",
        "initial",
        "charged",
        "committed",
        "maps",
        "failures",
    ]
    .into_iter()
    .zip(metadata::live_offsets())
    {
        h.report(name, |h| h.g.load_ptr_disp32(R::V0, R::V3, offset));
    }
}
fn retry(g: &mut crate::NativeCodeGenerator, overflow: u64) {
    let mut h = H::begin(g);
    let [limit, initial, charged, committed, _, _] = metadata::live_offsets();
    h.g.load_ptr_disp32(R::V0, R::V3, initial);
    h.g.add_reg_imm32(R::V0, 8192);
    h.g.store_ptr_disp32(R::V3, limit, R::V0);
    metadata::fail_next(h.g);
    map(&mut h, 4097);
    h.keep(29);
    h.report("failed_result", |h| h.value(Src::Raw(29)));
    for (name, offset) in [
        ("after_failure_charged", charged),
        ("after_failure_committed", committed),
    ] {
        h.report(name, |h| h.g.load_ptr_disp32(R::V0, R::V3, offset));
    }
    map(&mut h, 4097);
    h.keep(30);
    h.g.mov_imm64(R::V1, 84);
    h.g.store_ptr_disp32(R::V0, 8184, R::V1);
    h.report("address", |h| h.value(Src::Raw(30)));
    h.report("value", |h| {
        h.value(Src::Raw(30));
        h.g.load_ptr_disp32(R::V0, R::V0, 8184);
    });
    stats(&mut h);
    if overflow != 0 {
        map(&mut h, 1);
    }
}
#[test]
fn production_metadata_mapping_failure_rolls_back_charge_and_retry_fits_exact_budget() {
    let o = run(retry, 0, false);
    assert_eq!(o.get("failed_result"), 0, "{}", o.text());
    assert_eq!(o.get("after_failure_charged"), o.get("initial"));
    assert_eq!(o.get("after_failure_committed"), o.get("initial"));
    assert_eq!(o.get("charged"), o.get("initial") + 8192);
    assert_eq!(o.get("committed"), o.get("charged"));
    assert_eq!(o.get("limit"), o.get("charged"));
    assert_eq!(o.get("maps"), 2);
    assert_eq!(o.get("failures"), 1);
    assert!(o.get("address") > 0 && o.get("address") % 4096 == 0);
    assert_eq!(o.get("value"), 84);
}
#[test]
fn production_metadata_budget_refuses_the_next_map_after_an_exact_fit() {
    let output = execute(retry, 1, false, false);
    assert_eq!(output.status.code(), Some(1), "{output:?}");
    assert!(
        String::from_utf8_lossy(&output.stderr).contains("metadata budget exhausted"),
        "{output:?}"
    );
}
fn copies(g: &mut crate::NativeCodeGenerator, exhausted: u64) {
    let mut h = H::begin(g);
    if exhausted != 0 {
        let [limit, initial, ..] = metadata::live_offsets();
        h.g.load_ptr_disp32(R::V0, R::V3, initial);
        h.g.store_ptr_disp32(R::V3, limit, R::V0);
    }
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 84);
    h.root(0);
    for _ in 0..16 {
        h.collect();
        h.finish_collection();
    }
    h.report("value", |h| h.load(Src::Root(0), 0));
    h.report("backings", |h| metadata::copy_backings(h.g));
    h.report("copy_bytes", |h| {
        let bytes = metadata::copy_bytes(h.g);
        h.g.mov_imm64(R::V0, bytes);
    });
    for (name, offset) in ["unmaps", "release_failures"]
        .into_iter()
        .zip(metadata::release_offsets()[3..].iter().copied())
    {
        h.report(name, |h| h.g.load_ptr_disp32(R::V0, R::V3, offset));
    }
    stats(&mut h);
}
#[test]
fn production_metadata_copy_maps_match_owned_page_backings_and_preserve_values() {
    for worker in [false, true] {
        let o = run(copies, 0, worker);
        assert_eq!(o.get("value"), 84, "{}", o.text());
        assert!(o.get("backings") > 0);
        assert_eq!(o.get("maps") - o.get("unmaps"), 1 + o.get("backings"));
        assert!(o.get("unmaps") > 0);
        assert_eq!(o.get("release_failures"), 0);
        assert_eq!(
            o.get("committed"),
            o.get("initial") + o.get("backings") * o.get("copy_bytes")
        );
        assert_eq!(o.get("charged"), o.get("committed"));
        assert!(o.get("charged") <= o.get("limit"));
        assert_eq!(o.get("failures"), 0);
    }
}
#[test]
fn production_metadata_copying_obeys_the_budget_even_when_payload_space_is_free() {
    let output = execute(copies, 1, false, false);
    assert_eq!(output.status.code(), Some(1), "{output:?}");
    assert!(
        String::from_utf8_lossy(&output.stderr).contains("metadata budget exhausted"),
        "{output:?}"
    );
}
