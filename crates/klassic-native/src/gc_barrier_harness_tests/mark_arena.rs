//! Exhaust a small heap's real fresh-record bank, then reuse both banks.
use super::{H, Src, execute_with_heap_limits};
use crate::portable_asm::{PortableAsm, Reg as R};
use crate::{NativeCodeGenerator, gc_codegen::production::test_support::*};

// 21 hard Young pages, each holding floor(128 KiB / 48) minimum allocations.
const RECORDS: u64 = 57_330;
fn dense(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    let [records, capacity, used] = mark_arena_offsets();
    h.report("capacity", |h| h.g.load_ptr_disp32(R::V0, R::V3, capacity));
    h.report("bank0", |h| h.g.load_ptr_disp32(R::V0, R::V3, records));
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 84);
    h.root(0);
    h.repeat(RECORDS - 2, |h| {
        h.alloc(1, 16, 9);
        h.set_scalar(Src::Raw(9), 0, 77);
        h.root(9);
    });
    h.alloc(1, 16, 5);
    h.set_scalar(Src::Raw(5), 0, 99);
    h.root(5);
    h.report("used_at_limit", |h| h.g.load_ptr_disp32(R::V0, R::V3, used));
    h.report_counter("collections_at_limit", Counter::Collections);
    h.collect();
    h.finish_collection();
    h.report("bank1", |h| h.g.load_ptr_disp32(R::V0, R::V3, records));
    h.report("first_after_copy", |h| h.load(Src::Root(0), 0));
    h.report("last_after_copy", |h| h.load(Src::Root(5), 0));
    h.report("first_id", |h| h.object_id(Src::Root(0)));
    h.report("last_id", |h| h.object_id(Src::Root(5)));
    // Promote this dense set, freeing its fresh bank for another complete fill.
    h.collect();
    h.finish_collection();
    h.report("bank2", |h| h.g.load_ptr_disp32(R::V0, R::V3, records));
    h.report("cleared_used", |h| h.g.load_ptr_disp32(R::V0, R::V3, used));
    h.repeat(RECORDS, |h| {
        h.alloc(1, 16, 8);
        h.set_scalar(Src::Raw(8), 0, 55);
    });
    h.report("refilled_used", |h| h.g.load_ptr_disp32(R::V0, R::V3, used));
    h.report("old_first", |h| h.load(Src::Root(0), 0));
    h.report("old_last", |h| h.load(Src::Root(5), 0));
    h.report("fresh_last", |h| h.load(Src::Raw(8), 0));
    h.report_counter("worker_copies", Counter::WorkerCopies);
}
#[test]
fn production_scaled_mark_banks_fill_to_the_physical_young_limit_and_reuse_without_root_corruption()
{
    for worker in [false, true] {
        let output = execute_with_heap_limits(dense, 0, worker, false, None, Some(16 << 20));
        let text = String::from_utf8_lossy(&output.stdout);
        assert!(output.status.success(), "{output:?}");
        let get = |name: &str| -> u64 {
            text.lines()
                .filter_map(|l| l.split_once('='))
                .find(|(k, _)| *k == name)
                .unwrap()
                .1
                .parse()
                .unwrap()
        };
        assert_eq!(get("capacity"), RECORDS);
        if worker {
            // Concurrent collection can switch banks during the fill. Keep
            // the hard capacity/retained-value checks, and require real copying.
            assert!(get("used_at_limit") <= RECORDS, "{text}");
            assert!(get("refilled_used") <= RECORDS, "{text}");
            assert!(get("worker_copies") > 0, "{text}");
        } else {
            // Exact boundary/stride proof without asynchronous bank switches.
            assert_eq!(get("used_at_limit"), RECORDS, "{text}");
            assert_eq!(get("collections_at_limit"), 0);
            assert_eq!(get("bank1") - get("bank0"), RECORDS * 168);
            assert_eq!(get("bank2"), get("bank0"));
            assert_eq!(get("cleared_used"), 0);
            assert_eq!(get("refilled_used"), RECORDS);
        }
        for (name, value) in [
            ("first_after_copy", 84),
            ("last_after_copy", 99),
            ("first_id", 1),
            ("last_id", RECORDS),
            ("old_first", 84),
            ("old_last", 99),
            ("fresh_last", 55),
        ] {
            assert_eq!(get(name), value, "{name}: {text}");
        }
    }
}
