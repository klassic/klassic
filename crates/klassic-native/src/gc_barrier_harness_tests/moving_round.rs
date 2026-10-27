//! Real concurrent graph mutators cross physical bank transitions themselves.
use super::{H, Src, run, slot_offset};
use crate::gc_codegen::production::test_support::*;
use crate::portable_asm::{PortableAsm, Reg as R};
fn moving_impl(
    g: &mut crate::NativeCodeGenerator,
    owners: u64,
    retain_journal: bool,
    delayed_native: bool,
    cycles: u64,
    full: bool,
    flow: (bool, u64),
) {
    let (late_allocation, scheduled_mode) = flow;
    let scheduled = scheduled_mode != 0 && scheduled_mode != 4 && scheduled_mode != 5;
    let iterations = if scheduled { 22_000 } else { 12 };
    let mut h = H::begin(g);
    h.alloc(2, owners * 8, 0);
    for i in 0..owners {
        h.store(Src::Raw(0), i as u32, Src::Null, true);
    }
    h.root(0);
    publish_primary(h.g);
    h.report("hub_id_before", |h| h.object_id(Src::Root(0)));
    let actors: Vec<_> = (0..owners).map(|_| create_actor(h.g, 16, 8)).collect();
    h.g.asm
        .data
        .resize(h.g.asm.data.len().next_multiple_of(8), 0);
    let root = h.g.asm.data_label_with_i64s(&[0]);
    h.g.load_rbp_slot(R::V0, slot_offset(0));
    h.g.mov_data_addr(R::V1, root);
    h.g.store_ptr_disp32(R::V1, 0, R::V0);
    if (2..=3).contains(&scheduled_mode) {
        delay_moving_scheduler(h.g, scheduled_mode == 2);
    }
    if scheduled && full {
        request_full_collection(h.g);
    }
    let p = if scheduled_mode == 5 {
        run_parallel_aborted_old_graph(h.g, &actors, 12, root)
    } else if scheduled_mode == 4 {
        run_parallel_preserved_old_graph(h.g, &actors, 12, root, cycles)
    } else if scheduled {
        run_parallel_scheduled_moving_graph(h.g, &actors, iterations, root)
    } else {
        run_parallel_moving_graph(
            h.g,
            &actors,
            iterations,
            root,
            (
                retain_journal,
                delayed_native,
                cycles,
                full,
                late_allocation,
            ),
        )
    };
    h.report("starts_before_idle_polls", |h| moving_metric(h.g, 0));
    if scheduled {
        moving_idle_polls(h.g);
    }
    for (i, name) in [
        "allocation_base",
        "scheduler_polls",
        "start_requests",
        "end_requests",
        "stale_starts",
        "stale_ends",
        "allocated",
    ]
    .into_iter()
    .enumerate()
    {
        h.report(name, |h| moving_scheduler_metric(h.g, i));
    }
    h.report("late_adoptions", |h| {
        parallel_old_worker(h.g, &p, owners as usize - 1)
    });
    h.report("selected_before", |h| {
        parallel_moving_selected(h.g, &p, 0, 96)
    });
    h.report("selected_after", |h| {
        parallel_moving_selected(h.g, &p, 0, 104)
    });
    h.report("retained_tlab_retries", |h| {
        parallel_moving_selected(h.g, &p, 0, 72)
    });
    h.report("ready", |h| parallel_actor_ready(h.g, &p));
    h.report("hub_id_after", |h| h.object_id(Src::Root(0)));
    for (i, name) in [
        "starts",
        "closes",
        "cleanups",
        "retries",
        "bank",
        "closed_bank",
        "open",
        "retiring",
        "worker_copies",
        "pending",
        "young_epoch",
        "start_expected",
        "start_acknowledged",
        "captured_debt",
        "opened_debt",
        "native_adoptions",
        "full_collections",
        "aborted_starts",
        "old_epoch",
        "old_active",
        "old_valid",
        "phase",
    ]
    .into_iter()
    .enumerate()
    {
        h.report(name, |h| moving_metric(h.g, i));
    }
    for i in 0..owners {
        h.report(&format!("suspends{i}"), |h| {
            moving_owner_tlab_metric(h.g, actors[i as usize], false)
        });
        h.report(&format!("restores{i}"), |h| {
            moving_owner_tlab_metric(h.g, actors[i as usize], true)
        });
        h.report(&format!("stale_start{i}"), |h| {
            moving_owner_stale(h.g, actors[i as usize], true)
        });
        h.report(&format!("stale_end{i}"), |h| {
            moving_owner_stale(h.g, actors[i as usize], false)
        });
        h.report(&format!("observed_bank{i}"), |h| {
            parallel_moving_selected(h.g, &p, i as usize, 96)
        });
        h.report(&format!("resumed_bank{i}"), |h| {
            parallel_moving_selected(h.g, &p, i as usize, 104)
        });
        h.report(&format!("selected_value{i}"), |h| {
            parallel_moving_selected(h.g, &p, i as usize, 112)
        });
        h.report(&format!("selected_id{i}"), |h| {
            parallel_moving_selected(h.g, &p, i as usize, 120)
        });
        h.report(&format!("value{i}"), |h| {
            h.value(Src::Root(0));
            h.g.add_reg_imm32(R::V0, (i * 8) as i32);
            h.g.mov_reg_reg(R::V10, R::V0);
            let field = h.g.gc_gen.as_ref().unwrap().labels.field_load_actor;
            h.g.atomic_load_acquire(R::V0, R::V10);
            call_owner(h.g, field);
            h.g.mov_reg_reg(R::V10, R::V0);
            h.g.atomic_load_acquire(R::V0, R::V10);
            call_owner(h.g, field);
            h.g.load_ptr_disp32(R::V0, R::V0, 0);
        });
        h.report(&format!("tid{i}"), |h| {
            parallel_actor_tid(h.g, &p, i as usize)
        });
    }
}
#[test]
fn production_real_running_mutators_cross_a_physical_moving_round() {
    for n in [1, 2, 8] {
        let o = run(moving, n, true);
        assert_eq!(o.get("ready"), n as i64, "{}", o.text());
        assert_eq!(o.get("start_expected"), n as i64);
        assert_eq!(o.get("start_acknowledged"), n as i64);
        assert_eq!(o.get("hub_id_before"), o.get("hub_id_after"));
        assert_eq!(o.get("starts"), 1);
        assert_eq!(o.get("closes"), 1);
        assert_eq!(o.get("cleanups"), 1);
        assert_eq!(o.get("bank"), 2);
        assert_eq!(o.get("closed_bank"), 2);
        assert_eq!(o.get("open"), 0);
        assert_eq!(o.get("retiring"), 0);
        assert!(o.get("young_epoch") > 0);
        let mut tids = std::collections::HashSet::new();
        for i in 0..n {
            assert!(tids.insert(o.get(&format!("tid{i}"))));
            assert_eq!(o.get(&format!("value{i}")), (((i + 2) << 32) | 11) as i64);
        }
    }
}

fn moving(g: &mut crate::NativeCodeGenerator, owners: u64) {
    moving_impl(g, owners, false, false, 1, false, (false, 0));
}
fn moving_logged(g: &mut crate::NativeCodeGenerator, owners: u64) {
    g.gc_log = true;
    g.gc_log_format = crate::GcLogFormat::Json;
    g.gc_trace_capacity = 256;
    moving(g, owners);
}
fn moving_text_logged(g: &mut crate::NativeCodeGenerator, owners: u64) {
    g.gc_log = true;
    moving(g, owners);
}
#[test]
fn production_text_logging_with_running_owners() {
    let output = super::execute(moving_text_logged, 2, true, false);
    assert!(output.status.success(), "{output:?}");
}
#[test]
fn production_rendezvous_history_counts_actual_running_owners() {
    for owners in [2, 8] {
        let output = super::execute(moving_logged, owners, true, false);
        assert!(output.status.success(), "{output:?}");
        let text = String::from_utf8(output.stderr).unwrap();
        let records: Vec<_> = text
            .lines()
            .filter(|s| s.contains("\"event\":\"rendezvous\""))
            .collect();
        assert!(
            records
                .iter()
                .any(|s| s.contains(&format!("\"participants\":{owners},"))
                    && s.contains("\"clock_status\":7")),
            "{text}"
        );
        let parks: Vec<_> = text
            .lines()
            .filter(|s| s.contains("\"event\":\"mutator_park\""))
            .collect();
        let ids: std::collections::HashSet<u64> = parks
            .iter()
            .filter(|s| s.contains("\"clock_status\":3"))
            .map(|s| {
                s.split_once("\"mutator_id\":")
                    .unwrap()
                    .1
                    .split(',')
                    .next()
                    .unwrap()
                    .parse()
                    .unwrap()
            })
            .collect();
        assert!(ids.len() >= owners as usize && !ids.contains(&0), "{text}");
        let field = |line: &str, name: &str| -> u64 {
            line.split_once(&format!("\"{name}\":"))
                .unwrap()
                .1
                .split([',', '}'])
                .next()
                .unwrap()
                .parse()
                .unwrap()
        };
        assert!(
            parks.iter().all(|line| {
                field(line, "clock_status") == 3
                    && field(line, "timestamp_ns") >= field(line, "started_ns")
                    && field(line, "duration_ns")
                        == field(line, "timestamp_ns") - field(line, "started_ns")
            }),
            "{text}"
        );
    }
}
fn retained(g: &mut crate::NativeCodeGenerator, owners: u64) {
    moving_impl(g, owners, true, false, 1, false, (false, 0));
}
#[test]
fn production_real_root_journals_survive_the_physical_bank_switch() {
    for n in [1, 2, 8] {
        let o = run(retained, n, true);
        assert!(o.get("captured_debt") >= n as i64, "{}", o.text());
        assert!(o.get("opened_debt") >= n as i64, "{}", o.text());
        assert!(o.get("retries") > 0, "{}", o.text());
        assert!(o.get("retained_tlab_retries") > 0, "{}", o.text());
        assert_eq!(o.get("pending"), 0);
        assert_eq!(o.get("start_acknowledged"), n as i64);
        assert_eq!(o.get("closes"), 1);
        assert_eq!(o.get("cleanups"), 1);
        assert_eq!(o.get("hub_id_before"), o.get("hub_id_after"));
        for i in 0..n {
            assert_eq!(o.get(&format!("value{i}")), (((i + 2) << 32) | 11) as i64);
        }
    }
}

fn delayed(g: &mut crate::NativeCodeGenerator, owners: u64) {
    moving_impl(g, owners, false, true, 1, false, (false, 0));
}
#[test]
fn production_native_owner_returns_inside_a_live_moving_window() {
    for n in [2, 8] {
        let o = run(delayed, n, true);
        assert_eq!(o.get("start_expected"), n as i64 - 1, "{}", o.text());
        assert_eq!(o.get("start_acknowledged"), n as i64 - 1);
        assert_eq!(o.get("late_adoptions"), 1, "{}", o.text());
        assert!(o.get("native_adoptions") >= 1);
        assert_eq!(o.get("closes"), 1);
        assert_eq!(o.get("cleanups"), 1);
        assert_eq!(o.get("hub_id_before"), o.get("hub_id_after"));
        for i in 0..n {
            assert_eq!(o.get(&format!("value{i}")), (((i + 2) << 32) | 11) as i64);
        }
    }
}

fn repeated(g: &mut crate::NativeCodeGenerator, input: u64) {
    moving_impl(
        g,
        input & 15,
        false,
        false,
        (input >> 4) & 15,
        input & 256 != 0,
        (false, 0),
    );
}
#[test]
fn production_running_mutators_cross_repeated_young_and_full_moves() {
    for full in [false, true] {
        for cycles in [2, 4] {
            for n in [1, 2, 8] {
                let o = run(
                    repeated,
                    n | (cycles << 4) | if full { 256 } else { 0 },
                    true,
                );
                assert_eq!(o.get("starts"), cycles as i64, "{}", o.text());
                if full {
                    assert_eq!(o.get("full_collections"), cycles as i64);
                }
                assert_eq!(o.get("closes"), cycles as i64);
                assert_eq!(o.get("cleanups"), cycles as i64);
                assert_eq!(o.get("bank"), cycles as i64 + 1);
                assert_eq!(o.get("closed_bank"), cycles as i64 + 1);
                assert_eq!(o.get("start_expected"), n as i64);
                assert_eq!(o.get("start_acknowledged"), n as i64);
                assert_eq!(o.get("hub_id_before"), o.get("hub_id_after"));
                for i in 0..n {
                    assert_eq!(o.get(&format!("value{i}")), (((i + 2) << 32) | 11) as i64);
                }
            }
        }
    }
}

fn fault(g: &mut crate::NativeCodeGenerator, mode: u64) {
    moving_impl(g, 2, false, false, 4, false, (false, 0));
    moving_journal_fault(g, mode);
}
#[test]
fn production_moving_journal_rejects_reused_closed_or_invalid_source_snapshots() {
    for mode in 0..7 {
        let o = super::execute(fault, mode, true, false);
        assert_eq!(o.status.code(), Some(1), "mode={mode}: {o:?}");
        assert!(
            String::from_utf8_lossy(&o.stderr).contains("runtime invariant violated"),
            "mode={mode}: {o:?}"
        );
    }
}

fn delayed_repeated(g: &mut crate::NativeCodeGenerator, input: u64) {
    moving_impl(
        g,
        input & 15,
        false,
        true,
        (input >> 4) & 15,
        input & 256 != 0,
        (false, 0),
    );
}
#[test]
fn production_native_owner_returns_into_a_later_open_move_after_bank_reuse() {
    for full in [false, true] {
        for cycles in [2, 4] {
            for n in [2, 8] {
                let o = run(
                    delayed_repeated,
                    n | (cycles << 4) | if full { 256 } else { 0 },
                    true,
                );
                assert_eq!(o.get("start_expected"), n as i64 - 1, "{}", o.text());
                assert_eq!(o.get("start_acknowledged"), n as i64 - 1);
                assert_eq!(o.get("late_adoptions"), 1);
                assert_eq!(o.get("starts"), cycles as i64);
                assert_eq!(o.get("closes"), cycles as i64);
                assert_eq!(o.get("cleanups"), cycles as i64);
                assert_eq!(o.get("bank"), cycles as i64 + 1);
                if full {
                    assert_eq!(o.get("full_collections"), cycles as i64);
                }
                assert_eq!(o.get("hub_id_before"), o.get("hub_id_after"));
                for i in 0..n {
                    assert_eq!(o.get(&format!("value{i}")), (((i + 2) << 32) | 11) as i64);
                }
            }
        }
    }
}

fn post_select(g: &mut crate::NativeCodeGenerator, input: u64) {
    moving_impl(g, input & 15, true, false, 1, input & 256 != 0, (true, 0));
}
#[test]
fn production_post_select_allocations_are_sealed_and_survive_real_moving_rounds() {
    for full in [false, true] {
        for n in [1, 2, 8] {
            let o = run(post_select, n | if full { 256 } else { 0 }, true);
            assert_eq!(
                o.get("selected_after") - o.get("selected_before"),
                48 * n as i64,
                "{}",
                o.text()
            );
            assert_eq!(o.get("start_acknowledged"), n as i64);
            assert_eq!(o.get("closes"), 1);
            assert_eq!(o.get("cleanups"), 1);
            if full {
                assert_eq!(o.get("full_collections"), 1);
            }
            let mut ids = std::collections::HashSet::new();
            for i in 0..n {
                assert_eq!(
                    o.get(&format!("selected_value{i}")),
                    (((i + 2) << 32) | 85) as i64
                );
                assert!(ids.insert(o.get(&format!("selected_id{i}"))));
                assert!(o.get(&format!("selected_id{i}")) > 0);
                assert_eq!(o.get(&format!("value{i}")), (((i + 2) << 32) | 11) as i64);
            }
        }
    }
}

fn scheduled(g: &mut crate::NativeCodeGenerator, input: u64) {
    moving_impl(g, input & 15, false, false, 0, input & 256 != 0, (false, 1));
}
#[test]
fn production_ordinary_allocations_schedule_and_finish_physical_moving_cycles() {
    let mut old_started = false;
    for full in [false, true] {
        for n in [1, 2, 8] {
            let o = run(scheduled, n | if full { 256 } else { 0 }, true);
            assert!(o.get("starts") >= 2, "{}", o.text());
            assert!(o.get("start_requests") > 0, "{}", o.text());
            assert!(o.get("end_requests") > 0);
            old_started |= o.get("old_epoch") > 0;
            // Polling may start eligible independent Old work without any
            // new physical Young cycle. Check its real engine/phase instead.
            if o.get("old_active") != 0 {
                assert_eq!(o.get("old_active"), 1);
                assert!(o.get("old_epoch") > 0);
                assert_eq!((o.get("phase") >> 4) & 15, 2);
            }
            assert_eq!(o.get("starts_before_idle_polls"), o.get("starts"));
            assert_eq!(o.get("starts"), o.get("closes"));
            assert_eq!(o.get("starts"), o.get("cleanups"));
            assert_eq!(o.get("bank"), o.get("starts") + 1);
            assert_eq!(o.get("closed_bank"), o.get("bank"));
            assert_eq!(o.get("open"), 0);
            assert_eq!(o.get("retiring"), 0);
            assert_eq!(o.get("hub_id_before"), o.get("hub_id_after"));
            assert!(o.get("allocation_base") <= o.get("allocated"));
            if full {
                assert!(o.get("full_collections") >= 1);
            }
            for i in 0..n {
                assert!(o.get(&format!("suspends{i}")) > 0);
                assert_eq!(
                    o.get(&format!("suspends{i}")),
                    o.get(&format!("restores{i}"))
                );
                assert_eq!(
                    o.get(&format!("value{i}")),
                    (((i + 2) << 32) | 21_999) as i64
                );
            }
        }
    }
    assert!(
        old_started,
        "ordinary idle-bank polls must schedule an eligible Old snapshot"
    );
}

fn delayed_scheduled(g: &mut crate::NativeCodeGenerator, input: u64) {
    moving_impl(
        g,
        input & 15,
        false,
        false,
        0,
        false,
        (false, if input & 256 == 0 { 2 } else { 3 }),
    );
}
#[test]
fn production_delayed_moving_requests_keep_their_observed_bank_and_do_not_replay() {
    for start in [true, false] {
        for n in [2, 8] {
            let o = run(delayed_scheduled, n | if start { 0 } else { 256 }, true);
            let delayed: Vec<_> = (0..n)
                .filter(|i| o.get(&format!("observed_bank{i}")) != 0)
                .collect();
            assert_eq!(delayed.len(), 1, "{}", o.text());
            let i = delayed[0];
            assert_eq!(
                o.get(&format!("observed_bank{i}")),
                if start { 1 } else { 2 },
                "{}",
                o.text()
            );
            assert!(o.get(&format!("resumed_bank{i}")) > o.get(&format!("observed_bank{i}")));
            assert!(
                o.get(&format!("stale_{}{i}", if start { "start" } else { "end" })) >= 1,
                "{}",
                o.text()
            );
            assert_eq!(o.get("starts"), o.get("closes"));
            assert_eq!(o.get("starts"), o.get("cleanups"));
            assert_eq!(o.get("hub_id_before"), o.get("hub_id_after"));
            for i in 0..n {
                assert!(o.get(&format!("suspends{i}")) > 0);
                assert_eq!(
                    o.get(&format!("suspends{i}")),
                    o.get(&format!("restores{i}"))
                );
                assert_eq!(
                    o.get(&format!("value{i}")),
                    (((i + 2) << 32) | 21_999) as i64
                );
            }
        }
    }
}

fn preserved_old(g: &mut crate::NativeCodeGenerator, input: u64) {
    moving_impl(g, input & 15, false, false, input >> 4, false, (false, 4));
}
#[test]
fn production_running_mutators_preserve_one_old_snapshot_across_repeated_young_moves() {
    for cycles in [1, 2, 4] {
        for n in [1, 2, 8] {
            let o = run(preserved_old, n | (cycles << 4), true);
            assert_eq!(o.get("starts"), cycles as i64 + 1, "{}", o.text());
            assert_eq!(o.get("closes"), cycles as i64 + 1);
            assert_eq!(o.get("cleanups"), cycles as i64 + 1);
            assert_eq!(o.get("full_collections"), 1);
            assert_eq!(o.get("hub_id_before"), o.get("hub_id_after"));
            for i in 0..n {
                assert_eq!(o.get(&format!("observed_bank{i}")), 1);
                assert_eq!(o.get(&format!("resumed_bank{i}")), cycles as i64);
                assert_eq!(o.get(&format!("value{i}")), (((i + 2) << 32) | 11) as i64);
            }
        }
    }
}

fn late_full(g: &mut crate::NativeCodeGenerator, owners: u64) {
    moving_impl(g, owners, false, false, 1, false, (false, 5));
}
#[test]
fn production_late_full_request_reopens_the_unchanged_bank_before_finishing_old() {
    for n in [1, 2, 8] {
        let o = run(late_full, n, true);
        assert_eq!(o.get("aborted_starts"), 1, "{}", o.text());
        assert_eq!(o.get("starts"), 1);
        assert_eq!(o.get("bank"), 2);
        assert_eq!(o.get("full_collections"), 1);
        assert_eq!(o.get("closes"), 1);
        assert_eq!(o.get("cleanups"), 1);
        assert_eq!(o.get("start_acknowledged"), n as i64);
        assert_eq!(o.get("hub_id_before"), o.get("hub_id_after"));
        for i in 0..n {
            assert_eq!(o.get(&format!("value{i}")), (((i + 2) << 32) | 11) as i64);
        }
    }
}

fn moving_wait_deadline(g: &mut crate::NativeCodeGenerator, input: u64) {
    let mut h = H::begin(g);
    moving_wait_probe(h.g, input != 0);
    h.report("returned", |h| h.value(Src::Imm(1)));
}
#[test]
fn production_moving_fixture_deadline_requires_real_work_progress() {
    let rejected = super::run_failure(moving_wait_deadline, 0);
    assert!(!rejected.status.success());
    assert!(String::from_utf8_lossy(&rejected.stderr).contains("runtime invariant violated"));
    let advanced = run(moving_wait_deadline, 1, false);
    assert_eq!(advanced.get("returned"), 1);
}
