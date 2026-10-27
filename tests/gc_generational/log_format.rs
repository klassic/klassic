//! Parse emitted snapshots with an independent JSON parser, including full u64 configuration.
use super::{
    gc_stats,
    metadata::{CHURN, Fixture},
};
use std::collections::HashMap;
use std::io::Write;
use std::process::{Command, Stdio};

pub(super) fn json_stats(text: &str) -> HashMap<String, u64> {
    let mut child = Command::new("python3")
        .args([
            "-c",
            r#"
import json,sys
s=sys.stdin.read()
assert s.endswith("\n"), repr(s)
def unique(pairs):
    d={}
    for k,v in pairs:
        assert k not in d, k
        d[k]=v
    return d
rows=[json.loads(line,object_pairs_hook=unique) for line in s.splitlines()]
assert rows and rows[-1]['event']=='exit_snapshot'
d=rows[-1]
assert d.pop('schema_version') == 1
assert d.pop('collector') == 'generational'
assert d.pop('event') == 'exit_snapshot'
assert len(d) > 90
previous={1:(0,0,0),2:(0,0,0)}
latest_ns=0
latest_cycle=0
relocations=0
overlap=0
intervals=[]
previous_end=0
for index,e in enumerate(rows[:-1],1):
    assert e['schema_version']==1 and e['collector']=='generational'
    assert all(type(e[k]) is int and 0<=e[k]<(1<<63) for k in e if k not in ('collector','event'))
    assert e['sequence']==index
    assert e['timestamp_ns']>=latest_ns and e['timestamp_ns']>0,e
    latest_ns=e['timestamp_ns']
    if e['event']=='mutator_park':
        assert set(e)=={'schema_version','collector','event','sequence','timestamp_ns','round_id','started_ns','duration_ns','mutator_id','clock_status'},e
        assert e['round_id']>0 and e['mutator_id']>0 and e['clock_status']==3,e
        assert e['duration_ns']==e['timestamp_ns']-e['started_ns'],e
        continue
    if e['event']=='rendezvous':
        assert set(e)=={'schema_version','collector','event','sequence','timestamp_ns','round_id','time_to_safepoint_ns','hold_until_end_ns','participants','clock_status'},e
        assert e['round_id']>0 and e['participants']>0 and e['clock_status']==7,e
        assert e['time_to_safepoint_ns']+e['hold_until_end_ns']<=e['timestamp_ns'],e
        continue
    if e['event']=='mutator_collection':
        assert set(e)=={'schema_version','collector','event','mutator_id','sequence','timestamp_ns','cycle_id','started_ns','duration_ns','start_clock_valid','clock_status'},e
        assert e['mutator_id']==1 and e['clock_status']==3 and e['start_clock_valid']==1,e
        assert e['cycle_id']==previous[1][1] and e['cycle_id']>0,e
        assert previous_end<=e['started_ns']<=e['timestamp_ns'],e
        assert e['duration_ns']==e['timestamp_ns']-e['started_ns'],e
        previous_end=e['timestamp_ns']
        intervals.append(e['duration_ns'])
        continue
    assert set(e)=={'schema_version','collector','event','sequence','timestamp_ns','cycle_id','generation','phase','previous_phase','elapsed_phase_ns','clock_status'},e
    assert e['event']=='phase_transition'
    assert e['generation'] in (1,2) and 0<=e['phase']<=7
    old_phase,old_cycle,old_ns=previous[e['generation']]
    assert old_phase==e['previous_phase'] and old_phase!=e['phase'],e
    if old_cycle==0 or e['phase']==1:
        assert e['cycle_id']>latest_cycle,e
        latest_cycle=e['cycle_id']
    else:
        assert e['cycle_id']==old_cycle,e
    assert e['clock_status'] in (1,3),e
    if e['clock_status']==3:
        assert old_ns>0 and e['elapsed_phase_ns']==e['timestamp_ns']-old_ns,e
    else:
        assert old_ns==0 and e['elapsed_phase_ns']==0,e
    previous[e['generation']]=(e['phase'],0 if e['phase']==0 else e['cycle_id'],e['timestamp_ns'])
    if e['generation']==1 and e['phase']==6:
        relocations+=1
        if previous[2][0]==2: overlap+=1
assert d['trace_events']==len(rows)-1 and d['trace_capacity']==131072
assert d['trace_dropped']==0 and d['trace_clock_invalid']==0
assert relocations==d['collections'],(relocations,d['collections'])
assert len(intervals)==d['collections']==d['mutator_collect_calls']==d['mutator_collect_valid']
assert d['mutator_collect_invalid']==d['mutator_collect_clock_invalid']==0
assert sum(intervals)==d['mutator_collect_total_ns']
assert max(intervals,default=0)==d['mutator_collect_max_ns']
assert (intervals[-1] if intervals else 0)==d['mutator_collect_last_ns']
assert d['mutator_collect_last_status']==(3 if intervals else 0)
if d['collections']>0:
    assert any(e.get('generation')==2 for e in rows[:-1]) and overlap>0
print('events='+str(len(rows)-1)+' young='+str(sum(e.get('generation')==1 for e in rows[:-1]))+' old='+str(sum(e.get('generation')==2 for e in rows[:-1]))+' overlap='+str(overlap),file=sys.stderr)
for k,v in d.items():
    assert type(v) is int and 0 <= v <= (1<<64)-1, (k,v)
    print(k+'='+str(v))

"#,
        ])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();
    child
        .stdin
        .take()
        .unwrap()
        .write_all(text.as_bytes())
        .unwrap();
    let output = child.wait_with_output().unwrap();
    eprintln!(
        "trace-validation: {}",
        String::from_utf8_lossy(&output.stderr).trim()
    );
    assert!(output.status.success(), "JSON parser: {output:?}\n{text}");
    String::from_utf8(output.stdout)
        .unwrap()
        .lines()
        .map(|s| {
            let (k, v) = s.split_once('=').unwrap();
            (k.to_owned(), v.parse().unwrap())
        })
        .collect()
}

#[test]
fn log_format_json_is_parseable_and_matches_text_snapshot_fields_on_all_direct_targets() {
    for target in [
        "x86_64-unknown-linux-gnu",
        "x86_64-pc-windows-msvc",
        "aarch64-apple-darwin",
    ] {
        let Some(f) = Fixture::new(target) else {
            continue;
        };
        for (source, expected) in [("println(42)", "42\n"), (CHURN, "5120\n32\n")] {
            let mut snapshots = Vec::new();
            for format in ["text", "json"] {
                let output = f.run_log_format(source, format);
                assert!(output.status.success(), "{target}/{format}: {output:?}");
                assert_eq!(output.stdout, expected.as_bytes());
                let text = String::from_utf8(output.stderr).unwrap();
                let v = if format == "json" {
                    json_stats(&text)
                } else {
                    gc_stats(&text)
                };
                assert_eq!(v["mutator_collect_calls"], v["collections"]);
                assert_eq!(v["mutator_collect_valid"], v["collections"]);
                assert_eq!(v["mutator_collect_invalid"], 0);
                assert_eq!(v["mutator_collect_clock_invalid"], 0);
                assert_eq!(
                    v["mutator_collect_last_status"],
                    if v["collections"] > 0 { 3 } else { 0 }
                );
                assert!(v["mutator_collect_last_ns"] <= v["mutator_collect_max_ns"]);
                assert!(v["mutator_collect_max_ns"] <= v["mutator_collect_total_ns"]);
                assert_eq!(v["heap_uncommit_delay_ns"], u64::MAX);
                assert_eq!(v["heap_min_committed_bytes"], 128 << 10);
                assert_eq!(v["heap_initial_committed_bytes"], 128 << 10);
                assert_eq!(v["heap_capacity_bytes"], 64 << 20);
                assert_eq!(v["heap_soft_max_bytes"], 128 << 10);
                assert_eq!(
                    v["heap_pretouched_bytes"],
                    v["heap_commit_pages"] * (128 << 10)
                );
                assert_eq!(v["metadata_pending_bytes"], 0);
                assert_eq!(v["metadata_charged_bytes"], v["metadata_committed_bytes"]);
                assert!(v["metadata_committed_bytes"] <= v["metadata_limit_bytes"]);
                if source == CHURN {
                    assert!(v["collections"] > 0);
                }
                eprintln!(
                    "log-format: target={target} format={format} fields={} collections={}",
                    v.len(),
                    v["collections"]
                );
                snapshots.push(v);
            }
            assert_eq!(
                snapshots[0]
                    .keys()
                    .collect::<std::collections::BTreeSet<_>>(),
                snapshots[1].keys().collect()
            );
            assert_eq!(
                snapshots[1]["metadata_initial_bytes"],
                snapshots[0]["metadata_initial_bytes"] + (8 << 20)
            );
            assert_eq!(
                snapshots[1]["metadata_limit_bytes"],
                snapshots[0]["metadata_limit_bytes"] + (8 << 20)
            );
            assert_eq!(snapshots[0]["trace_events"], 0);
            assert_eq!(snapshots[0]["trace_capacity"], 0);
            for k in [
                "fresh_allocated_bytes",
                "heap_min_committed_bytes",
                "heap_capacity_bytes",
            ] {
                assert_eq!(snapshots[0][k], snapshots[1][k], "{target}: {k}");
            }
        }
    }
}

#[test]
fn phase_trace_metadata_history_requires_real_budget_before_program_start() {
    for target in [
        "x86_64-unknown-linux-gnu",
        "x86_64-pc-windows-msvc",
        "aarch64-apple-darwin",
    ] {
        let Some(f) = Fixture::new(target) else {
            continue;
        };
        let text = f.run_log_format("println(42)", "text");
        assert!(text.status.success());
        let v = gc_stats(&String::from_utf8_lossy(&text.stderr));
        let failed = f.run_log_budget("println(42)", "json", v["metadata_initial_bytes"]);
        assert_eq!(failed.status.code(), Some(1), "{target}: {failed:?}");
        assert!(failed.stdout.is_empty());
        let diag = String::from_utf8_lossy(&failed.stderr);
        assert!(
            diag.contains("metadata budget exhausted"),
            "{target}: {diag}"
        );
        assert!(!diag.contains("exit_snapshot"));
        eprintln!(
            "trace-budget: target={target} base={} failed=1",
            v["metadata_initial_bytes"]
        );
    }
}
