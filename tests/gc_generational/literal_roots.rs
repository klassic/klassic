//! User-facing high-level enum literal reads, deduplication and repeated stores.
use super::{gc_stats, linux_x86_64::build_and_run};

#[test]
fn generational_literal_cells_survive_churn_equality_and_repeated_function_calls() {
    for (n, flags) in [
        (350000, vec!["--gc=generational", "--gc-log"]),
        (30, vec!["--gc=generational", "--gc-log", "--gc-stress"]),
    ] {
        let source = format!(
            r#"
enum Opt {{ case Sm(v: Int); case Nn }}
def churn(n: Int): Int = {{
  mutable i = 0
  while(i < n) {{ val garbage = Sm(i); i += 1 }}
  n
}}
def show(n: Int): Unit = println([Sm(n) Nn])
println([Sm(1) Nn Sm(churn({n}))])
println(%(Sm(7) Sm(7) Nn))
println([Sm(1) Nn] == [Sm(1) Nn])
show(11)
show(12)
show(13)
"#
        );
        let output = build_and_run("literal_cells", &flags, &source);
        assert!(
            output.status.success(),
            "{}",
            String::from_utf8_lossy(&output.stderr)
        );
        assert_eq!(
            String::from_utf8_lossy(&output.stdout),
            format!(
                "[Sm(1), Nn, Sm({n})]\n%(Sm(7), Nn)\ntrue\n[Sm(11), Nn]\n[Sm(12), Nn]\n[Sm(13), Nn]\n"
            )
        );
        let stats = gc_stats(&String::from_utf8_lossy(&output.stderr));
        assert!(stats["collections"] > 0, "{stats:?}");
        assert!(
            stats["worker_copies"] + stats["mutator_copies"] > 0,
            "{stats:?}"
        );
    }
}
