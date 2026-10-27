//! Ordinary source exercises real registered roots across frames and movement.
use super::*;
use klassic_native::gc_abi::{MutatorContext, RootCell, RootRegistration, RootSegment};
use std::mem::{offset_of, size_of};

const FRAMES: &str = r#"
def large(s: String, n: Int): String = if (n == 0) s else large(s + s, n - 1)
def walk(s: String, n: Int, report: Boolean): Int = {
  val local = s + "x"
  if (n == 0) {
    if (report) {
      mutable k = 0
      while (k < 3) { val garbage = large("g", 21); k += 1 }
      println(101)
    }
    length(local)
  } else {
    val result = walk(local, n - 1, report)
    if (report) println(102)
    result + length(local)
  }
}
println(walk("a", 5, true))
mutable i = 0
while (i < 64) { walk("z", 5, false); i += 1 }
{
 val xs = split("a,b,c", ",")
 foreach(x in xs) { println(length(x)) }
}
println(8595182864)
println(Process#exit != null)
println(Process#exit == Process#exit)
println(Process#exit != Process#exit)
println(103)
"#;

const SCRATCH: &str = r#"
def grow(s: String, n: Int): String = if (n == 0) s else grow(s + "x", n - 1)
val combine = (a, b) => a + "|" + b
val kept = grow("keep", 2)
mutable current = kept
mutable i = 0
while (i < 40) {
  val pair = combine(grow("a", 2), grow("b", 3))
  current = replaceAll(pair, "x", "y")
  i += 1
}
println(kept)
println(current)
println(split(current, "|"))
println(split("ab", ""))
foreach(part in split("a,b", ",")) { println(part) }
"#;

#[test]
fn aarch64_frame_roots_register_live_recursion_and_reuse_cells_after_scope_exit() {
    let expected = evaluate("frame_roots_eval", FRAMES);
    let Some(python) = super::aarch64_macho::emulator_python() else {
        panic!("precise frame-root observation requires the Darwin API model");
    };
    let layout = [
        ("context_roots", offset_of!(MutatorContext, root_segments)),
        (
            "registration_context",
            offset_of!(RootRegistration, context),
        ),
        (
            "registration_segment",
            offset_of!(RootRegistration, segment),
        ),
        (
            "registration_next",
            offset_of!(RootRegistration, owner_next),
        ),
        ("segment_cells", offset_of!(RootSegment, cells)),
        ("segment_used", offset_of!(RootSegment, used)),
        ("segment_capacity", offset_of!(RootSegment, capacity)),
        ("cell_bytes", size_of::<RootCell>()),
        ("cell_value", offset_of!(RootCell, value)),
        ("cell_active", offset_of!(RootCell, active)),
    ]
    .into_iter()
    .map(|(name, offset)| format!("\"{name}\":{offset}"))
    .collect::<Vec<_>>()
    .join(",");
    let layout = format!("{{{layout}}}");
    for stress in [false, true] {
        let source = write_source("frame_roots", FRAMES);
        let image = temp_path("frame_roots", "macho");
        let mut flags = vec![
            "--target",
            "aarch64-apple-darwin",
            "--gc=generational",
            "--gc-log",
            "--gc-max-heap=16m",
        ];
        if stress {
            flags.push("--gc-stress");
        }
        let built = build(&flags, &source, &image);
        fs::remove_file(source).unwrap();
        assert!(built.status.success(), "{built:?}");
        let run = Command::new(&python)
            .arg(Path::new(env!("CARGO_MANIFEST_DIR")).join("tools/run-macho-aarch64.py"))
            .arg(&image)
            .env("KLASSIC_MACHO_ROOTCELL_LAYOUT", &layout)
            .env("KLASSIC_MACHO_REQUIRE_NATIVE_CONSOLE", "1")
            .env("KLASSIC_MACHO_WORKER_QUANTUM", "1000000")
            .env("KLASSIC_MACHO_WORKER_SECONDS", "300")
            .output()
            .unwrap();
        if std::env::var_os("KLASSIC_FRAME_ROOT_IMAGE_DIR").is_some() {
            let directory =
                PathBuf::from(std::env::var_os("KLASSIC_FRAME_ROOT_IMAGE_DIR").unwrap());
            fs::create_dir_all(&directory).unwrap();
            fs::copy(&image, directory.join(format!("frame-{stress}.macho"))).unwrap();
            fs::write(
                directory.join(format!("frame-{stress}.stderr")),
                &run.stderr,
            )
            .unwrap();
        }
        fs::remove_file(image).unwrap();
        assert!(run.status.success(), "{run:?}");
        assert_eq!(run.stdout, expected.as_bytes());
        let err = String::from_utf8(run.stderr).unwrap();
        let samples: Vec<_> = err
            .lines()
            .filter_map(|l| l.strip_prefix("frame-root-model: "))
            .map(|line| {
                let mut fields = line.split_whitespace();
                let data = fields
                    .next()
                    .unwrap()
                    .strip_prefix("data=")
                    .unwrap()
                    .to_string();
                let used: u64 = fields
                    .next()
                    .unwrap()
                    .strip_prefix("used=")
                    .unwrap()
                    .parse()
                    .unwrap();
                let live: u64 = fields
                    .next()
                    .unwrap()
                    .strip_prefix("live=")
                    .unwrap()
                    .parse()
                    .unwrap();
                let frame_live: u64 = fields
                    .next()
                    .unwrap()
                    .strip_prefix("frame_live=")
                    .unwrap()
                    .parse()
                    .unwrap();
                assert!(fields.next().is_none());
                (data, used, live, frame_live)
            })
            .collect();
        let deep = samples.iter().find(|v| v.0 == "3130310a").unwrap();
        assert!(deep.2 >= 12, "{samples:?}");
        let unwind: Vec<_> = samples
            .iter()
            .filter(|v| v.0 == "3130320a")
            .map(|v| v.2)
            .collect();
        assert_eq!(unwind.len(), 5);
        assert!(unwind.windows(2).all(|p| p[0] > p[1]), "{samples:?}");
        let last = samples.iter().find(|v| v.0 == "3c657869743e").unwrap();
        assert!(last.2 <= 1, "{samples:?}");
        assert_eq!(last.3, 0, "frame roots leaked: {samples:?}");
        assert_eq!(last.1, deep.1, "cells grew after warmup: {samples:?}");
        assert!(samples.iter().all(|v| v.1 <= 64), "{samples:?}");
        let stats = gc_stats(&err);
        assert!(stats["full_collections"] > 0, "{stats:?}");
        eprintln!(
            "frame-root-profile stress={stress} deep={} last={} used={} unwind={unwind:?} full={}",
            deep.2,
            last.2,
            samples.iter().map(|v| v.1).max().unwrap(),
            stats["full_collections"]
        );
        if stress {
            assert!(stats["collections"] > 400);
        }
        assert_eq!(stats["objects"], stats["published"]);
    }
}

#[test]
fn aarch64_precise_frame_roots_keep_mutable_values_lambda_arguments_and_string_scratch_alive() {
    let expected = evaluate("frame_scratch_eval", SCRATCH);
    for stress in [false, true] {
        let mut flags = vec!["--gc=generational", "--gc-log"];
        if stress {
            flags.push("--gc-stress");
        }
        let run = super::aarch64_macho::build_and_emulate("frame_scratch", &flags, SCRATCH)
            .expect("unicorn required");
        assert!(run.status.success(), "{run:?}");
        assert_eq!(run.stdout, expected.as_bytes());
        let stats = gc_stats(&String::from_utf8_lossy(&run.stderr));
        if stress {
            assert!(stats["collections"] > 700);
        }
        assert_eq!(stats["objects"], stats["published"]);
    }
}
