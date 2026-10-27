//! Retain SATB fixture binaries through Rust-side semantic assertions too.
use super::{ImageRetention, Observed, execute_with_image_retention};
use crate::NativeCodeGenerator;
use std::path::PathBuf;
use std::process::Output;

pub(super) struct ImageGuard {
    path: PathBuf,
    pub(super) output: Output,
    arg: u64,
    worker: bool,
}
impl Drop for ImageGuard {
    fn drop(&mut self) {
        if std::thread::panicking() {
            eprintln!(
                "SATB fixture executable retained: {} (arg={}, worker={}, status={:?})\nstdout:\n{}\nstderr:\n{}",
                self.path.display(),
                self.arg,
                self.worker,
                self.output.status,
                String::from_utf8_lossy(&self.output.stdout),
                String::from_utf8_lossy(&self.output.stderr),
            );
        } else {
            let _ = std::fs::remove_file(&self.path);
        }
    }
}

/// Bind the returned guard for the complete assertion scope. A successful
/// executable can still violate semantic assertions in the Rust caller.
pub(super) fn run(
    body: fn(&mut NativeCodeGenerator, u64),
    arg: u64,
    worker: bool,
) -> (Observed, ImageGuard) {
    let guard = capture(body, arg, worker);
    let text = String::from_utf8_lossy(&guard.output.stdout).into_owned();
    if std::env::var_os("KLASSIC_HARNESS_DUMP").is_some() {
        eprintln!("--- SATB harness output ---\n{text}");
    }
    assert!(
        guard.output.status.success(),
        "SATB harness failed ({:?}, arg={arg}, worker={worker})",
        guard.output.status,
    );
    let values = text
        .lines()
        .filter_map(|line| line.split_once('='))
        .filter_map(|(name, value)| Some((name.to_string(), value.trim().parse().ok()?)))
        .collect();
    (Observed { values, text }, guard)
}

/// Expected-rejection fixtures retain their image until all output predicates
/// pass too, including an unexpected successful exit or missing precondition.
pub(super) fn capture(
    body: fn(&mut NativeCodeGenerator, u64),
    arg: u64,
    worker: bool,
) -> ImageGuard {
    let (output, path) =
        execute_with_image_retention(body, arg, worker, false, None, None, ImageRetention::Always);
    ImageGuard {
        path: path.expect("Always retains the emitted executable"),
        output,
        arg,
        worker,
    }
}
