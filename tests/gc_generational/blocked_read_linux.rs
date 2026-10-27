//! Observe real Linux read blocking and worker MADV_DONTNEED syscalls.
use super::*;
use std::io::Write;
use std::os::unix::{fs::FileExt, fs::OpenOptionsExt, process::CommandExt};
const PHASE: usize = std::mem::offset_of!(klassic_native::gc_abi::MutatorContext, adopted_phase);
struct Traced(std::process::Child);
impl Drop for Traced {
    fn drop(&mut self) {
        self.0.stdin.take();
        if self.0.try_wait().unwrap().is_none() {
            // This tracer and its tracees own a fresh process group.
            let _ = Command::new("kill")
                .args(["-KILL", "--", &format!("-{}", self.0.id())])
                .status();
            let _ = self.0.wait();
        }
    }
}
#[derive(Debug)]
struct Progress {
    before: usize,
    after: usize,
    samples: usize,
    discarded: usize,
}
fn traces(f: &Fixture) -> Vec<PathBuf> {
    fs::read_dir(&f.directory)
        .unwrap()
        .map(|e| e.unwrap().path())
        .filter(|p| {
            p.file_name()
                .unwrap()
                .to_string_lossy()
                .starts_with("trace.")
        })
        .collect()
}
fn worker_decommits(f: &Fixture, owner: &Path) -> usize {
    traces(f)
        .into_iter()
        .filter(|p| p != owner)
        .map(|p| {
            fs::read_to_string(p)
                .unwrap()
                .matches("MADV_DONTNEED) = 0")
                .count()
        })
        .sum()
}
fn runtime(owner: u32, address: u64) -> std::io::Result<[u8; PHASE + 8]> {
    let mut bytes = [0; PHASE + 8];
    fs::File::open(format!("/proc/{owner}/mem"))?.read_exact_at(&mut bytes, address)?;
    Ok(bytes)
}
fn in_read(owner: u32, stdin: bool) -> bool {
    fs::read_to_string(format!("/proc/{owner}/syscall")).is_ok_and(|s| {
        let mut words = s.split_whitespace();
        words.next() == Some("0") && words.next().is_some_and(|fd| !stdin || fd == "0x0")
    })
}
fn probe(f: &Fixture, disable_worker: bool) -> (Output, Progress) {
    probe_with_observation_gaps(f, disable_worker, 0)
}
fn probe_with_observation_gaps(
    f: &Fixture,
    disable_worker: bool,
    discard_reads: usize,
) -> (Output, Progress) {
    let out = f.directory.join("out");
    let err = f.directory.join("err");
    if !f.stdin {
        fs::remove_file(f.directory.join("input")).unwrap();
        assert!(
            Command::new("mkfifo")
                .arg(f.directory.join("input"))
                .status()
                .unwrap()
                .success()
        );
    }
    let mut command = Command::new("strace");
    command
        .args(["-ff", "--seccomp-bpf", "-o"])
        .arg(f.directory.join("trace"))
        .args(["-e", "trace=clone,mmap,madvise,read"]);
    if disable_worker {
        command.args(["-e", "inject=clone:error=EAGAIN"]);
    }
    command
        .arg(&f.image)
        .stdin(Stdio::piped())
        .stdout(fs::File::create(&out).unwrap())
        .stderr(fs::File::create(&err).unwrap())
        .process_group(0);
    let mut traced = Traced(
        command
            .spawn()
            .expect("Linux blocked-read conformance requires strace"),
    );
    let mut fifo = None;
    let start = Instant::now();
    let (pid, path, address, before) = loop {
        assert!(
            traced.0.try_wait().unwrap().is_none(),
            "tracer exited before read: {}",
            fs::read_to_string(&err).unwrap()
        );
        assert!(
            start.elapsed() < Duration::from_secs(20),
            "owner did not reach blocked read"
        );
        if !f.stdin && fifo.is_none() {
            // Connect after the reader starts opening, without feeding bytes.
            match fs::OpenOptions::new()
                .write(true)
                .custom_flags(0x800)
                .open(f.directory.join("input"))
            {
                Ok(file) => fifo = Some(file),
                Err(e) => assert_eq!(e.raw_os_error(), Some(6), "{e}"), // ENXIO
            }
        }
        let found = traces(f).into_iter().find_map(|p| {
            let text = fs::read_to_string(&p).ok()?;
            let line = text.lines().find(|l| l.starts_with("mmap(NULL,"))?;
            let hex = line.split_once(" = 0x")?.1;
            let address = u64::from_str_radix(hex, 16).ok()?;
            let pid = p.extension()?.to_str()?.parse().ok()?;
            // The first mapping is shared Runtime. Its first word links to
            // OwnerState; MutatorContext is the first field of that owner.
            let shared = runtime(pid, address).ok()?;
            let address = u64::from_le_bytes(shared[..8].try_into().ok()?);
            if address == 0 || address & 7 != 0 {
                return None;
            }
            // Retain this actual context address for all subsequent samples.
            let bytes = runtime(pid, address).ok()?;
            (bytes[..8] == [4, 0, 0, 0, 2, 0, 0, 0] && in_read(pid, f.stdin))
                .then_some((pid, p, address, bytes))
        });
        if let Some(found) = found {
            break found;
        }
        std::thread::sleep(Duration::from_millis(20));
    };
    if disable_worker {
        let trace = fs::read_to_string(&path).unwrap();
        assert!(
            trace
                .lines()
                .any(|line| line.starts_with("clone(") && line.contains("EAGAIN")),
            "{trace}"
        );
        assert_eq!(
            traces(f).len(),
            1,
            "a worker started despite injected clone failure"
        );
    }
    let baseline = worker_decommits(f, &path);
    let started = Instant::now();
    // Reclaim waits for the configured age and bounded scans of all 128
    // pages. Allow scheduler contention from the parallel backend tests;
    // this checks progress, not a five-second runtime latency guarantee.
    let wait = Duration::from_secs(if disable_worker { 5 } else { 20 });
    let mut samples = 0;
    let mut discarded = 0;
    let after = loop {
        let bytes = runtime(pid, address).unwrap();
        assert_eq!(
            &bytes[..8],
            &before[..8],
            "owner left BlockedNative while kernel read remained pending"
        );
        assert_eq!(
            &bytes[PHASE..PHASE + 8],
            &before[PHASE..PHASE + 8],
            "owner adopted another phase during kernel read"
        );
        // /proc memory and syscall samples are not atomic. An unavailable or
        // transitioning syscall observation is not proof that read completed.
        // Keep checking Native/phase above and accept progress only with a
        // confirmed real read, within the original observation deadline.
        let mut confirmed = in_read(pid, f.stdin);
        if confirmed && discarded < discard_reads {
            discarded += 1; // Regression discards evidence; never invents it.
            confirmed = false;
        }
        if !confirmed {
            assert!(
                started.elapsed() < wait,
                "no confirmed kernel read: {:?}",
                fs::read_to_string(format!("/proc/{pid}/syscall"))
            );
            std::thread::sleep(Duration::from_millis(20));
            continue;
        }
        samples += 1;
        let after = worker_decommits(f, &path);
        if (after > baseline && samples > 1) || started.elapsed() > wait {
            break after;
        }
        std::thread::sleep(Duration::from_millis(20));
    };
    if f.stdin {
        let mut input = traced.0.stdin.take().unwrap();
        input.write_all(b"alpha\nbeta\n").unwrap();
    } else {
        fifo.take().unwrap().write_all(b"alpha\nbeta\n").unwrap();
    }
    let start = Instant::now();
    let status = loop {
        if let Some(status) = traced.0.try_wait().unwrap() {
            break status;
        }
        assert!(
            start.elapsed() < Duration::from_secs(15),
            "owner failed to resume from kernel read"
        );
        std::thread::sleep(Duration::from_millis(20));
    };
    (
        Output {
            status,
            stdout: fs::read(out).unwrap(),
            stderr: fs::read(err).unwrap(),
        },
        Progress {
            before: baseline,
            after,
            samples,
            discarded,
        },
    )
}

#[test]
fn linux_kernel_read_progress_is_confirmed_after_discarded_syscall_observations() {
    let f = Fixture::new("x86_64-unknown-linux-gnu", true);
    let (r, p) = probe_with_observation_gaps(&f, false, 3);
    assert_eq!(p.discarded, 3, "{p:?}");
    assert!(p.samples > 1 && p.after > p.before, "{p:?}");
    f.check(&r);
}
#[test]
fn linux_kernel_file_and_stdin_read_allow_worker_decommit_with_owner_phase_unchanged() {
    for stdin in [false, true] {
        let f = Fixture::new("x86_64-unknown-linux-gnu", stdin);
        let (r, p) = probe(&f, false);
        eprintln!("linux-kernel-read: stdin={stdin} {p:?}");
        assert!(
            p.after > p.before,
            "no worker decommit during kernel read: {p:?}"
        );
        f.check(&r);
        assert!(p.samples > 1, "{p:?}");
    }
}
#[test]
fn linux_kernel_read_probe_observes_no_worker_decommit_after_clone_failure() {
    let f = Fixture::new("x86_64-unknown-linux-gnu", true);
    let (r, p) = probe(&f, true);
    assert!(r.status.success(), "{r:?}");
    assert_eq!(r.stdout, f.expected(), "{r:?}");
    assert_eq!(
        p.after, p.before,
        "disabled worker unexpectedly decommitted: {p:?}"
    );
    assert_eq!(
        gc_stats(&String::from_utf8_lossy(&r.stderr))["heap_uncommit_pages"],
        traces(&f)
            .into_iter()
            .map(|path| {
                fs::read_to_string(path)
                    .unwrap()
                    .matches("MADV_DONTNEED) = 0")
                    .count() as u64
            })
            .sum::<u64>(),
        "{r:?}"
    );
    eprintln!("linux-kernel-read-disabled: {p:?}");
}
