//! Observe an actual blocked write, its real owner context and worker reclaim.
use super::*;
use std::io::{Read, Write};
use std::os::fd::OwnedFd;
use std::os::unix::{fs::FileExt, net::UnixStream, process::CommandExt};
use std::process::Stdio;
use std::time::{Duration, Instant};

struct Traced(std::process::Child);
impl Drop for Traced {
    fn drop(&mut self) {
        if self.0.try_wait().unwrap().is_none() {
            let _ = Command::new("kill")
                .args(["-KILL", "--", &format!("-{}", self.0.id())])
                .status();
            let _ = self.0.wait();
        }
    }
}
fn context(pid: u32, address: u64) -> std::io::Result<[u8; 24]> {
    let mut b = [0; 24];
    fs::File::open(format!("/proc/{pid}/mem"))?.read_exact_at(&mut b, address)?;
    Ok(b)
}
fn in_write(pid: u32) -> bool {
    fs::read_to_string(format!("/proc/{pid}/syscall")).is_ok_and(|s| {
        let mut words = s.split_whitespace();
        words.next() == Some("1") && words.next() == Some("0x1")
    })
}
fn traces(directory: &Path) -> Vec<PathBuf> {
    fs::read_dir(directory)
        .unwrap()
        .map(|p| p.unwrap().path())
        .filter(|p| {
            p.file_name()
                .unwrap()
                .to_string_lossy()
                .starts_with("trace.")
        })
        .collect()
}
fn reclaimed(directory: &Path, owner: &Path) -> usize {
    traces(directory)
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

#[test]
fn native_console_linux_kernel_write_stays_native_while_worker_reclaims() {
    let directory = temp_path("blocked_console", "dir");
    fs::create_dir(&directory).unwrap();
    let source = directory.join("source.kl");
    let image = directory.join("image");
    fs::write(
        &source,
        r#"
enum Tree { case Leaf(v: Int); case Node(l: Tree, r: Tree) }
def build(d: Int): Tree = if(d == 0) Leaf(1) else Node(build(d-1),build(d-1))
def sum(t: Tree): Int = t match { case Leaf(v) => v; case Node(l,r) => sum(l)+sum(r) }
def grow(s: String,n: Int): String = if(n<=0) s else grow(s+s,n-1)
val keep = build(6)
mutable i = 0
mutable total = 0
while(i<80) { total += sum(build(7)); i += 1 }
mutable large = 0
mutable round = 0
while(round<3) { large += length(grow("x",i-59)); round += 1 }
mutable answer = 42
println(answer)
println(total)
println(sum(keep))
"#,
    )
    .unwrap();
    let built = build(
        &[
            "--target",
            "x86_64-unknown-linux-gnu",
            "--gc=generational",
            "--gc-log",
            "--gc-max-heap=16MiB",
            "--gc-min-heap=128KiB",
            "--gc-uncommit-delay=1500ms",
        ],
        &source,
        &image,
    );
    assert!(built.status.success(), "{built:?}");
    let (mut reader, mut writer) = UnixStream::pair().unwrap();
    writer.set_nonblocking(true).unwrap();
    let mut filled = 0;
    for bytes in [&[b'P'; 8192][..], b"P"] {
        loop {
            match writer.write(bytes) {
                Ok(n) => filled += n,
                Err(e) if e.kind() == std::io::ErrorKind::WouldBlock => break,
                other => panic!("fill stdout: {other:?}"),
            }
        }
    }
    writer.set_nonblocking(false).unwrap();
    reader
        .set_read_timeout(Some(Duration::from_secs(30)))
        .unwrap();
    let stderr = directory.join("stderr");
    let mut traced = Traced(
        Command::new("strace")
            .args([
                "-ff",
                "--seccomp-bpf",
                "-e",
                "trace=clone,mmap,madvise,write",
                "-o",
            ])
            .arg(directory.join("trace"))
            .arg(&image)
            .stdout(Stdio::from(OwnedFd::from(writer)))
            .stderr(fs::File::create(&stderr).unwrap())
            .process_group(0)
            .spawn()
            .unwrap(),
    );
    let started = Instant::now();
    let (pid, owner, address, before) = loop {
        assert!(
            traced.0.try_wait().unwrap().is_none(),
            "tracer exited: {}",
            fs::read_to_string(&stderr).unwrap()
        );
        assert!(
            started.elapsed() < Duration::from_secs(20),
            "no blocked write"
        );
        let found = traces(&directory).into_iter().find_map(|p| {
            let text = fs::read_to_string(&p).ok()?;
            let first = text.lines().find(|s| s.starts_with("mmap(NULL,"))?;
            let base = u64::from_str_radix(first.split_once(" = 0x")?.1, 16).ok()?;
            let pid = p.extension()?.to_str()?.parse().ok()?;
            let shared = context(pid, base).ok()?;
            let address = u64::from_le_bytes(shared[..8].try_into().ok()?);
            if address == 0 || address & 7 != 0 {
                return None;
            }
            let bytes = context(pid, address).ok()?;
            (bytes[..8] == [4, 0, 0, 0, 2, 0, 0, 0] && in_write(pid))
                .then_some((pid, p, address, bytes))
        });
        if let Some(found) = found {
            break found;
        }
        std::thread::sleep(Duration::from_millis(20));
    };
    let baseline = reclaimed(&directory, &owner);
    let started = Instant::now();
    let mut samples = 0;
    let after = loop {
        let bytes = context(pid, address).unwrap();
        assert_eq!(
            bytes[..8],
            before[..8],
            "owner left Native during pending write"
        );
        assert_eq!(
            bytes[16..24],
            before[16..24],
            "phase changed during pending write"
        );
        if in_write(pid) {
            samples += 1;
        }
        let after = reclaimed(&directory, &owner);
        if after > baseline && samples > 1 {
            break after;
        }
        assert!(
            started.elapsed() < Duration::from_secs(20),
            "no worker reclaim while blocked"
        );
        std::thread::sleep(Duration::from_millis(20));
    };
    let mut output = Vec::new();
    reader.read_to_end(&mut output).unwrap();
    let status = traced.0.wait().unwrap();
    assert!(status.success(), "{}", fs::read_to_string(&stderr).unwrap());
    assert!(output[..filled].iter().all(|b| *b == b'P'));
    assert_eq!(&output[filled..], b"42\n10240\n64\n");
    let stats = gc_stats(&fs::read_to_string(&stderr).unwrap());
    assert_eq!(stats["native_buffer_writes"], 6);
    assert_eq!(stats["native_io_entries"], 6);
    assert_eq!(stats["native_io_exits"], 6);
    eprintln!("blocked console: {samples} confirmed samples, worker decommits {baseline}->{after}");
    fs::remove_dir_all(directory).unwrap();
}
