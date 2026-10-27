//! A machine-code harness for the production `--gc=generational` runtime.
//!
//! No Klassic program can overwrite a heap reference, so the write barriers
//! (`field_store`'s SATB pre-barrier and remembered-set registration) are
//! unreachable from source. This harness compiles the real runtime
//! (`production::emit`, worker build) into a tiny ELF whose "program" is a
//! hand-written instruction sequence instead of compiled source: it
//! allocates through the real `gc_alloc`, roots through the real RootCells,
//! stores through the real `field_store`, drives the real `mark_start` /
//! `mark_loop` and collections, and prints observations (`name=value`) that
//! the tests assert. By default no worker thread is started (`spawn` fails),
//! so every interleaving is deterministic; the worker code is still emitted.
use crate::gc_codegen::production::test_support::{
    Counter, HarnessLabels, emit_counter, emit_current_entry, emit_is_old, emit_marked,
    emit_unbounded_budget, harness_labels,
};
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};
use crate::{
    DataLabel, GcTestProgram, NativeCodeGenerator, NativeTarget, NativeTargetContext, SourceFile,
    TextLabel, write_executable_for_target,
};
use std::collections::HashMap;
use std::io::Read;
use std::os::unix::fs::PermissionsExt;
use std::process::{Command, Output, Stdio};
use std::time::{Duration, Instant};

/// Where an operand comes from.
#[derive(Clone, Copy)]
pub(super) enum Src {
    /// A raw payload address kept in a frame slot (valid until a collection).
    Raw(u32),
    /// A RootCell pointer kept in a frame slot (survives collections).
    Root(u32),
    Null,
    Imm(u64),
}

pub(super) struct H<'a> {
    pub g: &'a mut NativeCodeGenerator,
    l: HarnessLabels<TextLabel>,
    runtime: DataLabel,
    print_i64: TextLabel,
}

/// Frame slots reserved for the harness's own loops.
const SCRATCH_SLOT: u32 = 40;
const LOOP_SLOT: u32 = 41;

fn slot_offset(slot: u32) -> i32 {
    8 * (slot as i32 + 1)
}

impl<'a> H<'a> {
    pub(super) fn begin(g: &'a mut NativeCodeGenerator) -> Self {
        let gen_runtime = g.gc_gen.as_ref().expect("generational runtime");
        let l = harness_labels(&gen_runtime.labels);
        let runtime = gen_runtime.data.runtime;
        let print_i64 = g.print_i64;
        let h = Self {
            g,
            l,
            runtime,
            print_i64,
        };
        crate::gc_codegen::production::test_support::begin_primary_owner_fixture(h.g, runtime);
        h
    }

    fn reload(&mut self) {
        self.g.mov_data_addr(R::V3, self.runtime);
        self.g.load_ptr_disp32(R::V3, R::V3, 0);
        crate::gc_codegen::production::test_support::bind_primary_owner(self.g);
    }

    /// V0 = the operand's payload address (or immediate).
    pub(super) fn value(&mut self, src: Src) {
        match src {
            Src::Raw(slot) => self.g.load_rbp_slot(R::V0, slot_offset(slot)),
            Src::Root(slot) => {
                self.g.load_rbp_slot(R::V0, slot_offset(slot));
                self.g.call_label(self.l.root_load);
            }
            Src::Null => self.g.mov_imm64(R::V0, 0),
            Src::Imm(value) => self.g.mov_imm64(R::V0, value),
        }
    }

    /// Allocate `bytes` of shape `tag` (1 raw, 2 record) into a raw slot.
    pub(super) fn alloc(&mut self, tag: u64, bytes: u64, slot: u32) {
        self.g.mov_imm64(R::V7, bytes);
        self.g.mov_imm64(R::V6, tag);
        self.g.call_label(self.l.alloc);
        self.g.store_rbp_slot(slot_offset(slot), R::V0);
    }

    /// Root the raw address in `slot`; the slot then holds the cell pointer.
    pub(super) fn root(&mut self, slot: u32) {
        self.g.mov_reg_reg(R::V0, R::V5);
        self.g.add_reg_imm32(R::V0, -slot_offset(slot));
        self.g.call_label(self.l.root_adopt);
    }

    /// `slot = V0`.
    pub(super) fn keep(&mut self, slot: u32) {
        self.g.store_rbp_slot(slot_offset(slot), R::V0);
    }

    /// Raw store of a scalar into a field of a freshly allocated object.
    pub(super) fn set_scalar(&mut self, obj: Src, index: u32, scalar: u64) {
        self.value(obj);
        self.g.mov_imm64(R::V1, scalar);
        self.g.store_ptr_disp32(R::V0, (index * 8) as i32, R::V1);
    }

    /// `obj.field[index] = val`: through the real `field_store` barrier, or
    /// (`barrier == false`) encoding followed by a plain store that bypasses
    /// bookkeeping, so negative controls still use the field representation.
    pub(super) fn store(&mut self, obj: Src, index: u32, val: Src, barrier: bool) {
        self.value(val);
        self.g.mov_reg_reg(R::V6, R::V0);
        self.value(obj);
        self.g.add_reg_imm32(R::V0, (index * 8) as i32);
        self.g.mov_reg_reg(R::V7, R::V0);
        if barrier {
            self.g.call_label(self.l.field_store);
        } else {
            crate::gc_codegen::production::test_support::emit_encode_reference(
                self.g,
                R::V6,
                R::V1,
            );
            self.g.store_ptr_disp32(R::V7, 0, R::V6);
        }
    }

    /// V0 = `obj.field[index]`, read raw (no phase may be open).
    pub(super) fn load(&mut self, obj: Src, index: u32) {
        self.value(obj);
        self.g.load_ptr_disp32(R::V0, R::V0, (index * 8) as i32);
    }

    /// V0 = `obj.field[index]` through the load barrier.
    pub(super) fn load_barriered(&mut self, obj: Src, index: u32) {
        self.value(obj);
        self.g.add_reg_imm32(R::V0, (index * 8) as i32);
        self.g.mov_reg_reg(R::V10, R::V0);
        self.g.load_ptr_disp32(R::V0, R::V10, 0);
        self.g.call_label(self.l.field_load);
    }

    /// The compiler's generic read: raw boxes bypass reference decoding.
    pub(super) fn load_untyped(&mut self, obj: Src, index: u32) {
        self.value(obj);
        self.g.mov_reg_reg(R::V1, R::V0);
        self.g.add_reg_imm32(R::V0, (index * 8) as i32);
        crate::gc_codegen::production::emit_untyped_load(self.g, self.l.field_load);
    }

    /// V0 = a counter or flag.
    pub(super) fn counter(&mut self, counter: Counter) {
        emit_counter(self.g, counter);
    }

    pub(super) fn mark_start(&mut self) {
        self.g.call_label(self.l.mark_start);
    }

    /// The mutator finishes the mark (what the relocation pause does).
    pub(super) fn drain_marks(&mut self) {
        self.g.call_label(self.l.lock);
        emit_unbounded_budget(self.g);
        self.g.call_label(self.l.mark_loop);
        self.g.call_label(self.l.unlock);
    }

    /// Seal the Old snapshot explicitly, independently of a Young collection.
    pub(super) fn end_marks(&mut self) {
        self.g.call_label(self.l.lock);
        self.g.call_label(self.l.mark_end);
        self.g.call_label(self.l.unlock);
    }

    pub(super) fn mark_quantum(&mut self, units: u64) {
        self.g.call_label(self.l.lock);
        crate::gc_codegen::production::test_support::emit_budget(self.g, units);
        self.g.call_label(self.l.mark_loop);
        self.g.call_label(self.l.unlock);
    }

    pub(super) fn relocate_quantum(&mut self, units: u64, worker: bool) {
        self.g.call_label(self.l.lock);
        crate::gc_codegen::production::test_support::emit_budget(self.g, units);
        crate::gc_codegen::production::test_support::set_collecting(self.g, true);
        self.g.call_label(if worker {
            self.l.worker_cheney
        } else {
            self.l.cheney
        });
        crate::gc_codegen::production::test_support::set_collecting(self.g, false);
        self.g.call_label(self.l.unlock);
    }

    /// Pop the newest RootCell (root cells are LIFO).
    pub(super) fn pop_root(&mut self) {
        self.g.call_label(self.l.root_pop);
    }

    pub(super) fn marked(&mut self, obj: Src) {
        self.value(obj);
        emit_marked(self.g);
    }

    pub(super) fn object_metadata(&mut self, obj: Src, field: usize) {
        use crate::gc_abi::ObjectHeader;
        self.value(obj);
        self.g.load_ptr_disp32(
            R::V0,
            R::V0,
            std::mem::offset_of!(ObjectHeader, mark_record) as i32
                - std::mem::size_of::<ObjectHeader>() as i32,
        );
        let done = self.g.create_text_label();
        self.g.test_reg_reg(R::V0, R::V0);
        self.g.jcc_label(C::Equal, done);
        self.g.load_ptr_disp32(R::V0, R::V0, field as i32);
        self.g.bind_text_label(done);
    }

    pub(super) fn object_id(&mut self, obj: Src) {
        self.object_metadata(
            obj,
            std::mem::offset_of!(crate::gc_abi::MarkRecord, object_id),
        );
    }

    pub(super) fn collect(&mut self) {
        self.g.call_label(self.l.collect);
    }

    pub(super) fn finish_collection(&mut self) {
        self.g.call_label(self.l.lock);
        self.g.call_label(self.l.finish);
        self.g.call_label(self.l.unlock);
    }

    pub(super) fn worker_copy(&mut self, obj: Src) {
        self.value(obj);
        self.g.load_ptr_disp32(
            R::V1,
            R::V3,
            crate::gc_codegen::production::test_support::heap_base_offset(),
        );
        self.g.sub_reg_reg(R::V0, R::V1);
        self.g.shl_reg_imm8(R::V0, 16);
        self.g.mov_imm64(
            R::V1,
            u64::from(crate::gc_abi::PhaseSnapshot::initial().color()),
        );
        self.g.or_reg_reg(R::V0, R::V1);
        self.g.call_label(self.l.worker_forward);
        self.g.shr_reg_imm8(R::V0, 16);
        self.g.load_ptr_disp32(
            R::V1,
            R::V3,
            crate::gc_codegen::production::test_support::heap_base_offset(),
        );
        self.g.add_reg_reg(R::V0, R::V1);
    }

    pub(super) fn is_old(&mut self, obj: Src) {
        self.value(obj);
        emit_is_old(self.g);
    }

    pub(super) fn entry(&mut self, index: u32) {
        emit_current_entry(self.g, index);
    }

    /// V0 = 1 if `obj.field[index]` (through the load barrier) is the
    /// payload address of `other`.
    pub(super) fn field_is(&mut self, obj: Src, index: u32, other: Src) {
        let no = self.g.create_text_label();
        let end = self.g.create_text_label();
        self.load_barriered(obj, index);
        self.g.mov_reg_reg(R::V8, R::V0);
        self.value(other);
        self.g.cmp_reg_reg(R::V8, R::V0);
        self.g.jcc_label(C::NotEqual, no);
        self.g.mov_imm64(R::V0, 1);
        self.g.jmp_label(end);
        self.g.bind_text_label(no);
        self.g.mov_imm64(R::V0, 0);
        self.g.bind_text_label(end);
    }

    /// Allocate 60 000-byte raw objects until `n` collections have run.
    pub(super) fn gc_until(&mut self, n: i32) {
        let top = self.g.create_text_label();
        self.g.bind_text_label(top);
        self.g.mov_imm64(R::V7, 60000);
        self.g.mov_imm64(R::V6, 1);
        self.g.call_label(self.l.alloc);
        self.counter(Counter::Collections);
        self.g.cmp_reg_imm32(R::V0, n);
        self.g.jcc_label(C::Below, top);
    }

    /// Allocate until `n` *more* collections have run (target kept in a slot).
    pub(super) fn gc_more(&mut self, n: i32) {
        let top = self.g.create_text_label();
        self.counter(Counter::Collections);
        self.g.add_reg_imm32(R::V0, n);
        self.keep(SCRATCH_SLOT);
        self.g.bind_text_label(top);
        self.g.mov_imm64(R::V7, 60000);
        self.g.mov_imm64(R::V6, 1);
        self.g.call_label(self.l.alloc);
        self.g.load_rbp_slot(R::V1, slot_offset(SCRATCH_SLOT));
        self.counter(Counter::Collections);
        self.g.cmp_reg_reg(R::V0, R::V1);
        self.g.jcc_label(C::Below, top);
    }

    /// Small allocations until relocation closes, then finish its cleanup.
    pub(super) fn settle(&mut self) {
        let top = self.g.create_text_label();
        self.g.bind_text_label(top);
        self.g.mov_imm64(R::V7, 16);
        self.g.mov_imm64(R::V6, 1);
        self.g.call_label(self.l.alloc);
        self.counter(Counter::PhaseOpen);
        self.g.test_reg_reg(R::V0, R::V0);
        self.g.jcc_label(C::NotEqual, top);
        self.finish_collection();
    }

    /// Repeat `body` `times` times (the count lives in a frame slot).
    pub(super) fn repeat(&mut self, times: u64, mut body: impl FnMut(&mut Self)) {
        let top = self.g.create_text_label();
        self.g.mov_imm64(R::V0, times);
        self.keep(LOOP_SLOT);
        self.g.bind_text_label(top);
        body(self);
        self.g.load_rbp_slot(R::V0, slot_offset(LOOP_SLOT));
        self.g.sub_reg_imm8(R::V0, 1);
        self.keep(LOOP_SLOT);
        self.g.test_reg_reg(R::V0, R::V0);
        self.g.jcc_label(C::NotEqual, top);
    }

    /// Store V0 through the RootCell whose pointer is in `slot`.
    pub(super) fn set_root(&mut self, slot: u32) {
        self.g.mov_reg_reg(R::V1, R::V5);
        self.g.add_reg_imm32(R::V1, -slot_offset(slot));
        self.g.call_label(self.l.root_store);
    }

    /// V0 = 1 if current remembered-set entry `index` is `&obj.field[field]`.
    pub(super) fn entry_is_slot(&mut self, index: u32, obj: Src, field: u32) {
        let no = self.g.create_text_label();
        let end = self.g.create_text_label();
        self.entry(index);
        self.g.mov_reg_reg(R::V8, R::V0);
        self.value(obj);
        self.g.add_reg_imm32(R::V0, (field * 8) as i32);
        self.g.cmp_reg_reg(R::V8, R::V0);
        self.g.jcc_label(C::NotEqual, no);
        self.g.mov_imm64(R::V0, 1);
        self.g.jmp_label(end);
        self.g.bind_text_label(no);
        self.g.mov_imm64(R::V0, 0);
        self.g.bind_text_label(end);
    }

    /// V0 = 1 if the object `obj.field[index]` refers to is Old.
    pub(super) fn field_is_old(&mut self, obj: Src, index: u32) {
        self.load_barriered(obj, index);
        emit_is_old(self.g);
    }

    /// V0 = word `word` of the object `obj.field[index]` refers to.
    pub(super) fn deref_field(&mut self, obj: Src, index: u32, word: u32) {
        self.load_barriered(obj, index);
        self.g.load_ptr_disp32(R::V0, R::V0, (word * 8) as i32);
    }

    /// `name=value` on stdout, `value` computed into V0 by `compute`.
    pub(super) fn report(&mut self, name: &str, compute: impl FnOnce(&mut Self)) {
        let text = format!("{name}=");
        let label = self.g.asm.data_label_with_bytes(text.as_bytes());
        self.g.emit_write_data(1, label, text.len());
        self.reload();
        compute(self);
        self.g.mov_reg_reg(R::V7, R::V0);
        self.g.mov_imm64(R::V6, 1);
        self.g.mov_imm64(R::V2, 0);
        self.g.call_label(self.print_i64);
        let newline = self.g.newline;
        self.g.emit_write_data(1, newline, 1);
        self.reload();
    }

    pub(super) fn report_counter(&mut self, name: &str, counter: Counter) {
        self.report(name, |h| h.counter(counter));
    }
}

/// What a harness run printed.
pub(super) struct Observed {
    values: HashMap<String, i64>,
    text: String,
}

impl Observed {
    pub(super) fn get(&self, name: &str) -> i64 {
        *self
            .values
            .get(name)
            .unwrap_or_else(|| panic!("no `{name}` in the harness output:\n{}", self.text))
    }

    /// The raw output, for assertion messages.
    pub(super) fn text(&self) -> &str {
        &self.text
    }
}

/// Compile the production runtime around `body`, run it, and parse its
/// `name=value` lines. `worker_thread` starts the real GC worker.
pub(super) fn run(
    body: fn(&mut NativeCodeGenerator, u64),
    arg: u64,
    worker_thread: bool,
) -> Observed {
    run_with_stress(body, arg, worker_thread, false)
}

pub(super) fn run_with_stress(
    body: fn(&mut NativeCodeGenerator, u64),
    arg: u64,
    worker_thread: bool,
    stress: bool,
) -> Observed {
    let (output, _) = execute_with_image_retention(
        body,
        arg,
        worker_thread,
        stress,
        None,
        None,
        ImageRetention::Failure,
    );
    let text = String::from_utf8_lossy(&output.stdout).into_owned();
    if std::env::var_os("KLASSIC_HARNESS_DUMP").is_some() {
        eprintln!("--- harness output ---\n{text}");
    }
    assert!(
        output.status.success(),
        "harness failed ({:?}, arg={arg}, worker={worker_thread}, stress={stress})\nstdout:\n{text}\nstderr:\n{}",
        output.status,
        String::from_utf8_lossy(&output.stderr)
    );
    let values = text
        .lines()
        .filter_map(|line| line.split_once('='))
        .filter_map(|(name, value)| Some((name.to_string(), value.trim().parse().ok()?)))
        .collect();
    Observed { values, text }
}

mod remset;
mod satb;

/// Execute the same production harness when a resource failure is expected.
pub(super) fn run_failure(body: fn(&mut NativeCodeGenerator, u64), arg: u64) -> Output {
    execute(body, arg, false, false)
}

fn execute(
    body: fn(&mut NativeCodeGenerator, u64),
    arg: u64,
    worker_thread: bool,
    stress: bool,
) -> Output {
    execute_with_soft_limit(body, arg, worker_thread, stress, None)
}

fn execute_with_soft_limit(
    body: fn(&mut NativeCodeGenerator, u64),
    arg: u64,
    worker_thread: bool,
    stress: bool,
    soft_limit: Option<u64>,
) -> Output {
    execute_with_heap_limits(body, arg, worker_thread, stress, soft_limit, None)
}

fn execute_with_heap_limits(
    body: fn(&mut NativeCodeGenerator, u64),
    arg: u64,
    worker_thread: bool,
    stress: bool,
    soft_limit: Option<u64>,
    hard_limit: Option<u64>,
) -> Output {
    execute_with_image_retention(
        body,
        arg,
        worker_thread,
        stress,
        soft_limit,
        hard_limit,
        ImageRetention::None,
    )
    .0
}

enum ImageRetention {
    None,
    Failure,
    Always,
}

fn execute_with_image_retention(
    body: fn(&mut NativeCodeGenerator, u64),
    arg: u64,
    worker_thread: bool,
    stress: bool,
    soft_limit: Option<u64>,
    hard_limit: Option<u64>,
    retention: ImageRetention,
) -> (Output, Option<std::path::PathBuf>) {
    let path = create_harness_image(body, arg, worker_thread, stress, soft_limit, hard_limit);
    run_harness_image(path, arg, retention, Duration::from_secs(180))
}

fn create_harness_image(
    body: fn(&mut NativeCodeGenerator, u64),
    arg: u64,
    worker_thread: bool,
    stress: bool,
    soft_limit: Option<u64>,
    hard_limit: Option<u64>,
) -> std::path::PathBuf {
    let target = NativeTargetContext::for_target(NativeTarget::LinuxX86_64);
    let source = SourceFile::new("<gc-harness>", "1");
    let expr =
        klassic_syntax::parse_inline_expression("<gc-harness>", "1").expect("trivial source");
    let mut generator = NativeCodeGenerator::new(source, None, target.platform);
    generator.gc_generational = true;
    generator.gc_stress = stress;
    generator.gc_soft_max_heap = soft_limit;
    if let Some(hard) = hard_limit {
        generator.gc_max_heap = hard;
    }
    generator.gc_test = Some(GcTestProgram {
        body,
        arg,
        no_worker_thread: !worker_thread,
    });
    let object = generator.compile(&expr).expect("harness program compiles");
    let bytes = write_executable_for_target(target, object);
    let nanos = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map_or(0, |d| d.as_nanos());
    // Time alone is not a uniqueness guarantee for concurrent test threads.
    // Reserve the path exclusively before writing any executable bytes.
    static NEXT_IMAGE: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
    let (path, mut file) = loop {
        let id = NEXT_IMAGE.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        let path = std::env::temp_dir().join(format!(
            "klassic-harness-{}-{nanos}-{id}",
            std::process::id()
        ));
        match std::fs::OpenOptions::new()
            .write(true)
            .create_new(true)
            .open(&path)
        {
            Ok(file) => break (path, file),
            Err(e) if e.kind() == std::io::ErrorKind::AlreadyExists => continue,
            Err(e) => panic!("create harness executable: {e}"),
        }
    };
    std::io::Write::write_all(&mut file, &bytes).expect("write harness executable");
    file.set_permissions(std::fs::Permissions::from_mode(0o755))
        .expect("chmod");
    drop(file);
    path
}

/// Drain both pipes while waiting: a full pipe must not impersonate a GC hang.
fn supervise_harness_image(
    path: &std::path::Path,
    timeout: Duration,
) -> std::io::Result<(Output, bool)> {
    let mut child = Command::new(path)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()?;
    let drain = |mut pipe: Box<dyn Read + Send>| {
        std::thread::spawn(move || {
            let mut bytes = Vec::new();
            pipe.read_to_end(&mut bytes).map(|_| bytes)
        })
    };
    let stdout = drain(Box::new(child.stdout.take().unwrap()));
    let stderr = drain(Box::new(child.stderr.take().unwrap()));
    let started = Instant::now();
    let (status, timed_out) = loop {
        if let Some(status) = child.try_wait()? {
            break (status, false);
        }
        if started.elapsed() >= timeout {
            // All emitted workers belong to this process. Reap before joining
            // the pipe readers so neither workers nor blocked readers survive.
            let _ = child.kill();
            break (child.wait()?, true);
        }
        std::thread::sleep(Duration::from_millis(10));
    };
    Ok((
        Output {
            status,
            stdout: stdout.join().expect("harness stdout reader panicked")?,
            stderr: stderr.join().expect("harness stderr reader panicked")?,
        },
        timed_out,
    ))
}

fn run_harness_image(
    path: std::path::PathBuf,
    arg: u64,
    retention: ImageRetention,
    timeout: Duration,
) -> (Output, Option<std::path::PathBuf>) {
    let (output, timed_out) = (0..50)
        .find_map(|attempt| match supervise_harness_image(&path, timeout) {
            Ok(output) => Some(output),
            Err(error) if error.raw_os_error() == Some(26) && attempt < 49 => {
                // ETXTBSY: another test thread forked while the file was open.
                std::thread::sleep(Duration::from_millis(20));
                None
            }
            Err(error) => panic!(
                "run harness (image retained at {}): {error}",
                path.display()
            ),
        })
        .expect("harness ran");
    if let Some(export) = std::env::var_os("KLASSIC_HARNESS_IMAGE") {
        std::fs::copy(&path, export).expect("export harness executable");
    }
    if let Some(directory) = std::env::var_os("KLASSIC_HARNESS_IMAGE_DIR") {
        let directory = std::path::PathBuf::from(directory);
        std::fs::create_dir_all(&directory).expect("create harness export directory");
        std::fs::copy(
            &path,
            directory.join(format!(
                "{arg}-{}.elf",
                path.file_name().unwrap().to_string_lossy()
            )),
        )
        .expect("export harness executable by argument");
    }
    // A timeout is never an expected runtime rejection. Keep the image even
    // when the caller normally deletes expected-failure fixtures, and fail here
    // before it can inspect only the unsuccessful exit status.
    assert!(
        !timed_out,
        "harness timed out after {timeout:?}; executable retained: {}\nstdout:\n{}\nstderr:\n{}",
        path.display(),
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    let retained = if matches!(retention, ImageRetention::Always)
        || (matches!(retention, ImageRetention::Failure) && !output.status.success())
    {
        if !output.status.success() {
            eprintln!("failed harness executable retained: {}", path.display());
        }
        Some(path)
    } else {
        let _ = std::fs::remove_file(&path);
        None
    };
    (output, retained)
}

#[test]
fn harness_supervision_drains_large_stdout_and_stderr() {
    fn write_both(g: &mut NativeCodeGenerator, _: u64) {
        let text = g.asm.data_label_with_bytes(&vec![b'x'; 65536]);
        for _ in 0..64 {
            g.emit_write_data(1, text, 65536);
            g.emit_write_data(2, text, 65536);
        }
    }
    let path = create_harness_image(write_both, 0, false, false, None, None);
    let (output, retained) =
        run_harness_image(path, 0, ImageRetention::Failure, Duration::from_secs(30));
    assert!(output.status.success());
    assert!(retained.is_none());
    assert_eq!(output.stdout, vec![b'x'; 4 * 1024 * 1024]);
    assert_eq!(output.stderr, vec![b'x'; 4 * 1024 * 1024]);
}

#[test]
fn harness_timeout_is_not_an_expected_failure_and_retains_its_elf() {
    fn never_finishes(g: &mut NativeCodeGenerator, _: u64) {
        let text = g.asm.data_label_with_bytes(b"entered loop\n");
        g.emit_write_data(1, text, 13);
        let forever = g.create_text_label();
        g.bind_text_label(forever);
        g.jmp_label(forever);
    }
    let path = create_harness_image(never_finishes, 0, false, false, None, None);
    // `None` is the policy used by expected runtime-failure tests. A timeout
    // must panic before such a caller can mistake a killed process for success.
    let failure = std::panic::catch_unwind(|| {
        run_harness_image(
            path.clone(),
            0,
            ImageRetention::None,
            Duration::from_secs(1),
        )
    })
    .expect_err("a timeout must fail even an expected-failure fixture");
    let diagnostic = failure
        .downcast_ref::<String>()
        .expect("timeout diagnostic");
    assert!(diagnostic.contains("harness timed out after 1s"));
    assert!(diagnostic.contains("entered loop"));
    assert!(diagnostic.contains(&path.display().to_string()));
    assert!(std::fs::read(&path).unwrap().starts_with(b"\x7fELF"));
    std::fs::remove_file(path).unwrap();
}

#[test]
fn unexpected_runtime_failure_retains_the_actual_generated_executable() {
    fn fail(g: &mut NativeCodeGenerator, _: u64) {
        let h = H::begin(g);
        h.g.mov_imm64(R::V0, 0); // An actual null RootCell handle is invalid.
        h.g.call_label(h.l.root_load);
    }
    let (output, retained) =
        execute_with_image_retention(fail, 0, false, false, None, None, ImageRetention::Failure);
    assert!(!output.status.success());
    assert!(String::from_utf8_lossy(&output.stderr).contains("runtime invariant violated"));
    let path = retained.expect("unexpected runtime failure preserves its executable");
    let bytes = std::fs::read(&path).expect("preserved generated image remains readable");
    assert!(bytes.starts_with(b"\x7fELF"));
    let replay = Command::new(&path).output().expect("replay retained image");
    assert_eq!(replay.status.code(), output.status.code());
    assert_eq!(replay.stderr, output.stderr);
    std::fs::remove_file(path).expect("remove this intentional test artifact after replay");

    let (expected, retained) =
        execute_with_image_retention(fail, 0, false, false, None, None, ImageRetention::None);
    assert!(!expected.status.success());
    assert!(
        retained.is_none(),
        "expected negative cases keep normal cleanup"
    );
}

mod admission;
mod colours;
mod copy_claims;
mod detach;
mod exhaustion;
mod fields;
mod identity;
mod lease_transfer;
mod load_fast;
mod mark_roots;
mod mark_slots;
mod metadata_leases;
mod moving_round;
mod native_completed;
mod native_tail;
mod phase;
mod relocate_roots;
mod relocate_slots;
mod root_retirement;
mod thread_tls;

mod end_check;
mod literal_roots;
mod mark_end;
mod mark_epoch;
mod mark_prepare;
mod ready_cycle;
mod relocate_end;
mod remset_scan;
mod retire;

mod mark_frontier;

mod old_mark;

mod store_fast;

mod young_mark;

mod old_cycle;

mod old_policy;

mod tenuring;

mod remset_retire;

mod start_policy;

mod capacity;

mod generation;
mod slot_owner;

mod pool;

mod old_runs;

mod heap_geometry;

mod commit;
mod large_pages;
mod pre_touch;

mod worker_lifecycle;

mod metadata;

mod soft_heap;

mod metadata_retire;

mod mark_arena;

#[path = "gc_barrier_harness_tests/trace.rs"]
mod trace;

mod mutator_time;

mod large_object;
mod large_policy;
mod large_reserve;

mod full_capacity;
mod old_footprint;

mod promotion_reserve;

mod native_io;
pub(crate) mod nested_output;
mod participants;
mod registered_roots;

mod root_journal;

mod actors;
mod journal_regions;

mod owner_boundaries;

mod native_mark_sharing;

mod journal_obligations;

mod old_phase_rounds;

mod allocation_old_scheduler;
mod captured_journal;
mod dual_mark;
mod relocate_pages;
mod worker_journal;
mod worker_view;
mod young_scheduler;

mod page_runs;
mod weak;
mod weak_race;

mod native_roots;

mod pin_destination;

mod pin_copy;

mod pin_adopt;

mod scoped_pin;

mod pin_abi;

mod pin_native_windows;

mod queue_growth;
mod queue_spill_frontier;
mod thread_api;
mod thread_api_c;

mod lease_growth;
mod weak_growth;

mod remset_buffer;
mod remset_sealing;
