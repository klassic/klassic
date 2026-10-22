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
    Counter, HarnessLabels, counter_offset, emit_current_entry, emit_is_old, emit_marked,
    emit_unbounded_budget, harness_labels,
};
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};
use crate::{
    DataLabel, GcTestProgram, NativeCodeGenerator, NativeTarget, NativeTargetContext, SourceFile,
    TextLabel, write_executable_for_target,
};
use std::collections::HashMap;
use std::os::unix::fs::PermissionsExt;
use std::process::Command;

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
        let mut h = Self {
            g,
            l,
            runtime,
            print_i64,
        };
        h.g.sub_reg_imm32(R::V4, 2048);
        h.reload();
        h
    }

    fn reload(&mut self) {
        self.g.mov_data_addr(R::V3, self.runtime);
        self.g.load_ptr_disp32(R::V3, R::V3, 0);
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
    /// (`barrier == false`) a plain store that bypasses it.
    pub(super) fn store(&mut self, obj: Src, index: u32, val: Src, barrier: bool) {
        self.value(val);
        self.g.mov_reg_reg(R::V6, R::V0);
        self.value(obj);
        self.g.add_reg_imm32(R::V0, (index * 8) as i32);
        self.g.mov_reg_reg(R::V7, R::V0);
        if barrier {
            self.g.call_label(self.l.field_store);
        } else {
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

    /// V0 = a counter or flag.
    pub(super) fn counter(&mut self, counter: Counter) {
        self.g
            .load_ptr_disp32(R::V0, R::V3, counter_offset(counter));
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

    /// Pop the newest RootCell (root cells are LIFO).
    pub(super) fn pop_root(&mut self) {
        self.g.call_label(self.l.root_pop);
    }

    pub(super) fn marked(&mut self, obj: Src) {
        self.value(obj);
        emit_marked(self.g);
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

    /// Small allocations until the relocation phase has finished.
    pub(super) fn settle(&mut self) {
        let top = self.g.create_text_label();
        self.g.bind_text_label(top);
        self.g.mov_imm64(R::V7, 16);
        self.g.mov_imm64(R::V6, 1);
        self.g.call_label(self.l.alloc);
        self.counter(Counter::PhaseOpen);
        self.g.test_reg_reg(R::V0, R::V0);
        self.g.jcc_label(C::NotEqual, top);
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
    let target = NativeTargetContext::for_target(NativeTarget::LinuxX86_64);
    let source = SourceFile::new("<gc-harness>", "1");
    let expr =
        klassic_syntax::parse_inline_expression("<gc-harness>", "1").expect("trivial source");
    let mut generator = NativeCodeGenerator::new(source, None, target.platform);
    generator.gc_generational = true;
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
    let path = std::env::temp_dir().join(format!("klassic-harness-{}-{nanos}", std::process::id()));
    std::fs::write(&path, bytes).expect("write harness executable");
    std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o755)).expect("chmod");
    let output = (0..50)
        .find_map(|attempt| match Command::new(&path).output() {
            Ok(output) => Some(output),
            Err(error) if error.raw_os_error() == Some(26) && attempt < 49 => {
                // ETXTBSY: another test thread forked while the file was open.
                std::thread::sleep(std::time::Duration::from_millis(20));
                None
            }
            Err(error) => panic!("run harness: {error}"),
        })
        .expect("harness ran");
    let _ = std::fs::remove_file(&path);
    let text = String::from_utf8_lossy(&output.stdout).into_owned();
    if std::env::var_os("KLASSIC_HARNESS_DUMP").is_some() {
        eprintln!("--- harness output ---\n{text}");
    }
    assert!(
        output.status.success(),
        "harness failed ({:?})\nstdout:\n{text}\nstderr:\n{}",
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
