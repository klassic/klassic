//! Real OS TLS operations and read-only lifetime observations.
use super::super::*;
use super::TestActor;
pub(crate) fn tls_runtime(g: &mut crate::NativeCodeGenerator, initialize: bool) {
    let l = &g.gc_gen.as_ref().unwrap().labels;
    let entry = if initialize {
        l.tls_initialize
    } else {
        l.tls_destroy
    };
    g.call_label(entry);
}
pub(crate) fn tls_metric(g: &mut crate::NativeCodeGenerator, users: bool) {
    let rt = g.gc_gen.take().unwrap();
    worker::lock(g, &rt.labels);
    load_rt(
        g,
        R::V0,
        offset_of!(Runtime, thread_tls)
            + if users {
                offset_of!(thread_tls::State, users)
            } else {
                offset_of!(thread_tls::State, stage)
            },
    );
    worker::unlock(g, &rt.labels);
    g.gc_gen = Some(rt);
}
pub(crate) fn tls_actor_install(g: &mut crate::NativeCodeGenerator, actor: TestActor) {
    let entry = g.gc_gen.as_ref().unwrap().labels.tls_install_actor;
    g.push_reg(R::OwnerContext);
    g.mov_data_addr(R::V0, actor.pointer);
    g.load_ptr_disp32(R::OwnerContext, R::V0, 0);
    g.call_label(entry);
    g.pop_reg(R::OwnerContext);
}
pub(crate) fn tls_query_value(g: &mut crate::NativeCodeGenerator) {
    let output = g.asm.data_label_with_i64s(&[0]);
    let (query, fail) = {
        let l = &g.gc_gen.as_ref().unwrap().labels;
        (l.tls_query, l.fail)
    };
    g.mov_data_addr(R::V7, output);
    g.call_label(query);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, fail);
    g.mov_data_addr(R::V0, output);
    g.load_ptr_disp32(R::V0, R::V0, 0);
}
/// The test's foreign native GS association, deliberately not a GC context.
/// It must survive a refused fresh install unchanged. Restore it explicitly.
pub(crate) fn tls_foreign_gs(
    g: &mut crate::NativeCodeGenerator,
    install: bool,
) -> crate::DataLabel {
    let foreign = g.asm.data_label_with_i64s(&[84]);
    let request = g.asm.data_label_with_i64s(&[0, 0]);
    let (set, fail) = {
        let l = &g.gc_gen.as_ref().unwrap().labels;
        (l.tls_os.set, l.fail)
    };
    g.mov_data_addr(R::V7, request);
    if install {
        g.mov_data_addr(R::V0, foreign);
    } else {
        g.mov_imm64(R::V0, 0);
    }
    g.store_ptr_disp32(R::V7, 8, R::V0);
    g.push_reg(R::V0);
    g.push_reg(R::V0);
    g.mov_text_addr(R::V11, set);
    g.call_native_unary(R::V11);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, fail);
    g.pop_reg(R::V1);
    g.pop_reg(R::V1); // native value originally installed
    if install {
        g.push_reg(R::V1);
        g.push_reg(R::V1);
        tls_query_value(g);
        g.pop_reg(R::V1);
        g.pop_reg(R::V1);
        g.cmp_reg_reg(R::V0, R::V1);
        g.jcc_label(C::NotEqual, fail);
    }
    foreign
}
pub(crate) fn tls_foreign_matches(g: &mut crate::NativeCodeGenerator, foreign: crate::DataLabel) {
    let yes = g.create_text_label();
    let done = g.create_text_label();
    tls_query_value(g);
    g.mov_data_addr(R::V1, foreign);
    g.cmp_reg_reg(R::V0, R::V1);
    g.jcc_label(C::Equal, yes);
    g.mov_imm64(R::V0, 0);
    g.jmp_label(done);
    g.bind_text_label(yes);
    g.mov_imm64(R::V0, 1);
    g.bind_text_label(done);
}
pub(crate) fn tls_actor_clear(g: &mut crate::NativeCodeGenerator, actor: TestActor) {
    let entry = g.gc_gen.as_ref().unwrap().labels.tls_clear_actor;
    g.mov_data_addr(R::V7, actor.pointer);
    g.load_ptr_disp32(R::V7, R::V7, 0);
    g.call_label(entry);
}

pub(crate) fn tls_value_is_actor(g: &mut crate::NativeCodeGenerator, actor: TestActor) {
    let yes = g.create_text_label();
    let done = g.create_text_label();
    tls_query_value(g);
    g.mov_data_addr(R::V1, actor.pointer);
    g.load_ptr_disp32(R::V1, R::V1, 0);
    g.cmp_reg_reg(R::V0, R::V1);
    g.jcc_label(C::Equal, yes);
    g.mov_imm64(R::V0, 0);
    g.jmp_label(done);
    g.bind_text_label(yes);
    g.mov_imm64(R::V0, 1);
    g.bind_text_label(done);
}
pub(crate) fn tls_release_unregistered_probe(g: &mut crate::NativeCodeGenerator, actor: TestActor) {
    let (release, fail) = {
        let l = &g.gc_gen.as_ref().unwrap().labels;
        (l.metadata_native_release, l.fail)
    };
    g.mov_data_addr(R::V7, actor.pointer);
    g.load_ptr_disp32(R::V7, R::V7, 0);
    g.load_ptr_disp32(R::V0, R::V7, 0);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, fail);
    let bytes = offset_of!(OwnerState, roots) + size_of::<RootStorage>() + (16 + 8) * 32;
    g.mov_imm64(R::V6, bytes as u64);
    g.call_label(release);
}

pub(crate) fn tls_current_status(g: &mut crate::NativeCodeGenerator) {
    let output = g.asm.data_label_with_i64s(&[84]);
    let (entry, fail) = {
        let l = &g.gc_gen.as_ref().unwrap().labels;
        (l.tls_current, l.fail)
    };
    g.mov_data_addr(R::V7, output);
    g.call_label(entry);
    let valid = g.create_text_label();
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::Equal, valid);
    g.mov_data_addr(R::V1, output);
    g.load_ptr_disp32(R::V1, R::V1, 0);
    g.test_reg_reg(R::V1, R::V1);
    g.jcc_label(C::NotEqual, fail);
    g.bind_text_label(valid);
}
pub(crate) fn tls_current_is_actor(g: &mut crate::NativeCodeGenerator, actor: TestActor) {
    let output = g.asm.data_label_with_i64s(&[84]);
    let (entry, fail) = {
        let l = &g.gc_gen.as_ref().unwrap().labels;
        (l.tls_current, l.fail)
    };
    g.mov_data_addr(R::V7, output);
    g.call_label(entry);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, fail);
    g.mov_data_addr(R::V1, output);
    g.load_ptr_disp32(R::V1, R::V1, 0);
    g.mov_data_addr(R::V0, actor.pointer);
    g.load_ptr_disp32(R::V0, R::V0, 0);
    g.cmp_reg_reg(R::V0, R::V1);
    g.jcc_label(C::NotEqual, fail);
    g.mov_imm64(R::V0, 1);
}
pub(crate) fn tls_lookup_start(g: &mut crate::NativeCodeGenerator) -> crate::DataLabel {
    let lookup = g.asm.data_label_with_i64s(&[0; 7]);
    let entry = g.gc_gen.as_ref().unwrap().labels.tls_lookup_begin;
    g.mov_data_addr(R::V7, lookup);
    g.call_label(entry);
    lookup
}
pub(crate) fn tls_lookup_resume(g: &mut crate::NativeCodeGenerator, lookup: crate::DataLabel) {
    let entry = g.gc_gen.as_ref().unwrap().labels.tls_lookup_step;
    g.mov_data_addr(R::V7, lookup);
    g.call_label(entry);
}
pub(crate) fn tls_readers(g: &mut crate::NativeCodeGenerator) {
    let rt = g.gc_gen.take().unwrap();
    worker::lock(g, &rt.labels);
    load_rt(g, R::V0, thread_tls::READERS);
    worker::unlock(g, &rt.labels);
    g.gc_gen = Some(rt);
}
pub(crate) fn tls_set_unmapped_gs(g: &mut crate::NativeCodeGenerator, value: u64) {
    let request = g.asm.data_label_with_i64s(&[0, value as i64, 0]);
    let (set, fail) = {
        let l = &g.gc_gen.as_ref().unwrap().labels;
        (l.tls_os.set, l.fail)
    };
    g.mov_data_addr(R::V7, request);
    g.mov_text_addr(R::V11, set);
    g.call_native_unary(R::V11);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, fail);
}
