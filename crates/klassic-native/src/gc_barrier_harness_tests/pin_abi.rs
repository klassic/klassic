//! The complete production ELF calls a real C native body. Its C callback
//! reenters emitted managed bridges and performs actual moving collections.
use super::{H, Src, run, slot_offset};
use crate::gc_codegen::production::test_support::*;
use crate::portable_asm::{PortableAsm, Reg as R};
use std::fmt::Write;

pub(crate) fn scope_body(g: &mut crate::NativeCodeGenerator, _: u64) {
    scope_body_with_callback(g, 0, None);
}
pub(super) fn scope_body_with_callback(
    g: &mut crate::NativeCodeGenerator,
    mode: u64,
    callback: Option<&[u8]>,
) {
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 77);
    h.root(0);
    let (scope, user) = pin_abi_fixture(h.g, slot_offset(0));
    if let Some(callback) = callback {
        pin_abi_embed_callback(h.g, scope, user, callback, mode);
    }

    pin_copy_enable_moving(h.g);
    pin_abi_scope_call(h.g, scope);
    h.g.sub_reg_imm8(R::V0, 2);
    lease_require_ok(h.g);
    h.g.mov_imm64(R::V0, 0);
    h.set_root(0); // Only the scope's real independent strong root remains.
    pin_abi_scope_wait(h.g, scope);
    h.report("result", |h| pin_abi_scope_word(h.g, scope, 0));
    h.report("stage", |h| pin_abi_scope_word(h.g, scope, 1));
    h.report("address", |h| pin_abi_scope_word(h.g, scope, 3));
    h.report("error", |h| pin_abi_scope_word(h.g, scope, 4));
    // Terminal scope stepping is idempotent and does not call the body again.
    pin_abi_scope_call(h.g, scope);
    lease_require_ok(h.g);
    h.report("calls", |h| {
        h.g.mov_data_addr(R::V1, user);
        h.g.load_ptr_disp32(R::V0, R::V1, 16);
    });
    h.report("retries", |h| {
        h.g.mov_data_addr(R::V1, user);
        h.g.load_ptr_disp32(R::V0, R::V1, 32);
    });
    h.report("cleanup_yields", |h| {
        h.g.mov_data_addr(R::V1, user);
        h.g.load_ptr_disp32(R::V0, R::V1, 40);
    });
    h.report("roots", |h| native_root_active(h.g));
    h.report("leases", |h| lease_metric(h.g, 0));
    h.report("borrows", |h| root_native_borrows(h.g));
    h.report("adopted", |h| pin_adopt_metric(h.g));
}

#[test]
fn production_public_pin_scope_calls_real_c_body_and_cleans_up_native_and_running_error_returns() {
    let target = crate::NativeTargetContext::for_target(crate::NativeTarget::LinuxX86_64);
    let expr = klassic_syntax::parse_inline_expression("<pin-native-abi>", "1").unwrap();
    let mut g = crate::NativeCodeGenerator::new(
        crate::SourceFile::new("<pin-native-abi>", "1"),
        None,
        target.platform,
    );
    g.gc_generational = true;
    g.gc_max_heap = 16 << 20;
    g.gc_min_heap = 128 << 10;
    g.gc_test = Some(crate::GcTestProgram {
        body: scope_body,
        arg: 0,
        no_worker_thread: false,
    });
    let object = g.compile(&expr).unwrap();
    let image = crate::write_executable_for_target(target, object);
    let mut header = format!(
        "#define PIN_CALLBACK_MARKER UINT64_C({PIN_C_CALLBACK})\n#define PIN_MODE_MARKER UINT64_C({PIN_C_MODE})\nstatic const unsigned char pin_fixture[]={{"
    );
    for byte in image {
        write!(header, "{byte},").unwrap();
    }
    header.push_str("};\n");
    let output =
        crate::gc_atomic_tests::run_c_driver("pin_abi_fixture.h", &header, "pin_abi_harness.c");
    let expected: String = (0..4).map(|mode| format!("result=77\nstage=5\naddress=0\nerror=0\ncalls=1\nretries={}\ncleanup_yields={}\nroots=0\nleases=0\nborrows=0\nadopted=1\n", u8::from(mode==2), u8::from(mode==1))).collect();
    assert_eq!(output, expected);
}

#[test]
fn production_public_pin_scope_executes_win64_c_callbacks_in_complete_pe() {
    let target = crate::NativeTargetContext::for_target(crate::NativeTarget::WindowsX86_64);
    let expr = klassic_syntax::parse_inline_expression("<pin-win64-abi>", "1").unwrap();
    let mut g = crate::NativeCodeGenerator::new(
        crate::SourceFile::new("<pin-win64-abi>", "1"),
        None,
        target.platform,
    );
    g.gc_generational = true;
    g.gc_max_heap = 16 << 20;
    g.gc_min_heap = 128 << 10;
    g.gc_test = Some(crate::GcTestProgram {
        body: scope_body,
        arg: 0,
        no_worker_thread: false,
    });
    let image = crate::write_executable_for_target(target, g.compile(&expr).unwrap());
    let mut header = format!(
        "#define GC_WINDOWS_PRODUCTION_FIXTURE 1\n#define GC_WINDOWS_PRODUCTION_MODE 0\n#define GC_WINDOWS_PRODUCTION_PIN_ABI 1\n#define PIN_CALLBACK_MARKER UINT64_C({PIN_C_CALLBACK})\n#define PIN_MODE_MARKER UINT64_C({PIN_C_MODE})\nstatic const unsigned char production_fixture[]={{"
    );
    for byte in image {
        write!(header, "{byte},").unwrap();
    }
    header.push_str("};\n");
    let output = crate::gc_atomic_tests::run_c_driver(
        "windows_production_fixture.h",
        &header,
        "pin_abi_windows_harness.c",
    );
    let expected: String=(0..4).map(|mode|format!("result=77\nstage=5\naddress=0\nerror=0\ncalls=1\nretries={}\ncleanup_yields={}\nroots=0\nleases=0\nborrows=0\nadopted=1\n",u8::from(mode==2), u8::from(mode==1))).collect();
    assert_eq!(output, expected);
}

fn prepare_failure(g: &mut crate::NativeCodeGenerator, _: u64) {
    use crate::portable_asm::Condition as C;
    g.gc_max_heap = 16 << 20;
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 77);
    h.root(0);
    let (scope, user) = pin_abi_fixture(h.g, slot_offset(0));
    pin_abi_scope_unused_body(h.g, scope);
    pin_abi_scope_call(h.g, scope);
    h.g.sub_reg_imm8(R::V0, 2);
    lease_require_ok(h.g);
    h.report("funded", |h| native_root_active(h.g));
    let limit = pin_destination_limit(h.g);
    h.g.asm
        .data
        .resize(h.g.asm.data.len().next_multiple_of(8), 0);
    let tokens =
        h.g.asm
            .data_label_with_i64s(&vec![0; (limit as usize + 1) * 3]);
    h.g.mov_imm64(R::V0, 0);
    h.g.store_rbp_slot(1856, R::V0);
    let again = h.g.create_text_label();
    let full = h.g.create_text_label();
    h.g.bind_text_label(again);
    span_at(h.g, tokens);
    h.g.load_rbp_slot(R::V0, slot_offset(0));
    pin_destination_dynamic(h.g, false);
    h.g.cmp_reg_imm8(R::V0, 6);
    h.g.jcc_label(C::Equal, full);
    lease_require_ok(h.g);
    h.g.load_rbp_slot(R::V0, 1856);
    h.g.add_reg_imm32(R::V0, 1);
    h.g.store_rbp_slot(1856, R::V0);
    h.g.cmp_reg_imm32(R::V0, limit as i32);
    h.g.jcc_label(C::Below, again);
    h.g.mov_imm64(R::V0, 1);
    lease_require_ok(h.g);
    h.g.bind_text_label(full);
    pin_abi_scope_call(h.g, scope);
    h.keep(9);
    h.report("failure", |h| h.value(Src::Raw(9)));
    h.report("stage", |h| pin_abi_scope_word(h.g, scope, 1));
    h.report("error", |h| pin_abi_scope_word(h.g, scope, 4));
    h.report("address", |h| pin_abi_scope_word(h.g, scope, 3));
    h.report("pending_roots", |h| native_root_active(h.g));
    pin_abi_scope_call(h.g, scope);
    h.keep(9);
    h.report("repeat_failure", |h| h.value(Src::Raw(9)));
    h.report("terminal_stage", |h| pin_abi_scope_word(h.g, scope, 1));
    h.report("roots", |h| native_root_active(h.g));
    h.report("calls", |h| {
        h.g.mov_data_addr(R::V1, user);
        h.g.load_ptr_disp32(R::V0, R::V1, 16);
    });
    let cancel = h.g.create_text_label();
    h.g.bind_text_label(cancel);
    h.g.load_rbp_slot(R::V0, 1856);
    h.g.sub_reg_imm8(R::V0, 1);
    h.g.store_rbp_slot(1856, R::V0);
    span_at(h.g, tokens);
    pin_destination_dynamic(h.g, true);
    lease_require_ok(h.g);
    h.g.load_rbp_slot(R::V0, 1856);
    h.g.test_reg_reg(R::V0, R::V0);
    h.g.jcc_label(C::NotEqual, cancel);
    request_full_collection(h.g);
    h.collect();
    h.finish_collection();
    h.report("reservations", |h| pin_destination_metric(h.g, 0));
    h.report("large", |h| pin_destination_metric(h.g, 3));
    h.report("value", |h| h.load(Src::Root(0), 0));
}
fn span_at(g: &mut crate::NativeCodeGenerator, tokens: crate::DataLabel) {
    g.load_rbp_slot(R::V0, 1856);
    g.mov_reg_reg(R::V1, R::V0);
    g.shl_reg_imm8(R::V1, 1);
    g.add_reg_reg(R::V0, R::V1);
    g.shl_reg_imm8(R::V0, 3);
    g.mov_data_addr(R::V7, tokens);
    g.add_reg_reg(R::V7, R::V0);
}
#[test]
fn production_public_pin_scope_cleans_up_owned_preparation_failure_before_returning_error() {
    let o = run(prepare_failure, 0, false);
    assert_eq!(o.get("funded"), 1, "{}", o.text());
    assert_eq!(o.get("failure"), 2, "{}", o.text());
    assert_eq!(o.get("repeat_failure"), 6);
    assert_eq!(o.get("stage"), 4);
    assert_eq!(o.get("pending_roots"), 1);
    assert_eq!(o.get("terminal_stage"), 5);
    assert_eq!(o.get("error"), 6);
    assert_eq!(o.get("address"), 0);
    assert_eq!(o.get("roots"), 0);
    assert_eq!(o.get("calls"), 0);
    assert_eq!(o.get("reservations"), 0);
    assert_eq!(o.get("large"), 0);
    assert_eq!(o.get("value"), 77);
}
