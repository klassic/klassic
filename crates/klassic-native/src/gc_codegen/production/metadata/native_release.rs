//! Release exclusively owned native root backing, after actual registration
//! retirement. An OS failure retains the mapping and its accounting for retry.
use super::*;

pub(super) fn emit<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    // V7=address, V6=requested bytes -> 0 success, 6 OS failure.
    // No collector lock, guard or journal lease may cross the OS operation.
    a.bind_text_label(l.metadata_native_release);
    a.add_reg_imm32(R::V6, (a.gc_metadata_alignment() - 1) as i32);
    a.and_reg_imm32(R::V6, -(a.gc_metadata_alignment() as i32));
    a.push_reg(R::V6);
    a.push_reg(R::V6);
    #[cfg(test)]
    {
        let ordinary = a.create_text_label();
        let field = offset_of!(Runtime, metadata) + offset_of!(State, release_fail_after);
        load_rt(a, R::V0, field);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::Equal, ordinary);
        a.sub_reg_imm8(R::V0, 1);
        store_rt(a, field, R::V0);
        a.jcc_label(C::NotEqual, ordinary);
        a.mov_imm64(R::V7, u64::MAX); // Real OS rejects this; retain owned address/charge.
        a.bind_text_label(ordinary);
    }
    a.call_label(l.memory.release);
    a.pop_reg(R::V6);
    a.pop_reg(R::V6);
    super::super::worker::lock_raw(a, l);
    let failed = a.create_text_label();
    let done = a.create_text_label();
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, failed);
    a.mov_imm64(R::V8, 0);
    a.sub_reg_reg(R::V8, R::V6);
    adjust(a, COMMITTED);
    adjust(a, CHARGED);
    a.mov_imm64(R::V8, 1);
    adjust(a, reclaim::UNMAPS);
    a.mov_imm64(R::V0, 0);
    a.jmp_label(done);
    a.bind_text_label(failed);
    a.mov_imm64(R::V8, 1);
    adjust(a, reclaim::FAILURES);
    a.mov_imm64(R::V0, 6);
    a.bind_text_label(done);
    super::super::worker::unlock_raw(a, l);
    a.ret();
}
