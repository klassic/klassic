//! Read-only diagnostics at an unexpected generated Young-capacity failure.
//! Never alter collector state or retry a failed allocation on the test's behalf.
use super::super::*;

pub(crate) fn observe_unexpected_young_oom(g: &mut crate::NativeCodeGenerator) {
    let hook = g.create_text_label();
    let body = g.create_text_label();
    g.gc_gen.as_mut().unwrap().labels.oom_hook = Some(hook);
    g.jmp_label(body);
    g.bind_text_label(hook);
    let fields = [
        ("oom_owner", RT_CTX + offset_of!(MutatorContext, thread_id)),
        (
            "oom_phase",
            RT_CTX + offset_of!(MutatorContext, adopted_phase),
        ),
        ("oom_bank", allocation_records::REVISION),
        ("oom_bounds", RT_BOUNDS),
        ("oom_next", RT_NEXT_PAGE),
        ("oom_end", RT_SPACE_END),
        ("oom_open", RT_PHASE_OPEN),
        ("oom_retire", RT_RETIRE_STAGE),
        ("oom_collecting", RT_COLLECTING),
        ("oom_fresh_budget", capacity::FRESH_BUDGET),
        ("oom_active_pages", capacity::ACTIVE),
        ("oom_sealed_bytes", capacity::SEALED),
        (
            "oom_members",
            participants::HANDSHAKE + offset_of!(crate::gc_abi::HandshakeState, members),
        ),
        (
            "oom_native",
            participants::HANDSHAKE + offset_of!(crate::gc_abi::HandshakeState, native),
        ),
        ("oom_request", quiescence::REQUEST),
        ("oom_resuming", phase_round::RESUMING),
        ("oom_worker_done", RT_WORKER_DONE),
        ("oom_debt", root_work::PENDING),
        ("oom_tail_bytes", moving_round::NATIVE_TAIL_BYTES),
    ];
    if let Some(path) = std::env::var_os("KLASSIC_HARNESS_OOM_LAYOUT") {
        let fields = fields
            .iter()
            .map(|(name, field)| {
                format!(
                    "\"{name}\":{{\"owner\":{},\"offset\":{}}}",
                    owner_state::private(*field),
                    owner_state::raw(*field)
                )
            })
            .collect::<Vec<_>>()
            .join(",");
        std::fs::write(path, format!("{{{fields}}}\n"))
            .expect("export actual OOM diagnostic layout");
    }
    for (name, field) in fields {
        let text = format!("{name}=");
        let data = g.asm.data_label_with_bytes(text.as_bytes());
        g.emit_write_data(1, data, text.len());
        load_rt(g, R::V7, field);
        g.mov_imm64(R::V6, 1);
        g.mov_imm64(R::V2, 0);
        // The legacy scalar renderer uses backend scratch, including V3.
        // Each following observation must retain the failing runtime/owner.
        g.push_reg(R::V3);
        g.push_reg(R::OwnerContext);
        g.call_label(g.print_i64);
        g.pop_reg(R::OwnerContext);
        g.pop_reg(R::V3);
        g.emit_write_data(1, g.newline, 1);
    }
    g.ret();
    g.bind_text_label(body);
}
