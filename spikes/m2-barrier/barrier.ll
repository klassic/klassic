target triple = "x86_64-pc-linux-gnu"
@gc_bad_mask   = external global i64
@gc_strip_mask = external global i64
declare i64 @gc_load_barrier_slow(i64, ptr)
; barrier_load(slot) -> raw pointer (i64). Mirrors the emitted fast path.
define i64 @barrier_load(ptr %slot) {
entry:
  %v   = load i64, ptr %slot
  %bm  = load i64, ptr @gc_bad_mask
  %bad = and i64 %v, %bm
  %isb = icmp ne i64 %bad, 0
  br i1 %isb, label %slow, label %ok
slow:
  %healed = call i64 @gc_load_barrier_slow(i64 %v, ptr %slot)
  br label %ok
ok:
  %val = phi i64 [ %v, %entry ], [ %healed, %slow ]
  %sm  = load i64, ptr @gc_strip_mask
  %raw = and i64 %val, %sm
  ret i64 %raw
}
