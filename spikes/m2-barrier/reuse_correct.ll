target triple = "x86_64-pc-linux-gnu"
@gc_bad_mask=external global i64
@gc_strip_mask=external global i64
declare i64 @gc_load_barrier_slow(i64, ptr)
declare i64 @gc_safepoint_alloc()
define i64 @use_value(ptr %slot){
entry:
  %v=load i64, ptr %slot
  %bm=load i64,ptr @gc_bad_mask
  %b=and i64 %v,%bm
  %t=icmp ne i64 %b,0
  br i1 %t,label %s,label %o
s:
  %h=call i64 @gc_load_barrier_slow(i64 %v, ptr %slot)
  br label %o
o:
  %val=phi i64 [%v,%entry],[%h,%s]
  %sm=load i64,ptr @gc_strip_mask
  %raw=and i64 %val,%sm          ; stripped, remapped-at-this-moment pointer
  %sp=call i64 @gc_safepoint_alloc()   ; object evacuated + from-space freed here
  %v2=load i64,ptr %slot
  %h2=call i64 @gc_load_barrier_slow(i64 %v2, ptr %slot)
  %raw2=and i64 %h2,%sm
  ret i64 %raw2
}
