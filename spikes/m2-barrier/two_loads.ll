target triple = "x86_64-pc-linux-gnu"
@gc_bad_mask=external global i64
@gc_strip_mask=external global i64
declare i64 @gc_load_barrier_slow(i64, ptr)
define i64 @two_loads(ptr %slot){
entry:
  %v1=load i64, ptr %slot
  %bm1=load i64,ptr @gc_bad_mask
  %b1=and i64 %v1,%bm1
  %t1=icmp ne i64 %b1,0
  br i1 %t1,label %s1,label %o1
s1:
  %h1=call i64 @gc_load_barrier_slow(i64 %v1, ptr %slot)
  br label %o1
o1:
  %val1=phi i64 [%v1,%entry],[%h1,%s1]
  ; second barriered load of the SAME slot
  %v2=load i64, ptr %slot
  %bm2=load i64,ptr @gc_bad_mask
  %b2=and i64 %v2,%bm2
  %t2=icmp ne i64 %b2,0
  br i1 %t2,label %s2,label %o2
s2:
  %h2=call i64 @gc_load_barrier_slow(i64 %v2, ptr %slot)
  br label %o2
o2:
  %val2=phi i64 [%v2,%o1],[%h2,%s2]
  %sm=load i64,ptr @gc_strip_mask
  %raw=and i64 %val2,%sm
  ret i64 %raw
}

