#!/usr/bin/env bash
# M2 barrier feasibility spike (docs/llvm-backend-plan.md, "M2 findings").
# Proves the ZGC load barrier is expressible in LLVM IR and correct under
# clang -O2, and demonstrates the emission disciplines M7 must follow.
# Requires clang >= 15 (KLASSIC_CLANG overrides).
set -uo pipefail
CLANG="${KLASSIC_CLANG:-clang-15}"
here="$(cd "$(dirname "$0")" && pwd)"
tmp="$(mktemp -d)"; trap 'rm -rf "$tmp"' EXIT
rc=0
echo "== correctness: self-heal sticks (slow_calls=1), poison forces all-slow (=3), raw always right =="
"$CLANG" -O2 -Wno-override-module "$here/barrier.ll" "$here/correctness_driver.c" -o "$tmp/c" && "$tmp/c" || rc=1
echo "== DISCRIMINATING: frontend must NOT reuse a stripped pointer across a safepoint =="
echo "-- wrong (reuse) must report a UAF (stale from-space):"
"$CLANG" -O2 -Wno-override-module "$here/reuse_wrong.ll" "$here/reuse_driver.c" -o "$tmp/rw" && { "$tmp/rw"; [ $? -eq 1 ] || { echo "SPIKE REGRESSION: reuse should be UAF"; rc=1; }; }
echo "-- correct (re-barrier after safepoint) must be moving-safe:"
"$CLANG" -O2 -Wno-override-module "$here/reuse_correct.ll" "$here/reuse_driver.c" -o "$tmp/rc" && "$tmp/rc" || rc=1
echo "== emission model: masks are 'external dso_local' (defined once in the C GC), NOT defined in the .ll =="
echo "-- external dso_local keeps the folded fast path (testq mem-operand):"
"$CLANG" -O2 -Wno-override-module -S "$here/barrier_external_dso_local.ll" -o - 2>/dev/null | grep -E 'testq.*gc_bad_mask\(%rip\)' | head -1
echo "[M2 SPIKE $( [ $rc -eq 0 ] && echo OK || echo FAILED )]"
exit $rc
