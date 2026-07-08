#!/usr/bin/env bash
# M2 barrier feasibility spike (docs/llvm-backend-plan.md, "M2 findings").
# Proves the ZGC load barrier is expressible in LLVM IR and correct under
# clang -O2. Requires clang >= 15 (KLASSIC_CLANG overrides).
set -euo pipefail
CLANG="${KLASSIC_CLANG:-clang-15}"
here="$(cd "$(dirname "$0")" && pwd)"
tmp="$(mktemp -d)"; trap 'rm -rf "$tmp"' EXIT
echo "== correctness: self-heal sticks (slow_calls=1), poison forces all-slow (=3), raw always right =="
"$CLANG" -O2 -Wno-override-module "$here/barrier.ll" "$here/correctness_driver.c" -o "$tmp/c" && "$tmp/c"
echo "== moving-safe: 2nd load of an evacuated slot returns the to-space address, not stale from-space =="
"$CLANG" -O2 -Wno-override-module "$here/two_loads.ll" "$here/forwarding_driver.c" -o "$tmp/f" && "$tmp/f"
echo "== fast-path assembly with dso_local masks (bad-mask load folds into testq mem-operand) =="
"$CLANG" -O2 -Wno-override-module -S "$here/barrier_dso_local.ll" -o - \
  | grep -A9 '^barrier_load:' | grep -vE '^\s*(\.|#)' | head -8
echo "M2 SPIKE OK"
