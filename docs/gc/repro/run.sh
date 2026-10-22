#!/bin/bash
# Usage: docs/gc/repro/run.sh [extra klassic flags...]
# Builds every repro with --gc=generational --gc-log, compares stdout with the
# evaluator, and prints the gc[generational] line.
#   run.sh               plain
#   run.sh --gc-stress   collect before every allocation
set -u
K=${KLASSIC:-target/debug/klassic}
D=$(dirname "$0")
T=$(mktemp -d)
for f in "$D"/*.kl; do
  b=$(basename "$f" .kl)
  want=$("$K" "$f" 2>/dev/null)
  [ -f "${f%.kl}.expected" ] && want=$(cat "${f%.kl}.expected")  # evaluator overflows its stack on deep recursion
  "$K" --gc=generational --gc-log "$@" build "$f" -o "$T/$b" >/dev/null 2>&1 || { echo "BUILD FAIL $b"; continue; }
  got=$(timeout 600 "$T/$b" 2>"$T/$b.err")
  [ "$got" == "$want" ] && s=OK || s=DIFF
  echo "$s $b :: $(grep -a 'gc\[generational\]' "$T/$b.err" | cut -c1-110)"
done
rm -rf "$T"
