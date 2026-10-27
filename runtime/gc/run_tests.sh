#!/usr/bin/env bash
# Separate ASan+UBSan and TSan builds; every suite must run successfully.
set -euo pipefail
CC="${KLASSIC_CLANG:-clang-15}"
command -v "$CC" >/dev/null 2>&1 || CC=cc
here="$(cd "$(dirname "$0")" && pwd)"
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

# Presence alone does not establish permission to disable ASLR (containers
# commonly deny personality()). Failure falls back to an ordinary execution,
# never a skipped test. Sanitizer startup failures remain failures.
runner=()
if command -v setarch >/dev/null 2>&1 && setarch "$(uname -m)" -R true 2>/dev/null; then
  runner=(setarch "$(uname -m)" -R)
fi
passed=0
failed=0
run_case() {
  local label="$1"
  shift
  # Keep buffered suite markers separate from unbuffered worker diagnostics.
  # Sharing one file lets stderr split a stdout line at a stdio buffer boundary.
  if ASAN_OPTIONS=detect_leaks=0 "${runner[@]}" "$@" >"$tmp/result.stdout" 2>"$tmp/result.stderr"; then
    cat "$tmp/result.stdout"
    cat "$tmp/result.stderr" >&2
    echo "PASS $label"
    passed=$((passed + 1))
  else
    local status=$?
    cat "$tmp/result.stdout" >&2
    cat "$tmp/result.stderr" >&2
    echo "FAIL $label (exit $status; sanitizer startup failure is not a pass)" >&2
    failed=$((failed + 1))
  fi
}
for sanitizer in address,undefined thread; do
  for suite in gc_test gc_mt_test gc_boundary_test gc_lifecycle_test gc_heap_test gc_roots_test gc_store_test gc_timing_test; do
    sources=("$here/klassic_gc.c" "$here/$suite.c")
    # White-box boundary checks include the runtime to inspect ownership.
    if [[ "$suite" == gc_boundary_test || "$suite" == gc_lifecycle_test || "$suite" == gc_heap_test || "$suite" == gc_roots_test || "$suite" == gc_store_test || "$suite" == gc_timing_test ]]; then
      sources=("$here/$suite.c")
    fi
    binary="$tmp/${suite}_${sanitizer//,/_}"
    "$CC" -std=c11 -O1 -g -pthread -Wall -Wextra -Werror \
      "-fsanitize=$sanitizer" -fno-omit-frame-pointer \
      "${sources[@]}" -o "$binary"
    if [[ "$suite" == gc_boundary_test ]]; then
      for case in overflow attach zero header stats heal mark-retry trace-budget; do
        run_case "$sanitizer/$suite/$case" "$binary" "$case"
      done
    else
      run_case "$sanitizer/$suite" "$binary"
    fi
  done
done
echo "C GC suite: $passed passed, $failed failed"
[[ "$failed" == 0 ]]
