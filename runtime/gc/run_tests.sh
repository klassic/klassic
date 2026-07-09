#!/usr/bin/env bash
# Build and run the C GC unit tests under AddressSanitizer + UBSan.
# Requires clang (KLASSIC_CLANG overrides; falls back to cc). Part of the
# LLVM backend migration (docs/llvm-backend-plan.md, M6).
set -euo pipefail
CC="${KLASSIC_CLANG:-clang-15}"
command -v "$CC" >/dev/null 2>&1 || CC=cc
here="$(cd "$(dirname "$0")" && pwd)"
tmp="$(mktemp -d)"; trap 'rm -rf "$tmp"' EXIT
# Build under AddressSanitizer+UBSan and (since the collector runs a
# background GC thread, true-zgc M4) ThreadSanitizer. -pthread for the thread.
"$CC" -std=c11 -O1 -g -pthread -Wall -Wextra -Werror \
  -fsanitize=address,undefined -fno-omit-frame-pointer \
  "$here/klassic_gc.c" "$here/gc_test.c" -o "$tmp/gc_test_asan"
"$CC" -std=c11 -O1 -g -pthread -Wall -Wextra -Werror \
  -fsanitize=thread -fno-omit-frame-pointer \
  "$here/klassic_gc.c" "$here/gc_test.c" -o "$tmp/gc_test_tsan"

# Run with ASLR disabled. On some kernels (notably WSL2) a sanitizer binary's
# shadow-memory setup nondeterministically faults during startup -- before
# main() -- for certain ASLR placements; a trivial "int main" reproduces it
# at ~20%. That flakiness is in the sanitizer runtime, not the collector, so
# we pin the address-space layout with `setarch -R` to make the run
# deterministic. Where setarch is unavailable (e.g. macOS) it is not flaky,
# so fall back to a direct run.
run() {
  if command -v setarch >/dev/null 2>&1; then
    setarch "$(uname -m)" -R "$@"
  else
    "$@"
  fi
}
ASAN_OPTIONS=detect_leaks=0 run "$tmp/gc_test_asan"
run "$tmp/gc_test_tsan"
