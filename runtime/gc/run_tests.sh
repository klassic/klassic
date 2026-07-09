#!/usr/bin/env bash
# Build and run the C GC unit tests under AddressSanitizer + UBSan.
# Requires clang (KLASSIC_CLANG overrides; falls back to cc). Part of the
# LLVM backend migration (docs/llvm-backend-plan.md, M6).
set -euo pipefail
CC="${KLASSIC_CLANG:-clang-15}"
command -v "$CC" >/dev/null 2>&1 || CC=cc
here="$(cd "$(dirname "$0")" && pwd)"
tmp="$(mktemp -d)"; trap 'rm -rf "$tmp"' EXIT
"$CC" -std=c11 -O1 -g -Wall -Wextra -Werror \
  -fsanitize=address,undefined -fno-omit-frame-pointer \
  "$here/klassic_gc.c" "$here/gc_test.c" -o "$tmp/gc_test"

# Run with ASLR disabled. On some kernels (notably WSL2) an ASan binary's
# shadow-memory setup nondeterministically faults during startup -- before
# main() -- for certain ASLR placements; a trivial "int main" reproduces it
# at ~20%. That flakiness is in the sanitizer runtime, not the collector, so
# we pin the address-space layout with `setarch -R` to make the run
# deterministic. Where setarch is unavailable (e.g. macOS) ASan is not flaky,
# so fall back to a direct run.
if command -v setarch >/dev/null 2>&1; then
  ASAN_OPTIONS=detect_leaks=0 setarch "$(uname -m)" -R "$tmp/gc_test"
else
  ASAN_OPTIONS=detect_leaks=0 "$tmp/gc_test"
fi
