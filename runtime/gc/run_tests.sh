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
ASAN_OPTIONS=detect_leaks=0 "$tmp/gc_test"
