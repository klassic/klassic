# Installing Klassic

## One command (recommended)

On Linux x86_64 or macOS (Apple Silicon / Intel):

```bash
curl -fsSL https://raw.githubusercontent.com/klassic/klassic/main/install.sh | sh
```

The installer detects your platform, downloads the latest
[release](https://github.com/klassic/klassic/releases) into
`~/.klassic/bin` — the `klassic` binary plus `libklassic_runtime.a`
for the portable C backend — and **verifies the install by compiling
and running a Klassic program with it** before reporting success.

Then put it on your `PATH`:

```bash
export PATH="$HOME/.klassic/bin:$PATH"
klassic --version
```

Two environment variables tune the installer:

| Variable | Effect |
| --- | --- |
| `KLASSIC_VERSION=v0.6.0` | Pin a specific release instead of the latest |
| `KLASSIC_HOME=/opt/klassic` | Change the install root (binaries go to `$KLASSIC_HOME/bin`) |

The Linux build is statically linked (musl) and runs on any x86_64
Linux. The macOS builds run on macOS 11+.

## Windows

One line, in PowerShell:

```powershell
irm https://raw.githubusercontent.com/klassic/klassic/main/install.ps1 | iex
```

The installer downloads the latest release into `%USERPROFILE%\.klassic\bin`,
verifies it by running a Klassic program, and prints the one-liner that
adds it to your user `PATH`. Set `KLASSIC_VERSION` to pin a tag,
`KLASSIC_HOME` to change the install root — the same knobs as the Unix
installer.

Prefer doing it by hand? Download the
`klassic-vX.Y.Z-x86_64-pc-windows-msvc.zip` asset from the latest
[release](https://github.com/klassic/klassic/releases), extract it, and
put the folder containing `klassic.exe` on your `PATH`. Then confirm it
works from PowerShell or `cmd.exe`:

```bat
klassic --version
klassic -e "1 + 2"
```

Windows x86_64 is a tier-0 target with its own direct PE64 backend —
no Visual Studio, MSVC toolchain, or WSL required to run the evaluator,
REPL, or `klassic build`. The Windows release zip ships `klassic.exe`
alone (unlike the Linux/macOS archives, it does not bundle
`libklassic_runtime.a`), so `--backend c` is not available from the
downloaded binary; use the direct PE64 backend (the default on
Windows) for native builds.

## What works where

| | Evaluator / REPL | `klassic build` |
| --- | --- | --- |
| Linux x86_64 | ✅ | ✅ direct ELF64 (most complete) |
| macOS arm64 (Apple Silicon) | ✅ | ✅ direct signed Mach-O, portable-C fallback |
| macOS x86_64 | ✅ | ✅ portable C backend (needs Xcode CLT's `cc`) |
| Windows x86_64 | ✅ | ✅ direct PE64 (core language plus full OS-builtin coverage; ANSI-only paths/env) |
| anywhere Rust runs | ✅ | — |

## Build from source

If you prefer the bleeding edge, Klassic builds with the standard Rust
toolchain (a recent stable; CI tracks current lints):

```bash
git clone https://github.com/klassic/klassic.git
cd klassic
cargo build --release
export PATH="$PWD/target/release:$PATH"
```

## Sanity check

```bash
klassic -e "1 + 2"
# 3
```

If that prints `3`, you have a working install.

## Run the test suite (optional)

```bash
cargo test
cargo fmt --check
cargo clippy --all-targets --all-features -- -D warnings
```

The CI configuration in `.github/workflows/ci.yml` runs the same three
gates on Linux and macOS for every push.
