# Releasing

Releases are fully automated by `.github/workflows/release.yml`.

## Cutting a release

1. Bump `version` in the root `Cargo.toml` so `klassic --version`
   matches the tag (run `cargo build` once so `Cargo.lock` follows),
   and sweep the handful of prose spots that carry the version:
   `README.md`, `docs/book/src/getting-started/install.md`, and
   `docs/book/src/introduction.md` (the 0.3.0 bump missed the book
   files because nothing syncs them automatically).
2. Write the release body at `docs/release-notes/<tag>.md` — the
   `release-notes` workflow job sets it on the GitHub Release and
   fails if the file is missing.
3. Tag and push:

   ```bash
   git tag v0.4.0
   git push origin v0.4.0
   ```

The workflow runs the full test suite on Linux, macOS, and Windows,
builds a statically-linked `x86_64-unknown-linux-musl` binary (runs on
any x86_64 Linux with no shared-library dependencies), macOS binaries
for `aarch64-apple-darwin` and `x86_64-apple-darwin`, and a Windows
`x86_64-pc-windows-msvc` executable, then attaches
`klassic-<tag>-<target>.tar.gz` (`.zip` on Windows) for each to a
GitHub Release whose body is the curated notes file.

Users on Linux/macOS install releases with the one-liner (`install.sh`
at the repo root, served raw from `main`):

```bash
curl -fsSL https://raw.githubusercontent.com/klassic/klassic/main/install.sh | sh
```

Windows users download `klassic-<tag>-x86_64-pc-windows-msvc.zip` from
the GitHub Release, extract `klassic.exe`, and put it on `PATH`.

## Dry run

To verify the build and packaging without publishing anything, run the
workflow manually (the release step is gated on a tag ref):

```bash
gh workflow run release.yml
```

The musl build needs `musl-gcc` (the runner installs `musl-tools`);
`psm` — `stacker`'s dependency — fails the build without it, so local
verification requires the same package.
