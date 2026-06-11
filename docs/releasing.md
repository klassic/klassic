# Releasing

Releases are fully automated by `.github/workflows/release.yml`.

## Cutting a release

1. Bump `version` in the root `Cargo.toml` so `klassic --version`
   matches the tag, and commit it.
2. Tag and push:

   ```bash
   git tag v0.2.0
   git push origin v0.2.0
   ```

The workflow runs the full test suite on Linux and macOS, builds a
statically-linked `x86_64-unknown-linux-musl` binary (runs on any
x86_64 Linux with no shared-library dependencies) plus macOS binaries
for `aarch64-apple-darwin` and `x86_64-apple-darwin`, and attaches
`klassic-<tag>-<target>.tar.gz` for each to a GitHub Release with
generated notes.

Users install releases with the one-liner (`install.sh` at the repo
root, served raw from `main`):

```bash
curl -fsSL https://raw.githubusercontent.com/klassic/klassic/main/install.sh | sh
```

## Dry run

To verify the build and packaging without publishing anything, run the
workflow manually (the release step is gated on a tag ref):

```bash
gh workflow run release.yml
```

The musl build needs `musl-gcc` (the runner installs `musl-tools`);
`psm` — `stacker`'s dependency — fails the build without it, so local
verification requires the same package.
