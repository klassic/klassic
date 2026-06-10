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

The workflow runs the full test suite, builds a statically-linked
`x86_64-unknown-linux-musl` binary (runs on any x86_64 Linux with no
shared-library dependencies), and attaches
`klassic-<tag>-x86_64-unknown-linux-musl.tar.gz` to a GitHub Release
with generated notes.

## Dry run

To verify the build and packaging without publishing anything, run the
workflow manually (the release step is gated on a tag ref):

```bash
gh workflow run release.yml
```

The musl build needs `musl-gcc` (the runner installs `musl-tools`);
`psm` — `stacker`'s dependency — fails the build without it, so local
verification requires the same package.
