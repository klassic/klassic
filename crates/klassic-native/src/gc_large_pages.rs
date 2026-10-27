//! Policy for direct-runtime OS page backing, separate from GC page geometry.
use crate::NativeTarget;

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
#[repr(u64)]
pub enum GcLargePages {
    #[default]
    Off = 0,
    Try = 1,
    Required = 2,
}
impl std::str::FromStr for GcLargePages {
    type Err = &'static str;
    fn from_str(value: &str) -> Result<Self, Self::Err> {
        match value {
            "off" => Ok(Self::Off),
            "try" => Ok(Self::Try),
            "required" => Ok(Self::Required),
            _ => Err("--gc-large-pages must be off, try or required"),
        }
    }
}
pub fn validate_gc_large_pages(
    mode: GcLargePages,
    target: NativeTarget,
    minimum: u64,
    maximum: u64,
) -> Result<(), &'static str> {
    if mode != GcLargePages::Off && target != NativeTarget::LinuxX86_64 {
        return Err(
            "--gc-large-pages=try|required is not supported on this native target (Linux x86-64 only)",
        );
    }
    if mode == GcLargePages::Required
        && (!minimum.is_multiple_of(2 << 20) || !maximum.is_multiple_of(2 << 20))
    {
        return Err("--gc-large-pages=required needs min/max heap multiples of 2 MiB");
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn large_page_modes_validate_target_granule_and_public_compiler_scope() {
        for (s, mode) in [
            ("off", GcLargePages::Off),
            ("try", GcLargePages::Try),
            ("required", GcLargePages::Required),
        ] {
            assert_eq!(s.parse::<GcLargePages>(), Ok(mode));
        }
        for bad in ["", "true", "huge", "Required"] {
            assert!(bad.parse::<GcLargePages>().is_err());
        }
        for target in [
            NativeTarget::LinuxX86_64,
            NativeTarget::WindowsX86_64,
            NativeTarget::MacosAarch64,
        ] {
            assert!(
                validate_gc_large_pages(GcLargePages::Off, target, 128 << 10, 16 << 20).is_ok()
            );
            for mode in [GcLargePages::Try, GcLargePages::Required] {
                assert_eq!(
                    validate_gc_large_pages(mode, target, 4 << 20, 64 << 20).is_ok(),
                    target == NativeTarget::LinuxX86_64
                );
            }
        }
        for (min, max) in [(128 << 10, 16 << 20), (4 << 20, (64 << 20) + (128 << 10))] {
            assert!(
                validate_gc_large_pages(
                    GcLargePages::Required,
                    NativeTarget::LinuxX86_64,
                    min,
                    max
                )
                .is_err()
            );
            assert!(
                validate_gc_large_pages(GcLargePages::Try, NativeTarget::LinuxX86_64, min, max)
                    .is_ok()
            );
        }
        for (generational, target, mode) in [
            (false, NativeTarget::LinuxX86_64, GcLargePages::Off),
            (true, NativeTarget::WindowsX86_64, GcLargePages::Try),
            (true, NativeTarget::MacosAarch64, GcLargePages::Required),
        ] {
            let config = crate::NativeCompilerConfig {
                gc_generational: generational,
                target,
                gc_large_pages: Some(mode),
                ..Default::default()
            };
            assert!(crate::compile_source_for_target("pages.kl", "1", config).is_err());
        }
    }
}
