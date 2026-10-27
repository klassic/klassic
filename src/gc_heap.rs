//! Parse exact binary byte quantities before passing a hard limit to codegen.
pub(super) fn parse(text: &str) -> Result<u64, String> {
    let bytes = quantity(text, "--gc-max-heap")?;
    klassic_native::validate_gc_max_heap(bytes).map_err(str::to_owned)?;
    Ok(bytes)
}
pub(super) fn parse_min(text: &str) -> Result<u64, String> {
    let bytes = quantity(text, "--gc-min-heap")?;
    klassic_native::validate_gc_min_heap(bytes).map_err(str::to_owned)?;
    Ok(bytes)
}
pub(super) fn parse_soft(text: &str) -> Result<u64, String> {
    let bytes = quantity(text, "--gc-soft-max-heap")?;
    klassic_native::validate_gc_min_heap(bytes).map_err(str::to_owned)?;
    Ok(bytes)
}
pub(super) fn parse_metadata(text: &str) -> Result<u64, String> {
    let bytes = quantity(text, "--gc-max-metadata")?;
    klassic_native::validate_gc_max_metadata(bytes).map_err(str::to_owned)?;
    Ok(bytes)
}
fn quantity(text: &str, option: &str) -> Result<u64, String> {
    let digits = text.bytes().take_while(u8::is_ascii_digit).count();
    if digits == 0 {
        return Err(format!("invalid {option} size '{text}'"));
    }
    let number = text[..digits]
        .parse::<u64>()
        .map_err(|_| format!("{option} size overflows u64"))?;
    let multiplier = match text[digits..].to_ascii_lowercase().as_str() {
        "" | "b" => 1,
        "k" | "kib" => 1 << 10,
        "m" | "mib" => 1 << 20,
        "g" | "gib" => 1 << 30,
        _ => return Err(format!("unknown {option} unit in '{text}'")),
    };
    number
        .checked_mul(multiplier)
        .ok_or_else(|| format!("{option} size overflows u64"))
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn exact_binary_units_and_byte_counts_are_accepted() {
        for value in ["16M", "16MiB", "16mib", "16384KiB", "16777216", "16777216B"] {
            assert_eq!(parse(value).unwrap(), 16 << 20);
        }
        assert_eq!(parse("1GiB").unwrap(), 1 << 30);
    }
    #[test]
    fn metadata_limits_use_independent_page_aligned_signed_byte_counts() {
        assert_eq!(parse_metadata("4KiB").unwrap(), 4096);
        assert_eq!(parse_metadata("4GiB").unwrap(), 4 << 30);
        assert_eq!(
            parse_metadata("9223372036854771712").unwrap(),
            i64::MAX as u64 & !4095
        );
        for value in [
            "0",
            "1",
            "4097",
            "-4KiB",
            "4MB",
            "9223372036854775808",
            "18446744073709551615GiB",
        ] {
            assert!(parse_metadata(value).is_err(), "{value}");
        }
    }
    #[test]
    fn minimum_heap_accepts_page_granularity_and_rejects_zero_or_partial_pages() {
        assert_eq!(parse_min("128KiB").unwrap(), 128 << 10);
        assert_eq!(parse_min("4MiB").unwrap(), 4 << 20);
        for s in ["0", "1", "129KiB", "18014398509498368KiB"] {
            assert!(parse_min(s).is_err(), "{s}");
        }
    }
    #[test]
    fn malformed_overflowing_unaligned_and_out_of_range_sizes_are_rejected() {
        for value in [
            "",
            "M",
            "+16M",
            "-16M",
            "16MB",
            "16.5MiB",
            "16 MiB",
            "0",
            "8MiB",
            "16777217",
            "18446744073709551615GiB",
            "18446744073709551616",
            "18014398509498368KiB",
            "2GiB",
        ] {
            assert!(parse(value).is_err(), "{value}");
        }
    }
}
