//! Exact nanosecond conversion for the uncommit configuration.
pub(super) fn parse(text: &str) -> Result<u64, String> {
    let invalid = || {
        format!("invalid --gc-uncommit-delay duration '{text}' (use ns/us/ms/s/m; plain seconds)")
    };
    let digits = text.bytes().take_while(u8::is_ascii_digit).count();
    if digits == 0 {
        return Err(invalid());
    }
    let number = text[..digits].parse::<u64>().map_err(|_| invalid())?;
    let unit = match &text[digits..] {
        "ns" => 1,
        "us" => 1_000,
        "ms" => 1_000_000,
        "" | "s" => 1_000_000_000,
        "m" => 60_000_000_000,
        _ => return Err(invalid()),
    };
    number.checked_mul(unit).ok_or_else(invalid)
}
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn exact_units_zero_and_maximum_are_supported() {
        for (s, n) in [
            ("0", 0),
            ("1ns", 1),
            ("2us", 2_000),
            ("3ms", 3_000_000),
            ("300", 300_000_000_000),
            ("300s", 300_000_000_000),
            ("5m", 300_000_000_000),
            ("18446744073709551615ns", u64::MAX),
        ] {
            assert_eq!(parse(s).unwrap(), n, "{s}");
        }
    }
    #[test]
    fn malformed_or_overflowing_durations_are_rejected() {
        for s in [
            "",
            "s",
            "-1",
            "+1",
            " 1s",
            "1 s",
            "1.5s",
            "1h",
            "1MS",
            "18446744073709551616ns",
            "18446744074s",
            "18446744073710ms",
        ] {
            assert!(parse(s).is_err(), "{s}");
        }
    }
}
