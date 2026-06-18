//! C-ABI runtime shims for the portable backends (roadmap PR 10).
//!
//! The C backend emits calls to these `klassic_rt_*` functions; the
//! crate builds as a `staticlib` so `cc program.c libklassic_rt.a`
//! produces a native executable on any platform with a C compiler —
//! macOS and Windows enter the target matrix through this path, never
//! through direct Mach-O / PE syscall emission.
//!
//! Semantics are shared with the evaluator by construction: string
//! operations are written against Rust `str` exactly like
//! `klassic-eval`'s builtins (character counts, character-indexed
//! slicing), so the evaluator stays the semantic oracle.
//!
//! Memory: `klassic_rt_alloc` is currently a leak-everything bump
//! allocator — fine for short-lived programs, and the root-stack API
//! (`klassic_rt_push_root` / `klassic_rt_pop_roots`) is already part
//! of the ABI so generated code won't need to change when the
//! mark-sweep collector lands behind it.

use std::io::Write;

/// A Klassic string: UTF-8 bytes, not NUL-terminated. Returned
/// strings point at runtime-allocated memory; literal strings may
/// point at the program's static data.
#[repr(C)]
#[derive(Clone, Copy)]
pub struct KStr {
    pub ptr: *const u8,
    pub len: usize,
}

impl KStr {
    /// # Safety
    /// `ptr` must reference `len` bytes of valid UTF-8 that outlive
    /// the borrow.
    unsafe fn as_str<'a>(self) -> &'a str {
        if self.len == 0 {
            return "";
        }
        unsafe { std::str::from_utf8_unchecked(std::slice::from_raw_parts(self.ptr, self.len)) }
    }
}

fn kstr_from_owned(text: String) -> KStr {
    let len = text.len();
    let ptr = if len == 0 {
        std::ptr::NonNull::<u8>::dangling().as_ptr() as *const u8
    } else {
        Box::leak(text.into_boxed_str()).as_ptr()
    };
    KStr { ptr, len }
}

/// Allocate `size` zeroed bytes that live for the rest of the
/// program. The collector will reclaim through this same entry point
/// once it exists; generated code never frees.
#[unsafe(no_mangle)]
pub extern "C" fn klassic_rt_alloc(size: usize) -> *mut u8 {
    let buffer = vec![0u8; size.max(1)].into_boxed_slice();
    Box::leak(buffer).as_mut_ptr()
}

/// Root-stack ABI reserved for the future collector. No-ops today so
/// generated code can already emit the calls.
#[unsafe(no_mangle)]
pub extern "C" fn klassic_rt_push_root(_slot: *mut u8) {}

#[unsafe(no_mangle)]
pub extern "C" fn klassic_rt_pop_roots(_count: usize) {}

#[unsafe(no_mangle)]
pub extern "C" fn klassic_rt_print_str(s: KStr) {
    let stdout = std::io::stdout();
    let mut handle = stdout.lock();
    let _ = handle.write_all(unsafe { s.as_str() }.as_bytes());
    let _ = handle.flush();
}

#[unsafe(no_mangle)]
pub extern "C" fn klassic_rt_println_str(s: KStr) {
    let stdout = std::io::stdout();
    let mut handle = stdout.lock();
    let _ = handle.write_all(unsafe { s.as_str() }.as_bytes());
    let _ = handle.write_all(b"\n");
    let _ = handle.flush();
}

#[unsafe(no_mangle)]
pub extern "C" fn klassic_rt_println_i64(value: i64) {
    println!("{value}");
}

#[unsafe(no_mangle)]
pub extern "C" fn klassic_rt_println_bool(value: i64) {
    println!("{}", if value != 0 { "true" } else { "false" });
}

/// Render an `f64` exactly like the evaluator's `Value::Double` display:
/// a whole-number value keeps a single trailing decimal (`4.0`), while a
/// fractional value uses the default formatting (`3.14`). Rust's default
/// `f64` formatting drops the `.0`, which would print a `Double` as if it
/// were an `Int`.
fn format_double_like_evaluator(value: f64) -> String {
    if value.fract() == 0.0 {
        format!("{value:.1}")
    } else {
        format!("{value}")
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn klassic_rt_println_f64(value: f64) {
    // Match the evaluator's Double display.
    println!("{}", format_double_like_evaluator(value));
}

#[unsafe(no_mangle)]
pub extern "C" fn klassic_rt_str_concat(a: KStr, b: KStr) -> KStr {
    let mut text = String::with_capacity(a.len + b.len);
    text.push_str(unsafe { a.as_str() });
    text.push_str(unsafe { b.as_str() });
    kstr_from_owned(text)
}

#[unsafe(no_mangle)]
pub extern "C" fn klassic_rt_str_eq(a: KStr, b: KStr) -> i64 {
    i64::from(unsafe { a.as_str() } == unsafe { b.as_str() })
}

/// Character count — evaluator semantics, not byte length.
#[unsafe(no_mangle)]
pub extern "C" fn klassic_rt_str_len_chars(s: KStr) -> i64 {
    unsafe { s.as_str() }.chars().count() as i64
}

/// Character-indexed substring with the evaluator's clamping rules:
/// indices clamp to the character count and `end` never precedes
/// `start`.
#[unsafe(no_mangle)]
pub extern "C" fn klassic_rt_str_substring(s: KStr, start: i64, end: i64) -> KStr {
    let text = unsafe { s.as_str() };
    let chars: Vec<char> = text.chars().collect();
    let start = start.max(0) as usize;
    let start = start.min(chars.len());
    let end = (end.max(0) as usize).min(chars.len()).max(start);
    kstr_from_owned(chars[start..end].iter().collect())
}

#[unsafe(no_mangle)]
pub extern "C" fn klassic_rt_str_at(s: KStr, index: i64) -> KStr {
    klassic_rt_str_substring(s, index, index + 1)
}

/// Render an Int as a Klassic string (`toString` / interpolation).
#[unsafe(no_mangle)]
pub extern "C" fn klassic_rt_i64_to_str(value: i64) -> KStr {
    kstr_from_owned(value.to_string())
}

#[unsafe(no_mangle)]
pub extern "C" fn klassic_rt_f64_to_str(value: f64) -> KStr {
    kstr_from_owned(format_double_like_evaluator(value))
}

#[unsafe(no_mangle)]
pub extern "C" fn klassic_rt_bool_to_str(value: i64) -> KStr {
    kstr_from_owned(if value != 0 { "true" } else { "false" }.to_string())
}

/// Fatal runtime error: report on stderr and exit 1, exactly like the
/// direct native backend's runtime aborts. Never unwinds, so the
/// staticlib needs no unwinding runtime from the C side.
#[unsafe(no_mangle)]
pub extern "C" fn klassic_rt_fatal(message: KStr) -> ! {
    let stderr = std::io::stderr();
    let mut handle = stderr.lock();
    let _ = handle.write_all(b"klassic: ");
    let _ = handle.write_all(unsafe { message.as_str() }.as_bytes());
    let _ = handle.write_all(b"\n");
    drop(handle);
    std::process::exit(1);
}

#[cfg(test)]
mod tests {
    use super::*;

    fn kstr(text: &'static str) -> KStr {
        KStr {
            ptr: text.as_ptr(),
            len: text.len(),
        }
    }

    #[test]
    fn concat_joins_utf8() {
        let joined = klassic_rt_str_concat(kstr("こん"), kstr("にちは"));
        assert_eq!(unsafe { joined.as_str() }, "こんにちは");
    }

    #[test]
    fn eq_compares_contents() {
        assert_eq!(klassic_rt_str_eq(kstr("abc"), kstr("abc")), 1);
        assert_eq!(klassic_rt_str_eq(kstr("abc"), kstr("abd")), 0);
        assert_eq!(klassic_rt_str_eq(kstr(""), kstr("")), 1);
    }

    #[test]
    fn len_counts_characters() {
        assert_eq!(klassic_rt_str_len_chars(kstr("あいう")), 3);
        assert_eq!(klassic_rt_str_len_chars(kstr("abc")), 3);
        assert_eq!(klassic_rt_str_len_chars(kstr("")), 0);
    }

    #[test]
    fn substring_uses_char_indices_and_clamps() {
        let s = kstr("あいうえお");
        assert_eq!(
            unsafe { klassic_rt_str_substring(s, 1, 3).as_str() },
            "いう"
        );
        assert_eq!(
            unsafe { klassic_rt_str_substring(s, 3, 99).as_str() },
            "えお"
        );
        assert_eq!(unsafe { klassic_rt_str_substring(s, 4, 2).as_str() }, "");
        assert_eq!(unsafe { klassic_rt_str_at(s, 2).as_str() }, "う");
    }

    #[test]
    fn to_str_renders_scalars() {
        assert_eq!(unsafe { klassic_rt_i64_to_str(-42).as_str() }, "-42");
        assert_eq!(unsafe { klassic_rt_bool_to_str(1).as_str() }, "true");
    }

    #[test]
    fn alloc_returns_writable_zeroed_memory() {
        let ptr = klassic_rt_alloc(16);
        let slice = unsafe { std::slice::from_raw_parts_mut(ptr, 16) };
        assert!(slice.iter().all(|byte| *byte == 0));
        slice[0] = 7;
        assert_eq!(slice[0], 7);
    }
}
