//! Planned home for runtime values, builtins, modules, filesystem helpers,
//! concurrency helpers, and other runtime behavior.
//!
//! Today the crate only exposes the shared error type and the host
//! interface traits used by future shared builtins. Both the evaluator
//! and the portable native runtime call path will converge on these.
//! Native direct-syscall backends keep their existing inline syscall
//! emission and do not link to this crate.

pub mod builtins;
pub mod error;
pub mod fs;
pub mod system;

pub use error::{RuntimeError, RuntimeErrorKind};
pub use fs::FileSystemInterface;
pub use system::SystemInterface;
