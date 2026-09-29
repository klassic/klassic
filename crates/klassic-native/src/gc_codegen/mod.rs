//! Runtime emission for the generational collector. Startup/concurrency is
//! exercised independently before the default collector is migrated.
pub(crate) mod alloc_start;
pub(crate) mod context;
pub(crate) mod descriptors;
pub(crate) mod handshake;
pub(crate) mod heap;
pub(crate) mod native_builtin_registry;
pub(crate) mod native_metadata;
pub(crate) mod native_roots;
pub(crate) mod pages;
pub(crate) mod root_lifetime;
pub(crate) mod root_storage;
pub(crate) mod roots;
pub(crate) mod threads;
pub(crate) mod windows;
