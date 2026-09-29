//! Logical allocation shapes shared by compiler lowering and descriptor setup.
//! Legacy tags and generational IDs describe the same payload shapes, but their
//! headers, references, roots and publication protocols remain incompatible.
use crate::gc_abi::TypeDescriptor;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum BuiltinShape {
    RawBytes,
    PointerRecord,
    PointerList,
}

impl BuiltinShape {
    pub(crate) const ALL: [Self; 3] = [Self::RawBytes, Self::PointerRecord, Self::PointerList];

    pub(crate) const fn legacy_tag(self) -> u64 {
        match self {
            Self::RawBytes => crate::gc_layout::GC_TYPE_RAW_BYTES,
            Self::PointerRecord => crate::gc_layout::GC_TYPE_POINTER_RECORD,
            Self::PointerList => crate::gc_layout::GC_TYPE_POINTER_LIST,
        }
    }

    /// Pointer shapes require a stable native one-word bitmap containing 1.
    /// The returned descriptor is unpublished; the caller owns registration.
    pub(crate) const fn descriptor(self, bitmap: u64) -> TypeDescriptor {
        let (id, alignment, fixed_bytes, element_bytes, pointer) = match self {
            Self::RawBytes => (1, 16, 0, 1, false),
            Self::PointerRecord => (2, 8, 0, 8, true),
            Self::PointerList => (4, 8, 8, 8, true),
        };
        TypeDescriptor {
            id,
            alignment,
            fixed_bytes,
            element_bytes,
            fixed_bitmap: 0,
            element_bitmap: if pointer { bitmap } else { 0 },
            fixed_bitmap_words: 0,
            element_bitmap_words: if pointer { 1 } else { 0 },
        }
    }
}
