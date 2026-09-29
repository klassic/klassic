/* Native descriptor publication and bounded shape scanning. No managed heap
 * address is read or retained here. Caller supplies live, disjoint native
 * buffers of the stated sizes; validation is not an arbitrary-pointer probe.
 * Registry slots start at zero and cannot be removed/replaced while heap/jobs
 * use them. Descriptors and bitmaps become immutable BEFORE publication. */
#ifndef KLASSIC_GC_DESCRIPTORS_H
#define KLASSIC_GC_DESCRIPTORS_H
#include "gc_abi.h"
#define GC_DESC_OK 0u
#define GC_DESC_INVALID 1u
#define GC_DESC_DUPLICATE 2u
#define GC_SCAN_DONE 0u
#define GC_SCAN_MORE 1u
#define GC_SCAN_INVALID 2u

static inline bool gc_descriptor_bitmap_valid(uint64_t bytes, uint64_t address, uint64_t words) {
    if (!address) return words == 0;
    uint64_t slots = bytes / 8, expected = (slots + 63) / 64;
    if ((address & 7) || !words || words != expected || address > UINT64_MAX - words * 8) return false;
    uint64_t tail = slots & 63;
    return !tail || (((const uint64_t *)(uintptr_t)address)[words - 1] >> tail) == 0;
}
static inline bool gc_descriptor_registry_valid(const DescriptorRegistry *r) {
    return r->slots && !(r->slots & 7) && r->capacity > 1 && r->capacity <= UINT64_C(0x100000000) &&
        r->slots <= UINT64_MAX - r->capacity * 8;
}
static inline uint64_t gc_descriptor_register(const DescriptorRegistry *r, const TypeDescriptor *d) {
    if (!gc_descriptor_registry_valid(r) || !d->id || d->id >= r->capacity ||
        !d->alignment || d->alignment > 16 || (d->alignment & (d->alignment - 1)) ||
        d->fixed_bytes > GC_MAX_OFFSET || d->element_bytes > GC_MAX_OFFSET ||
        !gc_descriptor_bitmap_valid(d->fixed_bytes, d->fixed_bitmap, d->fixed_bitmap_words) ||
        !gc_descriptor_bitmap_valid(d->element_bytes, d->element_bitmap, d->element_bitmap_words) ||
        (d->element_bitmap && ((d->fixed_bytes | d->element_bytes) & 7))) return GC_DESC_INVALID;
    _Atomic uint64_t *slot = (_Atomic uint64_t *)(uintptr_t)r->slots + d->id;
    uint64_t empty = 0;
    return atomic_compare_exchange_strong_explicit(slot, &empty, (uintptr_t)d,
        memory_order_acq_rel, memory_order_acquire) ? GC_DESC_OK : GC_DESC_DUPLICATE;
}

/* No more than 256 bitmap candidates per call. Scalar-only regions are skipped
 * arithmetically, regardless of their byte length. Budgets measure candidate
 * slots actually examined, not the number of bytes skipped without heap reads.
 * On INVALID all request/output storage is unchanged. MORE retains only an
 * offset; a real collector job must retain its rooted owner and phase lifetime. */
static inline uint64_t gc_descriptor_scan(const DescriptorRegistry *r, DescriptorScan *s) {
    if (!gc_descriptor_registry_valid(r) || !s->descriptor_id || s->descriptor_id >= r->capacity ||
        s->payload_bytes > GC_MAX_OFFSET || s->next_offset > s->payload_bytes ||
        (s->next_offset != s->payload_bytes && (s->next_offset & 7)) ||
        !s->slot_budget || s->slot_budget > GC_DESCRIPTOR_SCAN_MAX_SLOTS ||
        !s->output_capacity || s->output_capacity > GC_DESCRIPTOR_SCAN_MAX_SLOTS ||
        !s->output || (s->output & 7) || s->output > UINT64_MAX - s->output_capacity * 8)
        return GC_SCAN_INVALID;
    uint64_t address = atomic_load_explicit((_Atomic uint64_t *)(uintptr_t)r->slots + s->descriptor_id, memory_order_acquire);
    if (!address) return GC_SCAN_INVALID;
    const TypeDescriptor *d = (const TypeDescriptor *)(uintptr_t)address;
    if (d->id != s->descriptor_id || s->payload_bytes < d->fixed_bytes ||
        (d->element_bytes ? (s->payload_bytes - d->fixed_bytes) % d->element_bytes :
                            s->payload_bytes != d->fixed_bytes)) return GC_SCAN_INVALID;
    uint64_t cursor = s->next_offset, written = 0, examined = 0;
    uint64_t *out = (uint64_t *)(uintptr_t)s->output;
    while (cursor < s->payload_bytes) {
        uint64_t bitmap, index;
        if (cursor < d->fixed_bytes) {
            if (!d->fixed_bitmap || d->fixed_bytes - cursor < 8) { cursor = d->fixed_bytes; continue; }
            bitmap = d->fixed_bitmap;
            index = cursor / 8;
        } else {
            if (!d->element_bitmap) { cursor = s->payload_bytes; break; }
            bitmap = d->element_bitmap;
            index = ((cursor - d->fixed_bytes) % d->element_bytes) / 8;
        }
        if (examined == s->slot_budget || written == s->output_capacity) break;
        examined++;
        if (((const uint64_t *)(uintptr_t)bitmap)[index / 64] & (UINT64_C(1) << (index & 63)))
            out[written++] = cursor;
        cursor += 8;
    }
    s->next_offset = cursor;
    s->written = written;
    s->examined = examined;
    return cursor == s->payload_bytes ? GC_SCAN_DONE : GC_SCAN_MORE;
}
#endif
