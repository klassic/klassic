#ifndef KLASSIC_GC_DESCRIPTOR_CASES_H
#define KLASSIC_GC_DESCRIPTOR_CASES_H
#include "gc_descriptors.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
typedef uint64_t (*DescriptorRegister)(const DescriptorRegistry *, const TypeDescriptor *);
typedef uint64_t (*DescriptorScanner)(const DescriptorRegistry *, DescriptorScan *);
typedef struct {
    DescriptorRegistry registry;
    TypeDescriptor descriptor;
    DescriptorScan scan;
    _Atomic uint64_t slots[8];
    uint64_t fixed[2], element[2], output[258];
} DescriptorFixture;
static const uint64_t descriptor_pointers[] = {
    offsetof(DescriptorFixture, registry.slots), offsetof(DescriptorFixture, descriptor.fixed_bitmap),
    offsetof(DescriptorFixture, descriptor.element_bitmap), offsetof(DescriptorFixture, scan.output),
    offsetof(DescriptorFixture, slots[0]), offsetof(DescriptorFixture, slots[1]),
    offsetof(DescriptorFixture, slots[2]), offsetof(DescriptorFixture, slots[3]),
    offsetof(DescriptorFixture, slots[4]), offsetof(DescriptorFixture, slots[5]),
    offsetof(DescriptorFixture, slots[6]), offsetof(DescriptorFixture, slots[7])
};
static FILE *descriptor_vectors[2];
static unsigned descriptor_cases[2];
static void descriptor_fixture(DescriptorFixture *f) {
    memset(f, 0, sizeof(*f));
    f->registry = (DescriptorRegistry){(uintptr_t)f->slots, 8};
    f->descriptor = (TypeDescriptor){.id = 3, .alignment = 8, .fixed_bytes = 32,
        .fixed_bitmap = (uintptr_t)f->fixed, .fixed_bitmap_words = 1};
    f->fixed[0] = 10; // scalar discriminant, reference, scalar, reference
    for (unsigned i = 0; i < 258; i++) f->output[i] = UINT64_C(0xdeadbeefabcd1234);
    f->scan = (DescriptorScan){3, 32, 0, 256, (uintptr_t)(f->output + 1), 256, 99, 99};
}
// Relocate only pointers actually inside this fixture, including interior/misaligned
// invalid inputs. Synthetic overflow addresses stay unchanged and are never read.
static uint64_t descriptor_normalize(DescriptorFixture *copy, const DescriptorFixture *base) {
    uint64_t mask = 0;
    for (unsigned i = 0; i < sizeof(descriptor_pointers) / 8; i++) {
        uint64_t value;
        unsigned char *field = (unsigned char *)copy + descriptor_pointers[i];
        memcpy(&value, field, 8);
        if (value >= (uintptr_t)base && value - (uintptr_t)base < sizeof(*base)) {
            value -= (uintptr_t)base; mask |= UINT64_C(1) << i;
            memcpy(field, &value, 8);
        }
    }
    return mask;
}
static void descriptor_copy(DescriptorFixture *dst, const DescriptorFixture *src) {
    memcpy(dst, src, sizeof(*dst));
    uint64_t mask = descriptor_normalize(dst, src);
    for (unsigned i = 0; i < sizeof(descriptor_pointers) / 8; i++) if (mask & (UINT64_C(1) << i)) {
        uint64_t value;
        unsigned char *field = (unsigned char *)dst + descriptor_pointers[i];
        memcpy(&value, field, 8); value += (uintptr_t)dst; memcpy(field, &value, 8);
    }
}
static uint64_t descriptor_compare(DescriptorFixture *a, DescriptorFixture *c, unsigned op,
        DescriptorRegister reg, DescriptorScanner scan, uint64_t expected) {
    DescriptorFixture before_a, before_c, x, y;
    memcpy(&before_a, a, sizeof(*a)); memcpy(&before_c, c, sizeof(*c));
    uint64_t result = op ? scan(&a->registry, &a->scan) : reg(&a->registry, &a->descriptor);
    uint64_t reference = op ? gc_descriptor_scan(&c->registry, &c->scan) :
                              gc_descriptor_register(&c->registry, &c->descriptor);
    if (result != expected || reference != expected) {
        fprintf(stderr, "descriptor op %u case %u got %llu C %llu expected %llu\n", op,
            descriptor_cases[op], (unsigned long long)result, (unsigned long long)reference,
            (unsigned long long)expected);
        abort();
    }
    if ((!op && result) || (op && result == GC_SCAN_INVALID)) {
        assert(memcmp(a, &before_a, sizeof(*a)) == 0);
        assert(memcmp(c, &before_c, sizeof(*c)) == 0);
    }
    memcpy(&x, a, sizeof(x)); memcpy(&y, c, sizeof(y));
    assert(descriptor_normalize(&x, a) == descriptor_normalize(&y, c));
    assert(memcmp(&x, &y, sizeof(x)) == 0);
    if (descriptor_vectors[op]) {
        memcpy(&x, &before_c, sizeof(x)); memcpy(&y, c, sizeof(y));
        uint64_t record[] = {0, reference, descriptor_normalize(&x, c), descriptor_normalize(&y, c)};
        assert(fwrite(record, sizeof(record), 1, descriptor_vectors[op]) == 1);
        assert(fwrite(&x, sizeof(x), 1, descriptor_vectors[op]) == 1);
        assert(fwrite(&y, sizeof(y), 1, descriptor_vectors[op]) == 1);
    }
    descriptor_cases[op]++;
    return result;
}
static void gc_run_descriptor_cases(DescriptorRegister reg, DescriptorScanner scan) {
    DescriptorFixture a, c;
    const char *names[] = {"KLASSIC_GC_DESCRIPTOR_VECTORS", "KLASSIC_GC_SCAN_VECTORS"};
    const char *magic[] = {"KLDESC01", "KLSCAN01"};
    for (unsigned op = 0; op < 2; op++) {
        descriptor_cases[op] = 0;
        const char *path = getenv(names[op]);
        if (path) {
            descriptor_vectors[op] = fopen(path, "wb");
            assert(descriptor_vectors[op] && fwrite(magic[op], 8, 1, descriptor_vectors[op]) == 1);
            uint64_t header[] = {sizeof(a), offsetof(DescriptorFixture, registry),
                op ? offsetof(DescriptorFixture, scan) : offsetof(DescriptorFixture, descriptor),
                sizeof(descriptor_pointers) / 8};
            assert(fwrite(header, sizeof(header), 1, descriptor_vectors[op]) == 1);
            assert(fwrite(descriptor_pointers, sizeof(descriptor_pointers), 1, descriptor_vectors[op]) == 1);
        }
    }
    for (unsigned fault = 0; fault < 29; fault++) {
        descriptor_fixture(&a);
        switch (fault) {
        case 0: a.registry.slots = 0; break;
        case 1: a.registry.slots++; break;
        case 2: a.registry.capacity = 1; break;
        case 3: a.registry.capacity = UINT64_C(0x100000001); break;
        case 4: a.registry.slots = UINT64_MAX - 7; break;
        case 5: a.descriptor.id = 0; break;
        case 6: a.descriptor.id = 8; break;
        case 7: a.descriptor.alignment = 0; break;
        case 8: a.descriptor.alignment = 3; break;
        case 9: a.descriptor.alignment = 32; break;
        case 10: a.descriptor.fixed_bytes = GC_MAX_OFFSET + 1; break;
        case 11: a.descriptor.element_bytes = GC_MAX_OFFSET + 1; break;
        case 12: a.descriptor.fixed_bitmap = 0; break;
        case 13: a.descriptor.fixed_bitmap++; break;
        case 14: a.descriptor.fixed_bitmap_words = 0; break;
        case 15: a.descriptor.fixed_bitmap_words = 2; break;
        case 16: a.fixed[0] = 16; break;
        case 17: a.descriptor.fixed_bytes = 7; break;
        case 18: a.descriptor.fixed_bitmap = UINT64_MAX - 7; break;
        case 19: a.descriptor.element_bitmap_words = 1; break;
        default:
            a.descriptor.element_bytes = 24; a.descriptor.element_bitmap_words = 1;
            a.descriptor.element_bitmap = (uintptr_t)a.element; a.element[0] = 5;
            switch (fault) {
            case 20: a.descriptor.element_bitmap++; break;
            case 21: a.descriptor.element_bitmap_words = 0; break;
            case 22: a.descriptor.element_bitmap_words = 2; break;
            case 23: a.element[0] = 8; break;
            case 24: a.descriptor.element_bytes = 0; break;
            case 25: a.descriptor.element_bytes = 25; break;
            case 26: a.descriptor.fixed_bytes = 33; break;
            case 27: a.descriptor.element_bitmap = UINT64_MAX - 7; break;
            case 28: a.descriptor.fixed_bitmap_words = UINT64_MAX; break;
            }
        }
        descriptor_copy(&c, &a);
        descriptor_compare(&a, &c, 0, reg, scan, GC_DESC_INVALID);
    }
    // Independent reference-offset expectations, including multiword/bit-63
    // bitmaps, mixed repeated elements, and a fixed region with a partial tail.
    for (unsigned shape = 0; shape < 8; shape++) for (unsigned n = 0; n < 5; n++)
    for (unsigned budget = 1; budget <= 256; budget *= 4) for (unsigned cap = 1; cap <= 256; cap *= 4) {
        descriptor_fixture(&a);
        uint64_t expected[512], count = 0, payload = 0;
        switch (shape) {
        case 0: // raw bytes
            a.descriptor.fixed_bytes = 0; a.descriptor.fixed_bitmap = a.descriptor.fixed_bitmap_words = 0;
            a.descriptor.element_bytes = 1; payload = n * 13; break;
        case 1: case 2: // pointer record / scalar-length-prefixed pointer list
            a.descriptor.fixed_bytes = shape == 2 ? 8 : 0;
            a.descriptor.fixed_bitmap = a.descriptor.fixed_bitmap_words = 0;
            a.descriptor.element_bytes = 8; a.descriptor.element_bitmap = (uintptr_t)a.element;
            a.descriptor.element_bitmap_words = 1; a.element[0] = 1;
            payload = a.descriptor.fixed_bytes + n * 8;
            for (unsigned j = 0; j < n; j++) expected[count++] = a.descriptor.fixed_bytes + j * 8;
            break;
        case 3: payload = 32; expected[count++] = 8; expected[count++] = 24; break;
        case 4:
            a.descriptor.fixed_bytes = 8; a.descriptor.fixed_bitmap = a.descriptor.fixed_bitmap_words = 0;
            a.descriptor.element_bytes = 24; a.descriptor.element_bitmap = (uintptr_t)a.element;
            a.descriptor.element_bitmap_words = 1; a.element[0] = 5; payload = 8 + n * 24;
            for (unsigned j = 0; j < n; j++) { expected[count++] = 8 + j * 24; expected[count++] = 24 + j * 24; }
            break;
        case 5:
            a.descriptor.fixed_bytes = 9; a.fixed[0] = 1; a.descriptor.element_bytes = 1;
            payload = 9 + n; expected[count++] = 0; break;
        case 6: case 7:
            a.descriptor.fixed_bytes = 1024; a.descriptor.fixed_bitmap_words = 2; payload = 1024;
            a.fixed[0] = shape == 6 ? UINT64_C(0x8000000000000001) : UINT64_MAX;
            a.fixed[1] = shape == 6 ? UINT64_C(0x8000000000000003) : 0;
            if (shape == 6) {
                expected[count++] = 0; expected[count++] = 504; expected[count++] = 512;
                expected[count++] = 520; expected[count++] = 1016;
            } else for (unsigned j = 0; j < 64; j++) expected[count++] = j * 8;
            break;
        }
        a.scan.payload_bytes = payload; a.scan.slot_budget = budget; a.scan.output_capacity = cap;
        descriptor_copy(&c, &a);
        descriptor_compare(&a, &c, 0, reg, scan, GC_DESC_OK);
        descriptor_compare(&a, &c, 0, reg, scan, GC_DESC_DUPLICATE);
        uint64_t seen = 0, calls = 0;
        for (;;) {
            // C decides chunk boundaries; independent list below proves the
            // combined answer and budget/progress constraints, not just equality.
            DescriptorFixture probe; descriptor_copy(&probe, &c);
            uint64_t wanted = gc_descriptor_scan(&probe.registry, &probe.scan);
            uint64_t old = a.scan.next_offset;
            uint64_t result = descriptor_compare(&a, &c, 1, reg, scan, wanted);
            assert(result != GC_SCAN_INVALID && a.scan.examined <= budget && a.scan.written <= cap);
            assert(a.scan.next_offset <= payload && (result == GC_SCAN_DONE || a.scan.next_offset > old));
            for (uint64_t j = 0; j < a.scan.written; j++) {
                assert(seen < count && a.output[j + 1] == expected[seen]); seen++;
            }
            assert(a.output[0] == UINT64_C(0xdeadbeefabcd1234));
            assert(a.output[cap + 1] == UINT64_C(0xdeadbeefabcd1234));
            assert(++calls <= 130);
            if (result == GC_SCAN_DONE) break;
        }
        assert(seen == count);
        descriptor_compare(&a, &c, 1, reg, scan, GC_SCAN_DONE);
        assert(!a.scan.written && !a.scan.examined);
    }
    // Every bitmap tail width, with both whole and partial final bytes.
    // The final representable reference is legal; the next bit is not.
    for (unsigned slots = 1; slots <= 128; slots++) for (unsigned partial = 0; partial < 2; partial++) {
        descriptor_fixture(&a);
        a.descriptor.fixed_bytes = slots * 8 + partial;
        a.descriptor.fixed_bitmap_words = (slots + 63) / 64;
        a.fixed[0] = a.fixed[1] = 0;
        a.fixed[(slots - 1) / 64] = UINT64_C(1) << ((slots - 1) & 63);
        a.scan.payload_bytes = a.descriptor.fixed_bytes;
        descriptor_copy(&c, &a);
        descriptor_compare(&a, &c, 0, reg, scan, GC_DESC_OK);
        descriptor_compare(&a, &c, 1, reg, scan, GC_SCAN_DONE);
        assert(a.scan.written == 1 && a.output[1] == (slots - 1) * 8);
        assert(a.scan.examined == slots && a.scan.next_offset == slots * 8 + partial);
        if (slots & 63) {
            // New fixture lifetime; never mutate a published bitmap.
            descriptor_fixture(&a);
            a.descriptor.fixed_bytes = slots * 8 + partial;
            a.descriptor.fixed_bitmap_words = (slots + 63) / 64;
            a.fixed[0] = a.fixed[1] = 0;
            a.fixed[slots / 64] = UINT64_C(1) << (slots & 63);
            descriptor_copy(&c, &a);
            descriptor_compare(&a, &c, 0, reg, scan, GC_DESC_INVALID);
        }
    }
    for (unsigned fault = 0; fault < 16; fault++) {
        descriptor_fixture(&a);
        assert(gc_descriptor_register(&a.registry, &a.descriptor) == GC_DESC_OK);
        switch (fault) {
        case 0: a.scan.descriptor_id = 0; break;
        case 1: a.scan.descriptor_id = 8; break;
        case 2: a.scan.descriptor_id = 2; break;
        case 3: a.scan.payload_bytes = GC_MAX_OFFSET + 1; break;
        case 4: a.scan.next_offset = 33; break;
        case 5: a.scan.next_offset = 1; break;
        case 6: a.scan.slot_budget = 0; break;
        case 7: a.scan.slot_budget = 257; break;
        case 8: a.scan.output_capacity = 0; break;
        case 9: a.scan.output_capacity = 257; break;
        case 10: a.scan.output = 0; break;
        case 11: a.scan.output++; break;
        case 12: a.scan.output = UINT64_MAX - 7; break;
        case 13: a.scan.payload_bytes = 24; break;
        case 14: a.scan.payload_bytes = 40; break;
        case 15: a.registry.capacity = 1; break;
        }
        descriptor_copy(&c, &a);
        descriptor_compare(&a, &c, 1, reg, scan, GC_SCAN_INVALID);
    }
    for (unsigned refs = 0; refs < 2; refs++) {
        descriptor_fixture(&a);
        a.descriptor.fixed_bytes = 0; a.descriptor.fixed_bitmap = a.descriptor.fixed_bitmap_words = 0;
        a.descriptor.element_bytes = refs ? 8 : 1;
        if (refs) { a.descriptor.element_bitmap = (uintptr_t)a.element; a.descriptor.element_bitmap_words = 1; a.element[0] = 1; }
        a.scan.payload_bytes = UINT64_C(1) << 40;
        descriptor_copy(&c, &a);
        descriptor_compare(&a, &c, 0, reg, scan, GC_DESC_OK);
        descriptor_compare(&a, &c, 1, reg, scan, refs ? GC_SCAN_MORE : GC_SCAN_DONE);
        assert(a.scan.examined == (refs ? 256 : 0) && a.scan.written == (refs ? 256 : 0));
        assert(a.scan.next_offset == (refs ? 2048 : UINT64_C(1) << 40));
        if (refs) {
            for (unsigned i = 0; i < 256; i++) assert(a.output[i + 1] == i * 8);
            a.scan.payload_bytes++; c.scan.payload_bytes++;
            descriptor_compare(&a, &c, 1, reg, scan, GC_SCAN_INVALID);
        }
    }
    for (unsigned op = 0; op < 2; op++) if (descriptor_vectors[op]) {
        assert(fclose(descriptor_vectors[op]) == 0); descriptor_vectors[op] = NULL;
    }
    printf("descriptor publication %u / bounded shape scan %u cases PASS\n", descriptor_cases[0], descriptor_cases[1]);
}
#endif
