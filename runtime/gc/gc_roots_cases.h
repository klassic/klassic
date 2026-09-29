#ifndef KLASSIC_GC_ROOTS_CASES_H
#define KLASSIC_GC_ROOTS_CASES_H
#include "gc_roots.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
typedef uint64_t (*RootStep)(MutatorContext *, RootRequest *, uint64_t);
typedef uint64_t (*RootScanner)(RootScan *);
typedef uint64_t (*RootDrainer)(RootDrain *);
typedef struct {
    MutatorContext ctx;
    RootSegment segment;
    RootRequest request;
    BufferCursor buffer;
    RootScan scan;
    RootDrain drain;
    RootCell cells[300];
    RootChange entries[7];
    uint64_t values[257];
    RootChange changes[257];
} RootFixture;
static const uint64_t root_pointers[] = {
    offsetof(RootFixture, segment.cells), offsetof(RootFixture, segment.journal),
    offsetof(RootFixture, request.segment), offsetof(RootFixture, request.handle.cell),
    offsetof(RootFixture, buffer.entries), offsetof(RootFixture, scan.segment),
    offsetof(RootFixture, scan.output), offsetof(RootFixture, drain.buffer), offsetof(RootFixture, drain.output)
};
static FILE *root_vectors[3];
static unsigned root_cases[3];
static void root_fixture(RootFixture *f) {
    memset(f, 0, sizeof(*f));
    f->ctx.abi_version = GC_ABI_VERSION; f->ctx.thread_id = 17;
    atomic_store(&f->ctx.adopted_phase, UINT64_C(0x12340122));
    f->segment.owner = 17; f->segment.scope = 101;
    f->segment.cells = (uintptr_t)f->cells; f->segment.capacity = 300;
    atomic_store(&f->segment.state, GC_ROOT_SEGMENT_ACTIVE);
    f->segment.journal = (uintptr_t)&f->buffer;
    f->request.segment = (uintptr_t)&f->segment;
    f->buffer.entries = (uintptr_t)f->entries; f->buffer.capacity = 7;
    f->scan = (RootScan){(uintptr_t)&f->segment, 0, (uintptr_t)f->values, 256, 999};
    f->drain = (RootDrain){(uintptr_t)&f->buffer, (uintptr_t)f->changes, 256, 999};
    memset(f->values, 0xab, sizeof(f->values)); memset(f->changes, 0xcd, sizeof(f->changes));
}
static uint64_t root_normalize(RootFixture *copy, const RootFixture *base) {
    uint64_t mask = 0;
    for (unsigned i = 0; i < sizeof(root_pointers) / 8; i++) {
        uint64_t value; unsigned char *field = (unsigned char *)copy + root_pointers[i];
        memcpy(&value, field, 8);
        if (value >= (uintptr_t)base && value - (uintptr_t)base < sizeof(*base)) {
            value -= (uintptr_t)base; mask |= UINT64_C(1) << i; memcpy(field, &value, 8);
        }
    }
    return mask;
}
static void root_copy(RootFixture *dst, const RootFixture *src) {
    memcpy(dst, src, sizeof(*dst)); uint64_t mask = root_normalize(dst, src);
    for (unsigned i = 0; i < sizeof(root_pointers) / 8; i++) if (mask & (UINT64_C(1) << i)) {
        uint64_t value; unsigned char *field = (unsigned char *)dst + root_pointers[i];
        memcpy(&value, field, 8); value += (uintptr_t)dst; memcpy(field, &value, 8);
    }
}
static void root_compare(RootFixture *a, RootFixture *c, unsigned kind, uint64_t op,
        RootStep step, RootScanner scan, RootDrainer drain, uint64_t expected) {
    RootFixture before_a, before_c, x, y;
    memcpy(&before_a, a, sizeof(*a)); memcpy(&before_c, c, sizeof(*c));
    uint64_t result = kind == 0 ? step(&a->ctx, &a->request, op) : kind == 1 ? scan(&a->scan) : drain(&a->drain);
    uint64_t reference = kind == 0 ? gc_root_step(&c->ctx, &c->request, op) :
                         kind == 1 ? gc_root_scan(&c->scan) : gc_root_drain(&c->drain);
    if (result != expected || reference != expected) {
        fprintf(stderr, "root kind %u op %llu case %u got %llu C %llu expected %llu\n", kind,
            (unsigned long long)op, root_cases[kind], (unsigned long long)result,
            (unsigned long long)reference, (unsigned long long)expected); abort();
    }
    if ((kind == 0 && result) || result == GC_ROOT_INVALID) {
        assert(memcmp(a, &before_a, sizeof(*a)) == 0 && memcmp(c, &before_c, sizeof(*c)) == 0);
    }
    uint64_t delta = kind == 0 && !result && (op == GC_ROOT_STORE || op == GC_ROOT_RELEASE);
    assert(atomic_load(&a->ctx.root_revision) == atomic_load(&before_a.ctx.root_revision) + delta);
    assert(atomic_load(&c->ctx.root_revision) == atomic_load(&before_c.ctx.root_revision) + delta);
    memcpy(&x, a, sizeof(x)); memcpy(&y, c, sizeof(y));
    assert(root_normalize(&x, a) == root_normalize(&y, c));
    assert(memcmp(&x, &y, sizeof(x)) == 0);
    if (root_vectors[kind]) {
        memcpy(&x, &before_c, sizeof(x)); memcpy(&y, c, sizeof(y));
        uint64_t record[] = {op, reference, root_normalize(&x, c), root_normalize(&y, c)};
        assert(fwrite(record, sizeof(record), 1, root_vectors[kind]) == 1);
        assert(fwrite(&x, sizeof(x), 1, root_vectors[kind]) == 1);
        assert(fwrite(&y, sizeof(y), 1, root_vectors[kind]) == 1);
    }
    root_cases[kind]++;
}
static inline void gc_run_root_cases(RootStep step, RootScanner scan, RootDrainer drain) {
    RootFixture *a = malloc(sizeof(*a)), *c = malloc(sizeof(*c)); assert(a && c);
    const char *names[] = {"KLASSIC_GC_ROOT_VECTORS", "KLASSIC_GC_ROOT_SCAN_VECTORS", "KLASSIC_GC_ROOT_DRAIN_VECTORS"};
    const char *magic[] = {"KLROOT01", "KLRSCN01", "KLRDRN01"};
    for (unsigned k = 0; k < 3; k++) {
        root_cases[k] = 0; const char *path = getenv(names[k]);
        if (path) {
            root_vectors[k] = fopen(path, "wb"); assert(root_vectors[k]);
            assert(fwrite(magic[k], 8, 1, root_vectors[k]) == 1);
            uint64_t header[] = {sizeof(*a), k == 0 ? offsetof(RootFixture, ctx) :
                k == 1 ? offsetof(RootFixture, scan) : offsetof(RootFixture, drain),
                offsetof(RootFixture, request), sizeof(root_pointers) / 8};
            assert(fwrite(header, sizeof(header), 1, root_vectors[k]) == 1);
            assert(fwrite(root_pointers, sizeof(root_pointers), 1, root_vectors[k]) == 1);
        }
    }
#define ROOT_CASE(kind, op, result) root_compare(a, c, kind, op, step, scan, drain, result)
    root_fixture(a); root_copy(c, a);
    for (unsigned i = 0; i < 300; i++) {
        ROOT_CASE(0, GC_ROOT_RESERVE, GC_ROOT_OK);
        assert(a->request.handle.cell == (uintptr_t)&a->cells[i] && a->request.handle.reuse_generation == 1);
        assert(a->request.handle.scope == 101 && atomic_load(&a->segment.used) == i + 1);
        a->request.value = c->request.value = ((uint64_t)(i + 1) << 20) | 0x1510;
        ROOT_CASE(0, GC_ROOT_STORE, GC_ROOT_OK);
        ROOT_CASE(0, GC_ROOT_READ_RAW, GC_ROOT_OK);
        assert(a->request.value == (((uint64_t)(i + 1) << 20) | 0x1510));
        ROOT_CASE(2, 0, GC_ROOT_OK);
        assert(a->drain.written == 1 && a->changes[0].previous == 0);
        assert(a->changes[0].next == a->request.value && a->changes[0].scope == 101);
        assert(a->changes[0].phase == atomic_load(&a->ctx.adopted_phase));
    }
    ROOT_CASE(0, GC_ROOT_RESERVE, GC_ROOT_FULL);
    ROOT_CASE(0, GC_ROOT_RETIRE, GC_ROOT_OK);
    assert(atomic_load(&a->segment.used) == 300 && atomic_load(&a->cells[299].value) == a->request.value);
    for (unsigned op = 0; op <= 4; op++) ROOT_CASE(0, op, GC_ROOT_INVALID);
    ROOT_CASE(1, 0, 2); assert(a->scan.written == 256 && a->scan.cursor == 256);
    for (unsigned i = 0; i < 256; i++) assert(a->values[i] == (((uint64_t)(i + 1) << 20) | 0x1510));
    assert(a->values[256] == UINT64_C(0xabababababababab));
    ROOT_CASE(1, 0, 0); assert(a->scan.written == 44 && a->scan.cursor == 300);
    for (unsigned i = 0; i < 44; i++) assert(a->values[i] == (((uint64_t)(257 + i) << 20) | 0x1510));
    ROOT_CASE(1, 0, 0); assert(!a->scan.written);

    root_fixture(a); root_copy(c, a); ROOT_CASE(0, GC_ROOT_RESERVE, 0);
    for (unsigned i = 0; i < 7; i++) {
        a->request.value = c->request.value = i % 3 == 0 ? 0 : UINT64_C(0x101510);
        ROOT_CASE(0, GC_ROOT_STORE, 0);
    }
    ROOT_CASE(0, GC_ROOT_STORE, GC_ROOT_FLUSH);
    ROOT_CASE(0, GC_ROOT_RELEASE, GC_ROOT_FLUSH);
    a->drain.capacity = c->drain.capacity = 3;
    ROOT_CASE(2, 0, 2); assert(a->drain.written == 3);
    assert(a->changes[0].previous == 0 && a->changes[0].next == 0);
    assert(a->changes[1].previous == 0 && a->changes[1].next == UINT64_C(0x101510));
    assert(a->changes[2].previous == UINT64_C(0x101510) && a->changes[2].next == UINT64_C(0x101510));
    ROOT_CASE(0, GC_ROOT_RELEASE, 0);
    assert(!atomic_load(&a->cells[0].value) && !a->cells[0].active);
    for (unsigned op = 1; op <= 3; op++) ROOT_CASE(0, op, GC_ROOT_INVALID);
    ROOT_CASE(2, 0, 2); ROOT_CASE(2, 0, 0); ROOT_CASE(2, 0, 0);
    assert(!a->drain.written);

    for (unsigned fault = 0; fault < 42; fault++) {
        root_fixture(a); assert(gc_root_step(&a->ctx, &a->request, 0) == 0);
        uint64_t op = GC_ROOT_STORE, result = GC_ROOT_INVALID;
        switch (fault) {
        case 0: op = 6; break;
        case 1: a->ctx.abi_version++; break;
        case 2: a->ctx.thread_id = 0; break;
        case 3: atomic_store(&a->ctx.state, GC_BLOCKED_NATIVE); break;
        case 4: atomic_store(&a->ctx.access_epoch, 1); break;
        case 5: a->request.segment = 0; break;
        case 6: a->request.segment++; break;
        case 7: a->segment.owner++; break;
        case 8: a->segment.scope = 0; break;
        case 9: a->segment.cells = 0; break;
        case 10: a->segment.cells++; break;
        case 11: a->segment.capacity = 0; break;
        case 12: a->segment.capacity = UINT64_C(0x100000000); break;
        case 13: a->segment.cells = UINT64_MAX - 7; break;
        case 14: atomic_store(&a->segment.state, 2); break;
        case 15: atomic_store(&a->segment.used, 301); break;
        case 16: a->request.handle.scope++; break;
        case 17: a->request.handle.cell = a->segment.cells - 8; break;
        case 18: a->request.handle.cell += sizeof(RootCell); break;
        case 19: a->request.handle.cell++; break;
        case 20: a->cells[0].active = 0; break;
        case 21: a->request.handle.reuse_generation = 0; break;
        case 22: a->request.handle.reuse_generation++; break;
        case 23: a->segment.journal = 0; break;
        case 24: a->segment.journal++; break;
        case 25: a->buffer.entries = 0; break;
        case 26: a->buffer.entries++; break;
        case 27: a->buffer.capacity = 0; break;
        case 28: a->buffer.capacity = UINT64_C(0x100000000); break;
        case 29: a->buffer.entries = UINT64_MAX - 7; break;
        case 30: a->buffer.top = 1; break;
        case 31: atomic_store(&a->buffer.consumed, 1); break;
        case 32: a->buffer.top = 8; atomic_store(&a->buffer.published, 8); break;
        case 33: a->buffer.top = UINT64_MAX; atomic_store(&a->buffer.published, UINT64_MAX);
            atomic_store(&a->buffer.consumed, UINT64_MAX); result = GC_ROOT_EXHAUSTED; break;
        case 34: op = 0; a->cells[1].active = 1; break;
        case 35: op = 0; atomic_store(&a->cells[1].value, 16); break;
        case 36: op = 0; a->cells[1].reuse_generation = UINT64_MAX; result = GC_ROOT_EXHAUSTED; break;
        case 37: atomic_store(&a->ctx.state, GC_AT_SAFEPOINT); break;
        case 38: atomic_store(&a->ctx.state, GC_DETACHING); break;
        case 39: atomic_store(&a->ctx.state, GC_DETACHED); break;
        case 40: a->segment.journal = 0; op = GC_ROOT_READ_RAW; result = GC_ROOT_OK; break;
        case 41: a->segment.journal = 0; op = GC_ROOT_RETIRE; result = GC_ROOT_OK; break;
        }
        root_copy(c, a); ROOT_CASE(0, op, result);
    }
    // A revision never wraps. Empty reservations and reads still work at MAX;
    // both ordinary mutation paths preserve the entire fixture on exhaustion.
    root_fixture(a); root_copy(c,a); ROOT_CASE(0,GC_ROOT_RESERVE,GC_ROOT_OK);
    atomic_store(&a->ctx.root_revision,UINT64_MAX-1);atomic_store(&c->ctx.root_revision,UINT64_MAX-1);
    ROOT_CASE(0,GC_ROOT_STORE,GC_ROOT_OK);
    ROOT_CASE(0,GC_ROOT_STORE,GC_ROOT_EXHAUSTED);
    ROOT_CASE(0,GC_ROOT_RELEASE,GC_ROOT_EXHAUSTED);
    ROOT_CASE(0,GC_ROOT_READ_RAW,GC_ROOT_OK);
    ROOT_CASE(0,GC_ROOT_RESERVE,GC_ROOT_OK);
    // Independent consumer expectations across physical ring wrap, including
    // high unsigned counters. Consumer must never read the private producer top.
    const uint64_t starts[] = {0, 1, UINT64_C(0x8000000000000000), UINT64_MAX - 7};
    const uint64_t budgets[] = {1, 3, 256};
    for (unsigned cap = 1; cap <= 7; cap++) for (unsigned st = 0; st < 4; st++)
    for (unsigned n = 0; n <= cap; n++) for (unsigned budget = 0; budget < 3; budget++) {
        root_fixture(a); a->buffer.capacity = cap; a->buffer.top = UINT64_MAX;
        uint64_t start = starts[st];
        atomic_store(&a->buffer.consumed, start); atomic_store(&a->buffer.published, start + n);
        a->drain.capacity = budgets[budget];
        for (unsigned i = 0; i < n; i++) a->entries[(start + i) % cap] = (RootChange){i, i + 1, 101, 0x12340122};
        root_copy(c, a); uint64_t count = n < budgets[budget] ? n : budgets[budget];
        ROOT_CASE(2, 0, count == n ? 0 : 2);
        assert(a->drain.written == count && atomic_load(&a->buffer.consumed) == start + count);
        for (unsigned i = 0; i < count; i++) assert(a->changes[i].previous == i && a->changes[i].next == i + 1);
        assert(a->changes[count].previous == UINT64_C(0xcdcdcdcdcdcdcdcd));
    }
    for (unsigned kind = 1; kind <= 2; kind++) for (unsigned fault = 0; fault < 13; fault++) {
        root_fixture(a);
        if (kind == 1) {
            switch (fault) {
            case 0: a->scan.segment = 0; break;
            case 1: a->scan.segment++; break;
            case 2: a->scan.output = 0; break;
            case 3: a->scan.output++; break;
            case 4: a->scan.capacity = 0; break;
            case 5: a->scan.capacity = 257; break;
            case 6: a->scan.output = UINT64_MAX - 7; break;
            case 7: a->segment.cells++; break;
            case 8: a->segment.scope = 0; break;
            case 9: atomic_store(&a->segment.state, 3); break;
            case 10: atomic_store(&a->segment.used, 301); break;
            case 11: a->scan.cursor = 1; break;
            case 12: a->segment.cells = UINT64_MAX - 7; break;
            }
        } else {
            switch (fault) {
            case 0: a->drain.buffer = 0; break;
            case 1: a->drain.buffer++; break;
            case 2: a->drain.output = 0; break;
            case 3: a->drain.output++; break;
            case 4: a->drain.capacity = 0; break;
            case 5: a->drain.capacity = 257; break;
            case 6: a->drain.output = UINT64_MAX - 7; break;
            case 7: a->buffer.entries = 0; break;
            case 8: a->buffer.entries++; break;
            case 9: a->buffer.capacity = 0; break;
            case 10: atomic_store(&a->buffer.consumed, 1); break;
            case 11: atomic_store(&a->buffer.published, 8); break;
            case 12: a->buffer.entries = UINT64_MAX - 7; break;
            }
        }
        root_copy(c, a); ROOT_CASE(kind, 0, GC_ROOT_INVALID);
    }
    for (unsigned k = 0; k < 3; k++) if (root_vectors[k]) { assert(fclose(root_vectors[k]) == 0); root_vectors[k] = NULL; }
    printf("root storage %u / scan %u / journal drain %u cases PASS\n", root_cases[0], root_cases[1], root_cases[2]);
    free(a); free(c);
#undef ROOT_CASE
}
#endif
