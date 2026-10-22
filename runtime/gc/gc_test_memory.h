/* Matching allocation/free pairs for aligned conformance-fixture payloads. */
#ifndef KLASSIC_GC_TEST_MEMORY_H
#define KLASSIC_GC_TEST_MEMORY_H
#include <stdlib.h>
#ifdef _WIN32
#include <malloc.h>
static inline void *gc_test_aligned_alloc(size_t alignment, size_t bytes) {
    return _aligned_malloc(bytes, alignment);
}
static inline void gc_test_aligned_free(void *memory) { _aligned_free(memory); }
#else
static inline void *gc_test_aligned_alloc(size_t alignment, size_t bytes) {
    return aligned_alloc(alignment, bytes);
}
static inline void gc_test_aligned_free(void *memory) { free(memory); }
#endif
#endif
