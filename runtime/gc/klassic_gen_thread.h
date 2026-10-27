/* Managed C callbacks and runtime-owned OS mutator launches. */
#ifndef KLASSIC_GEN_THREAD_H
#define KLASSIC_GEN_THREAD_H
#include "klassic_gen_runtime.h"
#ifdef __cplusplus
extern "C" {
#endif
typedef struct KgMutatorThread KgMutatorThread;
typedef uint64_t (*KgManagedCallback)(const RootCell *argument,void *userdata);
/* The callback borrows an independent owned argument root until normal return.
 * NULL argument means an empty root. Running and Native callers are accepted;
 * the caller's boundary state is restored, including nested Native intervals.
 * The callback must not release its borrowed root, detach, or shut down.
 * Nonlocal exits and asynchronous cancellation are unsupported. */
uint64_t kg_managed_callback(KgManagedCallback callback,const RootCell *argument,void *userdata);
/* Return only after a real child attach/import acknowledgement, preserving the
 * argument even if its original root is cleared immediately afterwards. NULL
 * means shutdown closed admission. userdata is native storage retained by the
 * caller through join. Results are native scalars, never unrooted GcRefs. */
KgMutatorThread *kg_thread_start(KgManagedCallback callback,const RootCell *argument,void *userdata);
/* Join once from an attached owner; Running/Native state is preserved. Opaque
 * handles contain monotonic identities and cannot alias reused control maps.
 * Shutdown also kernel-joins completed launches before runtime disposal. */
uint64_t kg_thread_join(KgMutatorThread *thread);
#ifdef __cplusplus
}
#endif
#endif
