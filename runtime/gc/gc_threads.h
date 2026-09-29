/* Shared thread-lifecycle entry points. The C collector implements these;
 * direct emitters must use the same context/state contract when wired. */
#ifndef KLASSIC_GC_THREADS_H
#define KLASSIC_GC_THREADS_H
#include "gc_abi.h"

/* The returned nonmoving context belongs to the calling OS thread. It is
 * borrowed only until detach; it must not be retained across slot reuse.
 * thread_id never repeats. Attach is idempotent for a Running owner and may
 * wait for an in-flight handshake; it cannot preserve a raw heap address. */
MutatorContext *gc_thread_attach(void);
MutatorContext *gc_thread_current(void); /* NULL if unattached; never parks */
void gc_thread_detach(MutatorContext *ctx);

/* Running -> BlockedNative -> Running. No raw managed address, access guard,
 * or unrooted managed value may cross either transition. Native code must not
 * inspect/change roots, TLABs, buffers or the managed heap while blocked.
 * Native buffers need copy-out or a scoped pin (pin API is not yet implemented).
 * Leave waits for any in-flight handshake before managed execution resumes.
 * For callbacks: leave before callback, enter after callback; nesting is not
 * implicit. The owning native stack/root storage must remain alive throughout.
 *
 * The current C collector still uses legacy shadow roots, scanned only while
 * their owner is quiescent. Generational root segments/buffers and phase-color
 * adoption are not activated here: adopted_phase stays at the initial snapshot.
 * Misuse fails with a diagnostic, rather than silently reattaching/continuing. */
void gc_enter_native(MutatorContext *ctx);
void gc_leave_native(MutatorContext *ctx);
#endif
