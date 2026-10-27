/* Rooted managed results for callbacks and runtime-owned mutator launches. */
#ifndef KLASSIC_GEN_THREAD_RESULT_H
#define KLASSIC_GEN_THREAD_RESULT_H
#include "klassic_gen_thread.h"
#ifdef __cplusplus
extern "C" {
#endif
typedef void (*KgManagedResultCallback)(const RootCell *argument,RootCell *result,void *userdata);
/* Both callback roots are borrowed until normal return. Populate result with
 * rooted APIs; never release either reservation. out remains an owned root in
 * the caller, including across Native return and subsequent collections. */
void kg_managed_callback_into(KgManagedResultCallback callback,const RootCell *argument,RootCell *out,void *userdata);
KgMutatorThread *kg_thread_start_managed(KgManagedResultCallback callback,const RootCell *argument,void *userdata);
/* Actual child-owned result root stays registered until authenticated import
 * into out and publication ACK. The child then releases/detaches; the kernel
 * join and native-control retirement finish before return. */
void kg_thread_join_into(KgMutatorThread *thread,RootCell *out);
#ifdef __cplusplus
}
#endif
#endif
