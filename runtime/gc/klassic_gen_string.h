/* Immutable UTF-8 strings in canonical RAW_BYTES objects. Only stable rooted
 * handles cross these calls; no managed payload pointer is returned. */
#ifndef KLASSIC_GEN_STRING_H
#define KLASSIC_GEN_STRING_H
#include "klassic_gen_runtime.h"
void kg_string_from_utf8(const unsigned char *,uint64_t,RootCell *);
void kg_string_concat(const RootCell *,const RootCell *,RootCell *);
uint64_t kg_string_length(const RootCell *);
int64_t kg_string_compare(const RootCell *,const RootCell *);
void kg_string_println(const RootCell *);
#endif
