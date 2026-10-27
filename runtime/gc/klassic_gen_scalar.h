/* Checked signed arithmetic used by canonical emitter output. */
#ifndef KLASSIC_GEN_SCALAR_H
#define KLASSIC_GEN_SCALAR_H
#include <stdint.h>
int64_t kg_i64_add(int64_t,int64_t);
int64_t kg_i64_sub(int64_t,int64_t);
int64_t kg_i64_mul(int64_t,int64_t);
int64_t kg_i64_div(int64_t,int64_t);
int64_t kg_i64_neg(int64_t);
/* Prefix is immutable native diagnostic bytes, not a managed String. */
int64_t kg_i64_add_at(int64_t,int64_t,const char *,uint64_t);
int64_t kg_i64_sub_at(int64_t,int64_t,const char *,uint64_t);
int64_t kg_i64_mul_at(int64_t,int64_t,const char *,uint64_t);
int64_t kg_i64_div_at(int64_t,int64_t,const char *,uint64_t);
int64_t kg_i64_neg_at(int64_t,const char *,uint64_t);
#endif
