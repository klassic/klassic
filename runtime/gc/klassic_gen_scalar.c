#include "klassic_gen_scalar.h"
#include "klassic_gen_runtime.h"
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
/* Language errors are diagnosed before any undefined signed operation. The
 * generated program is initialized, guard-free and retains all managed inputs
 * in roots. Publish its actual Native boundary before diagnostic I/O. This is
 * fail-stop language termination, not a successful GC disposal receipt. */
static _Noreturn void kg_numeric_error(const char *message,const char *prefix,uint64_t length){
    kg_native_enter();
    if(prefix&&length)fwrite(prefix,1,(size_t)length,stderr);else fputs("klassic: ",stderr);
    fprintf(stderr,"%s\n",message);fflush(stderr);_Exit(EXIT_FAILURE);
}
int64_t kg_i64_add_at(int64_t a,int64_t b,const char *prefix,uint64_t length){
    if((b>0&&a>INT64_MAX-b)||(b<0&&a<INT64_MIN-b))kg_numeric_error("integer overflow",prefix,length);
    return a+b;
}
int64_t kg_i64_sub_at(int64_t a,int64_t b,const char *prefix,uint64_t length){
    if((b>0&&a<INT64_MIN+b)||(b<0&&a>INT64_MAX+b))kg_numeric_error("integer overflow",prefix,length);
    return a-b;
}
int64_t kg_i64_mul_at(int64_t a,int64_t b,const char *prefix,uint64_t length){
    if(!a||!b)return 0;
    if((a==-1&&b==INT64_MIN)||(b==-1&&a==INT64_MIN))kg_numeric_error("integer overflow",prefix,length);
    if(a>0){if((b>0&&a>INT64_MAX/b)||(b<0&&b<INT64_MIN/a))kg_numeric_error("integer overflow",prefix,length);}
    else if((b>0&&a<INT64_MIN/b)||(b<0&&a<INT64_MAX/b))kg_numeric_error("integer overflow",prefix,length);
    return a*b;
}
int64_t kg_i64_div_at(int64_t a,int64_t b,const char *prefix,uint64_t length){
    if(!b)kg_numeric_error("division by zero",prefix,length);
    if(a==INT64_MIN&&b==-1)kg_numeric_error("integer overflow",prefix,length);
    return a/b;
}
int64_t kg_i64_neg_at(int64_t a,const char *prefix,uint64_t length){
    if(a==INT64_MIN)kg_numeric_error("integer overflow in unary negation",prefix,length);return -a;
}

int64_t kg_i64_add(int64_t a,int64_t b){return kg_i64_add_at(a,b,NULL,0);}

int64_t kg_i64_sub(int64_t a,int64_t b){return kg_i64_sub_at(a,b,NULL,0);}

int64_t kg_i64_mul(int64_t a,int64_t b){return kg_i64_mul_at(a,b,NULL,0);}

int64_t kg_i64_div(int64_t a,int64_t b){return kg_i64_div_at(a,b,NULL,0);}
int64_t kg_i64_neg(int64_t a){return kg_i64_neg_at(a,NULL,0);}
