#ifndef MEM_CTL_H
#define MEM_CTL_H

#include <stdlib.h>

#define NEW(type)               (type *) malloc(sizeof(type))
#define NEW_ARRAY(type,size)    (type *)calloc(size, sizeof(type))
#define FREE(ptr)               

#ifdef __cplusplus
extern "C" {
#endif

void noop_free(void* v);

#ifdef __cplusplus
}
#endif

#endif
