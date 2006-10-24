#ifndef MEM_CTL_H
#define MEM_CTL_H

#include <stdlib.h>
#include <gc.h>

#define NEW(type)               (type *) GC_MALLOC(sizeof(type))
#define NEW_ARRAY(type,size)    (type *)GC_CALLOC(size, sizeof(type))
#define FREE(ptr)               

#ifdef __cplusplus
extern "C" {
#endif

void noop_free(void* v);

#ifdef __cplusplus
}
#endif

#endif
