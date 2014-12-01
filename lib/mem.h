#ifndef MEM_H
#define MEM_H

#include <string.h>
#include <stdlib.h>

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#if defined(__GNUC__) && __GNUC__ >= 4
  #if defined(__GNUC_MINOR__) && __GNUC_MINOR__ >= 4
    #define MEM_WARN_UNUSED __attribute__((warn_unused_result))
    #define MEM_MALLOC_RETURN __attribute__((malloc))
    #else
    #define MEM_WARN_UNUSED
    #define MEM_MALLOC_RETURN
  #endif
#endif


#ifdef __cplusplus
extern "C" {
#endif

void *xmalloc(size_t size) MEM_WARN_UNUSED MEM_MALLOC_RETURN;
void xfree(void *ptr);
void *xcalloc(size_t nmemb, size_t size) MEM_WARN_UNUSED MEM_MALLOC_RETURN;
void *xrealloc(void *ptr, size_t size) MEM_WARN_UNUSED; // realloc does not have malloc property
char *xstrdup(const char *s) MEM_WARN_UNUSED MEM_MALLOC_RETURN;

#ifdef __cplusplus
}
#endif

// In C++11 there are inline functions that use malloc
#if !defined(HAVE_CXX11)
// Some systems redefine these as macros
#undef malloc
#undef calloc
#undef free
#undef realloc
#undef strdup

#define malloc (+use_xmalloc_instead)
#define calloc (+use_xcalloc_instead)
#define free   (+use_xfree_instead)
#define realloc (+use_xrealloc_instead)
#define strdup (+use_xstrdup_instead)
#endif

#endif // MEM_H
