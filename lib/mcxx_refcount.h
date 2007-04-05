#ifndef MCXX_REFCOUNT_H
#define MCXX_REFCOUNT_H

#include <stdlib.h>

typedef
enum _mcxx_ref_colour_tag
{
    _MCXX_BLACK, // In use or free
    _MCXX_GRAY, // Possible member of cycle
    _MCXX_WHITE, // Member of garbage cycle
    _MCXX_PURPLE, // Possible root of cycle
    // _MCXX_GREEN, // Acyclic (unused)
} _mcxx_ref_colour_t;

#define MCXX_REFCOUNT_OBJECT \
    int _mcxx_refcount; \
    int _mcxx_buffered; \
    _mcxx_ref_colour_t _mcxx_colour; \
    void (*_mcxx_children)(void*, void (*_mcxx_do)(void*))

typedef
struct _mcxx_base_refcount_tag
{
    MCXX_REFCOUNT_OBJECT;
} *_p_mcxx_base_refcount_t, _mcxx_base_refcount_t;

void *_mcxx_calloc(size_t nmemb, size_t size);

void _mcxx_increment(void *p);
void _mcxx_decrement(void *p);

void _mcxx_collectcycles(void);

#define MCXX_NEW(_type) (_type*)(_mcxx_calloc(1, sizeof(_type)))

#define MCXX_CHILDREN(x) ((x)->_mcxx_children)

#define MCXX_INCREF(x) _mcxx_increment(x)

#define MCXX_DECREF(x) _mcxx_decrement(x)


#endif // MCXX_REFCOUNT_H
