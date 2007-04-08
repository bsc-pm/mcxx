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

typedef void (*_mcxx_children_fun)(void *, void (*)(void*));

#define MCXX_REFCOUNT_OBJECT \
    int _mcxx_refcount; \
    int _mcxx_buffered; \
    _mcxx_ref_colour_t _mcxx_colour; \
    _mcxx_children_fun _mcxx_children

typedef
struct _mcxx_base_refcount_tag
{
    MCXX_REFCOUNT_OBJECT;
} *_p_mcxx_base_refcount_t, _mcxx_base_refcount_t;

void *_mcxx_calloc(size_t nmemb, size_t size);

void _mcxx_increment(void *p);
void _mcxx_decrement(void *p);

void _mcxx_collectcycles(void);
_mcxx_children_fun *_mcxx_children(void *p);

#define MCXX_NEW(_type) (_type*)(_mcxx_calloc(1, sizeof(_type)))

#define MCXX_CHILDREN(x) (*_mcxx_children(x))

#define MCXX_INCREF(x) _mcxx_increment(x)

#define MCXX_DECREF(x) _mcxx_decrement(x)

// Sets 'x' to point 'y'
#define MCXX_UPDATE_TO(x, y) \
    do { \
        if ((x) != NULL) \
        { \
            MCXX_DECREF(x); \
        } \
        (x) = (y); \
        if ((y) != NULL) \
        { \
            MCXX_INCREF(y); \
        } \
    } while (0)

#endif // MCXX_REFCOUNT_H
