#ifndef ITERATOR_H
#define ITERATOR_H

#include "s_types.h"

typedef struct _iterator Iterator;
typedef struct _iterator_ops IteratorOps;

#define ITERATOR(it) ((Iterator *)it)

/**
 *  @internal
 */

struct _iterator_ops
{
    void (*first) (Iterator *);
      bool_type(*finished) (Iterator *);
    void *(*item) (Iterator *);
    void (*next) (Iterator *);
    void (*remove) (Iterator *);
    void (*end) (Iterator *);
    void (*free) (Iterator *);
#if 1                           /* JAIRO */
    int (*items) (Iterator *);
#endif
};

struct _iterator
{
    IteratorOps *ops;
};

void iterator_init(Iterator * i, IteratorOps * ops);
void iterator_end(Iterator * i);
void iterator_destroy(Iterator * i);
void iterator_first(Iterator * i);
bool_type iterator_finished(Iterator * i);
void iterator_next(Iterator * i);
void *iterator_item(Iterator * i);
void iterator_remove(Iterator * i);

#if 1                           /* JAIRO */
int iterator_items(Iterator * i);
#endif

#endif
