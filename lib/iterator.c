#include "iterator.h"
#include "mem_ctl.h"

void iterator_init(Iterator * i, IteratorOps * ops)
{
    i->ops = ops;
}

void iterator_end(Iterator * i)
{
    if (!i)
        return;

    if (i->ops->end)
        i->ops->end(i);
}

void iterator_destroy(Iterator * i)
{
    if (!i)
        return;

    iterator_end(i);
    if (i->ops->free)
        i->ops->free(i);
    else
        FREE(i);
}

void iterator_first(Iterator * i)
{
    if (!i || !i->ops->first)
        return;

    i->ops->first(i);
}

bool_type iterator_finished(Iterator * i)
{
    if (!i || !i->ops->finished)
        return false;

    return i->ops->finished(i);
}

void iterator_next(Iterator * i)
{
    if (!i || !i->ops->next)
        return;

    i->ops->next(i);
}

void *iterator_item(Iterator * i)
{
    if (!i || !i->ops->item)
        return NULL;

    return i->ops->item(i);
}

void iterator_remove(Iterator * i)
{
    if (!i || !i->ops->remove)
        return;

    i->ops->remove(i);
}

#if 1                           /* JAIRO */
int iterator_items(Iterator * i)
{
    if (!i || !i->ops->items)
        return -1;

    return (i->ops->items(i));
}
#endif
