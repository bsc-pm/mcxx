#ifndef LIST_ITERATOR_H
#define LIST_ITERATOR_H

#include "s_types.h"
#include "list.h"
#include "iterator.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _list_iterator ListIterator;

struct _list_iterator
{
    Iterator iterator;
    ListNode *act;
    List *list;
};

void list_iterator_init(ListIterator * i, List * l);
ListIterator *list_iterator_create(List * l);

void list_riterator_init(ListIterator * i, List * l);
ListIterator *list_riterator_create(List * l);

#ifdef __cplusplus
}
#endif

#endif
