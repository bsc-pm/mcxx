/***************************************************************************
 
  PhantasienMud Development Team 2001 (c)

  list.c - Double linked list structure

  ChangeLog:

  Sat Jan 20 2001 - Created by Ingwe

***************************************************************************/

#include "list.h"

void list_init(List * l)
{
    if (l)
    {
        l->no_items = 0;
        l->first = NULL;
        l->last = NULL;
        l->free_items = false;
        l->df_items = NULL;
    }
}

List *list_create()
{
    List *new_list;

    new_list = NEW(List);
    list_init(new_list);
    return new_list;
}

void list_set(List * l, int op, void *value)
{
    switch (op)
    {
        case LIST_FREE_ITEMS:
            l->free_items = (bool_type) value;
            break;
        case LIST_ITEMS_DESTRUCTOR:
            l->df_items = value;
            break;
    }
}

void list_dump(List * l)
{
    ListNode *node, *node_next;

    if (!l)
        return;

    for (node = l->first; node; node = node_next)
    {
        node_next = node->next;
        if (l->free_items)
        {
            if (l->df_items)
                (*l->df_items) (node->data);
            FREE(node->data);
        }
        FREE(node);
    }
    l->first = l->last = NULL;
    l->no_items = 0;
}

void list_destroy(List * l)
{
    if (l)
    {
        list_dump(l);
        FREE(l);
    }
}

void node_destroy(List * l, ListNode * node)
{
    if (l->first == node)
        l->first = node->next;
    else
        node->prev->next = node->next;
    if (l->last == node)
        l->last = node->prev;
    else
        node->next->prev = node->prev;

    if (l->free_items)
    {
        if (l->df_items)
            (*l->df_items) (node->data);
        FREE(node->data);
    }
    FREE(node);
}

ListNode *list_first(List * l)
{
    return l->first;
}

ListNode *list_last(List * l)
{
    return l->last;
}

int list_num_items(List * l)
{
    return l->no_items;
}

ListNode *node_create(void *item)
{
    ListNode *node;

    node = NEW(ListNode);

    if (node)
    {
        node->data = item;
        node->no_references = 0;
        node->is_deleted = false;
        node->next = NULL;
        node->prev = NULL;
    }

    return node;
}

void list_append(List * l, void *item)
{
    ListNode *node;

    if (!l)
        return;

    node = node_create(item);

    if (l->last)
        l->last->next = node;
    if (!l->first)
        l->first = node;

    node->prev = l->last;
    l->last = node;
    l->no_items++;
}

void list_prepend(List * l, void *item)
{
    ListNode *node;

    if (!l)
        return;

    node = node_create(item);

    if (l->first)
        l->first->prev = node;
    if (!l->last)
        l->last = node;

    node->next = l->first;
    l->first = node;
    l->no_items++;
}

void list_insert_pre(List * l, ListNode * node_post, void *item)
{
    ListNode *node;

    if (!l)
        return;

    if (!node_post)
    {
        list_append(l, item);
        return;
    }

    if (!node_post->prev)
    {
        list_prepend(l, item);
        return;
    }

    node = node_create(item);
    if (!node)
        return;

    node->prev = node_post->prev;
    node->next = node_post;

    node_post->prev->next = node;
    node_post->prev = node;

    l->no_items++;
}


void node_lock(ListNode * node)
{
    node->no_references++;
}

void node_unlock(List * l, ListNode * node)
{
    node->no_references--;

    if (!node->no_references && node->is_deleted)
        node_destroy(l, node);
}

ListNode *node_next(List * l, ListNode * node, bool_type unlock)
{
    ListNode *aux;

    if (!l || !node)
        return NULL;

    for (aux = node->next; aux && aux->is_deleted; aux = aux->next);

    if (unlock)
    {
        node_unlock(l, node);
        if (aux)
            node_lock(aux);
    }
    return aux;
}

ListNode *node_prev(List * l, ListNode * node, bool_type unlock)
{
    ListNode *aux;

    if (!l || !node)
        return NULL;

    for (aux = node->prev; aux && aux->is_deleted; aux = aux->prev);

    if (unlock)
    {
        node_unlock(l, node);
        if (aux)
            node_lock(aux);
    }
    return aux;
}

void list_delete(List * l, ListNode * node)
{
    if (!l || !node)
        return;

    if (node->no_references)
        node->is_deleted = true;
    else
        node_destroy(l, node);

    l->no_items--;
}


void list_delete_item(List * l, void *item)
{
    ListNode *node;

    if (!l)
        return;

    for (node = l->first; node; node = node_next(l, node, false))
        if (node->data == item)
        {
            node_destroy(l, node);
            l->no_items--;
            return;
        }
}

void *list_get(List * l, int pos)
{
    ListNode *node;

    if (!l)
        return NULL;
    if (pos > l->no_items)
        return NULL;

    node = l->first;
    for (pos--; pos > 0; pos--)
    {
        node = node_next(l, node, false);
    }

    return node->data;

}
