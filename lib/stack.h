#ifndef STACK_H
#define STACK_H

#include "list.h"

typedef List Stack;

#define stack_create() list_create()
#define stack_init(stack) list_init(stack)
#define stack_set(stack,op,value) list_set(stack,op,value);
#define stack_dump(stack) list_dump(stack);
#define stack_destroy(stack) list_destroy(stack);
#define stack_push(stack,item) list_append(stack,item)
#define stack_pop(stack) list_delete(stack,list_last(stack))
#define stack_top(stack) node_data(list_last(stack))
#define stack_empty(stack) (list_num_items(stack) == 0)

#endif
