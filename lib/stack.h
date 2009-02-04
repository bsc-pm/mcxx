/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2008 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
    Barcelona Supercomputing Center - Centro Nacional de Supercomputacion
    Universitat Politecnica de Catalunya

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
#ifndef STACK_H
#define STACK_H

#include "libutils-common.h"
#include "list.h"

#ifdef __cplusplus
extern "C" {
#endif

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

#ifdef __cplusplus
}
#endif

#endif
