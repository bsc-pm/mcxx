/*--------------------------------------------------------------------
  (C) Copyright 2006-2013 Barcelona Supercomputing Center
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
  See AUTHORS file in the top level directory for information
  regarding developers and contributors.
  
  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 3 of the License, or (at your option) any later version.
  
  Mercurium C/C++ source-to-source compiler is distributed in the hope
  that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
  PURPOSE.  See the GNU Lesser General Public License for more
  details.
  
  You should have received a copy of the GNU Lesser General Public
  License along with Mercurium C/C++ source-to-source compiler; if
  not, write to the Free Software Foundation, Inc., 675 Mass Ave,
  Cambridge, MA 02139, USA.
--------------------------------------------------------------------*/



#ifndef RED_BLACK_TREE_H
#define RED_BLACK_TREE_H

#include "libutils-common.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct rb_red_blk_tree_tag rb_red_blk_tree;
typedef struct rb_red_blk_node_tag rb_red_blk_node;

LIBUTILS_EXTERN rb_red_blk_tree* rb_tree_create( int (*comp_func) (const void*,const void*),
			      void (*key_dtor_func) (const void*),
			      void (*info_dtor_func) (const void*));
// LIBUTILS_EXTERN rb_red_blk_node *rb_tree_add(rb_red_blk_tree*, const void* key, void* info);
LIBUTILS_EXTERN rb_red_blk_node *rb_tree_insert(rb_red_blk_tree*, const void* key, void* info);
LIBUTILS_EXTERN void rb_tree_walk(rb_red_blk_tree*, 
        void (*walk_func) (const void* key, void* value, void* data), 
        void *data);
LIBUTILS_EXTERN void rb_tree_delete(rb_red_blk_tree* , rb_red_blk_node* );
LIBUTILS_EXTERN void rb_tree_destroy(rb_red_blk_tree*);
LIBUTILS_EXTERN rb_red_blk_node* rb_tree_query(rb_red_blk_tree*, const void* key);

LIBUTILS_EXTERN const void* rb_node_get_key(rb_red_blk_node*);
LIBUTILS_EXTERN void* rb_node_get_info(rb_red_blk_node*);

#ifdef __cplusplus
}
#endif

#endif // RED_BLACK_TREE_H
