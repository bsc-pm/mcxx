/*--------------------------------------------------------------------
  (C) Copyright 2006-2011 Barcelona Supercomputing Center 
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



#ifndef CXX_TLTYPES_H
#define CXX_TLTYPES_H

#include "libmcxx-common.h"
#include "cxx-macros.h"
#include "cxx-ast.h"
#include "cxx-scope-decls.h"
#include "cxx-typeutils.h"

MCXX_BEGIN_DECLS

union tl_type_data_tag;

typedef 
struct tl_type_array_tag
{
    int _elements;
    union tl_type_data_tag* _array;
} tl_type_array_t;

typedef 
union tl_type_data_tag
{
    int _integer;
    char _boolean;
    AST _ast;
    tl_type_array_t _array;
    const char* _string;
    struct scope_entry_tag* _entry;
    struct type_tag* _type;
    void *_data;
} tl_type_data_t;

typedef 
enum tl_type_kind_tag
{
    TL_UNDEFINED = 0,
    TL_INTEGER, // int
    TL_BOOL, // char
    TL_AST, // AST
    TL_STRING, // char*
    TL_SYMBOL, // struct scope_entry_tag*
    TL_TYPE,  // struct type_tag*
    TL_OTHER, // void* to arbitrary data
} tl_type_kind_t;

typedef 
struct tl_type_tag
{
    tl_type_kind_t kind;
    tl_type_data_t data;
} tl_type_t;

LIBMCXX_EXTERN tl_type_t tl_bool(char c);
LIBMCXX_EXTERN tl_type_t tl_integer(int i);
LIBMCXX_EXTERN tl_type_t tl_ast(AST a);
LIBMCXX_EXTERN tl_type_t tl_string(const char* str);
LIBMCXX_EXTERN tl_type_t tl_symbol(struct scope_entry_tag* entry);
LIBMCXX_EXTERN tl_type_t tl_type(struct type_tag* t);
LIBMCXX_EXTERN tl_type_t tl_object(void *data);

MCXX_END_DECLS

#endif // CXX_TLTYPES_H
