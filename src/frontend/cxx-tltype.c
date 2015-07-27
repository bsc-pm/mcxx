/*--------------------------------------------------------------------
  (C) Copyright 2006-2014 Barcelona Supercomputing Center
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




#include <string.h>
#include "cxx-utils.h"
#include "cxx-tltype.h"

tl_type_t tl_bool(char c)
{
    tl_type_t result;

    result.kind = TL_BOOL;
    result.data._boolean = c;

    return result;
}

tl_type_t tl_unsigned_integer(unsigned int i)
{
    tl_type_t result;
    
    result.kind = TL_UNSIGNED_INTEGER;
    result.data._unsigned_integer = i;
    
    return result;
}

tl_type_t tl_integer(int i)
{
    tl_type_t result;

    result.kind = TL_INTEGER;
    result.data._integer = i;

    return result;
}

tl_type_t tl_type(type_t* t)
{
    tl_type_t result;
    
    result.kind = TL_TYPE;
    result.data._type = t;

    return result;
}

tl_type_t tl_string(const char* str)
{
    tl_type_t result;
    
    result.kind = TL_STRING;
    result.data._string = uniquestr(str);

    return result;
}

tl_type_t tl_symbol(scope_entry_t* entry)
{
    tl_type_t result;
    
    result.kind = TL_SYMBOL;
    result.data._entry = entry;

    return result;
}

tl_type_t tl_object(void *data)
{
    tl_type_t result;
    
    result.kind = TL_OTHER;
    result.data._data = data;

    return result;
}

tl_type_t tl_nodecl(nodecl_t n)
{
    tl_type_t result;
    result.kind = TL_NODECL;
    result.data._nodecl = n;

    return result;
}

tl_type_t tl_decl_context(const decl_context_t* decl_context)
{
    tl_type_t result;
    result.kind = TL_DECL_CONTEXT;
    result.data._decl_context = decl_context;

    return result;
}
