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
#include <string.h>
#include "cxx-tltype.h"

tl_type_t tl_bool(char c)
{
    tl_type_t result;

    result.kind = TL_BOOL;
    result.data._boolean = c;

    return result;
}

tl_type_t tl_integer(int i)
{
    tl_type_t result;

    result.kind = TL_INTEGER;
    result.data._integer = i;

    return result;
}

tl_type_t tl_ast(AST a)
{
    tl_type_t result;

    result.kind = TL_AST;
    result.data._ast = a;

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
    result.data._string = strdup(str);

    return result;
}

tl_type_t tl_symbol(scope_entry_t* entry)
{
    tl_type_t result;
    
    result.kind = TL_SYMBOL;
    result.data._entry = entry;

    return result;
}
