/*--------------------------------------------------------------------
  (C) Copyright 2006-2015 Barcelona Supercomputing Center
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


#include "cxx-cexpr-deep-copy.h"

#include "cxx-cexpr.h"
#include "cxx-nodecl-deep-copy.h"

const_value_t* const_value_deep_copy(const_value_t* source,
        symbol_map_t* symbol_map)
{
    if (source == NULL)
        return NULL;

    if (const_value_is_address(source))
    {
        return const_value_make_address(
                const_value_deep_copy(
                    const_value_address_dereference(source), symbol_map));
    }
    else if (const_value_is_object(source))
    {
        scope_entry_t* source_base = const_value_object_get_base(source);
        scope_entry_t* dest_base = symbol_map->map(symbol_map, source_base);

        int num_accessors = const_value_object_get_num_accessors(source);
        subobject_accessor_t accessors[ num_accessors + 1 ];

        // Currently the accessors do not have to be deep copied either
        const_value_object_get_all_accessors(source, accessors);

        return const_value_make_object(
                dest_base,
                num_accessors,
                accessors);
    }
    else
    {
        // Any other kind of constant does not have to be deep-copied
        // FIXME: an array may have pointers inside to other objects
        // these may have to be deep copied as well
        return source;
    }
}
