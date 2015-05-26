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




#include "tl-overload.hpp"
#include "cxx-overload.h"
#include "cxx-exprtype.h"
#include "cxx-entrylist.h"
#include "uniquestr.h"

namespace TL
{
    static void free_scope_entry_list(scope_entry_list_t* entry)
    {
        entry_list_free(entry);
    }

    Symbol Overload::solve(
            ObjectList<Symbol> candidate_functions,
            Type implicit_argument_type,
            ObjectList<Type> argument_types, 
            const std::string filename,
            int line,
            bool &valid, 
            ObjectList<Symbol>& viable_functions,
            ObjectList<Symbol>& argument_conversor)
    {
        valid = false;

        // Try hard to not to do useless work
        if (candidate_functions.empty())
        {
            return Symbol(NULL);
        }

        scope_entry_list_t* first_candidate_list = NULL;
        
        // Build the candidates list
        for (ObjectList<Symbol>::iterator it = candidate_functions.begin();
                it != candidate_functions.end();
                it++)
        {
            Symbol sym(*it);
            first_candidate_list = entry_list_add(first_candidate_list, sym.get_internal_symbol());
        }

        // Build the type array
        unsigned int i = argument_types.size();
        type_t** argument_types_array = new type_t*[argument_types.size() + 1];
        argument_types_array[0] = implicit_argument_type.get_internal_type();
        for (i = 0; i < argument_types.size(); i++)
        {
            argument_types_array[i+1] = argument_types[i].get_internal_type();
        }

        // Now we need a const decl_context_t* but we were not given any explicitly,
        // use the one of the first candidate
        const decl_context_t* decl_context = candidate_functions[0].get_scope().get_decl_context();

        // Unfold and mix!
        scope_entry_list_t* candidate_list = NULL;
        candidate_list = unfold_and_mix_candidate_functions(first_candidate_list,
                NULL /* builtins */,
                &argument_types_array[1], argument_types.size(),
                decl_context,
                uniquestr(filename.c_str()), line,
                NULL /* explicit template arguments */);

        {
            ObjectList<Symbol> list;
            Scope::convert_to_vector(candidate_list, list);
            viable_functions.append(list);
        }

        candidate_t* candidate_set = NULL;

        scope_entry_list_iterator_t* it = NULL;
        for (it = entry_list_iterator_begin(candidate_list);
                !entry_list_iterator_end(it);
                entry_list_iterator_next(it))
        {
            scope_entry_t* entry = entry_list_iterator_current(it);
            if (entry->entity_specs.is_member)
            {
                candidate_set = add_to_candidate_set(candidate_set,
                        entry,
                        argument_types.size() + 1,
                        argument_types_array);
            }
            else
            {
                candidate_set = add_to_candidate_set(candidate_set,
                        entry,
                        argument_types.size(),
                        argument_types_array + 1);
            }
        }
        entry_list_iterator_free(it);

        // We also need a scope_entry_t** for holding the conversor argument
        scope_entry_t** conversor_per_argument = new scope_entry_t*[argument_types.size() + 1];

        // Now invoke all the machinery
        scope_entry_t* entry_result =
        solve_overload(candidate_set,
                decl_context,
                uniquestr(filename.c_str()), line,
                conversor_per_argument);

        if (entry_result != NULL)
        {
            valid = true;
            // Store the arguments
            argument_conversor.clear();
            for (i = 0; i < argument_types.size(); i++)
            {
                argument_conversor.append(Symbol(conversor_per_argument[i]));
            }
        }

        // Free the conversor per argument
        delete[] conversor_per_argument;

        // Free the type array
        delete[] argument_types_array;

        // Free the scope entry list
        free_scope_entry_list(candidate_list);

        // This one has been allocated above
        free_scope_entry_list(first_candidate_list);

        return Symbol(entry_result);
    }
}
