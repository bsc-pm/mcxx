#include "tl-overload.hpp"
#include "cxx-overload.h"

namespace TL
{

    static void free_scope_entry_list(scope_entry_list_t* entry)
    {
        if (entry->next != NULL)
        {
            free_scope_entry_list(entry->next);
        }
        delete entry;
    }

    Symbol Overload::solve(
            ObjectList<Symbol> candidate_functions,
            Type implicit_argument_type,
            ObjectList<Type> argument_types, 
            const std::string filename,
            int line,
            bool &valid, 
            ObjectList<Symbol>& argument_conversor)
    {
        valid = false;

        // Try hard to not to do useless work
        if (candidate_functions.empty())
        {
            return Symbol(NULL);
        }

        scope_entry_list_t* candidate_list = NULL;
        
        // Build the candidates list
        for (ObjectList<Symbol>::iterator it = candidate_functions.begin();
                it != candidate_functions.end();
                it++)
        {
            Symbol sym(*it);

            scope_entry_list_t* new_item = new scope_entry_list_t;
            new_item->entry = sym.get_internal_symbol();
            new_item->next = candidate_list;

            candidate_list = new_item;
        }

        // Build the type array
        int i, N = argument_types.size();
        type_t** argument_types_array = new type_t*[argument_types.size() + 1];
        argument_types_array[0] = implicit_argument_type.get_internal_type();
        for (i = 1; i < N; i++)
        {
            argument_types_array[i] = argument_types[i].get_internal_type();
        }

        // Now we need a decl_context_t but we were not given any explicitly,
        // use the one of the first candidate
        decl_context_t decl_context = candidate_functions[0].get_scope().get_decl_context();

        // We also need a scope_entry_t** for holding the conversor argument
        scope_entry_t** conversor_per_argument = new scope_entry_t*[argument_types.size()];

        // Now invoke all the machinery
        scope_entry_t* entry_result =
        solve_overload(candidate_list, 
                argument_types_array, /* Number of arguments */ N + 1,
                decl_context,
                filename.c_str(), line,
                conversor_per_argument);

        if (entry_result != NULL)
        {
            valid = true;
            // Store the arguments
            for (i = 0; i < (N+1); i++)
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

        return Symbol(entry_result);
    }
}
