#include "tl-nodecl-utils-fortran.hpp"

#include "fortran03-buildscope.h"

namespace Nodecl { namespace Utils {

    void Fortran::copy_used_modules(TL::Scope orig_scope,
            TL::Scope new_scope)
    {
        // Copy USEd modules symbol
        scope_entry_t* original_used_modules_info
            = orig_scope.get_related_symbol().get_used_modules().get_internal_symbol();
        if (original_used_modules_info != NULL)
        {
            scope_entry_t* new_used_modules_info
                = ::get_or_create_used_modules_symbol_info(new_scope.get_decl_context());
            new_used_modules_info->entity_specs.related_symbols = original_used_modules_info->entity_specs.related_symbols;
            new_used_modules_info->entity_specs.num_related_symbols = original_used_modules_info->entity_specs.num_related_symbols;
        }
    }

    void Fortran::append_used_modules(TL::Scope orig_scope,
            TL::Scope new_scope)
    {
        scope_entry_t* original_used_modules_info
            = orig_scope.get_related_symbol().get_used_modules().get_internal_symbol();

        if (original_used_modules_info != NULL &&
                original_used_modules_info->entity_specs.num_related_symbols != 0)
        {
            scope_entry_t* new_used_modules_info
                = ::get_or_create_used_modules_symbol_info(new_scope.get_decl_context());

            if (new_used_modules_info->entity_specs.num_related_symbols != 0)
            {
                int total_related_symbols = new_used_modules_info->entity_specs.num_related_symbols
                    + original_used_modules_info->entity_specs.num_related_symbols;

                //FIXME: Memory leaks
                scope_entry_t**  list_related_symbols = (scope_entry_t**)
                    malloc(total_related_symbols * sizeof(*new_used_modules_info->entity_specs.related_symbols));

                int index = 0;
                // Copy all the symbols of the new_used_modules_info to the new list
                for (int j = 0; j < new_used_modules_info->entity_specs.num_related_symbols; j++, index++)
                {
                    list_related_symbols[index] = new_used_modules_info->entity_specs.related_symbols[j];
                }
                // Append all the symbols of the original_used_modules_info  to the new list
                for (int j = 0; j < original_used_modules_info->entity_specs.num_related_symbols; j++, index++)
                {
                    list_related_symbols[index] = original_used_modules_info->entity_specs.related_symbols[j];
                }

                new_used_modules_info->entity_specs.related_symbols = list_related_symbols;
                new_used_modules_info->entity_specs.num_related_symbols = total_related_symbols;
            }
            else
            {
                new_used_modules_info->entity_specs.related_symbols = original_used_modules_info->entity_specs.related_symbols;
                new_used_modules_info->entity_specs.num_related_symbols = original_used_modules_info->entity_specs.num_related_symbols;
            }
        }
    }

} }
