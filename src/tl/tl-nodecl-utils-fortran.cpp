#include "tl-nodecl-utils-fortran.hpp"

#include "fortran03-buildscope.h"

#include "mem.h"

namespace Nodecl { namespace Utils {

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

            // Append all the symbols of the original_used_modules_info  to the new list
            for (int j = 0; j < original_used_modules_info->entity_specs.num_related_symbols; j++)
            {
                scope_entry_t* appended_module =
                        original_used_modules_info->entity_specs.related_symbols[j];
                P_LIST_ADD_ONCE(new_used_modules_info->entity_specs.related_symbols,
                        new_used_modules_info->entity_specs.num_related_symbols,
                        appended_module);

                // Make sure the module has been loaded...
                if (!appended_module->entity_specs.is_builtin)
                    fortran_load_module(appended_module->symbol_name, /* intrinsic */ 0, make_locus("", 0, 0));
            }
        }
    }

} }
