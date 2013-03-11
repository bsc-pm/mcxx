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


} }
