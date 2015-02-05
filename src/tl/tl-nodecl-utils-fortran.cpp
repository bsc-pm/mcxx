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
                symbol_entity_specs_get_num_related_symbols(original_used_modules_info) != 0)
        {
            scope_entry_t* new_used_modules_info
                = ::get_or_create_used_modules_symbol_info(new_scope.get_decl_context());

            // Append all the symbols of the original_used_modules_info  to the new list
            for (int j = 0; j < symbol_entity_specs_get_num_related_symbols(original_used_modules_info); j++)
            {
                scope_entry_t* appended_module =
                        symbol_entity_specs_get_related_symbols_num(original_used_modules_info, j);
                symbol_entity_specs_insert_related_symbols(new_used_modules_info, appended_module);

                // Make sure the module has been loaded...
                if (!symbol_entity_specs_get_is_builtin(appended_module))
                    fortran_load_module(appended_module->symbol_name, /* intrinsic */ 0, make_locus("", 0, 0));
            }
        }
    }

    void Fortran::append_module_to_scope(TL::Symbol module,
            TL::Scope scope)
    {
        ERROR_CONDITION(!module.is_valid() || !module.is_fortran_module(), "Symbol must be a Fortran module", 0);

        scope_entry_t* used_modules_info
            = ::get_or_create_used_modules_symbol_info(scope.get_decl_context());

        symbol_entity_specs_insert_related_symbols(used_modules_info,
                module.get_internal_symbol());

        if (!symbol_entity_specs_get_num_related_symbols(module.get_internal_symbol()))
            fortran_load_module(module.get_internal_symbol()->symbol_name, /* intrinsic */ 0, make_locus("", 0, 0));
    }

} }
