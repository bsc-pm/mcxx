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

#ifndef TL_SYMBOL_UTILS_HPP
#define TL_SYMBOL_UTILS_HPP
#include "tl-symbol.hpp"
#include "tl-type.hpp"
#include "tl-objectlist.hpp"

namespace SymbolUtils
{
    // This function returns a new almost empty function symbol
    TL::Symbol new_function_symbol_for_deep_copy(
            TL::Symbol source,
            std::string new_symbol_name);

    TL::Symbol new_function_symbol(
            TL::Symbol function,
            const std::string& function_name);

    TL::Symbol new_function_symbol(
            TL::Symbol current_function,
            const std::string& function_name,
            TL::Type return_type,
            TL::ObjectList<std::string> parameter_names,
            TL::ObjectList<TL::Type> parameter_types);

    TL::Symbol new_function_symbol(
            TL::Symbol current_function,
            const std::string& function_name,
            const std::string& result_symbol_name,
            TL::Type return_type,
            TL::ObjectList<std::string> parameter_names,
            TL::ObjectList<TL::Type> parameter_types);

    TL::Symbol new_function_symbol(
            TL::Scope sc,
            const std::string& function_name,
            TL::Type return_type,
            TL::ObjectList<std::string> parameter_names,
            TL::ObjectList<TL::Type> parameter_types);

    TL::Symbol new_function_symbol(
            TL::Scope sc,
            const std::string& function_name,
            const std::string& result_symbol_name,
            TL::Type return_type,
            TL::ObjectList<std::string> parameter_names,
            TL::ObjectList<TL::Type> parameter_types);

    void build_empty_body_for_function(
            TL::Symbol function_symbol,
            Nodecl::NodeclBase &function_code,
            Nodecl::NodeclBase &empty_stmt);

    // This function creates a new class template and returns the symbol
    // of its primary class template
    TL::Symbol new_class_template(const std::string &template_name,
                                  template_parameter_list_t *tpl,
                                  TL::Scope orig_sc,
                                  const locus_t *locus);
}

#endif //TL_SYMBOL_UTILS_HPP
