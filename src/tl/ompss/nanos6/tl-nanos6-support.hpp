/*--------------------------------------------------------------------
  (C) Copyright 2016-2018 Barcelona Supercomputing Center
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


#ifndef TL_NANOS6_SUPPORT_HPP
#define TL_NANOS6_SUPPORT_HPP

#include "tl-nanos6.hpp"

#include "tl-datareference.hpp"
#include "tl-objectlist.hpp"
#include "tl-scope.hpp"
#include "tl-nodecl.hpp"
#include "tl-nodecl-utils.hpp"

namespace TL { namespace Nanos6 {

    TL::Symbol get_nanos6_class_symbol(const std::string &name);
    TL::Symbol get_nanos6_function_symbol(const std::string &name);

    void add_extra_mappings_for_vla_types(
            TL::Type t,
            Scope sc,
            /* out */
            Nodecl::Utils::SimpleSymbolMap &symbol_map,
            TL::ObjectList<TL::Symbol> &vla_vars);


    void create_static_variable_depending_on_function_context(
            const std::string &var_name,
            TL::Type var_type,
            Nodecl::NodeclBase context,
            LoweringPhase* phase,
            /* out */
            TL::Symbol &new_var);

    //! Create a detached symbol with the same name as the real one We need to
    //! do that otherwise Fortran codegen attempts to initialize this symbol
    //! (We may want to fix this somehow)
    Symbol fortran_create_detached_symbol_from_static_symbol(
            Symbol &static_symbol);

    Scope compute_scope_for_environment_structure(Symbol related_function);

    Symbol add_field_to_class(Symbol new_class_symbol,
            Scope class_scope,
            const std::string &var_name,
            const locus_t *var_locus,
            bool is_allocatable,
            Type field_type);

    //! This utility generates, from a DataReference, a list of expressions that
    //! represent the base address and the dimensionality information
    void compute_base_address_and_dimensionality_information(
            const TL::DataReference& data_ref,
            // Out
            TL::ObjectList<Nodecl::NodeclBase>& arguments_list);
} }

#endif // TL_NANOS6_SUPPORT_HPP
