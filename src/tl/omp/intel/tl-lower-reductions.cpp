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


#include "tl-lower-reductions.hpp"
#include "tl-symbol-utils.hpp"
#include "tl-nodecl-utils.hpp"
#include "tl-counters.hpp"

namespace TL
{
    TL::Symbol Intel::emit_callback_for_reduction(
            OpenMP::Reduction* reduction,
            Nodecl::NodeclBase location,
            TL::Symbol current_function)
    {
        TL::ObjectList<std::string> parameter_names;
        TL::ObjectList<TL::Type> parameter_types;

        parameter_names.append("red_omp_out");
        parameter_types.append(reduction->get_type().get_lvalue_reference_to());

        parameter_names.append("red_omp_in");
        parameter_types.append(reduction->get_type().get_lvalue_reference_to());

        TL::Counter &counters = TL::CounterManager::get_counter("intel-omp-reduction");
        std::stringstream ss;
        ss << "_red_" << (int)counters;
        counters++;

        TL::Symbol new_callback = SymbolUtils::new_function_symbol(
                current_function,
                ss.str(),
                get_void_type(),
                parameter_names,
                parameter_types);

        Nodecl::NodeclBase function_code, empty_stmt;

        SymbolUtils::build_empty_body_for_function(
                new_callback,
                function_code,
                empty_stmt);

        TL::Symbol red_omp_out = empty_stmt.retrieve_context().get_symbol_from_name("red_omp_out");
        TL::Symbol red_omp_in = empty_stmt.retrieve_context().get_symbol_from_name("red_omp_in");

        Nodecl::Utils::SimpleSymbolMap symbol_map;
        symbol_map.add_map(reduction->get_omp_out(), red_omp_out);
        symbol_map.add_map(reduction->get_omp_in(), red_omp_in);

        TL::Source combiner;
        combiner << as_expression(
                Nodecl::Utils::deep_copy(
                    reduction->get_combiner(),
                    empty_stmt,
                    symbol_map)
                ) << ";"
            ;

        Nodecl::NodeclBase new_body_tree = combiner.parse_statement(empty_stmt);
        empty_stmt.replace(new_body_tree);

        Nodecl::Utils::prepend_to_enclosing_top_level_location(location, function_code);

        return new_callback;
    }
}
