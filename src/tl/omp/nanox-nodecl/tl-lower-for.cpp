/*--------------------------------------------------------------------
  (C) Copyright 2006-2012 Barcelona Supercomputing Center
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


#include "tl-source.hpp"
#include "tl-lowering-visitor.hpp"
#include "tl-nodecl-utils.hpp"
#include "cxx-cexpr.h"
#include "tl-predicateutils.hpp"

namespace TL { namespace Nanox {

    void LoweringVisitor::visit(const Nodecl::OpenMP::For& construct)
    {
        Nodecl::List distribute_environment = construct.get_environment().as<Nodecl::List>();

        Nodecl::List ranges = construct.get_ranges().as<Nodecl::List>();
        Nodecl::NodeclBase statements = construct.get_statements();

        walk(statements);

        // Get the new statements
        statements = construct.get_statements();

        Nodecl::NodeclBase environment = construct.get_environment();

        OutlineInfo outline_info(environment);
        Symbol function_symbol = Nodecl::Utils::get_enclosing_function(construct);

        std::string outline_name = get_outline_name(function_symbol);

        // Add field wsd
        TL::Symbol sym = ReferenceScope(construct).get_scope().get_symbol_from_name("nanos_ws_desc_t");
        ERROR_CONDITION(sym.is_invalid(), "Invalid symbol", 0);

        TL::Type nanos_ws_desc_type = ::get_user_defined_type(sym.get_internal_symbol());
        nanos_ws_desc_type = nanos_ws_desc_type.get_pointer_to();

        OutlineDataItem &wsd_data_item = outline_info.prepend_field("wsd", nanos_ws_desc_type);
        wsd_data_item.set_sharing(OutlineDataItem::SHARING_CAPTURE);

        // Build the structure
        TL::Symbol structure_symbol = declare_argument_structure(outline_info, construct);

        Nodecl::NodeclBase placeholder1, placeholder2;
        Source for_code, reduction_code, barrier_code;
        if (ranges.size() == 1)
        {
            Nodecl::Range range_item = ranges.front().as<Nodecl::Range>();

            TL::Symbol ind_var = range_item.get_symbol();
            Nodecl::OpenMP::ForRange range(range_item.as<Nodecl::OpenMP::ForRange>());

            for_code
                << "while (nanos_loop_info.execute)"
                << "{"
                ;

            if (range.get_step().is_constant())
            {
                const_value_t* cval = range.get_step().get_constant();

                Nodecl::NodeclBase cval_nodecl = const_value_to_nodecl(cval);

                if (const_value_is_positive(cval))
                {

                    for_code
                        << "for (" << ind_var.get_name() << " = nanos_loop_info.lower;"
                        << ind_var.get_name() << " <= nanos_loop_info.upper;"
                        << ind_var.get_name() << " += " << cval_nodecl.prettyprint() << ")"
                        << "{"
                        ;
                }
                else
                {
                    for_code
                        << "for (" << ind_var.get_name() << " = nanos_loop_info.lower;"
                        << ind_var.get_name() << " >= nanos_loop_info.upper;"
                        << ind_var.get_name() << " += " << cval_nodecl.prettyprint() << ")"
                        << "{"
                        ;
                }

                for_code
                    << statement_placeholder(placeholder1)
                    << "}"
                    ;
            }
            else
            {

                for_code
                    << as_type(range.get_step().get_type()) << " nanos_step = " << as_expression(range.get_step()) << ";"
                    << "if (nanos_step > 0)"
                    << "{"
                    <<    "for (" << ind_var.get_name() << " = nanos_loop_info.lower;"
                    <<    ind_var.get_name() << " <= nanos_loop_info.upper;"
                    <<    ind_var.get_name() << " += nanos_step)"
                    <<    "{"
                    <<    statement_placeholder(placeholder1)
                    <<    "}"
                    << "}"
                    << "else"
                    << "{"
                    <<    "for (" << ind_var.get_name() << " = nanos_loop_info.lower;"
                    <<    ind_var.get_name() << " >= nanos_loop_info.upper;"
                    <<    ind_var.get_name() << " += nanos_step)"
                    <<    "{"
                    <<    statement_placeholder(placeholder2)
                    <<    "}"
                    << "}"
                    ;
            }

            for_code
                << "err = nanos_worksharing_next_item((nanos_ws_desc_t*)wsd, (nanos_ws_item_t*)&nanos_loop_info);"
                << "}"
                ;
        }
        else if (ranges.size() > 1)
        {
            internal_error("Collapsed ranges not implemented yet", 0);
        }
        else
        {
            internal_error("Code unreachable", 0);
        }

        Source outline_source;
        outline_source
            << "{"
            << "nanos_ws_item_loop_t nanos_loop_info;"
            << "nanos_err_t err;"
            << "err = nanos_worksharing_next_item((nanos_ws_desc_t*)wsd, (nanos_ws_item_t*)&nanos_loop_info);"
            << "if (err != NANOS_OK)"
            <<     "nanos_handle_error(err);"
            << for_code
            << reduction_code
            << "}"
            << barrier_code
            ;

        TL::ObjectList<OutlineDataItem> reduction_items = outline_info.get_data_items().filter(
                predicate(&OutlineDataItem::is_reduction));

        if (!reduction_items.empty())
        {
            for (TL::ObjectList<OutlineDataItem>::iterator it = reduction_items.begin();
                    it != reduction_items.end();
                    it++)
            {
                reduction_code
                    << "rdp_" << it->get_field_name() << "[omp_get_thread_num()] = " << it->get_symbol().get_name() << ";"
                    ;
            }
        }

        if (!reduction_items.empty()
                || !distribute_environment.find_first<Nodecl::OpenMP::BarrierAtEnd>().is_null())
        {
            barrier_code
                << full_barrier_source();
        }

        emit_outline(outline_info, statements, outline_source, outline_name, structure_symbol);

        // Now complete the placeholder
        Source iteration_source;
        iteration_source
            << statements.prettyprint()
            ;

        Nodecl::Utils::SymbolMap &symbol_map = outline_info.compute_symbol_map(placeholder1);
        placeholder1.integrate(
                Nodecl::Utils::deep_copy(statements, placeholder1, symbol_map)
                );

        if (!placeholder2.is_null())
        {
            Nodecl::Utils::SymbolMap &symbol_map2 = outline_info.compute_symbol_map(placeholder2);
            placeholder2.integrate(
                    Nodecl::Utils::deep_copy(statements, placeholder2, symbol_map)
                    );
        }

        loop_spawn(outline_info, construct, distribute_environment, ranges, outline_name, structure_symbol, outline_source);
    }

} }
