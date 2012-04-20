
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
#include "tl-nodecl-alg.hpp"

namespace TL { namespace Nanox {

    void LoweringVisitor::visit(const Nodecl::Parallel::Distribute& construct)
    {
        Nodecl::List distribute_environment = construct.get_environment().as<Nodecl::List>();

        Nodecl::List ranges = construct.get_ranges().as<Nodecl::List>();
        Nodecl::NodeclBase executable_part = construct.get_exec();

        ERROR_CONDITION(!executable_part.is<Nodecl::Parallel::Async>(), "Invalid tree", 0);

        Nodecl::Parallel::Async async = executable_part.as<Nodecl::Parallel::Async>();
        Nodecl::NodeclBase statements = async.get_statements();

        Nodecl::NodeclBase environment = async.get_environment();

        OutlineInfo outline_info(environment);
        Symbol function_symbol = Nodecl::Utils::get_enclosing_function(construct);

        std::string outline_name = get_outline_name(function_symbol);

        // Add field wsd
        TL::Symbol sym = Source::ReferenceScope(construct).get_scope().get_symbol_from_name("nanos_ws_desc_t");
        ERROR_CONDITION(sym.is_invalid(), "Invalid symbol", 0);

        TL::Type nanos_ws_desc_type = ::get_user_defined_type(sym.get_internal_symbol());
        nanos_ws_desc_type = nanos_ws_desc_type.get_lvalue_reference_to();

        OutlineDataItem &wsd_data_item = outline_info.prepend_field("wsd", nanos_ws_desc_type);
        wsd_data_item.set_sharing(OutlineDataItem::SHARING_CAPTURE);

        // Build the structure
        TL::Symbol structure_symbol = declare_argument_structure(outline_info, construct);

        Source for_header, for_footer;
        for (Nodecl::List::iterator it = ranges.begin();
                it != ranges.end();
                it++)
        {
            // FIXME - Negative step
            TL::Symbol ind_var = it->get_symbol();
            Nodecl::Parallel::DistributeRange range(it->as<Nodecl::Parallel::DistributeRange>());

            for_header 
                << "for (" << ind_var.get_name() << " = nanos_loop_info.lower;" 
                << ind_var.get_name() << " <= nanos_loop_info.upper;"
                << ind_var.get_name() << " += " << as_expression(range.get_step()) << ")"
                << "{"
                ;
            for_footer 
                << "}";
        }
        
        //     nanos_worksharing_next_item(_args->wsd, (nanos_ws_item_t *) &_nth_info);
        //     if (1 > 0)
        //     {
        //         while (_nth_info.execute)
        //         {
        //             for (i = _nth_info.lower;
        //                 i <= _nth_info.upper;
        //                 i += 1)

        Nodecl::NodeclBase placeholder;
        Source outline_source;
        outline_source
            << "{"
            << "nanos_ws_item_loop_t nanos_loop_info;"
            << "nanos_err_t err;"
            << "err = nanos_worksharing_next_item(&wsd, (nanos_ws_item_t*)&nanos_loop_info);"
            << "if (err != NANOS_OK)"
            <<     "nanos_handle_error(err);"
            << for_header
            << statement_placeholder(placeholder)
            << for_footer
            << "}"
            ;

        emit_outline(outline_info, statements, outline_source, outline_name, structure_symbol);

        // Now complete the placeholder
        Source iteration_source;
        iteration_source
            << statements.prettyprint()
            ;

        Nodecl::NodeclBase iteration_code = iteration_source.parse_statement(placeholder);
        placeholder.integrate(iteration_code);

        loop_spawn(outline_info, construct, distribute_environment, ranges, outline_name, structure_symbol);
    }

} }
