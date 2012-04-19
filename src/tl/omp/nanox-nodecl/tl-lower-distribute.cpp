
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
        // Unused
        // Nodecl::NodeclBase distribute_environment = construct.get_environment();

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
            Nodecl::Parallel::DistributeRange range(it->as<Nodecl::Parallel::DistributeRange>());
            TL::Symbol ind_var = it->get_symbol();
            for_header 
                << "for (" << ind_var.get_name() << " = " << as_expression(range.get_lower()) << ";" 
                << ind_var.get_name() << " <= " << as_expression(range.get_upper()) << ";"
                << ind_var.get_name() << " += " << as_expression(range.get_step()) << ")"
                << "{"
                ;
            for_footer 
                << "}";
        }

        Source outline_source;
        outline_source
            << "{"
            << for_header
            << as_statement(statements.copy())
            << for_footer
            << "}"
            ;

        FORTRAN_LANGUAGE()
        {
            // Parse in C
            Source::source_language = SourceLanguage::C;
        }
        Nodecl::NodeclBase outline_code = outline_source.parse_statement(construct);
        FORTRAN_LANGUAGE()
        {
            Source::source_language = SourceLanguage::Current;
        }

        emit_outline(outline_info, outline_code, outline_name, structure_symbol);

        loop_spawn(outline_info, construct, ranges, outline_name, structure_symbol);
    }

} }
