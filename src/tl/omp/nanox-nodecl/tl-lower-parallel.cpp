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
#include "tl-predicateutils.hpp"

namespace TL { namespace Nanox {

    // -- Not used yet
    struct ParallelEnvironmentVisitor : public Nodecl::ExhaustiveVisitor<void>
    {
        public:
            ParallelEnvironmentVisitor()
            {
            }
    };

    void LoweringVisitor::visit(const Nodecl::OpenMP::Parallel& construct)
    {
        Nodecl::NodeclBase num_replicas = construct.get_num_replicas();
        Nodecl::NodeclBase environment = construct.get_environment();
        Nodecl::NodeclBase statements = construct.get_statements();

        walk(statements);

        // Get the new statements
        statements = construct.get_statements();

        OutlineInfo outline_info(environment);

        TL::Symbol structure_symbol = declare_argument_structure(outline_info, construct);

        Symbol function_symbol = Nodecl::Utils::get_enclosing_function(construct);
        std::string outline_name = get_outline_name(function_symbol);

        Source outline_source, reduction_code;
        Nodecl::NodeclBase placeholder;
        outline_source
            << statement_placeholder(placeholder)
            << reduction_code
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

        emit_outline(outline_info, statements, outline_source, outline_name, structure_symbol);

        Source outline_statements_source;
        outline_statements_source
            << statements.prettyprint();

        Nodecl::NodeclBase outline_statements_code = outline_statements_source.parse_statement(placeholder);
        placeholder.integrate(outline_statements_code);

        // This function replaces the current construct
        parallel_spawn(outline_info, construct, num_replicas, outline_name, structure_symbol);
    }
} }
