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
#include "tl-devices.hpp"

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

        Source outline_source, reduction_code, reduction_initialization;
        Nodecl::NodeclBase inner_placeholder;
        outline_source
            << "nanos_err_t err = nanos_omp_set_implicit(nanos_current_wd());"
            << "if (err != NANOS_OK) nanos_handle_error(err);"
            << "err = nanos_enter_team();"
            << "if (err != NANOS_OK) nanos_handle_error(err);"
            << reduction_initialization
            << statement_placeholder(inner_placeholder)
            << reduction_code
            << "err = nanos_omp_barrier();"
            << "if (err != NANOS_OK) nanos_handle_error(err);"
            << "err = nanos_leave_team();"
            << "if (err != NANOS_OK) nanos_handle_error(err);"
            ;

        reduction_initialization << reduction_initialization_code(outline_info, construct);
        reduction_code << perform_partial_reduction(outline_info);

        // Outline

        // List of device names
        TL::ObjectList<std::string> device_names = outline_info.get_device_names();

        DeviceHandler device_handler = DeviceHandler::get_device_handler();
        for (TL::ObjectList<std::string>::const_iterator it = device_names.begin();
                it != device_names.end();
                it++)
        {
            std::string device_name = *it;
            DeviceProvider* device = device_handler.get_device(device_name);

            ERROR_CONDITION(device == NULL, " Device '%s' has not been loaded.", device_name.c_str());

            Nodecl::NodeclBase outline_placeholder;
            Nodecl::Utils::SymbolMap *symbol_map = NULL;

            // FIXME: Can it be done only once?
            CreateOutlineInfo info(outline_name, outline_info, statements, structure_symbol);
            device->create_outline(info, outline_placeholder, symbol_map);

            if (IS_FORTRAN_LANGUAGE)
            {
                Source::source_language = SourceLanguage::C;
            }

            outline_placeholder.replace(outline_source.parse_statement(outline_placeholder));

            if (IS_FORTRAN_LANGUAGE)
            {
                Source::source_language = SourceLanguage::Current;
            }

            Nodecl::NodeclBase outline_statements_code = Nodecl::Utils::deep_copy(statements, inner_placeholder, *symbol_map);
            delete symbol_map;

            inner_placeholder.replace(outline_statements_code);
        }

        // This function replaces the current construct
        parallel_spawn(outline_info, construct, num_replicas, outline_name, structure_symbol);
    }
} }
