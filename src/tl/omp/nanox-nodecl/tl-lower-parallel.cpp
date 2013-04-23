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

        Scope  enclosing_scope = construct.retrieve_context();
        Symbol function_symbol = Nodecl::Utils::get_enclosing_function(construct);
        OutlineInfo outline_info(environment,function_symbol);

        // Handle the special object 'this'
        if (IS_CXX_LANGUAGE
                && !function_symbol.is_static()
                && function_symbol.is_member())
        {
            TL::Symbol this_symbol = enclosing_scope.get_symbol_from_name("this");
            ERROR_CONDITION(!this_symbol.is_valid(), "Invalid symbol", 0);

            Nodecl::NodeclBase sym_ref = Nodecl::Symbol::make(this_symbol);
            sym_ref.set_type(this_symbol.get_type());

            // The object 'this' may already have an associated OutlineDataItem
            OutlineDataItem& argument_outline_data_item = outline_info.get_entity_for_symbol(this_symbol);

            // We must ensure that this OutlineDataItem is moved to the
            // first position of the list of OutlineDataItems.
            outline_info.move_at_begin(argument_outline_data_item);

            // This is a special kind of shared
            argument_outline_data_item.set_sharing(OutlineDataItem::SHARING_CAPTURE_ADDRESS);
            argument_outline_data_item.set_base_address_expression(sym_ref);
        }

        TL::Symbol structure_symbol = declare_argument_structure(outline_info, construct);

        Source outline_source, reduction_code_src, reduction_initialization_src;
        Nodecl::NodeclBase inner_placeholder;
        outline_source
            << "nanos_err_t err = nanos_omp_set_implicit(nanos_current_wd());"
            << "if (err != NANOS_OK) nanos_handle_error(err);"
            << "err = nanos_enter_team();"
            << "if (err != NANOS_OK) nanos_handle_error(err);"
            << reduction_initialization_src
            << statement_placeholder(inner_placeholder)
            << reduction_code_src
            << "err = nanos_omp_barrier();"
            << "if (err != NANOS_OK) nanos_handle_error(err);"
            << "err = nanos_leave_team();"
            << "if (err != NANOS_OK) nanos_handle_error(err);"
            ;

        Nodecl::NodeclBase reduction_initialization, reduction_code;
        if (there_are_reductions(outline_info))
        {
            reduction_initialization_src << statement_placeholder(reduction_initialization);
            reduction_code_src << statement_placeholder(reduction_code);
        }

        // Outline

        DeviceHandler device_handler = DeviceHandler::get_device_handler();

        OutlineInfo::implementation_table_t implementation_table =outline_info.get_implementation_table();
        OutlineInfo::implementation_table_t::iterator implementation_it = implementation_table.find(function_symbol);
        ERROR_CONDITION(implementation_it == implementation_table.end(),
                "No information from the implementation table", 0)

        TL::Symbol called_task_dummy;
        TargetInformation target_info = implementation_it->second;
        std::string outline_name = target_info.get_outline_name();
        CreateOutlineInfo info(outline_name,
                outline_info.get_data_items(),
                target_info,
                /* original statements */ statements,
                /* current task statements */ statements,
                /* task_label */ Nodecl::NodeclBase::null(),
                structure_symbol,
                called_task_dummy);

        // List of device names
        TL::ObjectList<std::string> device_names = outline_info.get_device_names(function_symbol);
        for (TL::ObjectList<std::string>::const_iterator it = device_names.begin();
                it != device_names.end();
                it++)
        {
            std::string device_name = *it;
            DeviceProvider* device = device_handler.get_device(device_name);

            ERROR_CONDITION(device == NULL, " Device '%s' has not been loaded.", device_name.c_str());

            Nodecl::NodeclBase outline_placeholder, output_statements;
            Nodecl::Utils::SymbolMap *symbol_map = NULL;
            device->create_outline(info, outline_placeholder, output_statements, symbol_map);

            if (IS_FORTRAN_LANGUAGE)
            {
                Source::source_language = SourceLanguage::C;
            }

            outline_placeholder.replace(outline_source.parse_statement(outline_placeholder));

            if (IS_FORTRAN_LANGUAGE)
            {
                Source::source_language = SourceLanguage::Current;
            }

            if (there_are_reductions(outline_info))
            {
                reduction_initialization_code(outline_info, reduction_initialization, construct);
                perform_partial_reduction(outline_info, reduction_code);
            }

            Nodecl::NodeclBase outline_statements_code = Nodecl::Utils::deep_copy(output_statements, outline_placeholder, *symbol_map);
            delete symbol_map;

            inner_placeholder.replace(outline_statements_code);
        }

        // This function replaces the current construct
        parallel_spawn(outline_info, construct, num_replicas, outline_name, structure_symbol);
    }
} }
