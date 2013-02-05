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
#include "tl-counters.hpp"
#include "cxx-cexpr.h"
#include "tl-predicateutils.hpp"
#include "tl-devices.hpp"

namespace TL { namespace Nanox {

    Source LoweringVisitor::get_loop_distribution_source_slicer(
            const Nodecl::OpenMP::For &construct,
            Nodecl::List& distribute_environment,
            Nodecl::List& ranges,
            OutlineInfo& outline_info,
            TL::Symbol slicer_descriptor,
            Nodecl::NodeclBase &placeholder1,
            Nodecl::NodeclBase &placeholder2)
    {
        Source for_code, reduction_code, lastprivate_code, reduction_initialization, step_initialization;
        Source instrument_before_opt, instrument_loop_opt, instrument_after_opt;
        if (ranges.size() > 1)
        {
            internal_error("Collapsed ranges not implemented yet", 0);
        }
        else if (ranges.size() == 1)
        {
            Nodecl::Range range_item = ranges.front().as<Nodecl::Range>();

            TL::Symbol ind_var = range_item.get_symbol();
            Nodecl::OpenMP::ForRange range(range_item.as<Nodecl::OpenMP::ForRange>());

            // Mark the induction variable as a private entity
            OutlineInfoRegisterEntities outline_info_register(outline_info, construct.retrieve_context());
            outline_info_register.add_private(ind_var);

            if (range.get_step().is_constant())
            {
                const_value_t* cval = range.get_step().get_constant();

                Nodecl::NodeclBase cval_nodecl = const_value_to_nodecl(cval);

                if (const_value_is_positive(cval))
                {

                    for_code
                        << instrument_loop_opt
                        << "for (" << ind_var.get_name() << " = nanos_lower;"
                        << ind_var.get_name() << " <= nanos_upper;"
                        << ind_var.get_name() << " += " << as_expression(cval_nodecl) << ")"
                        << "{"
                        ;
                }
                else
                {
                    for_code
                        << instrument_loop_opt
                        << "for (" << ind_var.get_name() << " = nanos_lower;"
                        << ind_var.get_name() << " >= nanos_upper;"
                        << ind_var.get_name() << " += " << as_expression(cval_nodecl) << ")"
                        << "{"
                        ;
                }

                for_code
                    << statement_placeholder(placeholder1)
                    << "}"
                    << lastprivate_code
                    ;
            }
            else
            {
                step_initialization
                    << "int nanos_step = " << slicer_descriptor.get_name() << ".step;"
                    ;
                for_code
                    << as_type(range.get_step().get_type()) << " nanos_step = " << as_expression(range.get_step()) << ";"
                    << "if (nanos_step > 0)"
                    << "{"
                    <<       instrument_loop_opt
                    <<       "for (" << ind_var.get_name() << " = nanos_lower;"
                    <<         ind_var.get_name() << " <= nanos_upper;"
                    <<         ind_var.get_name() << " += nanos_step)"
                    <<       "{"
                    <<       statement_placeholder(placeholder1)
                    <<       "}"
                    <<       lastprivate_code
                    << "}"
                    << "else"
                    << "{"
                    <<       instrument_loop_opt
                    <<       "for (" << ind_var.get_name() << " = nanos_lower;"
                    <<         ind_var.get_name() << " >= nanos_upper;"
                    <<         ind_var.get_name() << " += nanos_step)"
                    <<       "{"
                    <<          statement_placeholder(placeholder2)
                    <<       "}"
                    <<       lastprivate_code
                    << "}"
                    ;
            }

            if (_lowering->instrumentation_enabled())
            {
                instrument_before_opt
                    << "static int nanos_loop_init = 0;"
                    << "static nanos_event_key_t nanos_instr_loop_lower_key = 0;"
                    << "static nanos_event_key_t nanos_instr_loop_upper_key = 0;"
                    << "static nanos_event_key_t nanos_instr_loop_step_key = 0;"
                    << "static nanos_event_key_t nanos_instr_chunk_size_key = 0;"

                    << "if (nanos_loop_init == 0)"
                    << "{"
                    <<     "nanos_err_t err;"
                    <<     "err = nanos_instrument_get_key(\"loop-lower\", &nanos_instr_loop_lower_key);"
                    <<     "if (err != NANOS_OK) nanos_handle_error(err);"

                    <<     "err = nanos_instrument_get_key(\"loop-upper\", &nanos_instr_loop_upper_key);"
                    <<     "if (err != NANOS_OK) nanos_handle_error(err);"

                    <<     "err = nanos_instrument_get_key(\"loop-step\", &nanos_instr_loop_step_key);"
                    <<     "if (err != NANOS_OK) nanos_handle_error(err);"

                    <<     "nanos_loop_init = 1;"
                    << "}"

                    << "nanos_event_t loop_events[3];"
                    << "loop_events[0].type = NANOS_POINT;"
                    << "loop_events[1].type = NANOS_POINT;"
                    << "loop_events[2].type = NANOS_POINT;"

                    << "loop_events[0].key = nanos_instr_loop_lower_key;"
                    << "loop_events[1].key = nanos_instr_loop_upper_key;"
                    << "loop_events[2].key = nanos_instr_loop_step_key;"
                    ;

                instrument_loop_opt
                    << "loop_events[0].value = nanos_lower;"
                    << "loop_events[1].value = nanos_upper;"
                    ;
                if (range.get_step().is_constant())
                {
                    const_value_t* cval = range.get_step().get_constant();
                    Nodecl::NodeclBase cval_nodecl = const_value_to_nodecl(cval);

                    instrument_loop_opt
                        << "loop_events[2].value = " << as_expression(cval_nodecl) << ";"
                        ;
                }
                else
                {
                    instrument_loop_opt
                        << "loop_events[2].value = nanos_step;"
                        ;
                }
                instrument_loop_opt
                    << "nanos_instrument_events(3, loop_events);"
                    ;

                instrument_after_opt
                    << "loop_events[0].value = 0;"
                    << "loop_events[1].value = 0;"
                    << "loop_events[2].value = 1;"
                    << "nanos_instrument_events(3, loop_events);"
                    ;
            }
        }
        else
        {
            internal_error("Code unreachable", 0);
        }

        Source distribute_loop_source;
        distribute_loop_source
            << "{"
            << reduction_initialization
            << "int nanos_lower = " << slicer_descriptor.get_name() << ".lower;"
            << "int nanos_upper = " << slicer_descriptor.get_name() << ".upper;"
            << step_initialization
            << instrument_before_opt
            << for_code
            << instrument_after_opt
            << reduction_code
            << "}"
            ;

        reduction_initialization << reduction_initialization_code(outline_info, construct);
        reduction_code << perform_partial_reduction(outline_info);
        lastprivate_code << update_lastprivates(outline_info);

        return distribute_loop_source;
    }

    void LoweringVisitor::distribute_loop_with_outline_slicer(
           const Nodecl::OpenMP::For& construct,
           Nodecl::List& distribute_environment,
           Nodecl::List& ranges,
           OutlineInfo& outline_info,
           Nodecl::NodeclBase& statements,
           TL::Symbol slicer_descriptor,
           Source &outline_distribute_loop_source,
           // Loop (in the outline distributed code)
           Nodecl::NodeclBase& outline_placeholder1,
           // Auxiliar loop (when the step is not known at compile time, in the outline distributed code)
           Nodecl::NodeclBase& outline_placeholder2
           )
    {
        Symbol enclosing_function = Nodecl::Utils::get_enclosing_function(construct);


        OutlineDataItem &wsd_data_item = outline_info.prepend_field(slicer_descriptor);
        if (IS_FORTRAN_LANGUAGE)
        {
            wsd_data_item.set_in_outline_type(slicer_descriptor.get_type().get_lvalue_reference_to());
        }
        wsd_data_item.set_sharing(OutlineDataItem::SHARING_CAPTURE);

        // Outline

        DeviceHandler device_handler = DeviceHandler::get_device_handler();

        TL::Symbol called_task_dummy = TL::Symbol::invalid();
        TL::Symbol structure_symbol = declare_argument_structure(outline_info, construct);

        OutlineInfo::implementation_table_t implementation_table = outline_info.get_implementation_table();
        OutlineInfo::implementation_table_t::iterator implementation_it = implementation_table.find(enclosing_function);
        ERROR_CONDITION(implementation_it == implementation_table.end(),
                "No information from the implementation table", 0);

        TargetInformation target_info = implementation_it->second;
        std::string outline_name = target_info.get_outline_name();
        CreateOutlineInfo info(outline_name, outline_info.get_data_items(), target_info, statements,
                /* task_label */ Nodecl::NodeclBase::null(), structure_symbol, called_task_dummy);

        // List of device names
        TL::ObjectList<std::string> device_names = outline_info.get_device_names(enclosing_function);
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

            Source extended_outline_distribute_loop_source;
            extended_outline_distribute_loop_source
                << outline_distribute_loop_source
                ;

            if (IS_FORTRAN_LANGUAGE)
            {
                Source::source_language = SourceLanguage::C;
            }

            Nodecl::NodeclBase outline_code = extended_outline_distribute_loop_source.parse_statement(outline_placeholder);

            if (IS_FORTRAN_LANGUAGE)
            {
                Source::source_language = SourceLanguage::Current;
            }

            // Duplicate labels
            Nodecl::Utils::LabelSymbolMap label_symbol_map1(symbol_map, output_statements, outline_placeholder);
            outline_placeholder1.replace(Nodecl::Utils::deep_copy(output_statements, outline_placeholder1, label_symbol_map1));

            if (!outline_placeholder2.is_null())
            {
                Nodecl::Utils::LabelSymbolMap label_symbol_map2(symbol_map, output_statements, outline_placeholder);
                outline_placeholder2.replace(Nodecl::Utils::deep_copy(output_statements, outline_placeholder2, label_symbol_map2));
            }

            outline_placeholder.replace(Nodecl::Utils::deep_copy(outline_code, outline_placeholder, *symbol_map));

            delete symbol_map;
            symbol_map = NULL;
        }

        loop_spawn_slicer(outline_info, construct, distribute_environment, ranges, outline_name, structure_symbol, slicer_descriptor);
    }

    void LoweringVisitor::lower_for_slicer(const Nodecl::OpenMP::For& construct)
    {
        Nodecl::List ranges = construct.get_ranges().as<Nodecl::List>();

        Nodecl::List distribute_environment = construct.get_environment().as<Nodecl::List>();

        Nodecl::NodeclBase statements = construct.get_statements();

        walk(statements);

        // Get the new statements
        statements = construct.get_statements();

        // Slicer descriptor
        TL::Symbol nanos_slicer_loop_info_t_sym = ReferenceScope(construct).get_scope().get_symbol_from_name("nanos_loop_info_t");
        ERROR_CONDITION(nanos_slicer_loop_info_t_sym.is_invalid(), "Invalid symbol", 0);

        TL::Type nanos_slicer_desc_type = ::get_user_defined_type(nanos_slicer_loop_info_t_sym.get_internal_symbol());

        Counter& arg_counter = CounterManager::get_counter("nanos++-slicer-descriptor");
        std::stringstream ss;
        ss << "wsd_" << (int)arg_counter++;

        // Create a detached symbol. Will put in a scope later, in loop_spawn_slicer
        scope_entry_t* slicer_descriptor_internal = (scope_entry_t*)::calloc(1, sizeof(*slicer_descriptor_internal));
        // This is a transient scope but it will be changed before inserting the symbol
        // to its final scope
        slicer_descriptor_internal->decl_context = construct.retrieve_context().get_decl_context();
        TL::Symbol slicer_descriptor(slicer_descriptor_internal);
        slicer_descriptor.get_internal_symbol()->symbol_name = ::uniquestr(ss.str().c_str());
        slicer_descriptor.get_internal_symbol()->kind = SK_VARIABLE;
        slicer_descriptor.get_internal_symbol()->entity_specs.is_user_declared = 1;
        slicer_descriptor.get_internal_symbol()->type_information = nanos_slicer_desc_type.get_internal_type();

        Nodecl::NodeclBase environment = construct.get_environment();
        TL::Symbol enclosing_function = Nodecl::Utils::get_enclosing_function(construct);
        OutlineInfo outline_info(environment, enclosing_function);

        Nodecl::NodeclBase outline_placeholder1, outline_placeholder2;
        Source outline_distribute_loop_source = get_loop_distribution_source_slicer(construct,
                distribute_environment,
                ranges,
                outline_info,
                slicer_descriptor,
                outline_placeholder1,
                outline_placeholder2);

        distribute_loop_with_outline_slicer(construct,
                distribute_environment, ranges,
                outline_info,
                statements,
                slicer_descriptor,
                outline_distribute_loop_source,
                outline_placeholder1,
                outline_placeholder2);
    }

} }
