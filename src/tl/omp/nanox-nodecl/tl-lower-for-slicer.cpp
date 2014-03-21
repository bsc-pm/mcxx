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
#include "tl-counters.hpp"
#include "cxx-cexpr.h"
#include "tl-predicateutils.hpp"
#include "tl-devices.hpp"

namespace TL { namespace Nanox {

    Source LoweringVisitor::get_loop_distribution_source_slicer(
            const Nodecl::OpenMP::For &construct,
            Nodecl::List& distribute_environment,
            Nodecl::RangeLoopControl& range,
            OutlineInfo& outline_info,
            TL::Symbol slicer_descriptor,
            Nodecl::NodeclBase &placeholder1,
            Nodecl::NodeclBase &placeholder2,
            Nodecl::NodeclBase &reduction_initialization,
            Nodecl::NodeclBase &reduction_code)
    {
        Source for_code, lastprivate_code, step_initialization;
        Source instrument_before_opt, instrument_loop_opt, instrument_after_opt;

        TL::Symbol ind_var = range.get_induction_variable().get_symbol();

        // Mark the induction variable as a private entity
        OutlineInfoRegisterEntities outline_info_register(outline_info, construct.retrieve_context());
        outline_info_register.add_private(ind_var);

        Source loop_name;
        if (!construct.get_loop().as<Nodecl::ForStatement>().get_loop_name().is_null())
        {
            loop_name << " [ " << as_symbol(construct.get_loop().as<Nodecl::ForStatement>().get_loop_name().get_symbol()) << " ]";
        }

        if (range.get_step().is_constant())
        {
            const_value_t* cval = range.get_step().get_constant();

            Nodecl::NodeclBase cval_nodecl = const_value_to_nodecl(cval);

            if (const_value_is_positive(cval))
            {

                for_code
                    << instrument_loop_opt
                    << "for" << loop_name << " (" << ind_var.get_name() << " = nanos_lower;"
                    << ind_var.get_name() << " <= nanos_upper;"
                    << ind_var.get_name() << " += " << as_expression(cval_nodecl) << ")"
                    << "{"
                    ;
            }
            else
            {
                for_code
                    << instrument_loop_opt
                    << "for" << loop_name << " (" << ind_var.get_name() << " = nanos_lower;"
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
                << "if (nanos_step > 0)"
                << "{"
                <<       instrument_loop_opt
                <<       "for" << loop_name << " (" << ind_var.get_name() << " = nanos_lower;"
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
                <<       "for" << loop_name << " (" << ind_var.get_name() << " = nanos_lower;"
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

                << "nanos_err_t err;"
                << "if (nanos_loop_init == 0)"
                << "{"
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
                << "err = nanos_instrument_events(3, loop_events);"
                << "if (err != NANOS_OK) nanos_handle_error(err);"
                ;

            instrument_after_opt
                << "loop_events[0].value = 0;"
                << "loop_events[1].value = 0;"
                << "loop_events[2].value = 1;"
                << "err = nanos_instrument_events(3, loop_events);"
                << "if (err != NANOS_OK) nanos_handle_error(err);"
                ;
        }

        Source distribute_loop_source, reduction_initialization_src, reduction_code_src;
        distribute_loop_source
            << "{"
            << reduction_initialization_src
            << "int nanos_lower = " << slicer_descriptor.get_name() << ".lower;"
            << "int nanos_upper = " << slicer_descriptor.get_name() << ".upper;"
            << step_initialization
            << instrument_before_opt
            << for_code
            << instrument_after_opt
            << reduction_code_src
            << "}"
            ;


        if (there_are_reductions(outline_info))
        {
            reduction_initialization_src << statement_placeholder(reduction_initialization);
            reduction_code_src << statement_placeholder(reduction_code);
        }

        lastprivate_code << update_lastprivates(outline_info, slicer_descriptor.get_name());

        return distribute_loop_source;
    }

    void LoweringVisitor::distribute_loop_with_outline_slicer(
            const Nodecl::OpenMP::For& construct,
            Nodecl::List& distribute_environment,
            Nodecl::RangeLoopControl& range,
            OutlineInfo& outline_info,
            Nodecl::NodeclBase& statements,
            TL::Symbol slicer_descriptor,
            Source &outline_distribute_loop_source,
            // Loop (in the outline distributed code)
            Nodecl::NodeclBase& outline_placeholder1,
            // Auxiliar loop (when the step is not known at compile time, in the outline distributed code)
            Nodecl::NodeclBase& outline_placeholder2,
            Nodecl::NodeclBase& reduction_initialization,
            Nodecl::NodeclBase& reduction_code)
    {
        Symbol enclosing_function = Nodecl::Utils::get_enclosing_function(construct);

        Nodecl::NodeclBase task_label = construct.get_environment().as<Nodecl::List>()
            .find_first<Nodecl::OpenMP::TaskLabel>();

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
        CreateOutlineInfo info(outline_name,
                outline_info.get_data_items(),
                target_info,
                /* original task statements */ statements,
                /* current task statements */ statements,
                task_label, structure_symbol, called_task_dummy);

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
            Nodecl::Utils::SimpleSymbolMap *symbol_map = NULL;
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

            ERROR_CONDITION(!output_statements.is<Nodecl::List>(), "Unreachable code", 0);

            // Duplicate labels
            Nodecl::Utils::LabelSymbolMap label_symbol_map1(symbol_map, output_statements, outline_placeholder);
            Nodecl::List updated_output_statements =
                Nodecl::Utils::deep_copy(output_statements, outline_placeholder1, label_symbol_map1).as<Nodecl::List>();

            // Split the output statements in two new lists: one for the nested
            // function definitions and other for the rest of nodes
            Nodecl::List output_statements_filtered, nested_function_definitions;
            for (Nodecl::List::iterator it2 = updated_output_statements.begin();
                    it2 != updated_output_statements.end();
                    ++it2)
            {
                if (it2->is<Nodecl::FunctionCode>())
                    nested_function_definitions.append(*it2);
                else
                    output_statements_filtered.append(*it2);
            }

            outline_placeholder1.replace(output_statements_filtered);

            if (!outline_placeholder2.is_null())
            {
                Nodecl::Utils::LabelSymbolMap label_symbol_map2(symbol_map, output_statements_filtered, outline_placeholder);
                outline_placeholder2.replace(Nodecl::Utils::deep_copy(output_statements_filtered, outline_placeholder2, label_symbol_map2));
            }

            if (there_are_reductions(outline_info))
            {
                reduction_initialization_code_slicer(outline_info, reduction_initialization, construct);
                perform_partial_reduction_slicer(outline_info, reduction_code, symbol_map);
            }

            Nodecl::NodeclBase updated_outline_code = Nodecl::Utils::deep_copy(outline_code, outline_placeholder, *symbol_map);
            outline_placeholder.replace(updated_outline_code);

            Nodecl::Utils::append_items_after(outline_placeholder, nested_function_definitions);
            delete symbol_map;
        }

        loop_spawn_slicer(outline_info,
                construct,
                distribute_environment,
                range,
                outline_name,
                structure_symbol,
                slicer_descriptor,
                task_label);
    }

    void LoweringVisitor::lower_for_slicer(const Nodecl::OpenMP::For& construct)
    {
        TL::ForStatement for_statement(construct.get_loop().as<Nodecl::ForStatement>());
        ERROR_CONDITION(!for_statement.is_omp_valid_loop(), "Invalid loop at this point", 0);

        Nodecl::RangeLoopControl range =
            Nodecl::RangeLoopControl::make(
                    Nodecl::Symbol::make(
                        for_statement.get_induction_variable(),
                        construct.get_locus()),
                    for_statement.get_lower_bound(),
                    for_statement.get_upper_bound(),
                    for_statement.get_step(),
                    construct.get_locus());

        Nodecl::List distribute_environment = construct.get_environment().as<Nodecl::List>();

        Nodecl::NodeclBase statements = for_statement.get_statement();

        walk(statements);

        // Get the new statements
        statements = for_statement.get_statement();

        // Slicer descriptor
        TL::Symbol nanos_slicer_loop_info_t_sym = ReferenceScope(construct).get_scope().get_symbol_from_name("nanos_loop_info_t");
        ERROR_CONDITION(nanos_slicer_loop_info_t_sym.is_invalid(), "Invalid symbol", 0);

        TL::Type nanos_slicer_desc_type = ::get_user_defined_type(nanos_slicer_loop_info_t_sym.get_internal_symbol());

        Counter& arg_counter = CounterManager::get_counter("nanos++-slicer-descriptor");
        std::stringstream ss;
        ss << "wsd_" << (int)arg_counter++;

        // Create a detached symbol. Will put in a scope later, in loop_spawn_slicer
        scope_entry_t* slicer_descriptor_internal = (scope_entry_t*)::xcalloc(1, sizeof(*slicer_descriptor_internal));
        // This is a transient scope but it will be changed before inserting the symbol
        // to its final scope
        slicer_descriptor_internal->decl_context = construct.retrieve_context().get_decl_context();
        TL::Symbol slicer_descriptor(slicer_descriptor_internal);
        slicer_descriptor.get_internal_symbol()->symbol_name = ::uniquestr(ss.str().c_str());
        slicer_descriptor.get_internal_symbol()->kind = SK_VARIABLE;
        slicer_descriptor.get_internal_symbol()->entity_specs.is_user_declared = 1;
        slicer_descriptor.get_internal_symbol()->type_information = nanos_slicer_desc_type.get_internal_type();

        Nodecl::NodeclBase environment = construct.get_environment();
        Scope  enclosing_scope = construct.retrieve_context();
        TL::Symbol enclosing_function = Nodecl::Utils::get_enclosing_function(construct);
        OutlineInfo outline_info(environment, enclosing_function);

        // Handle the special object 'this'
        if (IS_CXX_LANGUAGE
                && !enclosing_function.is_static()
                && enclosing_function.is_member())
        {
            TL::Symbol this_symbol = enclosing_scope.get_symbol_from_name("this");
            ERROR_CONDITION(!this_symbol.is_valid(), "Invalid symbol", 0);

            Nodecl::NodeclBase sym_ref = Nodecl::Symbol::make(this_symbol);
            sym_ref.set_type(this_symbol.get_type());

            // The object 'this' may already have an associated OutlineDataItem
            OutlineDataItem& argument_outline_data_item =
                outline_info.get_entity_for_symbol(this_symbol);

            argument_outline_data_item.set_is_cxx_this(true);

            // This is a special kind of shared
            argument_outline_data_item.set_sharing(OutlineDataItem::SHARING_CAPTURE_ADDRESS);
            argument_outline_data_item.set_base_address_expression(sym_ref);
        }

        Nodecl::NodeclBase outline_placeholder1, outline_placeholder2, reduction_initialization, reduction_code;
        Source outline_distribute_loop_source = get_loop_distribution_source_slicer(construct,
                distribute_environment,
                range,
                outline_info,
                slicer_descriptor,
                outline_placeholder1,
                outline_placeholder2,
                reduction_initialization,
                reduction_code);

        distribute_loop_with_outline_slicer(construct,
                distribute_environment, range,
                outline_info,
                statements,
                slicer_descriptor,
                outline_distribute_loop_source,
                outline_placeholder1,
                outline_placeholder2,
                reduction_initialization,
                reduction_code);
    }

} }
