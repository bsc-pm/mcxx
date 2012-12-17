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

    void LoweringVisitor::loop_spawn_slicer(OutlineInfo& outline_info,
            Nodecl::NodeclBase construct,
            Nodecl::List distribute_environment,
            Nodecl::List ranges,
            const std::string& outline_name,
            TL::Symbol structure_symbol,
            TL::Symbol slicer_descriptor)
    {
        if (ranges.size() != 1)
        {
            internal_error("Only ranges of 1 dimension implemented", 0);
        }

        Nodecl::OpenMP::Schedule schedule = distribute_environment.find_first<Nodecl::OpenMP::Schedule>();
        ERROR_CONDITION(schedule.is_null(), "Schedule tree is missing", 0);

        Nodecl::OpenMP::ForRange distribute_range = ranges[0].as<Nodecl::OpenMP::ForRange>();
        Nodecl::NodeclBase lower = distribute_range.get_lower();
        Nodecl::NodeclBase upper = distribute_range.get_upper();
        Nodecl::NodeclBase step = distribute_range.get_step();

        Source struct_size;
        Source dynamic_size;
        Source struct_arg_type_name = structure_symbol.get_name();
        struct_size << "sizeof( " << struct_arg_type_name << " )" << dynamic_size;

        Source immediate_decl;
        allocate_immediate_structure(
                outline_info,
                struct_arg_type_name,
                struct_size,
                // out
                immediate_decl,
                dynamic_size);

        Nodecl::NodeclBase fill_outline_arguments_tree;
        Source fill_outline_arguments;

        Source fill_immediate_arguments;

        Source call_outline_function;

        Source schedule_setup;

        std::string schedule_name = schedule.get_text();

        std::string ompss_prefix = "ompss_";
        ERROR_CONDITION((schedule_name.substr(0, ompss_prefix.size()) != ompss_prefix), "Wrong schedule name", 0);

        schedule_name = schedule_name.substr(ompss_prefix.size());

        std::string slicer_name = schedule_name + "_for";

        schedule_setup
            << "nanos_slicer_t nanos_slicer; nanos_slicer = nanos_find_slicer(\"" << slicer_name << "\");"
            << "if (nanos_slicer == 0) nanos_handle_error(NANOS_UNIMPLEMENTED);"
            << "int nanos_chunk = " << (!schedule.get_chunk().is_null() ? as_expression(schedule.get_chunk()) : "1") << ";"
            ;

        std::multimap<std::string, std::string> dummy_multimap;
        Source const_wd_info;
        const_wd_info
            << fill_const_wd_info(struct_arg_type_name,
                    outline_name,
                    /* is_untied */ false,
                    /* mandatory_creation */ true,
                    /* num_copies */ count_copies(outline_info),
                    /* num_copies_dimensions */ count_copies_dimensions(outline_info),
                    outline_info.get_device_names(),
                    /* only used in task calls */ dummy_multimap,
                    construct);

        Source spawn_code, barrier_code;
        spawn_code
        << "{"
        <<     "nanos_err_t err;"
        <<     struct_arg_type_name << "* ol_args, imm_args;"
        <<     "ol_args = (" << struct_arg_type_name << "*) 0;"
        <<     "nanos_wd_t nanos_wd_ = (nanos_wd_t)0;"
        <<     "nanos_wd_dyn_props_t nanos_dyn_props;"
        <<     "nanos_dyn_props.tie_to = (nanos_thread_t)0;"
        <<     "nanos_dyn_props.priority = 0;"
        <<     const_wd_info
        <<     schedule_setup
        <<     "err = nanos_create_sliced_wd(&nanos_wd_, nanos_wd_const_data.base.num_devices, nanos_wd_const_data.devices, "
        <<            "sizeof(" << struct_arg_type_name << "),"
        <<            "nanos_wd_const_data.base.data_alignment,"
        <<            "(void**)&ol_args, nanos_current_wd(), nanos_slicer, &nanos_wd_const_data.base.props, &nanos_dyn_props,"
        <<            "0, 0, 0, 0);"
        <<     "if (err != NANOS_OK) nanos_handle_error(err);"
        <<     "ol_args->" << slicer_descriptor.get_name() << ".lower = " << as_expression(lower) << ";"
        <<     "ol_args->" << slicer_descriptor.get_name() << ".upper = " << as_expression(upper) << ";"
        <<     "ol_args->" << slicer_descriptor.get_name() << ".step = " << as_expression(step) << ";"
        <<     "ol_args->" << slicer_descriptor.get_name() << ".chunk = nanos_chunk;"
        <<     statement_placeholder(fill_outline_arguments_tree)
        <<     "err = nanos_submit(nanos_wd_, 0, 0, 0);"
        <<     "if (err != NANOS_OK) nanos_handle_error(err);"
        <<     barrier_code
        << "}"
        ;


        if (!distribute_environment.find_first<Nodecl::OpenMP::BarrierAtEnd>().is_null())
        {
            barrier_code 
                << "err = nanos_wg_wait_completion(nanos_current_wd(), 0);"
                << "if (err != NANOS_OK) nanos_handle_error(err);"
                ;
        }

        fill_arguments(construct, outline_info, fill_outline_arguments, fill_immediate_arguments);

        FORTRAN_LANGUAGE()
        {
            // Parse in C
            Source::source_language = SourceLanguage::C;
        }
        Nodecl::NodeclBase spawn_code_tree = spawn_code.parse_statement(construct);
        FORTRAN_LANGUAGE()
        {
            Source::source_language = SourceLanguage::Current;
        }

        // Now attach the slicer symbol to its final scope
        // See tl-lower-for.cpp
        slicer_descriptor.get_internal_symbol()->decl_context =
            fill_outline_arguments_tree.retrieve_context().get_decl_context();
        ::insert_entry(fill_outline_arguments_tree.retrieve_context().get_decl_context().current_scope,
                slicer_descriptor.get_internal_symbol());

        if (!fill_outline_arguments.empty())
        {
            Nodecl::NodeclBase new_tree = fill_outline_arguments.parse_statement(fill_outline_arguments_tree);
            fill_outline_arguments_tree.replace(new_tree);
        }

        construct.replace(spawn_code_tree);
    }

} }
