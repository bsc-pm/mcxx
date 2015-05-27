/*--------------------------------------------------------------------
  (C) Copyright 2006-2015 Barcelona Supercomputing Center
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
#include "tl-nanos.hpp"
#include "tl-predicateutils.hpp"

namespace TL { namespace Nanox {

    void LoweringVisitor::loop_spawn_worksharing(OutlineInfo& outline_info,
            Nodecl::NodeclBase construct,
            Nodecl::List distribute_environment,
            Nodecl::RangeLoopControl& range,
            const std::string& outline_name,
            TL::Symbol structure_symbol,
            TL::Symbol slicer_descriptor,
            Nodecl::NodeclBase task_label)
    {
        Symbol enclosing_function = Nodecl::Utils::get_enclosing_function(construct);

        Nodecl::OpenMP::Schedule schedule = distribute_environment.find_first<Nodecl::OpenMP::Schedule>();
        ERROR_CONDITION(schedule.is_null(), "Schedule tree is missing", 0);

        Nodecl::NodeclBase lower = range.get_lower();
        Nodecl::NodeclBase upper = range.get_upper();
        Nodecl::NodeclBase step = range.get_step();

        Source struct_size, dynamic_size, struct_arg_type_name;

        struct_arg_type_name
            << ((structure_symbol.get_type().is_template_specialized_type()
                        &&  structure_symbol.get_type().is_dependent()) ? "typename " : "")
            << structure_symbol.get_qualified_name(enclosing_function.get_scope())
            ;

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

        Nodecl::NodeclBase fill_immediate_arguments_tree;
        Source fill_immediate_arguments;

        Source call_outline_function;

        Source schedule_setup;
        schedule_setup
            <<     "int nanos_chunk;"
            ;
        if (schedule.get_text() == "runtime")
        {
            schedule_setup
                <<     "nanos_omp_sched_t nanos_runtime_sched;"
                <<     "nanos_err = nanos_omp_get_schedule(&nanos_runtime_sched, &nanos_chunk);"
                <<     "if (nanos_err != NANOS_OK)"
                <<         "nanos_handle_error(nanos_err);"
                <<     "nanos_ws_t current_ws_policy = nanos_omp_find_worksharing(nanos_runtime_sched);"
                ;
        }
        else
        {
            Source schedule_name;

            if (Nanos::Version::interface_is_at_least("openmp", 8))
            {
                schedule_name << "nanos_omp_sched_" << schedule.get_text();
            }
            else
            {
                // We used nanos_omp_sched in versions prior to 8
                schedule_name << "omp_sched_" << schedule.get_text();
            }

            schedule_setup
                <<     "nanos_ws_t current_ws_policy = nanos_omp_find_worksharing(" << schedule_name << ");"
                <<     "if (current_ws_policy == 0)"
                <<         "nanos_handle_error(NANOS_UNIMPLEMENTED);"
                <<     "nanos_chunk = " << as_expression(schedule.get_chunk()) << ";"
            ;
        }


        Source worksharing_creation;

        if (IS_CXX_LANGUAGE)
        {
            worksharing_creation
                << as_statement(Nodecl::CxxDef::make(Nodecl::NodeclBase::null(), slicer_descriptor));
        }
        worksharing_creation
            <<     "nanos_err = nanos_worksharing_create(&" << as_symbol(slicer_descriptor) << ", current_ws_policy, (void**)&nanos_setup_info_loop, &single_guard);"
            <<     "if (nanos_err != NANOS_OK)"
            <<         "nanos_handle_error(nanos_err);"
            ;

        std::string wd_description;
        if (!task_label.is_null())
        {
            wd_description = task_label.get_text();
        }
        else
        {
            wd_description = enclosing_function.get_name();
        }

        Source const_wd_info;
        const_wd_info
            << fill_const_wd_info(struct_arg_type_name,
                    /* is_untied */ false,
                    /* mandatory_creation */ true,
                    /* is_function_task */ false,
                    wd_description,
                    outline_info,
                    construct);

        Source dependence_type;
        dependence_type
            << "nanos_data_access_t*";

        Source dynamic_wd_info;
        dynamic_wd_info
            << "nanos_wd_dyn_props_t dyn_props;"
            << "dyn_props.tie_to = (nanos_thread_t)0;"
            << "dyn_props.priority = 0;"
            ;

        if (!_lowering->final_clause_transformation_disabled()
                && Nanos::Version::interface_is_at_least("master", 5024))
        {
            dynamic_wd_info
                << "dyn_props.flags.is_final = 0;"
                ;
        }

        // Only tasks created in a parallel construct are marked as implicit
        if (Nanos::Version::interface_is_at_least("master", 5029))
        {
            dynamic_wd_info
                << "dyn_props.flags.is_implicit = 0;"
                ;
        }

        Source spawn_code;
        spawn_code
        << "{"
        <<     as_type(get_bool_type()) << " single_guard;"
        <<     "nanos_err_t nanos_err;"
        <<     schedule_setup
        <<     "nanos_ws_info_loop_t nanos_setup_info_loop;"
        <<     "nanos_setup_info_loop.lower_bound = " << as_expression(lower) << ";"
        <<     "nanos_setup_info_loop.upper_bound = " << as_expression(upper) << ";"
        <<     "nanos_setup_info_loop.loop_step = "   << as_expression(step)  << ";"
        <<     "nanos_setup_info_loop.chunk_size = nanos_chunk;"
        <<     worksharing_creation
        <<     "if (single_guard)"
        <<     "{"
        <<         "int sup_threads;"
        <<         "nanos_err = nanos_team_get_num_supporting_threads(&sup_threads);"
        <<         "if (nanos_err != NANOS_OK)"
        <<             "nanos_handle_error(nanos_err);"
        <<         "if (sup_threads > 0)"
        <<         "{"
        <<             "nanos_err = nanos_malloc((void**)&(" << as_symbol(slicer_descriptor) << "->threads), sizeof(nanos_thread_t) * sup_threads, \"\", 0);"
        <<             "if (nanos_err != NANOS_OK)"
        <<                 "nanos_handle_error(nanos_err);"
        <<             "nanos_err = nanos_team_get_supporting_threads(&" << as_symbol(slicer_descriptor) << "->nths, " 
        <<                        as_symbol(slicer_descriptor) << "->threads);"
        <<             "if (nanos_err != NANOS_OK)"
        <<                 "nanos_handle_error(nanos_err);"
        <<             struct_arg_type_name << " *ol_args = (" << struct_arg_type_name <<"*) 0;"
        <<             const_wd_info
        <<             "nanos_wd_t nanos_wd_ = (nanos_wd_t) 0;"
        <<             dynamic_wd_info

        <<             "static nanos_slicer_t replicate = (nanos_slicer_t)0;"
        <<             "if (replicate == (nanos_slicer_t)0)"
        <<                 "replicate = nanos_find_slicer(\"replicate\");"
        <<             "if (replicate == (nanos_slicer_t)0)"
        <<                 "nanos_handle_error(NANOS_UNIMPLEMENTED);"
        <<             "nanos_err = nanos_create_sliced_wd(&nanos_wd_, "
        <<                                           "nanos_wd_const_data.base.num_devices, nanos_wd_const_data.devices, "
        <<                                           "(size_t)" << struct_size << ",  nanos_wd_const_data.base.data_alignment, "
        <<                                           "(void**)&ol_args, (nanos_wd_t*)0, replicate,"
        <<                                           "&nanos_wd_const_data.base.props, &dyn_props, 0, (nanos_copy_data_t**)0,"
        <<                                           "0, (nanos_region_dimension_internal_t**)0"
        <<                                           ");"
        <<             "if (nanos_err != NANOS_OK)"
        <<                 "nanos_handle_error(nanos_err);"
        <<             statement_placeholder(fill_outline_arguments_tree)
        <<             "nanos_err = nanos_submit(nanos_wd_, 0, ( " << dependence_type << ") 0, (nanos_team_t) 0);"
        <<             "if (nanos_err != NANOS_OK)"
        <<                 "nanos_handle_error(nanos_err);"
        <<             "nanos_err = nanos_free(" << as_symbol(slicer_descriptor) << "->threads);"
        <<             "if (nanos_err != NANOS_OK)"
        <<                 "nanos_handle_error(nanos_err);"
        <<         "}"
        <<     "}"
        <<     immediate_decl
        <<     statement_placeholder(fill_immediate_arguments_tree)
        /*      FIXME: This call always be always to smp outline function? */
        <<     "smp_" << outline_name << "(imm_args);"
        << "}"
        ;

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
            fill_immediate_arguments_tree.retrieve_context().get_decl_context();
        ::insert_entry(fill_immediate_arguments_tree.retrieve_context().get_decl_context()->current_scope,
                slicer_descriptor.get_internal_symbol());

        if (!fill_outline_arguments.empty())
        {
            Nodecl::NodeclBase new_tree = fill_outline_arguments.parse_statement(fill_outline_arguments_tree);
            fill_outline_arguments_tree.replace(new_tree);
        }

        if (!fill_immediate_arguments.empty())
        {
            Nodecl::NodeclBase new_tree = fill_immediate_arguments.parse_statement(fill_immediate_arguments_tree);
            fill_immediate_arguments_tree.replace(new_tree);
        }

        construct.replace(spawn_code_tree);
    }

} }
