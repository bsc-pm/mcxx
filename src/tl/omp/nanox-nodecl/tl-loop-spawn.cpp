
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

    void LoweringVisitor::loop_spawn(OutlineInfo& outline_info, 
            Nodecl::NodeclBase construct, 
            Nodecl::List distribute_environment, 
            Nodecl::List ranges, 
            const std::string& outline_name,
            TL::Symbol structure_symbol)
    {
        if (ranges.size() != 1)
        {
            internal_error("Only ranges of 1 dimension implemented", 0);
        }

        Nodecl::Parallel::Schedule schedule = distribute_environment[0].as<Nodecl::Parallel::Schedule>();
        ERROR_CONDITION(!schedule.is<Nodecl::Parallel::Schedule>(), "Invalid tree", 0);

        Nodecl::Parallel::DistributeRange distribute_range = ranges[0].as<Nodecl::Parallel::DistributeRange>();
        Nodecl::NodeclBase lower = distribute_range.get_lower();
        Nodecl::NodeclBase upper = distribute_range.get_upper();
        Nodecl::NodeclBase step = distribute_range.get_step();

        Source struct_size;
        Source dynamic_size;
        struct_size << "sizeof(imm_args)" << dynamic_size;

        Source struct_arg_type_name = structure_symbol.get_name();


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

        if (schedule.get_text() == "runtime")
        {
            schedule_setup
                <<     "omp_sched_t nanos_runtime_sched;"
                <<     "err = nanos_omp_get_schedule(&nanos_runtime_sched, &nanos_chunk);"
                <<     "if (err != NANOS_OK)"
                <<         "nanos_handle_error(err);"
                <<     "nanos_ws_t *current_ws_policy = nanos_omp_find_worksharing(nanos_runtime_sched);"
                ;
        }
        else
        {
            schedule_setup
                <<     "nanos_ws_t *current_ws_policy = nanos_omp_find_worksharing(omp_sched_" << schedule.get_text() << ");"
                ;
        }

        schedule_setup
            <<     "int nanos_chunk;"
            <<     "nanos_chunk = " << as_expression(schedule.get_chunk()) << ";"
            ;

        Source spawn_code;
        spawn_code 
        << "{"
        <<     immediate_decl
        <<     "_Bool single_guard;"
        <<     "nanos_err_t err;"
        <<     schedule_setup
        <<     "nanos_ws_info_loop_t info_loop;"
        <<     "info_loop.lower_bound = " << as_expression(lower) << ";"
        <<     "info_loop.upper_bound = " << as_expression(upper) << ";"
        <<     "info_loop.loop_step = "   << as_expression(step)  << ";"
        <<     "info_loop.chunk_size = nanos_chunk;"
        <<     "err = nanos_worksharing_create(&imm_args.wsd, *current_ws_policy, (nanos_ws_info_t *) &info_loop, &single_guard);"
        <<     "if (err != NANOS_OK)"
        <<         "nanos_handle_error(err);"
        <<     "if (single_guard)"
        <<     "{"
        <<         "int sup_threads;"
        <<         "err = nanos_team_get_num_supporting_threads(&sup_threads);"
        <<         "if (err != NANOS_OK)"
        <<             "nanos_handle_error(err);"
        // <<         "if (sup_threads > 0)"
        // <<         "{" 
        // <<             "imm_args.wsd->threads = (nanos_thread_t *) __builtin_alloca(sizeof(nanos_thread_t) * sup_threads);"
        // <<             "err = nanos_team_get_supporting_threads(&imm_args.wsd->nths, imm_args.wsd->threads);"
        // <<             "if (err != NANOS_OK)"
        // <<                 "nanos_handle_error(err);"
        // <<             struct_arg_type_name << " *ol_args_im = (" << struct_arg_type_name <<"*) 0;"
        // <<             "nanos_wd_t wd = (nanos_wd_t) 0;"
        // <<             "nanos_wd_props_t props;"
        // <<             "__builtin_memset(&props, 0, sizeof (props));"
        // <<             "props.mandatory_creation = 1;"
        // <<             "props.tied = 1;"
        // <<             "nanos_wd_dyn_props_t dyn_props = {0};"
        // <<             "/* SMP device descriptor */"
        // <<             "static nanos_smp_args_t _ol_f_0_smp_args = {(void (*)(void *)) _smp__ol_f_0};"
        // <<             "nanos_device_t _ol_f_0_devices[] = {{"
        // <<                 "nanos_smp_factory,"
        // <<                 "&_ol_f_0_smp_args"
        // <<             "}};"
        // <<             "static nanos_slicer_t replicate = 0;"
        // <<             "if (!replicate)"
        // <<                 "replicate = nanos_find_slicer("replicate");"
        // <<             "if (replicate == 0)"
        // <<                 "fprintf(stderr, "Cannot find replicate slicer plugin\n");"
        // <<             "err = nanos_create_sliced_wd(&wd, 1, _ol_f_0_devices, sizeof(_nx_data_env_0_t), __alignof__(_nx_data_env_0_t), (void **) &ol_args_im, nanos_current_wd(), replicate, &props, &dyn_props, 0, (nanos_copy_data_t **) 0);"
        // <<             "if (err != NANOS_OK)"
        // <<                 "nanos_handle_error(err);"
        // <<             "ol_args_im->wsd = ol_args.wsd;"
        // <<             "ol_args_im->s_0 = &(s);"
        // <<             "ol_args_im->k_0 = &(k);"
        // <<             "err = nanos_submit(wd, 0, (nanos_dependence_t *) 0, (nanos_team_t) 0);"
        // <<             "if (err != NANOS_OK)"
        // <<                 "nanos_handle_error(err);"
        // <<         "}"
        <<     "}"
        <<     statement_placeholder(fill_immediate_arguments_tree)
        <<     call_outline_function
        << "}"
        ;

        call_outline_function
            << outline_name << "(imm_args);"
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

        if (!fill_immediate_arguments.empty())
        {
            Nodecl::NodeclBase new_tree = fill_immediate_arguments.parse_statement(fill_immediate_arguments_tree);
            fill_immediate_arguments_tree.integrate(new_tree);
        }

        construct.integrate(spawn_code_tree);
    }

} }
