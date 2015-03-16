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
#include "tl-nanos.hpp"
#include "tl-lowering-visitor.hpp"

namespace TL { namespace Nanox {

    void LoweringVisitor::parallel_spawn(
            OutlineInfo& outline_info,
            Nodecl::NodeclBase construct,
            Nodecl::NodeclBase num_replicas,
            Nodecl::NodeclBase if_condition,
            const std::string& outline_name,
            TL::Symbol structure_symbol,
            Nodecl::NodeclBase task_label)
    {
        Source nanos_create_wd,
        nanos_create_wd_and_run,
        immediate_decl;

        Symbol current_function = Nodecl::Utils::get_enclosing_function(construct);

        Nodecl::NodeclBase code = current_function.get_function_code();

        Nodecl::Context context = (code.is<Nodecl::TemplateFunctionCode>())
            ? code.as<Nodecl::TemplateFunctionCode>().get_statements().as<Nodecl::Context>()
            : code.as<Nodecl::FunctionCode>().get_statements().as<Nodecl::Context>();

        TL::Scope function_scope = context.retrieve_context();
        Source struct_arg_type_name;

        struct_arg_type_name
            << ((structure_symbol.get_type().is_template_specialized_type()
                        &&  structure_symbol.get_type().is_dependent()) ? "typename " : "")
            << structure_symbol.get_qualified_name(function_scope);

        Source struct_size;
        Source dynamic_size;
        struct_size << "sizeof(imm_args)" << dynamic_size;

        // Fill argument structure
        allocate_immediate_structure(
                outline_info,
                struct_arg_type_name,
                struct_size,
                // out
                immediate_decl,
                dynamic_size);

        TL::Symbol xlate_function_symbol;

        TL::Source translation_fun_arg_name;
        translation_fun_arg_name << "(nanos_translate_args_t)0";

        Source copy_ol_decl,
               copy_ol_arg, 
               copy_ol_setup,
               copy_imm_arg,
               copy_imm_setup;

        Source num_dependences;

        nanos_create_wd
            << "nanos_create_wd_compact("
            <<       "&nanos_wd_, "
            <<       "&nanos_wd_const_data.base, "
            <<       "&dyn_props, "
            <<       struct_size << ", "
            <<       "(void**)&ol_args, "
            <<       "nanos_current_wd(), "
            <<       copy_ol_arg << ");"
            ;

        nanos_create_wd_and_run
            << "nanos_create_wd_and_run_compact("
            <<       "&nanos_wd_const_data.base, "
            <<       "&dyn_props, "
            <<       struct_size << ", "
            <<       "&imm_args,"
            <<       num_dependences << ", "
            <<       "dependences, "
            <<       copy_imm_arg << ", "
            <<       translation_fun_arg_name << ");"
            ;

        std::string wd_description;
        if (!task_label.is_null())
        {
            wd_description = task_label.get_text();
        }
        else
        {
            wd_description = current_function.get_name();
        }

        Source const_wd_info;
        const_wd_info << fill_const_wd_info(struct_arg_type_name,
                /* is_untied */ false,
                /* mandatory_creation */ true,
                /* is_function_task */ false,
                wd_description,
                outline_info,
                construct);

        Source num_threads, if_condition_code_opt;
        if (Nanos::Version::interface_is_at_least("openmp", 7))
        {
            num_threads << "nanos_omp_get_num_threads_next_parallel("
                << (num_replicas.is_null() ? "0" : as_expression(num_replicas))
                << ")";
        }
        else
        {
            if (num_replicas.is_null())
            {
                num_threads << "nanos_omp_get_max_threads()";
            }
            else
            {
                num_threads << as_expression(num_replicas);
            }
        }

        if (!if_condition.is_null())
        {
            // Do not remove the extra parenthesis (#2281)
            if_condition_code_opt << "if (!(" << as_expression(if_condition) << "))  nanos_num_threads = 1;";
        }

        Nodecl::NodeclBase fill_outline_arguments_tree,
            fill_dependences_outline_tree;
        Source fill_outline_arguments;

        Nodecl::NodeclBase fill_immediate_arguments_tree,
            fill_dependences_immediate_tree;
        Source fill_immediate_arguments;


        Source dependence_type, dependences_info;
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
                    << "dyn_props.flags.is_implicit = 1;"
                    ;
        }

        TL::Source extra_arg_nanos_create_team;
        if (Nanos::Version::interface_is_at_least("master", 5027))
        {
            extra_arg_nanos_create_team << ", &nanos_wd_const_data.base";
        }

        Source spawn_code;
        spawn_code
            << "{"
            <<   const_wd_info
            <<   immediate_decl
            <<   "unsigned int nanos_num_threads = " << num_threads << ";"
            <<   if_condition_code_opt
            <<   "nanos_err_t nanos_err;"
            <<   "nanos_team_t nanos_team = (nanos_team_t)0;"
            <<   "nanos_thread_t nanos_team_threads[nanos_num_threads];"
            <<   "nanos_err = nanos_create_team(&nanos_team, (nanos_sched_t)0, &nanos_num_threads,"
            <<              "(nanos_constraint_t*)0, /* reuse_current */ 1, nanos_team_threads"
            <<              extra_arg_nanos_create_team << ");"
            <<   "if (nanos_err != NANOS_OK) nanos_handle_error(nanos_err);"
            <<   dynamic_wd_info
            <<   dependences_info
            <<   "unsigned int nth_i;"
            <<   "for (nth_i = 1; nth_i < nanos_num_threads; nth_i = nth_i + 1)"
            <<   "{"
            //   We have to create a nanos_wd_ tied to a thread
            <<      "dyn_props.tie_to = nanos_team_threads[nth_i];"
            <<      struct_arg_type_name << " *ol_args = 0;"
            <<      "nanos_wd_t nanos_wd_ = (nanos_wd_t)0;"
            <<      copy_ol_decl
            <<      "nanos_err = " << nanos_create_wd
            <<      "if (nanos_err != NANOS_OK) nanos_handle_error(nanos_err);"
            // This is a placeholder because arguments are filled using the base language (possibly Fortran)
            <<      statement_placeholder(fill_outline_arguments_tree)
            <<      copy_ol_setup
            <<      "nanos_err = nanos_submit(nanos_wd_, 0, (" <<  dependence_type << ") 0, (nanos_team_t)0);"
            <<      "if (nanos_err != NANOS_OK) nanos_handle_error(nanos_err);"
            <<   "}"
            <<   "dyn_props.tie_to = nanos_team_threads[0];"
            // This is a placeholder because arguments are filled using the base language (possibly Fortran)
            <<   statement_placeholder(fill_immediate_arguments_tree)
            <<   copy_imm_setup
            <<   "nanos_err = " << nanos_create_wd_and_run
            <<   "if (nanos_err != NANOS_OK) nanos_handle_error(nanos_err);"
            <<   "nanos_err = nanos_end_team(nanos_team);"
            <<   "if (nanos_err != NANOS_OK) nanos_handle_error(nanos_err);"
            << "}"
            ;

        fill_arguments(construct, outline_info, fill_outline_arguments, fill_immediate_arguments);

        // Fill dependences for outline
        int num_static_dependences, num_dynamic_dependences;
        count_dependences(outline_info, num_static_dependences, num_dynamic_dependences);
        if (num_dynamic_dependences != 0)
        {
            internal_error("Not yet implemented", 0);
        }
        else
        {
            num_dependences << num_static_dependences;
        }

        Source num_copies;
        num_copies << 0;
        fill_copies(construct,
                outline_info,
                /* parameter_outline_info */ NULL,
                structure_symbol,

                num_copies,
                copy_ol_decl,
                copy_ol_arg,
                copy_ol_setup,
                copy_imm_arg,
                copy_imm_setup,
                xlate_function_symbol);

        fill_dependences(construct,
                outline_info,
                num_static_dependences,
                num_dynamic_dependences,
                num_dependences,
                dependences_info);

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
