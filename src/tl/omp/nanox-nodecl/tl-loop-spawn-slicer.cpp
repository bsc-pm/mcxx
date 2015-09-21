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
#include "cxx-cexpr.h"

namespace TL { namespace Nanox {

    void LoweringVisitor::loop_spawn_slicer(OutlineInfo& outline_info,
            Nodecl::NodeclBase construct,
            Nodecl::List distribute_environment,
            Nodecl::RangeLoopControl& range,
            const std::string& outline_name,
            TL::Symbol structure_symbol,
            TL::Symbol slicer_descriptor,
            Nodecl::NodeclBase task_label,
            Nodecl::NodeclBase final_clause)
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

        Source immediate_decl_dummy;
        allocate_immediate_structure(
                outline_info,
                struct_arg_type_name,
                struct_size,
                // out
                immediate_decl_dummy,
                dynamic_size);

        Nodecl::NodeclBase fill_outline_arguments_tree, fill_slicer_descriptor_tree;
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
            << "nanos_slicer_t nanos_slicer;"
            << "nanos_slicer = nanos_find_slicer(\"" << slicer_name << "\");"
            << "if (nanos_slicer == 0) nanos_handle_error(NANOS_UNIMPLEMENTED);"
            << "int nanos_chunk = " << (!schedule.get_chunk().is_null() ? as_expression(schedule.get_chunk()) : "1") << ";"
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


        Source dynamic_wd_info;
        dynamic_wd_info
            <<     "nanos_wd_dyn_props_t nanos_dyn_props;"
            <<     "nanos_dyn_props.tie_to = (nanos_thread_t)0;"
            <<     "nanos_dyn_props.priority = 0;"
            ;
        if (!_lowering->final_clause_transformation_disabled()
                && Nanos::Version::interface_is_at_least("master", 5024))
        {
           if (final_clause.is_null())
              final_clause = const_value_to_nodecl(const_value_get_signed_int(0));

           if (IS_FORTRAN_LANGUAGE
                 && !final_clause.is_constant())
           {
              dynamic_wd_info
                 << "if (" << as_expression(final_clause) << ")"
                 << "{"
                 <<      "nanos_dyn_props.flags.is_final = 1;"
                 << "}"
                 << "else"
                 << "{"
                 <<      "nanos_dyn_props.flags.is_final = 0;"
                 << "}"
                 ;
           }
           else
           {
              dynamic_wd_info
                 << "nanos_dyn_props.flags.is_final = " << as_expression(final_clause) << ";"
                 ;
           }
        }

        // Only tasks created in a parallel construct are marked as implicit
        if (Nanos::Version::interface_is_at_least("master", 5029))
        {
            dynamic_wd_info
                << "nanos_dyn_props.flags.is_implicit = 0;"
                ;
        }

        Source spawn_code, barrier_code;
        spawn_code
        << "{"
        <<     "nanos_err_t nanos_err;"
        <<     struct_arg_type_name << "* ol_args;"
        <<     "ol_args = (" << struct_arg_type_name << "*) 0;"
        <<     "nanos_wd_t nanos_wd_ = (nanos_wd_t)0;"
        <<     dynamic_wd_info
        <<     const_wd_info
        <<     schedule_setup
        <<     "nanos_err = nanos_create_sliced_wd(&nanos_wd_, nanos_wd_const_data.base.num_devices, nanos_wd_const_data.devices, "
        <<            "(size_t)" << struct_size << ","
        <<            "nanos_wd_const_data.base.data_alignment,"
        <<            "(void**)&ol_args, nanos_current_wd(), nanos_slicer, &nanos_wd_const_data.base.props, &nanos_dyn_props,"
        <<            "0, 0, 0, 0);"
        <<     "if (nanos_err != NANOS_OK) nanos_handle_error(nanos_err);"
        <<     statement_placeholder(fill_slicer_descriptor_tree)
        <<     statement_placeholder(fill_outline_arguments_tree)
        <<     "nanos_err = nanos_submit(nanos_wd_, 0, 0, 0);"
        <<     "if (nanos_err != NANOS_OK) nanos_handle_error(nanos_err);"
        <<     barrier_code
        << "}"
        ;


        if (!distribute_environment.find_first<Nodecl::OpenMP::BarrierAtEnd>().is_null())
        {
            barrier_code
                << "nanos_err = nanos_wg_wait_completion(nanos_current_wd(), 0);"
                << "if (nanos_err != NANOS_OK) nanos_handle_error(nanos_err);"
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
        ::insert_entry(fill_outline_arguments_tree.retrieve_context().get_decl_context()->current_scope,
                slicer_descriptor.get_internal_symbol());

        Source fill_slicer_descriptor_src, extra_cxx_declarations;
        if (IS_CXX_LANGUAGE)
        {
            extra_cxx_declarations << as_statement(
                    Nodecl::CxxDef::make(
                        /* context */ nodecl_null(),
                        slicer_descriptor,
                        construct.get_locus()));
        }

        fill_slicer_descriptor_src
            << extra_cxx_declarations
            << as_symbol(slicer_descriptor) << ".lower = " << as_expression(lower) << ";"
            << as_symbol(slicer_descriptor) << ".upper = " << as_expression(upper) << ";"
            << as_symbol(slicer_descriptor) << ".step = " << as_expression(step) << ";"
            << as_symbol(slicer_descriptor) << ".chunk = nanos_chunk;"
            ;

        FORTRAN_LANGUAGE()
        {
            // Parse in C
            Source::source_language = SourceLanguage::C;
        }
        Nodecl::NodeclBase fill_slicer_descriptor_new_tree = fill_slicer_descriptor_src.parse_statement(fill_slicer_descriptor_tree);
        FORTRAN_LANGUAGE()
        {
            Source::source_language = SourceLanguage::Current;
        }
        fill_slicer_descriptor_tree.replace(fill_slicer_descriptor_new_tree);


        if (!fill_outline_arguments.empty())
        {
            Nodecl::NodeclBase new_tree = fill_outline_arguments.parse_statement(fill_outline_arguments_tree);
            fill_outline_arguments_tree.replace(new_tree);
        }

        construct.replace(spawn_code_tree);
    }

} }
