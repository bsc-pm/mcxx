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


#include "tl-lowering-visitor.hpp"
#include "tl-outline-info.hpp"
#include "tl-predicateutils.hpp"
#include "tl-nanos.hpp"
#include "cxx-diagnostic.h"

namespace TL { namespace Nanox {

    TL::Symbol LoweringVisitor::create_reduction_function(OpenMP::Reduction* red, Nodecl::NodeclBase construct)
    {
        reduction_map_t::iterator it = _reduction_map.find(red);
        if (it != _reduction_map.end())
        {
            return it->second;
        }

        std::string fun_name;
        {
            std::stringstream ss;
            ss << "nanos_red_" << red << "_" << simple_hash_str(construct.get_filename().c_str());
            fun_name = ss.str();
        }

        Nodecl::NodeclBase function_body;
        Source src;
        src << "void " << fun_name << "(" << as_type(red->get_type()) << "@ref@ omp_out, " << as_type(red->get_type()) << "@ref@ omp_in)"
            << "{"
            << statement_placeholder(function_body)
            << "}"
            ;

        FORTRAN_LANGUAGE()
        {
            Source::source_language = SourceLanguage::C;
        }
        Nodecl::NodeclBase function_code = src.parse_global(construct);
        FORTRAN_LANGUAGE()
        {
            Source::source_language = SourceLanguage::Current;
        }

        Nodecl::Utils::SimpleSymbolMap symbol_map;

        TL::Scope inside_function = ReferenceScope(function_body).get_scope();
        TL::Symbol param_omp_in = inside_function.get_symbol_from_name("omp_in");
        ERROR_CONDITION(!param_omp_in.is_valid(), "Symbol omp_in not found", 0);
        TL::Symbol param_omp_out = inside_function.get_symbol_from_name("omp_out");
        ERROR_CONDITION(!param_omp_out.is_valid(), "Symbol omp_out not found", 0);

        TL::Symbol function_sym = inside_function.get_symbol_from_name(fun_name);
        ERROR_CONDITION(!function_sym.is_valid(), "Symbol %s not found", fun_name.c_str());

        symbol_map.add_map(red->get_omp_in(), param_omp_in);
        symbol_map.add_map(red->get_omp_out(), param_omp_out);

        function_body.replace(
                Nodecl::ExpressionStatement::make(
                    Nodecl::Utils::deep_copy(red->get_combiner(), inside_function, symbol_map)));

        _reduction_map[red] = function_sym;

        Nodecl::Utils::append_to_enclosing_top_level_location(construct, function_code);

        return function_sym;
    }

    TL::Symbol LoweringVisitor::create_reduction_cleanup_function(OpenMP::Reduction* red, Nodecl::NodeclBase construct)
    {
        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
        {
            internal_error("Currently only valid in Fortran", 0);
        }
        reduction_map_t::iterator it = _reduction_cleanup_map.find(red);
        if (it != _reduction_cleanup_map.end())
        {
            return it->second;
        }

        std::string fun_name;
        {
            std::stringstream ss;
            ss << "nanos_cleanup_" << red << "_" << simple_hash_str(construct.get_filename().c_str());
            fun_name = ss.str();
        }


        Source src;
        src << "SUBROUTINE " << fun_name << "(X)\n"
            <<     as_type(red->get_type()) << ", POINTER ::  X(:)\n"
            <<     "DEALLOCATE(X)\n"
            << "END SUBROUTINE\n"
            ;

        Nodecl::NodeclBase function_code = src.parse_global(construct);

        TL::Symbol function_sym = ReferenceScope(construct).get_scope().get_symbol_from_name(fun_name);
        ERROR_CONDITION(!function_sym.is_valid(), "Symbol %s not found", fun_name.c_str());

        _reduction_cleanup_map[red] = function_sym;

        Nodecl::Utils::append_to_enclosing_top_level_location(construct, function_code);

        return function_sym;
    }

    Source LoweringVisitor::reduction_initialization_code(
            OutlineInfo& outline_info,
            Nodecl::NodeclBase construct)
    {
        if (!Nanos::Version::interface_is_at_least("master", 5021))
        {
            running_error("%s: error: a newer version of Nanos++ (>=5021) is required for reductions support\n",
                    construct.get_locus().c_str());
            return Source();
        }

        TL::ObjectList<OutlineDataItem*> reduction_items = outline_info.get_data_items().filter(
                predicate(lift_pointer(functor(&OutlineDataItem::is_reduction))));
        if (reduction_items.empty())
            return Source();

        Source result;

        Source reduction_declaration,
               thread_initializing_reduction_info,
               thread_fetching_reduction_info;

        result
            << reduction_declaration
            << "{"
            << as_type(get_bool_type()) << " red_single_guard;"
            << "nanos_err_t err;"
            << "err = nanos_enter_sync_init(&red_single_guard);"
            << "if (err != NANOS_OK)"
            <<     "nanos_handle_error(err);"
            << "if (red_single_guard)"
            << "{"
            <<    "int nanos_num_threads = nanos_omp_get_num_threads();"
            <<    thread_initializing_reduction_info
            <<    "err = nanos_release_sync_init();"
            <<    "if (err != NANOS_OK)"
            <<        "nanos_handle_error(err);"
            << "}"
            << "else"
            << "{"
            <<    "err = nanos_wait_sync_init();"
            <<    "if (err != NANOS_OK)"
            <<        "nanos_handle_error(err);"
            <<    thread_fetching_reduction_info
            << "}"
            << "}"
            ;

            for (TL::ObjectList<OutlineDataItem*>::iterator it = reduction_items.begin();
                    it != reduction_items.end();
                    it++)
            {
                std::string nanos_red_name = "nanos_red_" + (*it)->get_symbol().get_name();

                OpenMP::Reduction *reduction = (*it)->get_reduction_info();
                ERROR_CONDITION(reduction == NULL, "Invalid reduction info", 0);

                TL::Type reduction_type = (*it)->get_symbol().get_type();
                if (reduction_type.is_any_reference())
                    reduction_type = reduction_type.references_to();

                reduction_declaration
                    << "nanos_reduction_t* " << nanos_red_name << ";"
                    ;

                Source allocate_private_buffer, cleanup_code;

                TL::Symbol basic_reduction_function = create_reduction_function(reduction, construct);

                thread_initializing_reduction_info
                    << "err = nanos_malloc((void**)&" << nanos_red_name << ", sizeof(nanos_reduction_t), " 
                    << "\"" << construct.get_filename() << "\", " << construct.get_line() << ");"
                    << "if (err != NANOS_OK)"
                    <<     "nanos_handle_error(err);"
                    << nanos_red_name << "->original = (void*)&" << (*it)->get_symbol().get_name() << ";"
                    << allocate_private_buffer
                    << nanos_red_name << "->vop = 0;"
                    << nanos_red_name << "->bop = (void(*)(void*,void*))" << as_symbol(basic_reduction_function) << ";"
                    << nanos_red_name << "->element_size = sizeof(" << as_type(reduction_type) << ");"
                    << cleanup_code
                    << "err = nanos_register_reduction(" << nanos_red_name << ");"
                    << "if (err != NANOS_OK)"
                    <<     "nanos_handle_error(err);"
                    ;

                if (IS_C_LANGUAGE
                        || IS_CXX_LANGUAGE)
                {
                    allocate_private_buffer
                        << "err = nanos_malloc(&" << nanos_red_name << "->privates, sizeof(" << as_type(reduction_type) << ") * nanos_num_threads, "
                        << "\"" << construct.get_filename() << "\", " << construct.get_line() << ");"
                        << "if (err != NANOS_OK)"
                        <<     "nanos_handle_error(err);"
                        << nanos_red_name << "->descriptor = " << nanos_red_name << "->privates;"
                        << "rdv_" << (*it)->get_field_name() << " = (" <<  as_type( (*it)->get_field_type() ) << ")" << nanos_red_name << "->privates;"
                        ;


                    thread_fetching_reduction_info
                        << "err = nanos_reduction_get(&" << nanos_red_name << ", &" << (*it)->get_symbol().get_name() << ");"
                        << "if (err != NANOS_OK)"
                        <<     "nanos_handle_error(err);"
                        << "rdv_" << (*it)->get_field_name() << " = (" <<  as_type( (*it)->get_field_type() ) << ")" << nanos_red_name << "->privates;"
                        ;
                    cleanup_code
                        << nanos_red_name << "->cleanup = nanos_free0;"
                        ;
                }
                else
                {
                    Type private_reduction_vector_type = (*it)->get_field_type();
                    private_reduction_vector_type = private_reduction_vector_type.get_array_to_with_descriptor(
                            Nodecl::NodeclBase::null(),
                            Nodecl::NodeclBase::null(),
                            construct.retrieve_context());
                    private_reduction_vector_type = private_reduction_vector_type.get_pointer_to();

                    allocate_private_buffer
                        << "@FORTRAN_ALLOCATE@((*rdv_" << (*it)->get_field_name() << ")[0:(nanos_num_threads-1)]);"
                        << nanos_red_name << "->privates = &(*rdv_" << (*it)->get_field_name() << ");"
                        // We leak here, this is complicated to fix
                        << "err = nanos_malloc(&" << nanos_red_name << "->descriptor, sizeof(" << as_type(private_reduction_vector_type) << "), "
                        << "\"" << construct.get_filename() << "\", " << construct.get_line() << ");"
                        << "if (err != NANOS_OK)"
                        <<     "nanos_handle_error(err);"
                        << "err = nanos_memcpy(" << nanos_red_name << "->descriptor, "
                            "&rdv_" << (*it)->get_field_name() << ", sizeof(" << as_type(private_reduction_vector_type) << "));"
                        << "if (err != NANOS_OK)"
                        <<     "nanos_handle_error(err);"
                        ;

                    thread_fetching_reduction_info
                        << "err = nanos_reduction_get(&" << nanos_red_name << ", &" << (*it)->get_symbol().get_name() << ");"
                        << "if (err != NANOS_OK)"
                        <<     "nanos_handle_error(err);"
                        << "err = nanos_memcpy("
                            << "&rdv_" << (*it)->get_field_name() << ","
                            << nanos_red_name << "->descriptor, "
                            << "sizeof(" << as_type(private_reduction_vector_type) << "));"
                        << "if (err != NANOS_OK)"
                        <<     "nanos_handle_error(err);"
                        ;

                    TL::Symbol reduction_cleanup = create_reduction_cleanup_function(reduction, construct);
                    cleanup_code
                        << nanos_red_name << "->cleanup = " << as_symbol(reduction_cleanup) << ";"
                        ;
                }

            }

        return result;
    }

#if 0
    void LoweringVisitor::reduction_initialization_code(
            Source max_threads,
            OutlineInfo& outline_info,
            Nodecl::NodeclBase construct,
            // out
            Source &reduction_declaration,
            Source &register_code,
            Source &fill_outline_arguments,
            Source &fill_immediate_arguments)
    {
        TL::ObjectList<OutlineDataItem*> reduction_items = outline_info.get_data_items().filter(
                predicate(lift_pointer(functor(&OutlineDataItem::is_reduction))));

        if (!reduction_items.empty())
        {
            for (TL::ObjectList<OutlineDataItem*>::iterator it = reduction_items.begin();
                    it != reduction_items.end();
                    it++)
            {
                std::string nanos_red_name = "nanos_red_" + (*it)->get_symbol().get_name();

                OpenMP::UDRInfoItem *reduction = (*it)->get_reduction_info();
                ERROR_CONDITION(reduction == NULL, "Invalid reduction info", 0);

                TL::Type reduction_type = (*it)->get_symbol().get_type();
                if (reduction_type.is_any_reference())
                    reduction_type = reduction_type.references_to();

                reduction_declaration
                    << "nanos_reduction_t* " << nanos_red_name << ";"
                    << as_type( (*it)->get_field_type() ) << " rdp_" << (*it)->get_field_name() << ";"
                    ;

                Source allocate_private_buffer, cleanup_code;

                register_code
                    << "err = nanos_malloc((void**)&" << nanos_red_name << ", sizeof(nanos_reduction_t), " 
                    << "\"" << construct.get_filename() << "\", " << construct.get_line() << ");"
                    << "if (err != NANOS_OK)"
                    <<     "nanos_handle_error(err);"
                    << nanos_red_name << "->original = (void*)&" << (*it)->get_symbol().get_name() << ";"
                    << allocate_private_buffer
                    << nanos_red_name << "->vop = 0;"
                    << nanos_red_name << "->bop = " << reduction->get_basic_reductor_function().get_name() << ";"
                    << nanos_red_name << "->element_size = sizeof(" << as_type(reduction_type) << ");"
                    << cleanup_code
                    << "err = nanos_register_reduction(" << nanos_red_name << ");"
                    << "if (err != NANOS_OK)"
                    <<     "nanos_handle_error(err);"
                    ;

                if (IS_C_LANGUAGE
                        || IS_CXX_LANGUAGE)
                {
                    allocate_private_buffer
                        << "err = nanos_malloc(&" << nanos_red_name << "->privates, sizeof(" << as_type(reduction_type) << ") * " << max_threads <<", "
                        << "\"" << construct.get_filename() << "\", " << construct.get_line() << ");"
                        << "if (err != NANOS_OK)"
                        <<     "nanos_handle_error(err);"
                        << "rdp_" << (*it)->get_field_name() << " = (" <<  as_type( (*it)->get_field_type() ) << ")" << nanos_red_name << "->privates;"
                        ;
                    cleanup_code
                        << nanos_red_name << "->cleanup = " << reduction->get_cleanup_function().get_name() << ";"
                        ;

                    fill_outline_arguments
                        << "ol_args->" << (*it)->get_field_name() << " = (" <<  as_type( (*it)->get_field_type() ) << ")" << nanos_red_name << "->privates;"
                        ;
                    fill_immediate_arguments
                        << "imm_args." << (*it)->get_field_name() << " = (" <<  as_type( (*it)->get_field_type() ) << ")" << nanos_red_name << "->privates;"
                        ;
                }
                else
                {
                    allocate_private_buffer
                        // FIXME - We have to do an ALLOCATE but this is C :)
                        << "@FORTRAN_ALLOCATE@((*rdp_" << (*it)->get_field_name() << ")[0:(" << max_threads << "-1)]);"
                        << nanos_red_name << "->privates = &(*rdp_" << (*it)->get_field_name() << ");"
                        << "err = nanos_malloc(&" << nanos_red_name << "->descriptor, sizeof(" << as_type((*it)->get_field_type()) << "), " 
                        << "\"" << construct.get_filename() << "\", " << construct.get_line() << ");"
                        << "if (err != NANOS_OK)"
                        <<     "nanos_handle_error(err);"
                        << "err = nanos_memcpy(" << nanos_red_name << "->descriptor, "
                            "&rdp_" << (*it)->get_field_name() << ", sizeof(" << as_type((*it)->get_field_type()) << "));"
                        << "if (err != NANOS_OK)"
                        <<     "nanos_handle_error(err);"
                        ;

                    cleanup_code
                        << nanos_red_name << "->cleanup = &nanos_reduction_default_cleanup_fortran;"
                        ;

                    fill_outline_arguments
                        << "ol_args->" << (*it)->get_field_name() << " = rdp_" << (*it)->get_field_name() << ";"
                        ;
                    fill_immediate_arguments
                        << "imm_args." << (*it)->get_field_name() << " = rdp_" << (*it)->get_field_name() << ";"
                        ;
                }

            }
        }
    }
#endif

    Source LoweringVisitor::perform_partial_reduction(OutlineInfo& outline_info)
    {
        Source reduction_code;

        TL::ObjectList<OutlineDataItem*> reduction_items = outline_info.get_data_items().filter(
                predicate(lift_pointer(functor(&OutlineDataItem::is_reduction))));
        if (!reduction_items.empty())
        {
            for (TL::ObjectList<OutlineDataItem*>::iterator it = reduction_items.begin();
                    it != reduction_items.end();
                    it++)
            {
                if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
                {
                    reduction_code
                        << "rdv_" << (*it)->get_field_name() << "[nanos_omp_get_thread_num()] = rdp_" << (*it)->get_symbol().get_name() << ";"
                        ;
                }
                else if (IS_FORTRAN_LANGUAGE)
                {
                    reduction_code
                        << "(*rdv_" << (*it)->get_field_name() << ")[nanos_omp_get_thread_num()] = rdp_" << (*it)->get_symbol().get_name() << ";"
                        ;
                }
                else
                {
                    internal_error("Code unreachable", 0);
                }
            }
        }

        return reduction_code;
    }

} }
