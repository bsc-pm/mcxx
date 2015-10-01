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


#include "tl-lowering-visitor.hpp"
#include "tl-outline-info.hpp"
#include "tl-predicateutils.hpp"
#include "tl-nanos.hpp"
#include "cxx-diagnostic.h"
#include "fortran03-typeutils.h"

namespace TL { namespace Nanox {

    TL::Symbol LoweringVisitor::create_reduction_function_fortran_slicer(OutlineDataItem* ol, Nodecl::NodeclBase construct)
    {
        OpenMP::Reduction *red = ol->get_reduction_info().first;

        reduction_map_t::iterator it = _reduction_map_ompss.find(red);
        if (it != _reduction_map_ompss.end())
        {
            return it->second;
        }

        std::string fun_name;
        {
            std::stringstream ss;
            ss << "nanos_red_s_" << red << "_" << simple_hash_str(construct.get_filename().c_str());
            fun_name = ss.str();
        }

        Nodecl::NodeclBase function_body;
        Source src;

        // We will use ELEMENTAL routines here
        src << "ELEMENTAL SUBROUTINE " << fun_name << "(omp_out, omp_in)\n"
            <<    "IMPLICIT NONE\n"
            <<    as_type(red->get_type()) << ", INTENT(INOUT) :: omp_out\n" 
            <<    as_type(red->get_type()) << ", INTENT(IN) :: omp_in\n"
            <<    statement_placeholder(function_body) << "\n"
            << "END SUBROUTINE " << fun_name << "\n";
        ;

        Nodecl::NodeclBase function_code = src.parse_global(construct);

        TL::Scope inside_function = ReferenceScope(function_body).get_scope();
        TL::Symbol param_omp_in = inside_function.get_symbol_from_name("omp_in");
        ERROR_CONDITION(!param_omp_in.is_valid(), "Symbol omp_in not found", 0);
        TL::Symbol param_omp_out = inside_function.get_symbol_from_name("omp_out");
        ERROR_CONDITION(!param_omp_out.is_valid(), "Symbol omp_out not found", 0);

        TL::Symbol function_sym = inside_function.get_symbol_from_name(fun_name);
        ERROR_CONDITION(!function_sym.is_valid(), "Symbol %s not found", fun_name.c_str());

        Nodecl::Utils::SimpleSymbolMap symbol_map;
        symbol_map.add_map(red->get_omp_in(), param_omp_in);
        symbol_map.add_map(red->get_omp_out(), param_omp_out);

        function_body.replace(
                Nodecl::ExpressionStatement::make(
                    Nodecl::Utils::deep_copy(
                        red->get_combiner(),
                        inside_function,
                        symbol_map)));

        Nodecl::Utils::append_to_enclosing_top_level_location(construct, function_code);

        _reduction_map_ompss[red] = function_sym;

        return function_sym;
    }

    TL::Symbol LoweringVisitor::create_reduction_function_slicer(OutlineDataItem* red, Nodecl::NodeclBase construct)
    {
        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
        {
            return create_basic_reduction_function_c(red->get_reduction_info().first, construct);
        }
        else if (IS_FORTRAN_LANGUAGE)
        {
            // We need a slightly different one for Fortran
            return create_reduction_function_fortran_slicer(red, construct);
        }
        else
        {
            internal_error("Code unreachable", 0);
        }
    }

    void LoweringVisitor::reduction_initialization_code_slicer(
            OutlineInfo& outline_info,
            Nodecl::NodeclBase ref_tree,
            Nodecl::NodeclBase construct)
    {
        ERROR_CONDITION(ref_tree.is_null(), "Invalid tree", 0);

        if (!Nanos::Version::interface_is_at_least("master", 5023))
        {
            fatal_printf_at(construct.get_locus(),
                    "a newer version of Nanos++ (>=5023) is required for reductions support\n");
        }

        TL::ObjectList<OutlineDataItem*> reduction_items = outline_info.get_data_items().filter(
                lift_pointer<bool, OutlineDataItem>(&OutlineDataItem::is_reduction));
        ERROR_CONDITION (reduction_items.empty(), "No reductions to process", 0);

        for (TL::ObjectList<OutlineDataItem*>::iterator it = reduction_items.begin();
                it != reduction_items.end();
                it++)
        {
            TL::Symbol basic_reduction_function = create_reduction_function_slicer(*it, construct);
            (*it)->reduction_set_basic_function(basic_reduction_function);
        }
    }

    void LoweringVisitor::perform_partial_reduction_slicer(OutlineInfo& outline_info,
            Nodecl::NodeclBase ref_tree,
            Nodecl::Utils::SimpleSymbolMap*& symbol_map)
    {
        ERROR_CONDITION(ref_tree.is_null(), "Invalid tree", 0);

        TL::ObjectList<OutlineDataItem*> reduction_items = outline_info.get_data_items().filter(
               lift_pointer<bool, OutlineDataItem>(&OutlineDataItem::is_reduction));
        if (!reduction_items.empty())
        {
            TL::ObjectList<Nodecl::NodeclBase> reduction_stmts;

            Nodecl::Utils::SimpleSymbolMap* simple_symbol_map = new Nodecl::Utils::SimpleSymbolMap(symbol_map);
            symbol_map = simple_symbol_map;

            for (TL::ObjectList<OutlineDataItem*>::iterator it = reduction_items.begin();
                    it != reduction_items.end();
                    it++)
            {
                scope_entry_t* shared_symbol = (*it)->get_symbol().get_internal_symbol();

                // We need this to avoid the original symbol be replaced
                // incorrectly
                scope_entry_t* shared_symbol_proxy = NEW0(scope_entry_t);
                shared_symbol_proxy->symbol_name = UNIQUESTR_LITERAL("<<reduction-variable>>"); // Crude way to ensure it is replaced
                shared_symbol_proxy->kind = shared_symbol->kind;
                symbol_entity_specs_copy_from(shared_symbol_proxy, shared_symbol);
                shared_symbol_proxy->decl_context = shared_symbol->decl_context;
                shared_symbol_proxy->type_information = shared_symbol->type_information;
                shared_symbol_proxy->locus = shared_symbol->locus;

                simple_symbol_map->add_map( shared_symbol_proxy,
                        (*it)->reduction_get_shared_symbol_in_outline() );

                Source reduction_code;
                Nodecl::NodeclBase partial_reduction_code;
                reduction_code
                    << "{"
                    << "nanos_lock_t* red_lock;"
                    << "nanos_err_t nanos_err;"
                    << "nanos_err = nanos_get_lock_address("
                    <<       ((*it)->get_private_type().is_array() ? "" : "&")
                    <<             as_symbol( shared_symbol_proxy ) << ", &red_lock);"
                    << "if (nanos_err != NANOS_OK) nanos_handle_error(nanos_err);"

                    << "nanos_err = nanos_set_lock(red_lock);"
                    << "if (nanos_err != NANOS_OK) nanos_handle_error(nanos_err);"
                    << statement_placeholder(partial_reduction_code)
                    << "nanos_err = nanos_unset_lock(red_lock);"
                    << "if (nanos_err != NANOS_OK) nanos_handle_error(nanos_err);"
                    << "}"
                    ;

                FORTRAN_LANGUAGE()
                {
                    Source::source_language = SourceLanguage::C;
                }
                Nodecl::NodeclBase statement = reduction_code.parse_statement(ref_tree);
                FORTRAN_LANGUAGE()
                {
                    Source::source_language = SourceLanguage::Current;
                }

                ERROR_CONDITION(!statement.is<Nodecl::List>(), "Expecting a list", 0);
                reduction_stmts.append(statement.as<Nodecl::List>()[0]);

                TL::Type elemental_type = (*it)->get_private_type();
                while (elemental_type.is_array())
                    elemental_type = elemental_type.array_element();

                Source partial_reduction_code_src;
                if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
                {
                    partial_reduction_code_src
                        << as_symbol( (*it)->reduction_get_basic_function() ) << "("
                        // This will be the reduction shared
                        <<       ((*it)->get_private_type().is_array() ? "" : "&")
                        <<       as_symbol( shared_symbol_proxy ) << ", "
                        // This will be the reduction private var
                        <<       ((*it)->get_private_type().is_array() ? "" : "&")
                        <<       as_symbol( (*it)->get_symbol() ) << ", "
                        <<    ((*it)->get_private_type().is_array() ?
                               (
                                  "sizeof(" + as_type( (*it)->get_private_type()) + ")"
                                   "/ sizeof(" + as_type(elemental_type) + ")"
                                )
                                : "1")
                        << ");"
                        ;

                }
                else if (IS_FORTRAN_LANGUAGE)
                {
                    // We use an ELEMENTAL call here
                    partial_reduction_code_src
                        << "CALL " << as_symbol ( (*it)->reduction_get_basic_function() ) << "("
                        // This will be the reduction shared
                        <<    as_symbol( shared_symbol_proxy ) << ", "
                        // This will be the reduction private var
                        <<    as_symbol( (*it)->get_symbol() )
                        << ")"
                        ;
                }
                else
                {
                    internal_error("Code unreachable", 0);
                }

                partial_reduction_code.replace(
                        partial_reduction_code_src.parse_statement(partial_reduction_code));
            }
            ref_tree.replace(
                    Nodecl::CompoundStatement::make(
                        Nodecl::List::make(reduction_stmts),
                        Nodecl::NodeclBase::null()
                        )
                    );
        }

    }
} }
