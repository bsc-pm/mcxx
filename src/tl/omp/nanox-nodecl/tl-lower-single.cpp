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

namespace TL { namespace Nanox {

    void LoweringVisitor::visit(const Nodecl::OpenMP::Single& construct)
    {
        Nodecl::List environment = construct.get_environment().as<Nodecl::List>();
        Nodecl::NodeclBase statements = construct.get_statements();

        walk(statements);

        statements = construct.get_statements();

        Nodecl::NodeclBase placeholder;

        Nodecl::Utils::SimpleSymbolMap symbol_map;

        TL::Source transform_code, final_barrier;
        transform_code
            << "{"
            << as_type(::get_bool_type()) << " single_guard;"
            << "nanos_err_t nanos_err = nanos_omp_single(&single_guard);"
            << "if (nanos_err != NANOS_OK) nanos_handle_error(nanos_err);"

            << "if (single_guard)"
            << "{"
            << statement_placeholder(placeholder)
            << "}"
            << "}"
            << final_barrier
            ;

        if (!environment.find_first<Nodecl::OpenMP::BarrierAtEnd>().is_null())
        {
            final_barrier << full_barrier_source();
        }

        FORTRAN_LANGUAGE()
        {
            // Parse in C
            Source::source_language = SourceLanguage::C;
        }
        Nodecl::NodeclBase n = transform_code.parse_statement(construct);
        FORTRAN_LANGUAGE()
        {
            Source::source_language = SourceLanguage::Current;
        }
        placeholder.set_locus(construct.get_locus());

        // Create the environment
        Nodecl::NodeclBase private_syms = environment.find_first<Nodecl::OpenMP::Private>();
        Nodecl::NodeclBase firstprivate_syms = environment.find_first<Nodecl::OpenMP::Firstprivate>();

        TL::Scope placeholder_scope = placeholder.retrieve_context();
        if (!private_syms.is_null())
        {
            Nodecl::List private_items = private_syms
                .as<Nodecl::OpenMP::Private>()
                .get_symbols()
                .as<Nodecl::List>();

            for (Nodecl::List::iterator it = private_items.begin();
                    it != private_items.end();
                    it++)
            {
                TL::Symbol orig_sym = it->get_symbol();
                ERROR_CONDITION(!orig_sym.is_valid(), "Invalid symbol", 0);

                // FIXME - Improve the naming scheme
                TL::Symbol sym = placeholder_scope.new_symbol("sp_" + orig_sym.get_name());
                sym.get_internal_symbol()->kind = SK_VARIABLE;
                sym.get_internal_symbol()->type_information = orig_sym.get_type().no_ref().get_internal_type();
                symbol_entity_specs_set_is_user_declared(sym.get_internal_symbol(), 1);

                symbol_map.add_map(orig_sym, sym);

                if (IS_CXX_LANGUAGE)
                {
                    Nodecl::NodeclBase def = Nodecl::CxxDef::make(Nodecl::NodeclBase::null(), sym);
                    Nodecl::Utils::prepend_items_before(placeholder, def);
                }
            }
        }

        if (!firstprivate_syms.is_null())
        {
            Nodecl::List private_items = firstprivate_syms
                .as<Nodecl::OpenMP::Firstprivate>()
                .get_symbols()
                .as<Nodecl::List>();

            for (Nodecl::List::iterator it = private_items.begin();
                    it != private_items.end();
                    it++)
            {
                TL::Symbol orig_sym = it->get_symbol();
                ERROR_CONDITION(!orig_sym.is_valid(), "Invalid symbol", 0);

                // FIXME - Improve the naming scheme
                TL::Symbol sym = placeholder_scope.new_symbol("sfp_" + orig_sym.get_name());
                sym.get_internal_symbol()->kind = SK_VARIABLE;
                sym.get_internal_symbol()->type_information =
                    orig_sym.get_type().no_ref().get_internal_type();
                symbol_entity_specs_set_is_user_declared(sym.get_internal_symbol(), 1);

                symbol_map.add_map(orig_sym, sym);

                if (IS_CXX_LANGUAGE)
                {
                    Nodecl::NodeclBase def = Nodecl::CxxDef::make(Nodecl::NodeclBase::null(), sym);
                    Nodecl::Utils::prepend_items_before(placeholder, def);
                }

                // Initialization of the private
                if (IS_FORTRAN_LANGUAGE
                        || !sym.get_type().no_ref().is_array())
                {
                    if (!orig_sym.is_saved_expression())
                    {
                        sym.get_internal_symbol()->type_information =
                            get_unqualified_type(sym.get_internal_symbol()->type_information);

                        Nodecl::Symbol sym_ref = sym.make_nodecl();
                        sym_ref.set_type(sym.get_type());

                        if (!sym_ref.get_type().is_any_reference())
                            sym_ref.set_type(sym.get_type().get_lvalue_reference_to());

                        Nodecl::Symbol orig_sym_ref = orig_sym.make_nodecl();
                        orig_sym_ref.set_type(orig_sym.get_type());

                        if (!orig_sym_ref.get_type().is_any_reference())
                            orig_sym_ref.set_type(orig_sym.get_type().get_lvalue_reference_to());

                        Nodecl::NodeclBase assig =
                            Nodecl::ExpressionStatement::make(
                                    Nodecl::Assignment::make(
                                        sym_ref,
                                        orig_sym_ref,
                                        sym_ref.get_type()));

                        Nodecl::Utils::prepend_items_before(placeholder, assig);
                    }
                    else
                    {
                        Nodecl::Symbol orig_sym_ref = orig_sym.make_nodecl();
                        orig_sym_ref.set_type(orig_sym.get_type());

                        if (!orig_sym_ref.get_type().is_any_reference())
                            orig_sym_ref.set_type(orig_sym.get_type().get_lvalue_reference_to());

                        sym.set_value(orig_sym_ref);
                        symbol_entity_specs_set_is_saved_expression(sym.get_internal_symbol(), 1);

                        Nodecl::NodeclBase init =
                            Nodecl::ObjectInit::make(sym);
                        Nodecl::Utils::prepend_items_before(placeholder, init);
                    }
                }
                else // This is not Fortran and the type of the symbol is array
                {
                    sym.get_internal_symbol()->type_information =
                        get_unqualified_type(sym.get_internal_symbol()->type_information);

                    Source src;
                    src << "__builtin_memcpy(" << as_symbol(sym) << ", "
                        << as_symbol(orig_sym) << ", "
                        << "sizeof(" << as_type(orig_sym.get_type().no_ref()) << "));"
                        ;

                    Nodecl::Utils::prepend_items_before(placeholder,
                            src.parse_statement(placeholder_scope));
                }
            }
        }

        Nodecl::NodeclBase copied_statements = Nodecl::Utils::deep_copy(statements, placeholder, symbol_map);
        placeholder.replace(copied_statements);

        construct.replace(n);
    }

} }
