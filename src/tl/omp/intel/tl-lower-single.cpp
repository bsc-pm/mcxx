/*--------------------------------------------------------------------
  (C) Copyright 2006-2014 Barcelona Supercomputing Center
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
#include "tl-lowering-utils.hpp"
#include "tl-nodecl-utils.hpp"
#include "tl-counters.hpp"

namespace TL { namespace Intel {

void LoweringVisitor::visit(const Nodecl::OpenMP::Single& construct)
{
    Nodecl::NodeclBase statements = construct.get_statements();
    walk(statements);
    statements = construct.get_statements(); // Should not be necessary

    Nodecl::List environment = construct.get_environment().as<Nodecl::List>();

    Nodecl::OpenMP::Private private_ = environment.find_first<Nodecl::OpenMP::Private>();
    Nodecl::OpenMP::Firstprivate firstprivate = environment.find_first<Nodecl::OpenMP::Firstprivate>();
    Nodecl::OpenMP::BarrierAtEnd barrier_at_end = environment.find_first<Nodecl::OpenMP::BarrierAtEnd>();

    Source single_code, barrier_source;
    Nodecl::NodeclBase stmt_placeholder;

    TL::Symbol ident_symbol = Intel::new_global_ident_symbol(construct);

    single_code
        << "{"
        <<     "if (__kmpc_single(&" << as_symbol(ident_symbol)
        <<                       ", __kmpc_global_thread_num(&" << as_symbol(ident_symbol) << ")))"
        <<     "{"
        <<        statement_placeholder(stmt_placeholder)
        <<     "}"
        <<     barrier_source
        << "}"
        ;

    TL::ObjectList<TL::Symbol> private_symbols;
    TL::ObjectList<TL::Symbol> firstprivate_symbols;

    if (!private_.is_null())
    {
        private_symbols.insert(private_
                .get_symbols()
                .as<Nodecl::List>()
                .to_object_list()
                .map<TL::Symbol>(&Nodecl::NodeclBase::get_symbol));
    }
    if (!firstprivate.is_null())
    {
        private_symbols.insert(firstprivate
                .get_symbols()
                .as<Nodecl::List>()
                .to_object_list()
                .map<TL::Symbol>(&Nodecl::NodeclBase::get_symbol));
        firstprivate_symbols.insert(firstprivate
                .get_symbols()
                .as<Nodecl::List>()
                .to_object_list()
                .map<TL::Symbol>(&Nodecl::NodeclBase::get_symbol));
    }

    if (!barrier_at_end.is_null())
    {
        barrier_source
            << "__kmpc_barrier(&" << as_symbol(ident_symbol)
            <<                ", __kmpc_global_thread_num(&" << as_symbol(ident_symbol) << "));"
            ;
    }

    Nodecl::NodeclBase single_tree = single_code.parse_statement(construct);

    Nodecl::Utils::SimpleSymbolMap symbol_map;
    TL::Counter &private_num = TL::CounterManager::get_counter("intel-omp-privates");

    TL::Scope block_scope = stmt_placeholder.retrieve_context();
    for (TL::ObjectList<TL::Symbol>::iterator it = private_symbols.begin();
            it != private_symbols.end();
            it++)
    {
        std::stringstream new_name;
        new_name << "p_" << it->get_name() << (int)private_num;
        private_num++;

        TL::Symbol new_private_sym = Intel::new_private_symbol(*it, block_scope);

        symbol_map.add_map(*it, new_private_sym);

        if (firstprivate_symbols.contains(*it))
        {
            if (!new_private_sym.get_type().is_array())
            {
                new_private_sym.set_value(it->make_nodecl(/* set_ref_type */ true));
            }
            else
            {
                Source init_array;

                // FIXME - Use assignment instead
                init_array
                    << "__builtin_memcpy(" << as_symbol(new_private_sym) << ","
                    <<                        as_symbol(*it)
                    <<                        ", sizeof(" << as_symbol(*it) << "));"
                    ;

                Nodecl::NodeclBase init_array_tree = init_array.parse_statement(stmt_placeholder);
                stmt_placeholder.prepend_sibling(init_array_tree);
            }
        }
    }

    Nodecl::NodeclBase single_body = Nodecl::Utils::deep_copy(statements,
            stmt_placeholder,
            symbol_map);

    stmt_placeholder.replace(single_body);

    construct.replace(single_tree);
}

} }
