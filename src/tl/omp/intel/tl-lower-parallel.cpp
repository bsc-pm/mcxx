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


#include "tl-counters.hpp"
#include "tl-lowering-visitor.hpp"
#include "tl-lowering-utils.hpp"
#include "tl-symbol-utils.hpp"


namespace TL { namespace Intel {

void LoweringVisitor::visit(const Nodecl::OpenMP::Parallel& construct)
{
    Nodecl::NodeclBase statements = construct.get_statements();
    walk(statements);
    statements = construct.get_statements(); // Should not be necessary

    Nodecl::List environment = construct.get_environment().as<Nodecl::List>();

    Nodecl::NodeclBase num_threads = construct.get_num_replicas();

    Nodecl::OpenMP::Shared shared = environment.find_first<Nodecl::OpenMP::Shared>();
    Nodecl::OpenMP::Private private_ = environment.find_first<Nodecl::OpenMP::Private>();
    Nodecl::OpenMP::Firstprivate firstprivate = environment.find_first<Nodecl::OpenMP::Firstprivate>();
    Nodecl::OpenMP::Reduction reduction = environment.find_first<Nodecl::OpenMP::Reduction>();

    ERROR_CONDITION(!reduction.is_null(), "Reductions not yet implemented", 0);

    TL::ObjectList<TL::Symbol> private_symbols;
    TL::ObjectList<TL::Symbol> firstprivate_symbols;
    if (!private_.is_null())
        private_symbols.insert(private_
                .get_private_symbols()
                .as<Nodecl::List>()
                .to_object_list()
                .map(functor(&Nodecl::NodeclBase::get_symbol)));
    if (!firstprivate.is_null())
    {
        private_symbols.insert(firstprivate
                .get_firstprivate_symbols()
                .as<Nodecl::List>()
                .to_object_list()
                .map(functor(&Nodecl::NodeclBase::get_symbol)));
        firstprivate_symbols.insert(firstprivate
                .get_firstprivate_symbols()
                .as<Nodecl::List>()
                .to_object_list()
                .map(functor(&Nodecl::NodeclBase::get_symbol)));
    }

    TL::ObjectList<TL::Symbol> all_shared_symbols; // Set of all references needed in the outline
    TL::ObjectList<TL::Symbol> shared_symbols;

    if (!shared.is_null())
    {
        shared_symbols.insert(shared
                .get_shared_symbols()
                .as<Nodecl::List>()
                .to_object_list()
                .map(functor(&Nodecl::NodeclBase::get_symbol)));
        all_shared_symbols.insert(shared
                .get_shared_symbols()
                .as<Nodecl::List>()
                .to_object_list()
                .map(functor(&Nodecl::NodeclBase::get_symbol)));
    }
    if (!firstprivate.is_null())
        all_shared_symbols.insert(firstprivate
                .get_firstprivate_symbols()
                .as<Nodecl::List>()
                .to_object_list()
                .map(functor(&Nodecl::NodeclBase::get_symbol)));

    TL::Type kmp_int32_type = Source("kmp_int32").parse_c_type_id(construct);
    ERROR_CONDITION(!kmp_int32_type.is_valid(), "Type kmp_int32 not in scope", 0);

    TL::Symbol enclosing_function = Nodecl::Utils::get_enclosing_function(construct);
    std::string outline_function_name;
    {
        TL::Counter &outline_num = TL::CounterManager::get_counter("intel-omp-outline");
        std::stringstream ss;
        ss << "_ol_" << enclosing_function.get_name() << "_" << (int)outline_num;
        outline_function_name = ss.str();
        outline_num++;
    }

    TL::ObjectList<std::string> parameter_names;
    TL::ObjectList<TL::Type> parameter_types;

    parameter_names.append("_global_tid"); parameter_types.append(kmp_int32_type.get_pointer_to());
    parameter_names.append("_bound_tid"); parameter_types.append(kmp_int32_type.get_pointer_to());

    for (TL::ObjectList<TL::Symbol>::iterator it = all_shared_symbols.begin();
            it != all_shared_symbols.end();
            it++)
    {
        parameter_names.append(it->get_name());
        parameter_types.append(it->get_type().no_ref().get_lvalue_reference_to());
    }

    TL::Symbol outline_function = SymbolUtils::new_function_symbol(
            enclosing_function,
            outline_function_name,
            TL::Type::get_void_type(),
            parameter_names,
            parameter_types);

    Nodecl::NodeclBase outline_function_code, outline_function_stmt;
    SymbolUtils::build_empty_body_for_function(outline_function,
            outline_function_code,
            outline_function_stmt);

    Nodecl::Utils::SimpleSymbolMap symbol_map;

    TL::Scope block_scope = outline_function_stmt.retrieve_context();

    for (TL::ObjectList<TL::Symbol>::iterator it = all_shared_symbols.begin();
            it != all_shared_symbols.end();
            it++)
    {
        TL::Symbol parameter = block_scope.get_symbol_from_name(it->get_name());
        ERROR_CONDITION(!parameter.is_valid(), "Invalid symbol", 0);

        symbol_map.add_map(*it, parameter);
    }

    for (TL::ObjectList<TL::Symbol>::iterator it = private_symbols.begin();
            it != private_symbols.end();
            it++)
    {
        TL::Symbol new_private_sym = Intel::new_private_symbol(*it, block_scope);

        if (firstprivate_symbols.contains(*it))
        {
            if (!new_private_sym.get_type().is_array())
            {
                new_private_sym.set_value(
                        symbol_map.map(*it).make_nodecl(/* set_ref_type */ true)
                        );
            }
            else
            {
                Source init_array;

                // FIXME - Use assignment instead
                init_array
                    << "__builtin_memcpy(" << as_symbol(new_private_sym) << ","
                    <<                        as_symbol(symbol_map.map(*it))
                    <<                        ", sizeof(" << as_symbol(*it) << "));"
                    ;

                Nodecl::NodeclBase init_array_tree = init_array.parse_statement(outline_function_stmt);
                outline_function_stmt.prepend_sibling(init_array_tree);
            }
        }

        // Will override firstprivates.
        // Do not move before the if (firstprivate_symbols.contains(*it))
        // or the __builtin_memcpy will not work
        symbol_map.add_map(*it, new_private_sym);
    }

    Nodecl::NodeclBase parallel_body = Nodecl::Utils::deep_copy(statements,
            outline_function_stmt,
            symbol_map);

    outline_function_stmt.prepend_sibling(parallel_body);

    Nodecl::Utils::prepend_to_top_level_nodecl(outline_function_code);

    // Spawn
    TL::Type kmp_micro_type = Source("kmpc_micro").parse_c_type_id(construct);

    TL::Symbol ident_symbol = Intel::new_global_ident_symbol(construct);

    Source fork_call;
    fork_call
        << "__kmpc_fork_call(&" << as_symbol(ident_symbol) << ", "
        <<                           all_shared_symbols.size() << ", (" << as_type(kmp_micro_type) << ")"
        <<                           as_symbol(outline_function)
        ;
    for (TL::ObjectList<TL::Symbol>::iterator it = all_shared_symbols.begin();
            it != all_shared_symbols.end();
            it++)
    {
        if (!it->get_type().is_array())
        {
            fork_call << ", &" << as_symbol(*it);
        }
        else
        {
            fork_call << ", " << as_symbol(*it);
        }
    }
    fork_call << ");";

    Nodecl::NodeclBase fork_call_tree = fork_call.parse_statement(construct);

    construct.replace(fork_call_tree);
}

} }
