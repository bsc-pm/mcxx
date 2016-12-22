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


#include "tl-counters.hpp"
#include "tl-lowering-visitor.hpp"
#include "tl-lowering-utils.hpp"
#include "tl-symbol-utils.hpp"
#include "tl-lower-reductions.hpp"
#include "cxx-diagnostic.h"


namespace TL { namespace GOMP {

void LoweringVisitor::visit(const Nodecl::OpenMP::Task& construct)
{
    Nodecl::NodeclBase statements = construct.get_statements();
    walk(statements);
    statements = construct.get_statements(); // Should not be necessary

    Nodecl::List environment = construct.get_environment().as<Nodecl::List>();

    TL::ObjectList<Nodecl::OpenMP::Shared> shared_list = environment.find_all<Nodecl::OpenMP::Shared>();
    TL::ObjectList<Nodecl::OpenMP::Private> private_list = environment.find_all<Nodecl::OpenMP::Private>();
    TL::ObjectList<Nodecl::OpenMP::Firstprivate> firstprivate_list = environment.find_all<Nodecl::OpenMP::Firstprivate>();
    TL::ObjectList<Nodecl::OpenMP::Reduction> reduction_list = environment.find_all<Nodecl::OpenMP::Reduction>();

    TL::ObjectList<TL::Symbol> all_symbols_passed; // Set of all symbols passed in the outline
    TL::ObjectList<TL::Symbol> private_symbols;
    TL::ObjectList<TL::Symbol> firstprivate_symbols;
    TL::ObjectList<TL::Symbol> shared_symbols;

    if (!shared_list.empty())
    {
        TL::ObjectList<Symbol> tmp =
            shared_list  // TL::ObjectList<OpenMP::Shared>
            .map<Nodecl::NodeclBase>(&Nodecl::OpenMP::Shared::get_symbols) // TL::ObjectList<Nodecl::NodeclBase>
            .map<Nodecl::List>(&Nodecl::NodeclBase::as<Nodecl::List>) // TL::ObjectList<Nodecl::List>
            .map<TL::ObjectList<Nodecl::NodeclBase> >(&Nodecl::List::to_object_list) // TL::ObjectList<TL::ObjectList<Nodecl::NodeclBase> >
            .reduction(TL::append_two_lists<Nodecl::NodeclBase>) // TL::ObjectList<Nodecl::NodeclBase>
            .map<TL::Symbol>(&Nodecl::NodeclBase::get_symbol) // TL::ObjectList<TL::Symbol>
            ;

        shared_symbols.insert(tmp);
        all_symbols_passed.insert(tmp);
    }
    if (!private_list.empty())
    {
        TL::ObjectList<Symbol> tmp =
            private_list  // TL::ObjectList<OpenMP::Private>
            .map<Nodecl::NodeclBase>(&Nodecl::OpenMP::Private::get_symbols) // TL::ObjectList<Nodecl::NodeclBase>
            .map<Nodecl::List>(&Nodecl::NodeclBase::as<Nodecl::List>) // TL::ObjectList<Nodecl::List>
            .map<TL::ObjectList<Nodecl::NodeclBase> >(&Nodecl::List::to_object_list) // TL::ObjectList<TL::ObjectList<Nodecl::NodeclBase> >
            .reduction(TL::append_two_lists<Nodecl::NodeclBase>) // TL::ObjectList<Nodecl::NodeclBase>
            .map<TL::Symbol>(&Nodecl::NodeclBase::get_symbol) // TL::ObjectList<TL::Symbol>
            ;

        private_symbols.insert(tmp);
    }
    if (!firstprivate_list.empty())
    {
        TL::ObjectList<Symbol> tmp =
            firstprivate_list  // TL::ObjectList<OpenMP::Firstprivate>
            .map<Nodecl::NodeclBase>(&Nodecl::OpenMP::Firstprivate::get_symbols) // TL::ObjectList<Nodecl::NodeclBase>
            .map<Nodecl::List>(&Nodecl::NodeclBase::as<Nodecl::List>) // TL::ObjectList<Nodecl::List>
            .map<TL::ObjectList<Nodecl::NodeclBase> >(&Nodecl::List::to_object_list) // TL::ObjectList<TL::ObjectList<Nodecl::NodeclBase> >
            .reduction(TL::append_two_lists<Nodecl::NodeclBase>) // TL::ObjectList<Nodecl::NodeclBase>
            .map<TL::Symbol>(&Nodecl::NodeclBase::get_symbol) // TL::ObjectList<TL::Symbol>
            ;

        private_symbols.insert(tmp);
        firstprivate_symbols.insert(tmp);
        all_symbols_passed.insert(tmp);
    }

    // Add the VLA symbols
    {
        TL::ObjectList<TL::Symbol> vla_symbols;
        for (TL::ObjectList<TL::Symbol>::iterator it = all_symbols_passed.begin();
                it != all_symbols_passed.end();
                it++)
        {
            GOMP::gather_vla_symbols(*it, vla_symbols);
        }

        // VLA symbols are always firstprivate
        private_symbols.insert(vla_symbols);
        firstprivate_symbols.insert(vla_symbols);

        // We want all the gathered VLA symbols be the first ones
        vla_symbols.insert(all_symbols_passed);
        all_symbols_passed = vla_symbols;
    }

    TL::ObjectList<Nodecl::OpenMP::ReductionItem> reduction_items;
    if (!reduction_list.empty())
    {
        reduction_items = reduction_list
            .map<Nodecl::NodeclBase>(&Nodecl::OpenMP::Reduction::get_reductions)
            .map<Nodecl::List>(&Nodecl::NodeclBase::as<Nodecl::List>)
            .map<TL::ObjectList<Nodecl::NodeclBase> >(&Nodecl::List::to_object_list)
            .reduction((&TL::append_two_lists<Nodecl::NodeclBase>))
            .map<Nodecl::OpenMP::ReductionItem>(&Nodecl::NodeclBase::as<Nodecl::OpenMP::ReductionItem>);
        TL::ObjectList<Symbol> reduction_symbols = reduction_items
            .map<Nodecl::NodeclBase>(&Nodecl::OpenMP::ReductionItem::get_reduced_symbol) // TL::ObjectList<Nodecl::NodeclBase>
            .map<TL::Symbol>(&Nodecl::NodeclBase::get_symbol); // TL::ObjectList<TL::Symbol>
        all_symbols_passed.insert(reduction_symbols);
    }

    TL::Symbol enclosing_function = Nodecl::Utils::get_enclosing_function(construct);
    std::string outline_function_name;
    {
        TL::Counter &outline_num = TL::CounterManager::get_counter("gomp-omp-outline");
        std::stringstream ss;
        ss << "_ol_" << enclosing_function.get_name() << "_" << (int)outline_num;
        outline_function_name = ss.str();
        outline_num++;
    }

    TL::Scope current_scope = construct.retrieve_context();

    TL::Type outline_struct = GOMP::create_outline_struct_task(
            all_symbols_passed,
            firstprivate_symbols,
            enclosing_function,
            construct.get_locus());

    CXX_LANGUAGE()
    {
        Nodecl::Utils::prepend_to_enclosing_top_level_location(
                construct,
                Nodecl::CxxDef::make(
                    /* context */ Nodecl::NodeclBase::null(),
                    outline_struct.get_symbol()
                    )
                );
    }

    std::string ol_data_name;
    {
        TL::Counter &outline_num = TL::CounterManager::get_counter("gomp-omp-outline-data");

        std::stringstream ss;
        ss << "ol_data_" << (int)outline_num;
        ol_data_name = ss.str();

        outline_num++;
    }

    TL::Symbol outline_data = current_scope.new_symbol(ol_data_name);
    outline_data.get_internal_symbol()->kind = SK_VARIABLE;
    outline_data.get_internal_symbol()->type_information = outline_struct.get_internal_type();
    symbol_entity_specs_set_is_user_declared(outline_data.get_internal_symbol(), 1);

    TL::ObjectList<std::string> parameter_names;
    TL::ObjectList<TL::Type> parameter_types;

    parameter_names.append(ol_data_name);
    parameter_types.append(outline_struct.get_pointer_to());

    TL::Symbol outline_function = SymbolUtils::new_function_symbol(
            enclosing_function,
            outline_function_name,
            TL::Type::get_void_type(),
            parameter_names,
            parameter_types);

    Nodecl::NodeclBase outline_function_code, outline_function_stmt;
    Nodecl::List reduction_code_list;
    SymbolUtils::build_empty_body_for_function(outline_function,
            outline_function_code,
            outline_function_stmt);

    Nodecl::Utils::SimpleSymbolMap symbol_map;

    TL::Scope block_scope = outline_function_stmt.retrieve_context();

    TL::Symbol ol_data_in_outline = block_scope.get_symbol_from_name(ol_data_name);
    ERROR_CONDITION(!ol_data_in_outline.is_valid(), "Invalid symbol", 0);

    for (TL::ObjectList<TL::Symbol>::iterator it = all_symbols_passed.begin();
            it != all_symbols_passed.end();
            it++)
    {
        TL::Symbol new_shared_sym = block_scope.new_symbol(it->get_name());
        new_shared_sym.get_internal_symbol()->kind = SK_VARIABLE;
        new_shared_sym.get_internal_symbol()->type_information = lvalue_ref(
                it->get_internal_symbol()->type_information);
        symbol_entity_specs_set_is_user_declared(
                new_shared_sym.get_internal_symbol(),
                1);

        Source init_ref_src;
        if (firstprivate_symbols.contains(*it))
            init_ref_src << as_symbol(ol_data_in_outline) << "->" << it->get_name();
        else
            init_ref_src << "*(" << as_symbol(ol_data_in_outline) << "->" << it->get_name() << ")";
        Nodecl::NodeclBase init_ref =
            init_ref_src.parse_expression(block_scope);
        new_shared_sym.set_value(init_ref);

        symbol_map.add_map(*it, new_shared_sym);

        CXX_LANGUAGE()
        {
            outline_function_stmt.prepend_sibling(
                    Nodecl::CxxDef::make(
                        /* context */ Nodecl::NodeclBase::null(),
                        new_shared_sym));
        }
    }

    for (TL::ObjectList<TL::Symbol>::iterator it = private_symbols.begin();
            it != private_symbols.end();
            it++)
    {
        // They are to be found in the struct
        if (firstprivate_symbols.contains(*it))
            continue;

        TL::Symbol new_private_sym = GOMP::new_private_symbol(*it, block_scope);

        new_private_sym.get_internal_symbol()->type_information = ::type_deep_copy(
                new_private_sym.get_internal_symbol()->type_information,
                outline_function_stmt.retrieve_context().get_decl_context(),
                symbol_map.get_symbol_map());

        symbol_map.add_map(*it, new_private_sym);

        CXX_LANGUAGE()
        {
            outline_function_stmt.prepend_sibling(
                    Nodecl::CxxDef::make(
                        /* context */ Nodecl::NodeclBase::null(),
                        new_private_sym));
        }
    }

    Nodecl::NodeclBase parallel_body = Nodecl::Utils::deep_copy(statements,
            outline_function_stmt,
            symbol_map);
    outline_function_stmt.prepend_sibling(parallel_body);
    Nodecl::Utils::prepend_to_enclosing_top_level_location(construct, outline_function_code);


    Source setup_data;
    CXX_LANGUAGE()
    {
        setup_data << as_statement(
                Nodecl::CxxDef::make(
                    /* context */ Nodecl::NodeclBase::null(),
                    outline_data))
            ;
    }

    for (TL::ObjectList<TL::Symbol>::iterator it = all_symbols_passed.begin();
            it != all_symbols_passed.end();
            it++)
    {
        if (firstprivate_symbols.contains(*it))
        {
            setup_data
                << as_symbol(outline_data) << "." << it->get_name() << " = " << as_symbol(*it) << ";"
                ;
        }
        else
        {
            setup_data
                << as_symbol(outline_data) << "." << it->get_name() << " = &" << as_symbol(*it) << ";"
                ;
        }
    }

    Source arg_size, arg_align, if_value, task_flags, copy_function;

    arg_size << outline_struct.get_size();
    arg_align << outline_struct.get_alignment_of();

    Nodecl::OpenMP::If if_clause = environment.find_first<Nodecl::OpenMP::If>();
    if (if_clause.is_null())
    {
        if_value << "0";
    }
    else
    {
        if_value << as_expression(if_clause.get_condition().shallow_copy());
    }

    // Task flags
    std::string task_flags_name;
    {
        TL::Counter &c = TL::CounterManager::get_counter("gomp-omp-task-flags");

        std::stringstream ss;
        ss << "gomp_task_flags_" << (int)c;
        task_flags_name = ss.str();

        c++;
    }

    Source set_task_flags;
    if (!environment.find_first<Nodecl::OpenMP::Untied>().is_null())
    {
        set_task_flags << task_flags_name << " |= GOMP_TASK_UNTIED;"
            ;
    }
    Nodecl::OpenMP::Final final_clause = environment.find_first<Nodecl::OpenMP::Final>();
    if (!final_clause.is_null())
    {
        set_task_flags
            << "if (" << as_expression(final_clause.get_condition().shallow_copy()) << ")"
            <<    task_flags_name << " |= GOMP_TASK_FINAL;" 
            ;
    }
    if (!environment.find_first<Nodecl::OpenMP::Mergeable>().is_null())
    {
        set_task_flags << task_flags_name << " |= GOMP_TASK_MERGEABLE;"
            ;
    }

    Source dependence_addresses;
    if (!environment.find_first<Nodecl::OpenMP::DepIn>().is_null()
            || !environment.find_first<Nodecl::OpenMP::DepOut>().is_null()
            || !environment.find_first<Nodecl::OpenMP::DepInout>().is_null())
    {
        set_task_flags << task_flags_name << " |= GOMP_TASK_DEPEND;";

        TL::Counter &c = TL::CounterManager::get_counter("gomp-omp-task-deps");

        std::stringstream ss;
        ss << "gomp_task_deps_" << (int)c;
        Source deps_buffer_name;
        deps_buffer_name << ss.str();

        Source size, outs, addresses;
        setup_data << "void *" << deps_buffer_name << "[] = {" << size << outs
                   << addresses << "};";
        int num_out_deps = 0;
        int num_in_deps = 0;

        Nodecl::NodeclBase dep_in
            = environment.find_first<Nodecl::OpenMP::DepIn>();
        if (!dep_in.is_null())
            dep_in = dep_in.as<Nodecl::OpenMP::DepIn>().get_exprs();
        Nodecl::NodeclBase dep_out
            = environment.find_first<Nodecl::OpenMP::DepOut>();
        if (!dep_out.is_null())
            dep_out = dep_out.as<Nodecl::OpenMP::DepOut>().get_exprs();
        Nodecl::NodeclBase dep_inout
            = environment.find_first<Nodecl::OpenMP::DepInout>();
        if (!dep_inout.is_null())
            dep_inout
                = dep_inout.as<Nodecl::OpenMP::DepInout>().get_exprs();

        struct all_deps_info_tag
        {
            Nodecl::NodeclBase &deps;
            int &counter;
        } all_deps_info[] = { { dep_out, num_out_deps },
                              { dep_inout, num_out_deps },
                              // The order is important: input
                              // dependences must go the last
                              { dep_in, num_in_deps } };

        for (all_deps_info_tag *p = all_deps_info;
             p < (all_deps_info_tag *)(&all_deps_info + 1);
             p++)
        {
            if (p->deps.is_null())
                continue;

            Nodecl::List l = p->deps.as<Nodecl::List>();

            for (Nodecl::List::iterator it = l.begin(); it != l.end(); it++)
            {
                DataReference data_ref(*it);
                p->counter++;

                addresses << as_expression(
                                 data_ref.get_base_address().shallow_copy())
                          << ",";
            }
        }

        size << "(void*)" << (num_in_deps + num_out_deps) << "U,";
        outs << "(void*)" << num_out_deps << "U,";
        dependence_addresses << deps_buffer_name;
    }
    else
    {
        dependence_addresses << "0";
    }


    // FIXME: we do not use the copy function because it seems to involve
    // another struct type. We do the copy here instead.
    // Check this though
    copy_function << "0";

    Source task_code;
    task_code
        << setup_data
        << "unsigned long " << task_flags_name << " = 0;"
        << set_task_flags
        << "GOMP_task((void(*)(void*))"
        <<     as_symbol(outline_function) << ", &" << as_symbol(outline_data) << ", "
        <<     "(void(*)(void*,void*))" << copy_function << ","
        <<     arg_size << ", "  << arg_align << ", "  << if_value << "," << task_flags_name << ","
        <<     dependence_addresses
        << ");"
        ;

    Nodecl::NodeclBase task_code_tree = task_code.parse_statement(construct);

    construct.replace(task_code_tree);
}

} }
