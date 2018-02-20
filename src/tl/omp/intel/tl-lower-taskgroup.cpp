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

static void create_red_init_func(const Nodecl::OpenMP::Taskgroup& construct,
                                 const TL::Scope& scope,
                                 Nodecl::OpenMP::ReductionItem& red_item,
                                 TL::Symbol& outline_task,
                                 Nodecl::NodeclBase& outline_task_code,
                                 Nodecl::NodeclBase& outline_task_stmt) {

    TL::Symbol reduced_symbol = red_item.get_reduced_symbol().get_symbol();
    TL::Symbol reductor = red_item.get_reductor().get_symbol();
    OpenMP::Reduction* reduction = OpenMP::Reduction::get_reduction_info_from_symbol(reductor);
    Nodecl::NodeclBase init_expr = reduction->get_initializer().shallow_copy();

    TL::Symbol enclosing_function = Nodecl::Utils::get_enclosing_function(construct);

    TL::Counter &red_num = TL::CounterManager::get_counter("intel-omp-task-red-init");
    std::stringstream outline_task_name;
    outline_task_name << "_red_" << reduced_symbol.get_name() << "_init" << "_" << (int)red_num;
    red_num++;

    TL::ObjectList<std::string> parameter_names;
    TL::ObjectList<TL::Type> parameter_types;

    parameter_names.append("_reduce_init");
    if (reduced_symbol.get_type().is_array()) {
        parameter_types.append(reduced_symbol.get_type().array_element().get_unqualified_type().get_pointer_to());
    }
    else {
        parameter_types.append(reduced_symbol.get_type().get_lvalue_reference_to());
    }

    outline_task = SymbolUtils::new_function_symbol(
            enclosing_function,
            outline_task_name.str(),
            TL::Type::get_void_type(),
            parameter_names,
            parameter_types);

    SymbolUtils::build_empty_body_for_function(outline_task,
            outline_task_code,
            outline_task_stmt);

    Source src_red_init_body;
    src_red_init_body
    << "_reduce_init = " << as_expression(init_expr) << ";";
    Nodecl::NodeclBase tree_red_init_body = src_red_init_body.parse_statement(outline_task_stmt);
    outline_task_stmt.prepend_sibling(tree_red_init_body);

    // As the reduction function is needed during the instantiation of
    // the task, this function should be inserted before the construct
    Nodecl::Utils::prepend_to_enclosing_top_level_location(construct, outline_task_code);
}

static void create_red_comb_func(const Nodecl::OpenMP::Taskgroup& construct,
                                 const TL::Scope& scope,
                                 Nodecl::OpenMP::ReductionItem& red_item,
                                 TL::Symbol& outline_task,
                                 Nodecl::NodeclBase& outline_task_code,
                                 Nodecl::NodeclBase& outline_task_stmt) {

    TL::Symbol reduced_symbol = red_item.get_reduced_symbol().get_symbol();
    TL::Symbol reductor = red_item.get_reductor().get_symbol();
    OpenMP::Reduction* reduction = OpenMP::Reduction::get_reduction_info_from_symbol(reductor);

    TL::Symbol enclosing_function = Nodecl::Utils::get_enclosing_function(construct);

    TL::Counter &red_num = TL::CounterManager::get_counter("intel-omp-task-red-comb");
    std::stringstream outline_task_name;
    outline_task_name << "_red_" << reduced_symbol.get_name() << "_comb" << "_" << (int)red_num;
    red_num++;

    TL::ObjectList<std::string> parameter_names;
    TL::ObjectList<TL::Type> parameter_types;

    if (reduced_symbol.get_type().is_array()) {
        parameter_names.append("_shared_data"); parameter_types.append(reduced_symbol.get_type().array_element().get_unqualified_type().get_lvalue_reference_to());
        parameter_names.append("_priv_data"); parameter_types.append(reduced_symbol.get_type().array_element().get_unqualified_type().get_lvalue_reference_to());
    }
    else {
        parameter_names.append("_shared_data"); parameter_types.append(reduced_symbol.get_type().get_lvalue_reference_to());
        parameter_names.append("_priv_data"); parameter_types.append(reduced_symbol.get_type().get_lvalue_reference_to());
    }

    outline_task = SymbolUtils::new_function_symbol(
            enclosing_function,
            outline_task_name.str(),
            TL::Type::get_void_type(),
            parameter_names,
            parameter_types);

    SymbolUtils::build_empty_body_for_function(outline_task,
            outline_task_code,
            outline_task_stmt);

    TL::Symbol param_priv = outline_task_stmt.retrieve_context().get_symbol_from_name("_priv_data");
    ERROR_CONDITION(!param_priv.is_valid(), "Symbol omp_in not found", 0);
    TL::Symbol param_shared = outline_task_stmt.retrieve_context().get_symbol_from_name("_shared_data");
    ERROR_CONDITION(!param_shared.is_valid(), "Symbol omp_out not found", 0);

    Nodecl::Utils::SimpleSymbolMap symbol_map;
    symbol_map.add_map(reduction->get_omp_in(), param_priv);
    symbol_map.add_map(reduction->get_omp_out(), param_shared);

    outline_task_stmt.replace(
            Nodecl::ExpressionStatement::make(
                Nodecl::Utils::deep_copy(
                    reduction->get_combiner(),
                    outline_task_stmt.retrieve_context(),
                    symbol_map)));

    // As the reduction function is needed during the instantiation of
    // the task, this function should be inserted before the construct
    Nodecl::Utils::prepend_to_enclosing_top_level_location(construct, outline_task_code);
}

void LoweringVisitor::visit(const Nodecl::OpenMP::Taskgroup& construct)
{
    Nodecl::NodeclBase statements = construct.get_statements();

    Nodecl::List environment = construct.get_environment().as<Nodecl::List>();
    TL::ObjectList<Nodecl::OpenMP::TaskReduction> reduction_list = environment.find_all<Nodecl::OpenMP::TaskReduction>();

    TL::Symbol ident_symbol = Intel::new_global_ident_symbol(construct);
    TL::Scope global_scope = CURRENT_COMPILED_FILE->global_decl_context;

    TL::ObjectList<Nodecl::OpenMP::ReductionItem> reduction_items;
    if (!reduction_list.empty()) {
        TL::ObjectList<Nodecl::OpenMP::ReductionItem> tmp
            = reduction_list
            .map<Nodecl::NodeclBase>(&Nodecl::OpenMP::TaskReduction::get_reductions)
            .map<Nodecl::List>(&Nodecl::NodeclBase::as<Nodecl::List>)
            .map<TL::ObjectList<Nodecl::NodeclBase> >(&Nodecl::List::to_object_list)
            .reduction((&TL::append_two_lists<Nodecl::NodeclBase>))
            .map<Nodecl::OpenMP::ReductionItem>(&Nodecl::NodeclBase::as<Nodecl::OpenMP::ReductionItem>);

        reduction_items.insert(tmp);
    }

    // Declare tg before task to make it accessible
    if (!reduction_items.empty()) {
        Source src_tg_decl;
        src_tg_decl
        << "void *tg;";

        Nodecl::NodeclBase tree_tg_decl = src_tg_decl.parse_statement(construct);
        construct.append_sibling(tree_tg_decl);
    }

    walk(statements);
    statements = construct.get_statements(); // Should not be necessary

    TL::Source src_taskgroup;
    Nodecl::NodeclBase stmt_taskgroup_body;
    Nodecl::NodeclBase stmt_reduction_init;
    src_taskgroup
    << "__kmpc_taskgroup(&" << as_symbol(ident_symbol)
    << ", __kmpc_global_thread_num(&" << as_symbol(ident_symbol) << "));"
    << statement_placeholder(stmt_reduction_init)
    << statement_placeholder(stmt_taskgroup_body)
    << "__kmpc_end_taskgroup(&" << as_symbol(ident_symbol)
    << ", __kmpc_global_thread_num(&" << as_symbol(ident_symbol) << "));";

    Nodecl::NodeclBase tree_taskgroup = src_taskgroup.parse_statement(construct);
    stmt_taskgroup_body.replace(statements);

    if (!reduction_items.empty()) {
        Source src_reduction_init;
        src_reduction_init
        << "kmp_task_red_input_t _red_info[" << reduction_items.size() << "];";

        int array_pos = 0;

        for (auto it = reduction_items.begin(); it != reduction_items.end(); it++, array_pos++) {
            TL::Symbol reduced_symbol = it->get_reduced_symbol().get_symbol();

            TL::Symbol outline_task_red_init;
            Nodecl::NodeclBase outline_task_red_init_code, outline_task_red_init_stmt;
            create_red_init_func(construct,
                                 global_scope,
                                 *it,
                                 outline_task_red_init,
                                 outline_task_red_init_code,
                                 outline_task_red_init_stmt);

            TL::Symbol outline_task_red_comb;
            Nodecl::NodeclBase outline_task_red_comb_code, outline_task_red_comb_stmt;
            create_red_comb_func(construct,
                                 global_scope,
                                 *it,
                                 outline_task_red_comb,
                                 outline_task_red_comb_code,
                                 outline_task_red_comb_stmt);

            src_reduction_init
            << "_red_info[" << array_pos << "].reduce_shar = &" << as_symbol(reduced_symbol) << ";"
            << "_red_info[" << array_pos << "].reduce_size = sizeof(" << as_symbol(reduced_symbol) << ");"
            << "_red_info[" << array_pos << "].reduce_init = &" << as_symbol(outline_task_red_init) << ";"
            << "_red_info[" << array_pos << "].reduce_fini = 0;"
            << "_red_info[" << array_pos << "].reduce_comb = &" << as_symbol(outline_task_red_comb) << ";"
            << "_red_info[" << array_pos << "].flags.lazy_priv = 0;"
            << "_red_info[" << array_pos << "].flags.reserved31 = 0;";
        }

        src_reduction_init
        << "tg = __kmpc_task_reduction_init(__kmpc_global_thread_num(&" << as_symbol(ident_symbol) << "),"
                                               << reduction_items.size() << ", _red_info);";

        Nodecl::NodeclBase tree_reduction_init = src_reduction_init.parse_statement(construct);
        stmt_reduction_init.replace(tree_reduction_init);
    }

    construct.replace(tree_taskgroup);
}

} }
