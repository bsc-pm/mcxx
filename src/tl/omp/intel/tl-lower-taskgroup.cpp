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
#include "tl-lower-task-common.hpp"

namespace TL { namespace Intel {

static void create_red_init_func(const Nodecl::OpenMP::Taskgroup& construct,
                                 Nodecl::OpenMP::ReductionItem& red_item,
                                 TL::Symbol& red_init,
                                 Nodecl::NodeclBase& red_init_code,
                                 Nodecl::NodeclBase& red_init_stmt) {

    TL::Symbol reduced_symbol = red_item.get_reduced_symbol().get_symbol();
    TL::Symbol reductor = red_item.get_reductor().get_symbol();
    OpenMP::Reduction* reduction = OpenMP::Reduction::get_reduction_info_from_symbol(reductor);
    Nodecl::NodeclBase init_expr = reduction->get_initializer();

    TL::Symbol enclosing_function = Nodecl::Utils::get_enclosing_function(construct);

    TL::Counter &red_num = TL::CounterManager::get_counter("intel-omp-task-red-init");
    std::stringstream red_init_name;
    red_init_name << "_red_" << reduced_symbol.get_name() << "_init" << "_" << (int)red_num;
    red_num++;

    TL::ObjectList<std::string> parameter_names;
    TL::ObjectList<TL::Type> parameter_types;

    parameter_names.append("_reduce_init");
    parameter_types.append(reduced_symbol.get_type().get_lvalue_reference_to());

    red_init = SymbolUtils::new_function_symbol(
            enclosing_function,
            red_init_name.str(),
            TL::Type::get_void_type(),
            parameter_names,
            parameter_types);

    SymbolUtils::build_empty_body_for_function(red_init,
            red_init_code,
            red_init_stmt);

    Source src_red_init_body;
    if (!reduced_symbol.get_type().no_ref().is_array()) {
        src_red_init_body
        << "_reduce_init = " << as_expression(init_expr) << ";";
    }
    else {
        src_red_init_body
        << "for (int i = 0; i < " << as_expression(reduced_symbol.get_type().no_ref().array_get_size()) << "; ++i) {"
        <<      "_reduce_init[i] = " << as_expression(init_expr) << ";"
        << "}";
    }
    Nodecl::NodeclBase tree_red_init_body = src_red_init_body.parse_statement(red_init_stmt);
    red_init_stmt.replace(tree_red_init_body);

    // As the reduction function is needed during the instantiation of
    // the task, this function should be inserted before the construct
    Nodecl::Utils::prepend_to_enclosing_top_level_location(construct, red_init_code);
}

// The function is created only if the struct/class provides a destructor
// Arrays of struct/class are considered
static void create_red_fin_func(const Nodecl::OpenMP::Taskgroup& construct,
                                 Nodecl::OpenMP::ReductionItem& red_item,
                                 TL::Symbol& red_fin,
                                 Nodecl::NodeclBase& red_fin_code,
                                 Nodecl::NodeclBase& red_fin_stmt) {

    TL::Symbol reduced_symbol = red_item.get_reduced_symbol().get_symbol();
    TL::Type reduced_type = reduced_symbol.get_type().no_ref();

    if (reduced_type.is_array()) reduced_type = reduced_type.array_element().no_ref();

    if (!reduced_type.is_class()) return;

    TL::Symbol destructor = class_type_get_destructor(reduced_type.get_internal_type());
    if (!destructor.is_valid()) return;

    TL::Symbol enclosing_function = Nodecl::Utils::get_enclosing_function(construct);

    TL::Counter &red_num = TL::CounterManager::get_counter("intel-omp-task-red-fin");
    std::stringstream red_fin_name;
    red_fin_name << "_red_" << reduced_symbol.get_name() << "_fin" << "_" << (int)red_num;
    red_num++;

    TL::ObjectList<std::string> parameter_names;
    TL::ObjectList<TL::Type> parameter_types;

    parameter_names.append("_reduce_fin");
    parameter_types.append(reduced_symbol.get_type().get_lvalue_reference_to());

    red_fin = SymbolUtils::new_function_symbol(
            enclosing_function,
            red_fin_name.str(),
            TL::Type::get_void_type(),
            parameter_names,
            parameter_types);

    SymbolUtils::build_empty_body_for_function(red_fin,
            red_fin_code,
            red_fin_stmt);

    Source src_red_fin_body;
    if (reduced_symbol.get_type().no_ref().is_array()) {
        src_red_fin_body
        << "for (int i = 0; i < " << as_expression(reduced_symbol.get_type().no_ref().array_get_size()) << "; ++i) {"
        <<      "_reduce_fin[i]." << destructor.get_name() << "();"
        << "}";
    }
    else {
        src_red_fin_body
        << "(_reduce_fin)." << destructor.get_name() << "();";
    }
    Nodecl::NodeclBase tree_red_fin_body = src_red_fin_body.parse_statement(red_fin_stmt);
    red_fin_stmt.replace(tree_red_fin_body);

    // As the reduction function is needed during the instantiation of
    // the task, this function should be inserted before the construct
    Nodecl::Utils::prepend_to_enclosing_top_level_location(construct, red_fin_code);
}

static void create_red_comb_func(const Nodecl::OpenMP::Taskgroup& construct,
                                 Nodecl::OpenMP::ReductionItem& red_item,
                                 TL::Symbol& red_comb,
                                 Nodecl::NodeclBase& red_comb_code,
                                 Nodecl::NodeclBase& red_comb_stmt) {

    TL::Symbol reduced_symbol = red_item.get_reduced_symbol().get_symbol();
    TL::Symbol reductor = red_item.get_reductor().get_symbol();
    OpenMP::Reduction* reduction = OpenMP::Reduction::get_reduction_info_from_symbol(reductor);

    TL::Symbol enclosing_function = Nodecl::Utils::get_enclosing_function(construct);

    TL::Counter &red_num = TL::CounterManager::get_counter("intel-omp-task-red-comb");
    std::stringstream red_comb_name;
    red_comb_name << "_red_" << reduced_symbol.get_name() << "_comb" << "_" << (int)red_num;
    red_num++;

    TL::ObjectList<std::string> parameter_names;
    TL::ObjectList<TL::Type> parameter_types;

    parameter_names.append("_shared_data"); parameter_types.append(reduced_symbol.get_type().get_lvalue_reference_to());
    parameter_names.append("_priv_data"); parameter_types.append(reduced_symbol.get_type().get_lvalue_reference_to());

    red_comb = SymbolUtils::new_function_symbol(
            enclosing_function,
            red_comb_name.str(),
            TL::Type::get_void_type(),
            parameter_names,
            parameter_types);

    SymbolUtils::build_empty_body_for_function(red_comb,
            red_comb_code,
            red_comb_stmt);

    TL::Symbol param_priv = red_comb_stmt.retrieve_context().get_symbol_from_name("_priv_data");
    ERROR_CONDITION(!param_priv.is_valid(), "Symbol omp_in not found", 0);
    TL::Symbol param_shared = red_comb_stmt.retrieve_context().get_symbol_from_name("_shared_data");
    ERROR_CONDITION(!param_shared.is_valid(), "Symbol omp_out not found", 0);

    if (reduced_symbol.get_type().no_ref().is_array()) {
        Source src_red_comb_body;
        Nodecl::NodeclBase stmt_comb;
        src_red_comb_body
        << "for (int i = 0; i < " << as_expression(param_shared.get_type().no_ref().array_get_size()) << "; ++i) {"
        <<      statement_placeholder(stmt_comb)
        << "}";
        Nodecl::NodeclBase tree_red_comb_body = src_red_comb_body.parse_statement(red_comb_stmt);
        red_comb_stmt.replace(tree_red_comb_body);

        TL::Symbol ind_var = red_comb_stmt.retrieve_context().get_symbol_from_name("i");
        ERROR_CONDITION(!ind_var.is_valid(), "Symbol i not found", 0);

        ReplaceInOutVect riov(reduction->get_omp_in(),
                             reduction->get_omp_out(),
                             param_priv,
                             param_shared,
                             ind_var,
                             stmt_comb.retrieve_context());
        Nodecl::NodeclBase combiner = reduction->get_combiner().shallow_copy();
        riov.walk(combiner);

        Source src_new_combiner;
        src_new_combiner
        << as_expression(combiner) << ";";
        Nodecl::NodeclBase tree_new_combiner = src_new_combiner.parse_statement(stmt_comb);
        stmt_comb.replace(tree_new_combiner);
    }
    else {
        Nodecl::Utils::SimpleSymbolMap symbol_map;
        symbol_map.add_map(reduction->get_omp_in(), param_priv);
        symbol_map.add_map(reduction->get_omp_out(), param_shared);

        red_comb_stmt.replace(
                Nodecl::ExpressionStatement::make(
                    Nodecl::Utils::deep_copy(
                        reduction->get_combiner(),
                        red_comb_stmt.retrieve_context(),
                        symbol_map)));
    }

    // As the reduction function is needed during the instantiation of
    // the task, this function should be inserted before the construct
    Nodecl::Utils::prepend_to_enclosing_top_level_location(construct, red_comb_code);
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
        construct.prepend_sibling(tree_tg_decl);

        for (auto it = reduction_items.begin(); it != reduction_items.end(); it++) {
            TL::Symbol reduced_symbol = it->get_reduced_symbol().get_symbol();
            TL::Symbol reductor = it->get_reductor().get_symbol();
            OpenMP::Reduction* reduction = OpenMP::Reduction::get_reduction_info_from_symbol(reductor);
            Nodecl::NodeclBase init_expr = reduction->get_initializer();

            TL::Symbol omp_orig = reduction->get_omp_orig();

            SymbolExistenceCheck sec(omp_orig);
            sec.walk(init_expr);

            if (sec.exist_symbol()) {
                Nodecl::NodeclBase decl_new_orig = Source(as_type(omp_orig
                                                .get_type()
                                                .no_ref()
                                                .get_pointer_to()) + reduced_symbol.get_name() + "_orig;").parse_declaration(construct);
                Nodecl::Utils::prepend_to_enclosing_top_level_location(construct, decl_new_orig);

                TL::Symbol new_orig = construct
                                        .retrieve_context()
                                        .get_symbol_from_name(reduced_symbol.get_name() + "_orig");
                ReplaceOrig ro(omp_orig, new_orig, construct.retrieve_context());
                ro.walk(init_expr);
            }
        }

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

            TL::Symbol red_init;
            Nodecl::NodeclBase red_init_code, red_init_stmt;
            create_red_init_func(construct,
                                 *it,
                                 red_init,
                                 red_init_code,
                                 red_init_stmt);

            TL::Symbol red_comb;
            Nodecl::NodeclBase red_comb_code, red_comb_stmt;
            create_red_comb_func(construct,
                                 *it,
                                 red_comb,
                                 red_comb_code,
                                 red_comb_stmt);

            TL::Symbol red_fin;
            Nodecl::NodeclBase red_fin_code, red_fin_stmt;
            create_red_fin_func(construct,
                                 *it,
                                 red_fin,
                                 red_fin_code,
                                 red_fin_stmt);

            src_reduction_init
            << "_red_info[" << array_pos << "].reduce_shar = &" << as_symbol(reduced_symbol) << ";"
            << "_red_info[" << array_pos << "].reduce_size = sizeof(" << as_symbol(reduced_symbol) << ");"
            << "_red_info[" << array_pos << "].reduce_init = (void *)&" << as_symbol(red_init) << ";"
            << "_red_info[" << array_pos << "].reduce_comb = (void *)&" << as_symbol(red_comb) << ";"
            << "_red_info[" << array_pos << "].flags.lazy_priv = 1;"
            << "_red_info[" << array_pos << "].flags.reserved31 = 0;";
            if (!red_fin.is_valid()) {
                src_reduction_init
                << "_red_info[" << array_pos << "].reduce_fini = 0;";
            }
            else{
                src_reduction_init
                << "_red_info[" << array_pos << "].reduce_fini = (void *)&" << as_symbol(red_fin) << ";";
            }
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
