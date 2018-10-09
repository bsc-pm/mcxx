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

struct ReductionInfo {
    Type type;
    bool is_array;
    bool has_destructor;
    Symbol omp_orig;
    Nodecl::NodeclBase array_size_expr;
    OpenMP::Reduction* reduction;
};

// This is used in task lowering to save the vla_size into a global value declared here
std::map<TL::Symbol, TL::Symbol> vla_sym_size_map;

static void create_red_init_func(const Nodecl::OpenMP::Taskgroup& construct,
                                 ReductionInfo& red_info,
                                 TL::Symbol& red_init,
                                 Nodecl::NodeclBase& red_init_code,
                                 Nodecl::NodeclBase& red_init_stmt) {

    TL::Symbol enclosing_function = Nodecl::Utils::get_enclosing_function(construct);

    TL::Counter &red_num = TL::CounterManager::get_counter("intel-omp-task-red-init");
    std::stringstream red_init_name;
    red_init_name << "_red_init" << "_" << (int)red_num;
    red_num++;

    TL::ObjectList<std::string> parameter_names;
    TL::ObjectList<TL::Type> parameter_types;

    parameter_names.append("_reduce_init");
    parameter_types.append(red_info.type.get_pointer_to());

    red_init = SymbolUtils::new_function_symbol(
            enclosing_function,
            red_init_name.str(),
            TL::Type::get_void_type(),
            parameter_names,
            parameter_types);

    SymbolUtils::build_empty_body_for_function(red_init,
            red_init_code,
            red_init_stmt);

    TL::Symbol new_orig = red_info.omp_orig;
    TL::Symbol omp_orig = red_info.reduction->get_omp_orig();
    Nodecl::NodeclBase init_expr = red_info.reduction->get_initializer().shallow_copy();

    if (!red_info.is_array) {
        Source src_red_init_body;

        std::map<TL::Symbol, std::string> m;
        std::stringstream fmt;
        fmt << "*" << as_symbol(new_orig);
        m[omp_orig] = fmt.str();
        ReplaceSymbols rs(m, construct.retrieve_context());

        rs.walk(init_expr);

        src_red_init_body
        << "*_reduce_init = " << as_expression(init_expr) << ";";

        Nodecl::NodeclBase tree_red_init_body = src_red_init_body.parse_statement(red_init_stmt);
        red_init_stmt.replace(tree_red_init_body);
    }
    else {
        Source src_red_init_body;
        Nodecl::NodeclBase stmt_init;
        src_red_init_body
        << "for (int i = 0; i < " << as_expression(red_info.array_size_expr) << "; ++i) {"
        <<      statement_placeholder(stmt_init)
        << "}";
        Nodecl::NodeclBase tree_red_init_body = src_red_init_body.parse_statement(red_init_stmt);
        red_init_stmt.replace(tree_red_init_body);

        TL::Symbol ind_var = red_init_stmt.retrieve_context().get_symbol_from_name("i");
        ERROR_CONDITION(!ind_var.is_valid(), "Symbol i not found", 0);

        std::map<TL::Symbol, std::string> m;
        std::stringstream fmt;
        fmt << as_symbol(new_orig) + "[" + as_symbol(ind_var) + "]";
        m[omp_orig] = fmt.str();
        ReplaceSymbols rs(m, construct.retrieve_context());

        rs.walk(init_expr);

        Source src_new_init;
        src_new_init
        << "_reduce_init[i] = " << as_expression(init_expr) << ";";
        Nodecl::NodeclBase tree_new_init = src_new_init.parse_statement(stmt_init);
        stmt_init.replace(tree_new_init);
    }

    // As the reduction function is needed during the instantiation of
    // the task, this function should be inserted before the construct
    Nodecl::Utils::prepend_to_enclosing_top_level_location(construct, red_init_code);
}

// The function is created only if the struct/class provides a destructor
// Arrays of struct/class are considered
static void create_red_fin_func(const Nodecl::OpenMP::Taskgroup& construct,
                                 ReductionInfo& red_info,
                                 TL::Symbol& red_fin,
                                 Nodecl::NodeclBase& red_fin_code,
                                 Nodecl::NodeclBase& red_fin_stmt) {

    if (!red_info.type.is_class()) return;
    TL::Symbol destructor = class_type_get_destructor(red_info.type.get_internal_type());
    if (!destructor.is_valid()) return;

    TL::Symbol enclosing_function = Nodecl::Utils::get_enclosing_function(construct);

    TL::Counter &red_num = TL::CounterManager::get_counter("intel-omp-task-red-fin");
    std::stringstream red_fin_name;
    red_fin_name << "_red_fin" << "_" << (int)red_num;
    red_num++;

    TL::ObjectList<std::string> parameter_names;
    TL::ObjectList<TL::Type> parameter_types;

    parameter_names.append("_reduce_fin");
    parameter_types.append(red_info.type.get_pointer_to());

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
    if (red_info.is_array) {
        src_red_fin_body
        << "for (int i = 0; i < " << as_expression(red_info.array_size_expr) << "; ++i) {"
        <<      "_reduce_fin[i]." << destructor.get_name() << "();"
        << "}";
    }
    else {
        src_red_fin_body
        << "(*_reduce_fin)." << destructor.get_name() << "();";
    }
    Nodecl::NodeclBase tree_red_fin_body = src_red_fin_body.parse_statement(red_fin_stmt);
    red_fin_stmt.replace(tree_red_fin_body);

    //As the reduction function is needed during the instantiation of
    //the task, this function should be inserted before the construct
    Nodecl::Utils::prepend_to_enclosing_top_level_location(construct, red_fin_code);
}

static void create_red_comb_func(const Nodecl::OpenMP::Taskgroup& construct,
                                 ReductionInfo& red_info,
                                 TL::Symbol& red_comb,
                                 Nodecl::NodeclBase& red_comb_code,
                                 Nodecl::NodeclBase& red_comb_stmt) {

    OpenMP::Reduction* reduction = red_info.reduction;

    TL::Symbol enclosing_function = Nodecl::Utils::get_enclosing_function(construct);

    TL::Counter &red_num = TL::CounterManager::get_counter("intel-omp-task-red-comb");
    std::stringstream red_comb_name;
    red_comb_name << "_red_comb" << "_" << (int)red_num;
    red_num++;

    TL::ObjectList<std::string> parameter_names;
    TL::ObjectList<TL::Type> parameter_types;

    parameter_names.append("_shared_data");
    parameter_names.append("_priv_data");

    parameter_types.append(red_info.type.get_pointer_to());
    parameter_types.append(red_info.type.get_pointer_to());

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

    if (red_info.is_array) {
        Source src_red_comb_body;
        Nodecl::NodeclBase stmt_comb;
        src_red_comb_body
        << "for (int i = 0; i < " << as_expression(red_info.array_size_expr) << "; ++i) {"
        <<      statement_placeholder(stmt_comb)
        << "}";
        Nodecl::NodeclBase tree_red_comb_body = src_red_comb_body.parse_statement(red_comb_stmt);
        red_comb_stmt.replace(tree_red_comb_body);

        TL::Symbol ind_var = red_comb_stmt.retrieve_context().get_symbol_from_name("i");
        ERROR_CONDITION(!ind_var.is_valid(), "Symbol i not found", 0);

        std::map<TL::Symbol, std::string> m;
        std::stringstream fmt1, fmt2;
        fmt1 << as_symbol(param_priv) + "[" + as_symbol(ind_var) + "]";
        m[reduction->get_omp_in()] = fmt1.str();
        fmt2 << as_symbol(param_shared) + "[" + as_symbol(ind_var) + "]";
        m[reduction->get_omp_out()] = fmt2.str();
        ReplaceSymbols rs(m, construct.retrieve_context());

        Nodecl::NodeclBase combiner = reduction->get_combiner().shallow_copy();
        rs.walk(combiner);

        Source src_new_combiner;
        src_new_combiner
        << as_expression(combiner) << ";";
        Nodecl::NodeclBase tree_new_combiner = src_new_combiner.parse_statement(stmt_comb);
        stmt_comb.replace(tree_new_combiner);
    }
    else {
        std::map<TL::Symbol, std::string> m;
        std::stringstream fmt1, fmt2;
        fmt1 << "*" << as_symbol(param_priv);
        m[reduction->get_omp_in()] = fmt1.str();
        fmt2 << "*" <<as_symbol(param_shared);
        m[reduction->get_omp_out()] = fmt2.str();
        ReplaceSymbols rs(m, construct.retrieve_context());

        Nodecl::NodeclBase combiner = reduction->get_combiner().shallow_copy();
        rs.walk(combiner);

        red_comb_stmt.replace(
            Nodecl::ExpressionStatement::make(
                combiner));

    }

    //As the reduction function is needed during the instantiation of
    //the task, this function should be inserted before the construct
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

    TL::ObjectList<ReductionInfo> reduction_infos;

    // Declare tg before task to make it accessible
    if (!reduction_items.empty()) {
        Source src_tg_decl;
        src_tg_decl
        << "void *tg;";

        Nodecl::NodeclBase tree_tg_decl = src_tg_decl.parse_statement(construct);
        construct.prepend_sibling(tree_tg_decl);

        for (auto it = reduction_items.begin(); it != reduction_items.end(); it++) {
            ReductionInfo red_info;

            TL::Symbol reduced_symbol = it->get_reduced_symbol().get_symbol();
            TL::Symbol reductor = it->get_reductor().get_symbol();
            OpenMP::Reduction* reduction = OpenMP::Reduction::get_reduction_info_from_symbol(reductor);
            Nodecl::NodeclBase init_expr = reduction->get_initializer().shallow_copy();

            red_info.reduction = reduction;

            Type type = reduced_symbol.get_type().no_ref();
            Source array_size;
            array_size << "1";

            red_info.is_array = false;
            red_info.type = type;
            while (type.is_array()) {
                red_info.is_array = true;

                Nodecl::NodeclBase size = type.array_get_size();
                if (size.is<Nodecl::Symbol>()
                        && size.get_symbol().is_saved_expression())
                {
                    TL::Symbol vla_size_sym = size.get_symbol();
                    Nodecl::NodeclBase decl_new_orig = Source(as_type(vla_size_sym
                                                    .get_type()
                                                    .no_ref()
                                                    .get_unqualified_type()) + vla_size_sym.get_name() + "_red;").parse_declaration(construct);
                    Nodecl::Utils::prepend_to_enclosing_top_level_location(construct, decl_new_orig);
                    TL::Symbol new_vla_size_sym = construct
                                            .retrieve_context()
                                            .get_symbol_from_name(vla_size_sym.get_name() + "_red");
                    array_size << " * " << as_symbol(new_vla_size_sym);

                    vla_sym_size_map[vla_size_sym] = new_vla_size_sym;
                }
                else {
                    array_size << " * " << as_expression(size);
                }
                type = type.array_element();
                red_info.type = type;
            }
            red_info.array_size_expr = array_size.parse_expression(construct);

            TL::Symbol omp_orig = reduction->get_omp_orig();

            SymbolExistenceCheck sec(omp_orig);
            sec.walk(init_expr);

            if (sec.exist_symbol()) {
                Nodecl::NodeclBase decl_new_orig = Source(as_type(type
                                                .get_pointer_to()) + reduced_symbol.get_name() + "_orig;").parse_declaration(construct);
                Nodecl::Utils::prepend_to_enclosing_top_level_location(construct, decl_new_orig);

                red_info.omp_orig = construct
                                    .retrieve_context()
                                    .get_symbol_from_name(reduced_symbol
                                                          .get_name() + "_orig");

            }

            reduction_infos.append(red_info);
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
        auto it1 = reduction_infos.begin();

        for (auto it = reduction_items.begin(); it != reduction_items.end(); it++, it1++, array_pos++) {
            TL::Symbol reduced_symbol = it->get_reduced_symbol().get_symbol();
            TL::Symbol red_init;
            Nodecl::NodeclBase red_init_code, red_init_stmt;
            create_red_init_func(construct,
                                 *it1,
                                 red_init,
                                 red_init_code,
                                 red_init_stmt);

            TL::Symbol red_comb;
            Nodecl::NodeclBase red_comb_code, red_comb_stmt;
            create_red_comb_func(construct,
                                 *it1,
                                 red_comb,
                                 red_comb_code,
                                 red_comb_stmt);

            TL::Symbol red_fin;
            Nodecl::NodeclBase red_fin_code, red_fin_stmt;
            create_red_fin_func(construct,
                                 *it1,
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
