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

#include "tl-lower-reductions.hpp"

#include "tl-counters.hpp"

#include "cxx-diagnostic.h"

namespace TL { namespace GOMP {

void LoweringVisitor::visit(const Nodecl::OpenMP::For& construct)
{
    lower_for(construct, Nodecl::NodeclBase::null(),
            Nodecl::NodeclBase::null());
}

void LoweringVisitor::lower_for(const Nodecl::OpenMP::For& construct,
        const Nodecl::NodeclBase &prependix,
        const Nodecl::NodeclBase &appendix)
{
    if (!prependix.is_null() || !appendix.is_null())
    {
        fatal_printf_at(construct.get_locus(),
                        "prependix or appendix are not supported");
    }

    TL::ForStatement for_statement(construct.get_loop().as<Nodecl::Context>().
            get_in_context().as<Nodecl::List>().front().as<Nodecl::ForStatement>());

    ERROR_CONDITION(!for_statement.is_omp_valid_loop(), "Invalid loop at this point", 0);

    Nodecl::List environment = construct.get_environment().as<Nodecl::List>();

    Nodecl::OpenMP::Schedule schedule = environment.find_first<Nodecl::OpenMP::Schedule>();
    ERROR_CONDITION(schedule.is_null(), "Schedule tree is missing", 0);

    Nodecl::NodeclBase statements = for_statement.get_statement();
    walk(statements);
    statements = for_statement.get_statement(); // Should not be necessary

    TL::ObjectList<Nodecl::OpenMP::Shared> shared_list =
        environment.find_all<Nodecl::OpenMP::Shared>();
    TL::ObjectList<Nodecl::OpenMP::Private> private_list =
        environment.find_all<Nodecl::OpenMP::Private>();
    TL::ObjectList<Nodecl::OpenMP::Firstprivate> firstprivate_list =
        environment.find_all<Nodecl::OpenMP::Firstprivate>();
    TL::ObjectList<Nodecl::OpenMP::Lastprivate> lastprivate_list =
        environment.find_all<Nodecl::OpenMP::Lastprivate>();
    TL::ObjectList<Nodecl::OpenMP::FirstLastprivate> firstlastprivate_list =
        environment.find_all<Nodecl::OpenMP::FirstLastprivate>();
    TL::ObjectList<Nodecl::OpenMP::Reduction> reduction_list = environment.find_all<Nodecl::OpenMP::Reduction>();

    Nodecl::OpenMP::BarrierAtEnd barrier_at_end = environment.find_first<Nodecl::OpenMP::BarrierAtEnd>();

    TL::ObjectList<TL::Symbol> private_symbols;
    TL::ObjectList<TL::Symbol> firstprivate_symbols;
    TL::ObjectList<TL::Symbol> lastprivate_symbols;
    if (!private_list.empty())
    {
        TL::ObjectList<Symbol> tmp
            = private_list.map<Nodecl::NodeclBase>(
                               &Nodecl::OpenMP::Private::get_symbols)
                  .map<Nodecl::List>(&Nodecl::NodeclBase::as<Nodecl::List>)
                  .map<TL::ObjectList<Nodecl::NodeclBase> >(
                       &Nodecl::List::to_object_list)
                  .reduction(TL::append_two_lists<Nodecl::NodeclBase>)
                  .map<TL::Symbol>(&Nodecl::NodeclBase::get_symbol);

        private_symbols.insert(tmp);
    }
    if (!firstprivate_list.empty())
    {
        TL::ObjectList<Symbol> tmp
            = firstprivate_list.map<Nodecl::NodeclBase>(
                                    &Nodecl::OpenMP::Firstprivate::get_symbols)
                  .map<Nodecl::List>(&Nodecl::NodeclBase::as<Nodecl::List>)
                  .map<TL::ObjectList<Nodecl::NodeclBase> >(
                       &Nodecl::List::to_object_list)
                  .reduction(TL::append_two_lists<Nodecl::NodeclBase>)
                  .map<TL::Symbol>(&Nodecl::NodeclBase::get_symbol);

        private_symbols.insert(tmp);
        firstprivate_symbols.insert(tmp);
    }
    if (!lastprivate_list.empty())
    {
        TL::ObjectList<Symbol> tmp
            = lastprivate_list.map<Nodecl::NodeclBase>(
                                   &Nodecl::OpenMP::Lastprivate::get_symbols)
                  .map<Nodecl::List>(&Nodecl::NodeclBase::as<Nodecl::List>)
                  .map<TL::ObjectList<Nodecl::NodeclBase> >(
                       &Nodecl::List::to_object_list)
                  .reduction(TL::append_two_lists<Nodecl::NodeclBase>)
                  .map<TL::Symbol>(&Nodecl::NodeclBase::get_symbol);

        private_symbols.insert(tmp);
        lastprivate_symbols.insert(tmp);
    }
    if (!firstlastprivate_list.empty())
    {
        TL::ObjectList<Symbol> tmp
            = firstlastprivate_list
                  .map<Nodecl::NodeclBase>(
                       &Nodecl::OpenMP::FirstLastprivate::get_symbols)
                  .map<Nodecl::List>(&Nodecl::NodeclBase::as<Nodecl::List>)
                  .map<TL::ObjectList<Nodecl::NodeclBase> >(
                       &Nodecl::List::to_object_list)
                  .reduction(TL::append_two_lists<Nodecl::NodeclBase>)
                  .map<TL::Symbol>(&Nodecl::NodeclBase::get_symbol);

        private_symbols.insert(tmp);
        firstprivate_symbols.insert(tmp);
        lastprivate_symbols.insert(tmp);
    }

    TL::ObjectList<Nodecl::OpenMP::ReductionItem> reduction_items
        = reduction_list.map<Nodecl::NodeclBase>(
                             &Nodecl::OpenMP::Reduction::get_reductions)
              .map<Nodecl::List>(&Nodecl::NodeclBase::as<Nodecl::List>)
              .map<TL::ObjectList<Nodecl::NodeclBase> >(
                   &Nodecl::List::to_object_list)
              .reduction((&TL::append_two_lists<Nodecl::NodeclBase>))
              .map<Nodecl::OpenMP::ReductionItem>(
                  &Nodecl::NodeclBase::as<Nodecl::OpenMP::ReductionItem>);

    Source loop_construct;
    Nodecl::NodeclBase stmt_placeholder;
    loop_construct
        << "{"
        << statement_placeholder(stmt_placeholder)
        << "}";

    Nodecl::NodeclBase loop_construct_tree = loop_construct.parse_statement(construct);

    Nodecl::Utils::SimpleSymbolMap symbol_map;

    TL::Counter &private_num = TL::CounterManager::get_counter("gomp-omp-privates");

    TL::Symbol induction_var = for_statement.get_induction_variable();
    TL::Type induction_var_type = induction_var.get_type().no_ref();

    private_symbols.insert(induction_var);

    TL::Scope block_scope = stmt_placeholder.retrieve_context();
    for (TL::ObjectList<TL::Symbol>::iterator it = private_symbols.begin();
            it != private_symbols.end();
            it++)
    {
        std::stringstream new_name;
        new_name << "p_" << it->get_name() << (int)private_num;
        private_num++;

        TL::Symbol new_private_sym = GOMP::new_private_symbol(*it, block_scope);

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

        CXX_LANGUAGE()
        {
            stmt_placeholder.prepend_sibling(
                    Nodecl::CxxDef::make(
                        /* context */ Nodecl::NodeclBase::null(),
                        new_private_sym));
        }
    }

    // TL::Symbol reduction_pack_symbol;
    if (!reduction_items.empty())
    {
        fatal_printf_at(construct.get_locus(),
                        "reductions not yet implemented");
#if 0
        TL::ObjectList<Symbol> reduction_symbols
            = reduction_items
                  .map<Nodecl::NodeclBase>(
                       &Nodecl::OpenMP::ReductionItem::get_reduced_symbol)
                  .map<TL::Symbol>(&Nodecl::NodeclBase::get_symbol);

        TL::Symbol reduction_pack_type = declare_reduction_pack(reduction_symbols, construct);
        reduction_pack_symbol = GOMP::new_private_symbol(
                "red",
                reduction_pack_type.get_user_defined_type(),
                SK_VARIABLE,
                block_scope);

        // Initialize every member with the neuter
        TL::ObjectList<TL::Symbol> fields = reduction_pack_symbol.get_type().get_fields();
        TL::ObjectList<TL::Symbol>::iterator it_fields = fields.begin();
        for (TL::ObjectList<Nodecl::OpenMP::ReductionItem>::iterator it = reduction_items.begin();
                it != reduction_items.end();
                it++, it_fields++)
        {
            Nodecl::OpenMP::ReductionItem &current(*it);

            TL::Symbol reductor = current.get_reductor().get_symbol();
            OpenMP::Reduction* omp_reduction = OpenMP::Reduction::get_reduction_info_from_symbol(reductor);

            Source init_field;
            init_field << as_symbol(reduction_pack_symbol) << "." << it_fields->get_name()
                << " = " << as_expression(omp_reduction->get_initializer().shallow_copy()) << ";"
                ;
            Nodecl::NodeclBase init_field_tree = init_field.parse_statement(stmt_placeholder);
            stmt_placeholder.prepend_sibling(init_field_tree);
        }

        CXX_LANGUAGE()
        {
            stmt_placeholder.prepend_sibling(
                    Nodecl::CxxDef::make(
                        /* context */ Nodecl::NodeclBase::null(),
                        reduction_pack_symbol));
        }
#endif
    }

    Source static_init, lower, upper, step, chunk_size, istart, iend, not_done;
    lower << "lower_" << (int)private_num;
    upper << "upper_" << (int)private_num;
    step << "step_" << (int)private_num;
    chunk_size << "chunk_size_" << (int)private_num;
    istart << "istart_" << (int)private_num;
    iend << "iend_" << (int)private_num;
    not_done << "not_done_" << (int)private_num;
    private_num++;

    Source common_initialization;
    common_initialization
        << as_type(induction_var_type) << " " << lower << " = "
        << as_expression(for_statement.get_lower_bound().shallow_copy()) << ";"
        << as_type(induction_var_type) << " " << upper << " = "
        << as_expression(for_statement.get_upper_bound().shallow_copy()) << ";"
        << as_type(induction_var_type) << " " << step << " = "
        << as_expression(for_statement.get_step().shallow_copy()) << ";"
        << as_type(induction_var_type) << " " << chunk_size << " = "
        << as_expression(schedule.get_chunk().shallow_copy()) << ";"
        << "long " << istart << ";"
        << "long " << iend << ";" << as_type(TL::Type::get_bool_type()) << " "
        << not_done << ";";

    TL::Symbol private_induction_var = symbol_map.map(induction_var);
    ERROR_CONDITION(private_induction_var == induction_var, "Induction variable was not privatized", 0);

    Nodecl::NodeclBase loop_body, reduction_code, barrier_code;

    TL::Source lastprivate_code;

    if (!lastprivate_symbols.empty())
    {
        lastprivate_code << "if (" << as_symbol(private_induction_var)
                         << " == " << upper << ") {";
    }

    for (TL::ObjectList<TL::Symbol>::iterator it = lastprivate_symbols.begin();
            it != lastprivate_symbols.end();
            it++)
    {
        if (!it->get_type().is_array())
        {
            lastprivate_code << as_symbol(*it) << "=" << as_symbol(symbol_map.map(*it)) << ";"
                ;
        }
        else
        {
            lastprivate_code
                << "__builtin_memcpy(" << as_symbol(*it) << "," << as_symbol(symbol_map.map(*it))
                << "                 , sizeof(" << as_type(it->get_type().no_ref()) << "));"
                ;

        }
    }

    if (!lastprivate_symbols.empty())
    {
        lastprivate_code << "}";
    }

    typedef std::map<std::string, std::pair<std::string, std::string> >
        schedule_map_t;


    schedule_map_t valid_schedules;
    valid_schedules.insert(std::make_pair(
        "static",
        std::make_pair("GOMP_loop_static_start", "GOMP_loop_static_next")));
    valid_schedules.insert(std::make_pair(
        "dynamic",
        std::make_pair("GOMP_loop_dynamic_start", "GOMP_loop_dynamic_next")));
    valid_schedules.insert(std::make_pair(
        "guided",
        std::make_pair("GOMP_loop_guided_start", "GOMP_loop_guided_next")));
    // GOMP maps auto to static
    valid_schedules.insert(std::make_pair(
        "auto",
        std::make_pair("GOMP_loop_static_start", "GOMP_loop_static_next")));
    valid_schedules.insert(std::make_pair(
        "runtime",
        std::make_pair("GOMP_loop_runtime_start", "GOMP_loop_runtime_next")));

    Source loop_start, loop_next;
    schedule_map_t::iterator schedule_info
        = valid_schedules.find(schedule.get_text());
    if (schedule_info == valid_schedules.end())
    {
        error_printf_at(construct.get_locus(),
                        "'%s' is not a valid OpenMP schedule\n",
                        schedule.get_text().c_str());
        schedule_info = valid_schedules.find("static");
    }
    ERROR_CONDITION(
        schedule_info == valid_schedules.end(), "Invalid schedule", 0);

    loop_start << schedule_info->second.first;
    loop_next << schedule_info->second.second;

    Source sched_loop;
    sched_loop << common_initialization << not_done << " = " << loop_start
               << " (" << lower << ", 1 + (" << upper << "), " << step << ", "
               << chunk_size << ", &" << istart << ", &" << iend << ");"
               << "while (" << not_done << ") {"
               << "for (" << as_symbol(private_induction_var) << " = " << istart
               << "; " << as_symbol(private_induction_var) << " < " << iend
               << ";" << as_symbol(private_induction_var) << "+=" << step << ")"
               << "{" << statement_placeholder(loop_body) << "}" << not_done
               << " = " << loop_next << "(&" << istart << ", &" << iend << ");"
               << "}" << lastprivate_code
               << statement_placeholder(reduction_code)
               << statement_placeholder(barrier_code);

    Nodecl::NodeclBase sched_loop_tree
        = sched_loop.parse_statement(stmt_placeholder);
    stmt_placeholder.prepend_sibling(sched_loop_tree);

    TL::Symbol enclosing_function = Nodecl::Utils::get_enclosing_function(construct);
    if (!reduction_items.empty())
    {
        fatal_printf_at(construct.get_locus(),
                        "reductions not yet implemented");
#if 0
        Source nowait;
        if (barrier_at_end.is_null())
        {
            nowait << "_nowait";
        }

        TL::Symbol callback = emit_callback_for_reduction(
                _lowering->simd_reductions_knc(),
                reduction_items,
                reduction_pack_symbol.get_type(),
                construct, enclosing_function);

        if (callback.is_valid())
        {
            Source master_combiner;
            TL::ObjectList<TL::Symbol> reduction_fields = reduction_pack_symbol.get_type().get_fields();
            TL::ObjectList<TL::Symbol>::iterator it_fields = reduction_fields.begin();
            for (TL::ObjectList<Nodecl::OpenMP::ReductionItem>::iterator it = reduction_items.begin();
                    it != reduction_items.end();
                    it++, it_fields++)
            {
                Nodecl::OpenMP::ReductionItem &current(*it);
                TL::Symbol reduced_symbol = current.get_reduced_symbol().get_symbol();
                TL::Symbol reductor = current.get_reductor().get_symbol();
                OpenMP::Reduction* reduction = OpenMP::Reduction::get_reduction_info_from_symbol(reductor);

                Nodecl::NodeclBase combiner_expr = reduction->get_combiner().shallow_copy();

                ReplaceInOutMaster replace_inout(
                        *it_fields,
                        reduction->get_omp_in(), reduction_pack_symbol,
                        reduction->get_omp_out(), reduced_symbol);
                replace_inout.walk(combiner_expr);

                master_combiner << as_expression(combiner_expr) << ";"
                    ;
            }

            Source reduction_src;
            reduction_src
                << "switch (__kmpc_reduce" << nowait << "(&" << as_symbol(ident_symbol)
                <<               ", __kmpc_global_thread_num(&" << as_symbol(ident_symbol) << ")"
                <<               ", " << reduction_items.size()
                <<               ", sizeof(" << as_type(reduction_pack_symbol.get_type()) << ")"
                <<               ", &" << as_symbol(reduction_pack_symbol)
                <<               ", (void(*)(void*,void*))" << as_symbol(callback)
                <<               ", &" << as_symbol(GOMP::get_global_lock_symbol(construct)) << "))"
                << "{"
                <<    "case 1:"
                <<    "{"
                <<       master_combiner
                <<       "__kmpc_end_reduce" << nowait << "(&" << as_symbol(ident_symbol)
                <<               ", __kmpc_global_thread_num(&" << as_symbol(ident_symbol) << ")"
                <<               ", &" << as_symbol(GOMP::get_global_lock_symbol(construct)) << ");"
                <<       "break;"
                <<    "}"
                <<    "case 0: break;"
                <<    "default: __builtin_abort();"
                << "}"
                ;

            Nodecl::NodeclBase reduction_tree = reduction_src.parse_statement(stmt_placeholder);
            reduction_code.prepend_sibling(reduction_tree);
        }
#endif
    }

    Source barrier_src;
    if (barrier_at_end.is_null())
    {
        barrier_src << "GOMP_loop_end_nowait();";
    }
    else
    {
        barrier_src << "GOMP_loop_end();";
    }
    barrier_code.replace(barrier_src.parse_statement(barrier_code));

    Nodecl::NodeclBase new_statements = Nodecl::Utils::deep_copy(statements,
            loop_body, symbol_map);
    loop_body.replace(new_statements);

    construct.replace(loop_construct_tree);
}

} }

