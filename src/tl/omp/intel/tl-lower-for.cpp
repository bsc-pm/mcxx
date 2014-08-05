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


#include "tl-lowering-visitor.hpp"
#include "tl-lowering-utils.hpp"

#include "tl-lower-reductions.hpp"

#include "tl-counters.hpp"

#include "cxx-diagnostic.h"

namespace TL { namespace Intel {

        void LoweringVisitor::visit(const Nodecl::OpenMP::For& construct)
        {
            lower_for(construct, Nodecl::NodeclBase::null());
        }

        void LoweringVisitor::visit(const Nodecl::OpenMP::ForAppendix& construct)
        {
            Nodecl::NodeclBase appendix = construct.get_appendix();
            if (!appendix.is_null())
                walk(appendix);

            // We cheat a bit in the first parameter
            lower_for(construct.as<Nodecl::OpenMP::For>(), appendix);
        }

        void LoweringVisitor::lower_for(const Nodecl::OpenMP::For& construct,
                const Nodecl::NodeclBase &appendix)
        {
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

            bool is_static_schedule = (schedule.get_text() == "static");

            TL::ObjectList<TL::Symbol> private_symbols;
            TL::ObjectList<TL::Symbol> firstprivate_symbols;
            TL::ObjectList<TL::Symbol> lastprivate_symbols;
            TL::ObjectList<TL::Symbol> reduction_symbols;
            if (!private_list.empty())
            {
                TL::ObjectList<Symbol> tmp =
                    private_list  // TL::ObjectList<OpenMP::Private>
                    .map(functor(&Nodecl::OpenMP::Private::get_symbols)) // TL::ObjectList<Nodecl::NodeclBase>
                    .map(functor(&Nodecl::NodeclBase::as<Nodecl::List>)) // TL::ObjectList<Nodecl::List>
                    .map(functor(&Nodecl::List::to_object_list)) // TL::ObjectList<TL::ObjectList<Nodecl::NodeclBase> >
                    .reduction(functor(TL::append_two_lists<Nodecl::NodeclBase>)) // TL::ObjectList<Nodecl::NodeclBase>
                    .map(functor(&Nodecl::NodeclBase::get_symbol)) // TL::ObjectList<TL::Symbol>
                    ;

                private_symbols.insert(tmp);
            }
            if (!firstprivate_list.empty())
            {
                TL::ObjectList<Symbol> tmp =
                    firstprivate_list  // TL::ObjectList<OpenMP::Firstprivate>
                    .map(functor(&Nodecl::OpenMP::Firstprivate::get_symbols)) // TL::ObjectList<Nodecl::NodeclBase>
                    .map(functor(&Nodecl::NodeclBase::as<Nodecl::List>)) // TL::ObjectList<Nodecl::List>
                    .map(functor(&Nodecl::List::to_object_list)) // TL::ObjectList<TL::ObjectList<Nodecl::NodeclBase> >
                    .reduction(functor(TL::append_two_lists<Nodecl::NodeclBase>)) // TL::ObjectList<Nodecl::NodeclBase>
                    .map(functor(&Nodecl::NodeclBase::get_symbol)) // TL::ObjectList<TL::Symbol>
                    ;

                private_symbols.insert(tmp);
                firstprivate_symbols.insert(tmp);
            }
            if (!lastprivate_list.empty())
            {
                TL::ObjectList<Symbol> tmp =
                    lastprivate_list  // TL::ObjectList<OpenMP::Lastprivate>
                    .map(functor(&Nodecl::OpenMP::Lastprivate::get_symbols)) // TL::ObjectList<Nodecl::NodeclBase>
                    .map(functor(&Nodecl::NodeclBase::as<Nodecl::List>)) // TL::ObjectList<Nodecl::List>
                    .map(functor(&Nodecl::List::to_object_list)) // TL::ObjectList<TL::ObjectList<Nodecl::NodeclBase> >
                    .reduction(functor(TL::append_two_lists<Nodecl::NodeclBase>)) // TL::ObjectList<Nodecl::NodeclBase>
                    .map(functor(&Nodecl::NodeclBase::get_symbol)) // TL::ObjectList<TL::Symbol>
                    ;

                private_symbols.insert(tmp);
                lastprivate_symbols.insert(tmp);
            }
            if (!firstlastprivate_list.empty())
            {
                TL::ObjectList<Symbol> tmp =
                    firstlastprivate_list  // TL::ObjectList<OpenMP::FirstLastprivate>
                    .map(functor(&Nodecl::OpenMP::FirstLastprivate::get_symbols)) // TL::ObjectList<Nodecl::NodeclBase>
                    .map(functor(&Nodecl::NodeclBase::as<Nodecl::List>)) // TL::ObjectList<Nodecl::List>
                    .map(functor(&Nodecl::List::to_object_list)) // TL::ObjectList<TL::ObjectList<Nodecl::NodeclBase> >
                    .reduction(functor(TL::append_two_lists<Nodecl::NodeclBase>)) // TL::ObjectList<Nodecl::NodeclBase>
                    .map(functor(&Nodecl::NodeclBase::get_symbol)) // TL::ObjectList<TL::Symbol>
                    ;

                private_symbols.insert(tmp);
                firstprivate_symbols.insert(tmp);
                lastprivate_symbols.insert(tmp);
            }
            if (!reduction_list.empty())
            {
                TL::ObjectList<Symbol> tmp =
                    reduction_list // TL::ObjectList<OpenMP::Reduction>
                    .map(functor(&Nodecl::OpenMP::Reduction::get_reductions)) // TL::ObjectList<Nodecl::NodeclBase>
                    .map(functor(&Nodecl::NodeclBase::as<Nodecl::List>)) // TL::ObjectList<Nodecl::List>
                    .map(functor(&Nodecl::List::to_object_list_as<Nodecl::OpenMP::ReductionItem>)) // TL::ObjectList<TL::ObjectList<Nodecl::OpenMP::ReductionItem> >
                    .reduction(functor(TL::append_two_lists<Nodecl::OpenMP::ReductionItem>)) // TL::ObjectList<OpenMP::ReductionItem>
                    .map(functor(&Nodecl::OpenMP::ReductionItem::get_reduced_symbol)) // TL::ObjectList<Nodecl::NodeclBase>
                    .map(functor(&Nodecl::NodeclBase::get_symbol)); // TL::ObjectList<TL::Symbol>

                private_symbols.insert(tmp);
                reduction_symbols.insert(tmp);
            }

            Source loop_construct;
            Nodecl::NodeclBase stmt_placeholder;
            loop_construct
                << "{"
                << statement_placeholder(stmt_placeholder)
                << "}";

            Nodecl::NodeclBase loop_construct_tree = loop_construct.parse_statement(construct);

            Nodecl::Utils::SimpleSymbolMap symbol_map;

            TL::Counter &private_num = TL::CounterManager::get_counter("intel-omp-privates");

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

                CXX_LANGUAGE()
                {
                    stmt_placeholder.prepend_sibling(
                            Nodecl::CxxDef::make(
                                /* context */ Nodecl::NodeclBase::null(),
                                new_private_sym));
                }
            }

            Source type_kind; // 4, 4u, 8, 8u
            type_kind << induction_var_type.get_size();
            if (is_unsigned_integral_type(induction_var_type.get_internal_type()))
                type_kind << "u";

            Source static_init, lower, upper, step, lastiter, chunk_size, stride;
            lower << "lower_" << (int)private_num;
            upper << "upper_" << (int)private_num;
            step << "step_" << (int)private_num;
            lastiter << "lastiter_" << (int)private_num;
            chunk_size << "chunk_size_" << (int)private_num;
            stride << "stride_" << (int)private_num;
            private_num++;

            Source common_initialization;
            common_initialization
                    << as_type(induction_var_type) << " " << lower << " = "
                    <<                      as_expression(for_statement.get_lower_bound().shallow_copy()) << ";"
                    << as_type(induction_var_type) << " " << upper << " = "
                    <<                      as_expression(for_statement.get_upper_bound().shallow_copy()) << ";"
                    << as_type(induction_var_type) << " " << step << " = "
                    <<                      as_expression(for_statement.get_step().shallow_copy()) << ";"
                    << as_type(induction_var_type) << " " << chunk_size << " = "
                    <<                                       as_expression(schedule.get_chunk().shallow_copy()) << ";"
                    << as_type(induction_var_type) << " " << stride << ";"
                    << "kmp_int32 " << lastiter << " = 0;"
                    ;

            TL::Symbol private_induction_var = symbol_map.map(induction_var);
            ERROR_CONDITION(private_induction_var == induction_var, "Induction variable was not privatized", 0);

            TL::Symbol ident_symbol = Intel::new_global_ident_symbol(construct);

            Nodecl::NodeclBase loop_body, reduction_code, appendix_code, barrier_code;

            TL::Source lastprivate_code;

            if (!lastprivate_symbols.empty())
            {
                lastprivate_code << "if (" << lastiter << ") {";
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

            if (is_static_schedule)
            {
                Source static_loop;
                static_loop
                    << common_initialization
                    << "__kmpc_for_static_init_" << type_kind << "(&" << as_symbol(ident_symbol)
                    <<                ",__kmpc_global_thread_num(&" << as_symbol(ident_symbol) << ")"
                    <<                ", kmp_sch_static"
                    <<                ", &" << lastiter
                    <<                ", &" << lower
                    <<                ", &" << upper
                    <<                ", &" << stride
                    <<                ", " << step
                    <<                ", " << chunk_size << ");"
                    << "for (" << as_symbol(private_induction_var) << " = " << lower << "; "
                    <<            as_symbol(private_induction_var) << "<=" << upper << ";"
                    <<            as_symbol(private_induction_var) << "+=" << step << ")"
                    << "{"
                    <<     statement_placeholder(loop_body)
                    << "}"
                    << lastprivate_code
                    << "__kmpc_for_static_fini(&" << as_symbol(ident_symbol) << ", __kmpc_global_thread_num("
                    <<                  "&" << as_symbol(ident_symbol) << "));"
                    << statement_placeholder(appendix_code)
                    << statement_placeholder(reduction_code)
                    << statement_placeholder(barrier_code)
                    ;

                Nodecl::NodeclBase static_loop_tree = static_loop.parse_statement(stmt_placeholder);
                stmt_placeholder.prepend_sibling(static_loop_tree);
            }
            else
            {
                Source dynamic_loop;
                Source sched_type, sched_init;
                sched_type << "_sched_" << (int)private_num;
                private_num++;

                std::map<std::string, std::string> valid_schedules;
                valid_schedules.insert(std::make_pair("dynamic", "kmp_sch_dynamic_chunked"));
                valid_schedules.insert(std::make_pair("guided", "kmp_sch_guided_chunked"));
                valid_schedules.insert(std::make_pair("auto", "kmp_sch_auto"));
                valid_schedules.insert(std::make_pair("runtime", "kmp_sch_runtime"));

                if (valid_schedules.find(schedule.get_text()) != valid_schedules.end())
                {
                    sched_init
                        << sched_type << " = " << valid_schedules.find(schedule.get_text())->second << ";"
                        ;
                }
                else
                {
                    error_printf("%s: error '%s' is not a valid OpenMP schedule\n",
                            construct.get_locus_str().c_str(),
                            schedule.get_text().c_str());
                }

                dynamic_loop
                    << "enum sched_type " << sched_type << ";"
                    << sched_init
                    << common_initialization
                    << "__kmpc_dispatch_init_" << type_kind << "(&" << as_symbol(ident_symbol)
                    <<                ",__kmpc_global_thread_num(&" << as_symbol(ident_symbol) << ")"
                    <<                "," << sched_type
                    <<                "," << lower
                    <<                "," << upper
                    <<                "," << step
                    <<                "," << chunk_size << ");"
                    << "while (__kmpc_dispatch_next_" << type_kind << "(&" << as_symbol(ident_symbol)
                    <<                ",__kmpc_global_thread_num(&" << as_symbol(ident_symbol) << ")"
                    <<                ",&" << lastiter
                    <<                ",&" << lower
                    <<                ",&" << upper
                    <<                ",&" << step << "))"
                    << "{"
                    <<     "for (" << as_symbol(private_induction_var) << " = " << lower << ";"
                    <<                as_symbol(private_induction_var) << "<=" << upper << ";"
                    <<                as_symbol(private_induction_var) << "+=" << step << ")"
                    <<     "{"
                    <<         statement_placeholder(loop_body)
                    <<     "}"
                    << "}"
                    << lastprivate_code
                    << statement_placeholder(reduction_code)
                    ;

                Nodecl::NodeclBase dynamic_loop_tree = dynamic_loop.parse_statement(stmt_placeholder);
                stmt_placeholder.prepend_sibling(dynamic_loop_tree);
            }

            TL::Symbol enclosing_function = Nodecl::Utils::get_enclosing_function(construct);
            if (!reduction_list.empty())
            {
                TL::ObjectList<Nodecl::OpenMP::ReductionItem> reduction_items = reduction_list
                    .map(functor(&Nodecl::OpenMP::Reduction::get_reductions))
                    .map(functor(&Nodecl::NodeclBase::as<Nodecl::List>))
                    .map(functor(&Nodecl::List::to_object_list))
                    .reduction(functor(&TL::append_two_lists<Nodecl::NodeclBase>))
                    .map(functor(&Nodecl::NodeclBase::as<Nodecl::OpenMP::ReductionItem>));


                for (TL::ObjectList<Nodecl::OpenMP::ReductionItem>::iterator it = reduction_items.begin();
                        it != reduction_items.end();
                        it++)
                {
                    Source nowait;

                    // If this is the last reduction computed and we need a
                    // barrier, piggyback it in the reduction itself, otherwise
                    // always do reductions without barrier
                    if (barrier_at_end.is_null()
                            || ((it + 1) != reduction_items.end()))
                    {
                        nowait << "_nowait";
                    }

                    Nodecl::OpenMP::ReductionItem &current(*it);

                    TL::Symbol reductor = current.get_reductor().get_symbol();
                    OpenMP::Reduction* omp_reduction = OpenMP::Reduction::get_reduction_info_from_symbol(reductor);

                    TL::Symbol reduced_symbol = current.get_reduced_symbol().get_symbol();

                    TL::Symbol private_symbol = symbol_map.map(reduced_symbol);
                    // FIXME - We should actually update the initializer for omp_orig and omp_priv
                    private_symbol.set_value(omp_reduction->get_initializer().shallow_copy());
                    stmt_placeholder.prepend_sibling(
                            Nodecl::ObjectInit::make(private_symbol)
                            );

                    TL::Symbol callback = emit_callback_for_reduction(omp_reduction, construct, enclosing_function);

                    Nodecl::Utils::SimpleSymbolMap combiner_map;
                    combiner_map.add_map(omp_reduction->get_omp_out(), reduced_symbol);
                    combiner_map.add_map(omp_reduction->get_omp_in(), private_symbol);

                    Source master_combiner;
                    master_combiner
                        << as_expression(
                                Nodecl::Utils::deep_copy(omp_reduction->get_combiner(), construct, combiner_map)
                                ) << ";";

                    Source reduction_src;
                    reduction_src
                        << "switch (__kmpc_reduce" << nowait << "(&" << as_symbol(ident_symbol)
                        <<               ", __kmpc_global_thread_num(&" << as_symbol(ident_symbol) << ")"
                        <<               ", 1"
                        <<               ", sizeof(" << as_type(omp_reduction->get_type()) << ")"
                        <<               ", &" << as_symbol(private_symbol)
                        <<               ", (void(*)(void*,void*))" << as_symbol(callback)
                        <<               ", &" << as_symbol(Intel::get_global_lock_symbol(construct)) << "))"
                        << "{"
                        <<    "case 1:"
                        <<    "{"
                        <<       master_combiner
                        <<       "__kmpc_end_reduce" << nowait << "(&" << as_symbol(ident_symbol)
                        <<               ", __kmpc_global_thread_num(&" << as_symbol(ident_symbol) << ")"
                        <<               ", &" << as_symbol(Intel::get_global_lock_symbol(construct)) << ");"
                        <<       "break;"
                        <<    "}"
                        <<    "case 0: break;"
                        <<    "default: __builtin_abort();"
                        << "}"
                        ;

                    Nodecl::NodeclBase reduction_tree = reduction_src.parse_statement(stmt_placeholder);
                    reduction_code.prepend_sibling(reduction_tree);
                }
            }

            if (!appendix.is_null())
            {
                appendix_code.prepend_sibling(
                        Nodecl::Utils::deep_copy(appendix, appendix_code, symbol_map));
            }

            // If we have to do a barrier, do it only if the reduction list is empty
            // otherwise we piggybacked the barrier in the reductions themselves
            if (!barrier_at_end.is_null() && reduction_list.empty())
            {
                barrier_code.prepend_sibling(
                        emit_barrier(construct)
                        );
            }

            Nodecl::NodeclBase new_statements = Nodecl::Utils::deep_copy(statements,
                    loop_body, symbol_map);

            loop_body.replace(new_statements);

            construct.replace(loop_construct_tree);
        }

} }

