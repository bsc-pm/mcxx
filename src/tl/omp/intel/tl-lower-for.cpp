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

namespace TL { namespace Intel {

        void LoweringVisitor::visit(const Nodecl::OpenMP::For& construct)
        {
            TL::ForStatement for_statement(construct.get_loop().as<Nodecl::ForStatement>());
            ERROR_CONDITION(!for_statement.is_omp_valid_loop(), "Invalid loop at this point", 0);

            Nodecl::List environment = construct.get_environment().as<Nodecl::List>();

            Nodecl::OpenMP::Schedule schedule = environment.find_first<Nodecl::OpenMP::Schedule>();
            ERROR_CONDITION(schedule.is_null(), "Schedule tree is missing", 0);

            Nodecl::NodeclBase statements = for_statement.get_statement();
            walk(statements);
            statements = for_statement.get_statement(); // Should not be necessary

            Nodecl::OpenMP::Shared shared = environment.find_first<Nodecl::OpenMP::Shared>();
            Nodecl::OpenMP::Private private_ = environment.find_first<Nodecl::OpenMP::Private>();
            Nodecl::OpenMP::Firstprivate firstprivate = environment.find_first<Nodecl::OpenMP::Firstprivate>();
            Nodecl::OpenMP::Lastprivate lastprivate = environment.find_first<Nodecl::OpenMP::Lastprivate>();
            Nodecl::OpenMP::FirstLastprivate firstlastprivate = environment.find_first<Nodecl::OpenMP::FirstLastprivate>();
            Nodecl::OpenMP::Reduction reduction = environment.find_first<Nodecl::OpenMP::Reduction>();

            Nodecl::OpenMP::BarrierAtEnd barrier_at_end = environment.find_first<Nodecl::OpenMP::BarrierAtEnd>();

            bool is_static_schedule = (schedule.get_text() == "static");

            TL::ObjectList<TL::Symbol> private_symbols;
            TL::ObjectList<TL::Symbol> firstprivate_symbols;
            TL::ObjectList<TL::Symbol> lastprivate_symbols;
            TL::ObjectList<TL::Symbol> reduction_symbols;
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
            if (!lastprivate.is_null())
            {
                private_symbols.insert(lastprivate
                        .get_lastprivate_symbols()
                        .as<Nodecl::List>()
                        .to_object_list()
                        .map(functor(&Nodecl::NodeclBase::get_symbol)));
                lastprivate_symbols.insert(lastprivate
                        .get_lastprivate_symbols()
                        .as<Nodecl::List>()
                        .to_object_list()
                        .map(functor(&Nodecl::NodeclBase::get_symbol)));
            }
            if (!firstlastprivate.is_null())
            {
                private_symbols.insert(firstlastprivate
                        .get_firstlastprivate_symbols()
                        .as<Nodecl::List>()
                        .to_object_list()
                        .map(functor(&Nodecl::NodeclBase::get_symbol)));
                firstprivate_symbols.insert(firstlastprivate
                        .get_firstlastprivate_symbols()
                        .as<Nodecl::List>()
                        .to_object_list()
                        .map(functor(&Nodecl::NodeclBase::get_symbol)));
                lastprivate_symbols.insert(firstlastprivate
                        .get_firstlastprivate_symbols()
                        .as<Nodecl::List>()
                        .to_object_list()
                        .map(functor(&Nodecl::NodeclBase::get_symbol)));
            }
            if (!reduction.is_null())
            {
                private_symbols.insert(reduction
                        .get_reductions()
                        .as<Nodecl::List>()
                        .to_object_list_as<Nodecl::OpenMP::ReductionItem>()
                        .map(functor(&Nodecl::OpenMP::ReductionItem::get_reduced_symbol))
                        .map(functor(&Nodecl::NodeclBase::get_symbol)));
                reduction_symbols.insert(reduction
                        .get_reductions()
                        .as<Nodecl::List>()
                        .to_object_list_as<Nodecl::OpenMP::ReductionItem>()
                        .map(functor(&Nodecl::OpenMP::ReductionItem::get_reduced_symbol))
                        .map(functor(&Nodecl::NodeclBase::get_symbol)));
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

            Nodecl::NodeclBase loop_body, reduction_code;

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
                    << "__kmpc_for_static_fini(&" << as_symbol(ident_symbol) << ", __kmpc_global_thread_num("
                    <<                  "&" << as_symbol(ident_symbol) << "));"
                    << statement_placeholder(reduction_code)
                    ;

                Nodecl::NodeclBase static_loop_tree = static_loop.parse_statement(stmt_placeholder);
                stmt_placeholder.prepend_sibling(static_loop_tree);
            }
            else
            {
                internal_error("Nonstatic schedules not yet implemented", 0);
            }

            TL::Symbol enclosing_function = Nodecl::Utils::get_enclosing_function(construct);
            if (!reduction.is_null())
            {
                Nodecl::List reduction_items = reduction.get_reductions().as<Nodecl::List>();

                Source nowait;

                Source reduction_src;
                for (Nodecl::List::iterator it = reduction_items.begin();
                        it != reduction_items.end();
                        it++)
                {
                    Nodecl::OpenMP::ReductionItem current (it->as<Nodecl::OpenMP::ReductionItem>());
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

            Nodecl::NodeclBase new_statements = Nodecl::Utils::deep_copy(statements,
                    loop_body, symbol_map);

            loop_body.replace(new_statements);

            construct.replace(loop_construct_tree);
        }

} }

