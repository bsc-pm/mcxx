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
#include "tl-omp-lowering-directive-environment.hpp"
#include "cxx-cexpr.h"

namespace TL { namespace Intel {

using TL::OpenMP::Lowering::DirectiveEnvironment;

void LoweringVisitor::visit(const Nodecl::OpenMP::For& construct)
{
    lower_for(construct, Nodecl::NodeclBase::null(),
            Nodecl::NodeclBase::null());
}

void LoweringVisitor::visit(const Nodecl::OpenMP::ForAppendix& construct)
{
    Nodecl::NodeclBase prependix = construct.get_prependix();
    if (!prependix.is_null())
        walk(prependix);

    Nodecl::NodeclBase appendix = construct.get_appendix();
    if (!appendix.is_null())
        walk(appendix);

    // We cheat a bit in the first parameter
    lower_for(construct.as<Nodecl::OpenMP::For>(), prependix, appendix);
}

void LoweringVisitor::lower_for(const Nodecl::OpenMP::For& construct,
        const Nodecl::NodeclBase &prependix,
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

    DirectiveEnvironment de(environment);

    Nodecl::OpenMP::BarrierAtEnd barrier_at_end = environment.find_first<Nodecl::OpenMP::BarrierAtEnd>();

    bool is_static_schedule = (schedule.get_text() == "static");

    TL::ObjectList<TL::Symbol> private_symbols = de.private_;
    TL::ObjectList<TL::Symbol> firstprivate_symbols = de.captured_value;
    TL::ObjectList<TL::Symbol> lastprivate_symbols = de.lastprivate;
    TL::ObjectList<TL::Symbol> firstlastprivate_symbols = de.firstlastprivate;
    if (!firstprivate_symbols.empty())
    {
        private_symbols.insert(firstprivate_symbols);
    }
    if (!lastprivate_symbols.empty())
    {
        private_symbols.insert(lastprivate_symbols);
    }
    if (!firstlastprivate_symbols.empty())
    {
        private_symbols.insert(firstlastprivate_symbols);
        firstprivate_symbols.insert(firstlastprivate_symbols);
        lastprivate_symbols.insert(firstlastprivate_symbols);
    }

    TL::ObjectList<TL::OpenMP::Lowering::ReductionItem> reduction_items
        = de.reduction;

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

    TL::Symbol reduction_pack_symbol;
    if (!reduction_items.empty())
    {
        TL::ObjectList<Symbol> reduction_symbols = reduction_items
        .map<TL::Symbol>(&TL::OpenMP::Lowering::ReductionItem::get_symbol); // TL::ObjectList<TL::Symbol>

        TL::Symbol reduction_pack_type = declare_reduction_pack(reduction_symbols, construct);
        reduction_pack_symbol = Intel::new_private_symbol(
                "red",
                reduction_pack_type.get_user_defined_type(),
                SK_VARIABLE,
                block_scope);

        // Initialize every member with the neuter
        TL::ObjectList<TL::Symbol> fields = reduction_pack_symbol.get_type().get_fields();
        CXX_LANGUAGE()
        {
            stmt_placeholder.prepend_sibling(
                    Nodecl::CxxDef::make(
                        /* context */ Nodecl::NodeclBase::null(),
                        reduction_pack_symbol));
        }

        TL::ObjectList<TL::Symbol>::iterator it_fields = fields.begin();
        for (auto it = reduction_items.begin();
                it != reduction_items.end();
                it++, it_fields++)
        {
            TL::OpenMP::Lowering::ReductionItem &current(*it);

            OpenMP::Reduction* omp_reduction = current._reduction_info;
            TL::Symbol reduced_symbol = current._symbol;

            Nodecl::NodeclBase init_stmt = omp_reduction->get_initializer().shallow_copy();
            if (omp_reduction->get_is_initialization()) {
                init_stmt = Nodecl::Assignment::make(
                        omp_reduction->get_omp_priv().make_nodecl(/* set_ref_type */ true),
                        init_stmt,
                        omp_reduction->get_omp_priv().get_type().no_ref());
            }
            init_stmt = Nodecl::List::make(Nodecl::ExpressionStatement::make(init_stmt));

            Type type = reduced_symbol.get_type();

            Type base_type = type;
            while (base_type.is_array()) base_type = base_type.array_element();

            std::map<TL::Symbol, Nodecl::NodeclBase> sym_to_nodecl_map;

            if (type.is_array()) {

                TL::Symbol ind_var;
                Nodecl::NodeclBase init_for_stmt = build_for(const_value_to_nodecl(const_value_get_signed_int(0)),
                          reduced_symbol.get_type().array_get_size().shallow_copy(),
                          const_value_to_nodecl(const_value_get_signed_int(1)),
                          init_stmt,
                          construct.retrieve_context(),
                          ind_var);
                sym_to_nodecl_map[omp_reduction->get_omp_priv()] =
                    TL::Source(as_symbol(reduction_pack_symbol)
                               + "."
                               + it_fields->get_name()
                               + "["
                               + as_symbol(ind_var)
                               + "]").parse_expression(construct);
                sym_to_nodecl_map[omp_reduction->get_omp_orig()] =
                    Nodecl::ArraySubscript::make(
                            reduced_symbol.make_nodecl(/* set_ref_type */ true),
                            Nodecl::List::make(
                                ind_var.make_nodecl(/* set_ref_type */ true)),
                            base_type);

                TranslateReductionExpr nodecl_replacer(sym_to_nodecl_map);
                nodecl_replacer.walk(init_stmt);
                stmt_placeholder.prepend_sibling(init_for_stmt);
            }
            else {
                sym_to_nodecl_map[omp_reduction->get_omp_priv()] =
                    TL::Source(as_symbol(reduction_pack_symbol)
                               + "."
                               + it_fields->get_name()).parse_expression(construct);
                sym_to_nodecl_map[omp_reduction->get_omp_orig()] =
                            reduced_symbol.make_nodecl(/* set_ref_type */ true);

                TranslateReductionExpr nodecl_replacer(sym_to_nodecl_map);
                nodecl_replacer.walk(init_stmt);
                stmt_placeholder.prepend_sibling(init_stmt);
            }

        }
    }

    Source type_kind; // 4, 4u, 8, 8u
    Source type_unsigned;
    Source type_bit_size; // 32, 64

    type_kind << induction_var_type.get_size();
    type_bit_size << (induction_var_type.get_size() == 4 ? "32" : "64");
    if (is_unsigned_integral_type(induction_var_type.get_internal_type()))
        type_unsigned << "u";

    type_kind << type_unsigned;

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
            << "kmp_" << type_unsigned << "int" << type_bit_size << " " << lower << " = "
            <<                      as_expression(for_statement.get_lower_bound().shallow_copy()) << ";"
            << "kmp_" << type_unsigned << "int" << type_bit_size << " " << upper << " = "
            <<                      as_expression(for_statement.get_upper_bound().shallow_copy()) << ";"
            << "kmp_int" << type_bit_size << " " << step << " = "
            <<                      as_expression(for_statement.get_step().shallow_copy()) << ";"
            << "kmp_int" << type_bit_size << " " << chunk_size << " = "
            <<                      as_expression(schedule.get_chunk().shallow_copy()) << ";"
            << "kmp_int" << type_bit_size << " " << stride << ";"
            << "kmp_int32 " << lastiter << " = 0;"
            ;

    Source pragma_noprefetch;
    Nodecl::OpenMP::NoPrefetch no_prefetch_node = environment.find_first<Nodecl::OpenMP::NoPrefetch>();
    if (!no_prefetch_node.is_null())
    {
        pragma_noprefetch
            << "\n"
            << "#pragma noprefetch\n";
    }

    TL::Symbol private_induction_var = symbol_map.map(induction_var);
    ERROR_CONDITION(private_induction_var == induction_var, "Induction variable was not privatized", 0);

    TL::Symbol ident_symbol = Intel::new_global_ident_symbol(construct);

    Nodecl::NodeclBase loop_body, reduction_code, prependix_code, appendix_code, barrier_code;

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
            << statement_placeholder(prependix_code)
            << pragma_noprefetch
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
            error_printf_at(construct.get_locus(),
                    "'%s' is not a valid OpenMP schedule\n",
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
            << pragma_noprefetch
            << "while (__kmpc_dispatch_next_" << type_kind << "(&" << as_symbol(ident_symbol)
            <<                ",__kmpc_global_thread_num(&" << as_symbol(ident_symbol) << ")"
            <<                ",&" << lastiter
            <<                ",&" << lower
            <<                ",&" << upper
            <<                ",&" << step << "))"
            << "{"
            << statement_placeholder(prependix_code)
            <<     "for (" << as_symbol(private_induction_var) << " = " << lower << ";"
            <<                as_symbol(private_induction_var) << "<=" << upper << ";"
            <<                as_symbol(private_induction_var) << "+=" << step << ")"
            <<     "{"
            <<         statement_placeholder(loop_body)
            <<     "}"
            << "}"
            << lastprivate_code
            << statement_placeholder(appendix_code)
            << statement_placeholder(reduction_code)
            << statement_placeholder(barrier_code)
            ;

        Nodecl::NodeclBase dynamic_loop_tree = dynamic_loop.parse_statement(stmt_placeholder);
        stmt_placeholder.prepend_sibling(dynamic_loop_tree);
    }

    TL::Symbol enclosing_function = Nodecl::Utils::get_enclosing_function(construct);
    if (!reduction_items.empty())
    {
        Source nowait;
        if (barrier_at_end.is_null())
        {
            nowait << "_nowait";
        }

        TL::Symbol callback = emit_callback_for_reduction(
                _lowering->get_combiner_isa(),
                reduction_items,
                reduction_pack_symbol.get_type(),
                construct, enclosing_function);

        if (callback.is_valid())
        {
            Source master_combiner;
            TL::ObjectList<TL::Symbol> reduction_fields = reduction_pack_symbol.get_type().get_fields();
            TL::ObjectList<TL::Symbol>::iterator it_fields = reduction_fields.begin();
            for (auto it = reduction_items.begin();
                    it != reduction_items.end();
                    it++, it_fields++)
            {
                TL::OpenMP::Lowering::ReductionItem &current(*it);

                OpenMP::Reduction* reduction = current._reduction_info;
                TL::Symbol reduced_symbol = current._symbol;

                Nodecl::NodeclBase combiner_expr = reduction->get_combiner().shallow_copy();
                Nodecl::NodeclBase red_item_comb_stmt = Source(as_expression(combiner_expr) + ";").parse_statement(construct);

                Type type = reduced_symbol.get_type();

                Type base_type = type;
                while (base_type.is_array()) base_type = base_type.array_element();

                std::map<TL::Symbol, Nodecl::NodeclBase> sym_to_nodecl_map;

                if (type.is_array()) {
                    TL::Symbol ind_var;
                    master_combiner << as_statement(build_for(const_value_to_nodecl(const_value_get_signed_int(0)),
                              reduced_symbol.get_type().array_get_size().shallow_copy(),
                              const_value_to_nodecl(const_value_get_signed_int(1)),
                              red_item_comb_stmt,
                              construct.retrieve_context(),
                              ind_var));
                    sym_to_nodecl_map[reduction->get_omp_in()] =
                        TL::Source(as_symbol(reduction_pack_symbol)
                                   + "."
                                   + it_fields->get_name()
                                   + "["
                                   + as_symbol(ind_var)
                                   + "]").parse_expression(construct);
                    sym_to_nodecl_map[reduction->get_omp_out()] =
                        Nodecl::ArraySubscript::make(
                                reduced_symbol.make_nodecl(/* set_ref_type */ true),
                                Nodecl::List::make(
                                    ind_var.make_nodecl(/* set_ref_type */ true)),
                                base_type);

                    TranslateReductionExpr nodecl_replacer(sym_to_nodecl_map);
                    nodecl_replacer.walk(red_item_comb_stmt);
                }
                else {
                    sym_to_nodecl_map[reduction->get_omp_in()] =
                        TL::Source(as_symbol(reduction_pack_symbol)
                                   + "."
                                   + it_fields->get_name()).parse_expression(construct);
                    sym_to_nodecl_map[reduction->get_omp_out()] =
                                reduced_symbol.make_nodecl(/* set_ref_type */ true);

                    TranslateReductionExpr nodecl_replacer(sym_to_nodecl_map);
                    nodecl_replacer.walk(red_item_comb_stmt);
                    master_combiner << as_statement(red_item_comb_stmt);
                }
            }

            Source reduction_size;
            Source reduction_data;
            Source reduction_extra_pre;

            if (!_lowering->simd_reductions())
            {
                reduction_size << "sizeof(" << as_type(reduction_pack_symbol.get_type()) << ")";
                reduction_data << "&" << as_symbol(reduction_pack_symbol);
            }
            else
            {
                TL::Symbol array_of_sizes = Intel::new_private_symbol("red_sizes",
                        TL::Type::get_size_t_type().get_array_to(
                            const_value_to_nodecl(
                                const_value_get_signed_int(reduction_items.size())
                                ),
                            block_scope),
                        SK_VARIABLE,
                        block_scope);

                TL::Symbol array_of_data = Intel::new_private_symbol("red_data",
                        TL::Type::get_void_type()
                            .get_pointer_to()
                            .get_array_to(
                                const_value_to_nodecl(
                                    const_value_get_signed_int(reduction_items.size())
                                    ),
                                block_scope),
                        SK_VARIABLE,
                        block_scope);

                reduction_size << as_symbol(array_of_sizes);
                // Huh, we are passing a pointer as an integer (a size_t)
                reduction_data << as_symbol(array_of_data);

                // Initialize both arrays
                TL::ObjectList<Nodecl::NodeclBase> reduction_data_items;
                TL::ObjectList<Nodecl::NodeclBase> reduction_sizes;

                for (TL::ObjectList<TL::Symbol>::iterator it = reduction_fields.begin();
                        it != reduction_fields.end();
                        it++)
                {
                    reduction_sizes.append(
                            const_value_to_nodecl_with_basic_type(
                                const_value_get_integer(
                                    it->get_type().get_size(),
                                    TL::Type::get_size_t_type().get_size(),
                                    /* sign */ 0),
                                TL::Type::get_size_t_type().get_internal_type()));
                    reduction_data_items.append(
                            Nodecl::Reference::make(
                                Nodecl::ClassMemberAccess::make(
                                    reduction_pack_symbol.make_nodecl(/* set_ref_type */ true),
                                    it->make_nodecl(),
                                    Nodecl::NodeclBase::null(),
                                    it->get_type().no_ref().get_lvalue_reference_to()),
                                it->get_type().no_ref().get_pointer_to())
                            );
                }

                Nodecl::NodeclBase array_of_sizes_initializer =
                    Nodecl::StructuredValue::make(
                            Nodecl::List::make(reduction_sizes),
                            Nodecl::StructuredValueBracedImplicit::make(),
                            array_of_sizes.get_type());
                array_of_sizes.set_value(array_of_sizes_initializer);

                Nodecl::NodeclBase array_of_data_initializer =
                    Nodecl::StructuredValue::make(
                            Nodecl::List::make(reduction_data_items),
                            Nodecl::StructuredValueBracedImplicit::make(),
                            array_of_data.get_type());
                array_of_data.set_value(array_of_data_initializer);

                reduction_extra_pre
                    << as_statement(
                            Nodecl::ObjectInit::make(array_of_sizes))
                    << as_statement(
                            Nodecl::ObjectInit::make(array_of_data));
            }

            Source reduction_src;
            reduction_src
                << reduction_extra_pre
                << "switch (__kmpc_reduce" << nowait << "(&" << as_symbol(ident_symbol)
                <<               ", __kmpc_global_thread_num(&" << as_symbol(ident_symbol) << ")"
                <<               ", " << reduction_items.size()
                <<               ", " << reduction_size
                <<               ", " << reduction_data
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

    if (!prependix.is_null())
    {
        Nodecl::NodeclBase lower_node =
            lower.parse_expression(stmt_placeholder);

        // Replace IV by IV LB
        Nodecl::Utils::nodecl_replace_nodecl_by_structure(
                prependix, /* haystack */
                induction_var.make_nodecl(true), /* needle */
                lower_node /* replacement */);

        Nodecl::NodeclBase prep =
                Nodecl::Utils::deep_copy(prependix, prependix_code, symbol_map);
        update_reduction_uses(prep, reduction_items, reduction_pack_symbol);
        prependix_code.prepend_sibling(prep);
    }

    if (!appendix.is_null())
    {
        Nodecl::NodeclBase appe = Nodecl::Utils::deep_copy(appendix, appendix_code, symbol_map);
        update_reduction_uses(appe, reduction_items, reduction_pack_symbol);
        appendix_code.prepend_sibling(appe);
    }

    // If we have to do a barrier, do it only if the reduction list is empty
    // otherwise we piggybacked the barrier in the reductions themselves
    if (!barrier_at_end.is_null() && reduction_items.empty())
    {
        barrier_code.prepend_sibling(
                emit_barrier(construct)
                );
    }

    Nodecl::NodeclBase new_statements = Nodecl::Utils::deep_copy(statements,
            loop_body, symbol_map);
    if (!reduction_items.empty())
    {
        update_reduction_uses(new_statements, reduction_items, reduction_pack_symbol);
    }

    loop_body.replace(new_statements);

    construct.replace(loop_construct_tree);
}

} }

