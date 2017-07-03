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

#include "hlt-loop-collapse.hpp"

#include "hlt-utils.hpp"
#include "tl-nodecl-utils.hpp"

#include "cxx-cexpr.h"
#include "cxx-diagnostic.h"

namespace TL { namespace HLT {

    LoopCollapse::LoopCollapse()
        : _loop(), _transformation(), _collapse_factor(-1)
    {
    }

    LoopCollapse& LoopCollapse::set_loop(Nodecl::NodeclBase loop)
    {
        _loop = loop;

        return *this;
    }

    LoopCollapse& LoopCollapse::set_collapse_factor(int collapse_factor)
    {
        _collapse_factor = collapse_factor;

        return *this;
    }

    LoopCollapse& LoopCollapse::set_pragma_context(const TL::Scope& context)
    {
        _pragma_context = context;

        return *this;
    }

    Nodecl::NodeclBase LoopCollapse::get_post_transformation_stmts() const
    {
        return _post_transformation_stmts;
    }

    TL::ObjectList<TL::Symbol> LoopCollapse::get_omp_capture_symbols() const
    {
        return _omp_capture_symbols;
    }

    namespace {

        struct LoopInfo
        {
            TL::Symbol induction_var;
            Nodecl::NodeclBase lower_bound;
            Nodecl::NodeclBase upper_bound;
            Nodecl::NodeclBase step;
        };

        void check_loop(Nodecl::NodeclBase node, int collapse_factor)
        {
            TL::ObjectList<TL::Symbol> induction_variables;
            for (int i = 0; i < collapse_factor; ++i)
            {
                if (!node.is<Nodecl::ForStatement>())
                    fatal_printf_at(
                            node.get_locus(),
                            "Trying to collapse %d 'For' loop(s) but only %d loop(s) found\n",
                            collapse_factor, i + 1);

                TL::ForStatement for_stmt(node.as<Nodecl::ForStatement>());

                if (!for_stmt.is_omp_valid_loop())
                    fatal_printf_at(
                            node.get_locus(),
                            "Trying to collapse an invalid 'For' loop in nesting level (%d)\n",
                            i + 1);

                struct LoopControlSymbolVisitor : public Nodecl::ExhaustiveVisitor<void>
                {
                    const TL::ObjectList<TL::Symbol>& _symbol_list;
                    bool _containsAnySymbol;

                    LoopControlSymbolVisitor(const TL::ObjectList<TL::Symbol>& symbol_list)
                        : _symbol_list(symbol_list), _containsAnySymbol(false)
                    { }

                    void visit(const Nodecl::Symbol& node)
                    {
                        TL::Symbol sym = node.get_symbol();
                        if (!sym.is_variable())
                            return;

                        walk(sym.get_value());

                        for (TL::ObjectList<TL::Symbol>::const_iterator it = _symbol_list.begin();
                                it != _symbol_list.end();
                                it++)
                        {
                            if (sym == *it)
                            {
                                _containsAnySymbol = true;
                                break;
                            }
                        }
                    }
                };

                LoopControlSymbolVisitor loopControlVisitor(induction_variables);
                loopControlVisitor.walk(for_stmt.get_loop_header());
                if (loopControlVisitor._containsAnySymbol)
                {
                    fatal_printf_at(
                            node.get_locus(),
                            "Trying to collapse a non-canonical loop: induction variable found in nested loop header\n");
                }
                induction_variables.append(for_stmt.get_induction_variable());

                node = for_stmt.get_statement()
                    .as<Nodecl::List>().front()
                    .as<Nodecl::Context>().get_in_context();

                ERROR_CONDITION(node.is_null(),
                        "Empty context within 'For'", 0);

                node = node
                    .as<Nodecl::List>().front();

                // Compound statement found in this level only for C/C++
                if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
                {
                    node = node
                        .as<Nodecl::CompoundStatement>().get_statements();

                    // Last nested loop won't necessarily have a nested
                    // context within the compound statement
                    if (i < collapse_factor - 1)
                    {
                        if (node.is_null())
                            fatal_printf_at(
                                    node.get_locus(),
                                    "Trying to collapse %d 'For' loop(s) but only %d loop(s) found\n",
                                    collapse_factor, i + 1);

                        node = node.as<Nodecl::List>().front();

                        if (!node.is<Nodecl::Context>())
                            fatal_printf_at(
                                    node.get_locus(),
                                    "Trying to collapse %d 'For' loop(s) but only %d loop(s) found\n",
                                    collapse_factor, i + 1);

                        node = node.as<Nodecl::Context>().get_in_context();

                        ERROR_CONDITION(node.is_null(),
                                "Empty context within 'Compound statement'", 0);

                        node = node.as<Nodecl::List>().front();
                    }
                }
            }
        }

        void compute_collapse_statements(
                int collapse_factor,
                TL::Symbol collapse_induction_var,
                Nodecl::Context context_innermost_loop,
                // Inout
                TL::ObjectList<LoopInfo>& loop_info,
                TL::Scope& collapse_scope,
                TL::Scope& loop_statements_scope,
                // Out
                Nodecl::List& collapse_statements,
                Nodecl::List& new_loop_statements,
                Nodecl::NodeclBase& condition_bound,
                TL::ObjectList<TL::Symbol>& omp_capture_symbols)
        {
            if (IS_CXX_LANGUAGE)
            {
                collapse_statements.append(
                        Nodecl::CxxDef::make(
                            /*context*/ Nodecl::NodeclBase::null(),
                            collapse_induction_var));
            }

            Nodecl::Utils::SimpleSymbolMap induction_var_map;

            // This node must be initialized with the value '1LLU' for ensuring
            // the proper type of the final expression
            //
            // This lower level call is used instead of 'const_value_to_nodecl'
            // because otherwise the resulting type would be restricted to the
            // smallest type where the constant fits
            Nodecl::NodeclBase num_elem_in_nested_loops =
                const_value_to_nodecl_with_basic_type(
                        const_value_get_unsigned_long_long_int(1),
                        TL::Type::get_unsigned_long_long_int_type().get_internal_type());

            // From innermost loop to outermost
            for (int i = collapse_factor - 1; i >= 0; --i)
            {
                // Computing required information

                // step variable
                std::stringstream step_ss;
                step_ss << "collapse_" << i << "_step";

                TL::Symbol step_var = collapse_scope.new_symbol(step_ss.str());
                symbol_entity_specs_set_is_user_declared(step_var.get_internal_symbol(), 1);
                step_var.get_internal_symbol()->kind = SK_VARIABLE;
                step_var.set_type(loop_info[i].step.get_type().no_ref().get_unqualified_type());
                step_var.set_value(loop_info[i].step);

                omp_capture_symbols.insert(step_var);

                // num_elem variable
                std::stringstream num_elem_ss;
                num_elem_ss << "collapse_" << i << "_num_elements";

                TL::Symbol num_elem_var = collapse_scope.new_symbol(num_elem_ss.str());
                symbol_entity_specs_set_is_user_declared(num_elem_var.get_internal_symbol(), 1);
                num_elem_var.get_internal_symbol()->kind = SK_VARIABLE;
                num_elem_var.set_type(loop_info[i].upper_bound.get_type().no_ref().get_unqualified_type());

                // This expression computes the ceiling division between the iteration space
                // and the step, to get the effective number of elements for this loop.
                //
                //     - Ceiling division A/B can be computed as (A + B +/- 1)/B
                //       (+/- depending on the sign of A and B)
                //
                //     - TL::ForStatement utility normalizes the upper bound (UB) so that
                //       this +/- 1 is already computed in its value, therefore, the ceiling
                //       division is computed as (UB - LB + step) / step
                num_elem_var.set_value(Nodecl::Div::make(
                            Nodecl::ParenthesizedExpression::make(
                                Nodecl::Add::make(
                                    Nodecl::Minus::make(
                                        Nodecl::ParenthesizedExpression::make(
                                            loop_info[i].upper_bound,
                                            loop_info[i].upper_bound.get_type()),
                                        Nodecl::ParenthesizedExpression::make(
                                            loop_info[i].lower_bound,
                                            loop_info[i].lower_bound.get_type()),
                                        loop_info[i].lower_bound.get_type()),
                                    step_var.make_nodecl(/* set_ref_type */ true),
                                    step_var.get_type()),
                                step_var.get_type()),
                            step_var.make_nodecl(/* set_ref_type */ true),
                            step_var.get_type()));

                omp_capture_symbols.insert(num_elem_var);

                // Effective number of elements must be positive or zero
                Nodecl::NodeclBase num_elem_update;
                if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
                {
                    num_elem_update = Nodecl::ExpressionStatement::make(
                            Nodecl::Assignment::make(
                                num_elem_var.make_nodecl(/* set_ref_type */ true),
                                Nodecl::ConditionalExpression::make(
                                    Nodecl::LowerThan::make(
                                        num_elem_var.make_nodecl(/* set_ref_type */ true),
                                        const_value_to_nodecl(const_value_get_signed_int(0)),
                                        num_elem_var.get_type()),
                                    const_value_to_nodecl(const_value_get_signed_int(0)),
                                    num_elem_var.make_nodecl(/* set_ref_type */ true),
                                    num_elem_var.get_type()),
                                num_elem_var.get_type().get_lvalue_reference_to()));
                }
                else
                {
                    num_elem_update = Nodecl::IfElseStatement::make(
                            Nodecl::LowerThan::make(
                                num_elem_var.make_nodecl(/* set_ref_type */ true),
                                const_value_to_nodecl(const_value_get_signed_int(0)),
                                num_elem_var.get_type()),
                            Nodecl::List::make(
                                Nodecl::ExpressionStatement::make(
                                    Nodecl::Assignment::make(
                                        num_elem_var.make_nodecl(/* set_ref_type */ true),
                                        const_value_to_nodecl(const_value_get_signed_int(0)),
                                        num_elem_var.get_type()))),
                            /* else */ Nodecl::NodeclBase::null());
                }

                // rounded_size variable
                std::stringstream rounded_size_ss;
                rounded_size_ss << "collapse_" << i << "_rounded_size";

                TL::Symbol rounded_size_var = collapse_scope.new_symbol(rounded_size_ss.str());
                symbol_entity_specs_set_is_user_declared(rounded_size_var.get_internal_symbol(), 1);
                rounded_size_var.get_internal_symbol()->kind = SK_VARIABLE;
                rounded_size_var.set_type(num_elem_var.get_type());

                // step could be < 0 while num_elem won't, and thus making the
                // rounded_size also < 0. However, this is not a problem because
                // (X % rounded_size) is always equal to (X % -rounded_size)
                rounded_size_var.set_value(Nodecl::Mul::make(
                            num_elem_var.make_nodecl(/* set_ref_type */ true),
                            step_var.make_nodecl(/* set_ref_type */ true),
                            num_elem_var.get_type()));

                omp_capture_symbols.insert(rounded_size_var);

                // Append definition statements
                collapse_statements.append(Nodecl::ObjectInit::make(step_var));
                collapse_statements.append(Nodecl::ObjectInit::make(num_elem_var));
                collapse_statements.append(num_elem_update);
                collapse_statements.append(Nodecl::ObjectInit::make(rounded_size_var));

                // Handle induction variable

                // Create induction variable expression like
                // 'LB + ((orig_induction_var/num_elem')*step)%rounded_size' where
                // num_elem' is the product of num_elem from previous iterations
                //
                // current_element_number is a partial computation that
                // represents the ordinal position of the current element in
                // this dimension (effective elements only), precise index
                // value is to be computed from it
                Nodecl::NodeclBase current_element_number = Nodecl::Conversion::make(
                        Nodecl::ParenthesizedExpression::make(
                            Nodecl::Div::make(
                                collapse_induction_var.make_nodecl(/* set_ref_type */ true),
                                Nodecl::ParenthesizedExpression::make(
                                    num_elem_in_nested_loops,
                                    num_elem_in_nested_loops.get_type()),
                                num_elem_in_nested_loops.get_type()),
                            num_elem_in_nested_loops.get_type()),
                        // This type should match the the type of the variable
                        // holding the total number of elements (as well as the
                        // induction variable), but in its signed form
                        TL::Type::get_long_long_int_type());
                current_element_number.set_text("C");

                Nodecl::NodeclBase induction_var_expr = Nodecl::Conversion::make(
                        Nodecl::Add::make(
                            loop_info[i].lower_bound.shallow_copy(),
                            Nodecl::Mod::make(
                                Nodecl::ParenthesizedExpression::make(
                                    Nodecl::Mul::make(
                                        current_element_number,
                                        step_var.make_nodecl(/* set_ref_type */ true),
                                        step_var.get_type()),
                                    step_var.get_type()),
                                rounded_size_var.make_nodecl(/* set_ref_type */ true),
                                rounded_size_var.get_type()),
                            loop_info[i].lower_bound.get_type()),
                        loop_info[i].induction_var.get_type());
                induction_var_expr.set_text("C");

                // Create/reuse induction variable
                Nodecl::NodeclBase new_stmt;

                // If current (new) scope IS enclosed by induction var scope,
                // the induction var is NOT declared inside the original loop, so we assign to it
                if (collapse_scope.scope_is_enclosed_by(loop_info[i].induction_var.get_scope()))
                {
                    new_stmt = Nodecl::ExpressionStatement::make(
                            Nodecl::Assignment::make(
                                loop_info[i].induction_var.make_nodecl(/* set_ref_type */ true),
                                induction_var_expr,
                                loop_info[i].induction_var.get_type().no_ref().get_lvalue_reference_to()));

                    // If we decide to reuse the symbol we should privatize it
                    omp_capture_symbols.insert(loop_info[i].induction_var);
                }
                // We need to create a new variable and map it to the original
                // symbol to deep_copy it, object initialization required
                else
                {
                    TL::Symbol induction_var = loop_statements_scope.new_symbol("collapse_" + loop_info[i].induction_var.get_name());
                    symbol_entity_specs_set_is_user_declared(induction_var.get_internal_symbol(), 1);
                    induction_var.get_internal_symbol()->kind = SK_VARIABLE;
                    induction_var.set_type(loop_info[i].induction_var.get_type());
                    induction_var.set_value(induction_var_expr);

                    induction_var_map.add_map(loop_info[i].induction_var, induction_var);

                    // Object initialization
                    new_stmt = Nodecl::ObjectInit::make(induction_var);
                }

                new_loop_statements.prepend(new_stmt);

                // Update 'num_elem_in_nested_loops' for next iteration,
                // parenthesizes are required to ensure type (1LLU) is properly
                // propagated in Fortran
                num_elem_in_nested_loops = Nodecl::Mul::make(
                        num_elem_var.make_nodecl(/* set_ref_type */ true),
                        Nodecl::ParenthesizedExpression::make(
                            num_elem_in_nested_loops.shallow_copy(),
                            num_elem_in_nested_loops.get_type()),
                        num_elem_in_nested_loops.get_type());
            }

            // Compute condition bound
            condition_bound = num_elem_in_nested_loops;

            // Deep copy to replace loop code to use new induction variables
            Nodecl::Context new_context_innermost_loop =
                Nodecl::Utils::deep_copy(
                        context_innermost_loop,
                        context_innermost_loop,
                        induction_var_map).as<Nodecl::Context>();

            new_loop_statements.append(new_context_innermost_loop);

            //Properly linking the new_context of the innermost loop with the loop_statements_scope
            TL::Scope scope_innermost_loop = context_innermost_loop.retrieve_context();
            scope_innermost_loop.get_decl_context()->current_scope->contained_in =
                loop_statements_scope.get_decl_context()->current_scope;
        }

        void compute_loop_information(
                Nodecl::NodeclBase node,
                int collapse_factor,
                // Out
                TL::ObjectList<LoopInfo>& loop_info,
                Nodecl::Context& context_innermost_loop,
                Nodecl::List& post_trasformation_stmts)
        {
            for (int i = 0; i < collapse_factor; ++i)
            {
                TL::ForStatement for_stmt(node.as<Nodecl::ForStatement>());

                LoopInfo info;

                info.induction_var = for_stmt.get_induction_variable();
                info.lower_bound = for_stmt.get_lower_bound();
                info.upper_bound = for_stmt.get_upper_bound();
                info.step = for_stmt.get_step();

                loop_info.append(info);

                context_innermost_loop =
                    for_stmt.get_statement().as<Nodecl::List>().front().as<Nodecl::Context>();

                node = context_innermost_loop.get_in_context();

                // Compute what value the induction variable should take after the loop
                if (!for_stmt.induction_variable_in_separate_scope())
                {
                    Nodecl::NodeclBase induction_variable =
                        for_stmt.get_induction_variable().make_nodecl(/* set_ref_type */ true);

                    Nodecl::NodeclBase expr =
                        HLT::Utils::compute_induction_variable_final_expr(for_stmt);

                    post_trasformation_stmts.append(
                            Nodecl::ExpressionStatement::make(
                                Nodecl::Assignment::make(
                                    induction_variable,
                                    expr,
                                    induction_variable.get_type())));
                }

                // Advance to next nested loop
                // Compound statement found in this level only for C/C++
                if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
                {
                    node = node
                        .as<Nodecl::List>().front()
                        .as<Nodecl::CompoundStatement>().get_statements();

                    // Last nested loop won't necessarily have a nested
                    // context within the compound statement
                    if (i < collapse_factor - 1)
                    {
                        node = node
                            .as<Nodecl::List>().front()
                            .as<Nodecl::Context>().get_in_context()
                            .as<Nodecl::List>().front();
                    }
                }
                else if (IS_FORTRAN_LANGUAGE && i < collapse_factor - 1)
                {
                    node = node
                        .as<Nodecl::List>().front();
                }
            }
        }
    }

    void LoopCollapse::collapse()
    {
        // Gather loop information

        check_loop(_loop, _collapse_factor);

        TL::ObjectList<LoopInfo> loop_info;
        Nodecl::Context context_innermost_loop;

        compute_loop_information(_loop, _collapse_factor, loop_info,
                context_innermost_loop, _post_transformation_stmts);

        // Compute collapse statements

        /*
                { // <- collapse_scope
                    ...
                    for ( // <-> loop_control_scope ... )
                    { // <- loop_statements_scope
                      ...
                    } // -> loop_statements_scope
                } // -> collapse_scope
         */
        TL::Scope collapse_scope = TL::Scope(
                new_block_context(_pragma_context.get_decl_context()));

        TL::Scope loop_control_scope = TL::Scope(
                new_block_context(collapse_scope.get_decl_context()));

        TL::Scope loop_statements_scope = TL::Scope(
                new_block_context(loop_control_scope.get_decl_context()));

        TL::Symbol induction_var = collapse_scope.new_symbol("collapse_it");
        symbol_entity_specs_set_is_user_declared(induction_var.get_internal_symbol(), 1);
        induction_var.get_internal_symbol()->kind = SK_VARIABLE;
        induction_var.set_type(TL::Type::get_unsigned_long_long_int_type());
        induction_var.set_value(const_value_to_nodecl(const_value_get_signed_int(0)));

        Nodecl::List collapse_statements;
        Nodecl::NodeclBase condition_bound;

        Nodecl::List new_loop_statements;
        compute_collapse_statements(
                _collapse_factor, induction_var, context_innermost_loop,
                loop_info, collapse_scope, loop_statements_scope,
                collapse_statements, new_loop_statements, condition_bound, _omp_capture_symbols);

        TL::Symbol condition_bound_var = collapse_scope.new_symbol("collapse_total_num_elements");
        symbol_entity_specs_set_is_user_declared(condition_bound_var.get_internal_symbol(), 1);
        condition_bound_var.get_internal_symbol()->kind = SK_VARIABLE;
        condition_bound_var.set_type(TL::Type::get_unsigned_long_long_int_type());

        collapse_statements.append(Nodecl::ObjectInit::make(condition_bound_var));

        // Create new For statement

        Nodecl::NodeclBase loop_control;

        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
        {
            condition_bound_var.set_value(condition_bound);

            Nodecl::NodeclBase assignment = Nodecl::Assignment::make(
                    induction_var.make_nodecl(/* set_ref_type */ true),
                    const_value_to_nodecl(const_value_get_signed_int(0)),
                    induction_var.get_type().no_ref());

            Nodecl::NodeclBase condition = Nodecl::LowerThan::make(
                    induction_var.make_nodecl(/* set_ref_type */ true),
                    condition_bound_var.make_nodecl(/* set_ref_type */ true),
                    TL::Type::get_bool_type());

            Nodecl::NodeclBase step = Nodecl::Preincrement::make(
                    induction_var.make_nodecl(/* set_ref_type */ true),
                    induction_var.get_type());

            loop_control = Nodecl::LoopControl::make(
                    Nodecl::List::make(assignment),
                    condition,
                    step);
        }
        else
        {
            condition_bound_var.set_value(Nodecl::Minus::make(
                        condition_bound,
                        const_value_to_nodecl(const_value_get_signed_int(1)),
                        condition_bound.get_type().no_ref()));

            Nodecl::NodeclBase step =
                const_value_to_nodecl(const_value_get_one(/* bytes */ 4, /* signed */ 1));

            loop_control = Nodecl::RangeLoopControl::make(
                    induction_var.make_nodecl(/* set_ref_type */ true),
                    const_value_to_nodecl(const_value_get_signed_int(0)),
                    condition_bound_var.make_nodecl(/* set_ref_type */ true),
                    step);
        }

        Nodecl::NodeclBase collapsed_loop = Nodecl::Context::make(
                Nodecl::List::make(
                    Nodecl::ForStatement::make(
                        loop_control,
                        Nodecl::List::make(
                            Nodecl::Context::make(
                                Nodecl::List::make(
                                    Nodecl::CompoundStatement::make(
                                        new_loop_statements,
                                        /* finally */ Nodecl::NodeclBase::null())),
                                loop_statements_scope)),
                        /* name */ Nodecl::NodeclBase::null(),
                        _loop.get_locus())),
                loop_control_scope.get_decl_context());

        collapse_statements.append(collapsed_loop);

        _transformation = Nodecl::Context::make(
                Nodecl::List::make(
                    Nodecl::CompoundStatement::make(
                        collapse_statements,
                        /* finally */ Nodecl::NodeclBase::null())),
                collapse_scope.get_decl_context());
    }

}}
