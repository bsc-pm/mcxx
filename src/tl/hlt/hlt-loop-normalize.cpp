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


#include "hlt-loop-normalize.hpp"
#include "tl-nodecl-utils.hpp"
#include "cxx-cexpr.h"

namespace TL { namespace HLT {

    LoopNormalize::LoopNormalize() { }

    LoopNormalize& LoopNormalize::set_loop(Nodecl::NodeclBase loop)
    {
        this->_loop = loop;
        ERROR_CONDITION (!this->_loop.is<Nodecl::ForStatement>(),
                "Only ForStatement can be unrolled. This is a %s",
                ast_print_node_type(loop.get_kind()));
        Nodecl::NodeclBase loop_control = this->_loop.as<Nodecl::ForStatement>().get_loop_header();
        ERROR_CONDITION(!loop_control.is<Nodecl::LoopControl>()
                && !loop_control.is<Nodecl::RangeLoopControl>(),
                "Only LoopControl or RangeLoopControl can be unrolled", 0);
        TL::ForStatement for_stmt(loop.as<Nodecl::ForStatement>());
        ERROR_CONDITION(!for_stmt.is_omp_valid_loop(), "Loop is too complicated", 0);

        return *this;
    }

    namespace {

    struct ReplaceInductionVar : public Nodecl::ExhaustiveVisitor<void>
    {
        TL::Symbol _induction_var;
        Nodecl::NodeclBase _lower, _step;
        ReplaceInductionVar(TL::Symbol induction_var,
                Nodecl::NodeclBase lower,
                Nodecl::NodeclBase step)
            : _induction_var(induction_var),
            _lower(lower),
            _step(step)
        {
            if (_lower.is_constant())
                _lower = const_value_to_nodecl(_lower.get_constant());
            if (_step.is_constant())
                _step = const_value_to_nodecl(_step.get_constant());
        }

        void replace_symbol(const Nodecl::NodeclBase& node, TL::Symbol sym)
        {
            if (sym == _induction_var)
            {
                // I -> S * I1 + L

                Nodecl::NodeclBase new_value;
                if (!_step.is_constant()
                        || !const_value_is_one(_step.get_constant()))
                {
                    new_value = Nodecl::Mul::make(
                            _step.shallow_copy(),
                            node.shallow_copy(),
                            node.get_type().no_ref());
                }
                else
                {
                    new_value = node.shallow_copy();
                }

                if (!_lower.is_constant()
                        || !const_value_is_zero(_lower.get_constant()))
                {
                    new_value = Nodecl::Add::make(
                            new_value,
                            _lower.shallow_copy(),
                            new_value.get_type().no_ref());
                }

                node.replace(new_value);
            }
        }

        virtual void visit(const Nodecl::Symbol& node)
        {
            replace_symbol(node, node.get_symbol());
        }

        virtual void visit(const Nodecl::Conversion& node)
        {
            Nodecl::NodeclBase nest = node.get_nest();
            if (nest.is<Nodecl::Symbol>()
                    && !node.get_type().is_any_reference()
                    && nest.get_type().is_lvalue_reference())
            {
                replace_symbol(node, nest.get_symbol());
            }
            else
            {
                walk(nest);
            }
        }
    };

    }

    void LoopNormalize::normalize()
    {
        Nodecl::ForStatement loop = this->_loop.as<Nodecl::ForStatement>();
        TL::Scope orig_loop_scope = loop.retrieve_context();

        TL::ForStatement for_stmt(loop);

        TL::Symbol induction_var = for_stmt.get_induction_variable();
        Nodecl::NodeclBase orig_loop_lower = for_stmt.get_lower_bound();
        Nodecl::NodeclBase orig_loop_upper = for_stmt.get_upper_bound();
        Nodecl::NodeclBase orig_loop_step = for_stmt.get_step();

        if ( orig_loop_lower.is_constant()
                && const_value_is_zero(orig_loop_lower.get_constant())
                && orig_loop_step.is_constant()
                && const_value_is_one(orig_loop_step.get_constant()))
        {
            // Do nothing
            _transformation = Nodecl::List::make(
                    this->_loop.shallow_copy()
                    );
            return;
        }


        //    DO I = L, U, S
        //      A(I)
        // becomes
        //    DO I1 = 0, (U-L)/S, 1
        //      A(S * I1 + L)
        //
        Nodecl::NodeclBase new_upper;
        if (orig_loop_upper.is_constant() && orig_loop_lower.is_constant())
        {
            new_upper = const_value_to_nodecl(
                    const_value_sub(
                        orig_loop_upper.get_constant(),
                        orig_loop_lower.get_constant()));

            if (orig_loop_step.is_constant())
            {
                new_upper = const_value_to_nodecl(
                        const_value_div(
                            new_upper.get_constant(),
                            orig_loop_step.get_constant()));
            }
            else
            {
                new_upper = Nodecl::Div::make(
                        new_upper,
                        orig_loop_step.shallow_copy(),
                        new_upper.get_type());
            }
        }
        else
        {
            new_upper = Nodecl::Div::make(
                    Nodecl::Minus::make(
                        orig_loop_upper.shallow_copy(),
                        orig_loop_lower.shallow_copy(),
                        orig_loop_upper.get_type()),
                    orig_loop_step.shallow_copy(),
                    orig_loop_upper.get_type());
        }

        TL::Scope normalized_loop_scope;
        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
        {
            // We do this because for C/C++ we will wrap everything into a bigger compound statement
            normalized_loop_scope = (new_block_context(orig_loop_scope.get_decl_context()));
        }
        else
        {
            normalized_loop_scope = orig_loop_scope;
        }

        Nodecl::NodeclBase orig_loop_body = loop.get_statement();
        Nodecl::NodeclBase normalized_loop_body = Nodecl::Utils::deep_copy(orig_loop_body, normalized_loop_scope);

        ReplaceInductionVar replace_induction_var(induction_var, orig_loop_lower, orig_loop_step);
        replace_induction_var.walk(normalized_loop_body);

        // Nodecl::NodeclBase normalized_for = 
        //     Nodecl::ForStatement::make(
        //             Nodecl::RangeLoopControl::make(
        //                 induction_var.make_nodecl(),
        //                 const_value_to_nodecl(const_value_get_signed_int(0)),
        //                 new_upper,
        //                 const_value_to_nodecl(const_value_get_signed_int(1))),
        //             normalized_loop_body,
        //             /* loop-name */ Nodecl::NodeclBase::null());

        // i = 0
        Nodecl::NodeclBase init = 
            Nodecl::Assignment::make(
                    induction_var.make_nodecl(/* ref */ true),
                    const_value_to_nodecl(const_value_get_signed_int(0)),
                    induction_var.get_type().no_ref().get_lvalue_reference_to());

        // i <= new_upper
        Nodecl::NodeclBase cond =
            Nodecl::LowerOrEqualThan::make(
                    induction_var.make_nodecl(/* set_ref_type */ true),
                    new_upper,
                    ::get_bool_type());

        // i = i + 1
        Nodecl::NodeclBase next = 
            Nodecl::Assignment::make(
                    induction_var.make_nodecl(/* set_ref_type */ true),
                    Nodecl::Add::make(
                        induction_var.make_nodecl(/* set_ref_type */ true),
                        const_value_to_nodecl(const_value_get_signed_int(1)),
                        induction_var.get_type().no_ref()),
                    induction_var.get_type().no_ref().get_lvalue_reference_to());

        // for (i = 0; i <= upper; i = i + 1)
        Nodecl::NodeclBase normalized_for = 
            Nodecl::ForStatement::make(
                    Nodecl::LoopControl::make(
                        Nodecl::List::make(init),
                        cond,
                        next),
                    normalized_loop_body,
                    /* loop-name */ Nodecl::NodeclBase::null());

        TL::ObjectList<Nodecl::NodeclBase> result_stmts;
        result_stmts.append(normalized_for);

        if (!for_stmt.induction_variable_in_separate_scope())
        {
            Nodecl::NodeclBase ind_var_ref =
                Nodecl::Conversion::make(
                    induction_var.make_nodecl(/* ref_type */ true),
                    induction_var.get_type().no_ref());
            replace_induction_var.walk(ind_var_ref);

            Nodecl::NodeclBase new_assig_stmt =
                Nodecl::ExpressionStatement::make(
                        Nodecl::Assignment::make(
                            induction_var.make_nodecl(/* ref_type */ true),
                            ind_var_ref,
                            induction_var.get_type().no_ref().get_lvalue_reference_to())
                        );

            result_stmts.append(new_assig_stmt);
        }

        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
        {
            _transformation = 
                Nodecl::List::make(
                        Nodecl::Context::make(
                            Nodecl::List::make(
                                Nodecl::CompoundStatement::make(
                                    Nodecl::List::make(result_stmts),
                                    /* destructors */ Nodecl::NodeclBase::null())),
                            orig_loop_scope)
                        );
        }
        else
        {
            _transformation = Nodecl::List::make(result_stmts);
        }
    }

} }
