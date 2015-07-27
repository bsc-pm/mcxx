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




#include "hlt-loop-unroll.hpp"
#include "tl-analysis-interface.hpp"
#include "cxx-cexpr.h"

namespace TL { namespace HLT {

    LoopUnroll::LoopUnroll()
        : Transform(), _loop(), _unrolled(), _epilog(), _unroll_factor(-1), _create_epilog(true)
    {
    }

    LoopUnroll& LoopUnroll::set_loop(Nodecl::NodeclBase loop)
    {
        this->_loop = loop;
        ERROR_CONDITION (!this->_loop.is<Nodecl::ForStatement>(),
                "Only ForStatement can be unrolled. This is a %s", ast_print_node_type(loop.get_kind()));
        Nodecl::NodeclBase loop_control = this->_loop.as<Nodecl::ForStatement>().get_loop_header();
        ERROR_CONDITION(!loop_control.is<Nodecl::LoopControl>()
                && !loop_control.is<Nodecl::RangeLoopControl>(),
                "Only LoopControl or RangeLoopControl can be unrolled", 0);
        TL::ForStatement for_stmt(loop.as<Nodecl::ForStatement>());
        ERROR_CONDITION(!for_stmt.is_omp_valid_loop(), "Loop is too complicated", 0);
        ERROR_CONDITION(!for_stmt.get_step().is_constant(),
                "Loop has a nonconstant step, normalize it first", 0);

        return *this;
    }

    LoopUnroll& LoopUnroll::set_unroll_factor(int n)
    {
        this->_unroll_factor = n;
        ERROR_CONDITION(this->_unroll_factor <= 1, "Invalid unroll factor", 0);

        return *this;
    }

    LoopUnroll& LoopUnroll::set_create_epilog(bool b)
    {
        this->_create_epilog = b;
        return *this;
    }

    namespace {

    struct ReplaceInductionVar : public Nodecl::ExhaustiveVisitor<void>
    {
        TL::Symbol _induction_var;
        Nodecl::NodeclBase _step;
        int _offset;
        ReplaceInductionVar(TL::Symbol induction_var, Nodecl::NodeclBase step, int offset)
            : _induction_var(induction_var), _step(step), _offset(offset)
        {
        }

        void replace_symbol(const Nodecl::NodeclBase& node, TL::Symbol sym)
        {
            if (sym == _induction_var)
            {
                Nodecl::NodeclBase offset_value;
                // Note, we know that the step is constant
                offset_value = const_value_to_nodecl(
                        const_value_mul(
                            const_value_get_signed_int(_offset),
                            _step.get_constant()));

                Nodecl::NodeclBase new_value = Nodecl::Add::make(
                        offset_value,
                        node.shallow_copy(),
                        _induction_var.get_type().no_ref());

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

        virtual void visit(const Nodecl::ObjectInit& node)
        {
            walk(node.get_symbol().get_value());
        }
    };

    }

    void LoopUnroll::unroll()
    {
        ERROR_CONDITION(this->_loop.is_null(), "No loop set", 0);
        ERROR_CONDITION(this->_unroll_factor < 0, "No factor set", 0);

        Nodecl::ForStatement loop = this->_loop.as<Nodecl::ForStatement>();
        TL::ForStatement for_stmt(loop);
        TL::Symbol induction_var = for_stmt.get_induction_variable();
        Nodecl::NodeclBase orig_loop_step = for_stmt.get_step();

        TL::Scope orig_loop_scope = loop.retrieve_context();
        Nodecl::NodeclBase orig_loop_body = loop.get_statement();

        TL::Scope unrolled_loop_scope;
        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
        {
            // We do this because for C/C++ we will wrap everything into a bigger compound statement
            unrolled_loop_scope = (new_block_context(orig_loop_scope.get_decl_context()));
        }
        else
        {
            unrolled_loop_scope = orig_loop_scope;
        }

        TL::ObjectList<Nodecl::NodeclBase> orig_loop_body_copies(this->_unroll_factor);

        orig_loop_body_copies[0] = Nodecl::Utils::deep_copy(orig_loop_body, unrolled_loop_scope);
        for (int i = 1; i < this->_unroll_factor; i++)
        {
            Nodecl::NodeclBase stmt_copy = Nodecl::Utils::deep_copy(orig_loop_body, unrolled_loop_scope);
            ReplaceInductionVar v(induction_var, orig_loop_step, i);
            v.walk(stmt_copy);

            orig_loop_body_copies[i] = stmt_copy;
        }

        Nodecl::NodeclBase unrolled_loop_body;
        unrolled_loop_body = Nodecl::List::make(orig_loop_body_copies);
        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
        {
            unrolled_loop_body =
                Nodecl::List::make(
                        Nodecl::Context::make(
                            Nodecl::List::make(
                                Nodecl::CompoundStatement::make(unrolled_loop_body, Nodecl::NodeclBase::null())),
                            unrolled_loop_scope));
        }

        Nodecl::NodeclBase unrolled_loop_step;
        unrolled_loop_step = const_value_to_nodecl(
                const_value_mul(
                    orig_loop_step.get_constant(),
                    const_value_cast_as_another(
                        const_value_get_signed_int(this->_unroll_factor),
                        orig_loop_step.get_constant())));

        Nodecl::NodeclBase orig_loop_upper_bound = for_stmt.get_upper_bound();
        Nodecl::NodeclBase unrolled_upper_bound;

        if (const_value_is_positive(orig_loop_step.get_constant()))
        {
            if (orig_loop_upper_bound.is_constant())
            {
                unrolled_upper_bound =
                    const_value_to_nodecl(
                            const_value_sub(
                                orig_loop_upper_bound.get_constant(),
                                const_value_mul(
                                    orig_loop_step.get_constant(),
                                    const_value_get_signed_int(this->_unroll_factor - 1))));
            }
            else
            {
                if (const_value_is_one(orig_loop_step.get_constant()))
                {
                    // Avoid creating a 1*x tree
                    unrolled_upper_bound =
                        Nodecl::Minus::make(
                                orig_loop_upper_bound.shallow_copy(),
                                const_value_to_nodecl(
                                    const_value_get_signed_int(this->_unroll_factor - 1)),
                                orig_loop_upper_bound.get_type().no_ref());
                }
                else
                {
                    unrolled_upper_bound =
                        Nodecl::Minus::make(
                                orig_loop_upper_bound.shallow_copy(),
                                Nodecl::Mul::make(
                                    orig_loop_step.shallow_copy(),
                                    const_value_to_nodecl(
                                        const_value_get_signed_int(this->_unroll_factor - 1)),
                                    orig_loop_upper_bound.get_type().no_ref()),
                                orig_loop_upper_bound.get_type().no_ref());
                }
            }
        }
        else if (const_value_is_negative(orig_loop_step.get_constant()))
        {
            if (orig_loop_upper_bound.is_constant())
            {
                unrolled_upper_bound =
                    const_value_to_nodecl(
                            const_value_sub(
                                orig_loop_upper_bound.get_constant(),
                                const_value_mul(
                                    orig_loop_step.get_constant(),
                                    const_value_get_signed_int(this->_unroll_factor - 1))));
            }
            else
            {
                unrolled_upper_bound =
                    Nodecl::Minus::make(
                            orig_loop_upper_bound.shallow_copy(),
                            Nodecl::Mul::make(
                                orig_loop_step.shallow_copy(),
                                const_value_to_nodecl(
                                    const_value_get_signed_int(this->_unroll_factor - 1)),
                                orig_loop_upper_bound.get_type().no_ref()),
                            orig_loop_upper_bound.get_type().no_ref());
            }
        }
        else
        {
            internal_error("Code unreachable", 0);
        }

        // i = lower
        Nodecl::NodeclBase init = 
            Nodecl::Assignment::make(
                    induction_var.make_nodecl(/* ref */ true),
                    for_stmt.get_lower_bound(),
                    induction_var.get_type().no_ref().get_lvalue_reference_to());

        Nodecl::NodeclBase cond;
        if (const_value_is_positive(orig_loop_step.get_constant()))
        {
            // i <= new_upper
            cond = Nodecl::LowerOrEqualThan::make(
                    induction_var.make_nodecl(/* set_ref_type */ true),
                    unrolled_upper_bound,
                    ::get_bool_type());
        }
        else // can't be zero
        {
            // i >= new_upper
            cond = Nodecl::GreaterOrEqualThan::make(
                    induction_var.make_nodecl(/* set_ref_type */ true),
                    unrolled_upper_bound,
                    ::get_bool_type());
        }

        // i = i + step
        Nodecl::NodeclBase next = 
            Nodecl::Assignment::make(
                    induction_var.make_nodecl(/* set_ref_type */ true),
                    Nodecl::Add::make(
                        induction_var.make_nodecl(/* set_ref_type */ true),
                        unrolled_loop_step,
                        induction_var.get_type().no_ref()),
                    induction_var.get_type().no_ref().get_lvalue_reference_to());

        // for (i = 0; i <= upper; i = i + 1)
        _unrolled = Nodecl::ForStatement::make(
                Nodecl::LoopControl::make(
                    Nodecl::List::make(init),
                    cond,
                    next),
                unrolled_loop_body,
                /* loop-name */ Nodecl::NodeclBase::null());

        if (_create_epilog)
        {
            Nodecl::NodeclBase epilog_loop_control = loop.get_loop_header();
            epilog_loop_control = epilog_loop_control.shallow_copy();
            if (epilog_loop_control.is<Nodecl::LoopControl>())
            {
                epilog_loop_control.as<Nodecl::LoopControl>().set_init(Nodecl::NodeclBase::null());
            }
            else if (epilog_loop_control.is<Nodecl::RangeLoopControl>())
            {
                epilog_loop_control.as<Nodecl::RangeLoopControl>().set_lower(
                        induction_var.make_nodecl());
            }
            else
            {
                internal_error("Code unreachable", 0);
            }

            TL::Scope epilog_loop_scope;
            if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
            {
                epilog_loop_scope = (new_block_context(orig_loop_scope.get_decl_context()));
            }
            else
            {
                epilog_loop_scope = orig_loop_scope;
            }

            Nodecl::NodeclBase epilog_loop_body = Nodecl::Utils::deep_copy(orig_loop_body, epilog_loop_scope);
            if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
            {
                epilog_loop_body = Nodecl::List::make(
                        Nodecl::Context::make(
                            Nodecl::List::make(
                                Nodecl::CompoundStatement::make(epilog_loop_body, Nodecl::NodeclBase::null())),
                            epilog_loop_scope));
            }

            _epilog = Nodecl::ForStatement::make(
                    epilog_loop_control,
                    epilog_loop_body,
                    /* loop-name */ Nodecl::NodeclBase::null());
        }

        TL::ObjectList<Nodecl::NodeclBase> transformation_stmts;

        if (for_stmt.induction_variable_in_separate_scope())
        {
            induction_var.set_value(Nodecl::NodeclBase::null());
            if (IS_CXX_LANGUAGE)
            {
                Nodecl::NodeclBase induction_var_def =
                    Nodecl::CxxDef::make(
                            /* context-of-decl */ Nodecl::NodeclBase::null(),
                            induction_var);

                transformation_stmts.append(induction_var_def);
            }
        }

        transformation_stmts.append(_unrolled);
        if (_create_epilog)
        {
            transformation_stmts.append(_epilog);
        }

        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
        {
            _transformation =
                Nodecl::List::make(
                        Nodecl::Context::make(
                            Nodecl::List::make(
                                Nodecl::CompoundStatement::make(
                                    Nodecl::List::make(transformation_stmts),
                                    /* destructors */ Nodecl::NodeclBase::null())),
                            orig_loop_scope)
                        );
        }
        else
        {
            _transformation = Nodecl::List::make(transformation_stmts);
        }
    }
} }

