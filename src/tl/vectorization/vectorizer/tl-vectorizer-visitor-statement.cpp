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

#include "tl-vectorizer-visitor-statement.hpp"

#include "cxx-cexpr.h"
#include "tl-nodecl-utils.hpp"

#include "tl-vectorization-utils.hpp"
#include "tl-vectorizer.hpp"
#include "tl-vectorizer-loop-info.hpp"
#include "tl-vectorizer-visitor-expression.hpp"

namespace TL
{
namespace Vectorization
{
    VectorizerVisitorStatement::VectorizerVisitorStatement(VectorizerEnvironment& environment,
            const bool cache_enabled)
        : _environment(environment), _cache_enabled(cache_enabled)
    {
    }

    void VectorizerVisitorStatement::visit(const Nodecl::Context& n)
    {
        walk(n.get_in_context());
    }

    void VectorizerVisitorStatement::visit(const Nodecl::CompoundStatement& n)
    {
        walk(n.get_statements());
    }

    // Nested ForStatement
    void VectorizerVisitorStatement::visit(const Nodecl::ForStatement& n)
    {
        VECTORIZATION_DEBUG()
        {
            fprintf(stderr, "VECTORIZER: --- Vectorizing nested loop ---\n");
        }

        _environment._analysis_scopes.push_back(n);

        VectorizerLoopInfo loop_info(n, _environment);
        Nodecl::LoopControl loop_control = n.get_loop_header().
            as<Nodecl::LoopControl>();

        VectorizerVisitorExpression visitor_expression(_environment,
                _cache_enabled);

        // PROCESING LOOP CONTROL
        // Init
        bool ivs_lb_depend_on_simd_iv =
            loop_info.ivs_lb_depend_on_simd_iv();

        if (ivs_lb_depend_on_simd_iv)
        {
            VECTORIZATION_DEBUG()
            {
                fprintf(stderr, "VECTORIZER: IVs init depend on SIMD IV\n");
            }
        }
        // Step
        bool ivs_step_depend_on_simd_iv =
            loop_info.ivs_step_depend_on_simd_iv();

        if (ivs_step_depend_on_simd_iv)
        {
            VECTORIZATION_DEBUG()
            {
                fprintf(stderr, "VECTORIZER: IVs step depend on SIMD IV\n");
            }
        }

        // If Init or Step depends on SIMD IV both need to be vectorized
        if (ivs_lb_depend_on_simd_iv || ivs_step_depend_on_simd_iv)
        {
            VECTORIZATION_DEBUG()
            {
                fprintf(stderr, "VECTORIZER: Vectorizing init\n");
            }

            visitor_expression.walk(loop_control.get_init());

            VECTORIZATION_DEBUG()
            {
                fprintf(stderr, "VECTORIZER: Vectorizing next\n");
            }

            visitor_expression.walk(loop_control.get_next());
        }

        // Condition
        bool condition_depends_on_simd_iv =
            loop_info.condition_depends_on_simd_iv();

        if (condition_depends_on_simd_iv)
        {
            VECTORIZATION_DEBUG()
            {
                fprintf(stderr, "VECTORIZER: Condition '%s' depends on SIMD IV\n",
                        loop_control.get_cond().prettyprint().c_str());
            }
        }

        if (condition_depends_on_simd_iv)
        {
            VECTORIZATION_DEBUG()
            {
                fprintf(stderr, "VECTORIZER: Vectorizing loop condition\n");
            }

            // New condition: while((mask = cmp) != 0) do
            Nodecl::NodeclBase mask_condition_symbol = Utils::get_new_mask_symbol(
                    // To be improved
                    n.get_parent().get_parent().get_parent().get_parent().
                    retrieve_context(), _environment._unroll_factor, true /*ref_type*/);

            Nodecl::NodeclBase condition = loop_control.get_cond();

            visitor_expression.walk(condition);

            Nodecl::Different new_condition =
                Nodecl::Different::make(
                        Nodecl::Assignment::make(mask_condition_symbol,
                            condition.shallow_copy(),
                            mask_condition_symbol.get_type()),
                        const_value_to_nodecl(const_value_get_zero(4, 1)),
                        Type::get_bool_type());

            condition.replace(new_condition);

            // Add loop condition as mask for vectorization
            _environment._mask_list.push_back(mask_condition_symbol);
        }

        // LOOP BODY
        VECTORIZATION_DEBUG()
        {
            fprintf(stderr, "VECTORIZER: -- Loop body vectorization --\n");
        }


        _environment._local_scope_list.push_back(n.get_statement().
                as<Nodecl::List>().front().retrieve_context());

        walk(n.get_statement());

        _environment._analysis_scopes.pop_back();
        _environment._local_scope_list.pop_back();


        if(condition_depends_on_simd_iv) // Remove mask pushed by loop control
        {
            _environment._mask_list.pop_back();
        }

        VECTORIZATION_DEBUG()
        {
            fprintf(stderr, "VECTORIZER: -------------------------------\n");
        }
    }

#define MASK_CHECK_THRESHOLD 9
    void VectorizerVisitorStatement::visit(const Nodecl::IfElseStatement& n)
    {
        Nodecl::NodeclBase condition = n.get_condition();

        // Non-constant comparison. Vectorize them with masks
        if(!n.is_constant())
        {
            Utils::LookForReturnVisitor look_for_return_visitor;
            Utils::MaskCheckCostEstimation mask_check_cost_visitor;

            Nodecl::List list;
            TL::Scope scope = _environment._local_scope_list.front();
            bool has_else = !n.get_else().is_null();
            unsigned int prev_mask_cost = _environment._mask_check_bb_cost.back();

            VectorizerVisitorExpression visitor_expression(_environment, _cache_enabled);
            visitor_expression.walk(condition);

            Nodecl::NodeclBase prev_mask =
                _environment._mask_list.back();

            // *******
            // IF Mask
            // *******

            // Condition mask
            Nodecl::NodeclBase mask_condition_symbol = Utils::get_new_mask_symbol(
                    scope, _environment._unroll_factor, true /*ref_type*/);

            Nodecl::ExpressionStatement mask_condition_exp =
                Nodecl::ExpressionStatement::make(
                        Nodecl::VectorMaskAssignment::make(mask_condition_symbol.shallow_copy(),
                            condition.shallow_copy(),
                            mask_condition_symbol.get_type(),
                            n.get_locus()));

            list.append(mask_condition_exp);

            // If mask symbol
            Nodecl::NodeclBase if_mask_symbol = Utils::get_new_mask_symbol(
                    scope, _environment._unroll_factor, true /*ref_type*/);

            // Mask value
            Nodecl::NodeclBase if_mask_value;
            if (Utils::is_all_one_mask(prev_mask)) // mask = if_cond
            {
                if_mask_value = mask_condition_symbol.shallow_copy();
            }
            else // mask = prev_mask & if_cond
            {
                if_mask_value = Nodecl::VectorMaskAnd::make(
                        prev_mask.shallow_copy(),
                        mask_condition_symbol.shallow_copy(), //condition.shallow_copy(),
                        prev_mask.get_type(),
                        n.get_locus());
            }

            // Expression that sets the mask
            Nodecl::ExpressionStatement if_mask_exp;

            // Mask types have the same type
            if(if_mask_symbol.get_type().no_ref().is_same_type(if_mask_value.get_type().no_ref()))
            {
                if_mask_exp =
                    Nodecl::ExpressionStatement::make(
                            Nodecl::VectorMaskAssignment::make(if_mask_symbol.shallow_copy(),
                                if_mask_value.shallow_copy(),
                                if_mask_symbol.get_type(),
                                n.get_locus()));
            }
            else
            {
                std::cerr << "warning: Masks have different type. Unsupported." << std::endl;
            }

            // Add masks to the source code
            list.append(if_mask_exp);

            // ***********
            // "Else" Mask: It will always exists! With or without real 'else statement'
            // ***********
            // New symbol mask
            Nodecl::NodeclBase else_mask_nodecl = Utils::get_new_mask_symbol(
                    scope, _environment._unroll_factor, true);

            // Mask value
            Nodecl::NodeclBase else_mask_value;
            if (Utils::is_all_one_mask(prev_mask)) // mask = !if_cond
            {
                else_mask_value = Nodecl::VectorMaskNot::make(
                        if_mask_symbol.shallow_copy(),
                        if_mask_symbol.get_type(),
                        n.get_locus());
            }
            else // mask = prev_mask & !if_cond
            {
                else_mask_value = Nodecl::VectorMaskAnd2Not::make(
                        prev_mask.shallow_copy(),
                        mask_condition_symbol.shallow_copy(),
                        prev_mask.get_type(),
                        n.get_locus());
            }

            // Expression that sets the mask
            Nodecl::ExpressionStatement else_mask_exp =
                Nodecl::ExpressionStatement::make(
                        Nodecl::VectorMaskAssignment::make(else_mask_nodecl.shallow_copy(),
                            else_mask_value.shallow_copy(),
                            else_mask_value.get_type(),
                            n.get_locus()));

            // Add masks to the source code
            list.append(else_mask_exp);

            // ***************
            // VISIT IF'S THEN
            // ***************
            // Before visiting, compute heuristics
            bool return_inside_if =
                look_for_return_visitor.walk(n.get_then());
            unsigned int mask_check_cost_if =
                mask_check_cost_visitor.get_mask_check_cost(n.get_then(),
                        prev_mask_cost, MASK_CHECK_THRESHOLD);

            _environment._inside_inner_masked_bb.push_back(true);
            _environment._mask_list.push_back(if_mask_symbol);
            _environment._local_scope_list.push_back(n.get_then().as<Nodecl::List>().
                    front().retrieve_context());
            _environment._mask_check_bb_cost.push_back(mask_check_cost_if);

            walk(n.get_then());

            VECTORIZATION_DEBUG()
            {
                fprintf(stderr, "Masking 'if' cost heuristic: %d, threshold: %d\n",
                        mask_check_cost_if, MASK_CHECK_THRESHOLD);
            }

            // Add mask check if necessary
            if(mask_check_cost_if > MASK_CHECK_THRESHOLD)
            {
                // Create IF to check if if_mask is all zero
                Nodecl::NodeclBase if_check =
                    Vectorization::Utils::get_if_mask_is_not_zero_nodecl(if_mask_symbol,
                            n.get_then().shallow_copy());

                list.append(if_check);
            }
            else
            {
                // Add THEN to the final code
                list.append(n.get_then());
            }

            _environment._mask_check_bb_cost.pop_back();
            _environment._local_scope_list.pop_back();
            // Update if_mask after visiting. It could have changed.
            if_mask_symbol = _environment._mask_list.back();

            _environment._mask_list.pop_back();
            _environment._inside_inner_masked_bb.pop_back();

            // ***************
            // VISIT ELSE'S THEN
            // ***************
            bool return_inside_else;
            if (has_else)
            {
                // Before visiting, compute heuristics
                return_inside_else = look_for_return_visitor.walk(n.get_else());
                unsigned int mask_check_cost_else =
                    mask_check_cost_visitor.get_mask_check_cost(n.get_else(),
                            prev_mask_cost, MASK_CHECK_THRESHOLD);

                _environment._mask_list.push_back(else_mask_nodecl);
                _environment._local_scope_list.push_back(n.get_else().as<Nodecl::List>().front().retrieve_context());
                _environment._mask_check_bb_cost.push_back(mask_check_cost_else);

                walk(n.get_else());

                VECTORIZATION_DEBUG()
                {
                    fprintf(stderr, "Masking 'else' cost: %d, threshold: %d\n",
                            mask_check_cost_else, MASK_CHECK_THRESHOLD);
                }

                // Add mask check if necessary
                if(mask_check_cost_else > MASK_CHECK_THRESHOLD)
                {
                    // Create IF to check if else_mask is all zero
                    Nodecl::NodeclBase else_check =
                        Vectorization::Utils::get_if_mask_is_not_zero_nodecl(else_mask_nodecl,
                                n.get_else().shallow_copy());

                    list.append(else_check);
                }
                else
                {
                    // Add it to the source code
                    list.append(n.get_else());
                }

                _environment._mask_check_bb_cost.pop_back();
                _environment._local_scope_list.pop_back();
                // Update else_mask after visiting. It could have changed.
                else_mask_nodecl = _environment._mask_list.back();
                _environment._mask_list.pop_back();


            }
            // *************
            // BB Exit Masks
            // *************
            if (return_inside_if || (has_else && return_inside_else))
            {
                // Remove previous mask. It will never be used again.
                _environment._mask_list.pop_back();

                objlist_nodecl_t bb_predecessor_masks;
                bb_predecessor_masks.push_back(if_mask_symbol);
                bb_predecessor_masks.push_back(else_mask_nodecl);

                Nodecl::NodeclBase new_exit_mask = Utils::emit_disjunction_mask(bb_predecessor_masks,
                        list, scope, _environment._unroll_factor);

                _environment._mask_list.push_back(new_exit_mask);
            }

            // Replace if/else with CompoundStatement
            Nodecl::CompoundStatement compound_stmt =
                Nodecl::CompoundStatement::make(list, Nodecl::NodeclBase::null(), n.get_locus());

            n.replace(compound_stmt);
        }
        else // Constant comparison. We do not vectorize them. Only the body
        {
            walk(n.get_then());
            walk(n.get_else());
        }
    }

    void VectorizerVisitorStatement::visit(const Nodecl::ExpressionStatement& n)
    {
        VectorizerVisitorExpression visitor_expression(_environment, _cache_enabled);
        visitor_expression.walk(n.get_nest());
    }

    void VectorizerVisitorStatement::visit(const Nodecl::ObjectInit& n)
    {
        TL::Symbol sym = n.get_symbol();

        TL::Type scalar_type = sym.get_type();
        TL::Type vector_type;

        // Boolean type is treated as mask type
        if (scalar_type.is_bool())
        {
            vector_type = TL::Type::get_mask_type(_environment._unroll_factor);
        }
        else
        {
            vector_type = Utils::get_qualified_vector_to(scalar_type,
                    _environment._unroll_factor);
        }

        // Vectorizing symbol type
        VECTORIZATION_DEBUG()
        {
            fprintf(stderr,"VECTORIZER: '%s' TL::Symbol type vectorization "\
                    "from '%s' to '%s'\n",
                    sym.make_nodecl().prettyprint().c_str(),
                    scalar_type.get_simple_declaration(
                        n.retrieve_context(), "").c_str(),
                    vector_type.get_simple_declaration(
                        n.retrieve_context(), "").c_str());
        }

        sym.set_type(vector_type);

        // Vectorizing initialization
        Nodecl::NodeclBase init = sym.get_value();
        if(!init.is_null())
        {
            VectorizerVisitorExpression visitor_expression(_environment, _cache_enabled);
            visitor_expression.walk(init);
        }
    }

    void VectorizerVisitorStatement::visit(const Nodecl::ReturnStatement& n)
    {
        Nodecl::NodeclBase return_value = n.get_value();
        Nodecl::NodeclBase mask = _environment._mask_list.back();

        VectorizerVisitorExpression visitor_expression(_environment, _cache_enabled);
        visitor_expression.walk(return_value);

        // If I'm inside an inner basic block with mask and mask exists but no special symbol,
        // then add return special symbol
        if (_environment._inside_inner_masked_bb.back() &&
                (_environment._function_return.is_invalid()) &&
                (!mask.is_null()))
        {
            // New return special symbol
            _environment._function_return = _environment._local_scope_list.front().new_symbol("__function_return");
            _environment._function_return.get_internal_symbol()->kind = SK_VARIABLE;
            _environment._function_return.get_internal_symbol()->entity_specs.is_user_declared = 1;

            TL::Type return_type = return_value.get_type();
            if(return_type.is_any_reference())
                return_type = return_type.references_to();

            _environment._function_return.set_type(return_type);
        }

        // Return special symbol if it exists
        if(_environment._function_return.is_valid())
        {
            ERROR_CONDITION(mask.is_null(),
                    "VisitorStatement: Mask list is null at %s", locus_to_str(n.get_locus()));

            Nodecl::ExpressionStatement new_exp_stmt =
                Nodecl::ExpressionStatement::make(
                        Nodecl::VectorAssignment::make(
                            _environment._function_return.make_nodecl(true, n.get_locus()),
                            return_value.shallow_copy(),
                            mask.shallow_copy(),
                            _environment._function_return.get_type(),
                            n.get_locus()),
                        n.get_locus());

            // If return is not in the first SIMD scope, then
            // current mask must be set to all zeros and a global
            // mask check must be added
            if (_environment._inside_inner_masked_bb.back())
            {
                // Global mask check
                /*                    Nodecl::NodeclBase global_mask = *(_environment._mask_list.begin());

                                      Nodecl::NodeclBase updated_global_mask;
                                      if (Utils::is_all_one_mask(global_mask))
                                      {
                                      updated_global_mask =
                                      Nodecl::VectorMaskNot::make(
                                      mask.shallow_copy(),
                                      mask.get_type(),
                                      n.get_locus());
                                      }
                                      else
                                      {
                                      updated_global_mask =
                                      Nodecl::VectorMaskAnd2Not::make(
                                      global_mask.shallow_copy(),
                                      mask.shallow_copy(),
                                      mask.get_type(),
                                      n.get_locus());
                                      }

                                      Nodecl::IfElseStatement if_global_mask_is_zero =
                                      Nodecl::IfElseStatement::make(
                                      Nodecl::Equal::make(
                                      updated_global_mask.shallow_copy(),
                                      Nodecl::IntegerLiteral::make(TL::Type::get_int_type(),
                                      const_value_get_zero(4, 0),
                                      n.get_locus()),
                                      TL::Type::get_bool_type(),
                                      n.get_locus()),
                                      Nodecl::List::make(Nodecl::ReturnStatement::make(
                                      _environment._function_return.make_nodecl(true, n.get_locus()),
                                      n.get_locus())),
                                      Nodecl::NodeclBase::null(),
                                      n.get_locus());
                 */
                // Update current mask
                Nodecl::ExpressionStatement mask_exp =
                    Nodecl::ExpressionStatement::make(
                            Nodecl::VectorMaskAssignment::make(
                                mask.shallow_copy(),
                                Nodecl::IntegerLiteral::make(TL::Type::get_int_type(),
                                    const_value_get_zero(2, 0),
                                    n.get_locus()),
                                mask.get_type(),
                                make_locus("", 0, 0)));

                n.append_sibling(mask_exp);
                //n.append_sibling(if_global_mask_is_zero);
            }
            /*
            // If return is not in the first SIMD scope, then
            // a real return may be necessary if mask is == 0
            // if (else_mask == zero) return

            if (_environment._inside_inner_masked_bb.back())
            {
            ERROR_CONDITION(_environment._mask_list.size() < 3,
            "Return Statement Visitor: Else mask does not exist", 0);

            std::list<Nodecl::NodeclBase>::iterator else_mask_it = _environment._mask_list.end();
            else_mask_it--; // Current mask
            else_mask_it--; // Previous mask

            Nodecl::IfElseStatement if_mask_is_zero =
            Nodecl::IfElseStatement::make(
            Nodecl::Equal::make(
            (*else_mask_it).shallow_copy(), // Else mask
            Nodecl::IntegerLiteral::make(TL::Type::get_int_type(),
            const_value_get_zero(4, 0),
            n.get_locus()),
            TL::Type::get_bool_type(),
            n.get_locus()),
            Nodecl::List::make(Nodecl::ReturnStatement::make(
            _environment._function_return.make_nodecl(true, n.get_locus()),
            n.get_locus())),
            Nodecl::NodeclBase::null(),
            n.get_locus());

            n.append_sibling(if_mask_is_zero);
            }
             */
            // Replace ReturnStatement for ExpressionStatement
            n.replace(new_exp_stmt);
        }
        // Otherwise, simple return. No changes are necessary
    }

    void VectorizerVisitorStatement::visit(const Nodecl::BreakStatement& n)
    {
        running_error("Vectorizer: The code is not vectorizable. Break statement detected.");
    }

    /*
       Nodecl::NodeclBase VectorizerVisitorStatement::process_return_inside(Nodecl::NodeclBase current_mask_nodecl)
       {
       Nodecl::NodeclBase prev_mask = _environment._mask_list.back();
       Nodecl::NodeclBase new_prev_mask_nodecl;

    // Mask value
    Nodecl::NodeclBase new_prev_mask_value;
    if (prev_mask.is_null())
    {
    // No previous mask, then new mask = !current_mask
    new_prev_mask_value =
    Nodecl::VectorMaskNot::make(
    current_mask_nodecl.shallow_copy(),
    current_mask_nodecl.get_type(),
    make_locus("", 0, 0));

    new_prev_mask_nodecl = current_mask_nodecl;
    }
    else
    {
    // Remove previous mask to update it
    _environment._mask_list.pop_back();

    new_prev_mask_value =
    Nodecl::VectorMaskAnd2Not::make(
    prev_mask.shallow_copy(),
    current_mask_nodecl.shallow_copy(),
    current_mask_nodecl.get_type(),
    make_locus("", 0, 0));

    new_prev_mask_nodecl = prev_mask;
    }

    // Expression that sets the mask
    Nodecl::ExpressionStatement new_prev_mask_exp =
    Nodecl::ExpressionStatement::make(
    Nodecl::VectorMaskAssignment::make(new_prev_mask_nodecl.shallow_copy(),
    new_prev_mask_value.shallow_copy(),
    new_prev_mask_nodecl.get_type(),
    make_locus("", 0, 0)));

    // Add mask to the mask list
    _environment._mask_list.push_back(new_prev_mask_nodecl);

    return new_prev_mask_exp;
    }
     */

    Nodecl::NodeclVisitor<void>::Ret VectorizerVisitorStatement::unhandled_node(const Nodecl::NodeclBase& n)
    {
        std::cerr << "VECTORIZER: Unknown 'Statement' node "
            << ast_print_node_type(n.get_kind())
            << " at " << n.get_locus()
            << std::endl;

        return Ret();
    }
}
}
