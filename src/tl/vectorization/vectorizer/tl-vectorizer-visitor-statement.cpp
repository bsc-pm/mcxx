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

#include "tl-vectorizer-visitor-statement.hpp"

#include "tl-vectorization-analysis-interface.hpp"
#include "tl-vectorizer-loop-info.hpp"
#include "tl-vectorizer-visitor-expression.hpp"
#include "tl-vectorizer.hpp"
#include "tl-vectorization-utils.hpp"
#include "tl-nodecl-utils.hpp"
#include "cxx-cexpr.h"


namespace TL
{
namespace Vectorization
{
    VectorizerVisitorStatement::VectorizerVisitorStatement(
            VectorizerEnvironment& environment)
        : _environment(environment)
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
        Nodecl::LoopControl main_loop_control = n.get_loop_header().
            as<Nodecl::LoopControl>();

        VectorizerVisitorExpression visitor_expression(
                _environment);

        // PROCESING LOOP CONTROL
        bool jump_stmts_inside_loop =
                Nodecl::Utils::nodecl_contains_nodecl_of_kind
                <Nodecl::BreakStatement>(n) || 
                Nodecl::Utils::nodecl_contains_nodecl_of_kind
                <Nodecl::ContinueStatement>(n) || 
                Nodecl::Utils::nodecl_contains_nodecl_of_kind
                <Nodecl::ReturnStatement>(n);

        bool init_next_need_vectorization =
            !loop_info.ivs_values_are_uniform_in_simd_scope();
        bool condition_needs_vectorization =
            jump_stmts_inside_loop || 
            !loop_info.condition_is_uniform_in_simd_scope();


        // Init

        if (init_next_need_vectorization)
        {
            VECTORIZATION_DEBUG()
            {
                fprintf(stderr, "VECTORIZER: Init and Next need vectorization\n");
            }
        }

        if (condition_needs_vectorization)
        {
            VECTORIZATION_DEBUG()
            {
                fprintf(stderr, "VECTORIZER: Condition '%s' needs vectorization\n",
                        main_loop_control.get_cond().prettyprint().c_str());
            }
        }

        Nodecl::NodeclBase mask_condition_symbol = 
            Utils::get_new_mask_symbol(_environment._analysis_simd_scope, 
                    _environment._vec_factor,
                    true /*ref_type*/);

        if (init_next_need_vectorization || condition_needs_vectorization)
        {
            Nodecl::ForStatement epilog = 
                Vectorizer::_vectorizer_analysis->
                deep_copy(n, n).as<Nodecl::ForStatement>();

            //bool only_epilog = !Utils::is_all_one_mask(
            //        _environment._mask_list.back());

            n.append_sibling(epilog);

            Nodecl::LoopControl epilog_loop_control =
                epilog.get_loop_header().as<Nodecl::LoopControl>();

            // If Init or Step depends on SIMD IV both need to be vectorized
//            if (init_next_need_vectorization)
            {
                VECTORIZATION_DEBUG()
                {
                    fprintf(stderr, "VECTORIZER: Vectorizing init\n");
                }

                visitor_expression.walk(main_loop_control.get_init());

            }

            // Condition
            VECTORIZATION_DEBUG()
            {
                fprintf(stderr, "VECTORIZER: Vectorizing loop condition\n");
            }

            //
            // EPILOG condition: while((mask != 0) do
            //
            Nodecl::NodeclBase epilog_loop_postcondition =
                Vectorizer::_vectorizer_analysis->
                deep_copy(epilog_loop_control.get_cond(),
                        epilog_loop_control.get_cond());

            Nodecl::NodeclBase epilog_condition =
                epilog_loop_control.get_cond();


            Nodecl::Different epilog_new_condition =
                Nodecl::Different::make(
                        mask_condition_symbol.shallow_copy(),
                        const_value_to_nodecl(const_value_get_zero(4, 1)),
                        Type::get_bool_type());

            epilog_condition.replace(epilog_new_condition);

            //
            // Main loop condition: while((mask == 0xFFFF) != 0) do
            //
            Nodecl::NodeclBase main_loop_precondition = 
                Vectorizer::_vectorizer_analysis->
                deep_copy(main_loop_control.get_cond(),
                        main_loop_control.get_cond());
            Nodecl::NodeclBase main_loop_postcondition =
                Vectorizer::_vectorizer_analysis->
                deep_copy(main_loop_control.get_cond(),
                        main_loop_control.get_cond());

            // Vectorize loop precondition
            visitor_expression.walk(main_loop_precondition);

            // Loop precondition statement
            Nodecl::ExpressionStatement main_loop_precond_stmt =
                Nodecl::ExpressionStatement::make(
                        Nodecl::VectorMaskAssignment::make(
                            mask_condition_symbol.shallow_copy(),
                            main_loop_precondition,
                            mask_condition_symbol.get_type()));

            // New loop condition expression
            Nodecl::Equal new_main_loop_condition =
                Nodecl::Equal::make(
                        mask_condition_symbol.shallow_copy(),
                        const_value_to_nodecl(const_value_get_unsigned_int(0xFFFF)),
                        Type::get_bool_type());

            main_loop_control.get_cond().replace(new_main_loop_condition);

            // Insert precondition
            n.prepend_sibling(Nodecl::ExpressionStatement::make(
                        main_loop_control.get_init().as<Nodecl::List>().
                        front().shallow_copy()));
            main_loop_control.set_init(Nodecl::NodeclBase::null());
            CXX_LANGUAGE()
            {
                n.prepend_sibling(
                        Nodecl::CxxDef::make(
                            Nodecl::NodeclBase::null(),
                            mask_condition_symbol.get_symbol(),
                            mask_condition_symbol.get_locus()));
            }
            n.prepend_sibling(main_loop_precond_stmt);
           
            _environment._analysis_scopes.pop_back();

            VECTORIZATION_DEBUG()
            {
                fprintf(stderr, "VECTORIZER: -- Loop body vectorization --\n");
            }

            //
            // EPILOG BODY
            //
            _environment._analysis_scopes.push_back(epilog);
            // Add loop condition as mask for epilog vectorization
            _environment._mask_list.push_back(mask_condition_symbol);

            walk(epilog.get_statement());

            // Remove Init
            epilog_loop_control.set_init(
                    Nodecl::NodeclBase::null());
             
            // Vectorize loop next
            visitor_expression.walk(epilog_loop_control.get_next());
           
            // Vectorize loop precondition
            visitor_expression.walk(epilog_loop_postcondition);


            // Loop postcondition statement
            Nodecl::VectorMaskAssignment epilog_loop_postcond_assig =
                Nodecl::VectorMaskAssignment::make(
                        mask_condition_symbol.shallow_copy(),
                        epilog_loop_postcondition,
                        mask_condition_symbol.get_type());

            Nodecl::NodeclBase epilog_old_next_copy =
                epilog_loop_control.get_next().shallow_copy();
            
            epilog_loop_control.set_next(
                    Nodecl::Comma::make(
                        epilog_old_next_copy,
                        epilog_loop_postcond_assig,
                        epilog_loop_postcond_assig.get_type()));

            // If jump statements, a extra mask will have been added
            if (jump_stmts_inside_loop)
            {
                _environment._mask_list.pop_back();
            }
 
            _environment._analysis_scopes.pop_back();
            _environment._mask_list.pop_back();


            //
            // MAIN LOOP BODY
            //
            _environment._analysis_scopes.push_back(n);
            walk(n.get_statement());

            // Vectorize loop next
            visitor_expression.walk(main_loop_control.get_next());

            // Vectorize loop precondition
            visitor_expression.walk(main_loop_postcondition);

            // Loop postcondition statement
            Nodecl::VectorMaskAssignment main_loop_postcond_assig =
                Nodecl::VectorMaskAssignment::make(
                        mask_condition_symbol.shallow_copy(),
                        main_loop_postcondition,
                        mask_condition_symbol.get_type());

            Nodecl::NodeclBase main_old_next_copy =
                main_loop_control.get_next().shallow_copy();
            
            main_loop_control.set_next(
                    Nodecl::Comma::make(
                        main_old_next_copy,
                        main_loop_postcond_assig,
                        main_loop_postcond_assig.get_type()));

            _environment._analysis_scopes.pop_back();

            // If jump statements, a extra mask will have been added
            if (jump_stmts_inside_loop)
            {
                _environment._mask_list.pop_back();
            }

            // We remove main & epilogue structure
            // It's going slower and generating only one
            // loop with initial mask seems not to slow down
            // the execution because masks instructions are
            // paired with vector instructions
            if (true) //(only_epilog)
            {
                Nodecl::Utils::remove_from_enclosing_list(n);
            }
        }
        else
        {
            // LOOP BODY
            VECTORIZATION_DEBUG()
            {
                fprintf(stderr, "VECTORIZER: -- Loop body vectorization --\n");
            }

            walk(n.get_statement());

            _environment._analysis_scopes.pop_back();
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
        if(!(condition.is_constant() ||
                Vectorizer::_vectorizer_analysis->is_uniform(
                    _environment._analysis_simd_scope, condition, condition)))
        {
            Utils::MaskCheckCostEstimation mask_check_cost_visitor;

            Nodecl::List list;
            bool has_else = !n.get_else().is_null();
            unsigned int prev_mask_cost = _environment._mask_check_bb_cost.back();

            VectorizerVisitorExpression visitor_expression(_environment);
            visitor_expression.walk(condition);

            Nodecl::NodeclBase prev_mask =
                _environment._mask_list.back();

            // *******
            // IF Mask
            // *******

            // Condition mask
            Nodecl::NodeclBase mask_condition_symbol = 
                Utils::get_new_mask_symbol(_environment._analysis_simd_scope,
                        _environment._vec_factor,
                        true /*ref_type*/);

            Nodecl::ExpressionStatement mask_condition_exp =
                Nodecl::ExpressionStatement::make(
                        Nodecl::VectorMaskAssignment::make(mask_condition_symbol.shallow_copy(),
                            condition.shallow_copy(),
                            mask_condition_symbol.get_type(),
                            n.get_locus()));

            CXX_LANGUAGE()
            {
                list.append(
                        Nodecl::CxxDef::make(
                            Nodecl::NodeclBase::null(),
                            mask_condition_symbol.get_symbol(),
                            mask_condition_symbol.get_locus()));
            }
            list.append(mask_condition_exp);

            // If mask symbol
            Nodecl::NodeclBase if_mask_symbol = 
                Utils::get_new_mask_symbol(_environment._analysis_simd_scope, 
                        _environment._vec_factor,
                        true /*ref_type*/);

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
                        prev_mask.get_type().no_ref(),
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
            CXX_LANGUAGE()
            {
                list.append(
                        Nodecl::CxxDef::make(
                            Nodecl::NodeclBase::null(),
                            if_mask_symbol.get_symbol(),
                            if_mask_symbol.get_locus()));
            }
            list.append(if_mask_exp);

            // ***********
            // "Else" Mask: It will always exists! With or without real 'else statement'
            // ***********
            // New symbol mask
            Nodecl::NodeclBase else_mask_symbol = 
                Utils::get_new_mask_symbol(_environment._analysis_simd_scope,
                        _environment._vec_factor,
                        true);

            // Mask value
            Nodecl::NodeclBase else_mask_value;
            if (Utils::is_all_one_mask(prev_mask)) // mask = !if_cond
            {
                else_mask_value = Nodecl::VectorMaskNot::make(
                        if_mask_symbol.shallow_copy(),
                        if_mask_symbol.get_type().no_ref(),
                        n.get_locus());
            }
            else // mask = prev_mask & !if_cond
            {
                else_mask_value = Nodecl::VectorMaskAnd2Not::make(
                        prev_mask.shallow_copy(),
                        mask_condition_symbol.shallow_copy(),
                        prev_mask.get_type().no_ref(),
                        n.get_locus());
            }

            // Expression that sets the mask
            Nodecl::ExpressionStatement else_mask_exp =
                Nodecl::ExpressionStatement::make(
                        Nodecl::VectorMaskAssignment::make(else_mask_symbol.shallow_copy(),
                            else_mask_value.shallow_copy(),
                            else_mask_value.get_type(),
                            n.get_locus()));

            // Add masks to the source code
            CXX_LANGUAGE()
            {
                list.append(
                        Nodecl::CxxDef::make(
                            Nodecl::NodeclBase::null(),
                            else_mask_symbol.get_symbol(),
                            else_mask_symbol.get_locus()));
            }
            list.append(else_mask_exp);

            // ***************
            // VISIT IF'S THEN
            // ***************
            // Before visiting, compute heuristics
            bool jump_stmts_inside_if =
                Nodecl::Utils::nodecl_contains_nodecl_of_kind
                <Nodecl::BreakStatement>(n.get_then()) || 
                Nodecl::Utils::nodecl_contains_nodecl_of_kind
                <Nodecl::ContinueStatement>(n.get_then()) || 
                Nodecl::Utils::nodecl_contains_nodecl_of_kind
                <Nodecl::ReturnStatement>(n.get_then());

            unsigned int mask_check_cost_if =
                mask_check_cost_visitor.get_mask_check_cost(n.get_then(),
                        prev_mask_cost, MASK_CHECK_THRESHOLD);

            _environment._inside_inner_masked_bb.push_back(true);
            _environment._mask_list.push_back(if_mask_symbol);
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
            // Update if_mask after visiting. It could have changed.
            if_mask_symbol = _environment._mask_list.back();

            _environment._mask_list.pop_back();
            _environment._inside_inner_masked_bb.pop_back();

            // ***************
            // VISIT ELSE'S THEN
            // ***************
            bool jump_stmts_inside_else = false;
            if (has_else)
            {
                // Before visiting, compute heuristics
                jump_stmts_inside_else = Nodecl::Utils::nodecl_contains_nodecl_of_kind
                    <Nodecl::BreakStatement>(n.get_else()) || 
                    Nodecl::Utils::nodecl_contains_nodecl_of_kind
                    <Nodecl::ContinueStatement>(n.get_else()) || 
                    Nodecl::Utils::nodecl_contains_nodecl_of_kind
                    <Nodecl::ReturnStatement>(n.get_else());

                unsigned int mask_check_cost_else =
                    mask_check_cost_visitor.get_mask_check_cost(n.get_else(),
                            prev_mask_cost, MASK_CHECK_THRESHOLD);

                _environment._mask_list.push_back(else_mask_symbol);
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
                        Vectorization::Utils::get_if_mask_is_not_zero_nodecl(
                                else_mask_symbol, n.get_else().shallow_copy());

                    list.append(else_check);
                }
                else
                {
                    // Add it to the source code
                    list.append(n.get_else());
                }

                _environment._mask_check_bb_cost.pop_back();
                // Update else_mask after visiting. It could have changed.
                else_mask_symbol = _environment._mask_list.back();
                _environment._mask_list.pop_back();


            }
            // *************
            // BB Exit Masks
            // *************
            if (jump_stmts_inside_if || (has_else && jump_stmts_inside_else))
            {
                // Remove previous mask. It will never be used again.
                //_environment._mask_list.pop_back();

                Nodecl::NodeclBase new_exit_mask = Utils::get_new_mask_symbol(
                    _environment._analysis_simd_scope,
                    _environment._vec_isa_desc.get_mask_max_elements(),
                    /* ref_type */ true);

                Nodecl::ExpressionStatement new_mask_exp = 
                    Nodecl::ExpressionStatement::make(
                            Nodecl::VectorMaskAssignment::make(
                                new_exit_mask.shallow_copy(),
                                Nodecl::VectorMaskOr::make(
                                    if_mask_symbol.shallow_copy(),
                                    else_mask_symbol.shallow_copy(),
                                    if_mask_symbol.get_type().no_ref(),
                                    make_locus("", 0, 0)),
                                if_mask_symbol.get_type(),
                                make_locus("", 0, 0)));

                CXX_LANGUAGE()
                {
                    list.append(
                            Nodecl::CxxDef::make(
                                Nodecl::NodeclBase::null(),
                                new_exit_mask.get_symbol(),
                                new_exit_mask.get_locus()));
                }

                list.append(new_mask_exp);
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
        VectorizerVisitorExpression visitor_expression(_environment);
        visitor_expression.walk(n.get_nest());
    }

    void VectorizerVisitorStatement::visit(const Nodecl::ObjectInit& n)
    {
        TL::Symbol sym = n.get_symbol();

        TL::Type scalar_type = sym.get_type();

        if (scalar_type.is_vector() || scalar_type.is_mask())
        {
            Nodecl::NodeclBase init = sym.get_value();

            // Vectorizing initialization
            if(!init.is_null())
            {
                nodecl_set_parent(init.get_internal_nodecl(), n.get_internal_nodecl());

                VectorizerVisitorExpression visitor_expression(_environment);
                visitor_expression.walk(init);

                nodecl_set_parent(init.get_internal_nodecl(), nodecl_null());
            }
        }
        else
        {
            VECTORIZATION_DEBUG()
            {
                fprintf(stderr,"VECTORIZER: '%s' TL::Symbol is kept scalar\n",
                        sym.make_nodecl().prettyprint().c_str());
            }
        }
    }

    void VectorizerVisitorStatement::visit(const Nodecl::ReturnStatement& n)
    {
        Nodecl::NodeclBase return_value = n.get_value();
        Nodecl::NodeclBase mask = _environment._mask_list.back();

        VectorizerVisitorExpression visitor_expression(_environment);
        visitor_expression.walk(return_value);

        // If I'm inside an inner basic block with mask and mask exists but no special symbol,
        // then add return special symbol
        if (_environment._inside_inner_masked_bb.back() &&
                (_environment._function_return.is_invalid()) &&
                (!mask.is_null()))
        {
            const std::string func_ret_sym_name("__function_return");

            TL::Scope func_scope = Nodecl::Utils::get_enclosing_function(n)
                                       .get_function_code()
                                       .as<Nodecl::FunctionCode>()
                                       .get_statements()
                                       .retrieve_context();

            // Try to reuse the symbol if it already exists in the function
            // NOTE: This should happen when vectorizing simd loops but return
            // statements, which is currently not supported
            TL::Symbol func_ret_symbol = func_scope.get_symbol_from_name(func_ret_sym_name);

            if (func_ret_symbol.is_invalid())
            {
                // New return special symbol
                func_ret_symbol = func_scope.new_symbol(func_ret_sym_name);
                func_ret_symbol.get_internal_symbol()->kind = SK_VARIABLE;
                symbol_entity_specs_set_is_user_declared(
                    func_ret_symbol.get_internal_symbol(), 1);

                TL::Type return_type = return_value.get_type();
                if (return_type.is_any_reference())
                    return_type = return_type.references_to();

                func_ret_symbol.set_type(return_type);
            }
        
            _environment._function_return = func_ret_symbol;
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
                            Nodecl::NodeclBase::null(), // HasBeenDefinedFlag
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
                // Nodecl::ExpressionStatement mask_exp =
                //     Nodecl::ExpressionStatement::make(
                //             Nodecl::VectorMaskAssignment::make(
                //                 mask.shallow_copy(),
                //                 Nodecl::IntegerLiteral::make(TL::Type::get_int_type(),
                //                     const_value_get_zero(2, 0),
                //                     n.get_locus()),
                //                 mask.get_type(),
                //                 make_locus("", 0, 0)));

                Nodecl::ExpressionStatement mask_exp =
                    Nodecl::ExpressionStatement::make(
                            Nodecl::VectorMaskAssignment::make(
                                mask.shallow_copy(),
                                Nodecl::MaskLiteral::make(
                                    mask.get_type().no_ref(),
                                    const_value_get_signed_int(0),
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
        Nodecl::NodeclBase mask = _environment._mask_list.back();

        if (Vectorizer::_vectorizer_analysis->is_uniform(
                    _environment._analysis_simd_scope, n, n))
        {
            VECTORIZATION_DEBUG()
            {
                fprintf(stderr,"VECTORIZER: break statement is uniform\n");
            }

            if (_environment._inside_inner_masked_bb.back())
            {
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

                n.replace(mask_exp);
            }
        }
        else
        {
            fatal_error("Vectorizer: The code is not vectorizable. Break statement detected.");
        }
    }

    void VectorizerVisitorStatement::visit(const Nodecl::ContinueStatement& n)
    {
        fatal_error("Vectorizer: Target loop contains a 'continue' statement. Unsupported.");
    }

    void VectorizerVisitorStatement::visit(const Nodecl::UnknownPragma& n)
    {
    }

    void VectorizerVisitorStatement::visit(const Nodecl::EmptyStatement& n)
    {
    }

    void VectorizerVisitorStatement::visit(const Nodecl::CxxDecl& n)
    {
    }

    void VectorizerVisitorStatement::visit(const Nodecl::CxxDef& n)
    {
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
