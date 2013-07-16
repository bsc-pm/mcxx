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
#include "tl-vectorizer-visitor-expression.hpp"
#include "tl-nodecl-utils.hpp"
#include "tl-vectorizer.hpp"

namespace TL 
{
    namespace Vectorization
    {
        VectorizerVisitorStatement::VectorizerVisitorStatement(VectorizerEnvironment& environment)
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
            _environment._local_scope_list.push_back(n.get_statement().as<Nodecl::List>().front().retrieve_context());
            walk(n.get_statement());
            _environment._local_scope_list.pop_back();
        }

        void VectorizerVisitorStatement::visit(const Nodecl::IfElseStatement& n)
        {
            Nodecl::NodeclBase condition = n.get_condition();

            // Non-constant comparison. Vectorize them with masks
            if(!n.is_constant())
            {
                Nodecl::List list;
                TL::Scope scope = _environment._local_scope_list.front();
                bool has_else = !n.get_else().is_null();

                VectorizerVisitorExpression visitor_expression(_environment);
                visitor_expression.walk(condition); 

                Nodecl::NodeclBase prev_mask =
                    _environment._mask_list.back();
                
                // *******
                // IF Mask
                // *******

                // New symbol mask
                TL::Symbol if_mask_sym = scope.new_symbol("__mask_" + 
                        Vectorizer::_vectorizer->get_var_counter());
                if_mask_sym.get_internal_symbol()->kind = SK_VARIABLE;
                if_mask_sym.get_internal_symbol()->entity_specs.is_user_declared = 1;
                if_mask_sym.set_type(TL::Type::get_mask_type(_environment._mask_size));

                Nodecl::Symbol if_mask_nodecl = if_mask_sym.make_nodecl(true, n.get_locus());
                Nodecl::Symbol else_mask_nodecl; 

                // Mask value
                Nodecl::NodeclBase if_mask_value;
                if (prev_mask.is_null()) // mask = if_cond
                {
                    if_mask_value = condition.shallow_copy();
                }
                else // mask = prev_mask & if_cond
                {
                    if_mask_value = Nodecl::VectorMaskAnd::make(
                            prev_mask.shallow_copy(),
                            condition.shallow_copy(),
                            if_mask_sym.get_type(),
                            n.get_locus());
                }

                // Expression that sets the mask
                Nodecl::ExpressionStatement if_mask_exp =
                    Nodecl::ExpressionStatement::make(
                            Nodecl::VectorMaskAssignment::make(if_mask_nodecl.shallow_copy(), 
                                if_mask_value.shallow_copy(),
                                if_mask_sym.get_type(),
                                n.get_locus()));

                // Add masks to the source code
                list.append(if_mask_exp);
 
                // *********
                // Else Mask
                // *********
                if (has_else)
                {
                    // New symbol mask
                    TL::Symbol else_mask_sym = scope.new_symbol("__mask_" + 
                            Vectorizer::_vectorizer->get_var_counter());
                    else_mask_sym.get_internal_symbol()->kind = SK_VARIABLE;
                    else_mask_sym.get_internal_symbol()->entity_specs.is_user_declared = 1;
                    else_mask_sym.set_type(TL::Type::get_mask_type(_environment._mask_size));

                    else_mask_nodecl = else_mask_sym.make_nodecl(true, n.get_locus());

                    // Mask value
                    Nodecl::NodeclBase else_mask_value;
                    if (prev_mask.is_null()) // mask = !if_cond
                    {
                        else_mask_value = Nodecl::VectorMaskNot::make(
                                if_mask_nodecl.shallow_copy(),
                                else_mask_sym.get_type(),
                                n.get_locus());
                    }
                    else // mask = prev_mask & !if_cond
                    {
                        else_mask_value = Nodecl::VectorMaskAnd2Not::make(
                                prev_mask.shallow_copy(),
                                condition.shallow_copy(),
                                else_mask_sym.get_type(),
                                n.get_locus());
                    }

                    // Expression that sets the mask
                    Nodecl::ExpressionStatement else_mask_exp =
                        Nodecl::ExpressionStatement::make(
                                Nodecl::VectorMaskAssignment::make(else_mask_nodecl.shallow_copy(), 
                                    else_mask_value.shallow_copy(),
                                    else_mask_sym.get_type(),
                                    n.get_locus()));
                    
                    // Add masks to the source code
                    list.append(else_mask_exp);

                    // Add else_mask to mask list:
                    // It should ALWAYS be after if_mask before visiting!
                    _environment._mask_list.push_back(else_mask_nodecl);
                }

                // ***************
                // VISIT IF'S THEN
                // ***************
                // Before visiting, look for returns
                bool return_inside_if;
                LookForReturnVisitor look_for_return_visitor_if(&return_inside_if);
                look_for_return_visitor_if.walk(n.get_then());

                _environment._inside_inner_masked_bb.push_back(true);
                _environment._mask_list.push_back(if_mask_nodecl);
                _environment._local_scope_list.push_back(n.get_then().as<Nodecl::List>().
                        front().retrieve_context());
                walk(n.get_then());
                _environment._local_scope_list.pop_back();
                _environment._mask_list.pop_back();
                _environment._inside_inner_masked_bb.pop_back();

                // TODO: Do you really want to aways create this checks?
                // Create IF to check if if_mask is all zero
                Nodecl::IfElseStatement if_check =
                    Nodecl::IfElseStatement::make(
                            Nodecl::Different::make(
                                if_mask_nodecl.shallow_copy(),
                                Nodecl::IntegerLiteral::make(TL::Type::get_int_type(),
                                    const_value_get_zero(4, 0),
                                    n.get_locus()),
                                TL::Type::get_bool_type(),
                                n.get_locus()),
                            n.get_then().shallow_copy(),
                            Nodecl::NodeclBase::null());

                // Add THEN to the final code
                list.append(if_check);

                // ***************
                // VISIT ELSE'S THEN
                // ***************
                bool return_inside_else;
                if (has_else)
                {
                    // Before visiting, look for returns
                    LookForReturnVisitor look_for_return_visitor_else(&return_inside_else);
                    look_for_return_visitor_else.walk(n.get_else());

                    _environment._local_scope_list.push_back(n.get_else().as<Nodecl::List>().front().retrieve_context());
                    walk(n.get_else());
                    _environment._local_scope_list.pop_back();
                    _environment._mask_list.pop_back();

                    // TODO: Do you really want to aways create this checks?
                    // Create IF to check if else_mask is all zero
                    Nodecl::IfElseStatement else_check =
                        Nodecl::IfElseStatement::make(
                                Nodecl::Different::make(
                                    else_mask_nodecl.shallow_copy(),
                                    Nodecl::IntegerLiteral::make(TL::Type::get_int_type(),
                                        const_value_get_zero(4, 0),
                                        n.get_locus()),
                                    TL::Type::get_bool_type(),
                                    n.get_locus()),
                                n.get_else().shallow_copy(),
                                Nodecl::NodeclBase::null());

                    // Add it to the source code
                    list.append(else_check);
                }

                // ****************
                // Return inside if
                // ****************
                if (return_inside_if)
                {
                    Nodecl::NodeclBase new_prev_mask_exp = 
                        process_return_inside(if_mask_nodecl);

                    // Add it to the source code
                    list.append(new_prev_mask_exp);
               }

                // ******************
                // Return inside else
                // ******************
                if(has_else)
                {
                    // Return inside else
                    if (return_inside_else)
                    {
                        Nodecl::NodeclBase new_prev_mask_exp = 
                            process_return_inside(else_mask_nodecl);

                        // Add it to the source code
                        list.append(new_prev_mask_exp);
                    }
                }
                
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

            // Vectorizing symbol type
            sym.set_type(get_qualified_vector_to(sym.get_type(), _environment._vector_length));

            // Vectorizing initialization
            Nodecl::NodeclBase init = sym.get_value();
            if(!init.is_null())
            {
                VectorizerVisitorExpression visitor_expression(_environment);
                visitor_expression.walk(init);
            }
        }

        void VectorizerVisitorStatement::visit(const Nodecl::ReturnStatement& n)
        {
            Nodecl::NodeclBase return_value = n.get_value();

            VectorizerVisitorExpression visitor_expression(_environment);
            visitor_expression.walk(return_value);
            
            // If I'm inside an inner basic block with mask and mask exists but no special symbol, 
            // then add return special symbol
            if (_environment._inside_inner_masked_bb.back() && 
                    (_environment._function_return.is_invalid()) &&
                    (!_environment._mask_list.back().is_null()))
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
                ERROR_CONDITION(_environment._mask_list.back().is_null(), 
                        "VisitorStatement: Mask list is null at %s", locus_to_str(n.get_locus()));

                Nodecl::ExpressionStatement new_exp_stmt =
                    Nodecl::ExpressionStatement::make(
                            Nodecl::VectorAssignmentMask::make(
                                _environment._function_return.make_nodecl(true, n.get_locus()),
                                return_value.shallow_copy(),
                                _environment._mask_list.back().shallow_copy(),
                                _environment._function_return.get_type(),
                                n.get_locus()),
                            n.get_locus());
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
