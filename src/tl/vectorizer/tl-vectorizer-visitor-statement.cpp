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
            Nodecl::List list;
            TL::Scope scope = _environment._local_scope_list.front();
           
            VectorizerVisitorExpression visitor_expression(_environment);
            visitor_expression.walk(n.get_condition()); 

            Nodecl::NodeclBase prev_mask =
                _environment._mask_list.back();

            // IF Mask
            TL::Symbol if_mask_sym = scope.new_symbol("__mask_" + 
                    Vectorizer::_vectorizer->get_var_counter());
            if_mask_sym.get_internal_symbol()->kind = SK_VARIABLE;
            if_mask_sym.get_internal_symbol()->entity_specs.is_user_declared = 1;
            if_mask_sym.set_type(TL::Type::get_mask_type(_environment._mask_size));
            
            Nodecl::Symbol if_mask_nodecl_sym = if_mask_sym.make_nodecl(true, n.get_locus());
            Nodecl::NodeclBase if_mask_value;

            if (prev_mask.is_null()) // mask = if_cond
            {
                if_mask_value = n.get_condition().shallow_copy();
            }
            else // mask = prev_mask & if_cond
            {
                if_mask_value = Nodecl::VectorMaskAnd::make(
                        prev_mask.shallow_copy(),
                        n.get_condition().shallow_copy(),
                        if_mask_sym.get_type(),
                        n.get_locus());
            }

            Nodecl::ExpressionStatement if_mask_exp =
                Nodecl::ExpressionStatement::make(
                        Nodecl::VectorMaskAssignment::make(if_mask_nodecl_sym, 
                            if_mask_value,
                            if_mask_sym.get_type(),
                            n.get_locus()));

            // Visit Then
            _environment._mask_list.push_back(if_mask_nodecl_sym.shallow_copy());
            _environment._local_scope_list.push_back(n.get_then().as<Nodecl::List>().
                    front().retrieve_context());
            walk(n.get_then());
            _environment._local_scope_list.pop_back();
            _environment._mask_list.pop_back();

            
            // ELSE Mask: There ALWAYS will be an ELSE mask, even if else does not exist!
            TL::Symbol else_mask_sym = scope.new_symbol("__mask_" + 
                    Vectorizer::_vectorizer->get_var_counter());
            else_mask_sym.get_internal_symbol()->kind = SK_VARIABLE;
            else_mask_sym.get_internal_symbol()->entity_specs.is_user_declared = 1;
            else_mask_sym.set_type(TL::Type::get_mask_type(_environment._mask_size));

            Nodecl::Symbol else_mask_nodecl_sym = else_mask_sym.make_nodecl(true, n.get_locus());

            Nodecl::NodeclBase else_mask_value;

            if (prev_mask.is_null()) // mask = !if_cond
            {
                else_mask_value = Nodecl::VectorMaskNot::make(
                        if_mask_nodecl_sym.shallow_copy(),
                        else_mask_sym.get_type(),
                        n.get_locus());
            }
            else // mask = prev_mask & !if_cond
            {
                else_mask_value = Nodecl::VectorMaskAnd2Not::make(
                        prev_mask.shallow_copy(),
                        n.get_condition().shallow_copy(),
                        else_mask_sym.get_type(),
                        n.get_locus());
            }

            Nodecl::ExpressionStatement else_mask_exp =
                Nodecl::ExpressionStatement::make(
                        Nodecl::VectorMaskAssignment::make(else_mask_nodecl_sym.shallow_copy(), 
                            else_mask_value,
                            else_mask_sym.get_type(),
                            n.get_locus()));

            // Add masks and if to the final code
            list.append(if_mask_exp);
            list.append(else_mask_exp);

            list.append(n.get_then().shallow_copy());

            // Add 'else' mask to mask list
            _environment._mask_list.push_back(else_mask_nodecl_sym);

            // Else body
            if (!n.get_else().is_null())
            {
                // Visit Else
                _environment._local_scope_list.push_back(n.get_else().as<Nodecl::List>().front().retrieve_context());
                walk(n.get_else());
                _environment._local_scope_list.pop_back();
                _environment._mask_list.pop_back();

                list.append(n.get_else().shallow_copy());
            }

            Nodecl::CompoundStatement compound_stmt =
                Nodecl::CompoundStatement::make(list, Nodecl::NodeclBase::null(), n.get_locus());

            n.replace(compound_stmt);
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
            
            // If mask exists but no special symbol, add return special symbol
            if ((_environment._function_return.is_invalid()) &&
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

            // Return special symbol exists
            if(_environment._function_return.is_valid())
            {
                ERROR_CONDITION(_environment._mask_list.back().is_null(), "Return Statement Visitor: Mask list is null", 0);

                Nodecl::ExpressionStatement new_return_expr =
                    Nodecl::ExpressionStatement::make(
                            Nodecl::VectorAssignmentMask::make(
                                _environment._function_return.make_nodecl(true, n.get_locus()),
                                return_value.shallow_copy(),
                                _environment._mask_list.back().shallow_copy(),
                                _environment._function_return.get_type(),
                                n.get_locus()),
                            n.get_locus());

                n.replace(new_return_expr);
            }
            // Otherwise, simple return. No changes are necessary
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
