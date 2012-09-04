/*--------------------------------------------------------------------
  (C) Copyright 2006-2012 Barcelona Supercomputing Center
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

namespace TL 
{
    namespace Vectorization
    {
        VectorizerVisitorStatement::VectorizerVisitorStatement(
                const std::string& device,
                const unsigned int vector_length,
                const TL::Type& target_type,
                const TL::Scope& simd_scope) : 
            _device(device), _vector_length(vector_length), _target_type(target_type),
            _simd_scope(simd_scope)
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
            walk(n.get_statement());
        }

        void VectorizerVisitorStatement::visit(const Nodecl::ExpressionStatement& n)
        {
            VectorizerVisitorExpression visitor_expression(
                    _device, _vector_length, _target_type, _simd_scope);
            visitor_expression.walk(n.get_nest());
        }

        void VectorizerVisitorStatement::visit(const Nodecl::ObjectInit& n)
        {
            if(n.has_symbol())
            {
                TL::Symbol sym = n.get_symbol();

                // Vectorizing symbol type
                sym.set_type(sym.get_type().get_vector_to(_vector_length));

                // Vectorizing initialization
                Nodecl::NodeclBase init = sym.get_value();
                if(!init.is_null())
                {
                    VectorizerVisitorExpression visitor_expression(
                            _device, _vector_length, _target_type, _simd_scope);
                    visitor_expression.walk(init);
                }
            }
        }

        void VectorizerVisitorStatement::visit(const Nodecl::ReturnStatement& n)
        {
            VectorizerVisitorExpression visitor_expression(
                    _device, _vector_length, _target_type, _simd_scope);
            visitor_expression.walk(n.get_value());
        }

        Nodecl::NodeclVisitor<void>::Ret VectorizerVisitorStatement::unhandled_node(const Nodecl::NodeclBase& n) 
        { 
            std::cerr << "Unknown 'Statement' node " 
                << ast_print_node_type(n.get_kind()) 
                << " at " << n.get_locus() 
                << std::endl;

            return Ret(); 
        }
    } 
}
