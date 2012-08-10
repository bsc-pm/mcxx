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

#include "tl-vectorizer-visitor-function.hpp"
#include "tl-vectorizer-visitor-statement.hpp"
#include "tl-nodecl-utils.hpp"

namespace TL 
{
    namespace Vectorization
    {
        VectorizerVisitorFunction::VectorizerVisitorFunction(
                const std::string& device,
                const unsigned int vector_length,
                const TL::Type& target_type) : 
            _device(device), _vector_length(vector_length), _target_type(target_type)
        {
        }

        void VectorizerVisitorFunction::visit(const Nodecl::FunctionCode& function_code)
        {
            //Vectorize function type and parameters
            TL::Symbol vect_func_sym = function_code.get_symbol();
            TL::Type func_type = vect_func_sym.get_type();
            TL::ObjectList<TL::Type> parameters_type = func_type.parameters();

            TL::ObjectList<TL::Type> parameters_vector_type;
            for(TL::ObjectList<TL::Type>::iterator it = parameters_type.begin();
                    it != parameters_type.end();
                    it++)
            {
                parameters_vector_type.append((*it).get_vector_to(_vector_length));
            }

            vect_func_sym.set_type(func_type.returns().get_vector_to(_vector_length).
                    get_function_returning(parameters_vector_type));

            // Vectorize function statements
            VectorizerVisitorStatement visitor_stmt(_device, _vector_length, _target_type);  
            visitor_stmt.walk(function_code.get_statements());
        }

        Nodecl::NodeclVisitor<void>::Ret VectorizerVisitorFunction::unhandled_node(const Nodecl::NodeclBase& n) 
        { 
            std::cerr << "Function Visitor: Unknown node " 
                << ast_print_node_type(n.get_kind()) 
                << " at " << n.get_locus() 
                << std::endl;

            return Ret(); 
        }
    } 
}
