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

#include "tl-vectorizer.hpp"
#include "tl-vectorizer-visitor-function.hpp"
#include "tl-vectorizer-visitor-statement.hpp"
#include "tl-nodecl-utils.hpp"

namespace TL 
{
    namespace Vectorization
    {
        VectorizerVisitorFunction::VectorizerVisitorFunction(VectorizerEnvironment& environment) :
            _environment(environment)
        {
        }

        void VectorizerVisitorFunction::visit(const Nodecl::FunctionCode& function_code)
        {
            // Get analysis info
            if ((Vectorizer::_analysis_info == 0) || 
                    (Vectorizer::_analysis_info->get_nodecl_origin() != function_code))
            {
                if(Vectorizer::_analysis_info != 0)
                    delete Vectorizer::_analysis_info;

                Vectorizer::_analysis_info = new Analysis::AnalysisStaticInfo(function_code,
                        Analysis::WhichAnalysis::CONSTANTS_ANALYSIS,
                        Analysis::WhereAnalysis::NESTED_ALL_STATIC_INFO, /* nesting level */ 100);
            }

            // Push FunctionCode as scope for analysis
            Vectorizer::_analysis_scopes = new std::list<Nodecl::NodeclBase>();
            Vectorizer::_analysis_scopes->push_back(function_code);

            //Vectorize function type and parameters
            TL::Symbol vect_func_sym = function_code.get_symbol();
            TL::Type func_type = vect_func_sym.get_type();
            TL::ObjectList<TL::Symbol> parameters = vect_func_sym.get_function_parameters();
            TL::ObjectList<TL::Type> parameters_type = func_type.parameters();

            TL::ObjectList<TL::Type> parameters_vector_type;
            
            TL::ObjectList<TL::Type>::iterator it_type;
            TL::ObjectList<TL::Symbol>::iterator it_param_sym;
            
            for(it_param_sym = parameters.begin(), it_type = parameters_type.begin();
                    it_type != parameters_type.end();
                    it_param_sym++, it_type++)
            {
                TL::Type sym_type = get_qualified_vector_to((*it_type), _environment._vector_length);

                // Set type to parameter TL::Symbol
                (*it_param_sym).set_type(sym_type);

                parameters_vector_type.append(sym_type);
            }

            vect_func_sym.set_type(get_qualified_vector_to(func_type.returns(), _environment._vector_length).
                    get_function_returning(parameters_vector_type));

            // Vectorize function statements
            VectorizerVisitorStatement visitor_stmt(_environment);
            visitor_stmt.walk(function_code.get_statements());

            Vectorizer::_analysis_scopes->pop_back();
            delete Vectorizer::_analysis_scopes;
            Vectorizer::_analysis_scopes = 0;
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
