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
#include "tl-vectorizer-utils.hpp"
#include "tl-vectorizer-visitor-function.hpp"
#include "tl-vectorizer-visitor-statement.hpp"
#include "tl-vectorizer-visitor-expression.hpp"
#include "tl-nodecl-utils.hpp"

namespace TL 
{
    namespace Vectorization
    {
        VectorizerVisitorFunction::VectorizerVisitorFunction(VectorizerEnvironment& environment,
                const bool masked_version) :
            _environment(environment), _masked_version(masked_version)
        {
        }

        void VectorizerVisitorFunction::visit(const Nodecl::FunctionCode& function_code)
        {
            // Set up enviroment
            _environment._external_scope =
                function_code.retrieve_context();
            _environment._local_scope_list.push_back(
                    function_code.get_statements().retrieve_context());

            // Push FunctionCode as scope for analysis
            _environment._analysis_simd_scope = function_code;
            _environment._analysis_scopes.push_back(function_code);

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
                TL::Type sym_type = Utils::get_qualified_vector_to((*it_type), _environment._unroll_factor);

                // Set type to parameter TL::Symbol
                (*it_param_sym).set_type(sym_type);

                parameters_vector_type.append(sym_type);
            }

            if(_masked_version)
            {
                TL::Scope scope = vect_func_sym.get_related_scope();

                // Create mask parameter
                TL::Symbol mask_sym = scope.new_symbol("__mask_param");
                mask_sym.get_internal_symbol()->kind = SK_VARIABLE;
                mask_sym.get_internal_symbol()->entity_specs.is_user_declared = 1;
                mask_sym.set_type(TL::Type::get_mask_type(_environment._unroll_factor));
               
                symbol_set_as_parameter_of_function(mask_sym.get_internal_symbol(), 
                        vect_func_sym.get_internal_symbol(),
                        /*nesting*/ 0, parameters.size());

                // Add mask symbol and type to parameters
                parameters.append(mask_sym);
                parameters_vector_type.append(mask_sym.get_type());
                vect_func_sym.set_related_symbols(parameters);

                // Take care of default_argument_info_t*
                //TODO: Move this into a function
                {
                int num_parameters =
                    vect_func_sym.get_internal_symbol()->entity_specs.num_parameters;
                default_argument_info_t** default_argument_info =
                    vect_func_sym.get_internal_symbol()->entity_specs.default_argument_info;

                num_parameters++;
                default_argument_info = (default_argument_info_t**)xrealloc(default_argument_info,
                        num_parameters * sizeof(*default_argument_info));
                default_argument_info[num_parameters-1] = NULL;

                vect_func_sym.get_internal_symbol()->entity_specs.default_argument_info = default_argument_info;
                vect_func_sym.get_internal_symbol()->entity_specs.num_parameters = num_parameters;
                }

                Nodecl::Symbol mask_nodecl_sym = 
                    mask_sym.make_nodecl(true, function_code.get_locus());

                _environment._mask_list.push_back(mask_nodecl_sym);
            }
            else // Add MaskLiteral to mask_list
            {
                Nodecl::MaskLiteral all_one_mask =
                    Nodecl::MaskLiteral::make(
                            TL::Type::get_mask_type(_environment._unroll_factor),
                            const_value_get_minus_one(_environment._unroll_factor, 1),
                            make_locus("", 0, 0));

                _environment._mask_list.push_back(all_one_mask);
            }

            vect_func_sym.set_type(Utils::get_qualified_vector_to(func_type.returns(), 
                        _environment._unroll_factor).get_function_returning(parameters_vector_type));

            // Vectorize function statements
            VectorizerVisitorStatement visitor_stmt(_environment);
            visitor_stmt.walk(function_code.get_statements());

            // Add final return if multi-return function
            if (_environment._function_return.is_valid())
            {
                // Return value
                Nodecl::Symbol return_value= _environment._function_return.make_nodecl(
                        false, function_code.get_locus());

//                VectorizerVisitorExpression visitor_sym(_environment);
//                visitor_sym.walk(return_value);

                // Return value at the end of the Compound Statement
                Nodecl::ReturnStatement return_stmt =
                    Nodecl::ReturnStatement::make(return_value, function_code.get_locus());

                function_code.get_statements().as<Nodecl::Context>().get_in_context().as<Nodecl::List>()
                    .front().as<Nodecl::CompoundStatement>().get_statements()
                    .as<Nodecl::List>().append(return_stmt);
            }
            
            _environment._analysis_scopes.pop_back();
            _environment._mask_list.pop_back();
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
