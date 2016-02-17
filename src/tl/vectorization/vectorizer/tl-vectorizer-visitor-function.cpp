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

#include "tl-vectorizer-visitor-function.hpp"

#include "cxx-cexpr.h"
#include "tl-nodecl-utils.hpp"

#include "tl-vectorization-utils.hpp"
#include "tl-vectorizer.hpp"
#include "tl-vectorizer-visitor-local-symbol.hpp"
#include "tl-vectorizer-visitor-statement.hpp"
#include "tl-vectorizer-visitor-expression.hpp"

namespace TL
{
    namespace Vectorization
    {
        VectorizerVisitorFunctionHeader::VectorizerVisitorFunctionHeader(
                VectorizerEnvironment& environment,
                const TL::ObjectList<TL::Symbol> &uniform_symbols,
                const std::map<TL::Symbol, int> &linear_symbols,
                const bool masked_version) :
            _environment(environment),
            _masked_version(masked_version),
            _uniform_symbols(uniform_symbols),
            _linear_symbols(linear_symbols)
        {
        }

        void VectorizerVisitorFunctionHeader::visit(const Nodecl::FunctionCode& function_code)
        {
            //Vectorize function type and parameters
            TL::Symbol vect_func_sym = function_code.get_symbol();
            TL::Type func_type = vect_func_sym.get_type();
            TL::Type func_ret_type = vect_func_sym.get_type().returns();

            objlist_tlsym_t parameters = vect_func_sym.get_function_parameters();
            TL::ObjectList<TL::Type> parameters_vector_type;

            if (!function_code.is_null())
                Nodecl::Utils::get_all_symbols(function_code.
                        as<Nodecl::FunctionCode>().get_statements());

            for(objlist_tlsym_t::iterator it = parameters.begin();
                    it != parameters.end();
                    it ++)
            {
                // If not uniform or linear, vectorize the parameter
                if (!_uniform_symbols.contains(*it)
                        && (_linear_symbols.find(*it) == _linear_symbols.end()))
                {
                    TL::Type vector_type;
                    TL::Type tl_sym_type = it->get_type().no_ref();

                    if (tl_sym_type.is_bool())
                    {
                        vector_type = TL::Type::get_mask_type(
                                _environment._vec_factor);
                    }
                    else if (tl_sym_type.is_integral_type()
                            || tl_sym_type.is_floating_type())
                    {
                        vector_type = Utils::get_qualified_vector_to(
                            tl_sym_type,
                            _environment._vec_isa_desc.get_vec_factor_for_type(
                                tl_sym_type, _environment._vec_factor));
                    }
                    else if (tl_sym_type.is_class()
                            && Utils::class_type_can_be_vectorized(tl_sym_type))
                    {
                        bool is_new = false;
                        vector_type = Utils::get_class_of_vector_fields_for_isa(
                                tl_sym_type,
                                _environment._vec_factor,
                                is_new,
                                _environment._vec_isa_desc);
                        if (is_new
                                && IS_CXX_LANGUAGE)
                        {
                            VECTORIZATION_DEBUG()
                            {
                                std::cerr << "NEW CLASS -> " << vector_type.get_symbol().get_qualified_name() << std::endl;
                                std::cerr << "(1) ENV = " << &_environment << std::endl;
                            }
                            _environment._vectorized_classes.append(
                                    std::make_pair(tl_sym_type, vector_type)
                                    );
                        }
                        VECTORIZATION_DEBUG()
                        {
                            std::cerr << "CLASS -> " << vector_type.get_symbol().get_qualified_name() << std::endl;
                        }
                    }
                    else
                    {
                        fatal_printf_at(
                                function_code.get_locus(),
                                "cannot vectorize parameter '%s' of type '%s'\n",
                                it->get_name().c_str(),
                                tl_sym_type.get_declaration(it->get_scope(), "").c_str());
                    }

                    if (it->get_type().is_lvalue_reference())
                    {
                        vector_type = vector_type.get_lvalue_reference_to();
                    }

                    it->set_type(vector_type);
                }

                // Adjust the type
                TL::Type param_type = it->get_type();

                if (param_type.is_array())
                {
                    param_type = param_type.array_element().get_pointer_to();
                }
                else if (param_type.is_function())
                {
                    param_type = param_type.get_pointer_to();
                }
                else
                {
                    param_type = param_type.get_unqualified_type();
                }

                parameters_vector_type.append(param_type);
            }

            if(_masked_version)
            {
                TL::Scope scope = vect_func_sym.get_related_scope();

                // Create mask parameter
                TL::Symbol mask_sym = scope.new_symbol("__mask_param");
                mask_sym.get_internal_symbol()->kind = SK_VARIABLE;
                symbol_entity_specs_set_is_user_declared(mask_sym.get_internal_symbol(), 1);
                mask_sym.set_type(TL::Type::get_mask_type(_environment._vec_factor));

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
                    symbol_entity_specs_add_default_argument_info(vect_func_sym.get_internal_symbol(), NULL);
                    // int num_parameters = symbol_entity_specs_get_num_parameters(vect_func_sym.get_internal_symbol());
                    // default_argument_info_t** default_argument_info =
                    //     vect_func_sym.get_internal_symbol()->entity_specs.default_argument_info;

                    // num_parameters++;
                    // default_argument_info = (default_argument_info_t**)xrealloc(default_argument_info,
                    //         num_parameters * sizeof(*default_argument_info));
                    // default_argument_info[num_parameters-1] = NULL;

                    // vect_func_sym.get_internal_symbol()->entity_specs.default_argument_info = default_argument_info;
                    // vect_func_sym.get_internal_symbol()->entity_specs.num_parameters = num_parameters;
                }
            }

            if (func_ret_type.is_void()) // We do not vectorize void types
            {
                vect_func_sym.set_type(func_ret_type.get_function_returning(
                    parameters_vector_type));
            }
            else
            {
                vect_func_sym.set_type(
                    Utils::get_qualified_vector_to(
                        func_ret_type,
                        _environment._vec_isa_desc.get_vec_factor_for_type(
                            func_ret_type, _environment._vec_factor))
                        .get_function_returning(parameters_vector_type));
            }
        }

        Nodecl::NodeclVisitor<void>::Ret VectorizerVisitorFunctionHeader::unhandled_node(const Nodecl::NodeclBase& n)
        {
            std::cerr << "Function Visitor HEADER: Unknown node "
                << ast_print_node_type(n.get_kind())
                << " at " << n.get_locus()
                << std::endl;

            return Ret();
        }



        // Vectorize Local Symbols
        //VectorizerVisitorLocalSymbol visitor_local_symbol(environment);
        //visitor_local_symbol.walk(function_code);
        VectorizerVisitorFunction::VectorizerVisitorFunction(VectorizerEnvironment& environment,
                const bool masked_version) :
            _environment(environment), _masked_version(masked_version)
        {
        }

        void VectorizerVisitorFunction::visit(const Nodecl::FunctionCode& function_code)
        {
            // Vectorize Local Symbols & Parameters
            VectorizerVisitorLocalSymbol visitor_local_symbol(_environment);
            visitor_local_symbol.walk(function_code);

            if(_masked_version)
            {
                TL::Symbol mask_sym = function_code.get_symbol().get_related_symbols().back();

                Nodecl::Symbol mask_nodecl_sym =
                    mask_sym.make_nodecl(true, function_code.get_locus());

                _environment._mask_list.push_back(mask_nodecl_sym);
            }

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

                function_code.get_statements().as<Nodecl::Context>()
                    .get_in_context().as<Nodecl::List>()
                    .front().as<Nodecl::CompoundStatement>().get_statements()
                    .as<Nodecl::List>().append(return_stmt);

                // Uninitialize function_return symbol
                _environment._function_return = TL::Symbol();
            }

            // Remove mask of masked version
            if(_masked_version)
                _environment._mask_list.pop_back();
        }

        Nodecl::NodeclVisitor<void>::Ret VectorizerVisitorFunction::unhandled_node(const Nodecl::NodeclBase& n)
        {
            std::cerr << "Function Visitor CODE: Unknown node "
                << ast_print_node_type(n.get_kind())
                << " at " << n.get_locus()
                << std::endl;

            return Ret();
        }
    }
}
