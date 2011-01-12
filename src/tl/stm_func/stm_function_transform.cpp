/*--------------------------------------------------------------------
  (C) Copyright 2006-2011 Barcelona Supercomputing Center 
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



#include "stm_function_transform.hpp"
#include "tl-compilerphase.hpp"
#include "tl-langconstruct.hpp"
#include "tl-traverse.hpp"
#include "tl-functionfilter.hpp"

namespace TL
{


    class STMFunctionDefFunctor : public TraverseFunctor
    {
        private:
            FunctionFilterFile function_filter;
        public:
            virtual void postorder(Context ctx, AST_t node) 
            {
                FunctionDefinition function_def(node, ctx.scope_link);
                IdExpression function_name = function_def.get_function_name();
                Symbol function_symbol = node.get_attribute(LANG_FUNCTION_SYMBOL);
                Type function_type = function_symbol.get_type();

                Type return_type = function_type.returns();

                Source stm_function_source;
                Source stm_parameters;
                Source stm_function_body;
                Statement function_tree = function_def.get_function_body();

                bool tm_callable = function_symbol.has_gcc_attribute("tm_callable");

                if (tm_callable
                        || (function_filter.match(function_name.prettyprint())
                            // We do not wrap member functions! (yet :)
                            && !function_symbol.is_member()))
                {
                    std::cerr << "Wrapping function '" 
                        << function_name.prettyprint() 
                        << " in " 
                        << function_name.get_ast().get_locus() << std::endl;
                }
                else
                {
                    // Do not do anything else
                    return;
                }

                stm_function_source
                    << return_type.get_declaration(function_def.get_scope(), "") 
                    << " "
                    << "__stm_" << function_name.prettyprint() << "_ (" << stm_parameters << ")"
                    << "{\n"
                    << "#pragma omp transaction converted_function(1)\n"
                    <<    stm_function_body
                    << "}"
                    ;

                stm_function_body 
                    << function_tree.prettyprint()
                    ;

                stm_parameters.append_with_separator("Transaction *__t", ",");

                bool has_ellipsis;
                ObjectList<Type> parameter_types = function_type.parameters(has_ellipsis);

                DeclaredEntity declared_entity = function_def.get_declared_entity();
                ObjectList<ParameterDeclaration> parameters = declared_entity.get_parameter_declarations();

                for (ObjectList<ParameterDeclaration>::iterator it = parameters.begin();
                        it != parameters.end();
                        it++)
                {
                    ParameterDeclaration &param(*it);
                    stm_parameters.append_with_separator(
                            param.get_type().get_declaration(
                                function_def.get_scope(), 
                                param.get_name().prettyprint(),
                                Type::PARAMETER_DECLARATION
                                ),
                            ","
                            );
                }

                if (has_ellipsis)
                {
                    stm_parameters.append_with_separator("...", ",");
                }

                AST_t stm_function_tree = stm_function_source.parse_declaration(function_def.get_ast(),
                        function_def.get_scope_link());

                function_def.prepend_sibling(stm_function_tree);
            }

            virtual ~STMFunctionDefFunctor() { }

            STMFunctionDefFunctor(const std::string& filter_file_name, 
                    const std::string& filter_mode_var)
                : function_filter(filter_file_name, filter_mode_var)
            {
            }
    };

    class STMFunctionTransform : public CompilerPhase
    {
        private:
            std::string filter_file_name_str;
            std::string filter_file_mode_str;
        public:
            STMFunctionTransform()
            {
                set_phase_name("STM Function Wrapper");
                set_phase_description("This phase creates STM versions of existing function definitions suitable "
                        "to be called from a transaction environment. It must be run before OMP phase");

                register_parameter("STM_wrap_functions_file",
                        "Filter file of STM-wrapped functions",
                        filter_file_name_str,
                        "./stm_wrap_functions_file");
                register_parameter("STM_wrap_functions_mode",
                        "Filter mode when STM-wrapping functions. It can be either 'normal' or 'inverted'",
                        filter_file_mode_str,
                        "normal"); // MM. changed from "normal" to "inverted"
            }

            virtual void run(DTO& dto)
            {
                AST_t root_node = dto["translation_unit"];
                ScopeLink scope_link = dto["scope_link"];
                
                PredicateAttr function_def_pred(LANG_IS_FUNCTION_DEFINITION) ;
                DepthTraverse depth_traverse;

                STMFunctionDefFunctor function_def_functor(filter_file_name_str, 
                        filter_file_mode_str);

                depth_traverse.add_predicate(function_def_pred, function_def_functor);

                depth_traverse.traverse(root_node, scope_link);
            }
    };
}

EXPORT_PHASE(TL::STMFunctionTransform);
