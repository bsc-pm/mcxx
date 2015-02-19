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




#include "cxx-utils.h"

#include "tl-instrumentation.hpp"
#include "tl-instrumentcalls.hpp"
#include "tl-compilerphase.hpp"
#include "tl-predicateutils.hpp"
#include "tl-langconstruct.hpp"
#include "tl-traverse.hpp"
#include "tl-scopelink.hpp"

#include <iostream>
#include <fstream>
#include <set>

namespace TL
{
    InstrumentCalls::InstrumentCallsFunctor::InstrumentCallsFunctor(InstrumentFilterFile& instrument_filter)
        : _instrument_filter(instrument_filter)
    {
    }

    void InstrumentCalls::InstrumentCallsFunctor::preorder(Context, AST_t)
    {
        // Do nothing
    }

    void InstrumentCalls::InstrumentCallsFunctor::postorder(Context ctx, AST_t node)
    {
        ScopeLink scope_link = ctx.scope_link;

        AST_t called_expression_tree = node.get_attribute(LANG_CALLED_EXPRESSION);
        AST_t arguments_tree = node.get_attribute(LANG_FUNCTION_ARGUMENTS);
        Expression called_expression(called_expression_tree, scope_link);

        // Only function-names are considered here
        if (!called_expression.is_id_expression())
        {
            // std::cerr << "Called expression is not an id expression" << std::endl;
            return;
        }

        IdExpression called_id_expression = called_expression.get_id_expression();

        if (!_instrument_filter.match(called_id_expression.prettyprint()))
            return;

        std::string shadow_function_name = 
            "_" + called_id_expression.mangle_id_expression() + "_instr";

        if (defined_shadows.find(shadow_function_name) == defined_shadows.end())
        {
            // The shadow has not been defined, define it here
            if (!define_shadow(called_id_expression, shadow_function_name))
            {
                // Ignore this shadow
                return;
            }
            defined_shadows.insert(shadow_function_name);
        }

        // Now replace the arguments
        Source replaced_arguments;

        replaced_arguments 
            << "\"" << node.get_file() << "\""
            << ","
            << node.get_line()
            ;

        if (arguments_tree.prettyprint() != "")
        {
            replaced_arguments << "," << arguments_tree.prettyprint(/*commas=*/true);
        }

        // Now create an expression tree
        Source shadow_function_call;
        shadow_function_call
            << shadow_function_name << "(" << replaced_arguments << ")"
            ;

        AST_t shadow_function_call_tree = 
            shadow_function_call.parse_expression(node, scope_link);

        node.replace(shadow_function_call_tree);
    }

    bool InstrumentCalls::InstrumentCallsFunctor::define_shadow(IdExpression function_name, std::string shadow_function_name)
    {
        FunctionDefinition function_definition = function_name.get_enclosing_function();

        Symbol function_symbol = function_name.get_symbol();

        if (!function_symbol.is_valid())
        {
            std::cerr << "Function '" << function_name.prettyprint() << "' referenced in " 
                << function_name.get_ast().get_locus() 
                << " is unknown. It will not be instrumented." << std::endl;
            return false;
        }

        Type function_type = function_symbol.get_type();

        ObjectList<std::string> parameter_names;

        std::string shadow_declaration;

        shadow_declaration = function_type.returns().get_declaration(function_symbol.get_scope(), "");
        shadow_declaration += " ";
        shadow_declaration += shadow_function_name;
        shadow_declaration += "(";
        shadow_declaration += "char* __file, int __line";

        bool has_ellipsis = false;
        ObjectList<Type> parameters = function_type.parameters(has_ellipsis);

        if (has_ellipsis)
        {
            std::cerr << "Function '" << function_name.prettyprint() << "' referenced in "
                << function_name.get_ast().get_locus()
                << " has variable-length arguments. It will not be instrumented." << std::endl;
            return false;
        }

        int param_num = 0;
        for (ObjectList<Type>::iterator it = parameters.begin();
                it != parameters.end();
                it++)
        {
            std::stringstream ss;
            ss << "_p_" << param_num;
            shadow_declaration += "," + it->get_declaration(function_symbol.get_scope(), ss.str());

            parameter_names.append(ss.str());
            param_num++;
        }

        if (has_ellipsis)
        {
            shadow_declaration += ", ...";
        }

        shadow_declaration += ")";
        // std::string shadow_declaration = function_type.get_declaration_with_parameters(function_symbol.get_scope(), 
        //         shadow_function_name, parameter_names);

        Source shadow_function_definition;

        Source original_arguments;

        for (ObjectList<std::string>::iterator it = parameter_names.begin();
                it != parameter_names.end();
                it++)
        {
            original_arguments.append_with_separator((*it), ",");
        }

        Source invocation;
        Source before_code;
        Source after_code;

        shadow_function_definition
            << "static inline " << shadow_declaration
            << "{"
            <<     invocation
            << "}"
            ;

        if (!function_type.returns().is_void())
        {
            invocation
                << before_code
                << function_type.returns().get_declaration(function_definition.get_scope(), "_result") << " = "
                << function_name.prettyprint() << "(" << original_arguments << ");"
                << after_code
                << "return _result;"
                ;
        }
        else
        {
            invocation
                << before_code
                << function_name.prettyprint() << "(" << original_arguments << ");"
                << after_code
                ;
        }

        // This mangling is annoying
        std::string mangled_function_name = "\"" + function_name.mangle_id_expression() + "\"";

        before_code
            << "nth_instrumentation_ctx ctx;"
            << "nth_instrument_push_ctx(&ctx, __file, __line, " << mangled_function_name << ");"
            ;

        after_code
            << "nth_instrument_pop_ctx();"
            ;

        AST_t shadow_function_def_tree = 
            shadow_function_definition.parse_global(function_definition.get_ast(),
                    function_definition.get_scope_link());

        function_definition.get_ast().prepend_sibling_function(shadow_function_def_tree);

        return true;
    }

    void InstrumentCalls::pre_run(DTO& data_flow)
    {
    }

    void InstrumentCalls::run(DTO& data_flow)
    {
        AST_t root_node = data_flow["translation_unit"];
        ScopeLink scope_link = data_flow["scope_link"];

        // Traversal of LANG_IS_FUNCTION_CALLs
        DepthTraverse depth_traverse;

        PredicateAttr function_call_pred(LANG_IS_FUNCTION_CALL) ;
        InstrumentCallsFunctor instrumentation_functor(_instrument_filter);

        depth_traverse.add_predicate(function_call_pred, instrumentation_functor);
        depth_traverse.traverse(root_node, scope_link);
    }

    InstrumentCalls::~InstrumentCalls()
    {
    }

    InstrumentCalls::InstrumentCalls(const std::string& instrument_file_name, const std::string& instrument_filter_mode)
        : _instrument_filter(instrument_file_name, instrument_filter_mode)
    {
        std::cerr << "Instrumentation of calls support loaded" << std::endl;
    }
}
