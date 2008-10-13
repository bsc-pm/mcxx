/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2008 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
    Barcelona Supercomputing Center - Centro Nacional de Supercomputacion
    Universitat Politecnica de Catalunya

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
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
        shadow_declaration += "const char* __file, int __line";

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
            // This will never trigger
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

        std::string mangled_function_name = "\"" + function_name.mangle_id_expression() + "\"";

        before_code
            << "const int EVENT_CALL_USER_FUNCTION = 60000018;"
            << "int _user_function_event = mintaka_index_get(__file, __line);"
            << "if (_user_function_event == -1)"
            << "{"
            << "   extern pthread_mutex_t __mintaka_instr_global_lock;"
            << "   pthread_mutex_lock(&__mintaka_instr_global_lock);"
            << "   {"
            << "        _user_function_event = mintaka_index_allocate2(__file, __line, "
            << mangled_function_name << ", EVENT_CALL_USER_FUNCTION);"
            << "   }"
            << "   pthread_mutex_unlock(&__mintaka_instr_global_lock);"
            << "}"
            << "mintaka_event(EVENT_CALL_USER_FUNCTION, _user_function_event);"
            ;

        after_code
            << "mintaka_event(EVENT_CALL_USER_FUNCTION, 0);"
            ;

        AST_t shadow_function_def_tree = 
            shadow_function_definition.parse_global(function_definition.get_ast(),
                    function_definition.get_scope_link());

        function_definition.get_ast().prepend_sibling_function(shadow_function_def_tree);

        return true;
    }

    InstrumentCalls::MainWrapper::MainWrapper(ScopeLink sl)
        : _sl(sl)
    {
    }

    void InstrumentCalls::PthreadFunctionDef::preorder(Context, AST_t) { }

    void InstrumentCalls::PthreadFunctionDef::postorder(Context ctx, AST_t node)
    {
        FunctionDefinition function_def(node, _sl);

        IdExpression function_name = function_def.get_function_name();

        std::cerr << "Wrapping pthread function: " << function_name.prettyprint() 
            << " at " << function_name.get_ast().get_locus() << std::endl;

        Statement function_body = function_def.get_function_body();

        Source new_function_body, begin_mintaka_src, end_mintaka_src;

        // Replace all returns
        PredicateAST<LANG_IS_RETURN_STATEMENT> return_statement_pred;
        ObjectList<AST_t> return_statements = function_body.get_ast().depth_subtrees(return_statement_pred);
        for (ObjectList<AST_t>::iterator it = return_statements.begin();
                it != return_statements.end();
                it++)
        {
            Source replaced_return;

            replaced_return
                << "{"
                <<    comment("Intercepted return")
                <<    "mintaka_thread_end();"
                <<    it->prettyprint()
                << "}"
                ;

            AST_t replaced_return_tree = replaced_return.parse_statement((*it),
                    ctx.scope_link);

            it->replace(replaced_return_tree);
        }

        new_function_body
            << "{"
            <<    comment("Begin of thread code")
            <<    begin_mintaka_src
            <<    function_body.prettyprint()
            <<    end_mintaka_src
            << "}"
            ;

        begin_mintaka_src
            << "{"
            << "extern pthread_mutex_t __mintaka_instr_global_lock;"
            << "extern int __mintaka_pthread_global_counter;"

            << "int __thread_id;"
            << "pthread_mutex_lock(&__mintaka_instr_global_lock);"
            << "__thread_id = (++__mintaka_pthread_global_counter);"
            << "pthread_mutex_unlock(&__mintaka_instr_global_lock);"
            << "mintaka_thread_begin(1, __thread_id);"
            << "mintaka_state_run();"
            << "}"
            ;

        end_mintaka_src
            << "{"
            << comment("Fallback exit")
            << "mintaka_thread_end();"
            << "}"
            ;

        Source fake_variables;

        fake_variables.parse_statement(function_body.get_ast(),
                ctx.scope_link);

        AST_t new_function_body_tree = new_function_body.parse_statement(function_body.get_ast(),
                ctx.scope_link);
        function_body.get_ast().replace(new_function_body_tree);
    }

    void InstrumentCalls::MainWrapper::preorder(Context, AST_t)
    {
        // Do nothing
    }

    void InstrumentCalls::MainWrapper::postorder(Context, AST_t node)
    {
        FunctionDefinition function_def(node, _sl);
        IdExpression function_name = function_def.get_function_name();

        Symbol function_symbol = function_name.get_symbol();
        Type function_type = function_symbol.get_type();

        ObjectList<std::string> parameters;

        Source main_declaration = function_type.get_declaration_with_parameters(function_symbol.get_scope(),
                "main", parameters);

        // "main" is always an unqualified name so this transformation is safe
        function_name.get_ast().replace_text("__instrumented_main");

        Source instrumented_main_declaration = function_type.get_declaration(function_symbol.get_scope(),
                "__instrumented_main");
        Source null_expr;

        C_LANGUAGE()
        {
            null_expr << "(void*)0";
        }
        CXX_LANGUAGE()
        {
            null_expr << "0";
        }

        Source new_main;
        new_main
            << instrumented_main_declaration << ";"
            << "pthread_mutex_t __mintaka_instr_global_lock;"
            << "int __mintaka_pthread_global_counter;"
            << main_declaration
            << "{"
            // Begin
            << "  pthread_mutex_init(&__mintaka_instr_global_lock, " <<  null_expr << ");"
            << "  __mintaka_pthread_global_counter = 0;"
            << "  mintaka_app_begin();"
            << "  mintaka_set_filebase(_p_1[0]);"
            << "  mintaka_thread_begin(1, ++__mintaka_pthread_global_counter);"
            << "  mintaka_state_run();"
            // Event definition
            << "  static const char* EVENT_CALL_USER_FUNCTION_DESCR = \"User function call\";"
            << "  const int EVENT_CALL_USER_FUNCTION = 60000018;"
            << "  mintaka_index_event(EVENT_CALL_USER_FUNCTION, EVENT_CALL_USER_FUNCTION_DESCR);"
            // Program
            << "  int __result = __instrumented_main(_p_0, _p_1);"
            // End
            << "  mintaka_thread_end();"
            << "  mintaka_app_end();"
            << "  mintaka_merge();"
            << "  mintaka_index_generate();"
            << "  return __result;"
            << "}"
            << node.prettyprint()
            ;

        AST_t new_main_tree = new_main.parse_global(function_def.get_ast(),
                function_def.get_scope_link());

        node.replace(new_main_tree);
    }

    InstrumentCalls::MainPredicate::MainPredicate(ScopeLink& sl)
        : _sl(sl)
    {
    }

    bool InstrumentCalls::MainPredicate::operator()(AST_t& t) const
    {
        if (is_function_def(t))
        {
            FunctionDefinition function_def(t, _sl);

            IdExpression function_name = function_def.get_function_name();

            if (function_name.mangle_id_expression() == "main")
            {
                Symbol function_symbol = function_name.get_symbol();

                if (!function_symbol.is_member())
                {
                    return true;
                }
            }

        }
        return false;
    }

    InstrumentCalls::PthreadFunctionPred::PthreadFunctionPred(ScopeLink& sl, InstrumentFilterFile& pthread_functions)
        : _sl(sl), _pthread_functions(pthread_functions)
    {
    }

    bool InstrumentCalls::PthreadFunctionPred::operator()(AST_t& t) const
    {
        if (is_function_def(t))
        {
            FunctionDefinition function_def(t, _sl);
            IdExpression function_name = function_def.get_function_name();

            if (_pthread_functions.match(function_name.mangle_id_expression()))
            {
                Symbol function_symbol = function_name.get_symbol();

                if (!function_symbol.is_member())
                {
                    return true;
                }
            }

        }
        return false;
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

        PredicateAST<LANG_IS_FUNCTION_CALL> function_call_pred;
        InstrumentCallsFunctor instrumentation_functor(_instrument_filter);

        MainWrapper mainwrapper_functor(scope_link);
        MainPredicate main_function_def(scope_link);

        PthreadFunctionDef pthread_wrapper(scope_link);
        PthreadFunctionPred pthread_function_defs(scope_link, _pthread_functions);

        depth_traverse.add_predicate(function_call_pred, instrumentation_functor);
        depth_traverse.add_predicate(pthread_function_defs, pthread_wrapper);
        depth_traverse.add_predicate(main_function_def, mainwrapper_functor);

        depth_traverse.traverse(root_node, scope_link);
    }

    InstrumentCalls::~InstrumentCalls()
    {
    }

    InstrumentCalls::InstrumentCalls(const std::string& instrument_file_name, const std::string& instrument_filter_mode,
            const std::string& pthread_functions_file)
        : _instrument_filter(instrument_file_name, instrument_filter_mode),
        _pthread_functions(pthread_functions_file, "normal")
    {
        // Kludge
        _instrument_filter.avoid_mintaka();

        std::cerr << "Instrumentation of calls support loaded" << std::endl;
    }
}
