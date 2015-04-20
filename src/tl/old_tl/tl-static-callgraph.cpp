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




#include "tl-static-callgraph.hpp"

namespace TL
{
    void StaticCallGraph::empty_data()
    {
        _explicitly_called.clear();
        _implicitly_called.clear();
        _call_map.clear();
    }

    void StaticCallGraph::compute_call_graph(AST_t a, 
            ObjectList<Symbol> &explicitly_called,
            ObjectList<Symbol> &implicitly_called)
    {
        // Explicit calls
        ObjectList<AST_t> explicit_function_calls =
            a.depth_subtrees(PredicateAttr(LANG_IS_FUNCTION_CALL));

        for (ObjectList<AST_t>::iterator it = explicit_function_calls.begin();
                it != explicit_function_calls.end();
                it++)
        {
            AST_t lang_called_expression = it->get_link_to_child(LANG_CALLED_EXPRESSION);
            Symbol called_symbol = lang_called_expression.get_attribute(LANG_FUNCTION_SYMBOL);

            if (called_symbol.is_valid())
            {
                explicitly_called.insert(called_symbol);
            }
            else
            {
                std::cerr << "Something is wrong with node "
                    << "'" << lang_called_expression.prettyprint() << "'"
                    << " at "
                    << "'" << lang_called_expression.get_locus() << "'" << std::endl;
            }
        }

        // Implicit calls (only in C++)
        ObjectList<AST_t> implicit_function_calls = 
            a.depth_subtrees(PredicateAttr(LANG_IS_IMPLICIT_CALL));

        for (ObjectList<AST_t>::iterator it = implicit_function_calls.begin();
                it != implicit_function_calls.end();
                it++)
        {
            Symbol called_symbol = it->get_attribute(LANG_IMPLICIT_CALL);

            if (called_symbol.is_valid())
            {
                implicitly_called.insert(called_symbol);
            }
            else
            {
                std::cerr << "Something is wrong with node "
                    << "'" << it->prettyprint() << "'"
                    << " at '" << it->get_locus() << "'" << std::endl;
            }
        }
    }

    void StaticCallGraph::compute_statement(Statement stmt)
    {
        empty_data();

        ObjectList<Symbol> explicit_function_calls;
        ObjectList<Symbol> implicit_function_calls;

        compute_call_graph(stmt.get_ast(),
                explicit_function_calls,
                implicit_function_calls);

        _explicitly_called.insert(explicit_function_calls);
        _implicitly_called.insert(implicit_function_calls);
    }

    void StaticCallGraph::compute_expression(Expression expr)
    {
        empty_data();

        ObjectList<Symbol> explicit_function_calls;
        ObjectList<Symbol> implicit_function_calls;

        compute_call_graph(expr.get_ast(),
                explicit_function_calls,
                implicit_function_calls);

        _explicitly_called.insert(explicit_function_calls);
        _implicitly_called.insert(implicit_function_calls);
    }

    void StaticCallGraph::compute_global(AST_t translation_unit, ScopeLink scope_link)
    {
        // Start anew
        empty_data();

        ObjectList<AST_t> function_definition_trees
            = translation_unit.depth_subtrees(FunctionDefinition::predicate);

        for (ObjectList<AST_t>::iterator it = function_definition_trees.begin();
                it != function_definition_trees.end();
                it++)
        {
            FunctionDefinition function_definition(*it, scope_link);
            DeclaredEntity declared_entity = function_definition.get_declared_entity();

            Symbol function_symbol = declared_entity.get_declared_symbol();

            if (function_symbol.is_valid())
            {
                ObjectList<Symbol> explicit_function_calls;
                ObjectList<Symbol> implicit_function_calls;

                compute_call_graph(function_definition.get_function_body().get_ast(),
                        explicit_function_calls,
                        implicit_function_calls);

                _call_map[function_symbol] = explicit_implicit_pair_t(explicit_function_calls, 
                        implicit_function_calls);

                _explicitly_called.insert(explicit_function_calls);
                _implicitly_called.insert(implicit_function_calls);
            }
            else
            {
                std::cerr << "Something is wrong in function definition "
                    << "'" 
                    << declared_entity.prettyprint() 
                    << "'"
                    << std::endl;
            }
        }
    }

    ObjectList<Symbol> StaticCallGraph::get_all_called_functions()
    {
        ObjectList<Symbol> result;
        result.insert(_explicitly_called);
        result.insert(_implicitly_called);

        return result;
    }

    ObjectList<Symbol> StaticCallGraph::get_all_explicitly_called_functions()
    {
        return _explicitly_called;
    }

    ObjectList<Symbol> StaticCallGraph::get_all_implicitly_called_functions()
    {
        return _implicitly_called;
    }

    ObjectList<Symbol> StaticCallGraph::get_all_called_functions_from(Symbol function)
    {
        ObjectList<Symbol> result;

        if (_call_map.find(function) != _call_map.end())
        {
            explicit_implicit_pair_t pair = _call_map[function];

            // Explicit
            result.insert(pair.first);
            // Implicit
            result.insert(pair.second);
        }

        return result;
    }

    ObjectList<Symbol> StaticCallGraph::get_all_explicitly_called_functions_from(Symbol function)
    {
        ObjectList<Symbol> result;

        if (_call_map.find(function) != _call_map.end())
        {
            explicit_implicit_pair_t pair = _call_map[function];

            // Explicit
            result.insert(pair.first);
        }

        return result;
    }

    ObjectList<Symbol> StaticCallGraph::get_all_implicitly_called_functions_from(Symbol function)
    {
        ObjectList<Symbol> result;

        if (_call_map.find(function) != _call_map.end())
        {
            explicit_implicit_pair_t pair = _call_map[function];

            // Implicit
            result.insert(pair.second);
        }

        return result;
    }

    ObjectList<Symbol> StaticCallGraph::get_all_functions_calling_to(Symbol function)
    {
        ObjectList<Symbol> result;
        for(std::map<Symbol, explicit_implicit_pair_t>::iterator it = _call_map.begin();
                it != _call_map.end();
                it++)
        {
            explicit_implicit_pair_t pair = it->second;

            // Explicit
            if (pair.first.contains(function))
            {
                result.insert(it->first);
            }
            // Implicit
            if (pair.second.contains(function))
            {
                result.insert(it->first);
            }
        }
        return result;
    }

    ObjectList<Symbol> StaticCallGraph::get_all_functions_calling_explicitly_to(Symbol function)
    {
        ObjectList<Symbol> result;
        for(std::map<Symbol, explicit_implicit_pair_t>::iterator it = _call_map.begin();
                it != _call_map.end();
                it++)
        {
            explicit_implicit_pair_t pair = it->second;

            // Explicit
            if (pair.first.contains(function))
            {
                result.insert(it->first);
            }
        }
        return result;
    }

    ObjectList<Symbol> StaticCallGraph::get_all_functions_calling_implicitly_to(Symbol function)
    {
        ObjectList<Symbol> result;
        for(std::map<Symbol, explicit_implicit_pair_t>::iterator it = _call_map.begin();
                it != _call_map.end();
                it++)
        {
            explicit_implicit_pair_t pair = it->second;

            // Implicit
            if (pair.second.contains(function))
            {
                result.insert(it->first);
            }
        }
        return result;
    }
}
