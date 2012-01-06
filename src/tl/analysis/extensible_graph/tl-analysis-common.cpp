/*--------------------------------------------------------------------
(C) Copyright 2006-2009 Barcelona Supercomputing Center 
Centro Nacional de Supercomputacion

This file is part of Mercurium C/C++ source-to-source compiler.

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

#include "tl-analysis-common.hpp"
#include "tl-nodecl-alg.hpp"

namespace TL
{
    namespace Analysis
    {
        // *** Structure storing symbols and the kind of use (used, defined or unknown) in a given function *** //
        
        var_usage_t::var_usage_t(Nodecl::Symbol s, char usage)
            : _sym(s), _usage(usage)
        {}
        
        Nodecl::Symbol var_usage_t::get_nodecl() const
        {
            return _sym;
        }
        
        char var_usage_t::get_usage() const
        {
            return _usage;
        }

        void var_usage_t::set_usage(char usage)
        {
            _usage = usage;
        }
        
        
        // *** Common functions *** //
        
        std::map<Symbol, Nodecl::NodeclBase> map_params_to_args(Nodecl::NodeclBase func_call, ExtensibleGraph* called_func_graph,
                                                                ObjectList<Symbol> &params, Nodecl::List &args)
        {
            params = called_func_graph->get_function_parameters();
            if (func_call.is<Nodecl::FunctionCall>())
            {
                Nodecl::FunctionCall aux = func_call.as<Nodecl::FunctionCall>();
                args = aux.get_arguments().as<Nodecl::List>();
            }
            else
            {   // is VirtualFunctionCall
                Nodecl::VirtualFunctionCall aux = func_call.as<Nodecl::VirtualFunctionCall>();
                args = aux.get_arguments().as<Nodecl::List>();
            }
            std::map<Symbol, Nodecl::NodeclBase> params_to_args;
            int i = 0;
            ObjectList<Symbol>::iterator itp = params.begin();
            Nodecl::List::iterator ita = args.begin();
            for(; itp != params.end() && ita != args.end(); ++itp, ++ita)
            {
                params_to_args[*itp] = *ita;
            }
            
            return params_to_args;
        }
        
        ExtensibleGraph* find_function_for_ipa(Symbol s, ObjectList<ExtensibleGraph*> cfgs)
        {
            for(ObjectList<ExtensibleGraph*>::iterator it = cfgs.begin(); it != cfgs.end(); ++it)
            {
                if (s.is_valid() && (s == (*it)->get_function_symbol()))
                {
                    return *it;
                }
            }
            return NULL;
        }
        
        // *** Operation over containers with non simple values *** //
        
        bool ext_sym_set_contains_sym(ExtensibleSymbol s, ext_sym_set sym_set)
        {
            if (sym_set.find(s).empty())
            {
                return false;
            }
            return true;
        }
        
        ext_sym_set sets_union(ext_sym_set set1, ext_sym_set set2)
        {
            ext_sym_set result = set1;
            for (ext_sym_set::iterator it = set2.begin(); it != set2.end(); ++it)
            {
                if (!ext_sym_set_contains_sym(*it, set1))
                    result.insert(*it);
            }
            return result;
            
//         std::vector<ExtensibleSymbol> v_result(set1.size() + set2.size());
//         std::vector<ExtensibleSymbol>::iterator it;
//         ext_sym_set result;
//         it = set_union(set1.begin(), set1.end(), set2.begin(), set2.end(), v_result.begin());
//         for(int i=0; i<int(it-v_result.begin()); i++)
//         {    
//             result.insert(v_result.at(i));
//         }
//         return result;

        }
    
        ext_sym_set sets_difference(ext_sym_set set1, ext_sym_set set2)
        {
            ext_sym_set result;
            for (ext_sym_set::iterator it = set1.begin(); it != set1.end(); ++it)
            {
                if (!ext_sym_set_contains_sym(*it, set2))
                    result.insert(*it);
            }
            return result;

//         std::vector<ExtensibleSymbol> v_result(set1.size());
//         std::vector<ExtensibleSymbol>::iterator it;
//         ext_sym_set result;
//         it = set_difference(set1.begin(), set1.end(), set2.begin(), set2.end(), v_result.begin());
//         for(int i=0; i<int(it-v_result.begin()); i++)
//         {    
//             result.insert(v_result.at(i));
//         }
//         return result;
        }

        bool sets_equals(ext_sym_set set1, ext_sym_set set2)
        {
            bool res = false;
            if (set1.size() == set2.size())
            {
                res = true;
                for(ext_sym_set::iterator it = set1.begin(); it != set1.end(); ++it)
                {
                    if (!ext_sym_set_contains_sym(*it, set2))
                    {    
                        res = false;
                        break;
                    }
                }
//             std::vector<ExtensibleSymbol>::iterator it;
//             std::vector<ExtensibleSymbol> v_result(set1.size());
//             it = set_intersection(set1.begin(), set1.end(), set2.begin(), set2.end(), v_result.begin());
//             return (int(it-v_result.begin()) == set1.size());
            }
            return res;
        } 
        
        bool usage_list_contains_sym(Nodecl::Symbol n, ObjectList<struct var_usage_t*> list)
        {
            for (ObjectList<struct var_usage_t*>::iterator it = list.begin(); it != list.end(); ++it)
            {
                if (Nodecl::Utils::equal_nodecls((*it)->get_nodecl(), n))
                {
                    return true;
                }
            }
            return false;
        }
        
        bool usage_list_contains_sym(Symbol n, ObjectList<struct var_usage_t*> list)
        {
            for (ObjectList<struct var_usage_t*>::iterator it = list.begin(); it != list.end(); ++it)
            {
                if ((*it)->get_nodecl().get_symbol() == n)
                {
                    return true;
                }
            }
            return false;
        }
        
        void print_function_call_nest(ExtensibleGraph *graph)
        {   // TODO Create a dot graph with the Call graph hanging from 'graph'
        }
    }
}