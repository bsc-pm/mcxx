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