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
    
    
        // *** Structure storing the call graph of a given function *** //
        
        func_call_graph_t::func_call_graph_t(Symbol s)
            : _root(s), _calls(), _visited(false)
        {}

        Symbol func_call_graph_t::get_symbol()
        {
            return _root;
        }

        void func_call_graph_t::set_symbol(Symbol s)
        {
            _root = s;
        }
        
        bool func_call_graph_t::is_visited()
        {
            return _visited;
        }
        
        void func_call_graph_t::set_visited()
        {
            _visited = true;
        }
        
        void func_call_graph_t::clear_visits()
        {
            if (_visited)
            {
                _visited = false;
                
                for(ObjectList<struct func_call_graph_t*>::iterator it = _calls.begin(); it != _calls.end(); ++it)
                {
                    (*it)->clear_visits();
                }
            }
        }
        
        // *** Common functions *** //
        
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
    }
}