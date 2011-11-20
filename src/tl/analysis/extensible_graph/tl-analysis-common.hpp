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

#ifndef TL_ANALYSIS_COMMON_HPP
#define TL_ANALYSIS_COMMON_HPP

#include "tl-nodecl.hpp"

namespace TL
{
    namespace Analysis
    {
        struct var_usage_t {
            Nodecl::Symbol _sym;
            char _usage;     // 0 => KILLED, 1 => UE, 2 => UE + KILLED, 3 => UNDEF
            
            var_usage_t(Nodecl::Symbol s, char usage);
            
            Nodecl::Symbol get_nodecl() const;
            char get_usage() const;
            void set_usage(char usage);
        };
        
        struct func_call_graph_t {
            Symbol _root;
            ObjectList<struct func_call_graph_t*> _calls;
            bool _visited;
            
            func_call_graph_t(Symbol s);

            Symbol get_symbol();
            void set_symbol(Symbol s);
            
            bool is_visited();
            void set_visited();
            void clear_visits();
        };
        
        bool usage_list_contains_sym(Nodecl::Symbol n, ObjectList<struct var_usage_t*> list);
    }
}

#endif      // TL_ANALYSIS_COMMON_HPP