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

#include "tl-functor.hpp"
#include "tl-node.hpp"
#include "tl-extensible-graph.hpp"
#include "tl-nodecl.hpp"

namespace TL
{
    namespace Analysis
    {
        class ExtensibleGraph;
        
        struct var_usage_t {
            ExtensibleSymbol _es;
            char _usage;     // 0 => KILLED, 1 => UE, 2 => UE + KILLED, 3 => UNDEF
            
            var_usage_t(ExtensibleSymbol es, char usage);
            
            ExtensibleSymbol get_extensible_symbol() const;
            Nodecl::NodeclBase get_nodecl() const;
            char get_usage() const;
            void set_usage(char usage);
        };

        bool ext_sym_set_contains_sym(ExtensibleSymbol s, ext_sym_set sym_set);
        ext_sym_set sets_union(ext_sym_set set1, ext_sym_set set2);
        ext_sym_set sets_difference(ext_sym_set set1, ext_sym_set set2);
        bool sets_equals(ext_sym_set set1, ext_sym_set set2);
        
        bool usage_list_contains_nodecl(Nodecl::NodeclBase n, ObjectList<struct var_usage_t*> list);
        bool usage_list_contains_sym(Symbol n, ObjectList<struct var_usage_t*> list);
        struct var_usage_t* get_var_in_list(Nodecl::NodeclBase n, ObjectList<struct var_usage_t*> list);
        struct var_usage_t* get_var_in_list(Symbol n, ObjectList<struct var_usage_t*> list);
        
        std::map<Symbol, Nodecl::NodeclBase> map_params_to_args(Nodecl::NodeclBase func_call, ExtensibleGraph* called_func_graph,
                                                                ObjectList<Symbol> &params, Nodecl::List &args);
        ExtensibleGraph* find_function_for_ipa(Symbol s, ObjectList<ExtensibleGraph*> cfgs);
        
        void print_function_call_nest(ExtensibleGraph *graph);
    }
}

#endif      // TL_ANALYSIS_COMMON_HPP