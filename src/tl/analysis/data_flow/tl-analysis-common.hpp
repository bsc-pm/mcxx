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


#ifndef TL_ANALYSIS_COMMON_HPP
#define TL_ANALYSIS_COMMON_HPP

#include "tl-functor.hpp"
#include "tl-extensible-graph.hpp"
#include "tl-node.hpp"
#include "tl-nodecl.hpp"

namespace TL
{
    namespace Analysis
    {
        class ExtensibleGraph;
        
        struct var_usage_t {
            ExtendedSymbol _es;
            char _usage;     // 0 => KILLED, 1 => UE, 2 => UE + KILLED, 3 => UNDEF, 4 => NOT_COMPUTED
            
            var_usage_t(ExtendedSymbol es, char usage);
            
            ExtendedSymbol get_extensible_symbol() const;
            Nodecl::NodeclBase get_nodecl() const;
            char get_usage() const;
            void set_usage(char usage);
        };

        bool ext_sym_set_contains_sym(ExtendedSymbol s, ext_sym_set sym_set);
        bool ext_sym_set_contains_nodecl(Nodecl::NodeclBase nodecl, ext_sym_set sym_set);
        bool ext_sym_set_contains_englobing_nodecl(ExtendedSymbol ei, ext_sym_set sym_set);
        bool ext_sym_set_contains_englobed_nodecl(ExtendedSymbol ei, ext_sym_set sym_set);
        void delete_englobing_var_from_list(ExtendedSymbol ei, ext_sym_set sym_set);
        void delete_englobed_var_from_list(ExtendedSymbol ei, ext_sym_set sym_set);
        ext_sym_set sets_union(ext_sym_set set1, ext_sym_set set2);
        ext_sym_set sets_difference(ext_sym_set set1, ext_sym_set set2);
        bool sets_equals(ext_sym_set set1, ext_sym_set set2);
        
        bool usage_list_contains_nodecl(Nodecl::NodeclBase n, ObjectList<struct var_usage_t*> list);
        bool usage_list_contains_sym(Symbol n, ObjectList<struct var_usage_t*> list);
        bool usage_list_contains_englobing_nodecl(Nodecl::NodeclBase n, ObjectList<struct var_usage_t*> list);
        bool usage_list_contains_englobed_nodecl(Nodecl::NodeclBase n, ObjectList<struct var_usage_t*> list);
        struct var_usage_t* get_var_in_list(Nodecl::NodeclBase n, ObjectList<struct var_usage_t*> list);
        struct var_usage_t* get_var_in_list(Symbol n, ObjectList<struct var_usage_t*> list);
        void delete_englobing_var_in_usage_list(Nodecl::NodeclBase n, ObjectList<struct var_usage_t*> list);
        void delete_englobed_var_in_usage_list(Nodecl::NodeclBase n, ObjectList<struct var_usage_t*> list);
        
        Nodecl::List get_func_call_args(Nodecl::NodeclBase func_call);
        std::map<Symbol, Nodecl::NodeclBase> map_reference_params_to_args(Nodecl::NodeclBase func_call, ExtensibleGraph* called_func_graph);
        ObjectList<Symbol> get_reference_params(ExtensibleGraph* called_func_graph);
        ObjectList<Nodecl::NodeclBase> get_reference_params_and_args(Nodecl::NodeclBase func_call, ExtensibleGraph* called_func_graph, 
                                                                     ObjectList<Symbol>& ref_params);
        ObjectList<Nodecl::NodeclBase> get_non_reference_args(Nodecl::NodeclBase func_call, ExtensibleGraph* called_func_graph);
        ExtensibleGraph* find_function_for_ipa(Symbol s, ObjectList<ExtensibleGraph*> cfgs);
        
        void print_function_call_nest(ExtensibleGraph *graph);
    }
}

#endif      // TL_ANALYSIS_COMMON_HPP