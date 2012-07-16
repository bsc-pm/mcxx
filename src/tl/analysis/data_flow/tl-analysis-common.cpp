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
#include "tl-nodecl-utils.hpp"

namespace TL
{
    namespace Analysis
    {   
        
        // *** Common functions *** //
        
        Nodecl::List get_func_call_args(Nodecl::NodeclBase func_call)
        {
          Nodecl::List args;
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
            return args;
        }
        
        std::map<Symbol, Nodecl::NodeclBase> map_reference_params_to_args(Nodecl::NodeclBase func_call, ExtensibleGraph* called_func_graph)
        {
            ObjectList<Symbol> params = called_func_graph->get_function_parameters();
            Nodecl::List args = get_func_call_args(func_call);
            ObjectList<Symbol> ref_params;
            ObjectList<Nodecl::NodeclBase> ref_args;
            
            // Keep only references and pointers
            ObjectList<Symbol>::iterator itp = params.begin();
            Nodecl::List::iterator ita = args.begin();
            for (; itp != params.end(); ++itp)
            {
                Type param_type = itp->get_type();
                if (param_type.is_any_reference() || param_type.is_pointer())
                {
                    ref_params.append(*itp);
                    ref_args.append(*ita);
                }
            }
            
            // Map parameters with arguments
            std::map<Symbol, Nodecl::NodeclBase> ref_params_to_args;
            itp = params.begin();
            ita = args.begin();
            for(; itp != params.end() && ita != args.end(); ++itp, ++ita)
            {
                ref_params_to_args[*itp] = *ita;
            }
            
            return ref_params_to_args;
        }

        ObjectList<Symbol> get_reference_params(ExtensibleGraph* called_func_graph)
        {
            ObjectList<Symbol> ref_params;
            ObjectList<Symbol> params = called_func_graph->get_function_parameters();
            for(ObjectList<Symbol>::iterator it = params.begin(); it != params.end(); ++it)
            {
                Type param_type = it->get_type();
                if (param_type.is_any_reference() || param_type.is_pointer())
                {
                    ref_params.append(*it);
                }
            }
            return ref_params;
        }
        
        ObjectList<Nodecl::NodeclBase> get_reference_params_and_args(Nodecl::NodeclBase func_call, ExtensibleGraph* called_func_graph, 
                                                                     ObjectList<Symbol>& ref_params)
        {
            ObjectList<Symbol> params = called_func_graph->get_function_parameters();
            Nodecl::List args = get_func_call_args(func_call);
            ObjectList<Nodecl::NodeclBase> ref_args;
            ObjectList<Symbol>::iterator itp = params.begin();
            Nodecl::List::iterator ita = args.begin();
            for (; itp != params.end(); ++itp, ++ita)
            {
                Type param_type = itp->get_type();
                if (param_type.is_any_reference() || param_type.is_pointer())
                {
                    ref_params.append(*itp);
                    ref_args.append(*ita);
                }
            }
            return ref_args;
        }
        
        ObjectList<Nodecl::NodeclBase> get_non_reference_args(Nodecl::NodeclBase func_call, ExtensibleGraph* called_func_graph)
        {
            ObjectList<Symbol> params = called_func_graph->get_function_parameters();
            Nodecl::List args = get_func_call_args(func_call);
            ObjectList<Nodecl::NodeclBase> non_ref_args;
            ObjectList<Symbol>::iterator itp = params.begin();
            Nodecl::List::iterator ita = args.begin();
            for (; itp != params.end(); ++itp, ++ita)
            {
                Type param_type = itp->get_type();
                if (!param_type.is_any_reference() && !param_type.is_pointer())
                {
                    non_ref_args.append(*ita);
                }
            }
            return non_ref_args;
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

        void print_function_call_nest(ExtensibleGraph *graph)
        {   // TODO Create a dot graph with the Call graph hanging from 'graph'
        }

    }
}