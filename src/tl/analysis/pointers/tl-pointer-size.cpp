/*--------------------------------------------------------------------
 ( C) Copyright 2006-2014 Barcelona Supercomputing Center             *
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


#include "tl-pointer-size.hpp"

namespace TL{
namespace Analysis{
    
    PointerSize::PointerSize(ExtensibleGraph* pcfg)
        : _pcfg(pcfg)
    {}
    
    void PointerSize::compute_pointer_vars_size_rec(Node* current)
    {
        if(current->is_visited())
            return;
        
        current->set_visited(true);
        if(current->is_graph_node())
        {
            compute_pointer_vars_size_rec(current->get_graph_entry_node());
        }
        else
        {
            if(current->has_statements())
            {
                NBase s;
                NBase value;
                TL::Type t;
                NodeclList stmts = current->get_statements();
                for(NodeclList::iterator it = stmts.begin(); it != stmts.end(); ++it)
                {
                    // If assignment (or object init) check whether its for is a dynamic allocation of resources for a pointer type
                    if(it->is<Nodecl::ObjectInit>() || it->is<Nodecl::Assignment>())
                    {
                        // Get the variable assigned and the value used for the assignment
                        if(it->is<Nodecl::ObjectInit>())
                        {
                            Symbol tmp(it->get_symbol());
                            s = Nodecl::Symbol::make(tmp);
                            t = tmp.get_type();
                            s.set_type(t);
                            value = tmp.get_value().no_conv();
                        }
                        else if(it->is<Nodecl::Assignment>())
                        {
                            s = it->as<Nodecl::Assignment>().get_lhs().no_conv();
                            t = s.get_type().no_ref();
                            if(!s.is<Nodecl::Symbol>() && !s.is<Nodecl::ClassMemberAccess>() && !s.is<Nodecl::ArraySubscript>())
                                continue;
                            value = it->as<Nodecl::Assignment>().get_rhs().no_conv();
                        }
                        
                        // Check whether this is a pointer and the assignment is a recognized memory operation
                        if(t.is_pointer() && !value.is_null())      // This can be null if uninitialized ObjectInit
                        {
                            if(value.is<Nodecl::FunctionCall>())
                            {
                                Symbol called_sym = value.as<Nodecl::FunctionCall>().get_called().get_symbol();
                                Type return_t = called_sym.get_type().returns();
                                Nodecl::List args = value.as<Nodecl::FunctionCall>().get_arguments().as<Nodecl::List>();
                                std::string sym_name = called_sym.get_name();
                                NBase size = NBase::null();
                                if((sym_name == "malloc") && (args.size() == 1))
                                {   // void* malloc (size_t size);
                                    Type arg0_t = args[0].get_type();
                                    if(return_t.is_pointer() && return_t.points_to().is_void() && arg0_t.is_same_type(get_size_t_type()))
                                    {   // We recognize the form 'sizeof(base_type) * n_elemes' and 'n_elemes * sizeof(base_type)'
                                        if(args[0].is<Nodecl::Mul>())
                                        {
                                            NBase lhs = args[0].as<Nodecl::Mul>().get_lhs().no_conv();
                                            NBase rhs = args[0].as<Nodecl::Mul>().get_rhs().no_conv();
                                            if(lhs.is<Nodecl::Sizeof>() && (rhs.is<Nodecl::IntegerLiteral>() || rhs.is<Nodecl::Symbol>()))
                                                size = rhs;
                                            else if(rhs.is<Nodecl::Sizeof>() && (lhs.is<Nodecl::IntegerLiteral>() || lhs.is<Nodecl::Symbol>()))
                                                size = lhs;
                                        }
                                    }
                                }
                                else if((sym_name == "calloc") && (args.size() == 2))
                                {   // void* calloc (size_t num, size_t size);
                                    Type arg0_t = args[0].get_type();
                                    Type arg1_t = args[1].get_type();
                                    if(return_t.is_pointer() && return_t.points_to().is_void()
                                            && arg0_t.is_same_type(get_size_t_type())
                                            && arg1_t.is_same_type(get_size_t_type()))
                                    {
                                        size = args[0];
                                    }
                                }
                                
                                if(!size.is_null())
                                    _pcfg->set_pointer_n_elems(s, size);
                            }
                        }
                        
                        // Clear up the common variables s, value and t
                        s = NBase::null();
                        value = NBase::null();
                        t = Type();
                    }
                }
            }
        }
        
        // Keep iterating over the children
        ObjectList<Node*> children = current->get_children();
        for(ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
            compute_pointer_vars_size_rec(*it);
    }
    
    void PointerSize::compute_pointer_vars_size()
    {
        Node* graph = _pcfg->get_graph();
        compute_pointer_vars_size_rec(graph);
        ExtensibleGraph::clear_visits(graph);
        
        _pcfg->purge_non_constant_pointer_n_elems();
    }
}
}
