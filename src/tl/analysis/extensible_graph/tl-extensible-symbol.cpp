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


#include <algorithm>

#include "cxx-codegen.h"
#include "cxx-process.h"

#include "tl-extensible-symbol.hpp"

namespace TL
{
    ExtensibleSymbol::ExtensibleSymbol()
        : _n(Nodecl::NodeclBase::null())
    {}
    
    ExtensibleSymbol::ExtensibleSymbol(Nodecl::NodeclBase n)
        : _n(n)
    {}
    
    Symbol ExtensibleSymbol::get_nodecl_symbol(Nodecl::NodeclBase n) const
    {
        if (n.is<Nodecl::Symbol>() || n.is<Nodecl::PointerToMember>())
        {
            return n.get_symbol();
        }
        else if (n.is<Nodecl::Derreference>())
        {
            Nodecl::Derreference aux = n.as<Nodecl::Derreference>();
            return get_nodecl_symbol(aux.get_rhs());
        }
        else if (n.is<Nodecl::ClassMemberAccess>())
        {
            Nodecl::ClassMemberAccess aux = n.as<Nodecl::ClassMemberAccess>();
            return get_nodecl_symbol(aux.get_member());
        }
        else if (n.is<Nodecl::ArraySubscript>())
        {
            Nodecl::ArraySubscript aux = n.as<Nodecl::ArraySubscript>();
            return get_nodecl_symbol(aux.get_subscripted());
        }
        else if (n.is<Nodecl::FunctionCall>())
        {
            return Symbol();
        }
        else
        {
            internal_error("Unexpected type of nodecl '%s' contained in an ExtendedSymbol", 
                           ast_print_node_type(n.get_kind()));
        }
    }
   
    Symbol ExtensibleSymbol::get_symbol() const
    {
        return get_nodecl_symbol(_n);
    }    
    
    std::string ExtensibleSymbol::get_name() const
    {
        std::string name = "";
        
        Symbol sym(get_nodecl_symbol(_n));
        if (sym.is_valid())
            name = sym.get_name();
        
        return name;
    }
    
    Type ExtensibleSymbol::get_type() const
    {
        Type res(NULL);
        
        Symbol sym(get_nodecl_symbol(_n));
        if (sym.is_valid())
            res = sym.get_type();
        
        return res;
    }
    
    Nodecl::NodeclBase ExtensibleSymbol::get_nodecl() const
    {
        return _n;
    }
    
    bool ExtensibleSymbol::is_simple_symbol() const
    {
        return (_n.is<Nodecl::Symbol>());
    }

    bool ExtensibleSymbol::equal_ast_nodes(nodecl_t n1, nodecl_t n2) const
    {
        if (nodecl_is_null(n1) == nodecl_is_null(n2))
        {
            if (!nodecl_is_null(n1))
            {
                if ((nodecl_get_kind(n1) == nodecl_get_kind(n2))
                    &&  (nodecl_get_symbol(n1) == nodecl_get_symbol(n2)))
                {
                    return true;
                }
            }
            else
            {
                return true;
            }
        }
        return false;
    }
    
    bool ExtensibleSymbol::equal_trees_rec(nodecl_t n1, nodecl_t n2) const
    {
        if (nodecl_is_null(n1) == nodecl_is_null(n2))
        {
            if (!nodecl_is_null(n1))
            {
                if ((nodecl_get_kind(n1) == nodecl_get_kind(n2))
                    &&  (nodecl_get_symbol(n1) == nodecl_get_symbol(n2)))
                {
                    bool equal = true;
                    
                    for (int i = 0; i < MCXX_MAX_AST_CHILDREN && equal; i++)
                    {
                        equal = equal_trees_rec(nodecl_get_child(n1, i), nodecl_get_child(n2, i));
                    }
                    return equal;
                }
            }
            else
            {
                return true;
            }
        }

        return false;
    }

    bool ExtensibleSymbol::equal_nodecls(Nodecl::NodeclBase n1, Nodecl::NodeclBase n2) const
    {
        nodecl_t n1_ = n1.get_internal_nodecl();
        nodecl_t n2_ = n2.get_internal_nodecl();

        if (nodecl_is_list(n1_) || nodecl_is_list(n2_))
        {
            std::cerr << "warning: method 'equal_nodecls' is implemented to compare nodecls containing trees with "
                      << " no lists inside. The method returns false but they can be the same tree" << std::endl;
            return false;
        }

        return equal_trees_rec(n1_, n2_);
    }

    bool ExtensibleSymbol::operator==(const ExtensibleSymbol& es) const
    {
        bool equals = equal_nodecls(_n, es._n);    
        return equals;
    }
    
    bool ExtensibleSymbol::operator<(const ExtensibleSymbol& es) const
    {
        // a < b -> ¬ (b > a)
        // a == b <=> ¬(a < b) /\ ¬(b < a)
        if (equal_nodecls(_n, es._n))
        {    
        std::cerr << "Nodecl " << c_cxx_codegen_to_str(_n.get_internal_nodecl()) 
                  << " == " << c_cxx_codegen_to_str(es._n.get_internal_nodecl()) << " are the same " << std::endl;                
            return false;
        }
        else
        {
                    std::cerr << "Nodecl " << c_cxx_codegen_to_str(_n.get_internal_nodecl()) 
                  << " == " << c_cxx_codegen_to_str(es._n.get_internal_nodecl()) << " are different " << std::endl;           
            return nodecl_get_ast(_n.get_internal_nodecl()) < nodecl_get_ast(es._n.get_internal_nodecl());
        }
    }
}