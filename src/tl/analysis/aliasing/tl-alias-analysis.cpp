/*--------------------------------------------------------------------
 (C) Copyright 2006-2014 Barcelona Supercomputing Center             *
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


#include "tl-alias-analysis.hpp"

namespace TL {
namespace Analysis {
        
    tribool accesses_may_be_alias(const Nodecl::NodeclBase& n, const Nodecl::NodeclBase& m)
    {
        AliasAnalysis aa1, aa2;
        aa1.walk(n);
        aa2.walk(m);

        tribool n_alias = aa1.may_cause_aliasing();
        tribool m_alias = aa2.may_cause_aliasing();

        if (n_alias.is_false() && m_alias.is_false())
        {
            return tribool::False;
        }
        else
        {
            Symbol n_sym = aa1.get_base_symbol();
            Symbol m_sym = aa2.get_base_symbol();

            if (n_sym != m_sym)
                return tribool::Unknown;
            else
                return tribool::True;
        }
    }

    // ********************************************************************** //
    // ****************** Visitor to check AST nodes types ****************** //

    AliasAnalysis::AliasAnalysis()
        : _base_symbol(), _may_cause_aliasing(false)
    {}

    void AliasAnalysis::visit(const Nodecl::Symbol& n)
    {
        _base_symbol = n.get_symbol();
    }

    void AliasAnalysis::visit(const Nodecl::ArraySubscript& n)
    {
        Type t = n.get_type().no_ref();
        if (t.is_pointer() && !t.is_restrict())
        {
            _may_cause_aliasing = true;
        }
        else
        {
            walk(n.get_subscripts());
            walk(n.get_subscripted());
        }
    }

    void AliasAnalysis::visit(const Nodecl::ClassMemberAccess& n)
    {   // Change the order 
        walk(n.get_member());
        walk(n.get_lhs());
    }
    
    void AliasAnalysis::visit(const Nodecl::Dereference& n)
    {
        Type t = n.get_type().no_ref();
        if (t.is_pointer() && !t.is_restrict())
        {
            _may_cause_aliasing = true;
        }
        else
        {
            walk(n.get_rhs());
        }
    }

    Symbol AliasAnalysis::get_base_symbol() const
    {
        return _base_symbol;
    }

    tribool AliasAnalysis::may_cause_aliasing() const
    {
        return _may_cause_aliasing;
    }
    
    // **************** END visitor to check AST nodes types **************** //
    // ********************************************************************** //

}
}
