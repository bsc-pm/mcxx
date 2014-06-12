/*--------------------------------------------------------------------
  (C) Copyright 2006-2013 Barcelona Supercomputing Center
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

#include <algorithm>

#include "cxx-codegen.h"
#include "cxx-process.h"
#include "tl-extended-symbol.hpp"
#include "tl-nodecl-utils.hpp"

namespace TL {
namespace Analysis {
namespace Utils {

    ExtendedSymbol::ExtendedSymbol( )
        : _n( Nodecl::NodeclBase::null( ) )
    {}

    ExtendedSymbol::ExtendedSymbol( const Nodecl::NodeclBase& n )
    {
        _n = n;
    }

    ObjectList<Symbol> ExtendedSymbol::get_symbols() const
    {
        return Nodecl::Utils::get_all_symbols( _n );
    }

    Symbol ExtendedSymbol::get_symbol( ) const
    {
        Symbol s;

        Nodecl::NodeclBase base_nodecl = get_nodecl_base( _n );
        if ( !base_nodecl.is_null( ) )
        {
            s = base_nodecl.get_symbol( );
        }

        return s;
    }

    Nodecl::NodeclBase ExtendedSymbol::get_nodecl( ) const
    {
        return _n;
    }

    bool ExtendedSymbol::is_simple_symbol( ) const
    {
        return ( _n.is<Nodecl::Symbol>( ) );
    }

    bool ExtendedSymbol::is_array( ) const
    {
        if( _n.is<Nodecl::ArraySubscript>( ) )
            return true;
        else if (_n.is<Nodecl::ClassMemberAccess>())
        {
            Nodecl::ClassMemberAccess member_acc = _n.as<Nodecl::ClassMemberAccess>();
            ExtendedSymbol class_(member_acc.get_lhs());
            ExtendedSymbol memb_(member_acc.get_member());
            return ( class_.is_array() || memb_.is_array() );
        }
        else if (_n.is<Nodecl::Reference>())
        {
            Nodecl::Reference ref = _n.as<Nodecl::Reference>();
            ExtendedSymbol ref_(ref.get_rhs());
            return ref_.is_array();
        }
        else if (_n.is<Nodecl::Dereference>())
        {
            Nodecl::Dereference ref = _n.as<Nodecl::Dereference>();
            ExtendedSymbol ref_(ref.get_rhs());
            return ref_.is_array();
        }
        else
            return false;
    }

    ExtendedSymbol& ExtendedSymbol::operator=( const ExtendedSymbol &es )
    {
        _n = es._n;
        return *this;
    }

    bool ExtendedSymbol::operator==( const ExtendedSymbol& es ) const
    {
        return Nodecl::Utils::structurally_equal_nodecls( _n, es._n, /*skip_conversion_nodes*/ true );
    }

    /*! Applied rules:
     *      a < b -> ¬ (b > a)
     *      a == b <=> ¬(a < b) /\ ¬(b < a)
     */
    bool ExtendedSymbol::operator<( const ExtendedSymbol& es ) const
    {
        return (Nodecl::Utils::structurally_cmp_nodecls(_n, es._n, /*skip_conversion_nodes*/ true) == -1);
    }
    
    bool ExtendedSymbol_structural_less::operator() (const ExtendedSymbol& e1, const ExtendedSymbol& e2) const
    {
        return (Nodecl::Utils::structurally_cmp_nodecls(e1.get_nodecl(), e2.get_nodecl(), /*skip_conversion_nodes*/true) < 0);
    }
}
}
}
