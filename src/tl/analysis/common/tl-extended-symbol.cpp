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

    ObjectList<Nodecl::NodeclBase> ExtendedSymbol::get_nodecls_base( const Nodecl::NodeclBase& n )
    {
        if (n.is<Nodecl::Symbol>() || n.is<Nodecl::PointerToMember>() || n.is<Nodecl::ObjectInit>() || n.is<Nodecl::FunctionCall>())
        {
            return ObjectList<Nodecl::NodeclBase>(1, n);
        }
        else if (n.is<Nodecl::IntegerLiteral>() || n.is<Nodecl::FloatingLiteral>() || n.is<Nodecl::ComplexLiteral>()
                || n.is<Nodecl::StringLiteral>() || n.is<Nodecl::BooleanLiteral>() || n.is<Nodecl::MaskLiteral>() )
        {
            return ObjectList<Nodecl::NodeclBase>();
        }
        else if (n.is<Nodecl::ClassMemberAccess>())
        {
            Nodecl::ClassMemberAccess aux = n.as<Nodecl::ClassMemberAccess>();
            return get_nodecls_base(aux.get_lhs());
        }
        else if (n.is<Nodecl::ArraySubscript>())
        {
            Nodecl::ArraySubscript aux = n.as<Nodecl::ArraySubscript>();
            return get_nodecls_base(aux.get_subscripted());
        }
        else if (n.is<Nodecl::Reference>())
        {
            Nodecl::Reference aux = n.as<Nodecl::Reference>();
            return get_nodecls_base(aux.get_rhs());
        }
        else if (n.is<Nodecl::Dereference>())
        {
            Nodecl::Dereference aux = n.as<Nodecl::Dereference>();
            return get_nodecls_base(aux.get_rhs());
        }
        else if (n.is<Nodecl::Conversion>())
        {
            Nodecl::Conversion aux = n.as<Nodecl::Conversion>();
            return get_nodecls_base(aux.get_nest());
        }
        else if (n.is<Nodecl::Cast>())
        {
            Nodecl::Cast aux = n.as<Nodecl::Cast>();
            return get_nodecls_base(aux.get_rhs());
        }
        /*!
        * We can have (pre- post-) in- de-crements and other arithmetic operations
        * Example:
        * T *curr_high = ...;
        * *curr_high-- = l;
        * "*curr_high--" is a _KILLED_VAR
        */
        else if (n.is<Nodecl::Predecrement>())
        {
            Nodecl::Predecrement aux = n.as<Nodecl::Predecrement>();
            return get_nodecls_base(aux.get_rhs());
        }
        else if (n.is<Nodecl::Postdecrement>())
        {
            Nodecl::Postdecrement aux = n.as<Nodecl::Postdecrement>();
            return get_nodecls_base(aux.get_rhs());
        }
        else if (n.is<Nodecl::Preincrement>())
        {
            Nodecl::Preincrement aux = n.as<Nodecl::Preincrement>();
            return get_nodecls_base(aux.get_rhs());
        }
        else if (n.is<Nodecl::Postincrement>())
        {
            Nodecl::Postincrement aux = n.as<Nodecl::Postincrement>();
            return get_nodecls_base(aux.get_rhs());
        }
        else if (n.is<Nodecl::Add>())
        {
            Nodecl::Add aux = n.as<Nodecl::Add>();
            ObjectList<Nodecl::NodeclBase> rhs = get_nodecls_base(aux.get_rhs());
            ObjectList<Nodecl::NodeclBase> lhs = get_nodecls_base(aux.get_lhs());
            return rhs.append(lhs);
        }
        else
        {
            internal_error("Unexpected type of nodecl '%s' contained in an ExtendedSymbol '%s'",
                        ast_print_node_type(n.get_kind()), n.prettyprint().c_str());
        }
    }

    ObjectList<Symbol> ExtendedSymbol::get_symbols() const
    {
        return Nodecl::Utils::get_all_symbols( _n );
    }

    Nodecl::NodeclBase ExtendedSymbol::get_nodecl_base( const Nodecl::NodeclBase& n )
    {
        Nodecl::NodeclBase nodecl;
        if( n.is<Nodecl::Symbol>( ) || n.is<Nodecl::PointerToMember>( ) || n.is<Nodecl::ObjectInit>( ) )
        {
            nodecl = n;
        }
        else if( Nodecl::Utils::nodecl_is_literal( n ) )
        {
            nodecl = Nodecl::NodeclBase::null();
        }
        else if (n.is<Nodecl::ClassMemberAccess>())
        {
            nodecl = get_nodecl_base( n.as<Nodecl::ClassMemberAccess>( ).get_lhs( ) );
        }
        else if (n.is<Nodecl::ArraySubscript>())
        {
            nodecl = get_nodecl_base( n.as<Nodecl::ArraySubscript>( ).get_subscripted( ) );
        }
        else if (n.is<Nodecl::Reference>())
        {
            nodecl = get_nodecl_base( n.as<Nodecl::Reference>( ).get_rhs( ) );
        }
        else if (n.is<Nodecl::Dereference>())
        {
            nodecl = get_nodecl_base( n.as<Nodecl::Dereference>( ).get_rhs( ) );
        }
        else if( n.is<Nodecl::Conversion>( ) )
        {
            nodecl = get_nodecl_base( n.as<Nodecl::Conversion>( ).get_nest( ) );
        }
        else if( n.is<Nodecl::Cast>( ) )
        {
            nodecl = get_nodecl_base( n.as<Nodecl::Cast>( ).get_rhs( ) );
        }
        else if( n.is<Nodecl::Postdecrement>( ) )
        {
            nodecl = get_nodecl_base( n.as<Nodecl::Postdecrement>( ).get_rhs( ) );
        }
        else if( n.is<Nodecl::Postincrement>( ) )
        {
            nodecl = get_nodecl_base( n.as<Nodecl::Postincrement>( ).get_rhs( ) );
        }
        else if (n.is<Nodecl::Predecrement>())
        {
            nodecl = get_nodecl_base( n.as<Nodecl::Predecrement>( ).get_rhs( ) );
        }
        else if( n.is<Nodecl::Preincrement>( ) )
        {
            nodecl = get_nodecl_base( n.as<Nodecl::Preincrement>( ).get_rhs( ) );
        }
        else
        {
            nodecl = Nodecl::NodeclBase::null( );
        }

        return nodecl;
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
        bool equals = Nodecl::Utils::equal_nodecls( _n, es._n, /* skip conversion nodes */ true );
        return equals;
    }

    /*! Applied rules:
     *      a < b -> ¬ (b > a)
     *      a == b <=> ¬(a < b) /\ ¬(b < a)
     */
    bool ExtendedSymbol::operator<( const ExtendedSymbol& es ) const
    {
        bool result;
        result = (Nodecl::Utils::cmp_nodecls(_n, es._n, /* skip conversion nodes */ true) == -1);
        return result;
    }
}
}
}
