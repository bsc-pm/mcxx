/*--------------------------------------------------------------------
 ( C) Copyright 2006-2012 Barcelona Supercomputing Center             *
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

#include "tl-extended-symbol-utils.hpp"
#include "tl-nodecl-utils.hpp"

#include <algorithm>
#include <iterator>

namespace TL {
namespace Analysis {
namespace Utils {

    // **************************************************************************************** //
    // *************** Methods for dealing with containers of Extended Symbols **************** //

    bool ext_sym_set_contains_sym( ExtendedSymbol s, ext_sym_set sym_set )
    {
        for( ext_sym_set::iterator it = sym_set.begin( ); it != sym_set.end( ); ++it )
        {
            if( it->get_symbol( ) == s.get_symbol( ) )
                return true;
        }

        return false;
    }

    bool ext_sym_set_contains_nodecl( Nodecl::NodeclBase nodecl, ext_sym_set sym_set )
    {
        for(ext_sym_set::iterator it = sym_set.begin( ); it != sym_set.end( ); ++it )
        {
            Nodecl::NodeclBase current = it->get_nodecl( );
            if( current.is<Nodecl::Conversion>( ) )
            {
                Nodecl::Conversion aux = current.as<Nodecl::Conversion>( );
                current = aux.get_nest( );
            }

            if( Nodecl::Utils::equal_nodecls( nodecl, current ) )
            {
                return true;
            }
        }

        return false;
    }

    bool ext_sym_set_contains_englobing_nodecl( ExtendedSymbol ei, ext_sym_set sym_set )
    {
        Nodecl::NodeclBase nodecl = ei.get_nodecl( );
        if( nodecl.is<Nodecl::ArraySubscript>( ) )
        {
            Nodecl::ArraySubscript arr = nodecl.as<Nodecl::ArraySubscript>( );
            return ( ext_sym_set_contains_nodecl(nodecl, sym_set )
                    || ext_sym_set_contains_englobing_nodecl( arr.get_subscripted( ), sym_set ) );
        }
        else if( nodecl.is<Nodecl::ClassMemberAccess>( ) )
        {
            Nodecl::ClassMemberAccess memb_access = nodecl.as<Nodecl::ClassMemberAccess>( );
            return ( ext_sym_set_contains_nodecl( nodecl, sym_set )
            || ext_sym_set_contains_englobing_nodecl( memb_access.get_lhs( ), sym_set) );
        }
        else if( nodecl.is<Nodecl::Conversion>( ) )
        {
            Nodecl::Conversion conv = nodecl.as<Nodecl::Conversion>( );
            return ext_sym_set_contains_englobing_nodecl( conv.get_nest( ), sym_set );
        }
        else
        {
            return ext_sym_set_contains_nodecl( nodecl, sym_set );
        }
    }

    bool ext_sym_set_contains_englobed_nodecl( ExtendedSymbol ei, ext_sym_set sym_set )
    {
        ext_sym_set fake_set;
        fake_set.insert( ei );
        for( ext_sym_set::iterator it = sym_set.begin( ); it != sym_set.end( ); ++it )
        {
            if( ext_sym_set_contains_englobing_nodecl( *it, fake_set ) )
                return true;
        }
        return false;
    }

    void delete_englobing_var_from_list( ExtendedSymbol ei, ext_sym_set sym_set )
    {
        for( ext_sym_set::iterator it = sym_set.begin( ); it != sym_set.end( ); ++it)
        {
            if( ext_sym_set_contains_englobing_nodecl( *it, sym_set ) )
            {
                sym_set.erase( it );
                return;
            }
        }
    }

    void delete_englobed_var_from_list( ExtendedSymbol ei, ext_sym_set sym_set )
    {
        for( ext_sym_set::iterator it = sym_set.begin( ); it != sym_set.end( ); ++it )
        {
            if( ext_sym_set_contains_englobed_nodecl( *it, sym_set ) )
            {
                sym_set.erase( it );
                return;
            }
        }
    }

    ext_sym_set sets_union( ext_sym_set set1, ext_sym_set set2 )
    {
        ext_sym_set result;
        std::set_union( set1.begin( ), set1.end( ), set2.begin( ), set2.end( ),
                        std::inserter( result, result.begin() ) );
        return result;
    }

    ext_sym_set sets_difference( ext_sym_set set1, ext_sym_set set2 )
    {
        ext_sym_set result;
        std::set_difference( set1.begin( ), set1.end( ), set2.begin( ), set2.end( ),
                             std::inserter( result, result.begin() ) );
        return result;
    }

    ext_sym_set sets_difference( ext_sym_set set1, ExtendedSymbol es )
    {
        ext_sym_set result, set2;
        set2.insert( es );
        return sets_difference( set1, set2 );
    }

    bool sets_equals( ext_sym_set set1, ext_sym_set set2 )
    {
        bool result = false;
        if( set1.size( ) == set2.size( ) )
        {
            ext_sym_set intersection;
            std::set_intersection( set1.begin( ), set1.end( ), set2.begin( ), set2.end( ),
                                   std::inserter( intersection, intersection.begin() ) );
            if( intersection.size( ) == set1.size( ) )
                result = true;
        }
        return result;
    }

    bool usage_list_contains_nodecl( Nodecl::NodeclBase n, ObjectList<ExtendedSymbolUsage> list )
    {
        for( ObjectList<ExtendedSymbolUsage>::iterator it = list.begin( ); it != list.end( ); ++it )
        {
            if( Nodecl::Utils::equal_nodecls( it->get_nodecl( ), n ) )
            {
                return true;
            }
        }
        return false;
    }

    bool usage_list_contains_sym( Symbol n, ObjectList<ExtendedSymbolUsage> list )
    {
        for( ObjectList<ExtendedSymbolUsage>::iterator it = list.begin( ); it != list.end( ); ++it )
        {
            Nodecl::NodeclBase current_n = it->get_nodecl( );
            if( current_n.is<Nodecl::Symbol>( ) )
            {
                if( current_n.get_symbol( ) == n )
                {
                    return true;
                }
            }
        }
        return false;
    }

    bool usage_list_contains_englobing_nodecl( Nodecl::NodeclBase n, ObjectList<ExtendedSymbolUsage> list )
    {
        for( ObjectList<ExtendedSymbolUsage>::iterator it = list.begin( ); it != list.end( ); ++it)
        {
            Nodecl::NodeclBase nodecl = it->get_nodecl( );

            if( Nodecl::Utils::equal_nodecls( nodecl, n ) )
            {
                return true;
            }

            if( n.is<Nodecl::ArraySubscript>( ) )
            {
                Nodecl::ArraySubscript arr = n.as<Nodecl::ArraySubscript>( );
                return ( usage_list_contains_englobing_nodecl( arr.get_subscripted( ), list) );
            }
            else if( n.is<Nodecl::ClassMemberAccess>( ) )
            {
                Nodecl::ClassMemberAccess memb_access = n.as<Nodecl::ClassMemberAccess>( );
                return ( usage_list_contains_englobing_nodecl( memb_access.get_member( ), list ) );
            }
            else if( n.is<Nodecl::Conversion>( ) )
            {
                Nodecl::Conversion conv = n.as<Nodecl::Conversion>( );
                return usage_list_contains_englobing_nodecl( conv.get_nest( ), list );
            }
        }
        return false;
    }

    bool usage_list_contains_englobed_nodecl( Nodecl::NodeclBase n, ObjectList<ExtendedSymbolUsage> list )
    {
        ExtendedSymbolUsage fake_usage( n, undefined_usage );
        ObjectList<ExtendedSymbolUsage> fake_list( 1, fake_usage );
        for(ObjectList<ExtendedSymbolUsage>::iterator it = list.begin( ); it != list.end( ); ++it )
        {
            if( usage_list_contains_englobing_nodecl( it->get_nodecl( ), fake_list ) )
                return true;
        }
        return false;
    }

    void delete_englobing_var_in_usage_list( Nodecl::NodeclBase n, ObjectList<ExtendedSymbolUsage> list )
    {
        for( ObjectList<ExtendedSymbolUsage>::iterator it = list.begin( ); it != list.end( ); ++it )
        {
            Nodecl::NodeclBase nodecl = it->get_nodecl( );

            if( Nodecl::Utils::equal_nodecls( nodecl, n ) )
            {
                list.erase( it );
                break;
            }

            if( n.is<Nodecl::ArraySubscript>( ) )
            {
                Nodecl::ArraySubscript arr = n.as<Nodecl::ArraySubscript>( );
                if( usage_list_contains_englobing_nodecl( arr.get_subscripted( ), list ) )
                {
                    list.erase( it );
                    break;
                }
            }
            else if( n.is<Nodecl::ClassMemberAccess>( ) )
            {
                Nodecl::ClassMemberAccess memb_access = n.as<Nodecl::ClassMemberAccess>( );
                if( usage_list_contains_englobing_nodecl( memb_access.get_member( ), list ) )
                {
                    list.erase( it );
                    break;
                }
            }
            else if( n.is<Nodecl::Conversion>( ) )
            {
                Nodecl::Conversion conv = n.as<Nodecl::Conversion>( );
                if( usage_list_contains_englobing_nodecl( conv.get_nest( ), list ) )
                {
                    list.erase( it );
                    break;
                }
            }
        }
    }

    void delete_englobed_var_in_usage_list( Nodecl::NodeclBase n, ObjectList<ExtendedSymbolUsage> list )
    {
        ExtendedSymbolUsage fake_usage( n, undefined_usage );
        ObjectList<ExtendedSymbolUsage> fake_list( 1, fake_usage );
        for( ObjectList<ExtendedSymbolUsage>::iterator it = list.begin( ); it != list.end( ); ++it )
        {
            if( usage_list_contains_englobing_nodecl( it->get_nodecl( ), fake_list ) )
            {
                list.erase( it );
                break;
            }
        }
    }

    ExtendedSymbolUsage get_var_in_list( Nodecl::NodeclBase n, ObjectList<ExtendedSymbolUsage> list )
    {
        for( ObjectList<ExtendedSymbolUsage>::iterator it = list.begin( ); it != list.end( ); ++it )
        {
            if ( Nodecl::Utils::equal_nodecls( it->get_nodecl(), n ) )
            {
                return *it;
            }
        }

        internal_error( "No nodecl '%s' founded in usage list", n.prettyprint( ).c_str( ) );
    }

    ExtendedSymbolUsage get_var_in_list( Symbol n, ObjectList<ExtendedSymbolUsage> list )
    {
        for( ObjectList<ExtendedSymbolUsage>::iterator it = list.begin( ); it != list.end( ); ++it )
        {
            Nodecl::NodeclBase current_n = it->get_nodecl( );
            if( current_n.is<Nodecl::Symbol>( ) )
            {
                if ( current_n.get_symbol( ) == n )
                {
                    return *it;
                }
            }
        }

        internal_error( "No symbol '%s' founded in usage list", n.get_name( ).c_str( ) );
    }

    // ************* END methods for dealing with containers of Extended Symbols ************** //
    // **************************************************************************************** //



    // **************************************************************************************** //
    // ******************* Class representing the usage of Extended Symbols ******************* //

    ExtendedSymbolUsage::ExtendedSymbolUsage( ExtendedSymbol es, UsageValue usage )
        : _es( es ), _usage( usage )
    {}

    ExtendedSymbol ExtendedSymbolUsage::get_extensible_symbol( ) const
    {
        return _es;
    }

    Nodecl::NodeclBase ExtendedSymbolUsage::get_nodecl( ) const
    {
        return _es.get_nodecl();
    }

    UsageValue ExtendedSymbolUsage::get_usage( ) const
    {
        return _usage;
    }

    void ExtendedSymbolUsage::set_usage( UsageValue usage )
    {
        _usage = usage;
    }


    bool ExtendedSymbolUsage::operator==( const ExtendedSymbolUsage& esu ) const
    {
        return ( Nodecl::Utils::equal_nodecls( this->get_nodecl( ), esu.get_nodecl( ) ) );
    }

    // ***************** END class representing the usage of Extended Symbols ***************** //
    // **************************************************************************************** //
}
}
}
