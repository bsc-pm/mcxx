/*--------------------------------------------------------------------
 ( C) Copyright 2006-2013 Barcelona Supercomputing Center             *
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
    // **************************** Class for Auto-Scoping purposes *************************** //

    AutoScopedVariables::AutoScopedVariables( )
            : _private_vars( ), _firstprivate_vars( ),
              _race_vars( ), _shared_vars( ), _undef_vars( )
    {}

    AutoScopedVariables::AutoScopedVariables( ext_sym_set private_vars, ext_sym_set firstprivate_vars,
                                              ext_sym_set race_vars, ext_sym_set shared_vars,
                                              ext_sym_set undef_vars )
            : _private_vars( private_vars ), _firstprivate_vars( firstprivate_vars ),
              _race_vars( race_vars ), _shared_vars( shared_vars ), _undef_vars( undef_vars )
    {}

    ext_sym_set AutoScopedVariables::get_private_vars( )
    {
        return _private_vars;
    }

    ext_sym_set AutoScopedVariables::get_firstprivate_vars( )
    {
        return _firstprivate_vars;
    }

    ext_sym_set AutoScopedVariables::get_race_vars( )
    {
        return _race_vars;
    }

    ext_sym_set AutoScopedVariables::get_shared_vars( )

    {
        return _shared_vars;
    }

    ext_sym_set AutoScopedVariables::get_undef_vars( )
    {
        return _undef_vars;
    }

    // ************************** END class for Auto-Scoping purposes ************************* //
    // **************************************************************************************** //



    // **************************************************************************************** //
    // *************** Methods for dealing with containers of Extended Symbols **************** //

    ObjectList<Nodecl::NodeclBase> get_nodecl_objectlist_from_ext_sym_list( ext_sym_set es )
    {
        ObjectList<Nodecl::NodeclBase> res;

        for( ext_sym_set::iterator it = es.begin( ); it != es.end( ); ++it )
        {
            res.insert( it->get_nodecl( ) );
        }

        return res;
    }

    ext_sym_set ext_sym_set_union( ext_sym_set c1, ext_sym_set c2 )
    {
        ext_sym_set result;
        std::set_union( c1.begin( ), c1.end( ), c2.begin( ), c2.end( ),
                        std::inserter( result, result.begin() ) );
        return result;
    }

    ext_sym_map ext_sym_map_union( ext_sym_map c1, ext_sym_map c2 )
    {
        ext_sym_map result = c1;

        for( ext_sym_map::iterator it = c2.begin(); it != c2.end(); ++it)
        {
            bool pair_already_in_map = false;
            std::pair<ext_sym_map::iterator, ext_sym_map::iterator> current_key_in_result = result.equal_range( it->first );
            for( ext_sym_map::iterator itt = current_key_in_result.first; itt != current_key_in_result.second; ++itt )
            {
                if( Nodecl::Utils::structurally_equal_nodecls( itt->second, it->second ) )
                {
                    pair_already_in_map = true;
                    break;
                }
            }
            if( !pair_already_in_map )
            {
                result.insert( std::pair<ExtendedSymbol, Nodecl::NodeclBase>( it->first, it->second ) );
            }
        }

        return result;
    }

    ext_sym_set ext_sym_set_difference( ext_sym_set c1, ext_sym_set c2 )
    {
        ext_sym_set result;
        std::set_difference( c1.begin( ), c1.end( ), c2.begin( ), c2.end( ),
                             std::inserter( result, result.begin() ), ExtendedSymbol_structural_less() );
        return result;
    }

    ext_sym_map ext_sym_map_minus_ext_sym_set( ext_sym_map c1, ext_sym_set c2 )
    {
        ext_sym_map result;
        for( ext_sym_map::iterator it = c1.begin(); it != c1.end(); ++it)
        {
            if( c2.find( it->first ) == c2.end( ) )
            {
                result.insert( std::pair<ExtendedSymbol, Nodecl::NodeclBase>( it->first, it->second ) );
            }
        }
        return result;
    }

    bool ext_sym_set_equivalence( ext_sym_set c1, ext_sym_set c2 )
    {
        bool result = false;
        if( c1.size( ) == c2.size( ) )
        {
            ext_sym_set intersection;
            std::set_intersection( c1.begin( ), c1.end( ), c2.begin( ), c2.end( ),
                                   std::inserter( intersection, intersection.begin() ) );
            if( intersection.size( ) == c1.size( ) )
                result = true;
        }
        return result;
    }

    bool ext_sym_map_equivalence( ext_sym_map c1, ext_sym_map c2 )
    {
        bool result = false;
        if( c1.size( ) == c2.size( ) )
        {
            result = true;
            ext_sym_map::iterator it1 = c1.begin( );
            ext_sym_map::iterator it2 = c2.begin( );
            for( ; it1 != c1.end( ); ++it1, ++it2 )
            {
                if( !Nodecl::Utils::structurally_equal_nodecls( it1->first.get_nodecl( ), it2->first.get_nodecl( ), /* skip Conversion nodes */ true ) ||
                    !Nodecl::Utils::structurally_equal_nodecls( it1->second, it2->second, /* skip Conversion nodes */ true ) )
                {
                    result = false;
                    break;
                }
            }
        }
        return result;
    }

    bool ext_sym_set_contains_sym( const ExtendedSymbol& s, const ext_sym_set& sym_set )
    {
        for( ext_sym_set::const_iterator it = sym_set.begin( ); it != sym_set.end( ); ++it )
        {
            if( it->get_symbol( ) == s.get_symbol( ) )
                return true;
        }

        return false;
    }

    bool ext_sym_set_contains_nodecl( const Nodecl::NodeclBase& nodecl, const ext_sym_set& sym_set )
    {
        for(ext_sym_set::iterator it = sym_set.begin( ); it != sym_set.end( ); ++it )
        {
            Nodecl::NodeclBase current = it->get_nodecl( );
            while( current.is<Nodecl::Conversion>( ) )
                current = current.as<Nodecl::Conversion>( ).get_nest( );

            if( Nodecl::Utils::structurally_equal_nodecls( nodecl, current ) )
                return true;
        }

        return false;
    }

    Nodecl::NodeclBase ext_sym_set_contains_enclosing_nodecl( const Nodecl::NodeclBase& n, const ext_sym_set& sym_set )
    {
        if( n.is<Nodecl::ArraySubscript>( ) )
        {
            Nodecl::ArraySubscript arr = n.as<Nodecl::ArraySubscript>( );
            if( ext_sym_set_contains_nodecl( n, sym_set ) )
                return n;
            else
                return ext_sym_set_contains_enclosing_nodecl( arr.get_subscripted( ), sym_set );
        }
        else if( n.is<Nodecl::ClassMemberAccess>( ) )
        {
            Nodecl::ClassMemberAccess memb_access = n.as<Nodecl::ClassMemberAccess>( );
            if( ext_sym_set_contains_nodecl( n, sym_set ) )
                return n;
            else
                return ext_sym_set_contains_enclosing_nodecl(  memb_access.get_lhs( ), sym_set );
        }
        else if( n.is<Nodecl::Conversion>( ) )
        {
            return ext_sym_set_contains_enclosing_nodecl( n.as<Nodecl::Conversion>( ).get_nest( ), sym_set );
        }
        else
        {
            if( ext_sym_set_contains_nodecl( n, sym_set ) )
                return n;
            else
                return Nodecl::NodeclBase::null( );
        }
    }

    Nodecl::List ext_sym_set_contains_enclosed_nodecl( const Nodecl::NodeclBase& n, const ext_sym_set& sym_set )
    {
        ext_sym_set fake_set;
        fake_set.insert( ExtendedSymbol( n ) );
        Nodecl::List result;
        for( ext_sym_set::iterator it = sym_set.begin( ); it != sym_set.end( ); ++it )
        {
            if( !ext_sym_set_contains_enclosing_nodecl( it->get_nodecl( ), fake_set ).is_null( ) )
                result.append(it->get_nodecl().shallow_copy());
        }
        return result;
    }

    void delete_enclosed_var_from_list( const ExtendedSymbol& ei, ext_sym_set& sym_set )
    {
        for( ext_sym_set::iterator it = sym_set.begin( ); it != sym_set.end( ); ++it )
        {
            ext_sym_set fake_set;
            fake_set.insert( *it );
            if( !ext_sym_set_contains_enclosed_nodecl( ei.get_nodecl( ), fake_set ).is_null( ) )
            {
                sym_set.erase( it );
                return;
            }
        }
    }

    // ************* END methods for dealing with containers of Extended Symbols ************** //
    // **************************************************************************************** //
}
}
}
