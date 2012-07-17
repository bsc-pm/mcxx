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
        ext_sym_set fake_list( 1, ei );
        for( ext_sym_set::iterator it = sym_set.begin( ); it != sym_set.end( ); ++it )
        {
            if( ext_sym_set_contains_englobing_nodecl( *it, fake_list ) )
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
        ext_sym_set result = set1;
        for( ext_sym_set::iterator it = set2.begin( ); it != set2.end( ); ++it )
        {
            if( set1.find( *it ).empty( ) )
                result.append( *it );
        }
        return result;
        
        //         std::vector<ExtendedSymbol> v_result(set1.size( ) + set2.size( ));
        //         std::vector<ExtendedSymbol>::iterator it;
        //         ext_sym_set result;
        //         it = set_union(set1.begin( ), set1.end( ), set2.begin( ), set2.end( ), v_result.begin( ));
        //         for(int i=0; i<int(it-v_result.begin( )); i++)
        //         {    
            //             result.insert(v_result.at(i));
        //         }
        //         return result;
        
    }
    
    ext_sym_set sets_difference( ext_sym_set set1, ext_sym_set set2 )
    {
        ext_sym_set result;
        for( ext_sym_set::iterator it = set1.begin( ); it != set1.end( ); ++it)
        {
            if( set2.find( *it ).empty( ) )
                result.insert( *it );
        }
        return result;
        
        //         std::vector<ExtendedSymbol> v_result(set1.size( ));
        //         std::vector<ExtendedSymbol>::iterator it;
        //         ext_sym_set result;
        //         it = set_difference(set1.begin( ), set1.end( ), set2.begin( ), set2.end( ), v_result.begin( ));
        //         for(int i=0; i<int(it-v_result.begin( )); i++)
        //         {    
            //             result.insert(v_result.at(i));
        //         }
        //         return result;
    }
    
    bool sets_equals( ext_sym_set set1, ext_sym_set set2 )
    {
        bool res = false;
        if( set1.size( ) == set2.size( ) )
        {
            res = true;
            for( ext_sym_set::iterator it = set1.begin( ); it != set1.end( ); ++it )
            {
                if( set1.find(*it).empty( ) )
                {    
                    res = false;
                    break;
                }
            }
            //             std::vector<ExtendedSymbol>::iterator it;
            //             std::vector<ExtendedSymbol> v_result(set1.size( ));
            //             it = set_intersection(set1.begin( ), set1.end( ), set2.begin( ), set2.end( ), v_result.begin( ));
            //             return (int(it-v_result.begin( )) == set1.size( ));
        }
        return res;
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