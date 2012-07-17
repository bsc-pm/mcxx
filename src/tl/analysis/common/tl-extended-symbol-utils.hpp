/*--------------------------------------------------------------------
 ( C) Copyright 2006-2012 Bar*celona Supercomputing Center             **
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

#ifndef TL_EXTENSIBLE_SYMBOL_UTILS_HPP
#define TL_EXTENSIBLE_SYMBOL_UTILS_HPP

#include "tl-extended-symbol.hpp"

namespace TL {
namespace Analysis {
namespace Utils {
    
    // **************************************************************************************** //
    // ******************* Class representing the usage of Extended Symbols ******************* //
    
    enum UsageValue {
        upper_exposed,
        killed,
        undetermined_usage,     //! Can't be determined'
        undefined_usage         //! Not yet computed
    };
    
    class LIBTL_CLASS ExtendedSymbolUsage
    {
    private:
        ExtendedSymbol _es;
        UsageValue _usage;
        
    public:
        // ************* Constructor ************* //
        
        ExtendedSymbolUsage( ExtendedSymbol es, UsageValue usage );
        
        
        // ********* Getters and setters ********* //
        
        //! Returns the Extended Symbol
        ExtendedSymbol get_extensible_symbol( ) const;
        
        //! Returns de Nodecl contained in the Extended Symbol
        Nodecl::NodeclBase get_nodecl( ) const;
        
        //! Returns the usage associated with the Extended Symbol
        UsageValue get_usage( ) const;
        
        //! Sets the usage of the Extended Sybmbol
        void set_usage( UsageValue usage );
        
        
        // ************* Comparators ************* //
        
        bool operator==( const ExtendedSymbolUsage& esu ) const;
    };
    
    // ***************** END class representing the usage of Extended Symbols ***************** //
    // **************************************************************************************** //
    
    
    
    // **************************************************************************************** //
    // *************** Methods for dealing with containers of Extended Symbols **************** //
    
    // Extended Symbol list
    
    bool ext_sym_set_contains_sym(ExtendedSymbol s, ext_sym_set sym_set);
    bool ext_sym_set_contains_nodecl(Nodecl::NodeclBase nodecl, ext_sym_set sym_set);
    
    bool ext_sym_set_contains_englobing_nodecl(ExtendedSymbol ei, ext_sym_set sym_set);
    bool ext_sym_set_contains_englobed_nodecl(ExtendedSymbol ei, ext_sym_set sym_set);
    
    void delete_englobing_var_from_list(ExtendedSymbol ei, ext_sym_set sym_set);
    void delete_englobed_var_from_list(ExtendedSymbol ei, ext_sym_set sym_set);
    
    ext_sym_set sets_union(ext_sym_set set1, ext_sym_set set2);
    ext_sym_set sets_difference(ext_sym_set set1, ext_sym_set set2);
    bool sets_equals(ext_sym_set set1, ext_sym_set set2);
    
    // Extended Symbol Usage list
    
    bool usage_list_contains_nodecl( Nodecl::NodeclBase n, ObjectList<ExtendedSymbolUsage> list );
    bool usage_list_contains_sym( Symbol n, ObjectList<ExtendedSymbolUsage> list );
    
    bool usage_list_contains_englobing_nodecl( Nodecl::NodeclBase n, ObjectList<ExtendedSymbolUsage> list );
    bool usage_list_contains_englobed_nodecl( Nodecl::NodeclBase n, ObjectList<ExtendedSymbolUsage> list );
    
    ExtendedSymbolUsage get_var_in_list( Nodecl::NodeclBase n, ObjectList<ExtendedSymbolUsage> list );
    ExtendedSymbolUsage get_var_in_list( Symbol n, ObjectList<ExtendedSymbolUsage> list );
    
    void delete_englobing_var_in_usage_list( Nodecl::NodeclBase n, ObjectList<ExtendedSymbolUsage> list );
    void delete_englobed_var_in_usage_list( Nodecl::NodeclBase n, ObjectList<ExtendedSymbolUsage> list );
    
    ExtendedSymbolUsage get_var_in_list( Nodecl::NodeclBase n, ObjectList<ExtendedSymbolUsage> list );
    ExtendedSymbolUsage get_var_in_list( Symbol n, ObjectList<ExtendedSymbolUsage> list );
    
    // ************* END methods for dealing with containers of Extended Symbols ************** //
    // **************************************************************************************** //
}
}
}

#endif      // TL_EXTENSIBLE_SYMBOL_UTILS_HPP