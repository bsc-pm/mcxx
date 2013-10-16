/*--------------------------------------------------------------------
 ( C) Copyright 2006-2013 Bar*celona Supercomputing Center             **
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

#ifndef TL_EXTENDED_SYMBOL_UTILS_HPP
#define TL_EXTENDED_SYMBOL_UTILS_HPP

#include "tl-extended-symbol.hpp"

#include <iostream>
#include <typeinfo>
using namespace std;

namespace TL {
namespace Analysis {
namespace Utils {

    // **************************************************************************************** //
    // ******************* Class representing the usage of Extended Symbols ******************* //

    struct UseDefVariant
    {
        // Macros defining the analysis to be computed
        enum Usage_tag
        {
            NONE        = 1u << 0,
            USED        = 1u << 1,
            DEFINED     = 1u << 2,
            UNDEFINED   = 1u << 3
        } _usage_variants;
        
        UseDefVariant( Usage_tag a );
        UseDefVariant( int a );
        UseDefVariant operator|( UseDefVariant a );
    };

    class LIBTL_CLASS ExtendedSymbolUsage
    {
    private:
        ExtendedSymbol _es;
        UseDefVariant _usage;

    public:
        // ************* Constructor ************* //

        ExtendedSymbolUsage( ExtendedSymbol es, UseDefVariant usage );


        // ********* Getters and setters ********* //

        //! Returns the Extended Symbol
        ExtendedSymbol get_extended_symbol( ) const;

        //! Returns de Nodecl contained in the Extended Symbol
        Nodecl::NodeclBase get_nodecl( ) const;

        //! Returns the usage associated with the Extended Symbol
        UseDefVariant get_usage( ) const;

        //! Sets the usage of the Extended Sybmbol
        void set_usage( UseDefVariant usage );


        // ************* Comparators ************* //

        bool operator==( const ExtendedSymbolUsage& esu ) const;
    };

    // ***************** END class representing the usage of Extended Symbols ***************** //
    // **************************************************************************************** //



    // **************************************************************************************** //
    // **************************** Class for Auto-Scoping purposes *************************** //

    class LIBTL_CLASS AutoScopedVariables
    {
    private:
        ext_sym_set _private_vars;
        ext_sym_set _firstprivate_vars;
        ext_sym_set _race_vars;
        ext_sym_set _shared_vars;
        ext_sym_set _undef_vars;

    public:
        // ************* Constructor ************* //

        AutoScopedVariables( );
        AutoScopedVariables( ext_sym_set private_vars, ext_sym_set firstprivate_vars,
                             ext_sym_set race_vars, ext_sym_set shared_vars, ext_sym_set undef_vars );

        // ********* Getters and setters ********* //

        ext_sym_set get_private_vars( );
        ext_sym_set get_firstprivate_vars( );
        ext_sym_set get_race_vars( );
        ext_sym_set get_shared_vars( );
        ext_sym_set get_undef_vars( );
    };

    // ************************** END class for Auto-Scoping purposes ************************* //
    // **************************************************************************************** //



    // **************************************************************************************** //
    // ********************** Methods for dealing with Extended Symbols *********************** //

    // ********** ExtendedSymbol comparisons ********* //

    bool extended_symbol_contains_extended_symbol( ExtendedSymbol container, ExtendedSymbol contained );

    // ********** Containers algorithms ********* //

    ObjectList<Nodecl::NodeclBase> get_nodecl_objectlist_from_ext_sym_list( ext_sym_set es );

    ext_sym_map ext_sym_map_union( ext_sym_map c1, ext_sym_map c2 );
    ext_sym_set ext_sym_set_union( ext_sym_set c1, ext_sym_set c2 );

    ext_sym_set ext_sym_set_difference( ext_sym_set c1, ext_sym_set c2 );
    ext_sym_map ext_sym_map_minus_ext_sym_set( ext_sym_map c1, ext_sym_set c2 );

    bool ext_sym_set_equivalence( ext_sym_set c1, ext_sym_set c2 );
    bool ext_sym_map_equivalence( ext_sym_map c1, ext_sym_map c2 );

    bool ext_sym_set_contains_sym(ExtendedSymbol s, ext_sym_set sym_set);
    bool ext_sym_set_contains_nodecl(Nodecl::NodeclBase nodecl, ext_sym_set sym_set);

    bool ext_sym_set_contains_englobing_nodecl(ExtendedSymbol ei, ext_sym_set sym_set);
    bool ext_sym_set_contains_englobed_nodecl(ExtendedSymbol ei, ext_sym_set sym_set);

    void delete_englobing_var_from_list(ExtendedSymbol ei, ext_sym_set sym_set);
    void delete_englobed_var_from_list(ExtendedSymbol ei, ext_sym_set sym_set);


    // ******* Extended Symbol Usage list ******* //

    bool usage_list_contains_extsym( ExtendedSymbol ei, ObjectList<ExtendedSymbolUsage> list );
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

    // ******************** END methods for dealing with Extended Symbols ********************* //
    // **************************************************************************************** //
}
}
}

#endif      // TL_EXTENDED_SYMBOL_UTILS_HPP