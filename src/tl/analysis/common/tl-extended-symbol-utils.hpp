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
    
    bool ext_sym_set_contains_sym( const ExtendedSymbol& s, const ext_sym_set& sym_set );
    bool ext_sym_set_contains_nodecl( const Nodecl::NodeclBase& nodecl, const ext_sym_set& sym_set );
    
    /*!This method returns a null nodecl if the list #sym_set does not contain a Nodecl equal to #n or containing #n
     * The cases where a nodecl contains a nodecl are the following:
     * - v contains v (this is the case when the two nodecls are equal)
     * - A contains A[i] (comparison between array ranges to be done)
     * - S contains S.r
     * Otherwise, it returns the nodecl equal or containing #n in #sym_set
     */
    Nodecl::NodeclBase ext_sym_set_contains_enclosing_nodecl( const Nodecl::NodeclBase& n, const ext_sym_set& sym_set );
    /*!This method returns a null nodecl if the list #sym_set does not contain a Nodecl equal to #n or contained in #n
     * - v is contained in v (this is the case when the two nodecls are equal)
     * - A[i] is contained in A (comparison between array ranges to be done)
     * - S.r is contained in S
     * Otherwise, it returns the nodecl equal or contained in #n in #sym_set
     */
    Nodecl::NodeclBase ext_sym_set_contains_enclosed_nodecl( const Nodecl::NodeclBase& n, const ext_sym_set& sym_set );
    
    void delete_enclosed_var_from_list( const ExtendedSymbol& ei, ext_sym_set& sym_set );

    // ******************** END methods for dealing with Extended Symbols ********************* //
    // **************************************************************************************** //
}
}
}

#endif      // TL_EXTENDED_SYMBOL_UTILS_HPP