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

#ifndef TL_INDUCTION_VARIABLES_DATA_HPP
#define TL_INDUCTION_VARIABLES_DATA_HPP

#include "tl-extended-symbol.hpp"
#include "tl-nodecl.hpp"

namespace TL {
namespace Analysis {
namespace Utils {

    // ********************************************************************************************* //
    // ************************* Class representing and induction variable ************************* //

    enum InductionVarType {
        BASIC_IV,
        DERIVED_IV
    };

    class LIBTL_CLASS InductionVariableData {
    private:
        ExtendedSymbol _var;

        Nodecl::NodeclBase _lb;         /*!< Lower bound within a loop */
        Nodecl::NodeclBase _ub;         /*!< Upper bound within a loop (included) */
        Nodecl::NodeclBase _incr;       /*!< Stride within a loop */

        ObjectList<Nodecl::NodeclBase> _incrs;  /*!< List of modifications to an Induction Variable */
                                                // Example: loop { iv = iv + 100; iv = iv + 200 }
                                                // _incrs = { 100, 200 } 
        
        InductionVarType _type;         /*!< Type of iv: '1' = basic, '2' = derived */
        Nodecl::NodeclBase _family;     /*!< Family of the IV. For basic IVs, the family is the IV itself */

    public:

        // *** Constructors *** //
        //! Constructor to store variables that have upper and lower limits and stride but aren't Induction Variables
        InductionVariableData( ExtendedSymbol var );
        //! Induction Variable common constructor
        InductionVariableData( ExtendedSymbol var, InductionVarType type, Nodecl::NodeclBase family );


        // *** Getters and Setters *** //
        ExtendedSymbol get_variable() const;
        void set_variable( Nodecl::NodeclBase s );

        Nodecl::NodeclBase get_lb( ) const;
        void set_lb( Nodecl::NodeclBase lb );

        Nodecl::NodeclBase get_ub( ) const;
        void set_ub( Nodecl::NodeclBase ub );

        Nodecl::NodeclBase get_increment( ) const;
        void set_increment( Nodecl::NodeclBase incr );
        bool is_increment_one( ) const;

        ObjectList<Nodecl::NodeclBase> get_increment_list( ) const;
        void set_increment_list( ObjectList<Nodecl::NodeclBase> incr_list );
        
        std::string get_type_as_string( ) const;

        Nodecl::NodeclBase get_family( ) const;

        bool is_basic( );

        bool operator==( const InductionVariableData& rhs ) const;
        
        std::string print_iv_as_range() const;
    };

    // *********************** END class representing and induction variable *********************** //
    // ********************************************************************************************* //



    // ********************************************************************************************* //
    // ********************************* Induction Variables utils ********************************* //

    typedef std::multimap<int, InductionVariableData*> InductionVarsPerNode;

    void print_induction_vars( InductionVarsPerNode ivs );
    
    std::string prettyprint_induction_vars( ObjectList<InductionVariableData*> iv_list );

    bool induction_variable_list_contains_variable( ObjectList<InductionVariableData*> iv_list,
                                                    Nodecl::NodeclBase var );

    InductionVariableData* get_induction_variable_from_list( ObjectList<InductionVariableData*> ivs,
                                                             Nodecl::NodeclBase var );
    
    InductionVariableData* get_induction_variable_from_list( Utils::InductionVarsPerNode ivs,
                                                             Nodecl::NodeclBase var );

    // ******************************* END Induction Variables utils ******************************* //
    // ********************************************************************************************* //
}
}
}

#endif      // TL_INDUCTION_VARIABLES_DATA_HPP