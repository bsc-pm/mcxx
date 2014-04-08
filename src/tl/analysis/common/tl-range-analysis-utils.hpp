

/*--------------------------------------------------------------------
 ( C) Copyright 2006-2012 Barcelona Supercomputing Center             **
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

#ifndef TL_RANGE_ANALYSIS_UTILS_HPP
#define TL_RANGE_ANALYSIS_UTILS_HPP

#include "tl-induction-variables-data.hpp"
#include "tl-nodecl-visitor.hpp"

namespace TL {
namespace Analysis {
namespace Utils {
    
    // ******************************************************************************************* //
    // ********************************** Intervals arithmetic *********************************** //
    
    bool is_empty_range( const Nodecl::Analysis::Range& r );
    Nodecl::Analysis::Range get_empty_range( const TL::Type& t );
    
    Nodecl::NodeclBase range_sub( const Nodecl::Analysis::Range& r1, const Nodecl::Analysis::Range& r2 );
    Nodecl::NodeclBase range_intersection( const Nodecl::Analysis::Range& r, const Nodecl::Analysis::Range& r2 );
    Nodecl::NodeclBase range_union( const Nodecl::Analysis::Range& r1, const Nodecl::Analysis::Range& r2 );

    // ******************************** END Intervals arithmetic ********************************* //
    // ******************************************************************************************* //
    
    
    
    // ******************************************************************************************* //
    // ******************************* Range Analysis Constraints ******************************** //
    
    /*! The possible constraints are:
     *  - Y = [lb, ub]
     *  - Y = expr | c
     *  - Y = phi(X1, X2)
     *  - Y = X ∩ [lb, ub]
     */
    struct Constraint {
        Nodecl::NodeclBase _original_var;     /*!< variable from the program to whom this constraint applies */
        TL::Symbol _constr_sym;               /*!< symbol associated to a given variable at this point of the program */
        Nodecl::NodeclBase _constraint;       /*!< actual constraint applying to the variable */
        
        Constraint( const Nodecl::NodeclBase orig_var, const TL::Symbol constr_sym, 
                    const Nodecl::NodeclBase& constraint )
            : _original_var( orig_var ), _constr_sym( constr_sym ), _constraint( constraint )
        {}
    };
    
    typedef std::map<Nodecl::NodeclBase, ObjectList<Constraint> > ConstraintMap;
    
    // ***************************** END Range Analysis Constraints ****************************** //
    // ******************************************************************************************* //
    
    
    
    // ******************************************************************************************* //
    // ************************* Range analysis methods and definitions ************************** //
    
    /*! A Range Expression has the following form:
     * RE := r | X | n x X | E + E
     *       |   |     |       |_ range addition
     *       |   |     |_________ scalar multiplication
     *       |   |_______________ range variable
     *       |___________________ range constant
     */
    union RangeValue_tag {
        Nodecl::NodeclBase* n;          // This represents a constant range
        InductionVariableData* iv;      // This represents a variable range
    };
    
    typedef std::pair<Nodecl::NodeclBase, ObjectList<RangeValue_tag> > RangeValuesMapEntry;
    typedef std::map<Nodecl::NodeclBase, ObjectList<RangeValue_tag> > RangeValuesMap;
    
    bool map_pair_compare( std::pair<Nodecl::NodeclBase, ObjectList<Utils::RangeValue_tag> > pair1, 
                           std::pair<Nodecl::NodeclBase, ObjectList<Utils::RangeValue_tag> > pair2 );
    
    // *********************** END Range analysis methods and definitions ************************ //
    // ******************************************************************************************* //

}
}
}

#endif          // TL_RANGE_ANALYSIS_UTILS_HPP