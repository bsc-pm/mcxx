

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
    
    enum RangeType {Regular, Empty};

    class Range {
    private:
        Nodecl::NodeclBase _lb;     // The lower bound of the range.
        Nodecl::NodeclBase _ub;     // The upper bound of the range.
        RangeType _type;
        
    public:
        
        // *** Constructors and destructor *** //
        Range( );
        Range( Nodecl::NodeclBase lb, Nodecl::NodeclBase ub, RangeType type = Regular );
        ~Range( ) {};

        // *** Getters and setters *** //
        Nodecl::NodeclBase get_lower( ) const;
        Nodecl::NodeclBase get_upper( ) const;
        void set_lower( const Nodecl::NodeclBase& new_lb );
        void set_upper( const Nodecl::NodeclBase& new_ub );
        bool is_empty( ) const;
        
        // *** Interval operations *** //
        Range range_add( const Range& r );
        Range range_sub( const Range& r );
        Range range_mul( const Range& r );
        Range range_div( const Range& r );
        Range range_intersection( const Range& r ) const;
        Range range_union( const Range& r ) const;
        bool operator==( const Range& r ) const;
        bool operator!=( const Range& r ) const;
    };

    // ******************************** END Intervals arithmetic ********************************* //
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

    struct Constraint {
        // TODO
    };
    
    typedef std::map<Nodecl::NodeclBase, ObjectList<Constraint> > ConstraintMap;
    
    // *********************** END Range analysis methods and definitions ************************ //
    // ******************************************************************************************* //

}
}
}

#endif          // TL_RANGE_ANALYSIS_UTILS_HPP