

/*--------------------------------------------------------------------
 (C) Copyright 2006-2014 Barcelona Supercomputing Center             **
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

#define RANGES_DEBUG

namespace Utils {
    
    // ******************************************************************************************* //
    // *********************************** Ranges arithmetic ************************************* //
    
    struct CycleDirection
    {
        // Macros defining the analysis to be computed
        enum CycleDirection_tag
        {
            NONE        = 1u << 1,
            POSITIVE    = 1u << 2,
            NEGATIVE    = 1u << 3,
        } _cycle_direction;
        
        CycleDirection(CycleDirection_tag a)
            : _cycle_direction(a)
        {}
        
        CycleDirection(int a)
            : _cycle_direction(CycleDirection_tag(a))
        {}
        
        CycleDirection operator|(CycleDirection a)
        {
            return CycleDirection(int(this->_cycle_direction) | int(a._cycle_direction));
        }
        
        std::string get_direction_as_str()
        {
            std::string result;
            if(_cycle_direction & POSITIVE)
                result = "Positive";
            else if(_cycle_direction & NEGATIVE)
                result = "Negative";
            else
                result = "None";
            return result;
        }
    };

    bool nodecl_is_Z_range(const NBase& n);

    NBase range_sub(const NBase& r1, const NBase& r2);

    NBase range_addition(const NBase& r1, const NBase& r2);
    NBase range_subtraction(const NBase& r1, const NBase& r2);
    NBase range_multiplication(const NBase& r1, const NBase& r2);
    NBase range_division(const NBase& r1, const NBase& r2);
    NBase range_intersection(const NBase& r, const NBase& r2, CycleDirection dir);
    NBase range_union(const NBase& r1, const NBase& r2);
    Nodecl::Range range_value_add(const Nodecl::Range& r, const NBase& v);
    Nodecl::Range range_value_sub(const Nodecl::Range& r, const NBase& v);
    Nodecl::Range range_value_mul(const Nodecl::Range& r, const NBase& v);
    Nodecl::Range range_value_div(const Nodecl::Range& r, const NBase& v);
    
    // ********************************* END Ranges arithmetic *********************************** //
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
        TL::Symbol _constr_sym;     /*!< symbol associated to a given variable at this point of the program */
        NBase _constraint;          /*!< actual constraint applying to the variable */

        // *** Constructors *** //
        Constraint();
        Constraint(const TL::Symbol& constr_sym, const NBase& constraint);

        // *** Getters and Setters *** //
        TL::Symbol get_symbol() const;
        void set_symbol(const TL::Symbol& s);
        NBase get_constraint() const;

        // *** Comparators *** //
        bool operator!=(const Constraint& c) const;
        bool operator==(const Constraint& c) const;
    };

    typedef std::map<NBase, Constraint, Nodecl::Utils::Nodecl_structural_less> VarToConstraintMap;
    typedef std::map<NBase, NBase, Nodecl::Utils::Nodecl_structural_less> RangeValuesMap;
    
    // ***************************** END Range Analysis Constraints ****************************** //
    // ******************************************************************************************* //
    
}
}
}

#endif          // TL_RANGE_ANALYSIS_UTILS_HPP
