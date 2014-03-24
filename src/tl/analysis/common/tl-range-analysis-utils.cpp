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

#include <limits>

#include "cxx-cexpr.h"

#include "tl-nodecl-calc.hpp"
#include "tl-range-analysis-utils.hpp"

namespace TL {
namespace Analysis {
namespace Utils {

    Optimizations::Calculator calc;
    
    /*! When the lower and upper bounds of both ranges is not a constant value,
     * we cannot tell whether the range is empty or not.
     * Being conservative, we answer false when we don't know the accurate answer
     */
    bool is_empty_range( const Nodecl::Analysis::Range& c )
    {
        return c.get_lower().is<Nodecl::Analysis::EmptyRange>();
    }
    
    Nodecl::Analysis::Range get_empty_range( const TL::Type& t )
    {
        return Nodecl::Analysis::Range::make( Nodecl::Analysis::EmptyRange::make(), Nodecl::NodeclBase::null(), t );
    }
    
    // A[al, au] − B[bl, bu] =  ∅, A and B completely overlap or B contains A
    //                          A, A and B do not overlap
    //                          [al, bl] U [bu, au], A contains B
    //                          [al, bl], al < bl
    //                          [au, bu], al > bl
    // When the operation cannot be computed, we return the nodecl representing the operation
    Nodecl::NodeclBase range_sub( const Nodecl::Analysis::Range& r1, const Nodecl::Analysis::Range& r2 )
    {
        if( is_empty_range( r1 ) || is_empty_range( r2 ) )
            return r1.shallow_copy();
            
        Nodecl::NodeclBase result;
        
        Nodecl::NodeclBase r1_lb = r1.get_lower();   Nodecl::NodeclBase r1_ub = r1.get_upper();
        Nodecl::NodeclBase r2_lb = r2.get_lower();   Nodecl::NodeclBase r2_ub = r2.get_upper();
        TL::Type t = r1_lb.get_type();
        
        if( ( r1_lb == r2_lb ) && ( r1_ub == r2_ub ) )
        {   // The two ranges are exactly the same
            result = get_empty_range( t );
        }
        else if( r1_lb.is_constant() && r1_ub.is_constant() && r2_lb.is_constant() && r2_ub.is_constant() )
        {   // Let's compare the constant values
            const_value_t* r1_lb_c = r1_lb.get_constant();
            const_value_t* r1_ub_c = r1_ub.get_constant();
            const_value_t* r2_lb_c = r2_lb.get_constant();
            const_value_t* r2_ub_c = r2_ub.get_constant();
            if( const_value_is_positive( const_value_sub( r1_lb_c, r2_ub_c ) ) || 
                const_value_is_positive( const_value_sub( r2_lb_c, r1_ub_c ) ) )
            {   // r1 and r2 do not overlap
                result = r1.shallow_copy();
            }
            else if( const_value_is_zero( const_value_sub( r1_lb_c, r2_lb_c ) ) && 
                     const_value_is_zero( const_value_sub( r1_ub_c, r2_ub_c ) ) )
            {   // r1 and r2 are the same range
                result = get_empty_range( t );
            }
            else if( const_value_is_positive( const_value_sub( r1_lb_c, r2_lb_c ) ) && 
                     const_value_is_positive( const_value_sub( r2_ub_c, r1_ub_c ) ) )
            {   // r2 contains r1
                result = get_empty_range( t );
            }
            else if( const_value_is_positive( const_value_sub( r2_lb_c, r1_lb_c ) ) && 
                     const_value_is_positive( const_value_sub( r1_ub_c, r2_ub_c ) ) )
            {   // r2 is contained in r1
                Nodecl::Analysis::Range tmp1 = Nodecl::Analysis::Range::make( r1_lb.shallow_copy(), r2_lb.shallow_copy(), t );
                Nodecl::Analysis::Range tmp2 = Nodecl::Analysis::Range::make( r2_ub.shallow_copy(), r1_ub.shallow_copy(), t );
                result = Nodecl::Analysis::RangeUnion::make( tmp1, tmp2, t );
            }
            else
            {   // r1 and r2 overlap at some point
                Nodecl::NodeclBase lb, ub;
                if( const_value_is_positive( const_value_sub( r2_lb_c, r1_lb_c ) ) )
                {   // r1 lower bound is smaller than r2
                    lb = r1_lb.shallow_copy( );
                    ub = r2_lb.shallow_copy( );
                }
                else
                {   // r2 lower bound is smaller than r1
                    lb = r2_ub.shallow_copy( );
                    ub = r1_ub.shallow_copy( );
                }
            }
        }
        else
        {   // We are not able to synthesize the result
            result = Nodecl::Analysis::RangeSub::make( r1, r2, t );
        }
        
        return result;
    }
    
    // A[al, au] ∩ B[bl, bu] = (al<bl<au || al<bu<au) ? [max(al, bl), min(au, bu)]      -> overlap
    //                                                : ∅
    Nodecl::NodeclBase range_intersection( const Nodecl::Analysis::Range& r1, const Nodecl::Analysis::Range& r2 )
    {
        if( is_empty_range( r1 ) )
            return r2.shallow_copy();
        else if( is_empty_range( r2 ) )
            return r1.shallow_copy();
        
        Nodecl::NodeclBase result;

        Nodecl::NodeclBase r1_lb = r1.get_lower();   Nodecl::NodeclBase r1_ub = r1.get_upper();
        Nodecl::NodeclBase r2_lb = r2.get_lower();   Nodecl::NodeclBase r2_ub = r2.get_upper();
        TL::Type t = r1_lb.get_type();
        if( ( r1_lb == r2_lb ) && ( r1_ub == r2_ub ) )
        {   // The two ranges are exactly the same
            result = r1.shallow_copy( );
        }
        else if( r1_lb.is_constant() && r1_ub.is_constant() && r2_lb.is_constant() && r2_ub.is_constant() )
        {   // Let's compare the constant values
            const_value_t* r1_lb_c = r1_lb.get_constant();
            const_value_t* r1_ub_c = r1_ub.get_constant();
            const_value_t* r2_lb_c = r2_lb.get_constant();
            const_value_t* r2_ub_c = r2_ub.get_constant();
            
            if( const_value_is_positive( const_value_sub( r1_lb_c, r2_ub_c ) ) || 
                const_value_is_positive( const_value_sub( r2_lb_c, r1_ub_c ) ) )
            {   // r1 and r2 do not overlap
                result = get_empty_range( t );
            }
            else if( const_value_is_zero( const_value_sub( r1_lb_c, r2_lb_c ) ) && 
                     const_value_is_zero( const_value_sub( r1_ub_c, r2_ub_c ) ) )
            {   // r1 and r2 are the same range
                result = r1.shallow_copy();
            }
            else
            {   // r1 and r2 overlap in some way
                Nodecl::NodeclBase lb, ub ;
                if( const_value_is_positive( const_value_sub( r1_lb_c, r2_lb_c ) ) )
                {
                    lb = r1_lb.shallow_copy();
                    ub = r2_ub.shallow_copy();
                }
                else
                {
                    lb = r2_lb.shallow_copy();
                    ub = r1_ub.shallow_copy();
                }
                result = Nodecl::Analysis::Range::make( lb, ub, t );
            }
        }
        else
        {   // We are not able to synthesize the result
            result = Nodecl::Analysis::RangeIntersection::make( r1.shallow_copy(), r2.shallow_copy(), t );
        }
        
        return result;
    }
    
    // A[al, au] ∪ B[bl, bu] = (al<bl<au || al<bu<au) ? [min(al, bl), max(au, bu)]      -> overlap
    //                                                : [al, au] ∪ [bl, bu]             -> cannot synthesize the result as a unique range
    Nodecl::NodeclBase range_union( const Nodecl::Analysis::Range& r1, const Nodecl::Analysis::Range& r2 )
    {
        if( is_empty_range( r1 ) && is_empty_range( r2 ) )
            return r1.shallow_copy();
            
        Nodecl::NodeclBase result;
        
        Nodecl::NodeclBase r1_lb = r1.get_lower();   Nodecl::NodeclBase r1_ub = r1.get_upper();
        Nodecl::NodeclBase r2_lb = r2.get_lower();   Nodecl::NodeclBase r2_ub = r2.get_upper();
        TL::Type t = r1_lb.get_type();
        if( ( r1_lb == r2_lb ) && ( r1_ub == r2_ub ) )
        {   // The two ranges are exactly the same
            result = r1.shallow_copy( );
        }
        else if( r1_lb.is_constant() && r1_ub.is_constant() && r2_lb.is_constant() && r2_ub.is_constant() )
        {
            const_value_t* r1_lb_c = r1_lb.get_constant();
            const_value_t* r1_ub_c = r1_ub.get_constant();
            const_value_t* r2_lb_c = r2_lb.get_constant();
            const_value_t* r2_ub_c = r2_ub.get_constant();
            
            if( const_value_is_positive( const_value_sub( r1_lb_c, r2_ub_c ) ) || 
                const_value_is_positive( const_value_sub( r2_lb_c, r1_ub_c ) ) )
            {   // r1 and r2 do not overlap
                result = Nodecl::Analysis::RangeUnion::make( r1.shallow_copy(), r2.shallow_copy(), t );
            }
            else if( const_value_is_zero( const_value_sub( r1_lb_c, r2_lb_c ) ) && 
                     const_value_is_zero( const_value_sub( r1_ub_c, r2_ub_c ) ) )
            {   // r1 and r2 are the same range
                result = r1.shallow_copy();
            }
            else
            {   // r1 and r2 overlap in some way
                Nodecl::NodeclBase lb = const_value_is_positive( const_value_sub( r1_lb_c, r2_lb_c ) ) ? r2_lb.shallow_copy() 
                                                                                                       : r1_lb.shallow_copy();
                Nodecl::NodeclBase ub = const_value_is_positive( const_value_sub( r1_ub_c, r2_ub_c ) ) ? r1_ub.shallow_copy() 
                                                                                                       : r2_ub.shallow_copy();
                result = Nodecl::Analysis::Range::make( lb, ub, t );
            }
        }
        else
        {
            result = Nodecl::Analysis::RangeUnion::make( r1.shallow_copy(), r2.shallow_copy(), t );
        }
        
        return result;
    }

    // ******************************************************************************************* //
    // ******************************* Range Analysis Constraints ******************************** //
    
    Constraint::Constraint()
        : _constr_sym(Symbol()), _constraint(Nodecl::NodeclBase::null())
    {}
        
    Constraint::Constraint( const TL::Symbol& constr_sym, const Nodecl::NodeclBase& constraint )
        : _constr_sym( constr_sym ), _constraint( constraint )
    {}
        
    TL::Symbol Constraint::get_symbol() const 
    {
        return _constr_sym;
    }
        
    Nodecl::NodeclBase Constraint::get_constraint() const 
    {
        return _constraint;
    }
        
    bool Constraint::operator!=(const Constraint& c) 
    {
        return ((this->_constr_sym != c._constr_sym) ||
                !Nodecl::Utils::equal_nodecls(this->_constraint, c._constraint, /*skip_conversion_nodes*/true));
    }
    
    // ***************************** END Range Analysis Constraints ****************************** //
    // ******************************************************************************************* //
    
}
}
}