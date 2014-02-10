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
    bool empty_interval( const Nodecl::Analysis::ClosedInterval& c )
    {
        bool result = false;
        if ( c.get_lower( ).is_constant( ) && c.get_upper( ).is_constant( ) )
        {
            result = const_value_is_negative( const_value_sub( c.get_upper( ).get_constant( ), c.get_lower( ).get_constant( ) ) );
        }
        return result;
    }
    
    // [a, b] + [c, d] = [a+d, b+c]
    Nodecl::NodeclBase interval_add( const Nodecl::Analysis::ClosedInterval& c1, const Nodecl::Analysis::ClosedInterval& c2 )
    {
        Nodecl::NodeclBase lb = Nodecl::Add::make( c1.get_lower( ).shallow_copy( ), c2.get_lower( ).shallow_copy( ), c1.get_type( ) );
        Nodecl::NodeclBase ub = Nodecl::Add::make( c1.get_upper( ).shallow_copy( ), c2.get_upper( ).shallow_copy( ), c1.get_type( ) );
        return Nodecl::Analysis::ClosedInterval::make( lb, ub, c1.get_type( ) );
    }
    
    // [a, b] − [c, d] = [a-d, b-c]
    Nodecl::NodeclBase interval_sub( const Nodecl::Analysis::ClosedInterval& c1, const Nodecl::Analysis::ClosedInterval& c2 )
    {
        Nodecl::NodeclBase lb = Nodecl::Minus::make( c1.get_lower( ).shallow_copy( ), c2.get_upper( ).shallow_copy( ), c1.get_type( ) );
        Nodecl::NodeclBase ub = Nodecl::Minus::make( c1.get_upper( ).shallow_copy( ), c2.get_lower( ).shallow_copy( ), c1.get_type( ) );
        return Nodecl::Analysis::ClosedInterval::make( lb, ub, c1.get_type( ) );
    }
    
    // [a, b] * [c, d] = [min(a*c, a*d, b*c, b*d), max(a*c, a*d, b*c, b*d)]
    Nodecl::NodeclBase range_mul( const Nodecl::Analysis::ClosedInterval& c1, const Nodecl::Analysis::ClosedInterval& c2 )
    {
        Nodecl::NodeclBase result;
        
        Nodecl::NodeclBase c1_lb = c1.get_lower( );   Nodecl::NodeclBase c1_ub = c1.get_upper( );
        Nodecl::NodeclBase c2_lb = c2.get_lower( );   Nodecl::NodeclBase c2_ub = c2.get_upper( );
        TL::Type t = c1_lb.get_type( );
        
        if( c1_lb.is_constant( ) && c1_ub.is_constant( ) && c2_lb.is_constant( ) && c2_ub.is_constant( ) )
        {   // Having constant values we can apply the arithmetic
            const_value_t* c1_lb_c = c1_lb.get_constant( );   const_value_t* c1_ub_c = c1_ub.get_constant( );
            const_value_t* c2_lb_c = c2_lb.get_constant( );   const_value_t* c2_ub_c = c2_ub.get_constant( );
            const_value_t* candidates[4] = {
                const_value_mul( c1_lb_c, c2_lb_c ), const_value_mul( c1_lb_c, c2_ub_c ),
                const_value_mul( c1_ub_c, c2_lb_c ), const_value_mul( c1_ub_c, c2_ub_c ) 
            };
            const_value_t* min = candidates[0];
            const_value_t* max = candidates[0];
            
            int i;
            for( i=1; i<4; ++i ) {
                if( const_value_is_positive( const_value_sub( min, candidates[i] ) ) )
                    min = candidates[i];
                if( const_value_is_negative( const_value_sub( max, candidates[i] ) ) )
                    max = candidates[i];
            }
                
            result = Nodecl::Analysis::ClosedInterval::make( const_value_to_nodecl( min ), const_value_to_nodecl( max ), t );
        }
        else
        {   // Otherwise, we represent the operations with Nodecl operations
            Nodecl::List expr_list = Nodecl::List::make( 
                    Nodecl::Mul::make( c1_lb.shallow_copy( ), c2_lb.shallow_copy( ), t ), 
                    Nodecl::Mul::make( c1_lb.shallow_copy( ), c2_ub.shallow_copy( ), t ), 
                    Nodecl::Mul::make( c1_ub.shallow_copy( ), c2_lb.shallow_copy( ), t ), 
                    Nodecl::Mul::make( c1_ub.shallow_copy( ), c2_ub.shallow_copy( ), t ) );
            Nodecl::Analysis::Minimum lhs = Nodecl::Analysis::Minimum::make( expr_list, t );
            Nodecl::Analysis::Maximum rhs = Nodecl::Analysis::Maximum::make( expr_list.shallow_copy( ), t );
            result = Nodecl::Analysis::ClosedInterval::make( lhs, rhs, t );
        }
        
        return result;
    }
    
    // [a, b] / [c, d] = [min(a/c, a/d, b/c, b/d), max(a/c, a/d, b/c, b/d)]
    Nodecl::NodeclBase range_div( const Nodecl::Analysis::ClosedInterval& c1, const Nodecl::Analysis::ClosedInterval& c2 )
    {
        Nodecl::NodeclBase result;
        
        Nodecl::NodeclBase c1_lb = c1.get_lower( );   Nodecl::NodeclBase c1_ub = c1.get_upper( );
        Nodecl::NodeclBase c2_lb = c2.get_lower( );   Nodecl::NodeclBase c2_ub = c2.get_upper( );
        TL::Type t = c1_lb.get_type( );
        
        if( c1_lb.is_constant( ) && c1_ub.is_constant( ) &&  c2_lb.is_constant( ) && c2_ub.is_constant( ) ) 
        {   // Having constant values we can apply the arithmetic
            const_value_t* c1_lb_c = c1_lb.get_constant( );   const_value_t* c1_ub_c = c1_ub.get_constant( );
            const_value_t* c2_lb_c = c2_lb.get_constant( );   const_value_t* c2_ub_c = c2_ub.get_constant( );
            const_value_t* candidates[4] = {
                const_value_div( c1_lb_c, c2_lb_c ), const_value_div( c1_lb_c, c2_ub_c ),
                const_value_div( c1_ub_c, c2_lb_c ), const_value_div( c1_ub_c, c2_ub_c )
            };
            const_value_t* min = candidates[0];
            const_value_t* max = candidates[0];
            for( int i=1; i<4; ++i ) {
                if( const_value_is_positive( const_value_sub( min, candidates[i] ) ) )
                    min = candidates[i];
                if( const_value_is_negative( const_value_sub( max, candidates[i] ) ) )
                    max = candidates[i];
            }
            
            result = Nodecl::Analysis::ClosedInterval::make( const_value_to_nodecl( min ), const_value_to_nodecl( max ), t );
            
        } else {
            if( ( c2_lb.is_constant( ) && const_value_is_zero( c2_lb.get_constant( ) ) ) || 
                ( c2_ub.is_constant( ) && const_value_is_zero( c2_ub.get_constant( ) ) ) ) 
            {
                WARNING_MESSAGE( "Division by an interval containing zero is not defined under the basic interval arithmetic." 
                                 "As a result of this division we return the maximum interval\n", 0 );
                result = Nodecl::Analysis::OpenInterval::make( Nodecl::Analysis::MinusInfinity::make( ), Nodecl::Analysis::PlusInfinity::make( ), t );
            }
            else
            {
                Nodecl::List expr_list = Nodecl::List::make( 
                        Nodecl::Div::make( c1_lb.shallow_copy( ), c2_lb.shallow_copy( ), t ), 
                        Nodecl::Div::make( c1_lb.shallow_copy( ), c2_ub.shallow_copy( ), t ), 
                        Nodecl::Div::make( c1_ub.shallow_copy( ), c2_lb.shallow_copy( ), t ), 
                        Nodecl::Div::make( c1_ub.shallow_copy( ), c2_ub.shallow_copy( ), t ) );
                Nodecl::Analysis::Minimum lhs = Nodecl::Analysis::Minimum::make( expr_list, t );
                Nodecl::Analysis::Maximum rhs = Nodecl::Analysis::Maximum::make( expr_list.shallow_copy( ), t );
                result = Nodecl::Analysis::ClosedInterval::make( lhs, rhs, t );
            }
        }
        
        return result;
    }
    
    // [a, b] ∩ [c, d] = [max(a, c), min(b, d)]
    Nodecl::NodeclBase range_intersection( const Nodecl::Analysis::ClosedInterval& c1, const Nodecl::Analysis::ClosedInterval& c2 )
    {
        Nodecl::NodeclBase result;
        
        Nodecl::NodeclBase c1_lb = c1.get_lower( );   Nodecl::NodeclBase c1_ub = c1.get_upper( );
        Nodecl::NodeclBase c2_lb = c2.get_lower( );   Nodecl::NodeclBase c2_ub = c2.get_upper( );
        TL::Type t = c1_lb.get_type( );
        
        if( !empty_interval( c1 ) && !empty_interval( c2 ) && 
            c1_lb.is_constant( ) && c1_ub.is_constant( ) && c2_lb.is_constant( ) && c2_ub.is_constant( ) )
        {
            Nodecl::NodeclBase lb, ub;
            if( const_value_is_positive( const_value_sub( c1_lb.get_constant( ), c2_lb.get_constant( ) ) ) )
                lb = c1_lb;
            else
                lb = c2_lb;
            if( const_value_is_positive( const_value_sub( c1_ub.get_constant( ), c2_ub.get_constant( ) ) ) )
                ub = c2_ub;
            else
                lb = c1_ub;
            result = Nodecl::Analysis::ClosedInterval::make( lb, ub, t );
        }
        else
        {
            Nodecl::Analysis::Maximum lb = Nodecl::Analysis::Maximum::make( Nodecl::List::make( c1_lb.shallow_copy( ), c2_lb.shallow_copy( ) ), t );
            Nodecl::Analysis::Minimum ub = Nodecl::Analysis::Minimum::make( Nodecl::List::make( c1_ub.shallow_copy( ), c2_ub.shallow_copy( ) ), t );
            result = Nodecl::Analysis::ClosedInterval::make( lb, ub, t );
        }
        
        return result;
    }
    
    // [a, b] ∪ [c, d] = [min(a, c), max(b, d)]
    Nodecl::NodeclBase range_union( const Nodecl::Analysis::ClosedInterval& c1, const Nodecl::Analysis::ClosedInterval& c2 )
    {
        Nodecl::Analysis::ClosedInterval result;
        
        Nodecl::NodeclBase c1_lb = c1.get_lower( ).shallow_copy( );   Nodecl::NodeclBase c1_ub = c1.get_upper( ).shallow_copy( );
        Nodecl::NodeclBase c2_lb = c2.get_lower( ).shallow_copy( );   Nodecl::NodeclBase c2_ub = c2.get_upper( ).shallow_copy( );
        TL::Type t = c1_lb.get_type( );
        
        if( !empty_interval( c1 ) && !empty_interval( c2 ) && 
            c1_lb.is_constant( ) && c1_ub.is_constant( ) && c2_lb.is_constant( ) && c2_ub.is_constant( ) )
        {
            Nodecl::NodeclBase lb = ( const_value_is_positive( const_value_sub( c1_lb.get_constant( ), c2_lb.get_constant( ) ) ) ? c2_lb : c1_lb );
            Nodecl::NodeclBase ub = ( const_value_is_positive( const_value_sub( c1_ub.get_constant( ), c2_ub.get_constant( ) ) ) ? c1_ub : c2_ub );
            result = Nodecl::Analysis::ClosedInterval::make( lb, ub, t );
        }
        else
        {
            Nodecl::Analysis::Minimum lb = Nodecl::Analysis::Minimum::make( Nodecl::List::make( c1_lb.shallow_copy( ), c2_lb.shallow_copy( ) ), t );
            Nodecl::Analysis::Maximum ub = Nodecl::Analysis::Maximum::make( Nodecl::List::make( c1_ub.shallow_copy( ), c2_ub.shallow_copy( ) ), t );
            result = Nodecl::Analysis::ClosedInterval::make( lb, ub, t );
        }
        return result;
    }

}
}
}