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
    const long double Min = std::numeric_limits<double>::min( );
    const long double Max = std::numeric_limits<double>::max( );
    
    Range::Range( )
        : _lb( const_value_to_nodecl( const_value_get_long_double( Min ) ) ), 
          _ub( const_value_to_nodecl( const_value_get_long_double( Max ) ) ),
          _type( Empty )
    {}
    
    Range::Range( Nodecl::NodeclBase lb, Nodecl::NodeclBase ub, RangeType type )
        : _lb( lb ), _ub( ub ), _type( type )
    {}

    Range::~Range( );

    Nodecl::NodeclBase Range::get_lower( ) const 
    {
        return _lb;
    }
    
    Nodecl::NodeclBase Range::get_upper( ) const 
    {
        return _ub;
    }
    
    void Range::set_lower( const Nodecl::NodeclBase& new_lb ) 
    { 
        this->_lb = new_lb; 
    }
    
    void Range::set_upper( const Nodecl::NodeclBase& new_ub ) 
    { 
        this->_ub = new_ub;
    }
    
    // _type==Empty || ub < lb
    bool Range::is_empty( ) const
    {
        return ( ( _lb.is_constant( ) && _ub.is_constant( ) && 
                   const_value_is_negative( const_value_sub( _ub.get_constant( ), _lb.get_constant( ) ) ) ) 
                 || ( _type == Empty ) );
    }
    
    // [a, b] + [c, d] = [a+d, b+c]
    Range Range::range_add( const Range& r )
    {
        Nodecl::NodeclBase lb = Nodecl::Add::make( _lb, r._lb, _lb.get_type( ) );
        Nodecl::NodeclBase ub = Nodecl::Add::make( _ub, r._ub, _ub.get_type( ) );
        return Range( lb, ub );
    }
    
    // [a, b] − [c, d] = [a-d, b-c]
    Range Range::range_sub( const Range& r )
    {
        Nodecl::NodeclBase lb = Nodecl::Minus::make( _lb, r._ub, _lb.get_type( ) );
        Nodecl::NodeclBase ub = Nodecl::Minus::make( _ub, r._lb, _ub.get_type( ) );
        return Range( lb, ub );
    }
    
    // [a, b] * [c, d] = [min(a*c, a*d, b*c, b*d), max(a*c, a*d, b*c, b*d)]
    Range Range::range_mul( const Range& r )
    {
        Range result;
        if( _lb.is_constant( ) && _ub.is_constant( ) && r._lb.is_constant( ) && r._ub.is_constant( ) )
        {   // Having constant values we can apply the arithmetic
            const_value_t* candidates[4] = {
                calc.compute_const_value( Nodecl::Mul::make( _lb, r._lb, _lb.get_type( ) ) ),
                calc.compute_const_value( Nodecl::Mul::make( _lb, r._ub, _lb.get_type( ) ) ),
                calc.compute_const_value( Nodecl::Mul::make( _ub, r._lb, _lb.get_type( ) ) ),
                calc.compute_const_value( Nodecl::Mul::make( _ub, r._ub, _lb.get_type( ) ) ) 
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
                
            result = Range( const_value_to_nodecl( min ), const_value_to_nodecl( max ) );
        }
        return result;
    }
    
    // [a, b] / [c, d] = [min(a/c, a/d, b/c, b/d), max(a/c, a/d, b/c, b/d)]
    Range Range::range_div( const Range& r )
    {
        Range result;
        if( _lb.is_constant( ) && _ub.is_constant( ) && r._lb.is_constant( ) && r._ub.is_constant( ) ) 
        {   // Having constant values we can apply the arithmetic
            const_value_t* lb = _lb.get_constant( );
            const_value_t* ub = _ub.get_constant( );
            const_value_t* rlb = r._lb.get_constant( );
            const_value_t* rub = r._ub.get_constant( );
            const_value_t* candidates[4] = {
                const_value_div( lb, rlb ),
                const_value_div( lb, rub ),
                const_value_div( ub, rlb ),
                const_value_div( ub, rub )
            };
            const_value_t* min = candidates[0];
            const_value_t* max = candidates[0];
            for( int i=1; i<4; ++i ) {
                if( const_value_is_positive( const_value_sub( min, candidates[i] ) ) )
                    min = candidates[i];
                if( const_value_is_negative( const_value_sub( max, candidates[i] ) ) )
                    max = candidates[i];
            }
            
            result = Range( const_value_to_nodecl( min ), const_value_to_nodecl( max ) );
            
        } else {
            if( ( r._lb.is_constant( ) && const_value_is_zero( r._lb.get_constant( ) ) ) || 
                ( r._ub.is_constant( ) && const_value_is_zero( r._ub.get_constant( ) ) ) ) {
                WARNING_MESSAGE( "Division by an interval containing zero is not defined under the basic interval arithmetic." 
                                 "As a result of this division we return the maximum interval\n", 0 );
            }
        }
        
        return result;
    }
    
    // [a, b] ∩ [c, d] = [max(a, c), min(b, d)]
    Range Range::range_intersection( const Range& r ) const
    {
        Range result;
        if( !is_empty( ) && !r.is_empty( ) && 
            _lb.is_constant( ) && _ub.is_constant( ) && r._lb.is_constant( ) && r._ub.is_constant( ) )
        {
            Nodecl::NodeclBase lb, ub;
            if( const_value_is_positive( const_value_sub( _lb.get_constant( ), r._lb.get_constant( ) ) ) )
                lb = _lb;
            else
                lb = r._lb;
            if( const_value_is_positive( const_value_sub( _ub.get_constant( ), r._ub.get_constant( ) ) ) )
                lb = r._ub;
            else
                lb = _ub;
            
        }
        return result;
    }
    
    // [a, b] ∪ [c, d] = [min(a, c), max(b, d)]
    Range Range::range_union( const Range& r ) const
    {
        Range result;
        if( !is_empty( ) && !r.is_empty( ) && 
            _lb.is_constant( ) && _ub.is_constant( ) && r._lb.is_constant( ) && r._ub.is_constant( ) )
        {
            Nodecl::NodeclBase lb, ub;
            if( const_value_is_positive( const_value_sub( _lb.get_constant( ), r._lb.get_constant( ) ) ) )
                lb = r._lb;
            else
                lb = _lb;
            if( const_value_is_positive( const_value_sub( _ub.get_constant( ), r._ub.get_constant( ) ) ) )
                lb = _ub;
            else
                lb = r._ub;
            
        }
        return result;
    }
    
    
    bool Range::operator==( const Range& r ) const
    {
        return ( Nodecl::Utils::equal_nodecls( _lb, r._lb, /*skip conversion*/ true ) && 
                 Nodecl::Utils::equal_nodecls( _ub, r._ub, /*skip conversion*/ true ) && 
                 _type == r._type );
    }
    
    bool Range::operator!=( const Range& r ) const
    {
        return ( !Nodecl::Utils::equal_nodecls( _lb, r._lb, /*skip conversion*/ true ) || 
                 !Nodecl::Utils::equal_nodecls( _ub, r._ub, /*skip conversion*/ true ) || 
                 _type != r._type );
    }
}
}
}