/*--------------------------------------------------------------------
  (C) Copyright 2006-2012 Barcelona Supercomputing Center
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

#include "tl-analysis-static-info.hpp"

namespace TL  {
namespace Analysis {
    
    // ********************************************************************************************* //
    // ************** Class to retrieve SIMD analysis info about one specific nodecl *************** //
    
    bool NodeclStaticInfo::is_adjacent_access( const Nodecl::NodeclBase& n ) const
    {
        bool result = true;
        
        if( n.is<Nodecl::ArraySubscript>( ) )
        {
            Nodecl::List subscript = n.as<Nodecl::ArraySubscript>( ).get_subscripts( ).as<Nodecl::List>( );
            Nodecl::List::iterator it = subscript.begin( );
            for( ; it != subscript.end( ) - 1; ++it )
            {   // All dimensions but the less significant must be constant
                if( !is_constant( *it ) )
                {
                    result = false;
                    break;
                }
            }
            // The less significant dimension must be accessed by an (+/-)c +/- IV, where c is a constant
            if( it == subscript.end( ) - 1 )
            {
                Nodecl::Utils::ReduceExpressionVisitor v;
                Nodecl::NodeclBase s = it->shallow_copy( );
                v.walk( s );
                
                AdjacentAccessVisitor iv_v( _induction_variables, _killed );
                bool constant = iv_v.walk( s );
                if( !constant )
                {
                    result = false;
                }
                else
                {
                    Utils::InductionVariableData* iv = iv_v.get_induction_variable( );
                    if( iv == NULL || !iv->is_increment_one( ) )
                    {
                        result = false;
                    }
                }
            }
        }
        
        return result;
    }
        
    bool NodeclStaticInfo::is_simd_aligned_access( const Nodecl::NodeclBase& n, ObjectList<Symbol> suitable_syms, 
                                                   int unroll_factor, int alignment ) const
    {
        if( !n.is<Nodecl::ArraySubscript>( ) )
        {
            std::cerr << "warning: returning false for is_simd_aligned_access when asking for nodecl '" 
                      << n.prettyprint( ) << "' which is not an array subscript" << std::endl;
            return false;
        }
        
        Nodecl::List subscripts = n.as<Nodecl::ArraySubscript>( ).get_subscripts( ).as<Nodecl::List>( );
        if( subscripts.size( ) != 1 )
        {
            std::cerr << "warning: returning false for is_simd_aligned_access when asking for nodecl '"
                      << n.prettyprint( ) << "', which is multidimensional. Only one dimension arrays are analyzed" << std::endl;
            return false;
        }
        
        bool result = false;
        
        Nodecl::NodeclBase subscripted = n.as<Nodecl::ArraySubscript>( ).get_subscripted( );
        Nodecl::NodeclBase subscript = *( subscripts.begin( ) );
        SuitableAlignmentVisitor sa_v( subscripted, _induction_variables, suitable_syms, unroll_factor, alignment );
        int subscript_alignment = sa_v.walk( subscript );
        if( subscript_alignment % alignment == 0 )
            result = true;
        
        return result;
    }
    
    // ************ END class to retrieve SIMD analysis info about one specific nodecl ************* //
    // ********************************************************************************************* //
    
    
    
    // ********************************************************************************************* //
    // ************************ Visitor retrieving suitable simd alignment ************************* //
    
    SuitableAlignmentVisitor::SuitableAlignmentVisitor( Nodecl::NodeclBase subscripted,
                                                        ObjectList<Utils::InductionVariableData*> induction_variables,
                                                        ObjectList<Symbol> suitable_syms, int unroll_factor, int alignment )
            : _subscripted( subscripted ), _induction_variables( induction_variables), _suitable_syms( suitable_syms ), 
              _unroll_factor( unroll_factor ), _alignment( alignment )
    {}
    
    int SuitableAlignmentVisitor::join_list( ObjectList<int>& list )
    {
        int result = 0;
        for( ObjectList<int>::iterator it = list.begin( ); it != list.end( ); ++it )
        {
            result = result + ( *it );
        }
        return result;
    }
    
    int SuitableAlignmentVisitor::visit( const Nodecl::Add& n )
    {
        int result = -1;
        
        int lhs_mod = walk( n.get_lhs( ) );
        int rhs_mod = walk( n.get_rhs( ) );
        
        if( ( lhs_mod != -1 ) && ( rhs_mod != -1 ) )
            result = lhs_mod + rhs_mod;
        
        return result;
    }
    
    int SuitableAlignmentVisitor::visit( const Nodecl::Minus& n )
    {
        int result = -1;
        
        int lhs_mod = walk( n.get_lhs( ) );
        int rhs_mod = walk( n.get_rhs( ) );
        
        if( ( lhs_mod != -1 ) && ( rhs_mod != -1 ) )
            result = lhs_mod - rhs_mod;
        
        return result;
    }
    
    int SuitableAlignmentVisitor::visit( const Nodecl::Symbol& n )
    {
        int result = -1;
        
        if( _suitable_syms.contains( n.get_symbol( ) ) )
        {
            result = 0;
        }
        else if( n.is_constant( ) )
        {
            result = ( ( const_value_cast_to_signed_int( n.get_constant( ) ) * _subscripted.get_type( ).get_size( ) )
                       % _alignment );
        }
        else if( Utils::induction_variable_list_contains_variable( _induction_variables, n ) )
        {
            Utils::InductionVariableData* iv = Utils::get_induction_variable_from_list( _induction_variables, n );
            Nodecl::Utils::ReduceExpressionVisitor v;
            
            Nodecl::NodeclBase lb = iv->get_lb( ).shallow_copy( );
            v.walk( lb );
            if( lb.is_constant( ) )
            {
                Nodecl::NodeclBase incr = iv->get_increment( ).shallow_copy( );
                v.walk( incr );
                if( incr.is_constant( ) )
                {
                    int n_align = const_value_cast_to_signed_int( lb.get_constant( ) ) 
                                  + ( const_value_cast_to_signed_int( incr.get_constant( ) ) 
                                      * _unroll_factor * _subscripted.get_type( ).get_size( ) );
                    result = ( n_align % _alignment );
                }
            }
        }
        
        return result;
    }
    
    // ********************** END visitor retrieving suitable simd alignment *********************** //
    // ********************************************************************************************* //
    
    
    
    // ********************************************************************************************* //
    // ***************** Visitor retrieving adjacent array accesses within a loop ****************** //
    
    AdjacentAccessVisitor::AdjacentAccessVisitor( ObjectList<Analysis::Utils::InductionVariableData*> ivs, 
                                                  Utils::ext_sym_set killed )
            : _induction_variables( ivs ), _killed( killed ), _iv( NULL ), _iv_found( false )
    {}
    
    Utils::InductionVariableData* AdjacentAccessVisitor::get_induction_variable( )
    {
        return _iv;
    }
    
    Utils::InductionVariableData* AdjacentAccessVisitor::variable_is_iv( const Nodecl::NodeclBase& n )
    {
        Utils::InductionVariableData* res = NULL;
        for( ObjectList<Utils::InductionVariableData*>::const_iterator it = _induction_variables.begin( );
            it != _induction_variables.end( ); ++it )
            {
                if( Nodecl::Utils::equal_nodecls( ( *it )->get_variable( ).get_nodecl( ), n, /* skip conversion nodes */ true ) )
                {
                    res = *it;
                    break;
                }
            }
            return res;
    }
    
    bool AdjacentAccessVisitor::visit_binary_node( const Nodecl::NodeclBase& lhs, const Nodecl::NodeclBase& rhs )
    {
        return ( walk( lhs ) && walk( rhs ) );
    }
    
    bool AdjacentAccessVisitor::visit_unary_node( const Nodecl::NodeclBase& rhs )
    {
        return walk( rhs );
    }
    
    bool AdjacentAccessVisitor::join_list( ObjectList<bool>& list )
    {
        bool result = true;
        for( ObjectList<bool>::iterator it = list.begin( ); it != list.end( ); ++it )
        {
            result = result && ( *it );
        }
        return result;
    }
    
    bool AdjacentAccessVisitor::visit( const Nodecl::Add& n )
    {
        return visit_binary_node( n.get_lhs( ), n.get_rhs( ) );
    }
    
    bool AdjacentAccessVisitor::visit( const Nodecl::AddAssignment& n )
    {
        return visit_binary_node( n.get_lhs( ), n.get_rhs( ) );
    }
    
    bool AdjacentAccessVisitor::visit( const Nodecl::ArithmeticShr& n )
    {
        return visit_binary_node( n.get_lhs( ), n.get_rhs( ) );
    }
    
    bool AdjacentAccessVisitor::visit( const Nodecl::ArithmeticShrAssignment& n )
    {
        return visit_binary_node( n.get_lhs( ), n.get_rhs( ) );
    }
    
    bool AdjacentAccessVisitor::visit( const Nodecl::ArraySubscript& n )
    {
        bool res = true;
        Utils::InductionVariableData* iv = variable_is_iv( n );
        if( !_iv_found && iv != NULL)
        {
            _iv = iv;
            _iv_found = true;
        }
        else
        {
            res = walk( n.get_subscripts( ) );
        }
        return res;
    }
    
    bool AdjacentAccessVisitor::visit( const Nodecl::Assignment& n )
    {
        return visit_binary_node( n.get_lhs( ), n.get_rhs( ) );
    }
    
    bool AdjacentAccessVisitor::visit( const Nodecl::BitwiseAnd& n )
    {
        return visit_binary_node( n.get_lhs( ), n.get_rhs( ) );
    }
    
    bool AdjacentAccessVisitor::visit( const Nodecl::BitwiseAndAssignment& n )
    {
        return visit_binary_node( n.get_lhs( ), n.get_rhs( ) );
    }
    
    bool AdjacentAccessVisitor::visit( const Nodecl::BitwiseNot& n )
    {
        return visit_unary_node( n.get_rhs( ) );
    }
    
    bool AdjacentAccessVisitor::visit( const Nodecl::BitwiseOr& n )
    {
        return visit_binary_node( n.get_lhs( ), n.get_rhs( ) );
    }
    
    bool AdjacentAccessVisitor::visit( const Nodecl::BitwiseOrAssignment& n )
    {
        return visit_binary_node( n.get_lhs( ), n.get_rhs( ) );
    }
    
    bool AdjacentAccessVisitor::visit( const Nodecl::BitwiseShl& n )
    {
        return visit_binary_node( n.get_lhs( ), n.get_rhs( ) );
    }
    
    bool AdjacentAccessVisitor::visit( const Nodecl::BitwiseShlAssignment& n )
    {
        return visit_binary_node( n.get_lhs( ), n.get_rhs( ) );
    }
    
    bool AdjacentAccessVisitor::visit( const Nodecl::BitwiseShr& n )
    {
        return visit_binary_node( n.get_lhs( ), n.get_rhs( ) );
    }
    
    bool AdjacentAccessVisitor::visit( const Nodecl::BitwiseShrAssignment& n)
    {
        return visit_binary_node( n.get_lhs( ), n.get_rhs( ) );
    }
    
    bool AdjacentAccessVisitor::visit( const Nodecl::BitwiseXor& n )
    {
        return visit_binary_node( n.get_lhs( ), n.get_rhs( ) );
    }
    
    bool AdjacentAccessVisitor::visit( const Nodecl::BitwiseXorAssignment& n )
    {
        return visit_binary_node( n.get_lhs( ), n.get_rhs( ) );
    }
    
    bool AdjacentAccessVisitor::visit( const Nodecl::BooleanLiteral& n )
    {
        return true;
    }
    
    bool AdjacentAccessVisitor::visit( const Nodecl::Cast& n )
    {
        return visit_unary_node( n.get_rhs( ) );
    }
    
    bool AdjacentAccessVisitor::visit( const Nodecl::ComplexLiteral& n )
    {
        return true;
    }
    
    bool AdjacentAccessVisitor::visit( const Nodecl::Conversion& n )
    {
        return visit_unary_node( n.get_nest( ) );
    }
    
    bool AdjacentAccessVisitor::visit( const Nodecl::Different& n )
    {
        return visit_binary_node( n.get_lhs( ), n.get_rhs( ) );
    }
    
    bool AdjacentAccessVisitor::visit( const Nodecl::Div& n )
    {
        return visit_binary_node( n.get_lhs( ), n.get_rhs( ) );
    }
    
    bool AdjacentAccessVisitor::visit( const Nodecl::DivAssignment& n )
    {
        return visit_binary_node( n.get_lhs( ), n.get_rhs( ) );
    }
    
    bool AdjacentAccessVisitor::visit( const Nodecl::Equal& n )
    {
        return visit_binary_node( n.get_lhs( ), n.get_rhs( ) );
    }
    
    bool AdjacentAccessVisitor::visit( const Nodecl::FloatingLiteral& n )
    {
        return true;
    }
    
    bool AdjacentAccessVisitor::visit( const Nodecl::FunctionCall& n )
    {
        // FIXME We may do something more here...
        return false;
    }
    
    bool AdjacentAccessVisitor::visit( const Nodecl::GreaterOrEqualThan& n )
    {
        return visit_binary_node( n.get_lhs( ), n.get_rhs( ) );
    }
    
    bool AdjacentAccessVisitor::visit( const Nodecl::GreaterThan& n )
    {
        return visit_binary_node( n.get_lhs( ), n.get_rhs( ) );
    }
    
    bool AdjacentAccessVisitor::visit( const Nodecl::IntegerLiteral& n )
    {
        return true;
    }
    
    bool AdjacentAccessVisitor::visit( const Nodecl::LogicalAnd& n )
    {
        return visit_binary_node( n.get_lhs( ), n.get_rhs( ) );
    }
    
    bool AdjacentAccessVisitor::visit( const Nodecl::LogicalNot& n )
    {
        return visit_unary_node( n.get_rhs( ) );
    }
    
    bool AdjacentAccessVisitor::visit( const Nodecl::LogicalOr& n )
    {
        return visit_binary_node( n.get_lhs( ), n.get_rhs( ) );
    }
    
    bool AdjacentAccessVisitor::visit( const Nodecl::LowerOrEqualThan& n )
    {
        return visit_binary_node( n.get_lhs( ), n.get_rhs( ) );
    }
    
    bool AdjacentAccessVisitor::visit( const Nodecl::LowerThan& n )
    {
        return visit_binary_node( n.get_lhs( ), n.get_rhs( ) );
    }
    
    bool AdjacentAccessVisitor::visit( const Nodecl::Minus& n )
    {
        return visit_binary_node( n.get_lhs( ), n.get_rhs( ) );
    }
    
    bool AdjacentAccessVisitor::visit( const Nodecl::MinusAssignment& n )
    {
        return visit_binary_node( n.get_lhs( ), n.get_rhs( ) );
    }
    
    bool AdjacentAccessVisitor::visit( const Nodecl::Mod& n )
    {
        return visit_binary_node( n.get_lhs( ), n.get_rhs( ) );
    }
    
    bool AdjacentAccessVisitor::visit( const Nodecl::ModAssignment& n )
    {
        return visit_binary_node( n.get_lhs( ), n.get_rhs( ) );
    }
    
    bool AdjacentAccessVisitor::visit( const Nodecl::Mul& n )
    {
        return visit_binary_node( n.get_lhs( ), n.get_rhs( ) );
    }
    
    bool AdjacentAccessVisitor::visit( const Nodecl::MulAssignment& n )
    {
        return visit_binary_node( n.get_lhs( ), n.get_rhs( ) );
    }
    
    bool AdjacentAccessVisitor::visit( const Nodecl::Neg& n )
    {
        return walk( n.get_rhs( ) );
    }
    
    bool AdjacentAccessVisitor::visit( const Nodecl::ObjectInit& n )
    {
        return walk( n.get_symbol( ).get_value( ) );
    }
    
    bool AdjacentAccessVisitor::visit( const Nodecl::Plus& n )
    {
        return visit_unary_node( n.get_rhs( ) );
    }
    
    bool AdjacentAccessVisitor::visit( const Nodecl::PointerToMember& n )
    {
        bool res = true;
        Utils::InductionVariableData* iv = variable_is_iv( n );
        if( !_iv_found && iv != NULL)
        {
            _iv = iv;
            _iv_found = true;
        }
        else
        {
            res = !Utils::ext_sym_set_contains_nodecl( n, _killed );
        }
        return res;
    }
    
    bool AdjacentAccessVisitor::visit( const Nodecl::Postdecrement& n )
    {
        walk( n.get_rhs( ) );
        return false;
    }
    
    bool AdjacentAccessVisitor::visit( const Nodecl::Postincrement& n )
    {
        walk( n.get_rhs( ) );
        return false;
    }
    
    bool AdjacentAccessVisitor::visit( const Nodecl::Power& n )
    {
        return visit_binary_node( n.get_lhs( ), n.get_rhs( ) );
    }
    
    bool AdjacentAccessVisitor::visit( const Nodecl::Predecrement& n )
    {
        walk( n.get_rhs( ) );
        return false;
    }
    
    bool AdjacentAccessVisitor::visit( const Nodecl::Preincrement& n )
    {
        walk( n.get_rhs( ) );
        return false;
    }
    
    bool AdjacentAccessVisitor::visit( const Nodecl::Reference& n )
    {
        return walk( n.get_rhs( ) );
    }
    
    bool AdjacentAccessVisitor::visit( const Nodecl::Sizeof& n )
    {
        return walk( n.get_size_type( ) );
    }
    
    bool AdjacentAccessVisitor::visit( const Nodecl::StringLiteral& n )
    {
        return true;
    }
    
    bool AdjacentAccessVisitor::visit( const Nodecl::Symbol& n )
    {
        bool res = true;
        Utils::InductionVariableData* iv = variable_is_iv( n );
        if( !_iv_found && iv != NULL)
        {
            _iv = iv;
            _iv_found = true;
        }
        else
        {
            res = !Utils::ext_sym_set_contains_nodecl( n, _killed );
        }
        return res;
    }
    
    // *************** END visitor retrieving adjacent array accesses within a loop **************** //
    // ********************************************************************************************* //

}
}