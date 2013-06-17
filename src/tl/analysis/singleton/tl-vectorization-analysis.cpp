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
#include <algorithm>

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
                
                ArrayAccessInfoVisitor iv_v( _induction_variables, _killed );
                iv_v.walk( s );
                result = iv_v.is_adjacent_access( );
            }
        }
        
        return result;
    }

    bool NodeclStaticInfo::is_induction_variable_dependent_access( const Nodecl::NodeclBase& n ) const
    {
        bool result = false;
        
//         std::cout << "Access: " << n.prettyprint() << "\n";

        if( n.is<Nodecl::ArraySubscript>( ) )
        {
            Nodecl::List subscript = n.as<Nodecl::ArraySubscript>( ).get_subscripts( ).as<Nodecl::List>( );

            for( Nodecl::List::iterator it = subscript.begin( ); it != subscript.end( ); it++ )
            { 
                Nodecl::Utils::ReduceExpressionVisitor v;
                Nodecl::NodeclBase s = it->shallow_copy( );
                v.walk( s );
                
                ArrayAccessInfoVisitor iv_v( _induction_variables, _killed );
                iv_v.walk( s );
                result = iv_v.depends_on_induction_vars( );
            }
        }
        
        return result;
    }

    bool NodeclStaticInfo::is_constant_access( const Nodecl::NodeclBase& n ) const
    {
        bool result = true;
        
        if( n.is<Nodecl::ArraySubscript>( ) )
        {
            Nodecl::List subscript = n.as<Nodecl::ArraySubscript>( ).get_subscripts( ).as<Nodecl::List>( );
            Nodecl::List::iterator it = subscript.begin( );
            for( ; it != subscript.end( ); ++it )
            {   // All dimensions must be constant
                if( !is_constant( *it ) )
                {
                    result = false;
                    break;
                }
            }
        }
        
        return result;
    }
        
    bool NodeclStaticInfo::is_simd_aligned_access( const Nodecl::NodeclBase& n, const Nodecl::List suitable_expressions, 
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

        int type_size = subscripted.get_type().basic_type().get_size();

        SuitableAlignmentVisitor sa_v( subscripted, _induction_variables, suitable_expressions, unroll_factor, type_size );
        int subscript_alignment = sa_v.walk( subscript );
        
        if( (subscript_alignment % alignment) == 0 )
            result = true;
        
        return result;
    }
    
    // ************ END class to retrieve SIMD analysis info about one specific nodecl ************* //
    // ********************************************************************************************* //
    
    
    
    // ********************************************************************************************* //
    // ************************ Visitor retrieving suitable simd alignment ************************* //
    
    SuitableAlignmentVisitor::SuitableAlignmentVisitor( Nodecl::NodeclBase subscripted,
                                                        ObjectList<Utils::InductionVariableData*> induction_variables,
                                                        Nodecl::List suitable_expressions, int unroll_factor, int type_size)
            : _subscripted( subscripted ), _induction_variables( induction_variables), _suitable_expressions( suitable_expressions ), 
              _unroll_factor( unroll_factor ), _type_size(type_size)
    {
    }
    
    int SuitableAlignmentVisitor::join_list( ObjectList<int>& list ) 
    {
        int result = 0;
        for( ObjectList<int>::iterator it = list.begin( ); it != list.end( ); ++it )
        {
            result = result + ( *it );
        }
        return result;
    }

    bool SuitableAlignmentVisitor::is_suitable_expression(Nodecl::NodeclBase n)
    {
        if( _suitable_expressions.end() == std::find_if(_suitable_expressions.begin(), _suitable_expressions.end(), 
                std::bind1st(std::ptr_fun(Nodecl::Utils::equal_nodecls), n)))
            return false;

        return true;
    }
    
    int SuitableAlignmentVisitor::visit( const Nodecl::Add& n )
    {
        if (is_suitable_expression(n))
        {
            return 0;
        }

        int lhs_mod = walk( n.get_lhs( ) );
        int rhs_mod = walk( n.get_rhs( ) );
        
        if( ( lhs_mod >= 0 ) && ( rhs_mod >= 0 ) )
            return lhs_mod + rhs_mod;
        
        return -1;
    }
    
    int SuitableAlignmentVisitor::visit( const Nodecl::Minus& n ) 
    {
        if (is_suitable_expression(n))
        {
            return 0;
        }

        int lhs_mod = walk( n.get_lhs( ) );
        int rhs_mod = walk( n.get_rhs( ) );
        
        if( ( lhs_mod >= 0 ) && ( rhs_mod >= 0 ) )
            return lhs_mod - rhs_mod;
        
        return -1;
    }

    int SuitableAlignmentVisitor::visit( const Nodecl::Mul& n ) 
    {
        if (is_suitable_expression(n))
        {
            return 0;
        }

        int lhs_mod = walk( n.get_lhs( ) );
        int rhs_mod = walk( n.get_rhs( ) );

        if( ( lhs_mod >= 0 ) && ( rhs_mod >= 0 ) )
            return lhs_mod * rhs_mod;

        return -1;
    }

    int SuitableAlignmentVisitor::visit( const Nodecl::IntegerLiteral& n )
    {
        return const_value_cast_to_signed_int( n.get_constant( )) * _type_size;
    }
    
    int SuitableAlignmentVisitor::visit( const Nodecl::Conversion& n ) 
    {
        if (is_suitable_expression(n))
        {
            return 0;
        }

        return walk(n.get_nest());
    }

    int SuitableAlignmentVisitor::visit( const Nodecl::ParenthesizedExpression& n ) 
    {
        if (is_suitable_expression(n))
        {
            return 0;
        }

        return walk(n.get_nest());
    }

    int SuitableAlignmentVisitor::visit( const Nodecl::Symbol& n ) 
    {
        if (is_suitable_expression(n))
        {
            return 0;
        }
        else if( n.is_constant( ) )
        {
            return const_value_cast_to_signed_int( n.get_constant( )) * _type_size;
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
                    return (const_value_cast_to_signed_int( lb.get_constant( ) ) 
                                  + ( const_value_cast_to_signed_int( incr.get_constant( ) ) 
                                      * _unroll_factor)) * _type_size;
                }
            }
        }
        
        return -1;
    }

    int SuitableAlignmentVisitor::unhandled_node(const Nodecl::NodeclBase& n) 
    {
        std::cerr << "Suitable Alignment Visitor: Unknown node "
            << ast_print_node_type(n.get_kind())
            << " at " << n.get_locus_str()
            << std::endl;

        return Ret();
    }


    // ********************** END visitor retrieving suitable simd alignment *********************** //
    // ********************************************************************************************* //
    
    
    
    // ********************************************************************************************* //
    // ******************* Visitor retrieving array accesses info within a loop ******************** //
    
    ArrayAccessInfoVisitor::ArrayAccessInfoVisitor( ObjectList<Analysis::Utils::InductionVariableData*> ivs, 
                                                  Utils::ext_sym_set killed )
            : _induction_variables( ivs ), _killed( killed ), _ivs( ), _is_adjacent_access( false )
    {}
    
    bool ArrayAccessInfoVisitor::variable_is_iv( const Nodecl::NodeclBase& n )
    {
        bool is_iv = false;
        for( ObjectList<Utils::InductionVariableData*>::const_iterator it = _induction_variables.begin( );
             it != _induction_variables.end( ); ++it )
        {
            if( Nodecl::Utils::equal_nodecls( ( *it )->get_variable( ).get_nodecl( ), n, /* skip conversion nodes */ true ) )
            {
                _ivs.insert( *it );
                is_iv = true;
                break;
            }
        }
        return is_iv;
    }
    
    static bool nodecl_is_zero( const Nodecl::NodeclBase& n )
    {
        bool res = false;

        if( n.is<Nodecl::IntegerLiteral>( ) )
            res = const_value_is_zero( n.as<Nodecl::IntegerLiteral>( ).get_constant( ) );
        else if( n.is<Nodecl::FloatingLiteral>( ) )
            res = const_value_is_zero( n.as<Nodecl::FloatingLiteral>( ).get_constant( ) );
        
        return res;
    }
    
    static bool nodecl_is_one( const Nodecl::NodeclBase& n )
    {
        bool res = false;
        
        if( n.is<Nodecl::IntegerLiteral>( ) )
            res = const_value_is_one( n.as<Nodecl::IntegerLiteral>( ).get_constant( ) );
        else if( n.is<Nodecl::FloatingLiteral>( ) )
            res = const_value_is_one( n.as<Nodecl::FloatingLiteral>( ).get_constant( ) );
        
        return res;
    }
    
    bool ArrayAccessInfoVisitor::is_adjacent_access( )
    {
        return _is_adjacent_access;
    }
    
    bool ArrayAccessInfoVisitor::depends_on_induction_vars( )
    {
        return !_ivs.empty( );
    }
    
    bool ArrayAccessInfoVisitor::unhandled_node( const Nodecl::NodeclBase& n )
    {
        std::cerr << "Unhandled node while parsing Array Subscript '"
                  << codegen_to_str( n.get_internal_nodecl( ),
                                    nodecl_retrieve_context( n.get_internal_nodecl( ) ) )
                  << "' of type '" << ast_print_node_type( n.get_kind( ) ) << "'" << std::endl;
        return false;
    }
    
    bool ArrayAccessInfoVisitor::join_list( ObjectList<bool>& list )
    {
        _is_adjacent_access = false;
        
        bool result = true;
        for( ObjectList<bool>::iterator it = list.begin( ); it != list.end( ); ++it )
        {
            result = result && ( *it );
        }
        return result;
    }
    
    bool ArrayAccessInfoVisitor::visit( const Nodecl::Add& n )
    {
        // Gather LHS info
        Nodecl::NodeclBase lhs = n.get_lhs( );
        bool lhs_is_const = walk( lhs );
        bool lhs_is_zero = false;
        if( lhs_is_const )
            lhs_is_zero = nodecl_is_zero( lhs );
        bool lhs_is_adjacent_access = _is_adjacent_access;
        
        // Gather RHS info
        Nodecl::NodeclBase rhs = n.get_rhs( );
        bool rhs_is_const = walk( rhs );
        bool rhs_is_zero = false;
        if( rhs_is_const )
            rhs_is_zero = nodecl_is_zero( rhs );
        bool rhs_is_adjacent_access = _is_adjacent_access;
        
        // Compute adjacency info
        _is_adjacent_access = ( ( lhs_is_adjacent_access && rhs_is_const && rhs_is_zero )
                              || ( lhs_is_const && lhs_is_zero && rhs_is_adjacent_access ) );
        
        return ( rhs_is_const && lhs_is_const );
    }
    
    bool ArrayAccessInfoVisitor::visit( const Nodecl::ArraySubscript& n )
    {
        // Collect information about the induction variables contained in the node
        bool n_is_iv = variable_is_iv( n );
        walk( n.get_subscripted( ) );
        walk( n.get_subscripts( ) );
        
        _is_adjacent_access = ( n_is_iv && _ivs.back( )->is_increment_one( ) );
        
        return !Utils::ext_sym_set_contains_nodecl( n, _killed );
    }
    
    bool ArrayAccessInfoVisitor::visit( const Nodecl::BooleanLiteral& n )
    {
        return true;
    }
    
    bool ArrayAccessInfoVisitor::visit( const Nodecl::Cast& n )
    {
        return walk( n.get_rhs( ) );
    }
    
    bool ArrayAccessInfoVisitor::visit( const Nodecl::ComplexLiteral& n )
    {
        return true;
    }
    
    bool ArrayAccessInfoVisitor::visit( const Nodecl::Conversion& n )
    {
        return walk( n.get_nest( ) );
    }

    bool ArrayAccessInfoVisitor::visit( const Nodecl::Div& n )
    {
        // Gather LHS info
        Nodecl::NodeclBase lhs = n.get_lhs( );
        bool lhs_is_const = walk( lhs );
        bool lhs_is_adjacent_access = _is_adjacent_access;
        
        // Gather RHS info
        Nodecl::NodeclBase rhs = n.get_rhs( );
        bool rhs_is_const = walk( rhs );
        bool rhs_is_one = false;
        if( rhs_is_const )
            rhs_is_one = nodecl_is_one( rhs );
        
        // Compute adjacency info
        _is_adjacent_access = lhs_is_adjacent_access && rhs_is_const && rhs_is_one;
        
        return ( lhs_is_const && rhs_is_const );
    }
    
    bool ArrayAccessInfoVisitor::visit( const Nodecl::FloatingLiteral& n )
    {
        return true;
    }
    
    bool ArrayAccessInfoVisitor::visit( const Nodecl::FunctionCall& n )
    {
        // Traverse arguments to find induction variables
        walk( n.get_arguments( ) );
        
        _is_adjacent_access = false;    // Reset this value
        
        return false; // Conservatively assume the result of the function call is not constant
    }
    
    bool ArrayAccessInfoVisitor::visit( const Nodecl::IntegerLiteral& n )
    {
        return true;
    }
    
    bool ArrayAccessInfoVisitor::visit( const Nodecl::Minus& n )
    {
        // Gather LHS info
        Nodecl::NodeclBase lhs = n.get_lhs( );
        bool lhs_is_const = walk( lhs );
        bool lhs_is_zero = false;
        if( lhs_is_const )
            lhs_is_zero = nodecl_is_zero( lhs );
        bool lhs_is_adjacent_access = _is_adjacent_access;
        
        // Gather RHS info
        Nodecl::NodeclBase rhs = n.get_rhs( );
        bool rhs_is_const = walk( rhs );
        bool rhs_is_zero = false;
        if( rhs_is_const )
            rhs_is_zero = nodecl_is_zero( rhs );
        bool rhs_is_adjacent_access = _is_adjacent_access;
        
        // Compute adjacency info
        _is_adjacent_access = ( ( lhs_is_adjacent_access && rhs_is_const && lhs_is_zero )
                              || ( lhs_is_const && rhs_is_zero && rhs_is_adjacent_access ) );
        
        return ( rhs_is_const && lhs_is_const );
    }
    
    bool ArrayAccessInfoVisitor::visit( const Nodecl::Mul& n )
    {
        // Gather LHS info
        Nodecl::NodeclBase lhs = n.get_lhs( );
        bool lhs_is_const = walk( lhs );
        bool lhs_is_one = false;
        if( lhs_is_const )
            lhs_is_one = nodecl_is_one( lhs );
        bool lhs_is_adjacent_access = _is_adjacent_access;
        
        // Gather RHS info
        Nodecl::NodeclBase rhs = n.get_rhs( );
        bool rhs_is_const = walk( rhs );
        bool rhs_is_one = false;
        if( rhs_is_const )
            rhs_is_one = nodecl_is_one( rhs );
        bool rhs_is_adjacent_access = _is_adjacent_access;
        
        // Compute adjacency info
        _is_adjacent_access = ( lhs_is_adjacent_access && rhs_is_const && rhs_is_one ) 
                              || ( rhs_is_adjacent_access && lhs_is_const && lhs_is_one );
        
        return ( lhs_is_const && rhs_is_const );
    }
    
    bool ArrayAccessInfoVisitor::visit( const Nodecl::Neg& n )
    {
        return walk( n.get_rhs( ) );
    }
    
    bool ArrayAccessInfoVisitor::visit( const Nodecl::PointerToMember& n )
    {
        // Collect information about the induction variables contained in the node
        bool n_is_iv = variable_is_iv( n );
        
        _is_adjacent_access = ( n_is_iv && _ivs.back( )->is_increment_one( ) );
        
        return !Utils::ext_sym_set_contains_nodecl( n, _killed );
    }
    
    bool ArrayAccessInfoVisitor::visit( const Nodecl::Postdecrement& n )
    {
        // Gather information about induction variables
        walk( n.get_rhs( ) );
        
        _is_adjacent_access = false;
        
        return false;
    }
    
    bool ArrayAccessInfoVisitor::visit( const Nodecl::Postincrement& n )
    {
        // Gather information about induction variables
        walk( n.get_rhs( ) );
        
        _is_adjacent_access = false;
        
        return false;
    }
    
    bool ArrayAccessInfoVisitor::visit( const Nodecl::Power& n )
    {
        // Gather LHS info
        Nodecl::NodeclBase lhs = n.get_lhs( );
        bool lhs_is_const = walk( lhs );
        bool lhs_is_adjacent_access = _is_adjacent_access;
        
        // Gather RHS info
        Nodecl::NodeclBase rhs = n.get_rhs( );
        bool rhs_is_const = walk( rhs );
        bool rhs_is_one = false;
        if( rhs_is_const )
            rhs_is_one = nodecl_is_one( rhs );
        
        // Compute adjacency info
        _is_adjacent_access = lhs_is_adjacent_access && rhs_is_const && rhs_is_one;
        
        return ( lhs_is_const && rhs_is_const );
    }
    
    bool ArrayAccessInfoVisitor::visit( const Nodecl::Predecrement& n )
    {
        walk( n.get_rhs( ) );
        
        _is_adjacent_access = false;
        
        return false;
    }
    
    bool ArrayAccessInfoVisitor::visit( const Nodecl::Preincrement& n )
    {
        walk( n.get_rhs( ) );
        
        _is_adjacent_access = false;
        
        return false;
    }
    
    bool ArrayAccessInfoVisitor::visit( const Nodecl::Reference& n )
    {
        return walk( n.get_rhs( ) );
    }
    
    bool ArrayAccessInfoVisitor::visit( const Nodecl::Sizeof& n )
    {
        bool n_is_const = walk( n.get_expr( ) );
        
        _is_adjacent_access = false;
        
        return n_is_const;
    }
    
    bool ArrayAccessInfoVisitor::visit( const Nodecl::StringLiteral& n )
    {
        return true;
    }
    
    bool ArrayAccessInfoVisitor::visit( const Nodecl::Symbol& n )
    {
        // Collect information about the induction variables contained in the node
        bool n_is_iv = variable_is_iv( n );

        _is_adjacent_access = ( n_is_iv && _ivs.back( )->is_increment_one( ) );
        
        return !Utils::ext_sym_set_contains_nodecl( n, _killed );
    }
    
    // ***************** END visitor retrieving array accesses info within a loop ****************** //
    // ********************************************************************************************* //

}
}
