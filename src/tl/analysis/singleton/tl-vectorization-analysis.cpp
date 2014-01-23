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

#include "tl-analysis-utils.hpp"
#include "tl-analysis-static-info.hpp"
#include <algorithm>

namespace TL  {
namespace Analysis {
    
    // ********************************************************************************************* //
    // ************** Class to retrieve SIMD analysis info about one specific nodecl *************** //
    
    bool NodeclStaticInfo::is_adjacent_access( const Nodecl::NodeclBase& n, 
                                               Node* scope_node, Node* n_node ) const
    {
        bool result = false;
        
        if( n.is<Nodecl::ArraySubscript>( ) )
        {
            result = true;

            Nodecl::List subscript = n.as<Nodecl::ArraySubscript>( ).get_subscripts( ).as<Nodecl::List>( );
            Nodecl::List::iterator it = subscript.begin( );
            for( ; it != subscript.end( ) - 1 && result; ++it )
            {   // All dimensions but the less significant must be constant
                if( !is_constant( *it ) )
                {
                    result = false;
                }
            }
            if( result )
            {   // The less significant dimension must be accessed by an (+/-)c +/- IV, where c is a constant
                // If the subscript is another ArraySubscript, then it is not adjacent
                if (it->is<Nodecl::ArraySubscript>())
                {
                    result = false;
                }
                else
                {
                    Nodecl::Utils::ReduceExpressionVisitor v;
                    Nodecl::NodeclBase s = it->shallow_copy( );
                    v.walk( s );
                    
                    ArrayAccessInfoVisitor iv_v( _induction_variables, _killed, scope_node, n_node );
                    iv_v.walk( s );
                    result = iv_v.is_adjacent_access( );
                }
            }
        }
        
        return result;
    }

    bool NodeclStaticInfo::contains_induction_variable( const Nodecl::NodeclBase& n, 
                                                        Node* scope_node, Node* n_node ) const
    {
        bool result = false;
        
        if( n.is<Nodecl::ArraySubscript>( ) )
        {
            Nodecl::Utils::ReduceExpressionVisitor v;
            Nodecl::NodeclBase s = n.shallow_copy( );
            v.walk( s );

            ArrayAccessInfoVisitor iv_v( _induction_variables, _killed, scope_node, n_node );
            iv_v.walk( s );
            result = iv_v.depends_on_induction_vars( );
        }
        else
        {
            // TODO: Write an appropriate message such us unsupported case.
            std::cerr << "warning: returning false '" 
                      << n.prettyprint( ) << "' which is not an array subscript" << std::endl;
        }
        
        return result;
    }
    
    bool NodeclStaticInfo::var_is_iv_dependent_in_scope( const Nodecl::NodeclBase& n, 
                                                         Node* scope_node, Node* n_node ) const
    {
        bool result = false;
        if( n.is<Nodecl::ArraySubscript>( ) )
        {
            ArrayAccessInfoVisitor iv_v( _induction_variables, _killed, scope_node, n_node );
            ObjectList<Nodecl::Symbol> syms = Nodecl::Utils::get_all_symbols_occurrences( n );
            for( ObjectList<Nodecl::Symbol>::iterator it = syms.begin( ); it != syms.end( ) && !result; ++it )
            {
                result = iv_v.var_is_iv_dependent_in_scope( *it );
            }
        }
        else
        {
            // TODO: Write an appropriate message such us unsupported case.
            std::cerr << "warning: returning false '" 
                      << n.prettyprint( ) << "' which is not an array subscript" << std::endl;
        }
        return result;
    }
    
    bool NodeclStaticInfo::is_constant_access( const Nodecl::NodeclBase& n ) const
    {
        bool result = true;
        
        if( n.is<Nodecl::ArraySubscript>( ) )
        {
            Nodecl::ArraySubscript array = n.as<Nodecl::ArraySubscript>( );

            //Check subscripted
            if ( !is_constant( array.get_subscripted( ) ))
            {
                return false;
            }

            // Check subscrips
            Nodecl::List subscript = array.get_subscripts( ).as<Nodecl::List>( );
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
        
    bool NodeclStaticInfo::is_simd_aligned_access( const Nodecl::NodeclBase& n, 
            const TL::ObjectList<Nodecl::NodeclBase>* suitable_expressions, 
            int unroll_factor, int alignment ) const
    {
        if( !n.is<Nodecl::ArraySubscript>( ) )
        {
            std::cerr << "warning: returning false for is_simd_aligned_access when asking for nodecl '" 
                      << n.prettyprint( ) << "' which is not an array subscript" << std::endl;
            return false;
        }
        
        bool result = false;
        
        Nodecl::NodeclBase subscripted = n.as<Nodecl::ArraySubscript>( ).get_subscripted( );
        int type_size = subscripted.get_type().basic_type().get_size();

        SuitableAlignmentVisitor sa_v( _induction_variables, suitable_expressions, unroll_factor, type_size, alignment );
        int subscript_alignment = sa_v.walk( n );
        
        if( (subscript_alignment % alignment) == 0 )
            result = true;
        
        return result;
    }

    bool NodeclStaticInfo::is_suitable_expression( const Nodecl::NodeclBase& n, 
            const TL::ObjectList<Nodecl::NodeclBase>* suitable_expressions, 
            int unroll_factor, int alignment, int& vector_size_module ) const
    {
        bool result = false;
        int type_size = n.get_type().basic_type().get_size();

        SuitableAlignmentVisitor sa_v( _induction_variables, suitable_expressions, unroll_factor, type_size, alignment );
        int subscript_alignment = sa_v.walk( n );

        // Remove me!
        printf("SUBSCRIPT ALIGNMENT %d\n", subscript_alignment);
        printf("SUITABLE LIST: ");
        if (suitable_expressions != NULL)
        {
            for(TL::ObjectList<Nodecl::NodeclBase>::const_iterator it = suitable_expressions->begin();
                    it != suitable_expressions->end();
                    it ++)
            {
                printf("%s ", it->prettyprint().c_str());        
            }
            printf("\n");
        }
        // End Remove me!

        vector_size_module = ( ( subscript_alignment == -1 ) ? subscript_alignment : 
                                                               subscript_alignment % alignment );
        if( vector_size_module == 0 )
            result = true;

        return result;
    }

    // ************ END class to retrieve SIMD analysis info about one specific nodecl ************* //
    // ********************************************************************************************* //
    
    
    
    // ********************************************************************************************* //
    // ************************ Visitor retrieving suitable simd alignment ************************* //
    
    SuitableAlignmentVisitor::SuitableAlignmentVisitor( const ObjectList<Utils::InductionVariableData*> induction_variables,
                                                        const ObjectList<Nodecl::NodeclBase>* suitable_expressions, int unroll_factor, 
                                                        int type_size, int alignment )
        : _induction_variables( induction_variables ), _suitable_expressions( suitable_expressions ), 
          _unroll_factor( unroll_factor ), _type_size( type_size ), _alignment( alignment )
    {
        _nesting_level = 0;
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

    bool SuitableAlignmentVisitor::is_suitable_expression( Nodecl::NodeclBase n )
    {
        /*
        std::cerr << &n << " of " << n.prettyprint() << " and " << &(_suitable_expressions->front()) << " of " << _suitable_expressions->front().prettyprint() 
            << Nodecl::Utils::equal_nodecls(n, _suitable_expressions->front(), true) << std::endl;
        */
        bool result = true;
        if( ( _suitable_expressions == NULL ) || !Nodecl::Utils::list_contains_nodecl( *_suitable_expressions, n ) )
            result = false;
        return result;
    }

    bool SuitableAlignmentVisitor::is_suitable_constant( int n )
    {
        if ( (n % _alignment) == 0 )
            return true;
        else
            return false;
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
    
    int SuitableAlignmentVisitor::visit( const Nodecl::ArraySubscript& n ) 
    {
        if( _nesting_level == 0 )  // Target access
        {
            _nesting_level++;
            
            int i;
            int alignment = 0;
            
            Nodecl::NodeclBase subscripted = n.get_subscripted( );
            TL::Type element_type = subscripted.get_type( );
            // TODO: subscript is aligned
            
            Nodecl::List subscripts = n.get_subscripts( ).as<Nodecl::List>( );
            int num_subscripts = subscripts.size( );
            
            // Get dimension sizes
            int *dimension_sizes = (int *)malloc( ( num_subscripts-1 ) * sizeof( int ) );
            
            for( i = 0; i < (num_subscripts-1); i++ ) // Skip the first one. It does not have size
            {
                // Iterate on array subscript type
                if( element_type.is_array( ) )
                {
                    element_type = element_type.array_element( );
                }
                else if( element_type.is_pointer( ) )
                {
                    element_type = element_type.points_to( );
                }
                else
                {
                    WARNING_MESSAGE( "Array subscript does not have array type or pointer to array type", 0 );
                    return -1;
                }
                
                if( !element_type.array_has_size( ) )
                {
                    WARNING_MESSAGE( "Array type does not have size", 0 );
                    return -1;
                }
                
                // Compute dimension alignment 
                Nodecl::NodeclBase dimension_size_node = element_type.array_get_size( );

                // If VLA, get the actual size
                if(dimension_size_node.is<Nodecl::Symbol>() &&
                        dimension_size_node.get_symbol().is_saved_expression())
                {
                    dimension_size_node = dimension_size_node.get_symbol().get_value();
                }
               
                int dimension_size = -1;
                if( dimension_size_node.is_constant( ) )
                {
                    dimension_size = const_value_cast_to_signed_int( dimension_size_node.get_constant( ) );
                    
                    if( is_suitable_constant( dimension_size * _type_size ) )
                        dimension_size = 0;
                }
                // If dimension size is suitable
                else if( is_suitable_expression( dimension_size_node ) )
                {
                    dimension_size = 0;
                }
                if( VERBOSE )
                    printf( "Dim %d, size %d\n", i, dimension_size );
                
                dimension_sizes[i] = dimension_size;
            }
            
            int it_alignment = -1;
            Nodecl::List::iterator it = subscripts.begin( );
            // Multiply dimension sizes by indexes
            for( i=0; it != subscripts.end( ); i++ )
            {
                it_alignment = walk( *it );
                
                it++;
                if( it == subscripts.end( ) ) break; // Last dimmension does not have to be multiplied
                
                // a[i][j][k] -> i -> i*J*K
                for( int j = i; j < (num_subscripts-1); j++ )
                {
                    if( ( dimension_sizes[j] == 0 ) || ( it_alignment == 0 ) )
                    {
                        it_alignment = 0;
                    }
                    else if( ( dimension_sizes[j] < 0 ) || ( it_alignment < 0 ) )
                    {
                        it_alignment = -1;
                    }
                    else
                    {
                        it_alignment *= dimension_sizes[j];
                    }
                }
                
                if( it_alignment < 0 )
                {
                    return -1;
                }
                
                alignment += it_alignment;
            }
            
            if( it_alignment < 0 )
            {
                return -1;
            }

            // Add adjacent dimension
            alignment += it_alignment;
            
            free(dimension_sizes);
            
            _nesting_level--;
            
            return alignment;
        }
        // Nested array subscript
        else
        {
            if (is_suitable_expression(n))
            {
                return 0;
            }
            
            return -1;
        }
    }
    
    int SuitableAlignmentVisitor::visit( const Nodecl::BitwiseShl& n )
    {
        if (is_suitable_expression(n))
        {
            return 0;
        }
        
        int lhs_mod = walk( n.get_lhs( ) );
        int rhs_mod = walk( n.get_rhs( ) );
        
        // Something suitable multiplied by anything is suitable
        if (rhs_mod > 0)
        {
            // Because a << const is: a * (1 << const)
            if( (is_suitable_constant(lhs_mod)) || (is_suitable_constant(1 << rhs_mod) )) 
                return 0;
            else if( ( lhs_mod > 0 ) && ( rhs_mod > 0 ) )
                return lhs_mod << rhs_mod;
        }

        return -1;
    }
    
    int SuitableAlignmentVisitor::visit( const Nodecl::BitwiseShr& n )
    {
        if (is_suitable_expression(n))
        {
            return 0;
        }
        
        int lhs_mod = walk( n.get_lhs( ) );
        int rhs_mod = walk( n.get_rhs( ) );
        
        // Something suitable multiplied by anything is suitable
        if (rhs_mod > 0)
        {
            // Because a << const is: a / (1 << const)
            if( (is_suitable_constant(lhs_mod)) || (is_suitable_constant(1 << rhs_mod) )) 
                return 0;
            else if( ( lhs_mod > 0 ) && ( rhs_mod > 0 ) )
                return lhs_mod >> rhs_mod;
        }

        return -1;
    }
    
    int SuitableAlignmentVisitor::visit( const Nodecl::Conversion& n ) 
    {
        if (is_suitable_expression(n))
        {
            return 0;
        }
        
        return walk(n.get_nest());
    }
    
    int SuitableAlignmentVisitor::visit( const Nodecl::IntegerLiteral& n )
    {
        return const_value_cast_to_signed_int( n.get_constant( )) * _type_size;
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
        {
            int result = lhs_mod - rhs_mod;
            if (result < 0)
                result = _alignment + result;

            return result;
        }
        
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

       // Something suitable multiplied by anything is suitable
        if( (is_suitable_constant(lhs_mod)) || (is_suitable_constant(rhs_mod) )) 
            return 0;
        else if( ( lhs_mod > 0 ) && ( rhs_mod > 0 ) )
            return lhs_mod * rhs_mod;

        return -1;
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
            int value = const_value_cast_to_signed_int( n.get_constant( )) * _type_size;

            if(is_suitable_constant(value))
                return 0;
            else
                return value;
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
            else if ( is_suitable_expression( lb ) )
            {
                Nodecl::NodeclBase incr = iv->get_increment( ).shallow_copy( );
                v.walk( incr );
                if( incr.is_constant( ) )
                {
                    return ( ( 0 /* assuming lb = 0 since it's suitable */ ) 
                                  + ( const_value_cast_to_signed_int( incr.get_constant( ) ) 
                                      * _unroll_factor)) * _type_size;
                }
            }
        }

        return -1;
    }

    int SuitableAlignmentVisitor::unhandled_node(const Nodecl::NodeclBase& n) 
    {
        WARNING_MESSAGE( "Suitable Alignment Visitor: Unknown node '%s' at '%s'\n", 
                         ast_print_node_type( n.get_kind( ) ), n.get_locus_str( ).c_str( ) );
        return -1;
    }


    // ********************** END visitor retrieving suitable simd alignment *********************** //
    // ********************************************************************************************* //
    
    
    
    // ********************************************************************************************* //
    // ******************* Visitor retrieving array accesses info within a loop ******************** //
    
    ArrayAccessInfoVisitor::ArrayAccessInfoVisitor( ObjectList<Analysis::Utils::InductionVariableData*> ivs, 
                                                    Utils::ext_sym_set killed, Node* scope, Node* n_node )
            : _induction_variables( ivs ), _killed( killed ), 
              _scope_node( scope ), _n_node( n_node ), 
              _ivs( ), _is_adjacent_access( false )
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
    
    bool ArrayAccessInfoVisitor::node_uses_iv( Node* node )
    {
        bool result = false;
        for( ObjectList<Utils::InductionVariableData*>::const_iterator it = _induction_variables.begin( ); 
             it != _induction_variables.end( ) && !result; ++it )
        {
            if( node->uses_var( ( *it )->get_variable( ).get_nodecl( ) ) )
                result = true;
        }
        return result;
    }
    
    bool ArrayAccessInfoVisitor::node_stmts_depend_on_iv( Node* node, int recursion_level, 
                                                          std::map<Node*, std::set<int> >& visits, 
                                                          std::set<Nodecl::Symbol>& visited_syms )
    {
        bool result = false;
        ObjectList<Nodecl::NodeclBase> stmts = node->get_statements( );
        for( ObjectList<Nodecl::NodeclBase>::iterator it = stmts.begin( ); ( it != stmts.end( ) ) && !result; ++it )
        {
            ObjectList<Nodecl::Symbol> syms = Nodecl::Utils::get_all_symbols_occurrences( *it );
            for( ObjectList<Nodecl::Symbol>::iterator its = syms.begin( ); ( its != syms.end( ) ) && !result; ++its )
            {
                if( visited_syms.find( *its ) == visited_syms.end( ) )
                    result = result || var_is_iv_dependent_in_scope_rec( *its, node,
                                                                         recursion_level+1, visits, visited_syms );
            }
        }
        return result;
    }
    
    bool ArrayAccessInfoVisitor::definition_depends_on_iv( const Nodecl::NodeclBase& n, Node* node )
    {
        bool result;
        for( ObjectList<Utils::InductionVariableData*>::const_iterator it = _induction_variables.begin( ); 
             it != _induction_variables.end( ) && !result; ++it )
        {   // Check whether the expression used to modify it depends on an induction variable
            result = Nodecl::Utils::stmtexpr_contains_nodecl( n, ( *it )->get_variable( ).get_nodecl( ) );
        }
        if( !result )
        {
            Utils::ext_sym_map reaching_defs_in = node->get_reaching_definitions_in( );
            for( Utils::ext_sym_map::iterator it = reaching_defs_in.begin( ); it != reaching_defs_in.end( ) && !result; ++it )
            {
                if( Nodecl::Utils::stmtexpr_contains_nodecl( it->first.get_nodecl( ), n ) )
                {   // n has been defined previously
                    result = definition_depends_on_iv( it->second, node );
                }
            }
        }
        return result;
    }
    
    bool ArrayAccessInfoVisitor::var_is_iv_dependent_in_scope_rec( const Nodecl::Symbol& n, Node* current, 
                                                                   int recursion_level, std::map<Node*, std::set<int> >& visits, 
                                                                   std::set<Nodecl::Symbol>& visited_syms )
    {
        bool result = false;
        visited_syms.insert( n );
        if( current != _scope_node )
        {
            bool visit_node = false;
            // If the node has never been visited or, if it was visit, it was in a different recursion level
            if( visits.find( current ) == visits.end( ) )
            {
                int recursion_level_value[] = { recursion_level };
                visits[current] = std::set<int>( recursion_level_value, recursion_level_value + 1 );
                visit_node = true;
            }
            else if( visits.find( current )->second.find( recursion_level ) == visits.find( current )->second.end( ) )
            {
                visits.find( current )->second.insert( recursion_level );
                visit_node = true;
            }
            
            if( visit_node )
            {
            
                // Treat the current node
                Utils::ext_sym_set killed = current->get_killed_vars( );
                if( killed.find( n ) != killed.end( ) )
                {
                    if( current->is_graph_node( ) )
                    {   // The current graph node defined the symbol \n
                        // Treat the inner nodes of the current graph node
                        result = var_is_iv_dependent_in_scope_rec( n, current->get_graph_exit_node( ), 
                                                                   recursion_level, visits, visited_syms );
                        if( !result )
                        {   
                            Node* current_entry = current->get_graph_entry_node( );
                            if( current->is_ifelse_statement( ) || current->is_switch_statement( ) || current->is_while_loop( ) )
                            {   // Case 1.1: This checks situations such as:
                                // if(i%2==0)       -> where 'i' is an induction variable in 'scope'
                                //     n=...;
                                // switch(i)        -> where 'i' is an induction variable in 'scope'
                                // {case 0:n=...;}
                                // while( i )       -> where 'i' is an induction variable in 'scope'
                                // {n=...;}
                                Node* cond = current_entry->get_children( )[0];
                                result = node_uses_iv( cond ) || node_stmts_depend_on_iv( cond, recursion_level, visits, visited_syms );
                            }
                            else if( current->is_for_loop( ) )
                            {   // Case 1.2: 
                                // for(;...<i;)      -> where 'i' is an induction variable in 'scope'
                                //     n=...;
                                Node* cond = current_entry->get_children( )[0];
                                if( ( cond->get_children( ).size( ) == 2 ) && ( cond->get_parents( ).size( ) == 2 ) )
                                {   // Recheck whether this node is the condition of the loop, or the condition is empty
                                    result = node_uses_iv( cond ) || node_stmts_depend_on_iv( cond, recursion_level, visits, visited_syms );
                                }
                            }
                            else if( current->is_do_loop( ) )
                            {   // Case 1.2: 
                                // do {n=...;}
                                // while(i)          -> where 'i' is an induction variable in 'scope'
                                Node* cond = current->get_graph_exit_node( )->get_parents( )[0];
                                result = node_uses_iv( cond ) || node_stmts_depend_on_iv( cond, recursion_level, visits, visited_syms );
                            }
                        }
                    }
                    else
                    {   // Case 2: This checks situations such as:
                        // n=i;                      -> where 'i' is an induction variable in 'scope'
                        Utils::ext_sym_map reaching_defs_out = current->get_reaching_definitions_out( );
                        for( Utils::ext_sym_map::iterator it = reaching_defs_out.begin( ); 
                             it != reaching_defs_out.end( ) && !result; ++it )
                        {
                            if( Nodecl::Utils::stmtexpr_contains_nodecl( it->first.get_nodecl( ), n ) )
                            {   // 'n' is being modified
                                result = definition_depends_on_iv( it->second, current );
                            }
                        }
                    }
                }
                
                // Recursively treat the parents of the current node
                if( !result )
                {
                    ObjectList<Node*> parents;
                    if( current->is_entry_node( ) )
                        parents.append( current->get_outer_node( ) );
                    else
                        parents = current->get_parents( );
                    for( ObjectList<Node*>::iterator it = parents.begin( ); it != parents.end( ) && !result; ++it )
                        result = var_is_iv_dependent_in_scope_rec( n, *it, recursion_level, visits, visited_syms );
                }
            }
        }
        return result;
    }
    
    // Check whether the definition of 'n' depends on the value of the '_scope' induction variable
    bool ArrayAccessInfoVisitor::var_is_iv_dependent_in_scope( const Nodecl::Symbol& n )
    {   
        std::map<Node*, std::set<int> > visits;
        std::set<Nodecl::Symbol> visited_syms;
        bool result = var_is_iv_dependent_in_scope_rec( n, _n_node, 0, visits, visited_syms );
        ExtensibleGraph::clear_visits_backwards_in_level( _n_node, _scope_node );
        return result;
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
        WARNING_MESSAGE( "Unhandled node while parsing Array Subscript '%s' of type '%s'", 
                         n.prettyprint( ).c_str( ), ast_print_node_type( n.get_kind( ) ) );
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
        bool lhs_is_adjacent_access = _is_adjacent_access;
        
        // Gather RHS info
        Nodecl::NodeclBase rhs = n.get_rhs( );
        bool rhs_is_const = walk( rhs );
        bool rhs_is_adjacent_access = _is_adjacent_access;
        
        // Compute adjacency info
        _is_adjacent_access = ( lhs_is_adjacent_access && rhs_is_const )
                           || ( lhs_is_const && rhs_is_adjacent_access );
                           
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
    
    bool ArrayAccessInfoVisitor::visit( const Nodecl::BitwiseShl& n )
    {
        // Gather LHS info
        Nodecl::NodeclBase lhs = n.get_lhs( );
        bool lhs_is_const = walk( lhs );
        bool lhs_is_adjacent_access = _is_adjacent_access;

        // Gather RHS info
        Nodecl::NodeclBase rhs = n.get_rhs( );
        bool rhs_is_const = walk( rhs );
        bool rhs_is_zero = false;
        if( rhs_is_const )
            rhs_is_zero = nodecl_is_zero( rhs );

        // Compute adjacency info
        _is_adjacent_access = lhs_is_adjacent_access && rhs_is_zero;

        return ( lhs_is_const && rhs_is_const );
    }
    
    bool ArrayAccessInfoVisitor::visit( const Nodecl::BitwiseShr& n )
    {
        // Gather LHS info
        Nodecl::NodeclBase lhs = n.get_lhs( );
        bool lhs_is_const = walk( lhs );
        bool lhs_is_adjacent_access = _is_adjacent_access;
        
        // Gather RHS info
        Nodecl::NodeclBase rhs = n.get_rhs( );
        bool rhs_is_const = walk( rhs );
        bool rhs_is_zero = false;
        if( rhs_is_const )
            rhs_is_zero = nodecl_is_zero( rhs );
        
        // Compute adjacency info
        _is_adjacent_access = lhs_is_adjacent_access && rhs_is_zero;
        
        return ( lhs_is_const && rhs_is_const );
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
        _is_adjacent_access = lhs_is_adjacent_access && rhs_is_one;
 
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
    
    bool ArrayAccessInfoVisitor::visit( const Nodecl::MaskLiteral& n )
    {
        return true;
    }
    
    bool ArrayAccessInfoVisitor::visit( const Nodecl::Minus& n )
    {
        // Gather LHS info
        Nodecl::NodeclBase lhs = n.get_lhs( );
        bool lhs_is_const = walk( lhs );
        bool lhs_is_adjacent_access = _is_adjacent_access;
        
        // Gather RHS info
        Nodecl::NodeclBase rhs = n.get_rhs( );
        bool rhs_is_const = walk( rhs );
        bool rhs_is_adjacent_access = _is_adjacent_access;
        
        // Compute adjacency info
        _is_adjacent_access = ( lhs_is_adjacent_access && rhs_is_const )
                           || ( lhs_is_const && rhs_is_adjacent_access );
        
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
        _is_adjacent_access = ( lhs_is_adjacent_access && rhs_is_one ) 
                           || ( rhs_is_adjacent_access && lhs_is_one );
        
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
        return !Utils::ext_sym_set_contains_nodecl( n, _killed ) || !var_is_iv_dependent_in_scope( n );
    }
    
    // ***************** END visitor retrieving array accesses info within a loop ****************** //
    // ********************************************************************************************* //

}
}
