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

#include "tl-range-analysis.hpp"

#include <algorithm>

namespace TL {
namespace Analysis {
    
namespace
{
    // TODO Complete this node, some cases are not properly handled
    void combine_ranges( ObjectList<Utils::RangeValue_tag>& original, 
                                ObjectList<Utils::RangeValue_tag> new_values )
    {
        ObjectList<Utils::RangeValue_tag> elems_to_add;
        ObjectList<ObjectList<Utils::RangeValue_tag>::iterator> elems_to_erase;
        for( ObjectList<Utils::RangeValue_tag>::iterator it = new_values.begin( ); it != new_values.end( ); ++it )
        {
            if( !it->n->is_null( ) )
            {   // Element is a Nodecl::NodeclBase*
                bool needs_insertion = true;        // Indicates whether we need to add *it to the original list
                for( ObjectList<Utils::RangeValue_tag>::iterator ito = original.begin( ); ito != original.end( ); ++ito )
                {
                    if( !ito->n->is_null( ) )
                    {
                        if( Nodecl::Utils::equal_nodecls( *it->n, *ito->n ) )
                        {
                            needs_insertion = false;
                            break;
                        }
                        else if( Nodecl::Utils::nodecl_contains_nodecl( *it->n, *ito->n ) )
                        {
                            needs_insertion = false;
                            elems_to_erase.append( ito );
                            elems_to_add.append( *it );
                            break;
                        }
                    }
                    else
                    {
                        if( Nodecl::Utils::equal_nodecls( ito->iv->get_variable( ).get_nodecl( ), *ito->n ) )
                        {
                            WARNING_MESSAGE( "Combining ranges between an InductionVariableData and a Nodecl is not yet supported.\n" 
                                             "Assuming values not overlapped", 0 );
                        }
                    }
                }
                if( needs_insertion )
                    elems_to_add.append( *it );
            }
            else
            {   // Element is an InductionVariableData*
                bool needs_insertion = true;        // Indicates whether we need to add *it to the original list
                for( ObjectList<Utils::RangeValue_tag>::iterator ito = original.begin( ); ito != original.end( ); ++ito )
                {
                    if( !ito->n->is_null( ) )
                    {
                        if( Nodecl::Utils::equal_nodecls( ito->iv->get_variable( ).get_nodecl( ), *ito->n ) )
                        {
                            WARNING_MESSAGE( "Combining ranges between an InductionVariableData and a Nodecl is not yet supported.\n" 
                                             "Assuming values do not overlap", 0 );
                        }
                    }
                    else
                    {
                        if( Nodecl::Utils::equal_nodecls( it->iv->get_variable( ).get_nodecl( ), 
                            ito->iv->get_variable( ).get_nodecl( ), /*skip conversion nodes*/ true ) && 
                            Nodecl::Utils::equal_nodecls( it->iv->get_lb( ), ito->iv->get_lb( ), /*skip conversion nodes*/ true ) && 
                            Nodecl::Utils::equal_nodecls( it->iv->get_ub( ), ito->iv->get_ub( ), /*skip conversion nodes*/ true ) && 
                            Nodecl::Utils::equal_nodecls( it->iv->get_increment( ), ito->iv->get_increment( ), /*skip conversion nodes*/ true ) && 
                            ( it->iv->is_basic( ) == ito->iv->is_basic( ) ) )
                        {
                            needs_insertion = false;
                        }
                        else
                        {
                            WARNING_MESSAGE( "Combining ranges between two InductionVariableData is not yet supported.\n" 
                                             "Assuming values do not overlap", 0 );
                        }
                    }
                }
                if( needs_insertion )
                    elems_to_add.append( *it );
            }
        }
        
        // Erase the elements of the original list that we don't want anymore
        for( ObjectList<ObjectList<Utils::RangeValue_tag>::iterator>::iterator it = elems_to_erase.begin( ); 
            it != elems_to_erase.end( ); ++it )
            {
                original.erase( *it );
            }
            // Add the new elements to the original list
            for( ObjectList<Utils::RangeValue_tag>::iterator it = elems_to_add.begin( ); it != elems_to_add.end( ); ++it )
                original.append( *it );
    }
    
    bool equal_maps( const Utils::RangeValuesMap& m1, const Utils::RangeValuesMap& m2 )
    {
        // No predicate needed because there is operator== for pairs already.
        return ( ( m1.size( ) == m2.size( ) )
        && std::equal( m1.begin( ), m1.end( ), m2.begin( ), Utils::map_pair_compare ) );
    }
}
    
    
    // **************************************************************************************************** //
    // ******************************** Class implementing range analysis ********************************* //
    
    RangeAnalysis::RangeAnalysis( ExtensibleGraph* graph )
        : _graph( graph )
    {}
    
    void RangeAnalysis::compute_range_analysis( )
    {
        bool changed = true;
        Node* entry = _graph->get_graph( )->get_graph_entry_node( );
        
        // Initialize range values with information about the reaching definitions out and killed variables
        initialize_range_values( entry );
        ExtensibleGraph::clear_visits( entry );
        
        // Iterate over the graph until no change is performed
//         while( changed )
//         {
//             changed = false;
//             compute_range_analysis_rec( entry, changed );
//             ExtensibleGraph::clear_visits( entry );
//         }
    }
    
    void RangeAnalysis::initialize_range_values( Node* current )
    {
        if( !current->is_visited( ) )
        {
            current->set_visited( true );
            
            if( current->is_graph_node( ) )
            {
                initialize_range_values( current->get_graph_entry_node( ) );
            }
            else
            {
                // Compute input ranges for the current node
                Utils::ext_sym_map reaching_defs_in = current->get_reaching_definitions_in( );
                for( Utils::ext_sym_map::iterator it = reaching_defs_in.begin( ); it != reaching_defs_in.end( ); )
                {
                    Nodecl::NodeclBase var = it->first.get_nodecl( );
                    ObjectList<Utils::RangeValue_tag> values;
                    while( Nodecl::Utils::equal_nodecls( var, it->first.get_nodecl( ) ) )
                    {
                        Nodecl::NodeclBase* value = new Nodecl::NodeclBase( it->second.get_internal_nodecl( ) );
                        Utils::RangeValue_tag rv; rv.n = value;
                        values.append( rv );
                        ++it;
                    }
                    current->set_range_in( var, values );
                }
                // Compute output ranges for the current node
                Utils::ext_sym_map reaching_defs_out = current->get_reaching_definitions_out( );
                DefinitionsPropagationVisitor dpv( reaching_defs_out );
                for( Utils::ext_sym_map::iterator it = reaching_defs_out.begin( ); it != reaching_defs_out.end( ); )
                {
                    Nodecl::NodeclBase var = it->first.get_nodecl( );
                    ObjectList<Utils::RangeValue_tag> values;
                    while( Nodecl::Utils::equal_nodecls( var, it->first.get_nodecl( ) ) )
                    {
                        Nodecl::NodeclBase* value = new Nodecl::NodeclBase( it->second.get_internal_nodecl( ) );
//                         dpv.walk( *value );
                        Utils::RangeValue_tag rv; rv.n = value;
                        values.append( rv );
                        ++it;
                    }
                    if( !values.empty( ) )
                    {
                        Utils::RangeValue_tag rv = values[0];
                        rv;
                    }
                    current->set_range_out( var, values );
                }
            }
            
            ObjectList<Node*> children = current->get_children( );
            for( ObjectList<Node*>::iterator it = children.begin( ); it != children.end( ); ++it )
            {
                initialize_range_values( *it );
            }
        }
    }
    
    void RangeAnalysis::compute_node_range_analysis( Node* node, bool& changed )
    {
        // Get parents ranges
        Utils::RangeValuesMap parents_range_values;
        ObjectList<Node*> parents = node->get_parents( );
        for( ObjectList<Node*>::iterator it = parents.begin( ); it != parents.end( ); ++it )
        {
            Utils::RangeValuesMap parent_range_out = ( *it )->get_ranges_out( );
            for( Utils::RangeValuesMap::iterator itr = parent_range_out.begin( ); itr != parent_range_out.end( ); ++itr )
            {
                if( parents_range_values.find( itr->first ) == parents_range_values.end( ) )
                {   // Parent value has not yet been added
                    parents_range_values.insert( Utils::RangeValuesMapEntry( itr->first, itr->second ) );
                }
                else
                {   // Combine results
                    combine_ranges( parents_range_values[itr->first], itr->second );
                }
            }
        }
        
        // Combine parents ranges with current range
        Utils::RangeValuesMap old_node_range_values = node->get_ranges_in( );
        Utils::RangeValuesMap new_node_range_values;
        Utils::ext_sym_map out_reach_defs = node->get_reaching_definitions_out( );
        for( Utils::ext_sym_map::iterator it = out_reach_defs.begin( ); it != out_reach_defs.end( ); )
        {
            Nodecl::NodeclBase reaching_def_var = it->first.get_nodecl( );
            if( parents_range_values.find( reaching_def_var ) != parents_range_values.end( ) )
            {
                Utils::RangeValue_tag rv; rv.n = &it->second;
                ObjectList<Utils::RangeValue_tag> values( 1, rv );
                if( it->second.is_constant( ) )
                {   // Set this new value, since the other are not available anymore
                    node->set_range_in( reaching_def_var, values );
                }
                else
                {   // Check whether the value is computed depending on ranges that are already computed
                    // TODO
                    WARNING_MESSAGE( "Combining parents ranges with current non-constant value is not yet supported\n" 
                                     "Assuming current value", 0 );
                    node->set_range_in( reaching_def_var, values );
                }
            }
            else
            {
                std::pair<Utils::ext_sym_map::iterator, Utils::ext_sym_map::iterator> var_reach_defs = 
                        out_reach_defs.equal_range( it->first );
                ObjectList<Utils::RangeValue_tag> range_values;
                while( var_reach_defs.first != var_reach_defs.second )
                {
                    Utils::RangeValue_tag rv; rv.n = &var_reach_defs.first->second;
                    range_values.append( rv );
                    ++var_reach_defs.first;
                }
                new_node_range_values.insert( Utils::RangeValuesMapEntry( reaching_def_var, range_values ) );
            }
            
            // scape all reaching definitions corresponding to the same variable
            ++it;
            while( Nodecl::Utils::equal_nodecls( it->first.get_nodecl( ), reaching_def_var ) )
                ++it;
        }
        
        if( !equal_maps( new_node_range_values, old_node_range_values ) )
            changed = true;
    }
    
    void RangeAnalysis::compute_range_analysis_rec( Node* current, bool& changed )
    {
        if( !current->is_visited( ) )
        {
            current->set_visited( true );
            
            // TODO
            if( current->is_graph_node( ) )
            {
                if( current->is_loop_node( ) )
                {   // Ranges are different depending on the node
                    
                }
                
                compute_range_analysis_rec( current->get_graph_entry_node( ), changed );
            }
            else
            {
                compute_node_range_analysis( current, changed );
            }
            
            ObjectList<Node*> children = current->get_children( );
            for( ObjectList<Node*>::iterator it = children.begin( ); it != children.end( ); ++it )
            {
                compute_range_analysis_rec( *it, changed );
            }
        }
    }
    
    // ****************************** End class implementing range analysis ******************************* //
    // **************************************************************************************************** //
    
    
    
    // **************************************************************************************************** //
    // *************** Class implementing reaching definitions substitution and propagation *************** //
    
    DefinitionsPropagationVisitor::DefinitionsPropagationVisitor( Utils::ext_sym_map reaching_defs )
        : _reaching_definitions( reaching_defs )
    {}
    
    void DefinitionsPropagationVisitor::visit( const Nodecl::ArraySubscript& n )
    {
        if( _reaching_definitions.find( n ) != _reaching_definitions.end( ) ) {
            std::cerr << "Replacing reach defs:  " << n.prettyprint( ) << "  ->  " 
                      << _reaching_definitions.find( n )->second.prettyprint( ) << std::endl;
            Nodecl::Utils::replace( n, _reaching_definitions.find( n )->second );
        }
    }
    
    void DefinitionsPropagationVisitor::visit( const Nodecl::ClassMemberAccess& n )
    {
        if( _reaching_definitions.find( n ) != _reaching_definitions.end( ) ) {
            std::cerr << "Replacing reach defs:  " << n.prettyprint( ) << "  ->  " 
                      << _reaching_definitions.find( n )->second.prettyprint( ) << std::endl;
            Nodecl::Utils::replace( n, _reaching_definitions.find( n )->second );
        }
    }
    
    void DefinitionsPropagationVisitor::visit( const Nodecl::Symbol& n )
    {
        if( _reaching_definitions.find( n ) != _reaching_definitions.end( ) ) {
            std::cerr << "Replacing reach defs:  " << n.prettyprint( ) << "  ->  " 
                      << _reaching_definitions.find( n )->second.prettyprint( ) << std::endl;
            Nodecl::Utils::replace( n, _reaching_definitions.find( n )->second );
        }
    }
    
    // ************* END class implementing reaching definitions substitution and propagation ************* //
    // **************************************************************************************************** //
    
}
}