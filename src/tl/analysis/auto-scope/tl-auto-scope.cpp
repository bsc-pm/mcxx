/*--------------------------------------------------------------------
 ( C) Copyright 2006-2012 Barcelona* Supercomputing Center             **
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
#include "tl-auto-scope.hpp"

namespace TL {
namespace Analysis {

namespace {
    
    bool sync_in_all_branches( Node* current, Node* original )
    {
        bool res = false;
        
        if( !current->is_visited_aux( ) )
        {
            current->set_visited_aux( true );
            
            if( !current->is_exit_node( ) )
            {
                if( current->is_graph_node( ) )
                {
                    if( current->is_ifelse_statement( ) || current->is_switch_statement( ) )
                    {
                        Node* condition = current->get_graph_entry_node( )->get_children( )[0];
                        ObjectList<Node*> children = condition->get_children( );
                        bool partial_res = true;
                        for( ObjectList<Node*>::iterator it = children.begin( ); it != children.end( ) && partial_res; ++it )
                        {
                            partial_res = partial_res && sync_in_all_branches( *it, original );
                        }
                        res = partial_res;
                    }
                    else if( current->is_omp_barrier_graph_node( ) )
                    {
                        res = true;
                    }
                    else
                    {
                        res = sync_in_all_branches( current->get_graph_entry_node( ), original );
                    }
                }
                else if( current->is_omp_taskwait_node( ) )
                {
                    res = true;
                }
                
                // If we are navigating inside a graph node
                if( !res && ( current != original ) )
                {
                    ObjectList<Node*> children = current->get_children( );
                    ERROR_CONDITION( children.size( ) != 1, 
                                     "PCFG non-conditional nodes other than a graph exit node, are expected to have one child.\n"\
                                     "Node '%d' has '%d' children.\n", current->get_id( ), children.size( ) );
                    bool partial_res = true;
                    for( ObjectList<Node*>::iterator it = children.begin( ); it != children.end( ) && partial_res; ++it )
                    {
                        partial_res = partial_res && sync_in_all_branches( *it, original );
                    }
                    res = partial_res;
                }
            }
        }
        
        return res;
    }
    
    void collect_tasks_between_nodes( Node* current, Node* last, Node* skip, ObjectList<Node*>& result )
    {
        if( !current->is_visited( ) && ( current != last ) )
        {
            current->set_visited( true );
            
            if( current->is_exit_node( ) )
                return;
            
            if( current->is_graph_node( ) )
            {
                // Add inner tasks recursively, if exist
                collect_tasks_between_nodes( current->get_graph_entry_node( ), last, skip, result );
                
                // Add current node if it is a task
                if( current->is_omp_task_node( ) && ( current != skip ) )
                    result.insert( current );
            }
            
            ObjectList<Node*> children = current->get_children( );
            for( ObjectList<Node*>::iterator it = children.begin( ); it != children.end( ); ++it )
            {
                collect_tasks_between_nodes( *it, last, skip, result );
            }
        }
    }
    
    bool task_in_loop_is_synchronized_within_loop( Node* task )
    {
        bool res = false;
        
        Node* task_sync = task->get_children( )[0];
        if( !task_sync->is_omp_virtual_tasksync( ) )
        {
            Node* task_outer = task->get_outer_node( );
            
            while( ( task_outer != NULL ) && !res )
            {
                // Get the next loop were the task is nested
                while( !task_outer->is_loop_node( ) && ( task_outer != NULL ) )
                    task_outer = task_outer->get_outer_node( );
                
                if( ( task_outer != NULL ) && ExtensibleGraph::node_contains_node( task_outer, task_sync ) )
                    res = true;
            }
        }
        
        return res;
    }
    
    void collect_previous_tasks_synchronized_after_scheduling_point( Node* task, ObjectList<Node*> currents, ObjectList<Node*>& result )
    {
        for( ObjectList<Node*>::iterator it = currents.begin( ); it != currents.end( ); ++it )
        {
            if( !( *it )->is_visited_aux( ) )
            {
                ( *it )->set_visited_aux( true );
                
                if( ( *it )->is_omp_task_node( ) )
                {
                    Node* it_sync = ( *it )->get_children( )[0];
                    if( ExtensibleGraph::node_is_ancestor_of_node( task, it_sync ) )
                    {
                        result.insert( *it );
                    }
                }
            }
        }
    }
    
    Utils::UsageKind compute_usage_in_region_rec( Node* current, Nodecl::NodeclBase ei_nodecl, Node* region )
    {
        Utils::UsageKind result( Utils::UsageKind::NONE );
        
        if( !current->is_visited_aux( ) && ExtensibleGraph::node_contains_node( region, current ) )
        {
            current->set_visited_aux( true );
            
            if( !current->is_exit_node( ) )
            {
                if( current->is_graph_node( ) )
                {
                    result = compute_usage_in_region_rec( current->get_graph_entry_node( ), ei_nodecl, region );
                }
                else
                {
                    Utils::ext_sym_set undef = current->get_undefined_behaviour_vars( );
                    if( Utils::ext_sym_set_contains_nodecl( ei_nodecl, undef ) )
                        result = Utils::UsageKind::UNDEFINED;
                    Utils::ext_sym_set ue = current->get_ue_vars( );
                    if( Utils::ext_sym_set_contains_nodecl( ei_nodecl, ue ) )
                        result = Utils::UsageKind::USED;
                    Utils::ext_sym_set killed = current->get_killed_vars( );
                    if( Utils::ext_sym_set_contains_nodecl( ei_nodecl, killed ) )
                        result = Utils::UsageKind::DEFINED;
                }
                
                if( result._usage_type & Utils::UsageKind::UNDEFINED )
                {}   // Nothing else to be done because we will not be able to say anything about this variable
                else
                {
                    ObjectList<Node*> children = current->get_children( );
                    for( ObjectList<Node*>::iterator it = children.begin( ); it != children.end( ); ++it )
                    {
                        result = result | compute_usage_in_region_rec( *it, ei_nodecl, region );
                    }
                }
            }
        }
        
        return result;
    }
    
    Utils::UsageKind compute_usage_in_region( Utils::ExtendedSymbol ei, Node* region )
    {
        Node* region_entry = region->get_graph_entry_node( );
        Utils::UsageKind result = compute_usage_in_region_rec( region_entry, ei.get_nodecl( ), region );
        ExtensibleGraph::clear_visits_aux_in_level( region_entry, region );
        return result;
    }
    
    Utils::UsageKind compute_usage_in_regions( Utils::ExtendedSymbol ei, ObjectList<Node*> regions )
    {
        Utils::UsageKind result = Utils::UsageKind::NONE;
        
        for( ObjectList<Node*>::iterator it = regions.begin( ); it != regions.end( ); it++ )
            result = result | compute_usage_in_region( ei, *it );
        
        return result;
    }
    
    bool access_are_synchronous_rec( Node* current, Nodecl::NodeclBase ei_nodecl, Node* region )
    {
        bool result = true;
        
        if( !current->is_visited_aux( ) )
        {
            current->set_visited_aux( true );
            
            if( !current->is_exit_node( ) )
            {
                if( current->is_graph_node( ) )
                {
                    result = access_are_synchronous_rec( current->get_graph_entry_node( ), ei_nodecl, region );
                }
                else
                {
                    Utils::ext_sym_set ue_vars = current->get_ue_vars( );
                    Utils::ext_sym_set killed_vars = current->get_killed_vars( );
                    if( ( Utils::ext_sym_set_contains_nodecl( ei_nodecl, ue_vars ) || 
                          Utils::ext_sym_set_contains_nodecl( ei_nodecl, killed_vars ) ) &&
                        !ExtensibleGraph::node_is_in_synchronous_construct( current ) )
                    {
                        result = false;
                    }
                }
                
                if( result )
                {
                    ObjectList<Node*> children = current->get_children( );
                    for( ObjectList<Node*>::iterator it = children.begin( ); ( it != children.end( ) ) && result; it++ )
                        result = access_are_synchronous_rec( *it, ei_nodecl, region );
                }
            }
        }
        
        return result;
    }
    
    bool access_are_synchronous( Utils::ExtendedSymbol ei, Node* region )
    {
        Node* region_entry = region->get_graph_entry_node( );
        bool result = access_are_synchronous_rec( region_entry, ei.get_nodecl( ), region );
        ExtensibleGraph::clear_visits_aux_in_level( region_entry, region );
        return result;
    }
    
    bool access_are_synchronous( Utils::ExtendedSymbol ei, ObjectList<Node*> regions )
    {
        bool result = true;
        for( ObjectList<Node*>::iterator it = regions.begin( ); ( it != regions.end( ) ) && result; ++it )
            result = result && access_are_synchronous( ei, *it );
        return result;
    }
    
}
    
    AutoScoping::AutoScoping( ExtensibleGraph* pcfg )
        : _graph( pcfg ), _simultaneous_tasks( ), _check_only_local( false )
    {}
    
    void AutoScoping::compute_auto_scoping( )
    {
        ObjectList<Node*> tasks = _graph->get_tasks_list( );
        for( ObjectList<Node*>::iterator it = tasks.begin( ); it != tasks.end( ); ++it )
            compute_task_auto_scoping( *it );
    }

    void AutoScoping::compute_task_auto_scoping( Node* task )
    {
        _simultaneous_tasks = _graph->get_task_concurrent_tasks( task );
        
        ObjectList<Node*> last_sync = _graph->get_task_last_synchronization( task );
        Node* next_sync = _graph->get_task_next_synchronization( task );
        if( VERBOSE )
        {
            std::cerr << " Task concurrent regions limits: \n";
            if( last_sync.empty( ) )
                std::cerr << "    - Last sync not fund" << std::endl;
            else
            {
                std::cerr << "    - Last sync:  ";
                for( ObjectList<Node*>::iterator it = last_sync.begin( ); it != last_sync.end( ); ++it )
                    std::cerr << (*it)->get_id( ) << ", ";
                std::cerr << std::endl;
            }
            if( next_sync == NULL )
                std::cerr << "    - Next sync not found" << std::endl;
            else
                std::cerr << "    - Next sync: " << next_sync->get_id( ) << std::endl;
        }
        if( last_sync.empty( ) || ( next_sync == NULL ) )
            _check_only_local = true;
        
        // Scope variables
        Utils::ext_sym_set scoped_vars;
        Node* task_entry = task->get_graph_entry_node( );
        compute_task_auto_scoping_rec( task, task_entry, scoped_vars );
        ExtensibleGraph::clear_visits( task_entry );
    }

    void AutoScoping::compute_task_auto_scoping_rec( Node* task, Node* current, Utils::ext_sym_set& scoped_vars )
    {
        if( !current->is_visited( ) )
        {
            current->set_visited( true );

            if( current->is_graph_node( ) )
            {
                compute_task_auto_scoping_rec( task, current->get_graph_entry_node( ), scoped_vars );
            }
            else if( current->has_statements( ) )
            {
                Utils::ext_sym_set undef = task->get_undefined_behaviour_vars( );
                Scope sc( task->get_graph_related_ast( ).retrieve_context( ) );

                Utils::ext_sym_set ue = current->get_ue_vars( );
                for( Utils::ext_sym_set::iterator it = ue.begin( ); it != ue.end( ); ++it )
                {
                    Symbol s( it->get_symbol( ) );
                    if( s.is_valid( ) && !s.get_scope( ).scope_is_enclosed_by( sc ) )
                        scope_variable( task, current, Utils::UsageKind::USED, *it, scoped_vars );
                }

                Utils::ext_sym_set killed = current->get_killed_vars( );
                for( Utils::ext_sym_set::iterator it = killed.begin( ); it != killed.end( ); ++it )
                {
                    Symbol s( it->get_symbol( ) );
                    if( s.is_valid( ) && !s.get_scope( ).scope_is_enclosed_by( sc ) )
                        scope_variable( task, current, Utils::UsageKind::DEFINED, *it, scoped_vars );
                }
            }

            ObjectList<Node*> children = current->get_children( );
            for( ObjectList<Node*>::iterator it = children.begin( ); it != children.end( ); ++it )
                compute_task_auto_scoping_rec( task, *it, scoped_vars );
        }
    }
    
    void AutoScoping::scope_variable( Node* task, Node* ei_node, Utils::UsageKind usage, Utils::ExtendedSymbol ei,
                                      Utils::ext_sym_set& scoped_vars )
    {
        if( !Utils::ext_sym_set_contains_enclosing_nodecl( ei, scoped_vars ) )
        {   // The expression is not a symbol local from the task
            scoped_vars.insert( ei );

            Utils::UsageKind usage_in_concurrent_regions = compute_usage_in_regions( ei, _simultaneous_tasks );
            Utils::UsageKind usage_in_task = compute_usage_in_region( ei, task );
            
            if( ( usage_in_concurrent_regions._usage_type & Utils::UsageKind::UNDEFINED ) || 
                ( usage_in_task._usage_type & Utils::UsageKind::UNDEFINED ) )
            {
                task->set_sc_undef_var( ei );
            }
            else if( usage_in_concurrent_regions._usage_type & Utils::UsageKind::NONE )
            {
                if( usage_in_task._usage_type & Utils::UsageKind::DEFINED )
                {
                    std::set<Symbol> global_vars = _graph->get_global_variables( );
                    Symbol sym( Utils::ExtendedSymbol::get_nodecl_base( ei.get_nodecl( ) ).get_symbol( ) );
                    ERROR_CONDITION( !sym.is_valid( ), 
                                     "An ExtendedSymbol must have a symbol associated to it base nodecl, but %s does not have one", 
                                     ei.get_nodecl( ).prettyprint( ).c_str( ) );
                    if( ( global_vars.find( sym ) != global_vars.end( ) ) || 
                        Utils::ext_sym_set_contains_nodecl( ei.get_nodecl( ), task->get_live_out_vars( ) ) )
                    {
                        task->set_sc_shared_var( ei );
                    }
                    else
                    {
                        if( usage._usage_type & Utils::UsageKind::DEFINED )
                            task->set_sc_private_var( ei );
                        else
                            task->set_sc_firstprivate_var( ei );
                    }
                }
                else
                {   // SHARED_OR_FIRSTPRIVATE
                    Type ei_type = ei.get_nodecl( ).get_type( );
                    if( ei_type.is_scalar_type( ) || ei_type.is_any_reference( ) )
                        task->set_sc_firstprivate_var( ei );
                    else
                        task->set_sc_shared_var( ei );
                }
            }
            else if( ( usage_in_concurrent_regions._usage_type & Utils::UsageKind::DEFINED ) || 
                     ( ( usage_in_concurrent_regions._usage_type & Utils::UsageKind::USED ) && 
                         usage._usage_type & Utils::UsageKind::DEFINED ) )
            {   // The variable is used in concurrent regions and at least one of the access is a write
                // Check for data race conditions
                if( access_are_synchronous( ei, _simultaneous_tasks ) && access_are_synchronous( ei, task ) )
                {
                    task->set_sc_shared_var( ei );
                }
                else
                {   // Avoid data race conditions by privatizing the variable
                    task->set_sc_private_var( ei );
                }
            }
            else
            {   // Either the variable not used in the concurrent regions, or, if it is, all the access are read
                // SHARED_OR_FIRSTPRIVATE
                Type ei_type = ei.get_nodecl( ).get_type( );
                if( ei_type.is_scalar_type( ) || ei_type.is_any_reference( ) )
                    task->set_sc_firstprivate_var( ei );
                else
                    task->set_sc_shared_var( ei );
            }
        }
    }
    
}
}