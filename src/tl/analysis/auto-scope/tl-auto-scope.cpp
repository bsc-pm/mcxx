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

    static bool sync_in_all_branches( Node* current, Node* original );
    static void collect_tasks_between_nodes( Node* current, Node* last, Node* skip, ObjectList<Node*>& result );
    static bool task_in_loop_is_synchronized_within_loop( Node* task );
    
    AutoScoping::AutoScoping( ExtensibleGraph* pcfg )
        : _graph( pcfg ),
          _simultaneous_tasks( ), _last_sync( ), _next_sync( NULL ), _check_only_local( false )
    {}
    
    void AutoScoping::compute_auto_scoping( )
    {
        ObjectList<Node*> tasks = _graph->get_tasks_list( );
        for( ObjectList<Node*>::iterator it = tasks.begin( ); it != tasks.end( ); ++it )
        {
            compute_task_auto_scoping( *it );
        }
    }

    void AutoScoping::compute_task_auto_scoping( Node* task )
    {
        define_concurrent_regions_limits( task );
        if( VERBOSE )
        {
            std::cerr << " Task concurrent regions limits: \n";
            std::cerr << "    - Last sync:  ";
            for( ObjectList<Node*>::iterator it = _last_sync.begin( ); it != _last_sync.end( ); ++it )
                std::cerr << (*it)->get_id( ) << ", ";
            std::cerr << std::endl;
            std::cerr << "    - Next sync:  " << _next_sync->get_id( ) << std::endl;
        }
        if( _last_sync.empty( ) )
            _check_only_local = true;
        
        // All variables with an undefined behavior cannot be scoped
        Utils::ext_sym_set undef_beh = task->get_undefined_behaviour_vars( );
        for( Utils::ext_sym_set::iterator it = undef_beh.begin( ); it != undef_beh.end( ); ++it )
        {
            Scope sc( task->get_graph_label( ).retrieve_context( ) );
            if( !it->get_symbol( ).get_scope( ).scope_is_enclosed_by( sc ) )
                task->set_sc_undef_var( *it );
        }   
        
        // FIXME Check from here!!!
        
        // Compute the regions of code that can be simultaneous with the current tasks
        compute_simultaneous_tasks( task );
//         for( ObjectList<Node*>::iterator it = task_parents.begin( ); it != task_parents.end( ); ++it )
//         {
//             ExtensibleGraph::clear_visits_aux_backwards( *it );
//         }
//         
//         // If the current task is created in a loop, then it is simultaneous with itself
//         bool is_in_loop = ExtensibleGraph::node_is_in_loop( task );
//         
//         // Scope non-undefined behavior variables
//         Utils::ext_sym_set scoped_vars;
//         Node* task_entry = task->get_graph_entry_node( );
//         compute_task_auto_scoping_rec( task, task_entry, is_in_loop, scoped_vars );
    }

    void AutoScoping::define_concurrent_regions_limits( Node* task )
    {
        // Compute _next_sync
        // -----------------------------------
        ObjectList<Node*> end_point = task->get_children( );
        
        if( end_point.size( ) != 1 )
        {
            Nodecl::OpenMP::Task task_label = task->get_graph_label( ).as<Nodecl::OpenMP::Task>( );
            internal_error( "The end point of a task should be one unique node representing "\
                            "a 'taskwait', a 'barrier' or a 'virtual synchronization'. "\
                            "Task (%d) '%s' has more than one exit", task_label.prettyprint( ).c_str( ) );
        }

        if( end_point[0]->is_omp_virtual_tasksync( ) )
        {
            Nodecl::OpenMP::Task task_label = task->get_graph_label( ).as<Nodecl::OpenMP::Task>( );
            WARNING_MESSAGE( "Task (%d) '%s' auto-scoping computation is limited to local variables because"\
                             " the synchronization point is not defined in the same function as the task.",
                             task->get_id( ), task_label.get_environment( ).prettyprint( ).c_str( ) );
            _check_only_local = true;
        }
        else
        {
            _next_sync = end_point[0];
        }

        // Compute _last_sync
        // Common _last_sync will be in the task parents, but when the task is within a loop, then also
        // the children may contain _last_sync due to the iterations
        // The order of the search is important: we first look in the parents and then in the children
        // That is because children may have back edges and we do not want to traverse them twice 
        // (from the children and from the parents)
        // -----------------------------------
        find_last_synchronization_point_in_parents( task );
        // FIXME I have to review this code with an example as follows
        //     ...
        //     #pragma omp taskwait
        //     ...
        //     for( i=0; i<N; i++ ) {
        //         #pragma omp task
        //         ...
        //         #pragma omp taskwait
        //     }
//         if( ExtensibleGraph::node_is_in_loop( task ) )
//         {
//             Node* loop = NULL;
//             Node* task_outer = task->get_outer_node( );
//             while( task_outer != NULL )
//             {
//                 if( task_outer->is_loop_node( ) )
//                     loop = task_outer;
//                 task_outer = task_outer->get_outer_node( );
//             }
//             ERROR_CONDITION( loop == NULL, "We can't find the loop where the task '%d' is supposed to be embedded\n", task->get_id( ) );
//             
//             find_last_synchronization_point_in_children( task, task_outer );
//         }
        ExtensibleGraph::clear_visits_backwards( task );
//         ExtensibleGraph::clear_visits_aux( task );
    }
    
    static bool sync_in_all_branches( Node* current, Node* original )
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
                    else
                    {
                        res = sync_in_all_branches( current->get_graph_entry_node( ), original );
                    }
                }
                else if( current->is_omp_taskwait_node( ) || current->is_omp_barrier_node( ) )
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
    
    void AutoScoping::find_last_synchronization_point_in_parents( Node* current )
    {
        if( !current->is_visited( ) )
        {
            current->set_visited( true );
            
            ObjectList<Node*> parents = current->get_parents( );
            for( ObjectList<Node*>::iterator it = parents.begin( ); it != parents.end( ); ++it )
            {
                Node* parent = *it;
                
                // Check for synchronization in current parent
                if( parent->is_omp_barrier_node( ) || parent->is_omp_taskwait_node( ) )
                {
                    _last_sync.insert( parent );
                }
                else if ( parent->is_ifelse_statement( ) || parent->is_switch_statement( ) )
                {
                    if( sync_in_all_branches( parent, parent ) )
                    {
                        _last_sync.insert( parent );
                    }
                    ExtensibleGraph::clear_visits_aux( parent );
                }
                else if( parent->is_graph_node( ) )
                {
                    find_last_synchronization_point_in_parents( parent->get_graph_entry_node( ) );
                }
                
                // Keep iterating, if necessary
                if( _last_sync.empty( ) )
                {
                    if( parent->is_entry_node( ) )
                    {
                        Node* parent_outer = parent->get_outer_node( );
                        if( parent_outer != NULL )
                        {
                            ObjectList<Node*> outer_parents = parent_outer->get_parents( );
                            for( ObjectList<Node*>::iterator itp = outer_parents.begin( ); 
                                 itp != outer_parents.end( ) && _last_sync.empty( ); ++itp )
                            {
                                find_last_synchronization_point_in_parents( *itp );
                            }
                        }
                    }
                    else
                    {
                        find_last_synchronization_point_in_parents( parent );
                    }
                }
            }
        }
    }
    
    // This method is only called when a task (the first recursion parameter) is inside a loop
    // We will stop when we find:
    //    - the loop increment, if exists
    //    - the loop conditional (its False child is the loop node exit)
    void AutoScoping::find_last_synchronization_point_in_children( Node* current, Node* loop )
    {
        if( !current->is_visited_aux( ) )
        {
            current->set_visited_aux( true );
            
            ObjectList<Node*> children = current->get_children( );
            // Since we do not traverse IfElse and Switch statements. the number of children must be <=1
            ERROR_CONDITION( children.size( ) == 1, "Number of children must be 1", 0 );
            
            Node* child = children[0];
            
            if( !current->is_exit_node( ) )
            {
                // Check for synchronization in current child
                if( child->is_omp_barrier_node( ) || child->is_omp_taskwait_node( ) )
                {
                    _last_sync.insert( child );
                }
                else
                {
                    if ( child->is_ifelse_statement( ) || child->is_switch_statement( ) )
                    {
                        // TODO Look for synchronizations in the branches
                        WARNING_MESSAGE( "Found a conditional branch while looking for the Last Synchronization Point "\
                        "in AutoScoping Phase. We skip any synchronization found here,"\
                        "but that might be incorrect.\nThis part is under development.", 0 );
                    }
                    else if( child->is_graph_node( ) )
                    {
                        find_last_synchronization_point_in_children( child->get_graph_entry_node( ), loop );
                    }
                }
            }
            
            // Keep iterating, if necessary
            if( _last_sync.empty( ) )
            {
                if( child->is_exit_node( ) )
                {
                    // Si sortim de la serie de loops on la tasca pot estar nested, ja no hem de buscar més!!
                    // Per això necessitem passar la tasca com a paràmetre d'aquesta funció
                    Node* child_outer = child->get_outer_node( );
                    if( ExtensibleGraph::node_contains_node( loop, child_outer ) )  
                        find_last_synchronization_point_in_children( child_outer, loop );
                }
                else
                {
                    find_last_synchronization_point_in_children( child, loop );
                }
            }
        }
    }
    
    static void collect_tasks_between_nodes( Node* current, Node* last, Node* skip, ObjectList<Node*>& result )
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
    
    static bool task_in_loop_is_synchronized_within_loop( Node* task )
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
    
    static void collect_previous_tasks_synchronized_after_scheduling_point( Node* task, ObjectList<Node*> currents, ObjectList<Node*>& result )
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
    
    void AutoScoping::compute_simultaneous_tasks( Node* task )
    {
        // When the task is in a loop and it is not synchronized inside the loop and
        // it is not synchronized between iterations, it is be concurrent with itself
        Node* skip_task = NULL;
        if( ExtensibleGraph::node_is_in_loop( task ) &&        // Task is created within a loop
            task_in_loop_is_synchronized_within_loop( task ) )
            skip_task = task;
        
        // When _last_sync is unknown, we use the entry of the graph where the task is defined
        ObjectList<Node*> last_sync;
        if( !_last_sync.empty( ) )
            last_sync = _last_sync;
        else
            last_sync.insert( ExtensibleGraph::get_extensible_graph_from_node( task )->get_graph_entry_node( ) );
        
        // When _next_sync is unknown, we use the exit of the graph where the task is defined
        Node* next_sync;
        if( _next_sync != NULL )
            next_sync = _next_sync;
        else
            next_sync = ExtensibleGraph::get_extensible_graph_from_node( task )->get_graph_exit_node( );
            

        for( ObjectList<Node*>::iterator it = last_sync.begin( ); it != last_sync.end( ); ++it )
        {
            collect_tasks_between_nodes( *it, next_sync, skip_task, _simultaneous_tasks );
            if( ( *it )->is_omp_taskwait_node( ) )
            {   // Only previous tasks in the same nesting level are synchronized.
                // We have to add here previous nested tasks here
                ObjectList<Node*> it_parents = ( *it )->get_parents( );
                collect_previous_tasks_synchronized_after_scheduling_point( task, it_parents, _simultaneous_tasks );
                // We don want to clear the visit in *it, but from its parents
                for( ObjectList<Node*>::iterator itp = it_parents.begin( ); itp != it_parents.end( ); ++itp )
                {
                    ExtensibleGraph::clear_visits_backwards( *itp );
                }
            }
        }
        for( ObjectList<Node*>::iterator it = last_sync.begin( ); it != last_sync.end( ); ++it )
        {
            ExtensibleGraph::clear_visits( *it );
        }
    }

    void AutoScoping::compute_task_auto_scoping_rec( Node* task, Node* current, bool is_in_loop, Utils::ext_sym_set& scoped_vars )
    {
        if( !current->is_visited( ) )
        {
            current->set_visited( true );

            if( current->is_graph_node( ) )
            {
                compute_task_auto_scoping_rec( task, current->get_graph_entry_node( ), is_in_loop, scoped_vars );
            }
            else if( current->has_statements( ) )
            {
                Utils::ext_sym_set undef = task->get_undefined_behaviour_vars( );
                Scope sc( task->get_graph_label( ).retrieve_context( ) );

                Utils::ext_sym_set ue = current->get_ue_vars( );
                for( Utils::ext_sym_set::iterator it = ue.begin( ); it != ue.end( ); ++it )
                {
                    Symbol s( it->get_symbol( ) );
                    if( s.is_valid( ) && !s.get_scope( ).scope_is_enclosed_by( sc )
                        && !ext_sym_set_contains_englobing_nodecl( *it, undef ) )
                        scope_variable( task, current, '1', *it, is_in_loop, scoped_vars );
                }

                Utils::ext_sym_set killed = current->get_killed_vars( );
                for( Utils::ext_sym_set::iterator it = killed.begin( ); it != killed.end( ); ++it )
                {
                    Symbol s( it->get_symbol( ) );
                    if( s.is_valid( ) && !s.get_scope( ).scope_is_enclosed_by( sc )
                        && !Utils::ext_sym_set_contains_englobing_nodecl( *it, undef ) )
                        scope_variable( task, current, '0', *it, is_in_loop, scoped_vars );
                }
            }

            ObjectList<Node*> children = current->get_children( );
            for( ObjectList<Node*>::iterator it = children.begin( ); it != children.end( ); ++it )
            {
                compute_task_auto_scoping_rec( task, *it, is_in_loop, scoped_vars );
            }
        }
    }

    void AutoScoping::scope_variable( Node* task, Node* ei_node, char usage, Utils::ExtendedSymbol ei,
                                      bool is_in_loop, Utils::ext_sym_set& scoped_vars )
    {
        if( !Utils::ext_sym_set_contains_englobing_nodecl( ei, scoped_vars ) )
        {   // The expression is not a symbol local from the task
            scoped_vars.insert( ei );
            ObjectList<Node*> uses_out = var_uses_out_task( task, ei );
            ObjectList<Node*> uses_in = var_uses_in_task(task, ei);
            ExtensibleGraph::clear_visits_aux(task);

            if( uses_out.empty( ) )
            {
                bool scoped = false;
                if( is_in_loop )
                {
                    scoped = scope_ie_in_iterated_task( task, ei_node, ei_node, usage, ei );
                    ExtensibleGraph::clear_visits_aux( ei_node );
                }

                if( !scoped )
                {   // No need to privatize the variable because of a race condition with the same task
                    Utils::ext_sym_set task_killed = task->get_killed_vars( );
                    if ( !Utils::ext_sym_set_contains_englobing_nodecl( ei, task_killed )
                        && !Utils::ext_sym_set_contains_englobed_nodecl( ei, task_killed ) )
                    {
                        task->set_sc_firstprivate_var( ei );
                    }
                    else
                    {
                        Utils::ext_sym_set task_live_out = task->get_live_out_vars( );
                        if ( ext_sym_set_contains_englobing_nodecl( ei, task_live_out )
                                || ext_sym_set_contains_englobed_nodecl( ei, task_live_out )
                                || ( is_in_loop && task_reads_and_writes( task, ei ) ) )
                        {
                            task->set_sc_shared_var( ei );
                        }
                        else
                        {
                            Utils::ext_sym_set task_live_in = task->get_live_in_vars( );
                            if ( Utils::ext_sym_set_contains_englobing_nodecl( ei, task_live_in )
                                    || Utils::ext_sym_set_contains_englobed_nodecl( ei, task_live_out ) )
                            {
                                task->set_sc_firstprivate_var( ei );
                            }
                            else
                            {
                                task->set_sc_private_var( ei );
                            }
                        }
                    }
                }
            }
            else
            {
                if( task_and_simultaneous_only_read( task, ei ) )
                {
                    task->set_sc_firstprivate_var( ei );
                }
                else
                {   // look for data race  conditions:
                    //   A. If it can occur a data race condition, then v has to be privatized. Sic:
                    //      − If the first action performed in v within the task is a write, then v is scoped as PRIVATE.
                    //      − If the first action performed in v within the task is a read, then v is scoped as FIRSTPRIVATE.
                    //   B. If we can assure that no data race can occur, then v is scoped as SHARED.
                    // TODO
                    WARNING_MESSAGE( "Variable '%s' within task is written, we should look for data race conditions here!! "\
                                        "This is not yet implemented", ei.get_nodecl( ).prettyprint( ).c_str( ) );
                    task->set_sc_race_var( ei );
                }
            }
        }
    }

    bool AutoScoping::task_and_simultaneous_only_read( Node* task, Utils::ExtendedSymbol ei )
    {
        Utils::ext_sym_set task_killed = task->get_killed_vars( );
        if( Utils::ext_sym_set_contains_englobing_nodecl( ei, task_killed )
            || Utils::ext_sym_set_contains_englobed_nodecl( ei, task_killed ) )
        {   // Variable is defined within the task
            return false;
        }
        else
        {
            // Look simultaneous tasks
            for( ObjectList<Node*>::iterator it = _simultaneous_tasks.begin( ); it != _simultaneous_tasks.end( ); ++it )
            {
                task_killed = ( *it )->get_killed_vars( );
                if (ext_sym_set_contains_englobing_nodecl( ei, task_killed ) || ext_sym_set_contains_englobed_nodecl( ei, task_killed ) )
                {   // Variable is defined within the task
                    return false;
                }
            }

            // TODO look simultaneous code in encountering thread
            WARNING_MESSAGE( "We must look for simultaneous code in the encountering thread "\
                              "But this is not yet implemented", 0 );

        }

        return true;
    }

    bool AutoScoping::task_reads_and_writes( Node* task, Utils::ExtendedSymbol ei )
    {
        bool read = false, write = false;

        Node* entry = task->get_graph_entry_node( );
        task_reads_and_writes_rec( task, entry, ei, read, write );
        ExtensibleGraph::clear_visits_aux( entry );

        return read && write;
    }

    void AutoScoping::task_reads_and_writes_rec( Node* task, Node* current, Utils::ExtendedSymbol ei, bool& read, bool& write )
    {
        if( !current->is_visited_aux( ) )
        {
            current->set_visited_aux( true );

            if( current->get_id( ) != task->get_graph_exit_node( )->get_id( ) )
            {
                if( Utils::ext_sym_set_contains_englobing_nodecl( ei, current->get_ue_vars( ) ) )
                    read = true;
                if( Utils::ext_sym_set_contains_englobing_nodecl( ei, current->get_killed_vars( ) ) )
                    write = true;
            }
            if( !read || !write )
            {
                ObjectList<Node*> children = current->get_children( );
                for( ObjectList<Node*>::iterator it = children.begin( ); it != children.end( ); ++it )
                {
                    task_reads_and_writes_rec( task, *it, ei, read, write );
                }
            }
        }
    }

    static bool is_blocked( Node* current )
    {
        if( current != NULL )
        {
            if( current->is_omp_atomic_node( ) || current->is_omp_critical_node( ) )
            {
                return true;
            }
            return is_blocked( current->get_outer_node( ) );
        }

        return false;
    }

    bool AutoScoping::scope_ie_in_iterated_task(Node* task, Node* current, Node* ei_node, char usage, Utils::ExtendedSymbol ei)
    {
        if( !current->is_visited_aux( ) )
        {
            current->set_visited_aux( true );

            if( current->get_id( ) != task->get_graph_exit_node( )->get_id( ) )
            {
                if( current->is_graph_node( ) )
                {
                    scope_ie_in_iterated_task( task, current->get_graph_entry_node( ), ei_node, usage, ei );
                }
                else if( current->has_statements( ) )
                {
                    Utils::ext_sym_set undef = task->get_undefined_behaviour_vars( );

                    if( usage == '0' )
                    {
                        Utils::ext_sym_set ue = current->get_ue_vars( );
                        Utils::ext_sym_set killed = current->get_killed_vars( );
                        if( Utils::ext_sym_set_contains_englobing_nodecl( ei, ue )
                            || Utils::ext_sym_set_contains_englobing_nodecl( ei, killed ) )
                        {
                            if( !is_blocked( ei_node->get_outer_node( ) ) && !is_blocked( current->get_outer_node( ) ) )
                            {   // We privatize the variable to avoid a race condition
                                task->set_sc_private_var( ei );
                                return true;
                            }
                        }
                    }
                    else
                    {   // usage == '1'
                        Utils::ext_sym_set killed = current->get_killed_vars( );
                        for( Utils::ext_sym_set::iterator it = killed.begin( ); it != killed.end( ); ++it )
                        {
                            if( !is_blocked( ei_node->get_outer_node( ) ) && !is_blocked( current->get_outer_node( ) ) )
                            {   // We privatize the variable to avoid a race condition
                                task->set_sc_firstprivate_var( ei );
                                return true;
                            }
                        }
                    }
                }

                ObjectList<Node*> children = current->get_children( );
                for( ObjectList<Node*>::iterator it = children.begin( ); it != children.end( ); ++it )
                {
                    scope_ie_in_iterated_task( task, *it, ei_node, usage, ei );
                }
            }
        }

        return false;
    }

    static ObjectList<Node*> uses_from_node_to_node( Node* current, Node* end, Utils::ExtendedSymbol ei, Node* task )
    {
        ObjectList<Node*> uses;
        if( ( current->get_id( ) ) != task->get_id( ) && ( !current->is_visited( ) ) )
        {
            current->set_visited( true );
            if( current->get_id( ) != end->get_id( ) )
            {
                Utils::ext_sym_set ue_vars = current->get_ue_vars( );
                Utils::ext_sym_set killed_vars = current->get_killed_vars( );
                Utils::ext_sym_set undef_vars = current->get_undefined_behaviour_vars( );

                if ( ( Utils::ext_sym_set_contains_englobing_nodecl( ei, ue_vars )
                    || Utils::ext_sym_set_contains_englobed_nodecl( ei, ue_vars )
                    || Utils::ext_sym_set_contains_englobing_nodecl( ei, killed_vars )
                    || Utils::ext_sym_set_contains_englobed_nodecl( ei, killed_vars ) )
                    && !Utils::ext_sym_set_contains_englobing_nodecl( ei, undef_vars )
                    && !Utils::ext_sym_set_contains_englobed_nodecl( ei, undef_vars) )
                {
                    uses.append( current );
                }

                ObjectList<Node*> children = current->get_children( );
                for( ObjectList<Node*>::iterator it = children.begin( ); it != children.end( ); ++it )
                {
                    uses.append( uses_from_node_to_node( *it, end, ei, task ) );
                }
            }
        }

        return uses;
    }


    // FIXME: clearing visits in this function is not correct
    ObjectList<Node*> AutoScoping::var_uses_out_task( Node* task, Utils::ExtendedSymbol ei )
    {
        ObjectList<Node*> uses;

        // Get the uses in the simultaneous tasks
        for( ObjectList<Node*>::iterator it = _simultaneous_tasks.begin( ); it != _simultaneous_tasks.end( ); ++it )
        {
            uses.append( var_uses_in_task( *it, ei ) );
            ExtensibleGraph::clear_visits_aux( *it );
        }

        // Get the uses in the encountering thread from the task scheduling point till the task synchronization point
        ObjectList<Node*> parents = task->get_parents( );
        // NOTE: We do not fusion these loops because parents will converge in some point and
        // keeping them separated we do not analyse the same part of the graph two times
        for( ObjectList<Node*>::iterator it = parents.begin( ); it != parents.end( ); ++it )
        {
            ObjectList<Node*> it_children = ( *it )->get_children( );
            for( ObjectList<Node*>::iterator itc = it_children.begin( ); itc != it_children.end( ); ++itc )
            {
                uses.append( uses_from_node_to_node( *itc, _next_sync, ei, task ) );
                ( *itc )->set_visited( false );
                _next_sync->set_visited( false );
            }
        }
        for( ObjectList<Node*>::iterator it = parents.begin( ); it != parents.end( ); ++it )
        {
            // FIXME Is this clearing correct?????
            ExtensibleGraph::clear_visits_avoiding_branch( *it, task );
        }

        return uses;
    }

    ObjectList<Node*> AutoScoping::var_uses_in_task( Node* current, Utils::ExtendedSymbol ei )
    {
        ObjectList<Node*> uses;

        if( !current->is_visited_aux( ) )
        {
            current->set_visited_aux( true );

            Utils::ext_sym_set ue_vars = current->get_ue_vars( );
            Utils::ext_sym_set killed_vars = current->get_killed_vars( );
            Utils::ext_sym_set undef_vars = current->get_undefined_behaviour_vars( );

            if( ( Utils::ext_sym_set_contains_englobing_nodecl(ei, ue_vars)
                    || ext_sym_set_contains_englobed_nodecl(ei, ue_vars)
                    || Utils::ext_sym_set_contains_englobing_nodecl(ei, killed_vars)
                    || ext_sym_set_contains_englobed_nodecl(ei, killed_vars) )
                    && !Utils::ext_sym_set_contains_englobing_nodecl(ei, undef_vars)
                    && !ext_sym_set_contains_englobed_nodecl(ei, undef_vars) )
            {
                uses.append( current );
            }

            ObjectList<Node*> children = current->get_children( );
            for( ObjectList<Node*>::iterator it = children.begin( ); it != children.end( ); ++it )
            {
                uses.append( var_uses_in_task( *it, ei ) );
            }
        }

        return uses;
    }

}
}