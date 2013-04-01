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

    AutoScoping::AutoScoping( ExtensibleGraph* pcfg )
        : _graph( pcfg ),
          _simultaneous_tasks( ), _last_sync( NULL ), _next_sync( NULL )
    {}

    void AutoScoping::compute_auto_scoping( )
    {
        Node* graph = _graph->get_graph( );
        compute_auto_scoping_rec( graph );
        ExtensibleGraph::clear_visits( graph );
    }

    void AutoScoping::compute_auto_scoping_rec( Node* current )
    {
        if( !current->is_visited( ) )
        {
            current->set_visited( true );

            if( current->is_exit_node( ) )
                return;

            if( current->is_graph_node( ) )
            {
                // Compute the scope from inner tasks first
                Node* entry_node = current->get_graph_entry_node( );
                compute_auto_scoping_rec( entry_node );

                if( current->is_omp_task_node( ) )
                {
                    ExtensibleGraph::clear_visits_in_level( entry_node, current );
                    compute_task_auto_scoping( current );
                }
            }

            ObjectList<Node*> children = current->get_children( );
            for( ObjectList<Node*>::iterator it = children.begin( ); it != children.end( ); ++it )
            {
                compute_auto_scoping_rec( *it );
            }
        }
    }

    void AutoScoping::compute_task_auto_scoping( Node* task )
    {
        ObjectList<Node*> end_point = task->get_children( );
        if( ( end_point.size() > 1 ) || end_point.empty() )
        {
            Nodecl::OpenMP::Task task_label = task->get_graph_label( ).as<Nodecl::OpenMP::Task>( );
            internal_error( "The end point of a task should be one unique node representing a 'taskwait', a 'barrier' or the 'exit' of a graph. "\
                            "Task (%d) '%s' has more than one exit", task_label.prettyprint( ).c_str( ) );
        }

        if( end_point[0]->is_exit_node( ) )
        {
            // FIXME We can know things about the variables that are defined in the same function and the parameters of the function
            Nodecl::OpenMP::Task task_label = task->get_graph_label( ).as<Nodecl::OpenMP::Task>( );
            WARNING_MESSAGE( "Task (%d) '%s' auto-scoping cannot be computed because the synchronization point "\
                             "is not defined in the same function as the task. This is still under development.",
                             task->get_id( ), task_label.get_environment( ).prettyprint( ).c_str( ) );
        }
        else
        {
            // All variables with an undefined behavior cannot be scoped
            Utils::ext_sym_set undef_beh = task->get_undefined_behaviour_vars( );
            for( Utils::ext_sym_set::iterator it = undef_beh.begin( ); it != undef_beh.end( ); ++it )
            {
                Scope sc( task->get_graph_label( ).retrieve_context( ) );
                if( !it->get_symbol( ).get_scope( ).scope_is_enclosed_by( sc ) )
                    task->set_sc_undef_var( *it );
            }

            // Compute the regions of code that can be simultaneous with the current tasks
            _next_sync = end_point[0];
            ObjectList<Node*> task_parents = task->get_parents( );
            for( ObjectList<Node*>::iterator it = task_parents.begin( ); it != task_parents.end( ); ++it )
            {
                compute_simultaneous_tasks( *it, /* current nest */ 0 );
            }
            for( ObjectList<Node*>::iterator it = task_parents.begin( ); it != task_parents.end( ); ++it )
            {
                ExtensibleGraph::clear_visits_aux_backwards( *it );
            }

            // If the current task is created in a loop, then it is simultaneous with itself
            bool is_in_loop = ExtensibleGraph::is_in_loop( task );

            // Scope non-undefined behavior variables
            Utils::ext_sym_set scoped_vars;
            Node* task_entry = task->get_graph_entry_node( );
            compute_task_auto_scoping_rec( task, task_entry, is_in_loop, scoped_vars );
        }
    }

    void AutoScoping::compute_simultaneous_tasks( Node* current, int current_nest, int taskwait_nest )
    {
        if( !current->is_visited_aux( ) )
        {
            current->set_visited_aux( true );

            if( current->is_entry_node( ) )
            {   // Look for the Last Synchronization point in the outer node
                Node* outer_node = current->get_outer_node( );
                if( outer_node == NULL )
                {    // We have parsed the whole graph
                    return;
                }
                else
                {
                    if( current_nest == taskwait_nest )
                    {   // Reset taskwait nest value because outer task will not be synchronized by this taskwait
                        taskwait_nest = INT_MAX;
                    }
                    // Check for the synchronization point in the parents of the outer node
                    ObjectList<Node*> parents = outer_node->get_parents( );
                    for( ObjectList<Node*>::iterator it = parents.begin( );
                         it != parents.end( ) && ( _last_sync == NULL ) ; ++it )
                    {
                        compute_simultaneous_tasks( outer_node, current_nest - 1 );
                    }
                }
            }
            else
            {
                if( current->is_omp_barrier_node( ) && !ExtensibleGraph::is_in_conditional_branch( current ) )
                {   // A barrier placed in a non-conditional place is the Last Synchronization point
                    _last_sync = current;
                }
                else if( current->is_omp_taskwait_node( ) )
                {   // A taskwait only synchronizes tasks in the scheduled by the encountering thread of the taskwait
                    taskwait_nest = current_nest;
                }
                else if( current->is_graph_node( ) )
                {
                    if( current->is_omp_task_node( )
                        && ( ( current_nest > taskwait_nest ) || ( taskwait_nest == INT_MAX /* no taskwait found */ ) ) )
                    {   // Possible inner tasks inside 'current' are added with the addition of this node
                        // FIXME Should we add recursively all tasks inside the current task to the _simultaneous_tasks set??
                        _simultaneous_tasks.append( current );
                    }
                    else
                    {   // Analyze recursively the inner nodes of the graph
                        compute_simultaneous_tasks( current->get_graph_exit_node( ), current_nest + 1, taskwait_nest );
                    }
                }

                if( _last_sync == NULL )
                {
                    // Look for more tasks in the parents
                    ObjectList<Node*> parents = current->get_parents( );
                    for( ObjectList<Node*>::iterator it = parents.begin( );
                         it != parents.end( ) && ( _last_sync == NULL ); ++it )
                    {
                        // A last_sync != NULL in any of the parents means the barrier will be reach
                        // by all threads that were running before this barrier
                        compute_simultaneous_tasks( *it, current_nest, taskwait_nest );
                    }
                }
            }
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