/*--------------------------------------------------------------------
  (C) Copyright 2006-2013 Barcelona Supercomputing Center
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

#include "tl-omp-lint.hpp"
#include "tl-datareference.hpp"
#include "tl-tribool.hpp"
#include "cxx-diagnostic.h"

namespace TL { 
namespace OpenMP {
    
    struct FunctionCodeVisitor : Nodecl::ExhaustiveVisitor<void>
    {
        typedef TL::ObjectList<TL::Analysis::Node*> PCFG_Node_list;
        
        std::string get_nodecl_list_str( const Nodecl::List& nodecl_list )
        {
            std::string result;
            for( Nodecl::List::const_iterator it = nodecl_list.begin( ); it != nodecl_list.end( ); )
            {
                result += it->prettyprint( );
                ++it;
                if( it != nodecl_list.end( ) )
                    result += ", ";
            }
            return result;
        }
        
        void visit( const Nodecl::FunctionCode& function_code )
        {
            TL::Analysis::AnalysisSingleton& singleton = TL::Analysis::AnalysisSingleton::get_analysis( );
            TL::Analysis::PCFGAnalysis_memento memento;
            // We compute liveness analysis (that includes PCFG and use-def) because 
            // we need the information computed by TaskConcurrency (last and next synchronization points of a task)
            TL::ObjectList<TL::Analysis::ExtensibleGraph*> extensible_graphs =
                    singleton.liveness( memento, function_code );
            ERROR_CONDITION( extensible_graphs.size() != 1, "I expected 1 graph per FunctionCode", 0 );
            
            TL::Analysis::ExtensibleGraph* graph = extensible_graphs[0];
            
            if (CURRENT_CONFIGURATION->debug_options.print_pcfg)
                graph->print_graph_to_dot(false, false, false, false, false, false);
            
            // Get all task nodes
            PCFG_Node_list tasks = graph->get_tasks_list();
            for( PCFG_Node_list::iterator it = tasks.begin(); it != tasks.end(); it++ )
            {
                // Automatic storage variables as shared
                {
                    Nodecl::List local_vars;
                    if( task_is_locally_bound(*it, local_vars).is_true( ) )
                    {
                        if( task_only_synchronizes_in_enclosing_scopes(*it).is_true( ) )
                        {
                            Nodecl::NodeclBase task = (*it)->get_graph_related_ast( );
                            std::string local_vars_str = get_nodecl_list_str( local_vars );
                            warn_printf( "%s: warning: '#pragma omp task' uses local data '%s' "
                                         "whose lifetime may have ended when the task is executed\n", 
                                         task.get_locus_str().c_str(), local_vars_str.c_str( ) );
                        }
                        else if( task_is_statically_determined_to_late_execution(*it).is_true( ) )
                        {
                            Nodecl::NodeclBase task = (*it)->get_graph_related_ast();
                            std::string local_vars_str = get_nodecl_list_str( local_vars );
                            warn_printf( "%s: warning: '#pragma omp task' uses local data '%s' but may be "
                                         "executed after the function ends\n",
                                         task.get_locus_str().c_str(), local_vars_str.c_str( ) );
                        }
                    }
                }
                
                // Race conditions
                {
                    Nodecl::List race_cond_vars;
                    if( task_may_cause_race_condition( graph, *it, race_cond_vars ).is_true( ) )
                    {
                        Nodecl::NodeclBase task = (*it)->get_graph_related_ast( );
                        std::string race_cond_vars_str = get_nodecl_list_str( race_cond_vars );
                        warn_printf( "%s: warning: '#pragma omp task' may have a race condition on '%s' "
                                     "because other threads access concurrently to the same data\n", 
                                     task.get_locus_str().c_str(), race_cond_vars_str.c_str( ) );
                    }
                }
            }
        }
        
        // If this function returns false it may mean both unknown/no
        tribool data_ref_is_local(TL::DataReference data_ref, Nodecl::List& local_data_refs)
        {
            if (!data_ref.is_valid())
            {
                // Somehow the data reference cannot be analyzed as valid
                // so act conservatively and return unknown
                return tribool();
            }
            
            return data_ref_is_local_rec(data_ref, local_data_refs);
        }
        
        tribool symbol_is_local(const Nodecl::NodeclBase& nodecl_sym, Nodecl::List& local_syms)
        {
            TL::Symbol sym = nodecl_sym.get_symbol();
            if (!sym.is_valid())
            {
                // Somehow the data reference cannot be analyzed as valid
                // so act conservatively and return unknown
                return tribool();
            }
            
            tribool result = !sym.get_type().is_any_reference() &&
                             sym.get_scope().is_block_scope();
            if( result.is_true( ) )
                local_syms.append( nodecl_sym );
            return result;
        }
        
        // If this function returns false it may mean both unknown/no
        tribool data_ref_is_local_rec(TL::DataReference data_ref, Nodecl::List& local_data_refs )
        {
            TL::Symbol base_sym = data_ref.get_base_symbol();
            if (!base_sym.is_valid())
                return false;
            
            tribool result = false;
            if (data_ref.is<Nodecl::Symbol>())
            {
                result = !base_sym.get_type().is_any_reference() &&
                         base_sym.get_scope().is_block_scope();
            }
            else if (data_ref.is<Nodecl::Dereference>())
            {
                // *&a -> a
                if (data_ref.as<Nodecl::Dereference>().get_rhs().is<Nodecl::Reference>())
                {
                    result = data_ref_is_local_rec(
                                data_ref.as<Nodecl::Dereference>().get_rhs().as<Nodecl::Reference>().get_rhs(), 
                                local_data_refs);
                }
                else
                {
                    result = data_ref_is_local_rec(data_ref.as<Nodecl::Dereference>().get_rhs(), 
                                                   local_data_refs) &&
                             base_sym.get_type().is_array();
                }
            }
            else if (data_ref.is<Nodecl::Reference>())
            {
                // &*a -> a
                if (data_ref.as<Nodecl::Reference>().get_rhs().is<Nodecl::Dereference>())
                {
                    result = data_ref_is_local_rec(
                                data_ref.as<Nodecl::Reference>().get_rhs().as<Nodecl::Dereference>().get_rhs(), 
                                local_data_refs);
                }
                else
                {
                    result = data_ref_is_local_rec(data_ref.as<Nodecl::Reference>().get_rhs(), local_data_refs);
                }
            }
            else if (data_ref.is<Nodecl::ArraySubscript>())
            {
                result = data_ref_is_local_rec(data_ref.as<Nodecl::ArraySubscript>().get_subscripted(), 
                                               local_data_refs) &&
                         base_sym.get_type().is_array();
            }
            else if (data_ref.is<Nodecl::ClassMemberAccess>())
            {
                result = data_ref_is_local_rec(data_ref.as<Nodecl::ClassMemberAccess>().get_lhs(), local_data_refs);
            }
            
            if( result.is_true( ) )
                local_data_refs.append( data_ref );
                
            return result;
        }
        
        tribool any_symbol_is_local(Nodecl::List item_list, Nodecl::List& local_syms)
        {
            tribool result( false );
            for( Nodecl::List::iterator it = item_list.begin(); it != item_list.end(); it++ )
                result = result || symbol_is_local( *it, local_syms );
            return result;
        }
        
        tribool any_data_ref_is_local(Nodecl::List item_list, Nodecl::List& local_data_refs)
        {
            tribool result( false );
            for( Nodecl::List::iterator it = item_list.begin(); it != item_list.end(); it++ )
                result = result || data_ref_is_local( *it, local_data_refs );
            return result;
        }
        
        tribool task_is_locally_bound( TL::Analysis::Node *n, Nodecl::List& local_vars )
        {
            ERROR_CONDITION( !n->is_omp_task_node( ), "Expecting a Task node, but found a '%s' node.", 
                             n->get_type_as_string( ).c_str( ) );
            
            Nodecl::NodeclBase task = n->get_graph_related_ast( );
            ERROR_CONDITION( task.is_null( ), "Invalid target task tree related to node %d.", n->get_id( ) );
            
            tribool result = false;
            
            TL::Analysis::PCFGPragmaInfo task_pragma_info = n->get_pragma_node_info( );
            if( task_pragma_info.has_clause( TL::Analysis::__shared ) )
            {
                Nodecl::List shared = task_pragma_info.get_clause( TL::Analysis::__shared ).get_args( );
                result = result || any_symbol_is_local( shared, local_vars );
            }
            if( task_pragma_info.has_clause( TL::Analysis::__in ) )
            {
                Nodecl::List in = task_pragma_info.get_clause( TL::Analysis::__in ).get_args( );
                result = result || any_data_ref_is_local( in, local_vars );
            }
            if( task_pragma_info.has_clause( TL::Analysis::__out ) )
            {
                Nodecl::List out = task_pragma_info.get_clause( TL::Analysis::__out ).get_args( );
                result = result || any_data_ref_is_local( out, local_vars );
            }
            if( task_pragma_info.has_clause( TL::Analysis::__inout ) )
            {
                Nodecl::List inout = task_pragma_info.get_clause( TL::Analysis::__inout ).get_args( );
                result = result || any_data_ref_is_local( inout, local_vars );
            }
                 
            return result;
        }
        
        // Returns true when task may synchronize at some point 
        // which is not enclosed in the scope where the task is created
        tribool task_only_synchronizes_in_enclosing_scopes( TL::Analysis::Node *n )
        {
            TL::ObjectList<TL::Analysis::Node*> children = n->get_children( );
            ERROR_CONDITION( children.empty( ), 
                             "We should have computed at least some exit edge for this task", 0 );
            
            tribool result = false;
            
            ObjectList<TL::Analysis::Node*> task_parents = n->get_parents( );
            ERROR_CONDITION( task_parents.size( ) != 1, 
                             "A task node must have a unique parent, which is the task creation node, "
                             " but task %d has %d parents.", n->get_id( ), task_parents.size( ) );
            TL::Analysis::Node* task_creation = task_parents[0];
            TL::Analysis::Node* task_creation_sc = TL::Analysis::ExtensibleGraph::get_enclosing_context( task_creation );
            ERROR_CONDITION( task_creation_sc == NULL, 
                             "The context of a task creation node cannot be NULL, but task's %d is NULL.", 
                             n->get_id( ) );
            for( TL::ObjectList<TL::Analysis::Node*>::iterator it = children.begin( ); 
                 it != children.end( ); ++it )
            {
                TL::Analysis::Node* sync_sc = TL::Analysis::ExtensibleGraph::get_enclosing_context( *it );
                if( sync_sc == NULL )   // This a Post_Sync
                    continue;
                
                if( !TL::Analysis::ExtensibleGraph::node_contains_node( task_creation_sc, sync_sc ) )
                {
                    result = true;
                    break;
                }
            }
            
            return result;
        }
        
        // Returns true when the task has a post synchronization, meaning that 
        // it may be synchronized after the function where it is created ends
        tribool task_is_statically_determined_to_late_execution( TL::Analysis::Node *n )
        {
            TL::ObjectList<TL::Analysis::Edge*> exit_edges = n->get_exit_edges( );            
            ERROR_CONDITION( exit_edges.empty( ), 
                             "We should have computed at least some exit edge for this task", 0 );
            
            tribool result = false;
            for( TL::ObjectList<TL::Analysis::Edge*>::iterator it = exit_edges.begin( );
                 it != exit_edges.end( ); it++ )
            {
                std::string exit_label = (*it)->get_label( );
                if( exit_label == "post" )
                {
                    result = true;
                    break;
                }
            }
            return result;
        }
        
        // This method returns in #concurrently_used_vars a relation of variables and 
        // the nodes where these variables have an access (use|definition|undefined)
        void compute_usage_between_nodes( TL::Analysis::Node* source, TL::Analysis::Node* target, 
                                          TL::Analysis::Node* ommited_node,
                                          std::map<Nodecl::NodeclBase, ObjectList<TL::Analysis::Node*> >& concurrently_used_vars, 
                                          const Nodecl::List& variables )
        {
            if( source->is_visited( ) || source == target )
                return;
            
            // Treat the current node
            if( source != ommited_node )
            {
                source->set_visited( true );
                if( source->is_graph_node( ) )
                    compute_usage_between_nodes( source->get_graph_entry_node( ), target, ommited_node, 
                                                 concurrently_used_vars, variables );
                else if( source->has_statements( ) )
                {
                    TL::Analysis::Utils::ext_sym_set ue = source->get_ue_vars( );
                    TL::Analysis::Utils::ext_sym_set kill = source->get_killed_vars( );
                    TL::Analysis::Utils::ext_sym_set undef = source->get_undefined_behaviour_vars( );
                    
                    TL::Analysis::Utils::ext_sym_set accessed_vars;
                    accessed_vars.insert( ue.begin( ), ue.end( ) );
                    accessed_vars.insert( kill.begin( ), kill.end( ) );
                    accessed_vars.insert( undef.begin( ), undef.end( ) );
                    
                    for( Nodecl::List::const_iterator it = variables.begin( ); it != variables.end( ); ++it )
                    {
                        // If the variable #*it or a subpart/superpart have some usage, we add it to the map
                        if( TL::Analysis::Utils::ext_sym_set_contains_nodecl( *it, accessed_vars ) || 
                            TL::Analysis::Utils::ext_sym_set_contains_enclosing_nodecl( *it, accessed_vars ) || 
                            TL::Analysis::Utils::ext_sym_set_contains_enclosed_nodecl( *it, accessed_vars ) )
                        {
                            if( concurrently_used_vars.find( *it ) != concurrently_used_vars.end( ) )
                                concurrently_used_vars[*it].insert( source );
                            else
                                concurrently_used_vars.insert( 
                                std::pair<Nodecl::NodeclBase, TL::ObjectList<TL::Analysis::Node*> >( 
                                *it, TL::ObjectList<TL::Analysis::Node*>( 1, source ) ) );
                        }
                    }
                }
            }
            
            // Treat the children
            TL::ObjectList<TL::Analysis::Node*> children;
            if( source->is_exit_node( ) )
                children = source->get_outer_node( )->get_children( );
            else
                children = source->get_children( );
            for( TL::ObjectList<TL::Analysis::Node*>::iterator it = children.begin( ); it != children.end( ); ++it )
                compute_usage_between_nodes( *it, target, ommited_node, concurrently_used_vars, variables );
        }
        
        tribool task_may_cause_race_condition( TL::Analysis::ExtensibleGraph* pcfg, 
                                               TL::Analysis::Node *n, Nodecl::List& race_cond_vars )
        {
            ERROR_CONDITION( !n->is_omp_task_node( ), "Expecting a Task node, but found a '%s' node.", 
                             n->get_type_as_string( ).c_str( ) );
            
            Nodecl::NodeclBase task = n->get_graph_related_ast( );
            ERROR_CONDITION( task.is_null( ), "Invalid target task tree related to node %d.", n->get_id( ) );
            
            tribool result = false;
            
            // Collect all symbols/data references that may cause a race condition
            Nodecl::List task_shared_variables;
            TL::Analysis::PCFGPragmaInfo task_pragma_info = n->get_pragma_node_info( );
            if( task_pragma_info.has_clause( TL::Analysis::__shared ) )
            {
                Nodecl::List shared = task_pragma_info.get_clause( TL::Analysis::__shared ).get_args( );
                task_shared_variables.append( shared );
            }
            if( task_pragma_info.has_clause( TL::Analysis::__in ) )
            {
                Nodecl::List in = task_pragma_info.get_clause( TL::Analysis::__in ).get_args( );
                task_shared_variables.append( in );
            }
            if( task_pragma_info.has_clause( TL::Analysis::__out ) )
            {
                Nodecl::List out = task_pragma_info.get_clause( TL::Analysis::__out ).get_args( );
                task_shared_variables.append( out );
            }
            if( task_pragma_info.has_clause( TL::Analysis::__inout ) )
            {
                Nodecl::List inout = task_pragma_info.get_clause( TL::Analysis::__inout ).get_args( );
                task_shared_variables.append( inout );
            }
            
            // Get the previous and next synchronization points (computed during Liveness analysis)
            TL::ObjectList<TL::Analysis::Node*> last_sync;
            TL::ObjectList<TL::Analysis::Node*> last_sync_list = pcfg->get_task_last_synchronization( n );
            if( last_sync_list.empty( ) )
            {   // Consider as last synchronization point the creation of the task or,
                // if the task is inside a loop, the entry of the loop
                TL::Analysis::Node* task_creation = n->get_parents( )[0];
                if( TL::Analysis::ExtensibleGraph::node_is_in_loop( task_creation ) )
                {
                    TL::Analysis::Node* outer_node = task_creation->get_outer_node( );
                    TL::Analysis::Node* most_outer_loop;
                    while( outer_node != NULL )
                    {
                        if( outer_node->is_loop_node( ) )
                            most_outer_loop = outer_node;
                        outer_node = outer_node->get_outer_node( );
                    }
                    ERROR_CONDITION( most_outer_loop == NULL, 
                                     "Node %d is recognized to be in a loop node, but it has not been found", n->get_id( ) );
                    last_sync.insert( most_outer_loop->get_graph_entry_node( ) );
                }
                else
                    last_sync.insert( task_creation );   // The task creation node
            }
            else
                last_sync = last_sync_list;
            
            TL::ObjectList<TL::Analysis::Node*> next_sync = pcfg->get_task_next_synchronization( n );
            if( next_sync.empty( ) )        // Consider as next synchronization point the exit of the graph
                next_sync.insert( pcfg->get_graph( )->get_graph_exit_node( ) );
            
            // Traverse the graph from last_sync to next_sync looking for uses of the shared variables found in the task
            std::map<Nodecl::NodeclBase, TL::ObjectList<TL::Analysis::Node*> > concurrently_used_vars;
            for( TL::ObjectList<TL::Analysis::Node*>::iterator itl = last_sync.begin( ); itl != last_sync.end( ); ++itl )
                for( TL::ObjectList<TL::Analysis::Node*>::iterator itn = next_sync.begin( ); itn != next_sync.end( ); ++itn )
                    compute_usage_between_nodes( *itl, *itn, n, concurrently_used_vars, task_shared_variables );
            std::map<Nodecl::NodeclBase, TL::ObjectList<TL::Analysis::Node*> > task_used_vars;
            TL::Analysis::Node* task_entry = n->get_graph_entry_node( );
            for( TL::ObjectList<TL::Analysis::Node*>::iterator itl = last_sync.begin( ); itl != last_sync.end( ); ++itl )
                TL::Analysis::ExtensibleGraph::clear_visits( *itl );
            compute_usage_between_nodes( task_entry, n->get_graph_exit_node( ), NULL, task_used_vars, task_shared_variables );
            TL::Analysis::ExtensibleGraph::clear_visits( task_entry );
            
            // Detect data races on the variables that appear in both the task and concurrent code with the task
            TL::Analysis::Utils::ext_sym_set task_defs = n->get_killed_vars( );
                    // To be conservative, the undef. variables count as definitions
            TL::Analysis::Utils::ext_sym_set task_undef = n->get_undefined_behaviour_vars( );  
            task_defs.insert( task_undef.begin( ), task_undef.end( ) );
            bool var_warned;
            for( std::map<Nodecl::NodeclBase, TL::ObjectList<TL::Analysis::Node*> >::iterator it = concurrently_used_vars.begin( ); 
                 it != concurrently_used_vars.end( ); ++it )
            {
                var_warned = false; // reset this value for each variable
                
                // At least one of the accesses must be a write
                TL::Analysis::Utils::ext_sym_set node_defs;
                for( TL::ObjectList<TL::Analysis::Node*>::iterator it2 = it->second.begin( ); it2 != it->second.end( ); ++it2 )
                {
                    TL::Analysis::Utils::ext_sym_set node_kill = ( *it2 )->get_killed_vars( );
                    TL::Analysis::Utils::ext_sym_set node_undef = ( *it2 )->get_undefined_behaviour_vars( );
                    node_defs.insert( node_kill.begin( ), node_kill.end( ) );
                    node_defs.insert( node_undef.begin( ), node_undef.end( ) );
                }
                if( TL::Analysis::Utils::ext_sym_set_contains_nodecl( it->first, task_defs ) || 
                    TL::Analysis::Utils::ext_sym_set_contains_enclosing_nodecl( it->first, task_defs ) || 
                    TL::Analysis::Utils::ext_sym_set_contains_enclosed_nodecl( it->first, task_defs ) || 
                    TL::Analysis::Utils::ext_sym_set_contains_nodecl( it->first, node_defs ) || 
                    TL::Analysis::Utils::ext_sym_set_contains_enclosing_nodecl( it->first, node_defs ) || 
                    TL::Analysis::Utils::ext_sym_set_contains_enclosed_nodecl( it->first, node_defs ) )
                {   // If all accesses are protected in a critical/atomic construct, then there is no race condition
                    // 1. Check accesses in the concurrent nodes
                    for( TL::ObjectList<TL::Analysis::Node*>::iterator it2 = it->second.begin( ); it2 != it->second.end( ); ++it2 )
                    {
                        if( !TL::Analysis::ExtensibleGraph::node_is_in_synchronous_construct( *it2 ) )
                        {
                            warn_printf( "%s: warning: '#pragma omp task' uses data '%s' "
                                         "which is in a race condition with another usage of the same variable\n",
                                         task.get_locus_str().c_str(), it->first.prettyprint( ).c_str( ) );
                            var_warned = true;
                            break;
                        }
                    }
                    // 2. Check accesses in the task
                    if( !var_warned )
                    {
                        std::map<Nodecl::NodeclBase, TL::ObjectList<TL::Analysis::Node*> >::iterator tmp = task_used_vars.find( it->first );
                        if( tmp == task_used_vars.end( ) )
                        {
                            warn_printf( "%s: warning: Variable %s is marked as shared in a task but it is not used inside the task\n", 
                                         task.get_locus_str().c_str(), it->first.prettyprint( ).c_str( ) );
                        }
                        else
                        {
                            for( TL::ObjectList<TL::Analysis::Node*>::iterator it2 = tmp->second.begin( ); it2 != tmp->second.end( ); ++it2 )
                            {
                                if( !TL::Analysis::ExtensibleGraph::node_is_in_synchronous_construct( *it2 ) )
                                {
                                    warn_printf( "%s: warning: '#pragma omp task' uses data '%s' "
                                                 "which is in a race condition with another usage of the same variable\n",
                                                 task.get_locus_str().c_str(), it->first.prettyprint( ).c_str( ) );
                                    break;
                                }
                            }
                        }
                    }
                }
            }
            
            return result;
        }
    };
            

    Lint::Lint()
        : _disable_phase("0")
    {
        set_phase_name("OpenMP Lint");
        set_phase_description("This phase is able to detect some common pitfalls when using OpenMP");

        register_parameter("disable-omp-lint",
                "Disables this phase. You should not need this. If you do, then it is an error. Please fill a bug",
                _disable_phase,
                "0");
    }

    void Lint::run(TL::DTO& dto)
    {
        Nodecl::NodeclBase top_level = dto["nodecl"];

        if (_disable_phase == "0")
        {
            FunctionCodeVisitor function_codes;
            function_codes.walk(top_level);
        }
    }

    void Lint::pre_run(TL::DTO& dto)
    {}

}
}

EXPORT_PHASE(TL::OpenMP::Lint)
