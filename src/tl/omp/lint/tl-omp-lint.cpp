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
                graph->print_graph_to_dot( );
            
            // Get all task nodes
            PCFG_Node_list tasks = graph->get_tasks_list();
            for( PCFG_Node_list::iterator it = tasks.begin(); it != tasks.end(); it++ )
            {
                // Automatic storage variables as shared
                // Example:
                // {
                //    int a;
                //    #pragma omp task shared(a)
                //       a = 0;
                // }
                {
                    Nodecl::List local_vars;
                    if( task_is_locally_bound(*it, local_vars).is_true( ) )
                    {
                        if( task_only_synchronizes_in_enclosing_scopes(*it).is_false( ) )
                        {
                            std::string task_locus = (*it)->get_graph_related_ast( ).get_locus_str();
                            std::string tabulation( (task_locus+": warning: ").size( ), ' ' );
                            std::string local_vars_str = get_nodecl_list_str( local_vars );
                            warn_printf( "%s: warning: OpenMP task defines as shared local data '%s' "
                                         "whose lifetime may have ended when the task is executed.\n"
                                         "%sConsider privatizing the variable or synchronizing "
                                         "the task before the local data is deallocated.\n", 
                                         task_locus.c_str(), local_vars_str.c_str(), tabulation.c_str() );
                        }
                    }
                }
                
                // Race conditions
                // Example:
                // #pragma omp task
                //    x++;
                // printf("x=%d\n", x);
                {
                    Nodecl::List race_cond_vars;
                    if( task_may_cause_race_condition( graph, *it, race_cond_vars ).is_true( ) )
                    {
                        Nodecl::NodeclBase task = (*it)->get_graph_related_ast( );
                        std::string race_cond_vars_str = get_nodecl_list_str( race_cond_vars );
                        warn_printf( "%s: warning: OpenMP task uses data '%s' "
                                     "which is in a race condition with another usage of the same variable\n",
                                     task.get_locus_str().c_str(), race_cond_vars_str.c_str() );
                        
                    }
                }
                
                // Incoherent data-sharing
                // Example:
                // #pragma omp task private(x)
                //    x++;
                {
                    std::string task_locus = (*it)->get_graph_related_ast().get_locus_str().c_str();
                    check_task_incoherent_data_sharing( *it, task_locus );
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
        
        // Returns false when task may synchronize at some point 
        // which is not enclosed in the scope where the task is created
        tribool task_only_synchronizes_in_enclosing_scopes( TL::Analysis::Node *n )
        {
            TL::ObjectList<TL::Analysis::Node*> children = n->get_children( );
            ERROR_CONDITION( children.empty( ), 
                             "We should have computed at least some exit edge for this task", 0 );
            
            tribool result = true;
            
            ObjectList<TL::Analysis::Node*> task_parents = n->get_parents( );
            ERROR_CONDITION( task_parents.size( ) != 1, 
                             "A task node must have a unique parent, which is the task creation node, "
                             " but task %d has %d parents.", n->get_id( ), task_parents.size( ) );
            TL::Analysis::Node* task_creation = task_parents[0];
            TL::Analysis::Node* task_creation_sc = TL::Analysis::ExtensibleGraph::get_enclosing_context( task_creation );
            ERROR_CONDITION( task_creation_sc == NULL, 
                             "The context of a task creation node cannot be NULL, but task's %d is NULL.", 
                             n->get_id( ) );
            for( TL::ObjectList<TL::Analysis::Node*>::iterator it = children.begin( ); it != children.end( ); ++it )
            {
                TL::Analysis::Node* sync_sc = TL::Analysis::ExtensibleGraph::get_enclosing_context( *it );
                if( ( sync_sc == NULL ) ||                                  // This a Post_Sync
                    ( task_creation_sc != sync_sc &&                        // Task synchronizes in an outbound scope 
                      !TL::Analysis::ExtensibleGraph::node_contains_node( task_creation_sc, sync_sc ) ) )
                {
                    result = false;
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
                            !TL::Analysis::Utils::ext_sym_set_contains_enclosing_nodecl( *it, accessed_vars ).is_null( ) || 
                            !TL::Analysis::Utils::ext_sym_set_contains_enclosed_nodecl( *it, accessed_vars ).is_null( ) )
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
                    TL::Analysis::Node* most_outer_loop = NULL;
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
                if( ( !task_defs.empty( ) &&
                      ( TL::Analysis::Utils::ext_sym_set_contains_nodecl( it->first, task_defs ) || 
                        !TL::Analysis::Utils::ext_sym_set_contains_enclosing_nodecl( it->first, task_defs ).is_null( ) || 
                        !TL::Analysis::Utils::ext_sym_set_contains_enclosed_nodecl( it->first, task_defs ).is_null( ) ) ) || 
                    ( !node_defs.empty( ) &&
                      ( TL::Analysis::Utils::ext_sym_set_contains_nodecl( it->first, node_defs ) || 
                        !TL::Analysis::Utils::ext_sym_set_contains_enclosing_nodecl( it->first, node_defs ).is_null( ) || 
                        !TL::Analysis::Utils::ext_sym_set_contains_enclosed_nodecl( it->first, node_defs ).is_null( ) ) ) )
                {   // If all accesses are protected in a critical/atomic construct, then there is no race condition
                    // 1. Check accesses in the concurrent nodes
                    for( TL::ObjectList<TL::Analysis::Node*>::iterator it2 = it->second.begin( ); it2 != it->second.end( ); ++it2 )
                    {
                        if( !TL::Analysis::ExtensibleGraph::node_is_in_synchronous_construct( *it2 ) )
                        {
                            result = true;
                            race_cond_vars.append( it->first );
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
                                    result = true;
                                    race_cond_vars.append( it->first );
                                    break;
                                }
                            }
                        }
                    }
                }
            }
            
            return result;
        }
        
        TL::Analysis::Node* get_var_last_definition( Nodecl::NodeclBase n, TL::Analysis::Node* task_exit )
        {
            TL::Analysis::Node* result = NULL;
            TL::ObjectList<TL::Analysis::Node*> parents = task_exit->get_parents( );
            TL::Analysis::Utils::ext_sym_set killed_vars;
            while( !parents.empty( ) && ( result == NULL ) )
            {
                TL::ObjectList<TL::Analysis::Node*> new_parents;
                for( TL::ObjectList<TL::Analysis::Node*>::iterator it = parents.begin( ); it != parents.end( ); ++it )
                {
                    killed_vars = (*it)->get_killed_vars( );
                    if( TL::Analysis::Utils::ext_sym_set_contains_nodecl( n, killed_vars ) ) {
                        result = *it;
                        break;
                    }
                    new_parents.insert( (*it)->get_parents( ) );
                }
                if( result != NULL && result->is_graph_node( ) )
                {
                    parents = result->get_graph_exit_node( )->get_parents( );
                    result = NULL;
                }
                else
                    parents = new_parents;
            }
                
            return result;
        }
        
        bool var_is_used_in_node_after_definition( TL::Analysis::Node* node, const Nodecl::NodeclBase& n )
        {
            bool result = false;
            
            // Get the last statement in the node that defines 'n'
            TL::ObjectList<Nodecl::NodeclBase> stmts = node->get_statements( );
            WritesVisitor wv;
            TL::ObjectList<Nodecl::NodeclBase>::iterator it, it2;
            for( it = stmts.begin( ); it != stmts.end( ); ++it )
            {
                wv.walk( *it );
                ObjectList<Nodecl::NodeclBase> defined_syms = wv.get_defined_symbols( );
                if( Nodecl::Utils::list_contains_nodecl( defined_syms, n ) )
                    it2 = it;
                wv.clear( );
            }
            
            // Check the statements after the last definition to check for uses of the variable
            it = it2; it++;
            for( ; it != stmts.end( ) && !result; ++it )
            {
                TL::ObjectList<Nodecl::NodeclBase> mem_accesses = Nodecl::Utils::get_all_memory_accesses( *it );
                if( Nodecl::Utils::list_contains_nodecl( mem_accesses, n ) )
                    result = true;
            }
            
            return result;
        }
        
        bool var_is_used_between_nodes( TL::Analysis::Node* source, TL::Analysis::Node* target, const Nodecl::NodeclBase& n )
        {
            if( ( source == target ) || source->is_exit_node( ) )
                return false;
            
            bool result = false;
            
            if( !source->is_visited( ) )
            {
                source->set_visited( true );
                
                // Treat the current node
                if( source->is_graph_node( ) )
                    result = var_is_used_between_nodes( source->get_graph_entry_node( ), target, n );
                else if( source->has_statements( ) )
                {
                    TL::ObjectList<Nodecl::NodeclBase> stmts = source->get_statements( );
                    for( TL::ObjectList<Nodecl::NodeclBase>::iterator it = stmts.begin( ); it != stmts.end( ) && !result; ++it )
                    {
                        TL::ObjectList<Nodecl::NodeclBase> mem_accesses = Nodecl::Utils::get_all_memory_accesses( *it );
                        if( Nodecl::Utils::list_contains_nodecl( mem_accesses, n ) )
                        {
                            result = true;
                            break;
                        }
                    }
                }
                
                // Treat the children
                if( !result )
                {
                    TL::ObjectList<TL::Analysis::Node*> children = source->get_children( );
                    for( TL::ObjectList<TL::Analysis::Node*>::iterator it = children.begin( ); ( it != children.end( ) ) && !result; ++it )
                    {
                        result = var_is_used_between_nodes( *it, target, n );
                    }
                }
            }
            
            return result;
        }
        
        bool var_is_used_in_task_after_definition( const Nodecl::NodeclBase& n, TL::Analysis::Node* task )
        {
            bool result = false;
            
            TL::Analysis::Node* task_exit = task->get_graph_exit_node( );
            TL::Analysis::Node* last_definition = get_var_last_definition( n, task_exit );
            ERROR_CONDITION( last_definition == NULL, 
                             "Variable '%s' is defined inside task %d, but the definition has not been found\n", 
                             n.prettyprint( ).c_str( ), task->get_id( ) );
            
            // Check is the variable is used in the same node it is defined, after the definition
            result = var_is_used_in_node_after_definition( last_definition, n );
            
            // Check is the variable is used between the node it is defined and the end of the task
            if( !result )
            {
                last_definition->set_visited( true );
                ObjectList<TL::Analysis::Node*> children = last_definition->get_children( );
                for( TL::ObjectList<TL::Analysis::Node*>::iterator it = children.begin( ); it != children.end( ) && !result; ++it )
                {
                    result = var_is_used_between_nodes( *it, task_exit, n );
                }
                TL::Analysis::ExtensibleGraph::clear_visits_backwards( last_definition );
            }
            
            return result;
        }
        
        void check_task_incoherent_data_sharing( TL::Analysis::Node *task, std::string task_locus )
        {
            // Collect all symbols/data references appearing in data-sharing clauses
            Nodecl::List firstprivate_vars, private_vars, all_private_vars, task_scoped_vars;
            TL::Analysis::PCFGPragmaInfo task_pragma_info = task->get_pragma_node_info( );
            if( task_pragma_info.has_clause( TL::Analysis::__firstprivate ) )
            {
                firstprivate_vars = task_pragma_info.get_clause( TL::Analysis::__firstprivate ).get_args( );
                all_private_vars.append( firstprivate_vars );
                task_scoped_vars.append( firstprivate_vars );
            } 
            else if( task_pragma_info.has_clause( TL::Analysis::__private ) )
            {
                private_vars = task_pragma_info.get_clause( TL::Analysis::__private ).get_args( );
                all_private_vars.append( private_vars );
                task_scoped_vars.append( private_vars );
            } 
            else if( task_pragma_info.has_clause( TL::Analysis::__shared ) )
            {
                Nodecl::List shared_vars = task_pragma_info.get_clause( TL::Analysis::__shared ).get_args( );
                task_scoped_vars.append( shared_vars );
            }
            
            // Collect usage of variables inside the task
            TL::Analysis::Utils::ext_sym_set ue_vars = task->get_ue_vars( );
            TL::Analysis::Utils::ext_sym_set killed_vars = task->get_killed_vars( );
            TL::Analysis::Utils::ext_sym_set undef_vars = task->get_undefined_behaviour_vars( );
            
            TL::Analysis::Utils::ext_sym_set private_ue_vars = task->get_private_ue_vars( );
            TL::Analysis::Utils::ext_sym_set private_killed_vars = task->get_private_killed_vars( );
            TL::Analysis::Utils::ext_sym_set private_undef_vars = task->get_private_undefined_behaviour_vars( );
            
            // Case1: No variable should be scoped if it is not used at all inside the task 
            Nodecl::List unnecessarily_scoped_vars;
            for( Nodecl::List::iterator it = task_scoped_vars.begin( ); it != task_scoped_vars.end( ); ++it )
            {
                if( !TL::Analysis::Utils::ext_sym_set_contains_nodecl( *it, ue_vars ) && 
                    !TL::Analysis::Utils::ext_sym_set_contains_nodecl( *it, killed_vars ) && 
                    !TL::Analysis::Utils::ext_sym_set_contains_nodecl( *it, undef_vars ) && 
                    !TL::Analysis::Utils::ext_sym_set_contains_nodecl( *it, private_ue_vars ) && 
                    !TL::Analysis::Utils::ext_sym_set_contains_nodecl( *it, private_killed_vars ) && 
                    !TL::Analysis::Utils::ext_sym_set_contains_nodecl( *it, private_undef_vars ) )
                {
                    unnecessarily_scoped_vars.append( *it );
                }
            }
            if( !unnecessarily_scoped_vars.empty( ) ) 
            {
                warn_printf( "%s: warning: OpenMP task defines the scope of the variables '%s' "
                             "which are not used at all within the task\n",
                             task_locus.c_str(), unnecessarily_scoped_vars.prettyprint().c_str() );
            }
            
            // Case2: Any private|firstprivate variable defined within the task must use that definition.
            // Otherwise the variable should be shared or the statement can be removed because it produces dead code
            Nodecl::List dead_code_vars;
            for( Nodecl::List::iterator it = all_private_vars.begin( ); it != all_private_vars.end( ); ++it )
            {
                if( TL::Analysis::Utils::ext_sym_set_contains_nodecl( *it, killed_vars ) || 
                    TL::Analysis::Utils::ext_sym_set_contains_nodecl( *it, private_killed_vars ) )
                {
                    if( !var_is_used_in_task_after_definition( *it, task ) )
                        dead_code_vars.append( *it );
                }
            }
            if( !dead_code_vars.empty( ) )
            {
                std::string tabulation( (task_locus+": warning: ").size( ), ' ' );
                warn_printf( "%s: warning: OpenMP task defines as (first)private the variables '%s' "
                             "and these variables are written inside the task, but never used.\n" 
                             "%sConsider defining them as shared or "
                             "removing the statement writing them because it is dead code.\n",
                             task_locus.c_str(), dead_code_vars.prettyprint().c_str(), tabulation.c_str() );
            }
            
            // Case3: Private variables must never be read before they are written
            Nodecl::List incoherent_private_vars;
            for( Nodecl::List::iterator it = private_vars.begin( ); it != private_vars.end( ); ++it )
            {
                if( TL::Analysis::Utils::ext_sym_set_contains_nodecl( *it, ue_vars ) || 
                    TL::Analysis::Utils::ext_sym_set_contains_nodecl( *it, private_ue_vars ) )
                    incoherent_private_vars.append( *it );
            }
            if( !incoherent_private_vars.empty( ) )
            {
                std::string tabulation( (task_locus+": warning: ").size( ), ' ' );
                warn_printf( "%s: warning: OpenMP task defines as private the variables '%s' "
                             "but those variables are upwards exposed.\n"
                             "%sConsider defining them as firstprivate instead.\n",
                             task_locus.c_str(), incoherent_private_vars.prettyprint().c_str(), tabulation.c_str() );
            }
            
            // Case4: Firstprivate variables must never be written before they are read
            Nodecl::List incoherent_firstprivate_vars;
            for( Nodecl::List::iterator it = firstprivate_vars.begin( ); it != firstprivate_vars.end( ); ++it )
            {
                if( !TL::Analysis::Utils::ext_sym_set_contains_nodecl( *it, ue_vars ) && 
                    !TL::Analysis::Utils::ext_sym_set_contains_nodecl( *it, private_ue_vars ) )
                    incoherent_firstprivate_vars.append( *it );
            }
            if( !incoherent_firstprivate_vars.empty( ) )
            {
                std::string tabulation( (task_locus+": warning: ").size( ), ' ' );
                warn_printf( "%s: warning: OpenMP task defines as firstprivate the variables '%s' "
                             "but those variables are not upwards exposed.\n"
                             "%sConsider defining them as private instead.\n",
                             task_locus.c_str(), incoherent_firstprivate_vars.prettyprint().c_str(), tabulation.c_str() );
            }
        }
    };
    

    
    // ********************************************************************************************* //
    // ******************************* Visitor for writing statements ****************************** //
    
    WritesVisitor::WritesVisitor( )
        : _defined_vars( ), _define( false )
    {}
    
    ObjectList<Nodecl::NodeclBase> WritesVisitor::get_defined_symbols( )
    {
        return _defined_vars;
    }
    
    void WritesVisitor::clear( )
    {
        _defined_vars.clear( );
        _define = false;
    }
    
    void WritesVisitor::visit_assignment( const Nodecl::NodeclBase& lhs, const Nodecl::NodeclBase& rhs )
    {
        _define = true;
        walk( lhs );
        _define = false;
        
        walk( rhs );
    }
    
    void WritesVisitor::visit_xx_crement( const Nodecl::NodeclBase& rhs )
    {
        _define = true;
        walk( rhs );
        _define = false;
    }
    
    void WritesVisitor::visit( const Nodecl::AddAssignment& n )
    {
        visit_assignment( n.get_lhs( ), n.get_rhs( ) );
    }
    
    void WritesVisitor::visit( const Nodecl::ArithmeticShrAssignment& n )
    {
        visit_assignment( n.get_lhs( ), n.get_rhs( ) );
    }
    
    void WritesVisitor::visit( const Nodecl::ArraySubscript& n )
    {
        if( _define )
            _defined_vars.insert( n );
        
        walk( n.get_subscripts( ) );
    }
    
    void WritesVisitor::visit( const Nodecl::Assignment& n )
    {
        visit_assignment( n.get_lhs( ), n.get_rhs( ) );
    }
    
    void WritesVisitor::visit( const Nodecl::BitwiseAndAssignment& n )
    {
        visit_assignment( n.get_lhs( ), n.get_rhs( ) );
    }
    
    void WritesVisitor::visit( const Nodecl::BitwiseOrAssignment& n )
    {
        visit_assignment( n.get_lhs( ), n.get_rhs( ) );
        }
    
    void WritesVisitor::visit( const Nodecl::BitwiseShlAssignment& n )
    {
        visit_assignment( n.get_lhs( ), n.get_rhs( ) );
    }
    
    void WritesVisitor::visit( const Nodecl::BitwiseShrAssignment& n )
    {
        visit_assignment( n.get_lhs( ), n.get_rhs( ) );
    }
    
    void WritesVisitor::visit( const Nodecl::BitwiseXorAssignment& n )
    {
        visit_assignment( n.get_lhs( ), n.get_rhs( ) );
    }
    
    void WritesVisitor::visit( const Nodecl::ClassMemberAccess& n )
    {
        if( _define )
            _defined_vars.insert( n );
    }
    
    void WritesVisitor::visit( const Nodecl::Dereference& n )
    {
        if( _define )
            _defined_vars.insert( n );
    }
    
    void WritesVisitor::visit( const Nodecl::DivAssignment& n )
    {
        visit_assignment( n.get_lhs( ), n.get_rhs( ) );
    }
    
    void WritesVisitor::visit( const Nodecl::MinusAssignment& n )
    {
        visit_assignment( n.get_lhs( ), n.get_rhs( ) );
    }
    
    void WritesVisitor::visit( const Nodecl::ModAssignment& n )
    {
        visit_assignment( n.get_lhs( ), n.get_rhs( ) );
    }
    
    void WritesVisitor::visit( const Nodecl::MulAssignment& n )
    {
        visit_assignment( n.get_lhs( ), n.get_rhs( ) );
    }
    
    void WritesVisitor::visit( const Nodecl::ObjectInit& n )
    {
        if( !n.get_symbol( ).get_value( ).is_null( ) )
        {
            Nodecl::Symbol n_sym = Nodecl::Symbol::make( n.get_symbol( ), n.get_locus( ) );
            _defined_vars.insert( n_sym );
        }
    }
    
    void WritesVisitor::visit( const Nodecl::Postdecrement& n )
    {
        visit_xx_crement( n.get_rhs( ) );
    }
    
    void WritesVisitor::visit( const Nodecl::Postincrement& n )
    {
        visit_xx_crement( n.get_rhs( ) );
    }
    
    void WritesVisitor::visit( const Nodecl::Predecrement& n )
    {
        visit_xx_crement( n.get_rhs( ) );
    }
    
    void WritesVisitor::visit( const Nodecl::Preincrement& n )
    {
        visit_xx_crement( n.get_rhs( ) );
    }
    
    void WritesVisitor::visit( const Nodecl::Reference& n )
    {
        if( _define )
            _defined_vars.insert( n );
    }
    
    void WritesVisitor::visit( const Nodecl::Symbol& n )
    {
        if( _define )
            _defined_vars.insert( n );
    }
    
    // ***************************** END Visitor for writing statements **************************** //
    // ********************************************************************************************* //
    
    
    
    // ********************************************************************************************* //
    // ******************************** OpenMP scope checking phase ******************************** //
    
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
    
    // ****************************** END OpenMP scope checking phase ****************************** //
    // ********************************************************************************************* //
}
}

EXPORT_PHASE(TL::OpenMP::Lint)
