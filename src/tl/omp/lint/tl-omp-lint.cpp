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

#include "cxx-diagnostic.h"
#include "tl-datareference.hpp"
#include "tl-omp-lint.hpp"
#include "tl-tribool.hpp"

#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <time.h>
#include <unistd.h>

namespace TL { 
namespace OpenMP {
    
namespace {
    std::string log_file_path;
    char log_file_name[PATH_MAX+1];
    std::string usr_name;
    
    #define CORRECTNESS_WARN_TYPE_LIST \
    CORRECTNESS_WARN_TYPE(Dead) \
    CORRECTNESS_WARN_TYPE(IncoherentFirstprivate) \
    CORRECTNESS_WARN_TYPE(IncoherentPrivate) \
    CORRECTNESS_WARN_TYPE(Race) \
    CORRECTNESS_WARN_TYPE(SharedAutoStorage) \
    CORRECTNESS_WARN_TYPE(Unused)
    
    enum CorrectnessWarn_type {
        #undef CORRECTNESS_WARN_TYPE
        #define CORRECTNESS_WARN_TYPE(X) __##X,
        CORRECTNESS_WARN_TYPE_LIST
        #undef CORRECTNESS_WARN_TYPE
    };
    
    inline std::string correctness_warn_type_str(CorrectnessWarn_type cwt)
    {
        std::string warn_t = "";
        switch(cwt)
        {
            #undef CORRECTNESS_WARN_TYPE
            #define CORRECTNESS_WARN_TYPE(X) case __##X : return #X;
            CORRECTNESS_WARN_TYPE_LIST
            #undef CORRECTNESS_WARN_TYPE
            default: WARNING_MESSAGE( "Unexpected type of correctness warnign type '%d'", cwt );
        };
        return warn_t;
    }
}
    
    static bool ompss_mode_enabled = false;
    
    struct FunctionCodeVisitor : Nodecl::ExhaustiveVisitor<void>
    {
        typedef TL::ObjectList<TL::Analysis::Node*> PCFG_Node_list;
        FILE* log_file;
        
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
        
        inline void print_warn_to_file(const Nodecl::NodeclBase& task, CorrectnessWarn_type warn_t)
        {
            if(!log_file_path.empty())
            {
                std::stringstream line_ss; line_ss << task.get_line();
                std::string log = usr_name + " # " 
                                + task.get_filename() + " # "
                                + line_ss.str() + " # " 
                                + correctness_warn_type_str(warn_t) + "\n";
                if(fputs(log.c_str(), log_file) == EOF)
                    internal_error("Unable to write to file '%s', to store a correctness log.", log_file_name);
            }
        }
        
        void visit( const Nodecl::FunctionCode& function_code )
        {
            TL::Analysis::AnalysisSingleton& singleton = TL::Analysis::AnalysisSingleton::get_analysis(ompss_mode_enabled);
            TL::Analysis::PCFGAnalysis_memento memento;
            // We compute liveness analysis (that includes PCFG and use-def) because 
            // we need the information computed by TaskConcurrency (last and next synchronization points of a task)
            TL::ObjectList<TL::Analysis::ExtensibleGraph*> extensible_graphs =
                    singleton.tune_task_synchronizations( memento, function_code );
            ERROR_CONDITION( extensible_graphs.size() != 1, "I expected 1 graph per FunctionCode", 0 );
            
            TL::Analysis::ExtensibleGraph* graph = extensible_graphs[0];
            
            if (CURRENT_CONFIGURATION->debug_options.print_pcfg)
                graph->print_graph_to_dot(/*use_def_computed*/true, /*liveness_computed*/true);
            
            // Create the log file that will store the logs
            if(!log_file_path.empty())
            {
            	// Make sure the logs directory exists
                struct stat st;
                if(stat(log_file_path.c_str(), &st) != 0)
                {   // the directory does not exist
                    int old_mask = umask(0000);
                    int dot_directory = mkdir(log_file_path.c_str(), S_IRWXU|S_IRWXG|S_IRWXO);
                    umask(old_mask);
                    if(dot_directory != 0)
                        internal_error("An error occurred while creating the dot files directory in '%s'", log_file_path.c_str());
                }

                // 1.- Get user name
                char* tmp_usr_name = getenv("USER");
                usr_name = std::string(tmp_usr_name);
                if(usr_name.empty())
                    usr_name = "undefined";
                
                // 2.- Get time
                std::string date_str;
                {
                    time_t t = time(NULL);
                    struct tm* tmp = localtime(&t);
                    if(tmp == NULL)
                    {
                        internal_error("localtime failed", 0);
                    }
                    char outstr[200];
                    if(strftime(outstr, sizeof(outstr), "%s", tmp) == 0)
                    {
                        internal_error("strftime failed", 0);
                    }
                    outstr[199] = '\0';
                    date_str = outstr;
                }
                
                // 3.- Build the name of the file
                char absolute_path[PATH_MAX+1];
                char* path_ptr = realpath(log_file_path.c_str(), absolute_path);
                ERROR_CONDITION(path_ptr == NULL, "Error retrieving the real path of path %s.\n", log_file_path.c_str());
                snprintf(log_file_name, PATH_MAX, "%s/__correctness_%s_%lu_%s.log",
                         absolute_path,
                         usr_name.c_str(), (unsigned long)getppid(), date_str.c_str());
                log_file_name[PATH_MAX-1] = '\0';
                
                // 4.- Create and open the file
                DEBUG_CODE()
                {
                    std::cerr << "OMP-LINT_ The correctness log files for this compilation will be stored in file: '" << log_file_name << "'" << std::endl;
                }
                int old_mask = umask(0022);
                log_file = fopen(log_file_name, "a+");
                umask(old_mask);
                if(log_file == NULL)
                    internal_error("Unable to open the file '%s' to store the correctness logs.", log_file_name);
            }
            
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
                            Nodecl::NodeclBase task = (*it)->get_graph_related_ast();
                            std::string task_locus = task.get_locus_str();
                            std::string tabulation( (task_locus+": omp-warning: ").size( ), ' ' );
                            std::string local_vars_str = get_nodecl_list_str( local_vars );
                            warn_printf( "%s: omp-warning: Local variables '%s' are defined as shared in the task, "
                                         "but their lifetime may have ended when the task is executed.\n"
                                         "%sConsider privatizing them or synchronizing the task before the local data is deallocated.\n", 
                                         task_locus.c_str(), local_vars_str.c_str(), tabulation.c_str() );
                            print_warn_to_file(task, __SharedAutoStorage);
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
                        warn_printf( "%s: omp-warning: Variables '%s' are in a race condition due to a concurrent usage.\n",
                                     task.get_locus_str().c_str(), race_cond_vars_str.c_str() );
                        print_warn_to_file(task, __Race);
                    }
                }
                
                // Incoherent data-sharing
                // Example:
                // #pragma omp task private(x)
                //    x++;
                {
                    check_task_incoherent_data_sharing(*it);
                }
            }
            
            // Close the logs file
            if(!log_file_path.empty())
            {
                fclose(log_file);
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
                Nodecl::List in_alloca = task_pragma_info.get_clause( TL::Analysis::__in ).get_args( );
                result = result || any_data_ref_is_local( in_alloca, local_vars );
            }
            if( task_pragma_info.has_clause( TL::Analysis::__in_alloca ) )
            {
                Nodecl::List in = task_pragma_info.get_clause( TL::Analysis::__in_alloca ).get_args( );
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
        
        static bool enclosing_context_contains_node(TL::Analysis::Node* ctx, TL::Analysis::Node* node)
        {
            bool found = false;
            while(!found && (ctx != NULL))
            {
                found = (ctx == node) || TL::Analysis::ExtensibleGraph::node_contains_node(ctx, node);
                ctx = TL::Analysis::ExtensibleGraph::get_enclosing_context(ctx);
            }
            return found;
        }
        
        // Returns false when task may synchronize at some point 
        // which is not enclosed in the scope where the task is created
        tribool task_only_synchronizes_in_enclosing_scopes( TL::Analysis::Node *n )
        {
            TL::ObjectList<TL::Analysis::Node*> children = n->get_children( );
            ERROR_CONDITION( children.empty( ), 
                             "We should have computed at least some exit edge for this task", 0 );
            
            tribool result = true;
            
            // The parent of a task may be a TASK_CREATION node, or any other TASK node that may synchronize with the task
            ObjectList<TL::Analysis::Node*> task_parents = n->get_parents( );
            TL::Analysis::Node* task_creation = NULL;
            for(ObjectList<TL::Analysis::Node*>::iterator it = task_parents.begin(); it != task_parents.end(); ++it)
            {
                if((*it)->is_omp_task_creation_node())
                {
                    task_creation = *it;
                    break;
                }
            }
            ERROR_CONDITION(task_creation==NULL, "Task creation of node %d not found in its list of parents.\n", n->get_id());
            TL::Analysis::Node* task_creation_sc = TL::Analysis::ExtensibleGraph::get_enclosing_context( task_creation );
            ERROR_CONDITION( task_creation_sc == NULL, 
                             "The context of a task creation node cannot be NULL, but task's %d is NULL.", 
                             n->get_id( ) );
            for( TL::ObjectList<TL::Analysis::Node*>::iterator it = children.begin( ); it != children.end( ); ++it )
            {
                TL::Analysis::Node* sync_sc = TL::Analysis::ExtensibleGraph::get_enclosing_context( *it );
                if( ( sync_sc == NULL ) ||                                  // This a Post_Sync
                    ( !enclosing_context_contains_node(task_creation_sc, sync_sc) ) )
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
            
            source->set_visited( true );
            
            // Treat the current node
            if( source != ommited_node )
            {
                if( source->is_graph_node( ) )
                {
                    compute_usage_between_nodes( source->get_graph_entry_node( ), target, ommited_node, 
                                                 concurrently_used_vars, variables );
                }
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
            if( task_pragma_info.has_clause( TL::Analysis::__in_alloca ) )
            {
                Nodecl::List in_alloca = task_pragma_info.get_clause( TL::Analysis::__in_alloca ).get_args( );
                task_shared_variables.append( in_alloca );
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
            TL::ObjectList<TL::Analysis::Node*> last_sync = pcfg->get_task_last_synchronization( n );
            TL::ObjectList<TL::Analysis::Node*> next_sync = pcfg->get_task_next_synchronization( n );
            
            if(VERBOSE)
            {
                // Get last synchronization node list as a string
                std::string last_syncs;
                for(TL::ObjectList<TL::Analysis::Node*>::iterator it = last_sync.begin(); it != last_sync.end(); )
                {
                    std::stringstream ss; ss << (*it)->get_id();
                    last_syncs +=  ss.str();
                    ++it;
                    if(it != last_sync.end())
                        last_syncs += ", ";
                }
                
                // Get next synchronization node list as a string
                std::string next_syncs;
                for(TL::ObjectList<TL::Analysis::Node*>::iterator it = next_sync.begin(); it != next_sync.end(); )
                {
                    std::stringstream ss; ss << (*it)->get_id();
                    next_syncs +=  ss.str();
                    ++it;
                    if(it != next_sync.end())
                        next_syncs += ", ";
                }
                 
                std::cerr << "OMP-LINT_ Task " << n->get_id() << " has last_syncs(" << last_syncs << ") and next_syncs(" << next_syncs << ")" << std::endl;
            }
            
            // Traverse the graph from last_sync to next_sync looking for uses of the shared variables found in the task
            std::map<Nodecl::NodeclBase, TL::ObjectList<TL::Analysis::Node*> > concurrently_used_vars;
            for( TL::ObjectList<TL::Analysis::Node*>::iterator itl = last_sync.begin( ); itl != last_sync.end( ); ++itl )
                for( TL::ObjectList<TL::Analysis::Node*>::iterator itn = next_sync.begin( ); itn != next_sync.end( ); ++itn )
                    compute_usage_between_nodes( *itl, *itn, n, concurrently_used_vars, task_shared_variables );
            for( TL::ObjectList<TL::Analysis::Node*>::iterator itl = last_sync.begin( ); itl != last_sync.end( ); ++itl )
                TL::Analysis::ExtensibleGraph::clear_visits( *itl );
            std::map<Nodecl::NodeclBase, TL::ObjectList<TL::Analysis::Node*> > task_used_vars;
            TL::Analysis::Node* task_entry = n->get_graph_entry_node( );
            compute_usage_between_nodes( task_entry, n->get_graph_exit_node( ), NULL, task_used_vars, task_shared_variables );
            TL::Analysis::ExtensibleGraph::clear_visits( task_entry );
            
            // Detect data races on the variables that appear in both the task and concurrent code with the task
            TL::Analysis::Utils::ext_sym_set task_defs = n->get_killed_vars( );
                    // To be conservative, the undef. variables count as definitions
            TL::Analysis::Utils::ext_sym_set task_undef = n->get_undefined_behaviour_vars( );  
            task_defs.insert( task_undef.begin( ), task_undef.end( ) );
            std::map<Nodecl::NodeclBase, bool, Nodecl::Utils::Nodecl_structural_less> warned_vars;
            for( std::map<Nodecl::NodeclBase, TL::ObjectList<TL::Analysis::Node*> >::iterator it = concurrently_used_vars.begin( ); 
                 it != concurrently_used_vars.end( ); ++it )
            {
                warned_vars[it->first] = false;
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
                            warned_vars[it->first] = true;
                            break;
                        }
                    }
                    // 2. Check accesses in the task
                    if( !warned_vars[it->first] )
                    {
                        std::map<Nodecl::NodeclBase, TL::ObjectList<TL::Analysis::Node*> >::iterator tmp = task_used_vars.find( it->first );
                        if( tmp == task_used_vars.end( ) )
                        {
                            // FIXME Not really sure about the case being covered within this branch
                            warn_printf( "%s: omp-warning: Variable %s is marked as shared in a task but it is not used inside the task\n", 
                                         task.get_locus_str().c_str(), it->first.prettyprint( ).c_str( ) );
                            print_warn_to_file(task, __Unused);
                        }
                        else
                        {
                            for( TL::ObjectList<TL::Analysis::Node*>::iterator it2 = tmp->second.begin( ); it2 != tmp->second.end( ); ++it2 )
                            {
                                if( !TL::Analysis::ExtensibleGraph::node_is_in_synchronous_construct( *it2 ) )
                                {
                                    result = true;
                                    race_cond_vars.append( it->first );
                                    warned_vars[it->first] = false;
                                    break;
                                }
                            }
                        }
                    }
                }
            }
            
            // If the task is in a parallel construct, then 
            // any shared variable of the task that is not protected 
            // with an atomic or critical construct, are in a race condition
            for(TL::ObjectList<TL::Analysis::Node*>::iterator it = last_sync.begin(); it != last_sync.end(); ++it)
            {
                if((*it)->is_omp_flush_node())
                {
                    // Check that some access is not protected and at least one of the accesses is a write
                    for(std::map<Nodecl::NodeclBase, TL::ObjectList<TL::Analysis::Node*> >::iterator itt = task_used_vars.begin(); 
                        itt != task_used_vars.end(); ++itt)
                    {
                        if (Nodecl::Utils::nodecl_is_in_nodecl_list(itt->first, task_shared_variables) && 
                            !warned_vars[itt->first])
                        {   // Check whether the accesses to the variable are protected or not
                            TL::ObjectList<TL::Analysis::Node*> nodes = itt->second;
                            bool defined = false;
                            bool safe = true;
                            for(TL::ObjectList<TL::Analysis::Node*>::iterator ittt = nodes.begin(); ittt != nodes.end(); ++ittt)
                            {
                                TL::Analysis::Utils::ext_sym_set killed_vars = (*ittt)->get_killed_vars();
                                if(!TL::Analysis::Utils::ext_sym_set_contains_enclosed_nodecl(itt->first, killed_vars).is_null())
                                    defined = true;
                                if(!TL::Analysis::ExtensibleGraph::node_is_in_synchronous_construct(*ittt))
                                    safe = false;
                            }
                            if(!safe && defined)
                            {   // Some access is not protected and at least one of the accesses is a write
                                race_cond_vars.append(itt->first);
                                result = true;
                            }
                        }
                    }
                    
                    break;
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
                    // When a variable has pointer or array type, only the variable itself is included in the data-sharing attributes list
                    // Nonetheless, we also need to check the usage of the pointed values
                    // Thus, we use the ext_sym_set_contains_enclosed_nodecl instead of using ext_sym_set_contains_nodecl
                    if( !TL::Analysis::Utils::ext_sym_set_contains_enclosed_nodecl( n, killed_vars ).is_null() ) {
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
            TL::ObjectList<Nodecl::NodeclBase>::iterator it;
            TL::ObjectList<Nodecl::NodeclBase>::iterator it2 = stmts.end();
            for( it = stmts.begin( ); it != stmts.end( ); ++it )
            {
                wv.walk( *it );
                ObjectList<Nodecl::NodeclBase> defined_syms = wv.get_defined_symbols( );
                if( Nodecl::Utils::list_contains_nodecl( defined_syms, n ) )
                    it2 = it;
                wv.clear( );
            }
            
            // Check the statements after the last definition to check for uses of the variable
            if(it2 != stmts.end())
            {
                it = it2; it++;
                for( ; it != stmts.end( ) && !result; ++it )
                {
                    TL::ObjectList<Nodecl::NodeclBase> mem_accesses = Nodecl::Utils::get_all_memory_accesses( *it );
                    if( Nodecl::Utils::list_contains_nodecl( mem_accesses, n ) )
                        result = true;
                }
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
        
        static bool list_elements_contain_nodecl(const Nodecl::List& list, const Nodecl::NodeclBase& n)
        {
            bool result = false;
            for(Nodecl::List::iterator it = list.begin(); it != list.end(); ++it)
            {
                if(Nodecl::Utils::structurally_equal_nodecls(n, *it, /*skip_conversion_nodes*/true) || 
                   Nodecl::Utils::stmtexpr_contains_nodecl_structurally(*it, n))
                {
                    result = true;
                    break;
                }
            }
            return result;
        }
        
        void check_task_incoherent_data_sharing(TL::Analysis::Node* task)
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
            if( task_pragma_info.has_clause( TL::Analysis::__private ) )
            {
                private_vars = task_pragma_info.get_clause( TL::Analysis::__private ).get_args( );
                all_private_vars.append( private_vars );
                task_scoped_vars.append( private_vars );
            } 
            if( task_pragma_info.has_clause( TL::Analysis::__shared ) )
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
            
            TL::Analysis::Utils::ext_sym_set all_vars = ue_vars;
            all_vars.insert(killed_vars.begin(), killed_vars.end());
            all_vars.insert(undef_vars.begin(), undef_vars.end());
            all_vars.insert(private_ue_vars.begin(), private_ue_vars.end());
            all_vars.insert(private_killed_vars.begin(), private_killed_vars.end());
            all_vars.insert(private_undef_vars.begin(), private_undef_vars.end());
            
            // Collect dependency clauses, for these may use variables that need to be scoped (shape expressions, array subscripts)
            Nodecl::List dependency_vars;
            if( task_pragma_info.has_clause( TL::Analysis::__in ) )
                dependency_vars.append(task_pragma_info.get_clause(TL::Analysis::__in).get_args());
            if( task_pragma_info.has_clause( TL::Analysis::__in_alloca ) )
                dependency_vars.append(task_pragma_info.get_clause(TL::Analysis::__in_alloca).get_args());
            if( task_pragma_info.has_clause( TL::Analysis::__out ) )
                dependency_vars.append(task_pragma_info.get_clause(TL::Analysis::__out).get_args());
            if( task_pragma_info.has_clause( TL::Analysis::__inout ) )
                dependency_vars.append(task_pragma_info.get_clause(TL::Analysis::__inout).get_args());
            
            // Collect the addresses used within the task
            TL::Analysis::Utils::ext_sym_set used_addresses = task->get_used_addresses( );
            
            // Case1: No variable should be scoped if it is not used at all inside the task 
            std::string unnecessarily_scoped_vars;
            for( Nodecl::List::iterator it = task_scoped_vars.begin( ); it != task_scoped_vars.end( ); ++it )
            {
                if( !TL::Analysis::Utils::ext_sym_set_contains_nodecl(*it, all_vars) && 
                    !list_elements_contain_nodecl(dependency_vars, *it) && 
                    TL::Analysis::Utils::ext_sym_set_contains_enclosed_nodecl(*it, used_addresses).is_null() )
                {
                    unnecessarily_scoped_vars += it->prettyprint() + ", ";
                }
            }
            if( !unnecessarily_scoped_vars.empty( ) ) 
            {
                Nodecl::NodeclBase task_nodecl = task->get_graph_related_ast();
                std::string task_locus = task_nodecl.get_locus_str();
                unnecessarily_scoped_vars = unnecessarily_scoped_vars.substr(0, unnecessarily_scoped_vars.size()-2);
                std::string tabulation( (task_locus+": omp-warning: ").size( ), ' ' );
                warn_printf( "%s: omp-warning: Variables '%s' are not used at all within the task "
                             "but they have a data-sharing attribute associated.\n" 
                             "%sConsider removing the attribute.\n",
                             task_locus.c_str(), unnecessarily_scoped_vars.c_str(), tabulation.c_str());
                print_warn_to_file(task_nodecl, __Unused);
            }
            
            // Case2: Any private|firstprivate variable defined within the task must use that definition.
            // Otherwise the variable should be shared or the statement can be removed because it produces dead code
            std::string dead_code_vars;
            for( Nodecl::List::iterator it = all_private_vars.begin( ); it != all_private_vars.end( ); ++it )
            {
                
                if( !TL::Analysis::Utils::ext_sym_set_contains_enclosed_nodecl( *it, killed_vars ).is_null() || 
                    !TL::Analysis::Utils::ext_sym_set_contains_enclosed_nodecl( *it, private_killed_vars ).is_null() )
                {
                    if( !var_is_used_in_task_after_definition( *it, task ) )
                        dead_code_vars += it->prettyprint() + ", ";
                }
            }
            if( !dead_code_vars.empty( ) )
            {
                Nodecl::NodeclBase task_nodecl = task->get_graph_related_ast();
                std::string task_locus = task_nodecl.get_locus_str();
                dead_code_vars = dead_code_vars.substr(0, dead_code_vars.size()-2);
                std::string tabulation( (task_locus+": omp-warning: ").size( ), ' ' );
                warn_printf( "%s: omp-warning: Variables '%s' are scoped as (first)private in the task."
                             "The updates on these variables will not be visible after the task.\n" 
                             "%sConsider defining them as shared or removing the statement writing them.\n",
                             task_locus.c_str(), dead_code_vars.c_str(), tabulation.c_str() );
                print_warn_to_file(task_nodecl, __Dead);
            }
            
            // Case3: Private variables must never be read before they are written
            std::string incoherent_private_vars;
            for( Nodecl::List::iterator it = private_vars.begin( ); it != private_vars.end( ); ++it )
            {
                if( TL::Analysis::Utils::ext_sym_set_contains_nodecl( *it, ue_vars ) || 
                    TL::Analysis::Utils::ext_sym_set_contains_nodecl( *it, private_ue_vars ) )
                    incoherent_private_vars += it->prettyprint() + ", ";
            }
            if( !incoherent_private_vars.empty( ) )
            {
                Nodecl::NodeclBase task_nodecl = task->get_graph_related_ast();
                std::string task_locus = task_nodecl.get_locus_str();
                incoherent_private_vars = incoherent_private_vars.substr(0, incoherent_private_vars.size()-2);
                std::string tabulation( (task_locus+": omp-warning: ").size( ), ' ' );
                warn_printf( "%s: omp-warning: Variables '%s' are scope as private in the task, "
                             "but their input value is used in a serial execution of the code.\n"
                             "%sConsider defining them as firstprivate instead.\n",
                             task_locus.c_str(), incoherent_private_vars.c_str(), tabulation.c_str() );
                print_warn_to_file(task_nodecl, __IncoherentPrivate);
            }
            
            // Case4: Firstprivate variables must never be written before they are read
            // unless they only appear in the directive within a shape expression or an array subscript
            std::string incoherent_firstprivate_vars;
            for( Nodecl::List::iterator it = firstprivate_vars.begin( ); it != firstprivate_vars.end( ); ++it )
            {
                if( !TL::Analysis::Utils::ext_sym_set_contains_nodecl( *it, ue_vars ) && 
                    !TL::Analysis::Utils::ext_sym_set_contains_nodecl( *it, private_ue_vars ) && 
                    TL::Analysis::Utils::ext_sym_set_contains_enclosed_nodecl(*it, used_addresses).is_null() && 
                    !list_elements_contain_nodecl(dependency_vars, *it) )
                    incoherent_firstprivate_vars += it->prettyprint() + ", ";
            }
            if( !incoherent_firstprivate_vars.empty( ) )
            {
                Nodecl::NodeclBase task_nodecl = task->get_graph_related_ast();
                std::string task_locus = task_nodecl.get_locus_str();
                incoherent_firstprivate_vars = incoherent_firstprivate_vars.substr(0, incoherent_firstprivate_vars.size()-2);
                std::string tabulation( (task_locus+": omp-warning: ").size( ), ' ' );
                warn_printf( "%s: omp-warning: Variables '%s' are scoped as firstprivate within the task, "
                             "but their value captured is never read.\n"
                             "%sConsider defining them as private instead.\n",
                             task_locus.c_str(), incoherent_firstprivate_vars.c_str(), tabulation.c_str() );
                print_warn_to_file(task_nodecl, __IncoherentFirstprivate);
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
        : _disable_phase("0"), _correctness_log_path("")
    {
        set_phase_name("OpenMP Lint");
        set_phase_description("This phase is able to detect some common pitfalls when using OpenMP");

        // Register parameters
        register_parameter("disable-omp-lint",
                "Disables this phase. You should not need this. If you do, then it is an error. Please fill a bug",
                _disable_phase,
                "0");
        
        register_parameter("correctness_log_dir",
                "Sets the path where correctness logs will be stored, in addition to showing them in the standard output",
                _correctness_log_path,
                "");
        
        register_parameter("ompss_mode",
                           "Enables OmpSs semantics instead of OpenMP semantics",
                           _ompss_mode_str,
                           "0").connect(functor(&Lint::set_ompss_mode, *this));
    }

    void Lint::run(TL::DTO& dto)
    {
        Nodecl::NodeclBase top_level = dto["nodecl"];

        if (_disable_phase == "0")
        {
            // Get the path to the file where we will store the logs (in addition to showing them in the stdout)
            log_file_path = _correctness_log_path;
            
            ompss_mode_enabled = _ompss_mode_enabled;
            FunctionCodeVisitor function_codes;
            function_codes.walk(top_level);
        }
    }

    void Lint::pre_run(TL::DTO& dto)
    {}
    
    void Lint::set_ompss_mode( const std::string& ompss_mode_str)
    {
        if( ompss_mode_str == "1")
            _ompss_mode_enabled = true;
    }
    
    // ****************************** END OpenMP scope checking phase ****************************** //
    // ********************************************************************************************* //
}
}

EXPORT_PHASE(TL::OpenMP::Lint)
