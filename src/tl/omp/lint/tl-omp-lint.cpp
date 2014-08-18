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
    
    static bool ompss_mode_enabled = false;
    
    std::string log_file_path;
    char log_file_name[PATH_MAX+1];
    std::string usr_name;
    static FILE* log_file = NULL;
    
    #define CORRECTNESS_WARN_TYPE_LIST \
    CORRECTNESS_WARN_TYPE(FP_Dead) \
    CORRECTNESS_WARN_TYPE(P_Dead) \
    CORRECTNESS_WARN_TYPE(FP_Incoherent) \
    CORRECTNESS_WARN_TYPE(P_Incoherent) \
    CORRECTNESS_WARN_TYPE(IN_Incoherent) \
    CORRECTNESS_WARN_TYPE(OUT_Incoherent) \
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
            default: WARNING_MESSAGE( "Unexpected type of correctness warning type '%d'", cwt );
        };
        return warn_t;
    }
    
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
    
    inline void print_warn_to_file(const Nodecl::NodeclBase& task, CorrectnessWarn_type warn_t, const std::string& vars)
    {
        if(!log_file_path.empty())
        {
            std::stringstream line_ss; line_ss << task.get_line();
            std::string log = usr_name + " # " 
                            + task.get_filename() + " # "
                            + line_ss.str() + " # " 
                            + correctness_warn_type_str(warn_t) 
                            + vars + "\n";
            if(fputs(log.c_str(), log_file) == EOF)
                internal_error("Unable to write to file '%s', to store a correctness log.", log_file_name);
        }
    }
    
    void get_message_common_info(
            TL::Analysis::Node* task, 
            std::string& task_locus, 
            std::string& task_label, 
            std::string& tabulation)
    {
        // Get task locus
        task_locus = task->get_graph_related_ast().get_locus_str();
        // Get task label, if it has
        TL::Analysis::PCFGPragmaInfo pragma_info(task->get_pragma_node_info());
        if(pragma_info.has_clause(NODECL_OPEN_M_P_TASK_LABEL))
            task_label = "::" + pragma_info.get_clause(NODECL_OPEN_M_P_TASK_LABEL).as<Nodecl::OpenMP::TaskLabel>().get_text();
        // Compute tabulation
        tabulation = std::string((task_locus + task_label + ": omp-warning: ").size(), ' ');
    }
    
    std::string get_auto_storage_message(bool use_plural, TL::Analysis::Node* task)
    {
        std::string task_locus, task_label, tabulation;
        get_message_common_info(task, task_locus, task_label, tabulation);
        // Build the message
        if(use_plural)
        {
            return task_locus + task_label + ": omp-warning: Local variables '%s' are shared, "
                   "but their lifetime may have ended when the task is executed.\n" +
                   tabulation + "Consider privatizing them or synchronizing the task before the local data is deallocated.\n";
        }
        else
        {
            return task_locus + task_label + ": omp-warning: Local variable '%s' is shared, "
                   "but its lifetime may have ended when the task is executed.\n" +
                   tabulation + "Consider privatizing the variable or synchronizing the task before the local data is deallocated.\n";
        }
    }
    
    std::string get_race_message(bool use_plural, TL::Analysis::Node* task)
    {
        std::string task_locus, task_label, tabulation;
        get_message_common_info(task, task_locus, task_label, tabulation);
        // Build the message
        if(use_plural)
        {
            return task_locus + task_label + ": omp-warning: Variables '%s' are in a race condition due to a concurrent usage.\n" + 
                   tabulation + "Consider synchronizing all concurrent accesses or privatizing the variables.\n";
        }
        else
        {
            return task_locus + task_label + ": omp-warning: Variable '%s' is in a race condition due to a concurrent usage.\n" + 
                   tabulation + "Consider synchronizing all concurrent accesses or privatizing the variable.\n";
        }
    }
    
    std::string get_dead_vars_message(bool use_plural, std::string data_sharing_atr, TL::Analysis::Node* task)
    {
        std::string task_locus, task_label, tabulation;
        get_message_common_info(task, task_locus, task_label, tabulation);
        // Build the message
        if(use_plural)
        {
            return task_locus + task_label + ": omp-warning: Variables '%s' are " + data_sharing_atr + ", " +
                   "therefore, updates on these variables will not be visible after the task.\n" +
                   tabulation + "Consider defining them as shared.\n";
        }
        else
        {
            return task_locus + task_label + ": omp-warning: Variable '%s' is " + data_sharing_atr + ", " +
                   "therefore, updates on this variable will not be visible after the task.\n" +
                   tabulation + "Consider defining it as shared.\n";
        }
    }
    
    std::string get_unused_scope_message(bool use_plural, TL::Analysis::Node* task)
    {
        std::string task_locus, task_label, tabulation;
        get_message_common_info(task, task_locus, task_label, tabulation);
        // Build the message
        if(use_plural)
        {
            return task_locus + task_label + ": omp-warning: Variables '%s' are not used within the task but they have been scoped.\n" +
                   tabulation + "This may slow down your application. Consider removing the data-sharing attributes.\n";
        }
        else
        {
            return task_locus + task_label + ": omp-warning: Variable '%s' is not used within the task but it has been scoped.\n" +
                   tabulation + "This may slow down your application. Consider removing the data-sharing attribute.\n";
        }
    }
    
    std::string get_incoherent_private_message(bool use_plural, TL::Analysis::Node* task)
    {
        std::string task_locus, task_label, tabulation;
        get_message_common_info(task, task_locus, task_label, tabulation);
        // Build the message
        if(use_plural)
        {
            return task_locus + task_label + ": omp-warning: Variables '%s' are private in the task, " +
                   "but their input value would have been used in a serial execution.\n" +
                   tabulation + "Consider defining them as firstprivate instead, to capture the initial value.\n";
        }
        else
        {
            return task_locus + task_label + ": omp-warning: Variable '%s' is private in the task, " +
                   "but its input value would have been used in a serial execution.\n" +
                   tabulation + "Consider defining it as firstprivate instead, to capture the initial value.\n";
        }
    }
    
    std::string get_incoherent_firstprivate_message(bool use_plural, TL::Analysis::Node* task)
    {
        std::string task_locus, task_label, tabulation;
        get_message_common_info(task, task_locus, task_label, tabulation);
        // Build the message
        if(use_plural)
        {
            return task_locus + task_label + ": omp-warning: Variables '%s' are firstprivate in the task, " +
                   "but their input value is never read.\n" +
                   tabulation + "Consider defining them as private instead.\n";
        }
        else
        {
            return task_locus + task_label + ": omp-warning: Variable '%s' is firstprivate in the task, " +
                   "but its input value is never read.\n" +
                   tabulation + "Consider defining it as private instead\n";
        }
    }
    
    std::string get_incoherent_in_deps_message(
            bool use_plural,
            bool pointed_obj_used, 
            TL::Analysis::Node* task)
    {
        std::string task_locus, task_label, tabulation;
        get_message_common_info(task, task_locus, task_label, tabulation);
        // Build the message
        if(pointed_obj_used)
        {
            if(use_plural)
            {
                return task_locus + task_label + ": omp-warning: Variables '%s' are IN dependences, " +
                    "but their input values are not read in the task. Instead, the object pointed by these variables is read.\n" +
                    tabulation + "Consider defining the dependencies on the pointed objects.\n";
            }
            else
            {
                return task_locus + task_label + ": omp-warning: Variable '%s' is an IN dependency, " +
                    "but its input value is not read in the task. Instead, the object pointed by this variable is read\n" +
                    tabulation + "Consider defining the dependency on the pointed object.\n";
            }
        }
        else
        {
            if(use_plural)
            {
                return task_locus + task_label + ": omp-warning: Variables '%s' are IN dependences, " +
                    "but their input values are not read in the task.\n" +
                    tabulation + "Consider removing these dependencies.\n";
            }
            else
            {
                return task_locus + task_label + ": omp-warning: Variable '%s' is an IN dependency, " +
                    "but its input value is not read in the task.\n" +
                    tabulation + "Consider removing this dependency.\n";
            }
        }
        
    }
    
    std::string get_incoherent_out_deps_message(
            bool use_plural,
            bool is_pointer_var, 
            TL::Analysis::Node* task)
    {
        std::string task_locus, task_label, tabulation;
        get_message_common_info(task, task_locus, task_label, tabulation);
        // Build the part of the message that depends on whether the variables are pointers or not
        std::string tmp = (is_pointer_var ? "and " + std::string(use_plural ? "their" : "its") + " pointed object are" 
                                          : "is");
        // Build the message
        if(use_plural)
        {
            return task_locus + task_label + ": omp-warning: Variables '%s' are OUT dependences, " +
                   "but they " + tmp + " not written in the task.\n" +
                   tabulation + "Consider removing this dependency.\n";
        }
        else
        {
            return task_locus + task_label + ": omp-warning: Variable '%s' is an OUT dependency, " +
                   "but the variable " + tmp + " not written in the task.\n" +
                   tabulation + "Consider removing this dependency.\n";
        }
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
    
    tribool any_symbol_is_local(const TL::Analysis::NodeclSet& item_list, Nodecl::List& local_syms)
    {
        tribool result( false );
        for (TL::Analysis::NodeclSet::const_iterator it = item_list.begin(); it != item_list.end(); it++)
            result = result || symbol_is_local(*it, local_syms);
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
        
        const TL::Analysis::NodeclSet& shared_vars = n->get_all_shared_variables();
        return any_symbol_is_local(shared_vars, local_vars);
    }
    
    bool enclosing_context_contains_node(TL::Analysis::Node* ctx, TL::Analysis::Node* node)
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
    tribool task_only_synchronizes_in_enclosing_scopes(
            TL::Analysis::Node *task, 
            const Nodecl::List& local_vars)
    {
        // Get the task creation node of the task and its children (synchronization points)
        TL::Analysis::Node* task_creation = TL::Analysis::ExtensibleGraph::get_task_creation_node(task);
        ERROR_CONDITION (task_creation==NULL, 
                         "Task creation of node %d not found in its list of parents.\n", 
                         task->get_id());
        const TL::ObjectList<TL::Analysis::Node*>& children = task->get_children();
        
        for(Nodecl::List::const_iterator itv = local_vars.begin(); itv != local_vars.end(); ++itv)
        {
            // 1.- Find the context node of the PCFG where the variable was declared
            // FIXME Maybe we can use itv->retrieve_context insted, but I don't know whether they return the same value
            Scope var_sc(TL::Analysis::Utils::get_nodecl_base(*itv).get_symbol().get_scope());
            TL::Analysis::Node* var_ctx_node = NULL;
            TL::Analysis::Node* task_outer = task_creation->get_outer_node();
            while(task_outer != NULL)
            {
                if(task_outer->is_context_node())
                {
                    const Nodecl::NodeclBase& ctx = task_outer->get_graph_related_ast();
                    Scope ctx_sc(ctx.retrieve_context());
                    if((ctx_sc == var_sc) || var_sc.scope_is_enclosed_by(ctx_sc))
                    {   // We have found the scope where the local variable was declared
                        var_ctx_node = task_outer;
                        goto check_sync;
                    }
                }
                task_outer = task_outer->get_outer_node();
            }
            // We have not found the context in the PCFG where the variable was declared
            internal_error ("PCFG context of local variable %s has not been found.", 
                            itv->prettyprint().c_str());
            
check_sync:
            // 2.- Check whether the context of the variable contains all synchronizations of the task
            for (TL::ObjectList<TL::Analysis::Node*>::const_iterator it = children.begin(); 
                 it != children.end(); ++it)
            {
                if((*it)->is_omp_virtual_tasksync())
                    return false;
                
                TL::Analysis::Node* sync = ((*it)->is_omp_task_node() ? TL::Analysis::ExtensibleGraph::get_task_creation_node(*it) 
                                                                      : *it);
                if(!TL::Analysis::ExtensibleGraph::node_contains_node(var_ctx_node, sync))
                    return false;
            }
        }
        
        return true;
    }
    
    typedef std::map<Nodecl::NodeclBase, TL::ObjectList<TL::Analysis::Node*>, Nodecl::Utils::Nodecl_structural_less> VarToNodesMap;
    
    // This method returns in #used_vars a relation of variables and 
    // the nodes where these variables have an access (use|definition|undefined)
    void compute_usage_between_nodes(
            TL::Analysis::Node* current,
            TL::Analysis::Node* source,
            TL::Analysis::Node* target,
            TL::Analysis::Node* ommited_node,
            VarToNodesMap& used_vars,
            const TL::Analysis::NodeclSet& variables, 
            bool skip_other_tasks)
    {
        if( current->is_visited( ) || current == target )
            return;
        
        current->set_visited( true );
        
        // Treat the current node
        if( current != ommited_node )
        {
            if( current->is_graph_node( ) )
            {
                compute_usage_between_nodes(current->get_graph_entry_node(), source, target, ommited_node, 
                                            used_vars, variables, skip_other_tasks);
            }
            else if( current->has_statements( ) )
            {
                TL::Analysis::NodeclSet ue = current->get_ue_vars( );
                TL::Analysis::NodeclSet kill = current->get_killed_vars( );
                TL::Analysis::NodeclSet undef = current->get_undefined_behaviour_vars( );
                
                TL::Analysis::NodeclSet accessed_vars;
                accessed_vars.insert( ue.begin( ), ue.end( ) );
                accessed_vars.insert( kill.begin( ), kill.end( ) );
                accessed_vars.insert( undef.begin( ), undef.end( ) );
                
                for (TL::Analysis::NodeclSet::const_iterator it = variables.begin(); it != variables.end(); ++it)
                {
                    // If the variable #*it or a subpart/superpart have some usage, we add it to the map
                    if( TL::Analysis::Utils::nodecl_set_contains_nodecl( *it, accessed_vars ) || 
                        !TL::Analysis::Utils::nodecl_set_contains_enclosing_nodecl( *it, accessed_vars ).is_null( ) || 
                        !TL::Analysis::Utils::nodecl_set_contains_enclosed_nodecl( *it, accessed_vars ).is_null( ) )
                    {
                        if( used_vars.find( *it ) != used_vars.end( ) )
                            used_vars[*it].insert( current );
                        else
                            used_vars.insert( 
                                    std::pair<Nodecl::NodeclBase, TL::ObjectList<TL::Analysis::Node*> >( 
                                    *it, TL::ObjectList<TL::Analysis::Node*>( 1, current ) ) );
                    }
                }
            }
        }
        
        // Treat the children
        TL::ObjectList<TL::Analysis::Node*> children;
        if( current->is_exit_node( ) )
        {   // Check we are not exiting the scope of the task scheduling point
            TL::Analysis::Node* outer = current->get_outer_node();
            if((outer != NULL) && TL::Analysis::ExtensibleGraph::node_contains_node(outer, source))
                children = current->get_outer_node()->get_children();
        }
        else
            children = current->get_children( );
        for( TL::ObjectList<TL::Analysis::Node*>::iterator it = children.begin( ); it != children.end( ); ++it )
        {
            if(!skip_other_tasks || !(*it)->is_omp_task_node())
                compute_usage_between_nodes(*it, source, target, ommited_node, used_vars, variables, skip_other_tasks);
        }
    }
    
    tribool task_may_cause_race_condition (TL::Analysis::ExtensibleGraph* pcfg, 
                                           TL::Analysis::Node *task, Nodecl::List& race_cond_vars)
    {
        // 1.- Collect all symbols/data references that may cause a race condition
        TL::Analysis::NodeclSet task_shared_variables = task->get_all_shared_variables();
        
        // 2.- Traverse the graph from last_sync to next_sync looking for uses of the shared variables found in the task
        // 2.1.- Get the previous and next synchronization points (computed during Liveness analysis)
        TL::ObjectList<TL::Analysis::Node*> last_sync = pcfg->get_task_last_synchronization(task);
        TL::ObjectList<TL::Analysis::Node*> next_sync = pcfg->get_task_next_synchronization(task);
        TL::ObjectList<TL::Analysis::Node*> concurrent_tasks = pcfg->get_task_concurrent_tasks(task);
        if(VERBOSE)
        {
            std::cerr << "OMP-LINT_ Task " << task->get_id()
                      << "  |  last_syncs:" << print_node_list(last_sync)
                      << "  |  next_syncs:" << print_node_list(next_sync)
                      << "  |  concurrent tasks: " << print_node_list(concurrent_tasks) << std::endl;
#if 0
            std::cerr << "Shared variables:";
            for (TL::Analysis::NodeclSet::iterator it = task_shared_variables.begin();
                 it != task_shared_variables.end(); )
            {
                std::cerr << it->prettyprint();
                ++it;
                if(it != task_shared_variables.end())
                    std::cerr << ", ";
            }
            std::cerr << std::endl;
#endif
        }
        
        tribool result = false;
        
        // 3.2.- Get shared variables used within the task
        VarToNodesMap task_used_vars;
        TL::Analysis::Node* task_entry = task->get_graph_entry_node();
        compute_usage_between_nodes(task_entry, task_entry, task->get_graph_exit_node(), NULL, 
                                    task_used_vars, task_shared_variables, /*skip_other_tasks*/ true);
        TL::Analysis::ExtensibleGraph::clear_visits_in_level(task_entry, task);

        // 3.3.- Get shared variables used in sequential code concurrent with the task
        VarToNodesMap concurrently_used_vars;
        for( TL::ObjectList<TL::Analysis::Node*>::iterator itl = last_sync.begin( ); itl != last_sync.end( ); ++itl )
            for( TL::ObjectList<TL::Analysis::Node*>::iterator itn = next_sync.begin( ); itn != next_sync.end( ); ++itn )
                compute_usage_between_nodes(*itl, *itl, *itn, task, 
                                            concurrently_used_vars, task_shared_variables, /*skip_other_tasks*/ true);
        for( TL::ObjectList<TL::Analysis::Node*>::iterator itl = last_sync.begin( ); itl != last_sync.end( ); ++itl )
            TL::Analysis::ExtensibleGraph::clear_visits( *itl );
        
        // 3.4.- Get shared variables used in tasks concurrent with the task
        for (ObjectList<TL::Analysis::Node*>::iterator it = concurrent_tasks.begin(); it != concurrent_tasks.end(); ++it)
        {
            TL::Analysis::Node* concurrent_task_entry = (*it)->get_graph_entry_node();
            compute_usage_between_nodes(concurrent_task_entry, concurrent_task_entry, (*it)->get_graph_exit_node(), task, 
                                        concurrently_used_vars, task_shared_variables, /*skip_other_tasks*/ false);
            TL::Analysis::ExtensibleGraph::clear_visits_in_level(concurrent_task_entry, (*it));
        }
        
        // 3.5.- Detect data races on the variables that appear in both the task and concurrent code with the task
        // 3.5.1.- Get the variables that are defined in the task node (at least one of the accesses must be a write)
        TL::Analysis::NodeclSet task_defs = task->get_killed_vars( );
            // To be conservative, the undef. variables count as definitions
            TL::Analysis::NodeclSet task_undef = task->get_undefined_behaviour_vars( );
            task_defs.insert( task_undef.begin( ), task_undef.end( ) );
            
        std::set<Nodecl::NodeclBase, Nodecl::Utils::Nodecl_structural_less> warned_vars;
        for (VarToNodesMap::iterator it = concurrently_used_vars.begin( ); 
                it != concurrently_used_vars.end( ); ++it )
        {
            const Nodecl::NodeclBase& var = it->first;
            const TL::ObjectList<TL::Analysis::Node*>& concurrent_nodes_using_var = it->second;
            // Do not warn the same variable twice
            if(warned_vars.find(var) != warned_vars.end())
                continue;
            // 3.5.2.- Get the variables that are defined in the concurrent nodes (at least one of the accesses must be a write)
            TL::Analysis::NodeclSet node_defs;
            for (TL::ObjectList<TL::Analysis::Node*>::const_iterator it2 = concurrent_nodes_using_var.begin(); 
                 it2 != concurrent_nodes_using_var.end(); ++it2)
            {
                TL::Analysis::NodeclSet node_killed = (*it2)->get_killed_vars();
                node_defs.insert(node_killed.begin(), node_killed.end());
                    // To be conservative, the undef. variables count as definitions
                    TL::Analysis::NodeclSet node_undef = (*it2)->get_undefined_behaviour_vars();
                    node_defs.insert(node_undef.begin(), node_undef.end());
            }
            
            // 3.5.3.- Check that, at least, one of the accesses is a write
            if ((!task_defs.empty() &&
                    (TL::Analysis::Utils::nodecl_set_contains_nodecl(var, task_defs) ||
                    !TL::Analysis::Utils::nodecl_set_contains_enclosing_nodecl(var, task_defs).is_null() ||
                    !TL::Analysis::Utils::nodecl_set_contains_enclosed_nodecl(var, task_defs).is_null())) ||
                (!node_defs.empty() &&
                    (TL::Analysis::Utils::nodecl_set_contains_nodecl( var, node_defs) ||
                    !TL::Analysis::Utils::nodecl_set_contains_enclosing_nodecl(var, node_defs).is_null() ||
                    !TL::Analysis::Utils::nodecl_set_contains_enclosed_nodecl(var, node_defs).is_null())))
            {   // If all accesses are protected in a critical/atomic construct, then there is no race condition
                // 3.5.4.- Check whether variables scoped in the task are really used within the task
                //         Otherwise we will report the warning __Unused in the adequate method
                VarToNodesMap::iterator task_nodes_using_var_it = task_used_vars.find(var);
                if (task_nodes_using_var_it == task_used_vars.end())
                    goto next_iteration;
                
                {
                    // 3.5.5.- Check whether accesses in task are protected
                    TL::ObjectList<TL::Analysis::Node*> task_nodes_using_var = task_nodes_using_var_it->second;
                    for (TL::ObjectList<TL::Analysis::Node*>::iterator it2 = task_nodes_using_var.begin();
                         it2 != task_nodes_using_var.end(); ++it2)
                    {
                        if (!TL::Analysis::ExtensibleGraph::node_is_in_synchronous_construct(*it2))
                        {
                            result = true;
                            race_cond_vars.append(var);
                            warned_vars.insert(var);
                            goto next_iteration;
                        }
                    }
                    
                    // 3.5.6.- Check whether accesses in concurrent nodes are protected
                    for (TL::ObjectList<TL::Analysis::Node*>::const_iterator it2 = concurrent_nodes_using_var.begin(); 
                        it2 != concurrent_nodes_using_var.end(); ++it2)
                    {
                        if (!TL::Analysis::ExtensibleGraph::node_is_in_synchronous_construct(*it2))
                        {
                            result = true;
                            race_cond_vars.append(var);
                            warned_vars.insert(var);
                            // If the access is not protected in the concurrent access, there is already a race condition
                            goto next_iteration;
                        }
                    }
                }
                
next_iteration: ;
            }
        }
        
        return result;
    }
    
    bool list_elements_contain_nodecl(const Nodecl::List& list, const Nodecl::NodeclBase& n)
    {
        bool result = false;
        for(Nodecl::List::iterator it = list.begin(); it != list.end(); ++it)
        {
            if(Nodecl::Utils::structurally_equal_nodecls(n, *it, /*skip_conversion_nodes*/true) || 
                Nodecl::Utils::nodecl_contains_nodecl_by_structure(*it, n))
            {
                result = true;
                break;
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
                    if( Nodecl::Utils::list_contains_nodecl_by_structure( mem_accesses, n ) )
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
            if( Nodecl::Utils::list_contains_nodecl_by_structure( defined_syms, n ) )
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
                if( Nodecl::Utils::list_contains_nodecl_by_structure( mem_accesses, n ) )
                    result = true;
            }
        }
        
        return result;
    }
    
    TL::Analysis::Node* get_var_last_definition( Nodecl::NodeclBase n, TL::Analysis::Node* task_exit )
    {
        TL::Analysis::Node* result = NULL;
        TL::ObjectList<TL::Analysis::Node*> parents = task_exit->get_parents( );
        TL::Analysis::NodeclSet killed_vars;
        while( !parents.empty( ) && ( result == NULL ) )
        {
            TL::ObjectList<TL::Analysis::Node*> new_parents;
            for( TL::ObjectList<TL::Analysis::Node*>::iterator it = parents.begin( ); it != parents.end( ); ++it )
            {
                killed_vars = (*it)->get_killed_vars( );
                // When a variable has pointer or array type, only the variable itself is included in the data-sharing attributes list
                // Nonetheless, we also need to check the usage of the pointed values
                // Thus, we use the nodecl_set_contains_enclosed_nodecl instead of using ext_sym_set_contains_nodecl
                if( !TL::Analysis::Utils::nodecl_set_contains_enclosed_nodecl( n, killed_vars ).is_null() ) {
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
    
        
    std::string get_dead_vars(
            const Nodecl::List& var_list, int& n_vars,
            const TL::Analysis::NodeclSet& killed_vars, 
            TL::Analysis::Node* task)
    {
        std::string dead_code_vars;
        for( Nodecl::List::iterator it = var_list.begin( ); it != var_list.end( ); ++it )
        {
            if (!TL::Analysis::Utils::nodecl_set_contains_enclosed_nodecl( *it, killed_vars ).is_null())
            {
                if (!var_is_used_in_task_after_definition( *it, task ) )
                {
                    n_vars++;
                    task->add_correctness_dead_var(*it);
                    dead_code_vars += it->prettyprint() + ", ";
                }
            }
        }
        return dead_code_vars;
    }
    
    void check_task_incoherent_data_sharing(TL::Analysis::Node* task)
    {
        // Collect all Symbols/DataReferences appearing in data-sharing clauses
        Nodecl::List firstprivate_vars, private_vars, task_scoped_vars;
        TL::Analysis::PCFGPragmaInfo task_pragma_info = task->get_pragma_node_info( );
        if (task_pragma_info.has_clause(NODECL_OPEN_M_P_FIRSTPRIVATE))
        {
            firstprivate_vars = task_pragma_info.get_clause(NODECL_OPEN_M_P_FIRSTPRIVATE).as<Nodecl::OpenMP::Firstprivate>().get_symbols().shallow_copy().as<Nodecl::List>();
            task_scoped_vars.append( firstprivate_vars );
        } 
        if (task_pragma_info.has_clause(NODECL_OPEN_M_P_PRIVATE))
        {
            private_vars = task_pragma_info.get_clause(NODECL_OPEN_M_P_PRIVATE).as<Nodecl::OpenMP::Private>().get_symbols().shallow_copy().as<Nodecl::List>();
            task_scoped_vars.append( private_vars );
        } 
        if (task_pragma_info.has_clause(NODECL_OPEN_M_P_SHARED))
        {
            Nodecl::List shared_vars = task_pragma_info.get_clause(NODECL_OPEN_M_P_SHARED).as<Nodecl::OpenMP::Shared>().get_symbols().shallow_copy().as<Nodecl::List>();
            task_scoped_vars.append( shared_vars );
        }
        if (task_pragma_info.has_clause(NODECL_OPEN_M_P_SHARED_AND_ALLOCA))
        {
            Nodecl::List shared_alloca_vars = task_pragma_info.get_clause(NODECL_OPEN_M_P_SHARED_AND_ALLOCA).as<Nodecl::OpenMP::SharedAndAlloca>().get_exprs().shallow_copy().as<Nodecl::List>();
            task_scoped_vars.append( shared_alloca_vars );
        }
        
        // Collect usage of variables inside the task
        TL::Analysis::NodeclSet all_ue_vars = task->get_ue_vars();
        TL::Analysis::NodeclSet private_ue_vars = task->get_private_ue_vars( );
        all_ue_vars.insert(private_ue_vars.begin(), private_ue_vars.end());
        
        TL::Analysis::NodeclSet all_killed_vars = task->get_killed_vars();
        TL::Analysis::NodeclSet private_killed_vars = task->get_private_killed_vars( );
        all_killed_vars.insert(private_killed_vars.begin(), private_killed_vars.end());
        
        TL::Analysis::NodeclSet all_undef_vars = task->get_undefined_behaviour_vars( );
        TL::Analysis::NodeclSet private_undef_vars = task->get_private_undefined_behaviour_vars( );
        all_undef_vars.insert(private_undef_vars.begin(), private_undef_vars.end());
        
        TL::Analysis::NodeclSet all_vars = all_ue_vars;
        all_vars.insert(all_killed_vars.begin(), all_killed_vars.end());
        all_vars.insert(all_undef_vars.begin(), all_undef_vars.end());
        
        // Collect dependency clauses, for these may use variables that need to be scoped (shape expressions, array subscripts)
        Nodecl::List dependency_vars;
        if (task_pragma_info.has_clause(NODECL_OPEN_M_P_DEP_IN))
            dependency_vars.append(task_pragma_info.get_clause(NODECL_OPEN_M_P_DEP_IN).as<Nodecl::OpenMP::DepIn>().get_in_deps().shallow_copy());
        if (task_pragma_info.has_clause(NODECL_OPEN_M_P_DEP_OUT))
            dependency_vars.append(task_pragma_info.get_clause(NODECL_OPEN_M_P_DEP_OUT).as<Nodecl::OpenMP::DepOut>().get_out_deps().shallow_copy());
        if (task_pragma_info.has_clause(NODECL_OPEN_M_P_DEP_INOUT))
            dependency_vars.append(task_pragma_info.get_clause(NODECL_OPEN_M_P_DEP_INOUT).as<Nodecl::OpenMP::DepInout>().get_inout_deps().shallow_copy());
        
        // Collect the addresses used within the task
        TL::Analysis::NodeclSet used_addresses = task->get_used_addresses( );
        
        // Case1: No variable should be scoped if it is not used at all inside the task 
        std::string unnecessarily_scoped_vars;
        unsigned int n_unnecessarily_scoped_vars = 0;
        for( Nodecl::List::iterator it = task_scoped_vars.begin( ); it != task_scoped_vars.end( ); ++it )
        {
            if( !TL::Analysis::Utils::nodecl_set_contains_nodecl(*it, all_vars) && 
                !list_elements_contain_nodecl(dependency_vars, *it) && 
                TL::Analysis::Utils::nodecl_set_contains_enclosed_nodecl(*it, used_addresses).is_null() )
            {
                unnecessarily_scoped_vars += it->prettyprint() + ", ";
                n_unnecessarily_scoped_vars++;
            }
        }
        if( !unnecessarily_scoped_vars.empty( ) ) 
        {
            unnecessarily_scoped_vars = unnecessarily_scoped_vars.substr(0, unnecessarily_scoped_vars.size()-2);
            warn_printf (get_unused_scope_message(n_unnecessarily_scoped_vars>1, task).c_str(),
                         unnecessarily_scoped_vars.c_str());
            print_warn_to_file(task->get_graph_related_ast(), __Unused, unnecessarily_scoped_vars);
        }
        
        // Case2: Private variables must never be read before they are written
        std::string incoherent_private_vars;
        unsigned int n_incoherent_private_vars = 0;
        for( Nodecl::List::iterator it = private_vars.begin( ); it != private_vars.end( ); ++it )
        {
            if( TL::Analysis::Utils::nodecl_set_contains_nodecl( *it, all_ue_vars ) )
            {
                incoherent_private_vars += it->prettyprint() + ", ";
                n_incoherent_private_vars++;
            }
        }
        if( !incoherent_private_vars.empty( ) )
        {
            incoherent_private_vars = incoherent_private_vars.substr(0, incoherent_private_vars.size()-2);
            warn_printf (get_incoherent_private_message(n_incoherent_private_vars>1, task).c_str(),
                         incoherent_private_vars.c_str());
            print_warn_to_file(task->get_graph_related_ast(), __P_Incoherent, incoherent_private_vars);
        }
        
        // Case3: Firstprivate variables must never be written before they are read
        // unless they only appear in the directive within a shape expression or an array subscript
        std::string incoherent_firstprivate_vars;
        unsigned int n_incoherent_firstprivate_vars = 0;
        for (Nodecl::List::iterator it = firstprivate_vars.begin( ); it != firstprivate_vars.end( ); ++it )
        {
            if( !TL::Analysis::Utils::nodecl_set_contains_nodecl( *it, all_ue_vars ) && 
                TL::Analysis::Utils::nodecl_set_contains_enclosed_nodecl(*it, used_addresses).is_null() && 
                !list_elements_contain_nodecl(dependency_vars, *it) )
            {
                incoherent_firstprivate_vars += it->prettyprint() + ", ";
                n_incoherent_firstprivate_vars++;
            }
        }
        if (!incoherent_firstprivate_vars.empty( ) )
        {
            incoherent_firstprivate_vars = incoherent_firstprivate_vars.substr(0, incoherent_firstprivate_vars.size()-2);
            warn_printf (get_incoherent_firstprivate_message(n_incoherent_firstprivate_vars>1, task).c_str(),
                         incoherent_firstprivate_vars.c_str());
            print_warn_to_file(task->get_graph_related_ast(), __FP_Incoherent, incoherent_firstprivate_vars);
        }
        
        // Case4: Any private|firstprivate variable defined within the task must use that definition.
        // Otherwise the variable should be shared or the statement can be removed because it produces dead code
        // Separate the cases (private|firstprivate) to report a more accurate message
        int n_fp_dead_vars = 0;
        std::string firstprivate_dead_vars = get_dead_vars(firstprivate_vars, n_fp_dead_vars, all_killed_vars, task);
        if (!firstprivate_dead_vars.empty())
        {
            firstprivate_dead_vars = firstprivate_dead_vars.substr(0, firstprivate_dead_vars.size()-2);
            warn_printf (get_dead_vars_message(/*use_plural*/ (n_fp_dead_vars>1), "firstprivate", task).c_str(), 
                         firstprivate_dead_vars.c_str());
        }
        int n_p_dead_vars = 0;
        std::string private_dead_vars = get_dead_vars(private_vars, n_p_dead_vars, all_killed_vars, task);
        if (!private_dead_vars.empty())
        {
            private_dead_vars = private_dead_vars.substr(0, private_dead_vars.size()-2);
            warn_printf (get_dead_vars_message(/*use_plural*/ (n_p_dead_vars>1), "private", task).c_str(), 
                         private_dead_vars.c_str());
            print_warn_to_file(task->get_graph_related_ast(), __P_Dead, private_dead_vars);
        }
    }
    
    void check_task_incoherent_dependencies(TL::Analysis::Node* task)
    {
        // 1.- Collect all Symbols/DataReferences appearing in dependency clauses
        Nodecl::List dep_in_vars, dep_out_vars;
        TL::Analysis::PCFGPragmaInfo task_pragma_info = task->get_pragma_node_info( );
        if (task_pragma_info.has_clause(NODECL_OPEN_M_P_DEP_IN))
        {
            dep_in_vars = task_pragma_info.get_clause(NODECL_OPEN_M_P_DEP_IN).as<Nodecl::OpenMP::DepIn>().get_in_deps().shallow_copy().as<Nodecl::List>();
        }
        if (task_pragma_info.has_clause(NODECL_OPEN_M_P_DEP_OUT))
        {
            dep_out_vars = task_pragma_info.get_clause(NODECL_OPEN_M_P_DEP_OUT).as<Nodecl::OpenMP::DepOut>().get_out_deps().shallow_copy().as<Nodecl::List>();
        }
        if (task_pragma_info.has_clause(NODECL_OPEN_M_P_DEP_INOUT))
        {
            Nodecl::List dep_inout_vars = task_pragma_info.get_clause(NODECL_OPEN_M_P_DEP_INOUT).as<Nodecl::OpenMP::DepInout>().get_inout_deps().shallow_copy().as<Nodecl::List>();
            dep_in_vars.append(dep_inout_vars);
            dep_out_vars.append(dep_inout_vars);
        }
        
        // 2.- Collect the use-definition information of the task
        TL::Analysis::NodeclSet ue_vars = task->get_ue_vars();
        TL::Analysis::NodeclSet killed_vars = task->get_killed_vars();
        
        // 3.1.- Check whether all input dependencies are read within the task
        std::string incoherent_depin_vars;
        std::string incoherent_depin_pointed_vars;
        unsigned int n_incoherent_depin_vars = 0;
        unsigned int n_incoherent_depin_pointed_vars = 0;
        for(Nodecl::List::iterator it = dep_in_vars.begin(); it != dep_in_vars.end(); ++it)
        {
            if(it->is<Nodecl::Shaping>())
            {   // Check for uses of the variable, any sub-part or the object pointed by the variable
                const Nodecl::NodeclBase& var = it->as<Nodecl::Shaping>().get_postfix();
                if (!TL::Analysis::Utils::nodecl_set_contains_nodecl(var, ue_vars) &&
                    TL::Analysis::Utils::nodecl_set_contains_enclosed_nodecl(var, ue_vars).is_null() && 
                    TL::Analysis::Utils::nodecl_set_contains_pointed_nodecl(var, ue_vars).is_null())
                {
                    incoherent_depin_vars += it->prettyprint() + ", ";
                    n_incoherent_depin_vars++;
                }
            }
            else
            {   // Check only for uses of the variable or sub-parts of the variable
                const Nodecl::NodeclBase& var = *it;
                if (!TL::Analysis::Utils::nodecl_set_contains_nodecl(var, ue_vars) &&
                    TL::Analysis::Utils::nodecl_set_contains_enclosed_nodecl(var, ue_vars).is_null())
                {
                    if(TL::Analysis::Utils::nodecl_set_contains_pointed_nodecl(var, ue_vars).is_null())
                    {
                        incoherent_depin_vars += it->prettyprint() + ", ";
                        n_incoherent_depin_vars++;
                    }
                    else
                    {
                        incoherent_depin_pointed_vars += it->prettyprint() + ", ";
                        n_incoherent_depin_pointed_vars++;
                    }
                }
            }
        }
        
        if (!incoherent_depin_vars.empty())
        {
            incoherent_depin_vars = incoherent_depin_vars.substr(0, incoherent_depin_vars.size()-2);
            warn_printf (get_incoherent_in_deps_message(/*use_plural*/ (n_incoherent_depin_vars>1), /*pointed_obj_used*/ false, task).c_str(), 
                         incoherent_depin_vars.c_str());
            print_warn_to_file(task->get_graph_related_ast(), __IN_Incoherent, incoherent_depin_vars);
        }
        if (!incoherent_depin_pointed_vars.empty())
        {
            incoherent_depin_pointed_vars = incoherent_depin_pointed_vars.substr(0, incoherent_depin_pointed_vars.size()-2);
            warn_printf (get_incoherent_in_deps_message(/*use_plural*/ (n_incoherent_depin_pointed_vars>1), /*pointed_obj_used*/ true, task).c_str(), 
                         incoherent_depin_pointed_vars.c_str());
            print_warn_to_file(task->get_graph_related_ast(), __IN_Incoherent, incoherent_depin_pointed_vars);
        }
        
        // 3.2.- Check whether all output dependencies are written within the task
        std::string incoherent_depout_vars;
        std::string incoherent_depout_pointed_vars;
        unsigned int n_incoherent_depout_vars = 0;
        unsigned int n_incoherent_depout_pointed_vars = 0;
        for(Nodecl::List::iterator it = dep_out_vars.begin(); it != dep_out_vars.end(); ++it)
        {
            if(it->is<Nodecl::Shaping>())
            {   // Check for uses of the variable, any sub-part or the object pointed by the variable
                const Nodecl::NodeclBase& var = it->as<Nodecl::Shaping>().get_postfix();
                if (!TL::Analysis::Utils::nodecl_set_contains_nodecl(var, killed_vars) &&
                    TL::Analysis::Utils::nodecl_set_contains_enclosed_nodecl(var, killed_vars).is_null() && 
                    TL::Analysis::Utils::nodecl_set_contains_pointed_nodecl(var, killed_vars).is_null())
                {
                    incoherent_depout_vars += it->prettyprint() + ", ";
                    n_incoherent_depout_vars++;
                }
            }
            else
            {   // Check only for uses of the variable or sub-parts of the variable
                const Nodecl::NodeclBase& var = *it;
                if (!TL::Analysis::Utils::nodecl_set_contains_nodecl(var, killed_vars) &&
                    TL::Analysis::Utils::nodecl_set_contains_enclosed_nodecl(var, killed_vars).is_null())
                {
                    if(TL::Analysis::Utils::nodecl_set_contains_pointed_nodecl(var, killed_vars).is_null())
                    {
                        incoherent_depout_vars += it->prettyprint() + ", ";
                        n_incoherent_depout_vars++;
                    }
                    else
                    {
                        incoherent_depout_pointed_vars += it->prettyprint() + ", ";
                        n_incoherent_depout_pointed_vars++;
                    }
                }
            }
            
        }
        if (!incoherent_depout_vars.empty())
        {
            incoherent_depout_vars = incoherent_depout_vars.substr(0, incoherent_depout_vars.size()-2);
            warn_printf (get_incoherent_out_deps_message(/*use_plural*/ (n_incoherent_depout_vars>1), /*pointed_obj_used*/ false, task).c_str(), 
                         incoherent_depout_vars.c_str());
            print_warn_to_file(task->get_graph_related_ast(), __OUT_Incoherent, incoherent_depout_vars);
        }
        if (!incoherent_depout_pointed_vars.empty())
        {
            incoherent_depout_pointed_vars = incoherent_depout_pointed_vars.substr(0, incoherent_depout_pointed_vars.size()-2);
            warn_printf (get_incoherent_out_deps_message(/*use_plural*/ (n_incoherent_depout_pointed_vars>1), /*pointed_obj_used*/ true, task).c_str(), 
                         incoherent_depout_pointed_vars.c_str());
            print_warn_to_file(task->get_graph_related_ast(), __OUT_Incoherent, incoherent_depout_pointed_vars);
        }
    }
}

    // *************************************************************************** //
    // ************************* Correctness as a service ************************ //
    
    void execute_correctness_checks(TL::Analysis::ExtensibleGraph* graph)
    {
        // Get all task nodes
        ObjectList<TL::Analysis::Node*> tasks = graph->get_tasks_list();
        for (ObjectList<TL::Analysis::Node*>::iterator it = tasks.begin(); it != tasks.end(); it++)
        {
            TL::Analysis::Node* task = *it;
            const Nodecl::NodeclBase& task_nodecl = task->get_graph_related_ast();
            // Automatic storage variables as shared
            // Example:
            // {
            //    int a;
            //    #pragma omp task shared(a)
            //       a = 0;
            // }
            {
                Nodecl::List local_vars;
                if (task_is_locally_bound(task, local_vars).is_true() && 
                    task_only_synchronizes_in_enclosing_scopes(task, local_vars).is_false())
                {
                    std::string local_vars_str = get_nodecl_list_str(local_vars);
                    warn_printf (get_auto_storage_message(/*use_plural*/ local_vars.size()>1, task).c_str(), 
                                 local_vars_str.c_str());
                    print_warn_to_file(task_nodecl, __SharedAutoStorage, local_vars_str);
                }
            }
            
            // Race conditions
            // Example:
            // #pragma omp task
            //    x++;
            // printf("x=%d\n", x);
            {
                Nodecl::List race_vars;
                if (task_may_cause_race_condition(graph, task, race_vars).is_true())
                {
                    std::string race_vars_str = get_nodecl_list_str(race_vars);
                    warn_printf (get_race_message(/*use_plural*/ race_vars.size()>1, task).c_str(),
                                 race_vars_str.c_str());
                    print_warn_to_file(task_nodecl, __Race, race_vars_str);
                }
            }
            
            // Incoherent data-sharing
            // Example:
            // #pragma omp task private(x)
            //    x++;
            {
                check_task_incoherent_data_sharing(task);
            }
            
            // Incoherent dependency clauses
            // Example:
            // #pragma omp task inout(a)
            //     t = a;
            {
                check_task_incoherent_dependencies(task);
            }
        }
    }

    // *********************** END Correctness as a service ********************** //
    // *************************************************************************** //
    
    
    struct FunctionCodeVisitor : Nodecl::ExhaustiveVisitor<void>
    {
        void visit( const Nodecl::FunctionCode& function_code )
        {
            TL::Analysis::AnalysisSingleton& singleton = TL::Analysis::AnalysisSingleton::get_analysis(ompss_mode_enabled);
            TL::Analysis::PCFGAnalysis_memento memento;
            // We compute liveness analysis (that includes PCFG and use-def) because 
            // we need the information computed by TaskConcurrency (last and next synchronization points of a task)
            if(VERBOSE)
            {
                std::cerr << "===========================================" << std::endl;
                std::cerr << "OMP-LINT_ Executing analysis required for OpenMP/OmpSs correctness checking in function '" 
                          << function_code.get_symbol().get_name() << "'" << std::endl;
            }
            singleton.tune_task_synchronizations(memento, function_code);
            if(VERBOSE)
                std::cerr << "===========================================" << std::endl;
            TL::ObjectList<TL::Analysis::ExtensibleGraph*> extensible_graphs = memento.get_pcfgs();
            ERROR_CONDITION( extensible_graphs.size() != 1, "I expected 1 graph per FunctionCode", 0 );
            
            TL::Analysis::ExtensibleGraph* graph = extensible_graphs[0];
            singleton.print_pcfg(memento, graph->get_name());
            
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
            
            execute_correctness_checks(graph);
            
            // Close the logs file
            if(!log_file_path.empty())
            {
                fclose(log_file);
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
