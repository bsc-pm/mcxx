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
            // std::cerr << "At function " << function_code.get_locus_str() << std::endl;
            
            TL::Analysis::AnalysisSingleton& singleton = TL::Analysis::AnalysisSingleton::get_analysis( );
            TL::Analysis::PCFGAnalysis_memento memento;
            TL::ObjectList<TL::Analysis::ExtensibleGraph*> extensible_graphs =
                    singleton.parallel_control_flow_graph( memento, function_code );
            ERROR_CONDITION( extensible_graphs.size() != 1, "I expected 1 graph per FunctionCode", 0 );
            
            TL::Analysis::ExtensibleGraph* graph = extensible_graphs[0];
            
            if (CURRENT_CONFIGURATION->debug_options.print_pcfg)
                graph->print_graph_to_dot(false, false, false, false, false, false);
            
            // Get all task nodes
            PCFG_Node_list tasks = graph->get_tasks_list();
            for( PCFG_Node_list::iterator it = tasks.begin(); it != tasks.end(); it++ )
            {
                Nodecl::List local_vars;
                
                if( task_is_locally_bound(*it, local_vars).is_true( ) )
                {
                    if( task_only_synchronizes_in_enclosing_scopes(*it).is_true( ) )
                    {
                        Nodecl::NodeclBase task = (*it)->get_graph_label( );
                        std::string local_vars_str = get_nodecl_list_str( local_vars );
                        warn_printf( "%s: warning: '#pragma omp task' uses local data '%s' "
                                     "whose lifetime may have ended when the task is executed\n", 
                        task.get_locus_str().c_str(), local_vars_str.c_str( ) );
                    }
                    else if( task_is_statically_determined_to_late_execution(*it).is_true( ) )
                    {
                        Nodecl::NodeclBase task = (*it)->get_graph_label();
                        std::string local_vars_str = get_nodecl_list_str( local_vars );
                        warn_printf( "%s: warning: '#pragma omp task' uses local data '%s' but may be"
                                     " executed after the function ends\n",
                        task.get_locus_str().c_str(), local_vars_str.c_str( ) );
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
            
            Nodecl::NodeclBase task = n->get_graph_label( );
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
