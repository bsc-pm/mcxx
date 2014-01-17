/*--------------------------------------------------------------------
 (C) Copyright 2006-2012 Barcelona Supercomputing Center             *
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

#include "tl-task-syncs-tune.hpp"

namespace TL { 
namespace Analysis {
namespace TaskAnalysis{

namespace {
    
    enum SyncModification
    {
        Keep = 0,
        MaybeToStatic = 1,
        Remove = 2
    };
    
    SyncModification compute_condition_for_unmatched_values( const Nodecl::NodeclBase& n, const Nodecl::NodeclBase& m, 
                                                             Nodecl::NodeclBase& condition )
    {
        if( condition.is_null( ) )
            condition = Nodecl::Equal::make( n, m, n.get_type( ) );
        else
            condition = Nodecl::LogicalAnd::make( condition.shallow_copy( ), 
                                                  Nodecl::Equal::make( n.shallow_copy( ), m.shallow_copy( ), n.get_type( ) ), 
                                                  condition.get_type( ) );
            return Keep;
    }
    
    SyncModification match_constant_values( const Nodecl::NodeclBase& n, const Nodecl::NodeclBase& m, 
                                            Nodecl::NodeclBase& condition )
    {
        SyncModification modification_type = Keep;
        
        if( Nodecl::Utils::equal_nodecls( n, m ) )
        {   // n == m | The two indexes are equal!
            if( condition.is_null( ) )
                // If we already have some condition, there is some previous subscript that has not been resolved
                modification_type = MaybeToStatic;
        }
        else
        {   // n != m | The accessed indexes are different => we can remove the dependency
            modification_type = Remove;
            condition = Nodecl::NodeclBase::null( );
            
        }
        
        return modification_type;
    }
    
    // Restriction: #n must be a constant nodecl and # m a non-constant nodecl 
    SyncModification match_const_and_var_values( const Nodecl::NodeclBase& n, const Nodecl::NodeclBase& m,
                                                 Node* m_node, Nodecl::NodeclBase& condition )
    {
        SyncModification modification_type = Keep;
        
        Utils::ext_sym_map m_reaching_defs_in = m_node->get_reaching_definitions_in( );
        Utils::ext_sym_set m_killed_vars = m_node->get_killed_vars( );
        if( ( m_reaching_defs_in.count( m ) == 1 ) && ( m_killed_vars.find( m ) == m_killed_vars.end( ) ) )
        {   // There is a unique reaching definition of the subscript and it is not defined inside the m_node node
            Nodecl::NodeclBase m_reach_def = m_reaching_defs_in.find( m )->second;
            if( m_reach_def.is_constant( ) )
            {   // m definition is constant
                modification_type = match_constant_values( n, m_reach_def, condition );
            }
            else
            {   
                if( m_reach_def.is<Nodecl::Symbol>( ) )
                {
                    modification_type = match_const_and_var_values( n, m_reach_def, m_node, condition );
                }
                else
                {   // We do not know whether the indexes are equal => compute the condition
                    modification_type = compute_condition_for_unmatched_values( n, m, condition );
                }
            }
        }
        else
        {   // We do not know whether the indexes are equal => compute the condition
            modification_type = compute_condition_for_unmatched_values( n, m, condition );
        }
        
        return modification_type;
    }
    
    SyncModification match_variable_values( const Nodecl::NodeclBase& n, const Nodecl::NodeclBase& m, 
                                            Node* n_node, Node* m_node, Nodecl::NodeclBase& condition )
    {
        SyncModification modification_type = Keep;
        
        Utils::ext_sym_map n_reaching_defs_in = n_node->get_reaching_definitions_in( );
        Utils::ext_sym_set n_killed_vars = n_node->get_killed_vars( );
        Utils::ext_sym_map m_reaching_defs_in = m_node->get_reaching_definitions_in( );
        Utils::ext_sym_set m_killed_vars = m_node->get_killed_vars( );
        
        if( ( n_reaching_defs_in.count( n ) == 1 ) && ( n_killed_vars.find( n ) == n_killed_vars.end( ) ) && 
            ( m_reaching_defs_in.count( m ) == 1 ) && ( m_killed_vars.find( m ) == m_killed_vars.end( ) ) )
        {
            Nodecl::NodeclBase n_reach_def = n_reaching_defs_in.find( n )->second;
            Nodecl::NodeclBase m_reach_def = m_reaching_defs_in.find( m )->second;
            
            if( n_reach_def.is_constant( ) )
            {   // n definition is constant
                if( m_reach_def.is_constant( ) )
                {   // m definition is constant
                    modification_type = match_constant_values( n_reach_def, m_reach_def, condition );
                }
                else
                {   // m is not constant | Try to compute equality from the reaching definition of m
                    modification_type = match_const_and_var_values( n_reach_def, m_reach_def, m_node, condition );
                }
            }
            else
            {
                if( m_reach_def.is_constant( ) )
                {   // n is not constant | Try to compute equality from the reaching definition of n
                    modification_type = match_const_and_var_values( m_reach_def, n_reach_def, m_node, condition );
                }
                else
                {
                    if( n_reach_def.is<Nodecl::Symbol>( ) && m_reach_def.is<Nodecl::Symbol>( ) )
                    {   // n, m reaching definitions are symbols | Try to compute the equality from its reaching definitions
                        modification_type = match_variable_values( n_reach_def, m_reach_def, n_node, m_node, condition );
                    }
                    else
                    {
                        modification_type = compute_condition_for_unmatched_values( n, m, condition );
                    }
                }
            }
        }
        else
        {   // We do not know whether the indexes are equal => compute the condition
            modification_type = compute_condition_for_unmatched_values( n, m, condition );
        }
        
        return modification_type;
    }
    
    SyncModification match_array_subscripts( Node* source, Node* target,
                                             const Nodecl::ArraySubscript& a, const Nodecl::ArraySubscript& b, 
                                             Nodecl::NodeclBase& condition )
    {
        SyncModification modification_type = Keep;
        
        Nodecl::List source_subscripts = a.get_subscripts( ).as<Nodecl::List>( );
        Nodecl::List target_subscripts = b.get_subscripts( ).as<Nodecl::List>( );
        Nodecl::List::iterator its = source_subscripts.begin( );
        Nodecl::List::iterator itt = target_subscripts.begin( );
        for( ; ( its != source_subscripts.end( ) ) && ( modification_type != Remove ); ++its, ++itt )
        {
            if( its->is_constant( ) )
            {   // source[c1]
                if( itt->is_constant( ) )
                {   // target[c2]
                    modification_type = match_constant_values( *its, *itt, condition );
                }
                else
                {   // target[v2]
                    modification_type = match_const_and_var_values( *its, *itt, target, condition );
                }
            }
            else
            {   // source[v1]
                if( itt->is_constant( ) )
                {   // target[c2]
                    modification_type = match_const_and_var_values( *itt, *its, source, condition );
                }
                else
                {   // targt_v2
                    
                }
            }
        }
        
        return modification_type;
    }
    
    SyncModification match_dependence( Node* source, Node* target, 
                                       const Nodecl::NodeclBase& src_dep, const Nodecl::NodeclBase& tgt_dep,
                                       Nodecl::NodeclBase& condition )
    {
        SyncModification modification_type = Keep;
        
        // Skip Conversion nodes
        if( src_dep.is<Nodecl::Conversion>( ) )
            match_dependence( source, target, src_dep.as<Nodecl::Conversion>( ).get_nest( ), tgt_dep, condition );
        if( tgt_dep.is<Nodecl::Conversion>( ) )
            match_dependence( source, target, src_dep, tgt_dep.as<Nodecl::Conversion>( ).get_nest( ), condition );
        
        // Skip shaping nodes
        if( src_dep.is<Nodecl::Shaping>( ) )
            match_dependence( source, target, src_dep.as<Nodecl::Shaping>( ).get_postfix( ), tgt_dep, condition );
        if( tgt_dep.is<Nodecl::Shaping>( ) )
            match_dependence( source, target, src_dep, tgt_dep.as<Nodecl::Shaping>( ).get_postfix( ), condition );
        
        // Compare the two dependencies
        if( src_dep.is<Nodecl::Symbol>( ) )
        {
            if( tgt_dep.is<Nodecl::Symbol>( ) )
            {
                if( Nodecl::Utils::equal_nodecls( src_dep, tgt_dep ) )
                    modification_type = MaybeToStatic;
                else
                    modification_type = Remove;
            }
            else
                modification_type = Remove;
        }
        else if( src_dep.is<Nodecl::Dereference>( ) )
        {   // TODO Alias analysis needed for further information here
        }
        else if( src_dep.is<Nodecl::ClassMemberAccess>( ) )
        {
            if( tgt_dep.is<Nodecl::ClassMemberAccess>( ) )
            {
                Nodecl::ClassMemberAccess src_dep_ = src_dep.as<Nodecl::ClassMemberAccess>( );
                Nodecl::ClassMemberAccess tgt_dep_ = tgt_dep.as<Nodecl::ClassMemberAccess>( );
                if( Nodecl::Utils::equal_nodecls( src_dep_.get_lhs( ), tgt_dep_.get_lhs( ) ) )
                    modification_type = match_dependence( source, target, 
                                                          src_dep_.get_member( ), src_dep_.get_member( ), 
                                                          condition );
                else
                    modification_type = Remove;
            }
            else
                modification_type = Remove;
        }
        else if( src_dep.is<Nodecl::ArraySubscript>( ) )
        {
            if( tgt_dep.is<Nodecl::ArraySubscript>( ) )
            {
                modification_type = match_array_subscripts( source, target, src_dep.as<Nodecl::ArraySubscript>( ),
                                                            tgt_dep.as<Nodecl::ArraySubscript>( ), condition );
            }
            else
                modification_type = Remove;
        }
        
        return modification_type;
    }
    
    /*!This method returns the condition that has to be associated to an edge of type 'maybe' that connects two tasks which:
     * \param source_environ is the environment of the source task
     * \param target_environ is the environment of the target task
     * Two cases may happen:
     * - The edge is determined to be 'static', meaning that the target task depends for sure of the source task.
     *   In this case, the return value is a null NodeclBase
     * - The edge cannot be determined to be 'static'.
     *   In this case, the return value is the condition that has to be fulfilled to determine a dependency between the two tasks
     */
    Nodecl::NodeclBase match_dependencies( Node* source, Node* target )
    {
        Nodecl::NodeclBase condition;
        
        typedef std::pair<ObjectList<Nodecl::NodeclBase>, ObjectList<Nodecl::NodeclBase> > nodecl_object_list_pair;
        
        Nodecl::List source_environ = source->get_graph_related_ast( ).as<Nodecl::OpenMP::Task>( ).get_environment( ).as<Nodecl::List>( );
        Nodecl::List target_environ = target->get_graph_related_ast( ).as<Nodecl::OpenMP::Task>( ).get_environment( ).as<Nodecl::List>( );
        
        // For the source task we are interested only in out or inout dependencies
        // FIXME cannot call to a generic function because ObjectList<X> cannot be converted into ObjectList<Y>
        ObjectList<Nodecl::NodeclBase> source_out_deps = source_environ.find_all<Nodecl::OpenMP::DepOut>( )
                .map( functor( &Nodecl::OpenMP::DepOut::get_out_deps ) )                // ObjectList<Nodecl::NodeclBase>
                .map( functor( &Nodecl::NodeclBase::as<Nodecl::List> ) )                // ObjectList<Nodecl::List>
                .map( functor( &Nodecl::List::to_object_list ) )                        // ObjectList<ObjectList<Nodecl::NodeclBase> >
                .reduction( functor( append_two_lists<Nodecl::NodeclBase> ) )           // ObjectList<Nodecl::NodeclBase>
                ;
        ObjectList<Nodecl::NodeclBase> source_inout_deps = source_environ.find_all<Nodecl::OpenMP::DepInout>( )
                .map( functor( &Nodecl::OpenMP::DepInout::get_inout_deps ) )            // ObjectList<Nodecl::NodeclBase>
                .map( functor( &Nodecl::NodeclBase::as<Nodecl::List> ) )                // ObjectList<Nodecl::List>
                .map( functor( &Nodecl::List::to_object_list ) )                        // ObjectList<ObjectList<Nodecl::NodeclBase> >
                .reduction( functor( append_two_lists<Nodecl::NodeclBase> ) )           // ObjectList<Nodecl::NodeclBase>
                ;
        ObjectList<Nodecl::NodeclBase> source_deps = append_two_lists( nodecl_object_list_pair( source_out_deps, source_inout_deps ) );
        
        // For the target task we need to check all kind of dependencies
        ObjectList<Nodecl::NodeclBase> target_in_deps = target_environ.find_all<Nodecl::OpenMP::DepIn>( )
                .map( functor( &Nodecl::OpenMP::DepIn::get_in_deps ) )                  // ObjectList<Nodecl::NodeclBase>
                .map( functor( &Nodecl::NodeclBase::as<Nodecl::List> ) )                // ObjectList<Nodecl::List>
                .map( functor( &Nodecl::List::to_object_list ) )                        // ObjectList<ObjectList<Nodecl::NodeclBase> >
                .reduction( functor( append_two_lists<Nodecl::NodeclBase> ) )           // ObjectList<Nodecl::NodeclBase>
                ;
        ObjectList<Nodecl::NodeclBase> target_out_deps = target_environ.find_all<Nodecl::OpenMP::DepOut>( )
                .map( functor( &Nodecl::OpenMP::DepOut::get_out_deps ) )                // ObjectList<Nodecl::NodeclBase>
                .map( functor( &Nodecl::NodeclBase::as<Nodecl::List> ) )                // ObjectList<Nodecl::List>
                .map( functor( &Nodecl::List::to_object_list ) )                        // ObjectList<ObjectList<Nodecl::NodeclBase> >
                .reduction( functor( append_two_lists<Nodecl::NodeclBase> ) )           // ObjectList<Nodecl::NodeclBase>
                ;
        ObjectList<Nodecl::NodeclBase> target_inout_deps = target_environ.find_all<Nodecl::OpenMP::DepInout>( )
                .map( functor( &Nodecl::OpenMP::DepInout::get_inout_deps ) )            // ObjectList<Nodecl::NodeclBase>
                .map( functor( &Nodecl::NodeclBase::as<Nodecl::List> ) )                // ObjectList<Nodecl::List>
                .map( functor( &Nodecl::List::to_object_list ) )                        // ObjectList<ObjectList<Nodecl::NodeclBase> >
                .reduction( functor( append_two_lists<Nodecl::NodeclBase> ) )           // ObjectList<Nodecl::NodeclBase>
                ;
        ObjectList<Nodecl::NodeclBase> target_deps = 
                append_two_lists( nodecl_object_list_pair( target_inout_deps,
                                                           append_two_lists( nodecl_object_list_pair( target_in_deps, target_out_deps ) ) ) );
        
        for( ObjectList<Nodecl::NodeclBase>::iterator its = source_deps.begin( ); its != source_deps.end( ); ++its )
            for( ObjectList<Nodecl::NodeclBase>::iterator itt = target_deps.begin( ); itt != target_deps.end( ); ++itt )
                match_dependence( source, target, *its, *itt, condition );
        
        return condition;
    }
    
}
    
    TaskSyncTunning::TaskSyncTunning( ExtensibleGraph* pcfg )
        : _pcfg( pcfg )
    {}
    
    void TaskSyncTunning::tune_task_synchronizations( )
    {
        Node* entry = _pcfg->get_graph( )->get_graph_entry_node( );
        tune_task_synchronizations_rec( entry );
        ExtensibleGraph::clear_visits( entry );
    }

    void TaskSyncTunning::tune_task_synchronizations_rec( Node* current )
    {
        if( !current->is_visited( ) )
        {
            current->set_visited( true );
            
            // Treat the current node
            if( current->is_graph_node( ) )
            {
                if( current->is_omp_task_node( ) )
                {
                    // Tune the synchronizations with its children, if possible
                    ObjectList<Edge*> exits = current->get_exit_edges( );
                    for( ObjectList<Edge*>::iterator it = exits.begin( ); it != exits.end( ); ++it )
                    {
                        std::string label = ( *it )->get_label( );
                        if( label == "maybe" )
                        {   // Can we tune this edge to make it static
                            // if so, remove the rest of the edges
                            Nodecl::NodeclBase target_task_environ = ( *it )->get_target( )->get_graph_related_ast( ).as<Nodecl::OpenMP::Task>( ).get_environment( );
                            Nodecl::NodeclBase condition = match_dependencies( current, ( *it )->get_target( ) );
                            // TODO Shall the 'condition' be stored in the linked data associated to the edge??
                            // This information will be used when building the Task Dependency Graph
                        }
                    }
                }
                
                // Treat the inner nodes recursively
                tune_task_synchronizations_rec( current->get_graph_entry_node( ) );
            }
            
            // Treat the children recursively
            ObjectList<Node*> children = current->get_children( );
            for( ObjectList<Node*>::iterator it = children.begin( ); it != children.end( ); ++it )
            {
                tune_task_synchronizations_rec( *it );
            }
        }
    }
    
}   
}
}