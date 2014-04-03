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

#include <iostream>
#include <fstream>

#include "tl-analysis-utils.hpp"
#include "config.h"
#include "tl-node.hpp"
#include "tl-pcfg-visitor.hpp"      // For IPA analysis
#include "tl-rename-visitor.hpp"
#include "tl-use-def.hpp"

namespace TL {
namespace Analysis {

    // **************************************************************************************************** //
    // **************************** Class implementing use-definition analysis **************************** //
    
    UseDef::UseDef( ExtensibleGraph* graph, ObjectList<ExtensibleGraph*>* pcfgs )
        : _graph( graph ), _pcfgs( pcfgs ), _global_vars( ), _reference_params( )
    {
        // Initialized Global Variables usage to NONE
        std::set<Symbol> global_vars = _graph->get_global_variables( );
        for( std::set<Symbol>::iterator it = global_vars.begin( ); it != global_vars.end( ); ++it )
            _global_vars[*it] = Utils::UsageKind::NONE;
        
        // Initialized Reference parameters usage to NONE
        Symbol func_sym = graph->get_function_symbol( );
        if( func_sym.is_valid( ) )
        {   // The PCFG contains a FunctionCode
            ObjectList<TL::Symbol> params = func_sym.get_function_parameters( );
            for( ObjectList<TL::Symbol>::iterator it = params.begin( ); it != params.end( ); ++it )
            {
                Type param_type = it->get_type( );
                if( param_type.is_any_reference( ) || param_type.is_pointer( ) )
                    _reference_params[*it] = Utils::UsageKind::NONE;
            }
        }
    }

    void UseDef::compute_usage( )
    {
        Node* graph = _graph->get_graph( );
        compute_usage_rec( graph );
        ExtensibleGraph::clear_visits( graph );
    }

    // Top bottom traversal
    void UseDef::compute_usage_rec( Node* current )
    {
        if( !current->is_visited( ) )
        {
            current->set_visited( true );

            if( current->is_exit_node( ) )
                return;

            if( current->is_graph_node( )
                && !current->is_asm_def_node( ) && !current->is_asm_op_node( ) )
            {
                // Use-def info is computed from inner nodes to outer nodes
                compute_usage_rec( current->get_graph_entry_node( ) );

                // Propagate usage info from inner to outer nodes
                ExtensibleGraph::clear_visits( current );
                set_graph_node_use_def( current );
                
                if( current->is_omp_task_node( ) )
                {   // Propagate usage to its task creation node
                    Node* task_creation = current->get_parents( )[0];
                    propagate_usage_over_task_creation( task_creation );
                }
            }
            else
            {
                // Treat statements in the current node
                ObjectList<Nodecl::NodeclBase> stmts = current->get_statements( );
                for( ObjectList<Nodecl::NodeclBase>::iterator it = stmts.begin( ); it != stmts.end( ); ++it )
                {
                    UsageVisitor uv( current, _graph, _pcfgs, &_global_vars, &_reference_params );
                    uv.compute_statement_usage( *it );
                }
            }

            // Compute usage form children
            ObjectList<Node*> children = current->get_children( );
            for( ObjectList<Node*>::iterator it = children.begin( ); it != children.end( ); ++it )
                compute_usage_rec( *it );
        }
    }

    // Bottom up traversal
    void UseDef::propagate_usage_over_task_creation( Node* task_creation )
    {
        // Propagate current created task usage
        // Task creation children may be: created task, task synchronization, another task creation
        ObjectList<Node*> children = task_creation->get_children( );
        for( ObjectList<Node*>::iterator it = children.begin( ); it != children.end( ); ++it )
        {
            task_creation->set_ue_var( ( *it )->get_ue_vars( ) );
            task_creation->set_killed_var( ( *it )->get_killed_vars( ) );
            task_creation->set_undefined_behaviour_var( ( *it )->get_undefined_behaviour_vars( ) );
        }
        
        // Keep propagating to parents if they still are task creation nodes
        ObjectList<Node*> parents = task_creation->get_parents( );
        for( ObjectList<Node*>::iterator it = parents.begin( ); it != parents.end( ); ++it )
        {
            if( ( *it )->is_omp_task_creation_node( ) )
            {
                propagate_usage_over_task_creation( *it );
            }
        }
    }
    
    /*!Try to insert a new variable in a list
     * If an enclosing variable of the current variable already exists, then we don't include the variable
     * If any variable enclosed by the current variable exists, then we delete the variable
     * If the variable is an array en we can form a range with the to access, we do that deleting the existing element of the list and
     * including the new ranged access
     */
    static Utils::ext_sym_set insert_var_in_list( Nodecl::NodeclBase var, Utils::ext_sym_set list )
    {
        Utils::ext_sym_set new_list;
        if( !Utils::ext_sym_set_contains_enclosing_nodecl( var, list ) )
        {
            // Create a new list with the elements of 'list' that are not enclosed by 'var'
            Utils::ext_sym_set aux_list;
            aux_list.insert( Utils::ExtendedSymbol( var ) );
            for( Utils::ext_sym_set::iterator it = list.begin( ); it != list.end( ); ++it )
            {
                if( !Utils::ext_sym_set_contains_enclosing_nodecl( it->get_nodecl( ), aux_list ) )
                {
                    new_list.insert( *it );
                }
            }

            // Insert the new variable
            new_list.insert( var );
        }
        else
        {   // No need to insert the variable, its enclosing symbol is already there
            // FIXME We can create ranges for array accesses here
            new_list = list;
        }
        return new_list;
    }

    /*!
     * Inserts the elements in 'l' to the list 'in_l' when they are not in the list 'killed' nor in 'undef'
     * When avoiding lists, it take cares of elements enclosing the current variable and of elements enclosed by the current variable
     */
    static Utils::ext_sym_set compute_use_def_with_children( Utils::ext_sym_set l, Utils::ext_sym_set in_l,
                                                             Utils::ext_sym_set& killed, Utils::ext_sym_set& undef,
                                                             char compute_undef )
    {
        Utils::ext_sym_set new_l = in_l;
        for( Utils::ext_sym_set::iterator it = l.begin( ); it != l.end( ); ++it )
        {
            Nodecl::NodeclBase var = it->get_nodecl( );
            if( !Utils::ext_sym_set_contains_enclosing_nodecl( var, killed ) )
            {   // No enclosing variable in the avoiding list 1
                // Look for variables in avoiding list 1 enclosed by 'var'
                Utils::ext_sym_set aux_set;
                aux_set.insert( Utils::ExtendedSymbol( var ) );
                Utils::ext_sym_set::iterator itk = killed.begin( );
                for( ; itk != killed.end( ); ++itk )
                {
                    if( Utils::ext_sym_set_contains_enclosing_nodecl(itk->get_nodecl( ), aux_set) )
                    {   // Delete from 'var' the enclosed part of (*itk) and put the result in 'var'
                        // TODO
                        WARNING_MESSAGE( "Part of nodecl '%s' found in the current var must be avoided. " \
                                         "A subpart is killed.", itk->get_nodecl( ).prettyprint( ).c_str( ),
                                         var.prettyprint( ).c_str( ) );
                        //                             var = nodecl_subtract(var, ita->get_nodecl( ) );
                        killed.erase( itk );
                        if( compute_undef == '1' )
                            new_l = insert_var_in_list( var, new_l );
                        else
                            undef.insert( var );
                        break;
                    }
                }

                if( !Utils::ext_sym_set_contains_enclosing_nodecl( var, undef ) )
                {   // No enclosing variable in the avoiding list 2
                    // Look for variables in avoiding list 2 enclosed by 'var'
                    Utils::ext_sym_set aux_set_2; aux_set_2.insert( *it );
                    Utils::ext_sym_set::iterator itu = undef.begin( );
                    for( ; itu != undef.end( ); ++itu )
                    {
                        if( Utils::ext_sym_set_contains_enclosing_nodecl( itu->get_nodecl( ), aux_set_2 ) )
                        {   // Delete from var the enclosed part of (*itu) and put the result in 'var'
                            // TODO
                            WARNING_MESSAGE( "Part of nodecl found in the current var must be avoided. "\
                                             "A subpart is undefined.", itu->get_nodecl( ).prettyprint( ).c_str( ),
                                             var.prettyprint( ).c_str( ) );
                            undef.erase( itu );
                            if( compute_undef == '1' )
                                new_l = insert_var_in_list( var, new_l );
                            else
                                undef.insert( var );
                            break;
                        }
                    }
                    if( itk == killed.end( ) && itu == undef.end( ) )
                    {
                        new_l = insert_var_in_list( var, new_l );
                    }
                }
            }
        }
        return new_l;
    }

    ObjectList<Utils::ext_sym_set> UseDef::get_use_def_over_nodes( Node* current )
    {
        ObjectList<Utils::ext_sym_set> use_def, use_def_aux;

        if( !current->is_visited( ) )
        {
            current->set_visited( true );

            // Use-Def in current node
            Utils::ext_sym_set ue_vars = current->get_ue_vars( );
            Utils::ext_sym_set killed_vars = current->get_killed_vars( );
            Utils::ext_sym_set undef_vars = current->get_undefined_behaviour_vars( );

            // Concatenate info from children nodes
            ObjectList<Node*> children = current->get_children( );
            Utils::ext_sym_set ue_children, killed_children, undef_children;
            for( ObjectList<Node*>::iterator it = children.begin( ); it != children.end( ); ++it )
            {
                use_def_aux = get_use_def_over_nodes( *it );
                if( !use_def_aux.empty( ) )
                {
                    ue_children = ext_sym_set_union( ue_children, use_def_aux[0] );
                    killed_children = ext_sym_set_union( killed_children, use_def_aux[1] );
                    undef_children = ext_sym_set_union( undef_children, use_def_aux[2] );
                }
            }

            // Append to current node info from children
            ue_vars = compute_use_def_with_children( ue_children, ue_vars,
                                                     killed_vars, undef_vars, /*compute_undef*/ '0' );
            undef_vars = compute_use_def_with_children( undef_children, undef_vars,
                                                        killed_vars, undef_vars, /*compute_undef*/ '1' );
            killed_vars = compute_use_def_with_children( killed_children, killed_vars,
                                                         killed_vars, undef_vars, /*compute_undef*/ '0' );

            use_def.append( ue_vars );
            use_def.append( killed_vars );
            use_def.append( undef_vars );
        }

        return use_def;
    }

    void UseDef::set_graph_node_use_def( Node* current )
    {
        if( current->is_graph_node( ) )
        {
            if( !current->is_visited( ) )
            {
                current->set_visited( true );

                Utils::ext_sym_set ue_vars, killed_vars, undef_vars;
                ObjectList<Utils::ext_sym_set> usage = get_use_def_over_nodes( current->get_graph_entry_node( ) );
                ue_vars = usage[0];
                killed_vars = usage[1];
                undef_vars = usage[2];

                if( current->is_omp_loop_node( ) || current->is_omp_sections_node( ) || current->is_omp_single_node( )
                    || current->is_omp_parallel_node( ) || current->is_omp_task_node( ) )
                {   // Take into account data-sharing clauses in Use-Def Task node computation
                    Nodecl::List environ =
                            current->get_graph_related_ast( ).as<Nodecl::OpenMP::Task>( ).get_environment( ).as<Nodecl::List>( );
                    for( Nodecl::List::iterator it = environ.begin( ); it != environ.end( ); ++it )
                    {
                        if( it->is<Nodecl::OpenMP::Private>( ) )
                        {   // Remove any usage computed in the inner nodes,
                            // because is the usage of a copy of this variable
                            Nodecl::List private_syms = it->as<Nodecl::OpenMP::Private>( ).get_symbols( ).as<Nodecl::List>( );
                            for( Nodecl::List::iterator it_p = private_syms.begin( ); it_p != private_syms.end( ); ++it_p )
                            {
                                if( Utils::ext_sym_set_contains_nodecl( *it_p, undef_vars ) )
                                {
                                    undef_vars.erase( Utils::ExtendedSymbol( *it_p ) );
                                }
                                else
                                {
                                    if( Utils::ext_sym_set_contains_nodecl( *it_p, ue_vars ) )
                                        ue_vars.erase( Utils::ExtendedSymbol( *it_p ) );
                                    if( Utils::ext_sym_set_contains_nodecl( *it_p, killed_vars ) )
                                        killed_vars.erase( Utils::ExtendedSymbol( *it_p ) );
                                }
                            }
                        }
                        if( it->is<Nodecl::OpenMP::Firstprivate>( ) )
                        {   // This variable is Upper Exposed in the task
                            Nodecl::List firstprivate_syms = it->as<Nodecl::OpenMP::Firstprivate>( ).get_symbols( ).as<Nodecl::List>( );
                            for( Nodecl::List::iterator it_fp = firstprivate_syms.begin( ); it_fp != firstprivate_syms.end( ); ++it_fp )
                            {
                                if( !Utils::ext_sym_set_contains_nodecl( *it_fp, ue_vars ) )
                                    ue_vars.insert( Utils::ExtendedSymbol( *it_fp ) );
                            }
                        }
                    }
                }

                current->set_ue_var( ue_vars );
                current->set_killed_var( killed_vars );
                current->set_undefined_behaviour_var( undef_vars );
            }
        }
        else
        {
            internal_error( "Cannot propagate use-def info from inner nodes to outer nodes "\
                            "in node '%d' with type '%s'. GRAPH_NODE expected\n",
                            current->get_id( ), current->get_type_as_string( ).c_str( ) );
        }
    }

    // ************************** End class implementing use-definition analysis ************************** //
    // **************************************************************************************************** //



    // **************************************************************************************************** //
    // ***************************** Class implementing use-definition visitor **************************** //

    static void get_use_def_variables( Node* actual, int id_target_node,
                                       Utils::ext_sym_set &ue_vars,
                                       Utils::ext_sym_set &killed_vars,
                                       Utils::ext_sym_set &undef_vars )
    {
        ObjectList<Node*> children = actual->get_children( );
        for( ObjectList<Node*>::iterator it = children.begin( ); it != children.end( ); ++it )
        {
            if( ( *it )->get_id( ) != id_target_node )
            {
                ue_vars = ext_sym_set_union( ue_vars, ( *it )->get_ue_vars( ) );
                killed_vars = ext_sym_set_union( killed_vars, ( *it )->get_killed_vars( ) );
                undef_vars = ext_sym_set_union( undef_vars, ( *it )->get_undefined_behaviour_vars( ) );

                get_use_def_variables( *it, id_target_node, ue_vars, killed_vars, undef_vars );
            }
        }
    }

    static sym_to_nodecl_map map_reference_params_to_args( ObjectList<TL::Symbol> parameters,
                                                           Nodecl::List arguments )
    {
        sym_to_nodecl_map ref_params_to_args;

        ObjectList<TL::Symbol>::iterator itp = parameters.begin( );
        Nodecl::List::iterator ita = arguments.begin( );

        //TODO: parameters.size() must be == to arguments.size()
        for( ; ( ita != arguments.end( ) ) && ( itp != parameters.end( ) ); ++itp, ++ita )
        {
            Type param_type = itp->get_type( );
            if( ( param_type.is_any_reference( ) || param_type.is_pointer( ) ) )
                ref_params_to_args[*itp] = *ita;
        }

        return ref_params_to_args;
    }

    static sym_to_nodecl_map map_non_reference_params_to_args( ObjectList<TL::Symbol> parameters,
                                                               Nodecl::List arguments )
    {
        sym_to_nodecl_map non_ref_params_to_args;

        ObjectList<TL::Symbol>::iterator itp = parameters.begin( );
        Nodecl::List::iterator ita = arguments.begin( );

        //TODO: parameters.size() must be == to arguments.size()
        for( ; ( ita != arguments.end( ) ) && ( itp != parameters.end( ) ); ++itp, ++ita )
        {
            Type param_type = itp->get_type( );
            if( !param_type.is_any_reference( ) && !param_type.is_pointer( ) )
            {
                // If some memory access in the argument is a symbol, then we add the tuple to the map
                ObjectList<Nodecl::NodeclBase> obj = Nodecl::Utils::get_all_memory_accesses( *ita );
                for( ObjectList<Nodecl::NodeclBase>::iterator it = obj.begin( ); it != obj.end( ); ++it )
                {
                    if( !it->is_constant( ) )
                    {
                        non_ref_params_to_args[*itp] = *ita;
                        break;
                    }
                }
            }
        }

        return non_ref_params_to_args;
    }

    UsageVisitor::UsageVisitor( Node* fake_node )
        : _node( fake_node ), _define( false ), _current_nodecl( Nodecl::NodeclBase::null( ) ),
          _global_vars( ), _reference_params( ), _avoid_func_calls( false ), _pcfg( NULL ), _pcfgs( )
    {}
    
    UsageVisitor::UsageVisitor( Node* n,
                                ExtensibleGraph* pcfg,
                                ObjectList<ExtensibleGraph*>* pcfgs,
                                std::map<Symbol, Utils::UsageKind>* global_vars,
                                std::map<Symbol, Utils::UsageKind>* reference_params )
        : _node( n ), _define( false ), _current_nodecl( Nodecl::NodeclBase::null( ) ),
          _global_vars( global_vars ), _reference_params( reference_params ), 
          _avoid_func_calls( false ), _pcfg( pcfg ), _pcfgs( pcfgs )
    {}
    
    void UsageVisitor::compute_statement_usage( Nodecl::NodeclBase st )
    {
        Node* outer_node = _node->get_outer_node( );
        if( outer_node->is_split_statement( ) && !_node->is_function_call_node( ) )
        {   // The function calls that can appear in the split statement have already been analyzed
            // We want to avoid computing the usage again. In exchange, we want to propagate the previously compute usage
            // F.i.:   int c = foo(a, b)
            //         PCFG:
            //           ______________________________________________
            //          |  [SPLIT_STMT]                                |
            //          |  __________________________________________  |
            //          | | [FUNC_CALL]                              | |
            //          | |  _______       ___________       ______  | |
            //          | | |       |     |           |     |      | | |
            //          | | | ENTRY |---->| foo(a, b) |---->| EXIT | | |
            //          | | |_______|     |___________|     |______| | |
            //          | |__________________________________________| |
            //          |               _______|_______                |
            //          |              |               |               |
            //          |              | c = foo(a, b) |               |
            //          |              |_______________|               |
            //          |______________________________________________|
            //
            //         When computing Use-Def of "c = foo(a, b)", we want to propagate
            //             the info calculated for "b=foo(a, b)" regarding to the function call
            ObjectList<Node*> parents = _node->get_parents( );
            while( !parents.empty( ) && !parents[0]->is_entry_node( ) )
            {
                ERROR_CONDITION( parents.size( ) != 1, 
                                 "Ancestors of a non function call node which are inside the enclosing split statement "\
                                 "must not have any sibling, but we have found %d siblings", parents.size( ) );
                
                _node->set_ue_var( parents[0]->get_ue_vars( ) );
                _node->set_killed_var( parents[0]->get_killed_vars( ) );
                _node->set_undefined_behaviour_var( parents[0]->get_undefined_behaviour_vars( ) );
                
                parents = parents[0]->get_parents( );
            }
            
            _avoid_func_calls = true;
        }
        
        walk( st );
    }

    void UsageVisitor::unhandled_node( const Nodecl::NodeclBase& n )
    {
        nodecl_t internal_n = n.get_internal_nodecl( );
        WARNING_MESSAGE( "Unhandled node '%s' with type '%s' during Use-Def Analysis",
                         codegen_to_str( internal_n, nodecl_retrieve_context( internal_n ) ),
                         ast_print_node_type( n.get_kind( ) ) );
    }

    template<typename T>
    void UsageVisitor::visit_assignment( const T& n )
    {
        _define = false;
        walk( n.get_rhs( ) );
        _define = true;
        walk( n.get_lhs( ) );
        _define = false;
    }
    
    template<typename T>
    void UsageVisitor::visit_binary_assignment( const T& n )
    {
        // Traverse the use of both the lhs and the rhs
        walk( n.get_rhs( ) );
        walk( n.get_lhs( ) );

        // Traverse the definition of the lhs
        _define = true;
        walk( n.get_lhs( ) );
        _define = false;
    }

    void UsageVisitor::parse_parameter( std::string current_param, const Nodecl::NodeclBase& arg )
    {
        size_t first_slash_pos = current_param.find( "#" );
        if( first_slash_pos != std::string::npos )
        {   // Parameter is pointer
            // The address is used
            _node->set_ue_var( Utils::ExtendedSymbol( arg ) );
            size_t second_slash_pos = current_param.find( "#", first_slash_pos );
            std::string pointed_param_usage = current_param.substr( first_slash_pos, second_slash_pos - first_slash_pos );
            // TODO: What do we want to do with the pointed value??
        }
        else
        {
            ObjectList<Nodecl::NodeclBase> obj = Nodecl::Utils::get_all_memory_accesses( arg );
            for( ObjectList<Nodecl::NodeclBase>::iterator it_o = obj.begin( ); it_o != obj.end( ); ++it_o )
            {
                // Set all arguments as upper exposed
                Symbol s( it_o->get_symbol( ) );
                if( !s.is_valid( ) )
                {   // ArraySubscript and ClassMemberAccess are memory accesses but do not have a symbol associated
                    // In these cases, we need to get the nodecl base
                    s = Utils::ExtendedSymbol::get_nodecl_base( *it_o ).get_symbol( );
                    ERROR_CONDITION( !s.is_valid( ), 
                                     "A memory access must have a symbol associated, but %s does not have", it_o->prettyprint( ).c_str( ) );
                }
                _node->set_ue_var( Utils::ExtendedSymbol( *it_o ) );
                if( ( _global_vars->find( s ) != _global_vars->end( ) ) && 
                    (*_global_vars)[s]._usage_type & Utils::UsageKind::NONE )
                {
                    (*_global_vars)[s] = Utils::UsageKind::USED;
                }
                else if( ( _reference_params->find( s ) != _reference_params->end( ) ) && 
                         ( (*_reference_params)[s]._usage_type & Utils::UsageKind::NONE ) )
                {
                    (*_reference_params)[s] = Utils::UsageKind::USED;
                }
            }
        }
    }

    bool UsageVisitor::parse_c_functions_file( Symbol func_sym, const Nodecl::List& args )
    {
        bool side_effects = true;

        std::string cLibFuncsPath = std::string( MCXX_ANALYSIS_DATA_PATH ) + "/cLibraryFunctionList" ;
        std::ifstream cLibFuncs( cLibFuncsPath.c_str( ) );
        if( cLibFuncs.is_open( ) )
        {
            std::string func_decl;
            while( cLibFuncs.good( ) )
            {
                getline( cLibFuncs, func_decl );
                if( func_decl.substr( 0, 2 ) != "//" )
                {
                    size_t open_parenth_pos = func_decl.find( "(" );
                    std::string func_name = func_decl.substr( 0, open_parenth_pos - 1 );
                    if( func_sym.get_name( ) == func_name )
                    {   // No global variable is read / written
                        // Check for parameters usage
                        side_effects = false;

                        size_t comma_pos = func_decl.find( "," );
                        if( comma_pos == std::string::npos )
                        {
                            comma_pos = func_decl.find( ")" );
                        }
                        size_t last_comma_pos = open_parenth_pos + 1;
                        std::string current_param;
                        Nodecl::List::iterator it = args.begin( );
                        while( comma_pos != std::string::npos && /* not a default parameter*/ it != args.end( ) )
                        {
                            current_param = func_decl.substr( last_comma_pos, comma_pos - last_comma_pos );
                            parse_parameter( current_param, *it );
                            it++;
                            last_comma_pos = comma_pos + 1;
                            comma_pos = func_decl.find( ",", last_comma_pos );
                        }
                        // Last parameter
                        if( it != args.end( ) )
                        {
                            current_param = func_decl.substr( last_comma_pos, func_decl.find( ")", last_comma_pos ) - last_comma_pos );
                            if( current_param == "..." )
                            {   // Arguments are supposed to be only used
                                ObjectList<Nodecl::NodeclBase> obj;
                                while( it != args.end( ) )
                                {
                                    obj = Nodecl::Utils::get_all_memory_accesses( *it );
                                    for( ObjectList<Nodecl::NodeclBase>::iterator it_o = obj.begin( ); it_o  != obj.end( ); ++it_o )
                                    {
                                        _node->set_ue_var( Utils::ExtendedSymbol( *it_o ) );
                                    }
                                    ++it;
                                }
                            }
                            else
                            {
                                parse_parameter( current_param, *it );
                            }
                        }
                    }
                }
            }

            if( side_effects && VERBOSE )
            {
                WARNING_MESSAGE( "Function's '%s' code not reached. \nUsage of global variables and "\
                                  "reference parameters will be limited. \nIf you know the side effects of this function, "\
                                  "add it to the file and recompile your code. \n(If you recompile the compiler, "\
                                  "you want to add the function in $MCC_HOME/src/tl/analysis/use_def/cLibraryFunctionList instead).",
                                  func_sym.get_name( ).c_str( ), cLibFuncsPath.c_str( ) );
            }
            cLibFuncs.close();
        }
        else
        {
            WARNING_MESSAGE( "File containing C library calls Usage info cannot be opened. \n"\
                             "Path tried: '%s'", cLibFuncsPath.c_str( ) );
        }

        return side_effects;
    }

    static ExtensibleGraph* find_graph_in_list_from_function_symbol( Symbol func_sym, ObjectList<ExtensibleGraph*>* pcfgs )
    {
        ExtensibleGraph* result = NULL;
        for( ObjectList<ExtensibleGraph*>::iterator it = pcfgs->begin( ); it != pcfgs->end( ); ++it )
        {
            Symbol s( ( *it )->get_function_symbol( ) );
            if( s.is_valid( ) && ( s == func_sym ) )
            {
                result = *it;
                break;
            }
        }
        return result;
    }
    
    static bool nodecl_is_dereference( const Nodecl::NodeclBase& n )
    {
        bool result = false;
        if( n.is<Nodecl::Dereference>( ) )
            result = true;
        else if( n.is<Nodecl::Cast>( ) )
            result = nodecl_is_dereference( n.as<Nodecl::Cast>( ).get_rhs( ) );
        else if( n.is<Nodecl::Conversion>( ) )
            result = nodecl_is_dereference( n.as<Nodecl::Conversion>( ).get_nest( ) );
        return result;
    }
    
    static sym_to_nodecl_map map_params_to_args( const ObjectList<TL::Symbol>& parameters, 
                                                 const Nodecl::List& arguments )
    {
        sym_to_nodecl_map result;
        int n_iters = std::min( arguments.size( ), parameters.size( ) );
        if( n_iters > 0 )
        {
            Nodecl::List::const_iterator ita = arguments.begin( );
            ObjectList<TL::Symbol>::const_iterator itp = parameters.begin( );
            int i;
            for( i = 0; i < n_iters; ++i  )
            {
                result[*itp] = *ita;
                ita++; itp++;
            }
        }
        return result;
    }
    
    static Nodecl::NodeclBase rename_param_usage_to_argument( const Nodecl::NodeclBase& n,
                                                              const ObjectList<TL::Symbol>& parameters, 
                                                              const Nodecl::List& arguments )
    {
        sym_to_nodecl_map param_to_arg_map = map_params_to_args( parameters, arguments );
        Nodecl::NodeclBase var_copy = n.shallow_copy( );
        RenameVisitor rv( param_to_arg_map );
        rv.rename_expressions( var_copy );
        return var_copy;
    }
    
    Utils::ext_sym_set UsageVisitor::get_ipa_usage( Utils::UsageKind usage_kind, const Utils::ext_sym_set& list,
                                                    const Nodecl::List& arguments, const TL::Symbol& func_sym )
    {
        Utils::ext_sym_set result;
        ObjectList<TL::Symbol> parameters = func_sym.get_function_parameters( );
        for( Utils::ext_sym_set::iterator it = list.begin( ); it != list.end( ); ++it )
        {
            Nodecl::NodeclBase it_nodecl = it->get_nodecl( );
            Nodecl::NodeclBase var = Utils::ExtendedSymbol::get_nodecl_base( it_nodecl );
            Symbol s( var.get_symbol( ) );
            ERROR_CONDITION( !s.is_valid( ), 
                             "The base nodecl of an extended symbol must have a symbol associated, but %s does not have one", 
                             it_nodecl.prettyprint( ).c_str( ) );
            if( _global_vars->find( s ) != _global_vars->end( ) )
            {
                Nodecl::NodeclBase var_copy = rename_param_usage_to_argument( it_nodecl, parameters, arguments );
                result.insert( var_copy );
                (*_global_vars)[s] = usage_kind;
            }
            else
            {
                ObjectList<TL::Symbol> var_param = parameters.find( s );
                if( !var_param.empty( ) )
                {
                    Type var_param_type = var_param[0].get_type( );
                    if( var_param_type.is_any_reference( ) || var_param_type.is_pointer( ) )
                    {
                        // Find the matching between arguments and parameters
                        int n_iters = std::min( arguments.size( ), parameters.size( ) );
                        if( n_iters > 0 )
                        {
                            Nodecl::List::const_iterator ita = arguments.begin( );
                            ObjectList<TL::Symbol>::const_iterator itp = parameters.begin( );
                            int i;
                            for( i = 0; i < n_iters; ++i  )
                            {
                                if( *itp == var_param[0] )
                                    break;
                                ita++; itp++;
                            }
                            ERROR_CONDITION( i > n_iters, "Parameter %s not found in function %s\n", 
                                             itp->get_name( ).c_str( ), func_sym.get_name( ).c_str( ) );
                            
                            Type param_type = itp->get_type( );
                            if( param_type.is_any_reference( ) )
                            {   // type & variable
                                ERROR_CONDITION( !ita->get_symbol( ).is_valid( ), "Invalid argument %s passed by reference", 
                                                                               ita->prettyprint( ).c_str( ) );
                                Nodecl::NodeclBase var_copy = rename_param_usage_to_argument( it_nodecl, parameters, arguments );
                                result.insert( Utils::ExtendedSymbol( var_copy ) );
                                (*_reference_params)[s] = usage_kind;
                            }
                            else if( param_type.is_pointer( ) )
                            {   // type* variable
                                // Replace the used var by the argument used in the call
                                Nodecl::NodeclBase var_copy = rename_param_usage_to_argument( it_nodecl, parameters, arguments );
                                if( usage_kind._usage_type & Utils::UsageKind::DEFINED ) // The list being treated is a Kill list
                                {
                                    if( nodecl_is_dereference( it_nodecl ) )   // *variable = ...
                                    {
                                        result.insert( Utils::ExtendedSymbol( var_copy ) );
                                        (*_reference_params)[s] = usage_kind;
                                    }
                                    else
                                    {}  // variable = ...
                                        // Nothing to be done because the address cannot be changed
                                }
                                else
                                {
                                    result.insert( Utils::ExtendedSymbol( var_copy ) );
                                    (*_reference_params)[s] = usage_kind;
                                }
                            }
                            else
                            {} // Nothing tobe done because the function uses a copy of the argument
                        }
                    }
                }
            }
        }
        return result;
    }
    
    static std::map<Symbol, Utils::UsageKind> set_of_symbols_to_usage_map( const std::set<Symbol>& s )
    {
        std::map<Symbol, Utils::UsageKind> result;
        for( std::set<Symbol>::iterator it = s.begin( ); it != s.end( ); ++it )
            result[*it] = Utils::UsageKind::NONE;
        return result;
    }
    
    void UsageVisitor::function_visit( const Nodecl::NodeclBase& called_sym, const Nodecl::List& arguments )
    {
        if( _avoid_func_calls )
            return;
        
        // The function called must be analyzed only in case it has not been analyzed previously
        TL::Symbol func_sym = called_sym.get_symbol( );
        if( func_sym.is_valid( ) )
        {   // The called function is not a pointer to function
            ObjectList<TL::Symbol> params = func_sym.get_function_parameters( );
            ExtensibleGraph* called_pcfg = find_graph_in_list_from_function_symbol( func_sym, _pcfgs );
            if( called_pcfg != NULL )
            {   // Called function code is reachable
                if( called_pcfg->usage_is_computed( ) )
                {
                    // Propagate values that have been computed in the called graph 
                    // mapping parameters usage into arguments
                    Node* pcfg_node = called_pcfg->get_graph( );
                    std::set<Symbol> call_graph_global_vars_set = called_pcfg->get_global_variables( );
                    _pcfg->set_global_vars( call_graph_global_vars_set );
                    std::map<Symbol, Utils::UsageKind> call_graph_global_vars_map = set_of_symbols_to_usage_map( call_graph_global_vars_set );
                    _global_vars->insert( call_graph_global_vars_map.begin( ), call_graph_global_vars_map.end( ) );
                    Utils::ext_sym_set ue_vars = get_ipa_usage( Utils::UsageKind::USED, pcfg_node->get_ue_vars( ), 
                                                                arguments, func_sym );
                    Utils::ext_sym_set killed_vars = get_ipa_usage( Utils::UsageKind::DEFINED, pcfg_node->get_killed_vars( ), 
                                                                    arguments, func_sym );
                    Utils::ext_sym_set undef_vars = get_ipa_usage( Utils::UsageKind::UNDEFINED, pcfg_node->get_undefined_behaviour_vars( ), 
                                                                   arguments, func_sym );

                    // Add the usage of the arguments, since they are, at least, read
                    UsageVisitor uv( _node, _pcfg, _pcfgs, _global_vars, _reference_params );
                    for( Nodecl::List::const_iterator it = arguments.begin( ); it != arguments.end( ); ++it )
                    {
                        Nodecl::NodeclBase it_nodecl = it->no_conv();
                        while( it_nodecl.is<Nodecl::Cast>( ) )
                            it_nodecl = it_nodecl.as<Nodecl::Cast>( ).get_rhs( );
                        if( !it_nodecl.is<Nodecl::Symbol>( ) || 
                            !it_nodecl.get_symbol( ).is_valid( ) || 
                            !it_nodecl.get_symbol( ).is_function( ) )
                        {
                            uv.compute_statement_usage( *it );
                        }
                    }
                    
                    _node->set_ue_var( ue_vars );
                    _node->set_killed_var( killed_vars );
                    _node->set_undefined_behaviour_var( undef_vars );
                }
                else
                {   // We are arguments are used when calling the function
                    for( Nodecl::List::const_iterator it = arguments.begin( ); it != arguments.end( ); ++it )
                    {
                        ObjectList<Nodecl::NodeclBase> obj = Nodecl::Utils::get_all_memory_accesses( *it );
                        for( ObjectList<Nodecl::NodeclBase>::iterator it_o = obj.begin( ); it_o != obj.end( ); ++it_o )
                            _node->set_ue_var( Utils::ExtendedSymbol( *it_o ) );
                    }

                    // Check for the usage in the graph of the function to propagate Usage (Global variables and reference parameters)
                    // until the point we are currently
                    for( std::map<Symbol, Utils::UsageKind>::iterator it = _global_vars->begin( ); it != _global_vars->end( ); ++it )
                    {
                        Nodecl::NodeclBase sym = Nodecl::Symbol::make( it->first );
                        if( it->second._usage_type & Utils::UsageKind::UNDEFINED )
                            _node->set_undefined_behaviour_var( Utils::ExtendedSymbol( sym ) );
                        else
                        {
                            if( it->second._usage_type & Utils::UsageKind::USED )
                                _node->set_ue_var( Utils::ExtendedSymbol( sym ) );
                            if( it->second._usage_type & Utils::UsageKind::DEFINED )
                                _node->set_killed_var( Utils::ExtendedSymbol( sym ) );
                        }
                    }
                    for( std::map<Symbol, Utils::UsageKind>::iterator it = _reference_params->begin( ); 
                         it != _reference_params->end( ); ++it )
                    {
                        Nodecl::NodeclBase sym = Nodecl::Symbol::make( it->first );
                        if( it->second._usage_type & Utils::UsageKind::UNDEFINED )
                            _node->set_undefined_behaviour_var( Utils::ExtendedSymbol( sym ) );
                        else
                        {
                            if( it->second._usage_type & Utils::UsageKind::USED )
                                _node->set_ue_var( Utils::ExtendedSymbol( sym ) );
                            if( it->second._usage_type & Utils::UsageKind::DEFINED )
                                _node->set_killed_var( Utils::ExtendedSymbol( sym ) );
                        }
                    }
                }
            }
            else
            {   // Called function code is not reachable
                // Check whether we have enough attributes in the function symbol
                // to determine the function side effects
                bool side_effects = true;

                if( func_sym.has_gcc_attributes( ) )
                {   // Check for information synthesized by gcc
                    ObjectList<GCCAttribute> gcc_attrs = func_sym.get_gcc_attributes( );
                    for( ObjectList<GCCAttribute>::iterator it = gcc_attrs.begin( );
                            it != gcc_attrs.end( ); ++it )
                    {
                        std::string attr_name = it->get_attribute_name( );
                        if( attr_name == "const" || attr_name == "pure" )
                        {   // No side effects except the return value.
                            // Only examine the arguments ( and global variables in 'pure' case)
                            side_effects = false;

                            Utils::ext_sym_set ue_vars;
                            // Set all parameters as used ( if not previously killed or undefined )
                            for( Nodecl::List::iterator it_arg = arguments.begin( ); it_arg != arguments.end( ); ++it_arg )
                            {
                                Utils::ExtendedSymbol es( *it_arg );
                                if( _node->get_killed_vars( ).find( es ) == _node->get_killed_vars( ).end( )
                                    && _node->get_undefined_behaviour_vars( ).find( es ) == _node->get_undefined_behaviour_vars( ).end( ) )
                                {
                                    ue_vars.insert( es );
                                }
                            }

                            if( attr_name == "pure" )
                            {   // Set all global variables variables as upper exposed ( if not previously killed or undefined )
                                for( std::map<Symbol, Utils::UsageKind>::iterator it_g = _global_vars->begin( ); 
                                     it_g != _global_vars->end( ); ++it_g )
                                {
                                    if( it_g->second._usage_type & Utils::UsageKind::NONE )
                                    {
                                        ue_vars.insert( Utils::ExtendedSymbol( Nodecl::Symbol::make( it_g->first ) ) );
                                        it_g->second._usage_type = Utils::UsageKind::USED;
                                    }
                                }
                            }
                            _node->set_ue_var( ue_vars );
                            if( attr_name == "pure" )
                                break;
                        }
                    }
                }

                if( side_effects )
                {
                    // Check in Mercurium function attributes data-base
                    side_effects = parse_c_functions_file( func_sym, arguments );

                    // Still cannot determine which are the side effects of the function...
                    if( side_effects )
                    {
                        if( func_sym.get_type( ).lacks_prototype( ) )
                        {   // All parameters are passed by value
                            for( Nodecl::List::iterator it = arguments.begin( ); it != arguments.end( ); ++it )
                                if( !it->is_constant( ) )
                                {
                                    ObjectList<Nodecl::NodeclBase> mem_access = Nodecl::Utils::get_all_memory_accesses( *it );
                                    for( ObjectList<Nodecl::NodeclBase>::iterator ita = mem_access.begin( ); 
                                        ita != mem_access.end( ); ++ita )
                                    {
                                        _node->set_ue_var( Utils::ExtendedSymbol( *ita ) );
                                        if( ita->get_type( ).is_pointer( ) )
                                        {
                                            Nodecl::Dereference pointed_var = Nodecl::Dereference::make( *ita, ita->get_type( ) );
                                            _node->set_undefined_behaviour_var( Utils::ExtendedSymbol( pointed_var ) );
                                        }
                                    }
                                }
                        }
                        else
                        {
                            Utils::ext_sym_set killed = _node->get_killed_vars( );
                            Utils::ext_sym_set undef = _node->get_undefined_behaviour_vars( );
                            
                            // Set all reference parameters to undefined
                            sym_to_nodecl_map ref_params = map_reference_params_to_args( params, arguments );
                            for( sym_to_nodecl_map::iterator it = ref_params.begin( ); it != ref_params.end( ); ++it )
                            {
                                if( Nodecl::Utils::nodecl_is_modifiable_lvalue( it->second ) && 
                                    !Utils::ext_sym_set_contains_enclosing_nodecl( it->second, killed ) && 
                                    !Utils::ext_sym_set_contains_enclosed_nodecl( it->second, killed ) )
                                {
                                    _node->set_undefined_behaviour_var_and_recompute_use_and_killed_sets(
                                        Utils::ExtendedSymbol( it->second ) );
                                }
                            }
                            
                            // Set the value passed parameters as upper exposed
                            sym_to_nodecl_map non_ref_params = map_non_reference_params_to_args( params, arguments );
                            for( sym_to_nodecl_map::iterator it = non_ref_params.begin( ); it != non_ref_params.end( ); ++it )
                            {
                                ObjectList<Nodecl::NodeclBase> obj = Nodecl::Utils::get_all_memory_accesses( it->second );
                                for( ObjectList<Nodecl::NodeclBase>::iterator it_o = obj.begin( ); it_o != obj.end( ); ++it_o )
                                {
                                    if( !Utils::ext_sym_set_contains_enclosing_nodecl( it->second, killed ) && 
                                        !Utils::ext_sym_set_contains_enclosed_nodecl( it->second, killed ) && 
                                        !Utils::ext_sym_set_contains_enclosing_nodecl( it->second, undef ) &&
                                        !Utils::ext_sym_set_contains_enclosed_nodecl( it->second, undef ) )
                                    {
                                        _node->set_ue_var( Utils::ExtendedSymbol( *it_o ) );
                                    }
                                }
                                    
                            }

                            // Set all global variables to undefined
                            for( std::map<Symbol, Utils::UsageKind>::iterator it = _global_vars->begin( ); 
                                 it != _global_vars->end( ); ++it )
                            {
                                Nodecl::NodeclBase sym = Nodecl::Symbol::make( it->first );
                                if( !Utils::ext_sym_set_contains_enclosing_nodecl( sym, killed ) && 
                                    !Utils::ext_sym_set_contains_enclosed_nodecl( sym, killed ) )
                                {
                                    _node->set_undefined_behaviour_var_and_recompute_use_and_killed_sets( Utils::ExtendedSymbol( sym ) );
                                    it->second._usage_type = Utils::UsageKind::UNDEFINED;
                                }
                            }
                        }
                    }
                }
            }
        }
        else
        {   // Calling a pointer to function: neither code nor prototype are reachable, thus:
            // - all parameters as undefined behavior, we do not know whether they are passed by value or by reference
            // - all global variables as undefined behavior
            Utils::ext_sym_set killed = _node->get_killed_vars( );
            for( Nodecl::List::iterator it = arguments.begin( ); it != arguments.end( ); ++it )
            {
                if( !Utils::ext_sym_set_contains_enclosing_nodecl( *it, killed ) && 
                    !Utils::ext_sym_set_contains_enclosed_nodecl( *it, killed ) )
                {
                    _node->set_undefined_behaviour_var( Utils::ExtendedSymbol( *it ) );
                }
            }
            
            for( std::map<Symbol, Utils::UsageKind>::iterator it = _global_vars->begin( ); it != _global_vars->end( ); ++it )
            {
                Nodecl::NodeclBase sym = Nodecl::Symbol::make( it->first );
                if( !Utils::ext_sym_set_contains_enclosing_nodecl( sym, killed ) && 
                    !Utils::ext_sym_set_contains_enclosed_nodecl( sym, killed ) )
                {
                    _node->set_undefined_behaviour_var_and_recompute_use_and_killed_sets( Utils::ExtendedSymbol( sym ) );
                    it->second._usage_type = Utils::UsageKind::UNDEFINED;
                }
            }
        }
    }

    template<typename T>
    void UsageVisitor::visit_increment( const T& n )
    {
        // Use of the rhs
        walk( n.get_rhs( ) );

        // Definition of the rhs
        _define = true;
        Nodecl::NodeclBase current_nodecl = _current_nodecl;
        _current_nodecl = Nodecl::NodeclBase::null( );
        walk( n.get_rhs( ) );
        _current_nodecl = current_nodecl;
        _define = false;

    }

    void UsageVisitor::visit( const Nodecl::AddAssignment& n )
    {
        visit_binary_assignment( n );
    }

    void UsageVisitor::visit( const Nodecl::ArithmeticShrAssignment& n )
    {
        visit_binary_assignment( n );
    }

    void UsageVisitor::visit( const Nodecl::ArraySubscript& n )
    {
        Nodecl::NodeclBase current_nodecl = _current_nodecl;
        bool define = _define;

        // Use of the subscripts
        _define = false;
        _current_nodecl = Nodecl::NodeclBase::null( );
        walk( n.get_subscripts( ) );

        // Use of the ArraySubscript
        _define = define;   // Just in case
        if( current_nodecl.is_null( ) )
            _current_nodecl = n;
        else
            _current_nodecl = current_nodecl;
        walk( n.get_subscripted( ) );
        _current_nodecl = Nodecl::NodeclBase::null( );
    }
    
    void UsageVisitor::visit( const Nodecl::Assignment& n )
    {
        visit_assignment( n );
    }

    void UsageVisitor::visit( const Nodecl::BitwiseAndAssignment& n )
    {
        visit_binary_assignment( n );
    }

    void UsageVisitor::visit( const Nodecl::BitwiseOrAssignment& n )
    {
        visit_binary_assignment( n );
    }

    void UsageVisitor::visit( const Nodecl::BitwiseShlAssignment& n )
    {
        visit_binary_assignment( n );
    }

    void UsageVisitor::visit( const Nodecl::BitwiseShrAssignment& n )
    {
        visit_binary_assignment( n );
    }

    void UsageVisitor::visit( const Nodecl::BitwiseXorAssignment& n )
    {
        visit_binary_assignment( n );
    }

    void UsageVisitor::visit( const Nodecl::ClassMemberAccess& n )
    {
        if( _current_nodecl.is_null( ) )
            _current_nodecl = n;

        // walk( n.get_lhs( ) );  // In a member access, the use/definition is always of the member, not the base
        walk( n.get_member( ) );

        _current_nodecl = Nodecl::NodeclBase::null( );
    }

    void UsageVisitor::visit( const Nodecl::Dereference& n )
    {
        Nodecl::NodeclBase current_nodecl = _current_nodecl;
        bool define = _define;

        // Use of the Dereferenced variable
        _define = false;
        _current_nodecl = Nodecl::NodeclBase::null( );
        walk( n.get_rhs( ) );

        // Use of the Dereference
        if( current_nodecl.is_null( ) )
        {
            _define = define;
            _current_nodecl = n;
        }
        
        walk( n.get_rhs( ) );
        
        // If we were traversing some object, then the use of that access
        if( !current_nodecl.is_null( ) )
        {
            _define = define;       // Just in case
            _current_nodecl = current_nodecl;
            walk( n.get_rhs( ) );
        }
        
        _current_nodecl = Nodecl::NodeclBase::null( );
    }

    void UsageVisitor::visit( const Nodecl::DivAssignment& n )
    {
        visit_binary_assignment( n );
    }

    void UsageVisitor::visit( const Nodecl::FunctionCall& n )
    {
        function_visit( n.get_called( ), n.get_arguments( ).as<Nodecl::List>( ) );
    }

    void UsageVisitor::visit( const Nodecl::MinusAssignment& n )
    {
        visit_binary_assignment( n );
    }

    void UsageVisitor::visit( const Nodecl::ModAssignment& n )
    {
        visit_binary_assignment( n );
    }

    void UsageVisitor::visit( const Nodecl::MulAssignment& n )
    {
        visit_binary_assignment( n );
    }

    void UsageVisitor::visit( const Nodecl::ObjectInit& n )
    {
        Nodecl::Symbol n_sym = Nodecl::Symbol::make( n.get_symbol( ), n.get_locus() );
        _node->set_killed_var( Utils::ExtendedSymbol( n_sym ) );

        // Value of initialization, in case it exists
        walk( n.get_symbol( ).get_value( ) );
    }

    void UsageVisitor::visit( const Nodecl::Postdecrement& n )
    {
        visit_increment( n );
    }

    void UsageVisitor::visit( const Nodecl::Postincrement& n )
    {
        visit_increment( n );
    }

    void UsageVisitor::visit( const Nodecl::Predecrement& n )
    {
        visit_increment( n );
    }

    void UsageVisitor::visit( const Nodecl::Preincrement& n )
    {
        visit_increment( n );
    }

    void UsageVisitor::visit( const Nodecl::Range& n )
    {
        walk( n.get_lower() );
        walk( n.get_upper() );
        walk( n.get_stride() );
    }

    void UsageVisitor::visit( const Nodecl::Reference& n )
    {
        Nodecl::NodeclBase rhs = n.get_rhs( );
        if( !_current_nodecl.is_null( ) )
        {
            walk( rhs );
        }
        else
        {   // Only pointers to member are really used
            ReferenceUsageVisitor ruv;
            ruv.walk( rhs );
            _node->set_ue_var( ruv.get_ue_vars( ) );
        }
    }

    void UsageVisitor::visit( const Nodecl::Symbol& n )
    {
        Nodecl::NodeclBase var_in_use = n;
        if( !_current_nodecl.is_null( ) )
            var_in_use = _current_nodecl;

        Symbol sym( var_in_use.get_symbol( ) ); 
        if( _define )
        {
            Utils::ExtendedSymbol ei( var_in_use );
            _node->set_killed_var( ei );
            
            if( _global_vars->find( sym ) != _global_vars->end( ) )
            {
                if( (*_global_vars)[sym]._usage_type & Utils::UsageKind::NONE )
                    (*_global_vars)[sym] = Utils::UsageKind::DEFINED;
                else if( (*_global_vars)[sym]._usage_type & Utils::UsageKind::USED )
                    (*_global_vars)[sym] = Utils::UsageKind::USED | Utils::UsageKind::DEFINED;
            }
            else if( ( _reference_params->find( sym ) != _reference_params->end( ) ) && 
                ( (*_reference_params)[sym]._usage_type & ( Utils::UsageKind::USED | Utils::UsageKind::NONE ) ) )
            {
                if( (*_reference_params)[sym]._usage_type & Utils::UsageKind::NONE )
                    (*_reference_params)[sym] = Utils::UsageKind::DEFINED;
                else if( (*_reference_params)[sym]._usage_type & Utils::UsageKind::USED )
                    (*_reference_params)[sym] = Utils::UsageKind::USED | Utils::UsageKind::DEFINED;
            }
        }
        else
        {
            if( !Utils::ext_sym_set_contains_nodecl( var_in_use, _node->get_killed_vars( ) ) )
            {
                _node->set_ue_var( Utils::ExtendedSymbol( var_in_use ) );
                
                if( ( _global_vars->find( sym ) != _global_vars->end( ) ) && 
                    ( (*_global_vars)[sym]._usage_type & Utils::UsageKind::NONE ) )
                {
                    (*_global_vars)[sym] = Utils::UsageKind::USED;
                }
                else if( ( _reference_params->find( sym ) != _reference_params->end( ) ) && 
                    ( (*_reference_params)[sym]._usage_type & Utils::UsageKind::NONE ) )
                {
                    (*_reference_params)[sym] = Utils::UsageKind::USED;
                }
            }
        }
    }
    
    void UsageVisitor::visit( const Nodecl::UnalignedVectorStore& n )
    {
        visit_assignment( n );
    }
    
    void UsageVisitor::visit( const Nodecl::VectorAssignment& n )
    {
        visit_assignment( n );
    }
    
    // It is used: the base, the strides (if variables) and the memory positions formed by base+stride_i
    void UsageVisitor::visit( const Nodecl::VectorGather& n )
    {
        Nodecl::NodeclBase base = n.get_base( );
        Nodecl::NodeclBase strides = n.get_strides( );
        
        // Usage of the base
        walk( base );

        if( strides.is<Nodecl::VectorLiteral>( ) )
        {
            Nodecl::List stride_list = strides.as<Nodecl::VectorLiteral>().get_scalar_values().as<Nodecl::List>();
            for( Nodecl::List::iterator it = stride_list.begin( ); it != stride_list.end( ); ++it )
            {
                // Usage of base+stride_i
                Nodecl::Add current_access = Nodecl::Add::make( base.shallow_copy( ), it->shallow_copy( ), base.get_type( ), it->get_locus( ) );
                if( !Utils::ext_sym_set_contains_nodecl( current_access, _node->get_killed_vars( ) ) )
                    _node->set_ue_var( Utils::ExtendedSymbol( current_access ) );
            }
        }
        else
        {
            // Usage of the stride
            walk( strides );
            
            Nodecl::Add current_access = Nodecl::Add::make( base.shallow_copy( ), strides.shallow_copy( ), base.get_type( ), strides.get_locus( ) );
            if( !Utils::ext_sym_set_contains_nodecl( current_access, _node->get_killed_vars( ) ) )
                _node->set_ue_var( Utils::ExtendedSymbol( current_access ) );
        }
    }
    
    void UsageVisitor::visit( const Nodecl::VectorMaskAssignment& n )
    {
        visit_assignment( n );
    }
    
    // It is used: the strides (if variables). It is defined the memory positions formed by base+stride_i
    void UsageVisitor::visit( const Nodecl::VectorScatter& n )
    {
        Nodecl::NodeclBase base = n.get_base( );
        Nodecl::NodeclBase strides = n.get_strides( );
        Nodecl::NodeclBase source = n.get_source( );
        
        // Usage of source and base
        walk( source );
        walk( base );

        if( strides.is<Nodecl::VectorLiteral>( ) )
        {
            Nodecl::List stride_list = strides.as<Nodecl::VectorLiteral>().get_scalar_values().as<Nodecl::List>();
            for( Nodecl::List::iterator it = stride_list.begin( ); it != stride_list.end( ); ++it )
            {
                // Usage of base+stride_i
                Nodecl::Add current_access = Nodecl::Add::make( base.shallow_copy( ), it->shallow_copy( ), base.get_type( ), it->get_locus( ) );
                if( !Utils::ext_sym_set_contains_nodecl( current_access, _node->get_killed_vars( ) ) )
                    _node->set_killed_var( Utils::ExtendedSymbol( current_access ) );
            }
        }
        else
        {
            // Usage of strides
            walk( strides );
            
            Nodecl::Add current_access = Nodecl::Add::make( base.shallow_copy( ), strides.shallow_copy( ), base.get_type( ), strides.get_locus( ) );
            if( !Utils::ext_sym_set_contains_nodecl( current_access, _node->get_killed_vars( ) ) )
                _node->set_killed_var( Utils::ExtendedSymbol( current_access ) );
        }

    }
    
    void UsageVisitor::visit( const Nodecl::VectorStore& n )
    {
        visit_assignment( n );
    }
    
    void UsageVisitor::visit( const Nodecl::VirtualFunctionCall& n )
    {
        function_visit( n.get_called( ), n.get_arguments( ).as<Nodecl::List>( ) );
    }

    ReferenceUsageVisitor::ReferenceUsageVisitor( )
        : _current_nodecl( Nodecl::NodeclBase::null( ) ), _store_symbol( false ), _used_ext_syms( )
    {}
    
    Utils::ext_sym_set ReferenceUsageVisitor::get_ue_vars( )
    {
        return _used_ext_syms;
    }
    
    void ReferenceUsageVisitor::visit( const Nodecl::ArraySubscript& n )
    {
        // Walk the base
        Nodecl::NodeclBase subscripted = n.get_subscripted( );
        if( subscripted.get_type( ).is_pointer( ) )
        {   // lhs is used only when it has pointer type
            _store_symbol = true;
            _current_nodecl = n;
            walk( subscripted );
            _current_nodecl = Nodecl::NodeclBase::null( );
            _store_symbol = false;
        }
        
        // Walk the subscripts
        _store_symbol = true;
        walk( n.get_subscripts( ) );
        _store_symbol = false;
    }
    
    void ReferenceUsageVisitor::visit( const Nodecl::ClassMemberAccess& n )
    {
        if( _current_nodecl.is_null( ) )
            _current_nodecl = n;
        walk( n.get_member( ) );
        _current_nodecl = Nodecl::NodeclBase::null( );
    }
    
    void ReferenceUsageVisitor::visit( const Nodecl::Reference& n )
    {
        if( _current_nodecl.is_null( ) )
            _current_nodecl = n;
        walk( n.get_rhs( ) );
        _current_nodecl = Nodecl::NodeclBase::null( );
    }
    
    void ReferenceUsageVisitor::visit( const Nodecl::Symbol& n )
    {
        if( _store_symbol )
        {
            Nodecl::NodeclBase var_in_use = n;
            if( !_current_nodecl.is_null( ) )
                var_in_use = _current_nodecl;
            
            _used_ext_syms.insert( var_in_use );
        }
    }
    
    // *************************** End class implementing use-definition visitor ************************** //
    // **************************************************************************************************** //

}
}
