/*--------------------------------------------------------------------
( C) Copyright 2006-2012 Barcelona Supercomputing Center             *
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
#include "tl-extended-symbol.hpp"
#include "tl-node.hpp"
#include "tl-pcfg-visitor.hpp"      // For IPA analysis
#include "tl-rename-visitor.hpp"
#include "tl-use-def.hpp"

namespace TL {
namespace Analysis {

    // **************************************************************************************************** //
    // **************************** Class implementing use-definition analysis **************************** //

    UseDef::UseDef( ExtensibleGraph* graph )
            : _graph( graph )
    {}

    void UseDef::compute_usage( ObjectList<TL::Symbol> visited_functions,
                                ObjectList<Utils::ExtendedSymbolUsage> visited_global_vars,
                                bool ipa, Utils::nodecl_set ipa_arguments )
    {
        Node* graph = _graph->get_graph( );
        visited_global_vars.insert( _graph->get_global_variables( ) );
        compute_usage_rec( graph, visited_functions, visited_global_vars, ipa, ipa_arguments );
        ExtensibleGraph::clear_visits( graph );
    }

    void UseDef::compute_usage_rec( Node* current, ObjectList<TL::Symbol>& visited_functions,
                                    ObjectList<Utils::ExtendedSymbolUsage>& visited_global_vars,
                                    bool ipa, Utils::nodecl_set ipa_arguments )
    {
        if( !current->is_visited( ) )
        {
            current->set_visited( true );

            if( current->is_exit_node( ) )
                return;

            if( current->is_graph_node( ) )
            {
                // Use-def info is computed from inner nodes to outer nodes
                compute_usage_rec( current->get_graph_entry_node( ),
                                   visited_functions, visited_global_vars,
                                   ipa, ipa_arguments );

                // Propagate usage info from inner to outer nodes
                ExtensibleGraph::clear_visits( current );
                set_graph_node_use_def( current );
            }
            else
            {
                // Treat statements in the current node
                ObjectList<Nodecl::NodeclBase> stmts = current->get_statements( );
                for( ObjectList<Nodecl::NodeclBase>::iterator it = stmts.begin( ); it != stmts.end( ); ++it )
                {
                    UsageVisitor uv( current, visited_functions, visited_global_vars,
                                     ipa, _graph->get_scope( ), ipa_arguments );
                    uv.compute_statement_usage( *it );

                    visited_functions.insert( uv.get_visited_functions( ) );
                    visited_global_vars.insert( uv.get_visited_global_variables( ) );
                }
            }

            // Compute usage form children
            ObjectList<Node*> children = current->get_children( );
            for( ObjectList<Node*>::iterator it = children.begin( ); it != children.end( ); ++it )
            {
                compute_usage_rec( *it,
                                   visited_functions, visited_global_vars,
                                   ipa, ipa_arguments );
            }
        }
    }

    /*!Try to insert a new variable in a list
     * If an englobing variable of the current variable already exists, then we don't include the variable
     * If any variable englobed by the current variable exists, then we delete the variable
     * If the variable is an array en we can form a range with the to access, we do that deleting the existing element of the list and
     * including the new ranged access
     */
    static Utils::ext_sym_set insert_var_in_list( Nodecl::NodeclBase var, Utils::ext_sym_set list )
    {
        Utils::ext_sym_set new_list;
        if( !Utils::ext_sym_set_contains_englobing_nodecl( var, list ) )
        {
            // Create a new list with the elements of 'list' that are not englobed by 'var'
            Utils::ext_sym_set aux_list;
            aux_list.insert( Utils::ExtendedSymbol( var ) );
            for( Utils::ext_sym_set::iterator it = list.begin( ); it != list.end( ); ++it )
            {
                if( !Utils::ext_sym_set_contains_englobing_nodecl( it->get_nodecl( ), aux_list ) )
                {
                    new_list.insert( *it );
                }
            }

            // Insert the new variable
            new_list.insert( var );
        }
        else
        {   // No need to insert the variable, its englobing symbol is already there
            // FIXME We can create ranges for array accesses here
            new_list = list;
        }
        return new_list;
    }

    /*!
     * Inserts the elements in 'l' to the list 'in_l' when they are not in the list 'killed' nor in 'undef'
     * When avoiding lists, it take cares of elements englobing the current variable and of elements englobed by the current variable
     */
    static Utils::ext_sym_set compute_use_def_with_children( Utils::ext_sym_set l, Utils::ext_sym_set in_l,
                                                             Utils::ext_sym_set& killed, Utils::ext_sym_set& undef,
                                                             char compute_undef )
    {
        Utils::ext_sym_set new_l = in_l;
        for( Utils::ext_sym_set::iterator it = l.begin( ); it != l.end( ); ++it )
        {
            Nodecl::NodeclBase var = it->get_nodecl( );
            if( !Utils::ext_sym_set_contains_englobing_nodecl( var, killed ) )
            {   // No englobing variable in the avoiding list 1
                // Look for variables in avoiding list 1 englobed by 'var'
                Utils::ext_sym_set aux_set;
                aux_set.insert( Utils::ExtendedSymbol( var ) );
                Utils::ext_sym_set::iterator itk = killed.begin( );
                for( ; itk != killed.end( ); ++itk )
                {
                    if( Utils::ext_sym_set_contains_englobing_nodecl(itk->get_nodecl( ), aux_set) )
                    {   // Delete from 'var' the englobed part of (*itk) and put the result in 'var'
                    // TODO
                    std::cerr << "warning: Part of nodecl " << itk->get_nodecl( ).prettyprint( ) << " founded in the current var "
                              << var.prettyprint( ) << " must be avoided. A subpart is killed." << std::endl;
                    //                             var = nodecl_subtract(var, ita->get_nodecl( ) );
                    killed.erase( itk );
                    if( compute_undef == '1' )
                        new_l = insert_var_in_list( var, new_l );
                    else
                        undef.insert( var );
                    break;
                    }
                }

                if(!Utils::ext_sym_set_contains_englobing_nodecl(var, undef) )
                {   // No englobing variable in the avoiding list 2
                    // Look for variables in avoiding list 2 englobed by 'var'
                    Utils::ext_sym_set aux_set; aux_set.insert(*it);
                    Utils::ext_sym_set::iterator itu = undef.begin( );
                    for (; itu != undef.end( ); ++itu)
                    {
                        if(Utils::ext_sym_set_contains_englobing_nodecl(itu->get_nodecl( ), aux_set) )
                        {   // Delete from var the englobed part of (*itu) and put the result in 'var'
                            // TODO
                            std::cerr << "warning: Part of nodecl " << itu->get_nodecl( ).prettyprint( ) << " founded in the current var "
                            << var.prettyprint( ) << " must be avoided. A subpart is undefined." << std::endl;
                            undef.erase(itu);
                            if(compute_undef == '1')
                                new_l = insert_var_in_list(var, new_l);
                            else
                                undef.insert(var);
                            break;
                        }
                    }
                    if(itk == killed.end( ) && itu == undef.end( ) )
                    {
                        new_l = insert_var_in_list(var, new_l);
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
            Utils::ext_sym_set null_list;
            undef_vars = compute_use_def_with_children( undef_children, undef_vars,
                                                        killed_vars, undef_vars, /*compute_undef*/ '1' );
            ue_vars = compute_use_def_with_children( ue_children, ue_vars,
                                                     killed_vars, undef_vars, /*compute_undef*/ '0' );
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

                Node* entry_node = current->get_graph_entry_node( );

                ObjectList<Utils::ext_sym_set> use_def = get_use_def_over_nodes( entry_node );
                current->set_ue_var( use_def[0] );
                current->set_killed_var( use_def[1] );
                current->set_undefined_behaviour_var( use_def[2] );
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

    static Utils::nodecl_set get_arguments_list( sym_to_nodecl_map ref_params_to_args )
    {
        Utils::nodecl_set result;

        for( sym_to_nodecl_map::iterator it = ref_params_to_args.begin( );
             it != ref_params_to_args.end( ); ++it )
        {
            result.insert( it->second );
        }

        return result;
    }

    static sym_to_nodecl_map map_reference_params_to_args( ObjectList<TL::Symbol> parameters,
                                                           Nodecl::List arguments )
    {
        sym_to_nodecl_map ref_params_to_args;

        ObjectList<TL::Symbol>::iterator itp = parameters.begin( );
        Nodecl::List::iterator ita = arguments.begin( );
        for( ; itp != parameters.end( ); ++itp, ++ita )
        {
            Type param_type = itp->get_type( );
            if( ( param_type.is_any_reference( ) || param_type.is_pointer( ) ) )
            {
                ref_params_to_args[*itp] = *ita;
            }
        }

        return ref_params_to_args;
    }

    static sym_to_nodecl_map map_lvalue_non_reference_params_to_args( ObjectList<TL::Symbol> parameters,
                                                                      Nodecl::List arguments )
    {
        sym_to_nodecl_map non_ref_params_to_args;

        ObjectList<TL::Symbol>::iterator itp = parameters.begin( );
        Nodecl::List::iterator ita = arguments.begin( );
        for( ; itp != parameters.end( ); ++itp, ++ita )
        {
            Type param_type = itp->get_type( );
            if( !param_type.is_any_reference( ) && !param_type.is_pointer( ) &&
                Nodecl::Utils::nodecl_is_modifiable_lvalue( *ita )  )
            {
                non_ref_params_to_args[*itp] = *ita;
            }
        }

        return non_ref_params_to_args;
    }

    UsageVisitor::UsageVisitor( Node* n,
                                ObjectList<Symbol> visited_functions,
                                ObjectList<Utils::ExtendedSymbolUsage> visited_global_vars,
                                bool ipa, Scope sc, Utils::nodecl_set ipa_arguments )
        : _node( n ), _define( false ), _actual_nodecl( Nodecl::NodeclBase::null( ) ),
          _visited_functions( visited_functions ), _visited_global_vars( visited_global_vars ),
          _ipa( ipa ), _sc( sc ), _ipa_arguments( ipa_arguments )
    { }

    UsageVisitor::UsageVisitor( const UsageVisitor& v )
    {
        _node = v._node;
        _define = v._define;
        _actual_nodecl = v._actual_nodecl;
        _visited_functions = v._visited_functions;
        _visited_global_vars = v._visited_global_vars;
        _ipa = v._ipa;
        _sc = v._sc;
        _ipa_arguments = v._ipa_arguments;
    }

    ObjectList<Symbol> UsageVisitor::get_visited_functions( ) const
    {
        return _visited_functions;
    }

    ObjectList<Utils::ExtendedSymbolUsage> UsageVisitor::get_visited_global_variables( ) const
    {
        return _visited_global_vars;
    }

    bool UsageVisitor::variable_is_in_context( Nodecl::NodeclBase var )
    {
        // When IPA, only global variables and referenced parameters are in context
        // Otherwise, any variable is in context
        if ( ( _ipa && ( !var.retrieve_context( ).scope_is_enclosed_by( _sc )
                         || ( _ipa_arguments.find( var ) != _ipa_arguments.end( ) ) ) )
               || !_ipa )
        {
             return true;
        }
        return false;
    }

    void UsageVisitor::compute_statement_usage( Nodecl::NodeclBase st )
    {
        walk( st );

        // Propagate Use-Def info from inner nodes to outer node
        if( _node->is_split_statement( ) )
        {
            Node* inner_graph = _node->get_graph_entry_node( )->get_children( )[0];
            Utils::ext_sym_set inner_ue = inner_graph->get_ue_vars( );
            Utils::ext_sym_set inner_killed = inner_graph->get_killed_vars( );
            Utils::ext_sym_set inner_undef = inner_graph->get_undefined_behaviour_vars( );

            _node->set_ue_var(
                    Utils::ext_sym_set_difference(
                            Utils::ext_sym_set_difference(
                                    Utils::ext_sym_set_union( _node->get_ue_vars( ),
                                                              inner_ue ),
                                    inner_killed ),
                            inner_undef ) );
            _node->set_killed_var(
                    Utils::ext_sym_set_difference(
                            Utils::ext_sym_set_union( _node->get_killed_vars(),
                                                      inner_killed ),
                            inner_undef ) );
            _node->set_undefined_behaviour_var(
                    Utils::ext_sym_set_union( _node->get_undefined_behaviour_vars( ),
                                              inner_undef ) );
        }
    }

    UsageVisitor::Ret UsageVisitor::unhandled_node( const Nodecl::NodeclBase& n )
    {
        nodecl_t internal_n = n.get_internal_nodecl( );
        WARNING_MESSAGE( "Unhandled node '%s' with type '%s 'during Use-Def Analysis'",
                         codegen_to_str( internal_n, nodecl_retrieve_context( internal_n ) ),
                         ast_print_node_type( n.get_kind( ) ) );
    }

    UsageVisitor::Ret UsageVisitor::binary_assignment_visit( Nodecl::NodeclBase lhs, Nodecl::NodeclBase rhs )
    {
        // Traverse the use of both the lhs and the rhs
        walk( rhs );
        walk( lhs );

        // Traverse the definition of the lhs
        _define = true;
        walk( lhs );
        _define = false;
    }

    void UsageVisitor::function_visit( Nodecl::NodeclBase called_sym, Nodecl::NodeclBase arguments )
    {
        TL::Symbol func_sym = called_sym.get_symbol( );

        // The function called must be analyzed only in case it has not been analyzed previously
        if( !_visited_functions.contains( func_sym ) )
        {
            _visited_functions.append( func_sym );
            Nodecl::FunctionCode called_func =
                func_sym.get_function_code( ).as<Nodecl::FunctionCode>( );

            ObjectList<TL::Symbol> params = func_sym.get_function_parameters( );
            Nodecl::List args = arguments.as<Nodecl::List>( );
            if( !called_func.is_null( ) )
            {
                Nodecl::FunctionCode copied_func =
                        called_func.shallow_copy( ).as<Nodecl::FunctionCode>( );

                // Create renaming map
                sym_to_nodecl_map renaming_map =
                        map_reference_params_to_args( params, args );

                // Rename the parameters with the arguments
                Nodecl::NodeclBase stmts = copied_func.get_statements( );
                RenameVisitor rv( renaming_map );
                rv.rename_expressions( stmts );

                // Create the PCFG for the renamed code
                PCFGVisitor pcfgv( Utils::generate_hashed_name( copied_func ), called_func );
                ExtensibleGraph* pcfg = pcfgv.parallel_control_flow_graph( copied_func );

                _visited_global_vars.insert( pcfg->get_global_variables( ) );

                // Compute the Use-Def variables of the code
                UseDef ue( pcfg );
                ue.compute_usage( _visited_functions, _visited_global_vars,
                                  /* ipa */ true, get_arguments_list( renaming_map ) );

                // Set the node usage
                Node* pcfg_node = pcfg->get_graph( );
                    // reference parameters and global variables
                Utils::ext_sym_set ue_vars = pcfg_node->get_ue_vars( );
                Utils::ext_sym_set killed_vars = pcfg_node->get_killed_vars( );
                Utils::ext_sym_set undef_vars = pcfg_node->get_undefined_behaviour_vars( );
                    // value parameters
                sym_to_nodecl_map non_ref_params = map_lvalue_non_reference_params_to_args( params, args );
                for( sym_to_nodecl_map::iterator it = non_ref_params.begin( );
                    it != non_ref_params.end( ); ++it )
                {
                    ue_vars.insert( Utils::ExtendedSymbol( it->second ) );
                }
                    // set the values
                _node->set_ue_var( ue_vars );
                _node->set_killed_var( killed_vars );
                _node->set_undefined_behaviour_var( undef_vars );

                // Propagate the function call Use-Def info to outer nodes
                // in case the call is inside a bigger instruction.
                // F.i.:   int b = foo(a, b)
                //         PCFG:
                //           ______________________________________________
                //          |  [SPIT_STMT]                                 |
                //          |  __________________________________________  |
                //          | | [FUNC_CALL]                              | |
                //          | |  _______       ___________       ______  | |
                //          | | |       |     |           |     |      | | |
                //          | | | ENTRY |---->| foo(a, b) |---->| EXIT | | |
                //          | | |_______|     |___________|     |______| | |
                //          | |__________________________________________| |
                //          |               _______|_______                |
                //          |              |               |               |
                //          |              | b = foo(a, b) |               |
                //          |              |_______________|               |
                //          |______________________________________________|
                //
                //         When computing Use-Def of "foo(a, b)", we can easily propagate
                //             the info to the node "b=foo(a, b)".
                //             We will propagate, the function Use-Def to the outer nodes
                //             of a FUNC_CALL node until we find a non-SPLIT_STMT node.
                Node* func_node = _node->get_outer_node();
                ERROR_CONDITION( func_node->is_function_call_node( ),
                                 "Outer node of a statement containing only a function call "\
                                 "must be of type FUNC_CALL. Instead, outer node of node %d, "\
                                 "with cal to function %s, has type %s\n", _node->get_id( ),
                                 called_sym.get_symbol( ).get_name( ).c_str( ),
                                 func_node->get_graph_type_as_string( ).c_str( ) );
                Node* outer_node = func_node->get_outer_node( );
                while( outer_node->is_split_statement( ) )
                {
                    ERROR_CONDITION( outer_node->get_graph_exit_node( )->get_parents().size( ) != 1,
                                     "Expecting only one node as parent of the Exit Node in "\
                                     "the SPLIT_STMT node %d", outer_node->get_id( ) );
                    Node* split_stmt_node = outer_node->get_graph_exit_node( )->get_parents()[0];
                    split_stmt_node->set_ue_var( ue_vars );
                    split_stmt_node->set_killed_var( killed_vars );
                    split_stmt_node->set_undefined_behaviour_var( undef_vars );

                    outer_node = outer_node->get_outer_node( );
                }
            }
            else
            {   // We do not have access to the called code
                // Set all reference parameters to undefined
                sym_to_nodecl_map ref_params = map_reference_params_to_args( params, args );
                for( sym_to_nodecl_map::iterator it = ref_params.begin( );
                     it != ref_params.end( ); ++it )
                {
                    if( Nodecl::Utils::nodecl_is_modifiable_lvalue( it->second ) )
                    {
                        _node->set_undefined_behaviour_var_and_recompute_use_and_killed_sets(
                                Utils::ExtendedSymbol( it->second ) );
                    }
                }

                // Set the value passed parameters as Used
                sym_to_nodecl_map non_ref_params = map_lvalue_non_reference_params_to_args( params, args );
                for( sym_to_nodecl_map::iterator it = non_ref_params.begin( );
                     it != non_ref_params.end( ); ++it )
                {
                    _node->set_ue_var( Utils::ExtendedSymbol( it->second ) );
                }

                // Set all global variables to undefined
                for( ObjectList<Utils::ExtendedSymbolUsage>::iterator it =
                     _visited_global_vars.begin( ); it != _visited_global_vars.end( ); ++it )
                {
                    _node->set_undefined_behaviour_var_and_recompute_use_and_killed_sets(
                            it->get_extended_symbol() );
                }
            }
        }
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::ArithmeticShrAssignment& n )
    {
        binary_assignment_visit( n.get_lhs( ), n.get_rhs( ) );
    }

    UsageVisitor::Ret UsageVisitor::visit_pre( const Nodecl::ArraySubscript& n )
    {
        if(_actual_nodecl.is_null( ) )
            _actual_nodecl = n;
    }
    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::ArraySubscript& n )
    {
        walk( n.get_subscripted( ) );
        _define = false;        // We may come form a LHS walk and subscripts not defined!
        walk( n.get_subscripts( ) );
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::Assignment& n )
    {
        Nodecl::NodeclBase assig = n;
        walk( n.get_rhs( ) );
        _define = true;
        walk( n.get_lhs( ) );
        _define = false;
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::BitwiseAndAssignment& n )
    {
        binary_assignment_visit( n.get_lhs( ), n.get_rhs( ) );
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::BitwiseOrAssignment& n )
    {
        binary_assignment_visit( n.get_lhs( ), n.get_rhs( ) );
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::BitwiseShlAssignment& n )
    {
        binary_assignment_visit( n.get_lhs( ), n.get_rhs( ) );
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::BitwiseShrAssignment& n )
    {
        binary_assignment_visit( n.get_lhs( ), n.get_rhs( ) );
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::BitwiseXorAssignment& n )
    {
        binary_assignment_visit( n.get_lhs( ), n.get_rhs( ) );
    }

    UsageVisitor::Ret UsageVisitor::visit_pre( const Nodecl::ClassMemberAccess& n )
    {
        if( _actual_nodecl.is_null( ) )
            _actual_nodecl = n;
    }
    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::ClassMemberAccess& n )
    {
        // walk( n.get_lhs( ) );  // In a member access, the use/definition is always of the member, not the base
        walk( n.get_member( ) );
    }

    UsageVisitor::Ret UsageVisitor::visit_pre( const Nodecl::Dereference& n )
    {
        if( _actual_nodecl.is_null( ) )
            _actual_nodecl = n;
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::Dereference& n )
    {
        walk( n.get_rhs( ) );
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::DivAssignment& n )
    {
        binary_assignment_visit( n.get_lhs( ), n.get_rhs( ) );
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::FunctionCall& n )
    {
        function_visit( n.get_called( ), n.get_arguments( ) );
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::MinusAssignment& n )
    {
        binary_assignment_visit( n.get_lhs( ), n.get_rhs( ) );
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::ModAssignment& n )
    {
        binary_assignment_visit( n.get_lhs( ), n.get_rhs( ) );
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::MulAssignment& n )
    {
        binary_assignment_visit( n.get_lhs( ), n.get_rhs( ) );
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::ObjectInit& n )
    {
        Nodecl::Symbol s = Nodecl::Symbol::make( n.get_symbol( ), n.get_filename( ), n.get_line( ) );
        _node->set_killed_var( Utils::ExtendedSymbol( s ) );
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::PointerToMember& n )
    {
        internal_error( "PointerToMemeber not yet implemented in UsageVisitor.", 0 );
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::Postdecrement& n )
    {
        Nodecl::NodeclBase rhs = n.get_rhs( );
        walk( rhs );
        _define = true;
        walk( rhs );
        _define = false;
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::Postincrement& n )
    {
        Nodecl::NodeclBase rhs = n.get_rhs( );
        walk( rhs );
        _define = true;
        walk( rhs );
        _define = false;
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::Predecrement& n )
    {
        Nodecl::NodeclBase rhs = n.get_rhs( );
        walk( rhs );
        _define = true;
        walk( rhs );
        _define = false;
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::Preincrement& n )
    {
        Nodecl::NodeclBase rhs = n.get_rhs( );
        walk( rhs );
        _define = true;
        walk( rhs );
        _define = false;
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::Range& n )
    {
        walk( n.get_lower() );
        walk( n.get_upper() );
        walk( n.get_stride() );
    }

    UsageVisitor::Ret UsageVisitor::visit_pre( const Nodecl::Reference& n )
    {
        if( _actual_nodecl.is_null( ) )
            _actual_nodecl = n;
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::Reference& n )
    {
        walk( n.get_rhs( ) );
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::Symbol& n )
    {
        Nodecl::NodeclBase defined_var = n;
        if( !_actual_nodecl.is_null( ) )
        {
            defined_var = _actual_nodecl;
            _actual_nodecl = Nodecl::NodeclBase::null( );
        }

        if( variable_is_in_context( defined_var ) )
        {
            if( _define )
                _node->set_killed_var( Utils::ExtendedSymbol( defined_var ) );
            else
            {
                if( !Utils::ext_sym_set_contains_nodecl( defined_var, _node->get_killed_vars() ) )
                    _node->set_ue_var( Utils::ExtendedSymbol( defined_var ) );
            }
        }
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::VirtualFunctionCall& n )
    {
        function_visit( n.get_called( ), n.get_arguments( ) );
    }

    // *************************** End class implementing use-definition visitor ************************** //
    // **************************************************************************************************** //

}
}