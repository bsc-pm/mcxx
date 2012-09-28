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


#include "tl-extended-symbol.hpp"
#include "tl-use-def.hpp"

namespace TL {
namespace Analysis {

    // **************************************************************************************************** //
    // **************************** Class implementing use-definition analysis **************************** //

    UseDef::UseDef( ExtensibleGraph* graph )
        : _graph( graph )
    {}

    void UseDef::compute_usage_rec( Node* current )
    {
        if( !current->is_visited( ) )
        {
            current->set_visited( true );

            if( current->is_exit_node( ) )
                return;


            if( current->is_graph_node( ) )
            {
                // Use-def info is computed from inner nodes to outer nodes
                compute_usage_rec( current->get_graph_entry_node( ) );

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
                    UsageVisitor uv( current );
                    uv.walk( *it );
                }
            }

            // Compute usage form children
            ObjectList<Node*> children = current->get_children( );
            for( ObjectList<Node*>::iterator it = children.begin( ); it != children.end( ); ++it )
            {
                compute_usage_rec( *it );
            }
        }
    }

    void UseDef::compute_usage( )
    {
        Node* graph = _graph->get_graph( );
        compute_usage_rec( graph );
        ExtensibleGraph::clear_visits( graph );
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
                    ue_children = sets_union( ue_children, use_def_aux[0] );
                    killed_children = sets_union( killed_children, use_def_aux[1] );
                    undef_children = sets_union( undef_children, use_def_aux[2] );
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
            internal_error( "Can't propagate use-def info from inner nodes to outer nodes in node '%d' with type '%s'.\n",
                            current->get_id( ), current->get_type_as_string( ).c_str( ) );
        }
    }

    // ************************** End class implementing use-definition analysis ************************** //
    // **************************************************************************************************** //



    // **************************************************************************************************** //
    // ***************************** Class implementing use-definition visitor **************************** //

    static ObjectList<Symbol> get_symbols(Nodecl::NodeclBase n)
    {
        if( n.get_symbol( ).is_valid( ) )
        {
            return ObjectList<Symbol>(1, n.get_symbol( ) );
        }

        ObjectList<Symbol> result;
        ObjectList<Nodecl::NodeclBase> children = n.children( );
        for(ObjectList<Nodecl::NodeclBase>::iterator it = children.begin( ); it != children.end( ); ++it)
        {
            result.append(get_symbols(*it) );
        }

        return result;
    }

    static void get_use_def_variables(Node* actual, int id_target_node,
                                      Utils::ext_sym_set &ue_vars, Utils::ext_sym_set &killed_vars, Utils::ext_sym_set &undef_vars)
    {
        ObjectList<Node*> children = actual->get_children( );
        for( ObjectList<Node*>::iterator it = children.begin( ); it != children.end( ); ++it )
        {
            if( ( *it )->get_id( ) != id_target_node )
            {
                ue_vars = sets_union( ue_vars, ( *it )->get_ue_vars( ) );
                killed_vars = sets_union( killed_vars, ( *it )->get_killed_vars( ) );
                undef_vars = sets_union( undef_vars, ( *it )->get_undefined_behaviour_vars( ) );

                get_use_def_variables( *it, id_target_node, ue_vars, killed_vars, undef_vars );
            }
        }
    }

    UsageVisitor::UsageVisitor( Node* n )
            : _node( n ), _define( false ),
              _actual_nodecl( Nodecl::NodeclBase::null( ) )
    {}

    UsageVisitor::UsageVisitor( const UsageVisitor& v )
    {
        _node = v._node;
        _define = v._define;
        _actual_nodecl = v._actual_nodecl;
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

    UsageVisitor::Ret UsageVisitor::binary_visit( Nodecl::NodeclBase lhs, Nodecl::NodeclBase rhs )
    {
        walk( lhs );
        walk( rhs );
    }

    void UsageVisitor::function_visit( Nodecl::NodeclBase called_func  )
    {
        // FIXME We must implement IPA here!

        Node* outer_node = _node->get_outer_node( );
        Utils::ext_sym_set ue_vars, killed_vars, undef_vars;
        get_use_def_variables( outer_node->get_graph_entry_node( ), _node->get_id( ),
                               ue_vars, killed_vars, undef_vars );
        _node->set_ue_var( ue_vars );
        _node->set_killed_var( killed_vars );
        _node->set_undefined_behaviour_var( undef_vars );
    }

    UsageVisitor::Ret UsageVisitor::unary_visit( Nodecl::NodeclBase rhs )
    {
        walk( rhs );
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::Add& n )
    {
        binary_visit( n.get_lhs( ), n.get_rhs( ) );
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::AddAssignment& n )
    {
        binary_assignment_visit( n.get_lhs( ), n.get_rhs( ) );
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::ArithmeticShr& n )
    {
        binary_visit( n.get_lhs( ), n.get_rhs( ) );
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::ArithmeticShrAssignment& n )
    {
        binary_assignment_visit( n.get_lhs( ), n.get_rhs( ) );
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::ArraySubscript& n )
    {
        if(_actual_nodecl.is_null( ) )
        {
            _actual_nodecl = n;
        }

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

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::BitwiseAnd& n )
    {
        binary_visit( n.get_lhs( ), n.get_rhs( ) );
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::BitwiseAndAssignment& n )
    {
        binary_assignment_visit( n.get_lhs( ), n.get_rhs( ) );
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::BitwiseOr& n )
    {
        binary_visit( n.get_lhs( ), n.get_rhs( ) );
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::BitwiseOrAssignment& n )
    {
        binary_assignment_visit( n.get_lhs( ), n.get_rhs( ) );
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::BitwiseShl& n )
    {
        binary_visit( n.get_lhs( ), n.get_rhs( ) );
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::BitwiseShlAssignment& n )
    {
        binary_assignment_visit( n.get_lhs( ), n.get_rhs( ) );
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::BitwiseShr& n )
    {
        binary_visit( n.get_lhs( ), n.get_rhs( ) );
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::BitwiseShrAssignment& n )
    {
        binary_assignment_visit( n.get_lhs( ), n.get_rhs( ) );
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::BitwiseXor& n )
    {
        binary_visit( n.get_lhs( ), n.get_rhs( ) );
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::BitwiseXorAssignment& n )
    {
        binary_assignment_visit( n.get_lhs( ), n.get_rhs( ) );
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::BooleanLiteral& n )
    {   // Nothing to be done
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::Cast& n )
    {
        walk( n.get_rhs( ) );
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::ClassMemberAccess& n )
    {
        if( _actual_nodecl.is_null( ) )
        {
            _actual_nodecl = n;
        }

        // walk( n.get_lhs( ) );  // In a member access, the use/definition is always of the member, not the base
        walk( n.get_member( ) );
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::Comma& n )
    {
        binary_visit( n.get_lhs( ), n.get_rhs( ) );
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::ComplexLiteral& n )
    {   // Nothing to be done
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::Concat& n )
    {
        binary_visit( n.get_lhs( ), n.get_rhs( ) );
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::Dereference& n )
    {
        if( _actual_nodecl.is_null( ) )
        {
            _actual_nodecl = n;
        }

        unary_visit( n.get_rhs(  ) );
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::Different& n )
    {
        binary_visit( n.get_lhs( ), n.get_rhs( ) );
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::Div& n )
    {
        binary_visit( n.get_lhs( ), n.get_rhs( ) );
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::DivAssignment& n )
    {
        binary_assignment_visit( n.get_lhs( ), n.get_rhs( ) );
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::EmptyStatement& n )
    {   // Nothing to be done
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::Equal& n )
    {
        binary_visit( n.get_lhs( ), n.get_rhs( ) );
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::FloatingLiteral& n )
    {   // Nothing to be done
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::FunctionCall& n )
    {
        function_visit( n.get_called( ) );
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::GreaterOrEqualThan& n )
    {
        binary_visit( n.get_lhs( ), n.get_rhs( ) );
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::GreaterThan& n )
    {
        binary_visit( n.get_lhs( ), n.get_rhs( ) );
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::IntegerLiteral& n )
    {   // Nothing to be done
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::LogicalAnd& n )
    {
        binary_visit( n.get_lhs( ), n.get_rhs( ) );
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::LogicalNot& n )
    {
        unary_visit( n.get_rhs(  ) );
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::LogicalOr& n )
    {
        binary_visit( n.get_lhs( ), n.get_rhs( ) );
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::LowerOrEqualThan& n )
    {
        binary_visit( n.get_lhs( ), n.get_rhs( ) );
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::LowerThan& n )
    {
        binary_visit( n.get_lhs( ), n.get_rhs( ) );
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::Minus& n )
    {
        binary_visit( n.get_lhs( ), n.get_rhs( ) );
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::MinusAssignment& n )
    {
        binary_assignment_visit( n.get_lhs( ), n.get_rhs( ) );
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::Mod& n )
    {
        binary_visit( n.get_lhs( ), n.get_rhs( ) );
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::ModAssignment& n )
    {
        binary_assignment_visit( n.get_lhs( ), n.get_rhs( ) );
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::Mul& n )
    {
        binary_visit( n.get_lhs( ), n.get_rhs( ) );
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::MulAssignment& n )
    {
        binary_assignment_visit( n.get_lhs( ), n.get_rhs( ) );
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::Neg& n )
    {
        unary_visit( n.get_rhs(  ) );
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::ObjectInit& n )
    {
        Nodecl::Symbol s = Nodecl::Symbol::make( n.get_symbol( ), n.get_filename( ), n.get_line( ) );
        _node->set_killed_var( Utils::ExtendedSymbol( s ) );
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::Offset& n )
    {   // Nothing to be done
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::Offsetof& n )
    {   // Nothing to be done
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::Plus& n )
    {
        unary_visit( n.get_rhs(  ) );
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::PointerToMember& n )
    {
        internal_error( "PointerToMemeber not yet implemented in PointerToMember.", 0 );
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

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::Power& n )
    {
        binary_visit( n.get_lhs( ), n.get_rhs( ) );
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
        internal_error( "PointerToMemeber not yet implemented in Range.", 0 );
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::Reference& n )
    {
        if( _actual_nodecl.is_null( ) )
        {
            _actual_nodecl = n;
        }

        unary_visit( n.get_rhs(  ) );
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::ReturnStatement& n )
    {   // Nothing to be done
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::Sizeof& n )
    {
        internal_error( "Sizeof not yet implemented in Range.", 0 );
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::StringLiteral& n )
    {   // Nothing to be done
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::Symbol& n )
    {
        Nodecl::NodeclBase defined_var = n;
        if( !_actual_nodecl.is_null( ) )
        {
            defined_var = _actual_nodecl;
            _actual_nodecl = Nodecl::NodeclBase::null( );
        }

        if( _define )
            _node->set_killed_var( Utils::ExtendedSymbol( defined_var ) );
        else
            _node->set_ue_var( Utils::ExtendedSymbol( defined_var ) );
    }

    UsageVisitor::Ret UsageVisitor::visit( const Nodecl::VirtualFunctionCall& n )
    {
        function_visit( n.get_called( ) );
    }

    // *************************** End class implementing use-definition visitor ************************** //
    // **************************************************************************************************** //

}
}