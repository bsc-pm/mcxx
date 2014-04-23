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

#include "tl-expression-evolution-visitor.hpp"

#include "cxx-cexpr.h"
//#include "tl-analysis-utils.hpp"
//#include "tl-analysis-static-info.hpp"
//#include "tl-expression-reduction.hpp"
//#include <algorithm>

namespace TL
{
namespace Analysis
{
    ExpressionEvolutionVisitor::ExpressionEvolutionVisitor( Node* scope, Node* n_node, ExtensibleGraph* pcfg )
            : _induction_variables( scope->get_induction_variables() ), _killed( scope->get_killed_vars() ),
              _pcfg( pcfg ), _scope_node( scope ), _n_node( n_node ),
              _ivs( ), _is_adjacent_access( false ), _has_constant_evolution( false )
    {}

    bool ExpressionEvolutionVisitor::variable_is_iv( const Nodecl::NodeclBase& n )
    {
        bool is_iv = false;
        for( ObjectList<Utils::InductionVariableData*>::const_iterator it = _induction_variables.begin( );
             it != _induction_variables.end( ); ++it )
        {
            if( Nodecl::Utils::structurally_equal_nodecls( ( *it )->get_variable( ).get_nodecl( ), n, /* skip conversion nodes */ true ) )
            {

                _ivs.insert( *it );
                is_iv = true;
                break;
            }
        }

        return is_iv;
    }

    bool ExpressionEvolutionVisitor::node_uses_iv( Node* node )
    {
        bool result = false;
        for( ObjectList<Utils::InductionVariableData*>::const_iterator it = _induction_variables.begin( );
             it != _induction_variables.end( ) && !result; ++it )
        {
            if( node->uses_var( ( *it )->get_variable( ).get_nodecl( ) ) )
                result = true;
        }
        return result;
    }

    bool ExpressionEvolutionVisitor::node_stmts_depend_on_iv( Node* node, int recursion_level,
                                                          std::map<Node*, std::set<int> >& visits,
                                                          std::set<Nodecl::Symbol, Nodecl::Utils::Nodecl_structural_less>& visited_syms )
    {
        bool result = false;
        ObjectList<Nodecl::NodeclBase> stmts = node->get_statements( );
        for( ObjectList<Nodecl::NodeclBase>::iterator it = stmts.begin( ); ( it != stmts.end( ) ) && !result; ++it )
        {
            ObjectList<Nodecl::Symbol> syms = Nodecl::Utils::get_all_symbols_first_occurrence( *it );
            for( ObjectList<Nodecl::Symbol>::iterator its = syms.begin( ); ( its != syms.end( ) ) && !result; ++its )
            {
                if( visited_syms.find( *its ) == visited_syms.end( ) )
                    result = result || var_is_iv_dependent_in_scope_forward( *its, node,
                                recursion_level+1, visits, visited_syms );
            }
        }
        return result;
    }

    bool ExpressionEvolutionVisitor::definition_depends_on_iv( const Nodecl::NodeclBase& n, Node* node )
    {
        bool result = false;
        for( ObjectList<Utils::InductionVariableData*>::const_iterator it = _induction_variables.begin( );
             it != _induction_variables.end( ) && !result; ++it )
        {   // Check whether the expression used to modify it depends on an induction variable
            result = Nodecl::Utils::stmtexpr_contains_nodecl_structurally( n, ( *it )->get_variable( ).get_nodecl( ) );
        }
        if( !result )
        {
            Utils::ext_sym_map reaching_defs_in = node->get_reaching_definitions_in( );
            for( Utils::ext_sym_map::iterator it = reaching_defs_in.begin( ); it != reaching_defs_in.end( ) && !result; ++it )
            {
                if( Nodecl::Utils::stmtexpr_contains_nodecl_structurally( it->first.get_nodecl( ), n ) )
                {   // n has been defined previously
                    result = definition_depends_on_iv( it->second, node );
                }
            }
        }
        return result;
    }

    bool ExpressionEvolutionVisitor::var_is_iv_dependent_in_scope_backwards( const Nodecl::Symbol& n, Node* current,
            int recursion_level, std::map<Node*, std::set<int> >& visits,
            std::set<Nodecl::Symbol, Nodecl::Utils::Nodecl_structural_less>& visited_syms )
    {
        bool result = false;
        visited_syms.insert( n );
        if( current != _scope_node )
        {
            bool visit_node = false;
            // If the node has never been visited or, if it was visit, it was in a different recursion level
            if( visits.find( current ) == visits.end( ) )
            {
                int recursion_level_value[] = { recursion_level };
                visits[current] = std::set<int>( recursion_level_value, recursion_level_value + 1 );
                visit_node = true;
            }
            else if( visits.find( current )->second.find( recursion_level ) == visits.find( current )->second.end( ) )
            {
                visits.find( current )->second.insert( recursion_level );
                visit_node = true;
            }

            if( visit_node )
            {
                // Treat the current node
                Utils::ext_sym_set killed = current->get_killed_vars( );
                if( killed.find( n ) != killed.end( ) )
                {
                    if( current->is_graph_node( ) )
                    {   // The current graph node defined the symbol \n
                        // Treat the inner nodes of the current graph node
                        result = var_is_iv_dependent_in_scope_backwards( n, current->get_graph_exit_node( ),
                                                                   recursion_level, visits, visited_syms );
                        if( !result )
                        {
                            Node* current_entry = current->get_graph_entry_node( );
                            if( current->is_ifelse_statement( ) || current->is_switch_statement( ) || current->is_while_loop( ) )
                            {   // Case 1.1: This checks situations such as:
                                // if(i%2==0)       -> where 'i' is an induction variable in 'scope'
                                //     n=...;
                                // switch(i)        -> where 'i' is an induction variable in 'scope'
                                // {case 0:n=...;}
                                // while( i )       -> where 'i' is an induction variable in 'scope'
                                // {n=...;}
                                Node* cond = current_entry->get_children( )[0];
                                result = node_uses_iv( cond ) || node_stmts_depend_on_iv( cond, recursion_level, visits, visited_syms );
                            }
                            else if( current->is_for_loop( ) )
                            {   // Case 1.2:
                                // for(;...<i;)      -> where 'i' is an induction variable in 'scope'
                                //     n=...;
                                Node* cond = current_entry->get_children( )[0];
                                if( ( cond->get_children( ).size( ) == 2 ) && ( cond->get_parents( ).size( ) == 2 ) )
                                {   // Recheck whether this node is the condition of the loop, or the condition is empty
                                    result = node_uses_iv( cond ) || node_stmts_depend_on_iv( cond, recursion_level, visits, visited_syms );
                                }
                            }
                            else if( current->is_do_loop( ) )
                            {   // Case 1.2:
                                // do {n=...;}
                                // while(i)          -> where 'i' is an induction variable in 'scope'
                                Node* cond = current->get_graph_exit_node( )->get_parents( )[0];
                                result = node_uses_iv( cond ) || node_stmts_depend_on_iv( cond, recursion_level, visits, visited_syms );
                            }
                        }
                    }
                    else
                    {   // Case 2: This checks situations such as:
                        // n=i;                      -> where 'i' is an induction variable in 'scope'
                        Utils::ext_sym_map reaching_defs_out = current->get_reaching_definitions_out( );
                        for( Utils::ext_sym_map::iterator it = reaching_defs_out.begin( );
                             it != reaching_defs_out.end( ) && !result; ++it )
                        {
                            if( Nodecl::Utils::stmtexpr_contains_nodecl_structurally( it->first.get_nodecl( ), n ) )
                            {   // 'n' is being modified
                                result = definition_depends_on_iv( it->second, current );
                            }
                        }
                    }
                }

                // Recursively treat the parents of the current node
                if( !result )
                {
                    ObjectList<Node*> parents;
                    if( current->is_entry_node( ) )
                        parents.append( current->get_outer_node( ) );
                    else
                        parents = current->get_parents( );
                    for( ObjectList<Node*>::iterator it = parents.begin( ); it != parents.end( ) && !result; ++it )
                        result = var_is_iv_dependent_in_scope_backwards( n, *it, recursion_level, visits, visited_syms );
                }
            }
        }
        return result;
    }

    bool ExpressionEvolutionVisitor::var_is_iv_dependent_in_scope_forward( const Nodecl::Symbol& n, Node* current,
            int recursion_level, std::map<Node*, std::set<int> >& visits,
            std::set<Nodecl::Symbol, Nodecl::Utils::Nodecl_structural_less>& visited_syms )
    {
        bool result = false;
        visited_syms.insert( n );
        if( current != _scope_node->get_graph_exit_node() )
        {
            bool visit_node = false;
            // If the node has never been visited or, if it was visit, it was in a different recursion level
            if( visits.find( current ) == visits.end( ) )
            {
                int recursion_level_value[] = { recursion_level };
                visits[current] = std::set<int>( recursion_level_value, recursion_level_value + 1 );
                visit_node = true;
            }
            else if( visits.find( current )->second.find( recursion_level ) == visits.find( current )->second.end( ) )
            {
                visits.find( current )->second.insert( recursion_level );
                visit_node = true;
            }

            if( visit_node )
            {
                // Treat the current node
                Utils::ext_sym_set killed = current->get_killed_vars( );
                if( killed.find( n ) != killed.end( ) )
                {
                    if( current->is_graph_node( ) )
                    {   // The current graph node defined the symbol \n
                        // Treat the inner nodes of the current graph node
                        result = var_is_iv_dependent_in_scope_forward( n, current->get_graph_entry_node( ),
                                                                       recursion_level, visits, visited_syms );
                        if( !result )
                        {
                            Node* current_entry = current->get_graph_entry_node( );
                            if( current->is_ifelse_statement( ) || current->is_switch_statement( ) || current->is_while_loop( ) )
                            {   // Case 1.1: This checks situations such as:
                                // if(i%2==0)       -> where 'i' is an induction variable in 'scope'
                                //     n=...;
                                // switch(i)        -> where 'i' is an induction variable in 'scope'
                                // {case 0:n=...;}
                                // while( i )       -> where 'i' is an induction variable in 'scope'
                                // {n=...;}
                                Node* cond = current_entry->get_children( )[0];
                                result = node_uses_iv( cond ) || node_stmts_depend_on_iv( cond, recursion_level, visits, visited_syms );
                            }
                            else if( current->is_for_loop( ) )
                            {   // Case 1.2:
                                // for(;...<i;)      -> where 'i' is an induction variable in 'scope'
                                //     n=...;
                                Node* cond = current_entry->get_children( )[0];
                                if( ( cond->get_children( ).size( ) == 2 ) && ( cond->get_parents( ).size( ) == 2 ) )
                                {   // Recheck whether this node is the condition of the loop, or the condition is empty
                                    result = node_uses_iv( cond ) || node_stmts_depend_on_iv( cond, recursion_level, visits, visited_syms );
                                }
                            }
                            else if( current->is_do_loop( ) )
                            {   // Case 1.2:
                                // do {n=...;}
                                // while(i)          -> where 'i' is an induction variable in 'scope'
                                Node* cond = current->get_graph_exit_node( )->get_parents( )[0];
                                result = node_uses_iv( cond ) || node_stmts_depend_on_iv( cond, recursion_level, visits, visited_syms );
                            }
                        }
                    }
                    else
                    {   // Case 2: This checks situations such as:
                        // n=i;                      -> where 'i' is an induction variable in 'scope'
                        Utils::ext_sym_map reaching_defs_out = current->get_reaching_definitions_out( );
                        for( Utils::ext_sym_map::iterator it = reaching_defs_out.begin( );
                             it != reaching_defs_out.end( ) && !result; ++it )
                        {
                            if( Nodecl::Utils::stmtexpr_contains_nodecl_structurally( it->first.get_nodecl( ), n ) )
                            {   // 'n' is being modified
                                result = definition_depends_on_iv( it->second, current );
                            }
                        }
                    }
                }

                // Recursively treat the children of the current node
                if( !result )
                {
                    ObjectList<Node*> children = current->get_children();
                    for( ObjectList<Node*>::iterator it = children.begin( ); it != children.end( ) && !result; ++it )
                    {
                        result = var_is_iv_dependent_in_scope_forward( n, *it, recursion_level, visits, visited_syms );
                    }
                }
            }
        }
        return result;
    }

    // Check whether the definition of 'n' depends on the value of the '_scope' induction variable
    bool ExpressionEvolutionVisitor::var_is_iv_dependent_in_scope( const Nodecl::Symbol& n )
    {
        std::map<Node*, std::set<int> > visits;
        std::set<Nodecl::Symbol, Nodecl::Utils::Nodecl_structural_less> visited_syms;
        Node* init = ((_n_node==NULL) ? _scope_node->get_graph_entry_node() : _n_node);
        bool result = false;
        if(_n_node == NULL)
        {
            result = var_is_iv_dependent_in_scope_forward( n, init, 0, visits, visited_syms );
            ExtensibleGraph::clear_visits_in_level(init, _scope_node);
        }
        else
        {
            result = var_is_iv_dependent_in_scope_backwards( n, init, 0, visits, visited_syms );
            ExtensibleGraph::clear_visits_backwards_in_level( init, _scope_node );
        }
        return result;
    }

    static bool nodecl_is_zero( const Nodecl::NodeclBase& n )
    {
        bool res = false;

        if( n.is<Nodecl::IntegerLiteral>( ) )
            res = const_value_is_zero( n.as<Nodecl::IntegerLiteral>( ).get_constant( ) );
        else if( n.is<Nodecl::FloatingLiteral>( ) )
            res = const_value_is_zero( n.as<Nodecl::FloatingLiteral>( ).get_constant( ) );

        return res;
    }

    static bool nodecl_is_one( const Nodecl::NodeclBase& n )
    {
        bool res = false;

        if( n.is<Nodecl::IntegerLiteral>( ) )
            res = const_value_is_one( n.as<Nodecl::IntegerLiteral>( ).get_constant( ) );
        else if( n.is<Nodecl::FloatingLiteral>( ) )
            res = const_value_is_one( n.as<Nodecl::FloatingLiteral>( ).get_constant( ) );

        return res;
    }

    bool ExpressionEvolutionVisitor::has_constant_evolution( )
    {
        return _has_constant_evolution;
    }

    bool ExpressionEvolutionVisitor::is_adjacent_access( )
    {
        return _is_adjacent_access;
    }

    bool ExpressionEvolutionVisitor::depends_on_induction_vars( )
    {
        return !_ivs.empty( );
    }

    bool ExpressionEvolutionVisitor::unhandled_node( const Nodecl::NodeclBase& n )
    {
        WARNING_MESSAGE( "Unhandled node while parsing Nodecl '%s' of type '%s'",
                         n.prettyprint( ).c_str( ), ast_print_node_type( n.get_kind( ) ) );
        return false;
    }

    bool ExpressionEvolutionVisitor::join_list( ObjectList<bool>& list )
    {
        bool result = true;
        for( ObjectList<bool>::iterator it = list.begin( ); it != list.end( ); ++it )
        {
            result = result && ( *it );
        }
        return result;
    }

    bool ExpressionEvolutionVisitor::visit( const Nodecl::Add& n )
    {
        // Gather LHS info
        Nodecl::NodeclBase lhs = n.get_lhs( );
        bool lhs_is_const = walk( lhs );
        bool lhs_is_adjacent_access = _is_adjacent_access;
        bool lhs_has_constant_evolution = _has_constant_evolution;

        // Gather RHS info
        Nodecl::NodeclBase rhs = n.get_rhs( );
        bool rhs_is_const = walk( rhs );
        bool rhs_is_adjacent_access = _is_adjacent_access;
        bool rhs_has_constant_evolution = _has_constant_evolution;

        //std::cerr << "In " << n.prettyprint() << " lhs is constant " << lhs_is_const
        //    << " and rhs is constant " << rhs_is_const << std::endl;

        // Compute adjacency info
        _is_adjacent_access = ( lhs_is_adjacent_access && rhs_is_const )
                           || ( lhs_is_const && rhs_is_adjacent_access )
                           || ( lhs_is_adjacent_access && rhs_has_constant_evolution )
                           || ( lhs_has_constant_evolution && rhs_is_adjacent_access );

        _has_constant_evolution = lhs_has_constant_evolution && rhs_has_constant_evolution;

        return ( rhs_is_const && lhs_is_const );
    }

    bool ExpressionEvolutionVisitor::visit( const Nodecl::ArraySubscript& n )
    {
        // Collect information about the induction variables contained in the node
        bool n_is_iv = variable_is_iv( n );
        walk( n.get_subscripted( ) );
        walk( n.get_subscripts( ) );

        _is_adjacent_access = ( n_is_iv && _ivs.back( )->is_increment_one( ) );

        return !Utils::ext_sym_set_contains_nodecl( n, _killed );
    }

    bool ExpressionEvolutionVisitor::visit( const Nodecl::Assignment& n )
    {
        // Gather LHS info
        Nodecl::NodeclBase lhs = n.get_lhs( );
        bool lhs_is_const = walk( lhs );
        // lhs does not affect adjacency

        // Gather RHS info
        Nodecl::NodeclBase rhs = n.get_rhs( );
        bool rhs_is_const = walk( rhs );

        return ( rhs_is_const && lhs_is_const );
    }

    bool ExpressionEvolutionVisitor::visit( const Nodecl::BitwiseShl& n )
    {
        // Gather LHS info
        Nodecl::NodeclBase lhs = n.get_lhs( );
        bool lhs_is_const = walk( lhs );
        bool lhs_is_adjacent_access = _is_adjacent_access;
        bool lhs_has_constant_evolution = _has_constant_evolution;

        // Gather RHS info
        Nodecl::NodeclBase rhs = n.get_rhs( );
        bool rhs_is_const = walk( rhs );
        bool rhs_is_zero = false;
        if( rhs_is_const )
            rhs_is_zero = nodecl_is_zero( rhs );
        bool rhs_has_constant_evolution = _has_constant_evolution;

        // Compute adjacency info
        _is_adjacent_access = lhs_is_adjacent_access && rhs_is_zero;

        // Compute evolution info
        _has_constant_evolution = lhs_has_constant_evolution && rhs_has_constant_evolution;

        return ( lhs_is_const && rhs_is_const );
    }

    bool ExpressionEvolutionVisitor::visit( const Nodecl::BitwiseShr& n )
    {
        // Gather LHS info
        Nodecl::NodeclBase lhs = n.get_lhs( );
        bool lhs_is_const = walk( lhs );
        bool lhs_is_adjacent_access = _is_adjacent_access;
        bool lhs_has_constant_evolution = _has_constant_evolution;

        // Gather RHS info
        Nodecl::NodeclBase rhs = n.get_rhs( );
        bool rhs_is_const = walk( rhs );
        bool rhs_is_zero = false;
        if( rhs_is_const )
            rhs_is_zero = nodecl_is_zero( rhs );
        bool rhs_has_constant_evolution = _has_constant_evolution;

        // Compute adjacency info
        _is_adjacent_access = lhs_is_adjacent_access && rhs_is_zero;

        // Compute evolution info
        _has_constant_evolution = lhs_has_constant_evolution && rhs_has_constant_evolution;

        return ( lhs_is_const && rhs_is_const );
    }

    bool ExpressionEvolutionVisitor::visit( const Nodecl::BooleanLiteral& n )
    {
        _has_constant_evolution = true;
        return true;
    }

    bool ExpressionEvolutionVisitor::visit( const Nodecl::Cast& n )
    {
        return walk( n.get_rhs( ) );
    }

    bool ExpressionEvolutionVisitor::visit( const Nodecl::ComplexLiteral& n )
    {
        _has_constant_evolution = true;
        return true;
    }

    bool ExpressionEvolutionVisitor::visit( const Nodecl::Conversion& n )
    {
        return walk( n.get_nest( ) );
    }

    bool ExpressionEvolutionVisitor::visit( const Nodecl::Div& n )
    {
        // Gather LHS info
        Nodecl::NodeclBase lhs = n.get_lhs( );
        bool lhs_is_const = walk( lhs );
        bool lhs_is_adjacent_access = _is_adjacent_access;
        bool lhs_has_constant_evolution = _has_constant_evolution;

        // Gather RHS info
        Nodecl::NodeclBase rhs = n.get_rhs( );
        bool rhs_is_const = walk( rhs );
        bool rhs_is_one = false;
        if( rhs_is_const )
            rhs_is_one = nodecl_is_one( rhs );
        bool rhs_has_constant_evolution = _has_constant_evolution;

        // Compute adjacency info
        _is_adjacent_access = lhs_is_adjacent_access && rhs_is_one;

        // Compute evolution info
        _has_constant_evolution = lhs_has_constant_evolution && rhs_has_constant_evolution;

        return ( lhs_is_const && rhs_is_const );
    }

    bool ExpressionEvolutionVisitor::visit( const Nodecl::FloatingLiteral& n )
    {
        _has_constant_evolution = true;
        return true;
    }

    bool ExpressionEvolutionVisitor::visit( const Nodecl::FunctionCall& n )
    {
        // Traverse arguments to find induction variables
        walk( n.get_arguments( ) );

        _is_adjacent_access = false;      // Reset this value
        _has_constant_evolution = false;  // Reset this value

        return false; // Conservatively assume the result of the function call is not constant
    }

    bool ExpressionEvolutionVisitor::visit( const Nodecl::IntegerLiteral& n )
    {
        _has_constant_evolution = true;
        return true;
    }

    bool ExpressionEvolutionVisitor::visit( const Nodecl::LowerThan& n )
    {
        // Gather LHS info
        Nodecl::NodeclBase lhs = n.get_lhs( );
        bool lhs_is_const = walk( lhs );
        bool lhs_has_constant_evolution = _has_constant_evolution;

        // Gather RHS info
        Nodecl::NodeclBase rhs = n.get_rhs( );
        bool rhs_is_const = walk( rhs );
        bool rhs_has_constant_evolution = _has_constant_evolution;

        // Adjacency is not applicable
        _is_adjacent_access = false;

        // Compute evolution info
        _has_constant_evolution = lhs_has_constant_evolution && rhs_has_constant_evolution;

        return ( rhs_is_const && lhs_is_const );
    }

    bool ExpressionEvolutionVisitor::visit( const Nodecl::MaskLiteral& n )
    {
        return true;
    }

    bool ExpressionEvolutionVisitor::visit( const Nodecl::Minus& n )
    {
        // Gather LHS info
        Nodecl::NodeclBase lhs = n.get_lhs( );
        bool lhs_is_const = walk( lhs );
        bool lhs_is_adjacent_access = _is_adjacent_access;
        bool lhs_has_constant_evolution = _has_constant_evolution;

        // Gather RHS info
        Nodecl::NodeclBase rhs = n.get_rhs( );
        bool rhs_is_const = walk( rhs );
        bool rhs_is_adjacent_access = _is_adjacent_access;
        bool rhs_has_constant_evolution = _has_constant_evolution;

        // Compute adjacency info
        _is_adjacent_access = ( lhs_is_adjacent_access && rhs_is_const )
                           || ( lhs_is_const && rhs_is_adjacent_access );

        // Compute evolution info
        _has_constant_evolution = lhs_has_constant_evolution && rhs_has_constant_evolution;

        return ( rhs_is_const && lhs_is_const );
    }

    bool ExpressionEvolutionVisitor::visit( const Nodecl::Mul& n )
    {
        // Gather LHS info
        Nodecl::NodeclBase lhs = n.get_lhs( );
        bool lhs_is_const = walk( lhs );
        bool lhs_is_one = false;
        if( lhs_is_const )
            lhs_is_one = nodecl_is_one( lhs );
        bool lhs_is_adjacent_access = _is_adjacent_access;
        bool lhs_has_constant_evolution = _has_constant_evolution;

        // Gather RHS info
        Nodecl::NodeclBase rhs = n.get_rhs( );
        bool rhs_is_const = walk( rhs );
        bool rhs_is_one = false;
        if( rhs_is_const )
            rhs_is_one = nodecl_is_one( rhs );
        bool rhs_is_adjacent_access = _is_adjacent_access;
        bool rhs_has_constant_evolution = _has_constant_evolution;

        // Compute adjacency info
        _is_adjacent_access = ( lhs_is_adjacent_access && rhs_is_one )
                           || ( rhs_is_adjacent_access && lhs_is_one );

         // Compute evolution info
        _has_constant_evolution = lhs_has_constant_evolution && rhs_has_constant_evolution;

        return ( lhs_is_const && rhs_is_const );
    }

    bool ExpressionEvolutionVisitor::visit( const Nodecl::Neg& n )
    {
        return walk( n.get_rhs( ) );
    }

    bool ExpressionEvolutionVisitor::visit( const Nodecl::PointerToMember& n )
    {
        // Collect information about the induction variables contained in the node
        bool n_is_iv = variable_is_iv( n );

        _is_adjacent_access = ( n_is_iv && _ivs.back( )->is_increment_one( ) );

        // TODO: Compute evolution info
        //_has_constant_evolution = lhs_has_constant_evolution && rhs_has_constant_evolution;

        return !Utils::ext_sym_set_contains_nodecl( n, _killed );
    }

    bool ExpressionEvolutionVisitor::visit( const Nodecl::Postdecrement& n )
    {
        // Gather information about induction variables
        walk( n.get_rhs( ) );

        _is_adjacent_access = false;
        //TODO: return true? evolution true?
        _has_constant_evolution = false;

        return false;
    }

    bool ExpressionEvolutionVisitor::visit( const Nodecl::Postincrement& n )
    {
        // Gather information about induction variables
        walk( n.get_rhs( ) );

        _is_adjacent_access = false;
        //TODO: return true? evolution true?
        _has_constant_evolution = false;

        return false;
    }

    bool ExpressionEvolutionVisitor::visit( const Nodecl::Power& n )
    {
        // Gather LHS info
        Nodecl::NodeclBase lhs = n.get_lhs( );
        bool lhs_is_const = walk( lhs );
        bool lhs_is_adjacent_access = _is_adjacent_access;
        bool lhs_has_constant_evolution = _has_constant_evolution;

        // Gather RHS info
        Nodecl::NodeclBase rhs = n.get_rhs( );
        bool rhs_is_const = walk( rhs );
        bool rhs_is_one = false;
        if( rhs_is_const )
            rhs_is_one = nodecl_is_one( rhs );
        bool rhs_has_constant_evolution = _has_constant_evolution;

        // Compute adjacency info
        _is_adjacent_access = lhs_is_adjacent_access && rhs_is_const && rhs_is_one;

        // Compute evolution info
        _has_constant_evolution = lhs_has_constant_evolution && rhs_has_constant_evolution;

        return ( lhs_is_const && rhs_is_const );
    }

    bool ExpressionEvolutionVisitor::visit( const Nodecl::Predecrement& n )
    {
        walk( n.get_rhs( ) );

        _is_adjacent_access = false;
        //TODO: return true? evolution true?
        _has_constant_evolution = false;

        return false;
    }

    bool ExpressionEvolutionVisitor::visit( const Nodecl::Preincrement& n )
    {
        walk( n.get_rhs( ) );

        _is_adjacent_access = false;
        //TODO: return true? evolution true?
        _has_constant_evolution = false;

        return false;
    }

    bool ExpressionEvolutionVisitor::visit( const Nodecl::Reference& n )
    {
        return walk( n.get_rhs( ) );
    }

    bool ExpressionEvolutionVisitor::visit( const Nodecl::Sizeof& n )
    {
        bool n_is_const = walk( n.get_expr( ) );

        _is_adjacent_access = false;
        //TODO: evolution true?
        _has_constant_evolution = false;

        return n_is_const;
    }

    bool ExpressionEvolutionVisitor::visit( const Nodecl::StringLiteral& n )
    {
        return true;
    }

    bool ExpressionEvolutionVisitor::visit( const Nodecl::Symbol& n )
    {
        // Collect information about the induction variables contained in the node
        bool n_is_iv = variable_is_iv( n );

        //std::cerr << "Studying " << n.prettyprint() << std::endl;

        bool is_constant = false;
        bool reach_def_is_adjacent = false;
        bool reach_def_has_constant_evolution = false;

        if(!n_is_iv && (_n_node!=NULL))
        {
            Utils::ext_sym_map reach_def_in = _n_node->get_reaching_definitions_in( );
            //         Utils::ext_sym_map reach_def_out = _n_node->get_reaching_definitions_out( );
            // FIXME Compare the two maps.
            //       - If they are equal, continue.
            //       - Otherwise, if 'n' is modified within the node, after the access (original call to walk method), continue
            //       -            if 'n' is modified before the access, analyze the reaching definition
            Utils::ExtendedSymbol es(n);
            if(reach_def_in.find(es)!=reach_def_in.end())
            {
                std::pair<Utils::ext_sym_map::iterator, Utils::ext_sym_map::iterator> def_nodes = reach_def_in.equal_range(es);
                for(Utils::ext_sym_map::iterator it = def_nodes.first; it != def_nodes.second; ++it)
                {
                    Nodecl::NodeclBase current_def = it->second;
                    if(current_def.is<Nodecl::Undefined>())
                        continue;
                    Node* reach_def_node = _pcfg->find_nodecl_pointer(current_def);
                    ERROR_CONDITION(reach_def_node==NULL, "Nodecl corresponding to reaching definition %s not found\n", current_def.prettyprint().c_str() );
                    // FIXME This comparison is not enough because we can have cycles in the reaching definitions of the variables
                    // A solution might be to store the list of nodes we have visited. In that case, this list must be reseted each time we initiate a walk
                    if(reach_def_node!=_n_node)
                    {
                        ExpressionEvolutionVisitor eev(_scope_node, reach_def_node, _pcfg);
                        eev.walk(current_def);
                        reach_def_is_adjacent |= eev.is_adjacent_access();

                        // NEW Constant evolution
                        reach_def_has_constant_evolution |= eev.has_constant_evolution();

                        //std::cerr << current_def.prettyprint() << " has constant ev " << reach_def_has_constant_evolution << std::endl;
                    }
                }
            }

            // OLD Constant evolution
            /*
            Utils::InductionVariableData* iv = NodeclStaticInfo::get_nested_induction_variable(_scope_node, _n_node, n);
            if(iv!=NULL)
            {
                std::cerr << "is nested IV" << std::endl;

                _has_constant_evolution = !Utils::ext_sym_set_contains_nodecl( n, _killed );
                // check the lower boun of the induction variable
                Nodecl::NodeclBase lb = iv->get_lb();
                if(lb.is<Nodecl::Symbol>())
                {
                    _has_constant_evolution = _has_constant_evolution || !var_is_iv_dependent_in_scope(lb.as<Nodecl::Symbol>());
                }
                else if(lb.is_constant())
                {
                    _has_constant_evolution = true;
                }
                else
                {
                    internal_error("Induction variable has lb %s. Required a symbol to call method "\
                            "var_is_iv_dependent_in_scope. We have to implement this case.\n",
                            lb.prettyprint().c_str());
                }

                std::cerr << "LB _has_constant_evolution " << _has_constant_evolution << std::endl;

                // check the increment of the induction variable
                Nodecl::NodeclBase increment = iv->get_increment();
                if(increment.is<Nodecl::Symbol>())
                {
                    _has_constant_evolution = _has_constant_evolution || !var_is_iv_dependent_in_scope(increment.as<Nodecl::Symbol>());
                    std::cerr << "Increment _has_constant_evolution " << _has_constant_evolution << std::endl;
                }
                else if(increment.is_constant())
                {
                    _has_constant_evolution = true;
                }
                else
                {
                    internal_error("Induction variable has increment %s. Required a symbol to call method "\
                            "var_is_iv_dependent_in_scope. We have to implement this case.\n",
                            increment.prettyprint().c_str());
                }

                std::cerr << "Increment _has_constant_evolution " << _has_constant_evolution << std::endl;
            }
            else
            {
                _has_constant_evolution = !Utils::ext_sym_set_contains_nodecl( n, _killed ) ||
                    ((iv==NULL) && !var_is_iv_dependent_in_scope( n ));
            }
            */
        }

        _is_adjacent_access = ( n_is_iv && _ivs.back( )->is_increment_one( ) ) || reach_def_is_adjacent;
        _has_constant_evolution = reach_def_has_constant_evolution;
        is_constant = !Utils::ext_sym_set_contains_nodecl( n, _killed ) || !var_is_iv_dependent_in_scope( n );

        //std::cerr << n.prettyprint() << ": is adj " << _is_adjacent_access << ", has constant evolution " 
        //    << _has_constant_evolution << ", is constant " << is_constant << std::endl;

        return is_constant;
    }
}
}
