/*--------------------------------------------------------------------
  (C) Copyright 2006-2014 Barcelona Supercomputing Center
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

#include "tl-vectorization-analysis-internals.hpp"
#include "tl-analysis-internals.hpp"
#include "tl-tribool.hpp"
#include "cxx-cexpr.h"
//#include "tl-analysis-utils.hpp"
//#include "tl-analysis-static-info.hpp"
//#include "tl-expression-reduction.hpp"
//#include <algorithm>

namespace TL
{
namespace Vectorization
{
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

    struct adjacency_property
    {
        TL::tribool operator()(
                Analysis::Node* const scope_node,
                Analysis::Node* const stmt_node,
                const Nodecl::NodeclBase& n,
                const Nodecl::NodeclBase& prev_n,
                Analysis::ExtensibleGraph* const pcfg,
                std::set<Nodecl::NodeclBase> visited_nodes)
        {
            if (n.is<Nodecl::Symbol>())
            {
                bool is_linear = is_linear_internal(scope_node, n);
                bool is_linear_stride_one = false;

                if (is_linear)
                {
                    //std::set<Nodecl::NodeclBase, Nodecl::Utils::Nodecl_structural_less> lower_bounds
                    //        = get_linear_variable_lower_bound_internal(scope_node, n);
                    Nodecl::NodeclBase step = get_linear_variable_increment_internal(scope_node, n);

                    //ERROR_CONDITION(lower_bounds.size() != 1,
                    //                "Induction variable '%s' has %d lower bounds. "
                    //                "Only 1 lower bound supported.\n",
                    //                n.get_symbol().get_name().c_str(), lower_bounds.size());

                    //Nodecl::NodeclBase lower_bound = *lower_bounds.begin();

                    if (step.is_constant())
                    {
                        TL::Type sym_type = n.get_type().no_ref();

                        if (sym_type.is_pointer())
                        {
                            int pointed_type_size = sym_type.points_to().get_size();
                            int step_int = const_value_cast_to_4(step.get_constant());
                            
                            if (pointed_type_size == step_int)
                                is_linear_stride_one = true;
                        }
                        else
                        {
                            if (nodecl_is_one(step))
                                is_linear_stride_one = true;
                        }
                    }
                }

                if (is_linear)
                {
                    if (is_linear_stride_one)
                    {
                        return true;
                    }
                    else
                    { 
                        return false;
                    }
                }
                else
                {
                    return TL::tribool::unknown;
                }
            }
            // Unknown RD
            else if(n.is<Nodecl::Unknown>())
            {
                // Unknown parameter must be taken into account only
                // if is inside the scope_node, i.e. is a scope_node
                // is a function code
                if (scope_node->is_function_code_node())
                {
                    if (prev_n.is<Nodecl::Symbol>() && 
                            prev_n.get_symbol().is_parameter())
                    {
                        // TODO: if 'n' is linear with step 1, then true

                        // So far, assume not uniform by default
                        return false;
                    }
                    else
                    {
                        internal_error("AdjacencyProperty: Unknown RD from a non parameter node '%s'",
                                n.prettyprint().c_str());
                    }
                }
                else
                {
                    // Unknown parameter is not inside the scope
                    // E.g. it's uniform
                    return false;
                }
            }
            else
            {
                // TODO: n instead of stmt_node.
                // If the 'n' is not contained in the scope node,
                // then n is uniform in the scope, so is not
                // adjacent
                if(!Analysis::ExtensibleGraph::node_contains_node(
                            scope_node, stmt_node))
                    return false;

                ExpressionEvolutionVisitor expression_evolution(
                        scope_node, stmt_node, pcfg, visited_nodes);
                expression_evolution.walk(n);

                return expression_evolution.is_adjacent_access();
            }
        }
    };

    ExpressionEvolutionVisitor::ExpressionEvolutionVisitor(
            Analysis::Node* scope, Analysis::Node* n_node, 
            Analysis::ExtensibleGraph* pcfg,
            std::set<Nodecl::NodeclBase> adjacency_visited_nodes )
            : _scope(scope),
              _killed( scope->get_killed_vars() ),
              _pcfg( pcfg ), _scope_node( scope ), _n_node( n_node ),
              _is_adjacent_access( false ), 
              _adjacency_visited_nodes(adjacency_visited_nodes)
    {}

    bool ExpressionEvolutionVisitor::is_adjacent_access( )
    {
        return _is_adjacent_access;
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
        bool lhs_is_uniform = walk( lhs );
        bool lhs_is_adjacent_access = _is_adjacent_access;

        // Gather RHS info
        Nodecl::NodeclBase rhs = n.get_rhs( );
        bool rhs_is_uniform = walk( rhs );
        bool rhs_is_adjacent_access = _is_adjacent_access;

        //std::cerr << "In " << n.prettyprint() << " lhs is constant " << lhs_is_uniform
        //    << " and rhs is constant " << rhs_is_uniform << std::endl;

        // Compute adjacency info
        _is_adjacent_access = ( lhs_is_adjacent_access && rhs_is_uniform )
                           || ( lhs_is_uniform && rhs_is_adjacent_access );

        return ( rhs_is_uniform && lhs_is_uniform );
    }

    bool ExpressionEvolutionVisitor::visit( const Nodecl::ArraySubscript& n )
    {
        // Collect information about the induction variables contained in the node
        bool n_is_linear = is_linear_internal( _scope, n );
        walk( n.get_subscripted( ) );
        walk( n.get_subscripts( ) );

        bool is_linear = is_linear_internal(_scope, n);
        bool is_linear_stride_one = false;

        if (is_linear)
        {
            //std::set<Nodecl::NodeclBase, Nodecl::Utils::Nodecl_structural_less> lower_bounds
            //        = get_linear_variable_lower_bound_internal(_scope, n);
            Nodecl::NodeclBase step = get_linear_variable_increment_internal(_scope, n);

            //ERROR_CONDITION(lower_bounds.size() != 1,
            //                "Induction variable '%s' has %d lower bounds. "
            //                "Only 1 lower bound supported.\n",
            //                n.prettyprint().c_str(), lower_bounds.size());

            //Nodecl::NodeclBase lower_bound = *lower_bounds.begin();

            if (step.is_constant() && nodecl_is_one(step))
            {
                is_linear_stride_one = true;
            }
        }

        _is_adjacent_access = ( n_is_linear && is_linear_stride_one);

        // 
        return !Analysis::Utils::nodecl_set_contains_nodecl( n, _killed );
    }

    bool ExpressionEvolutionVisitor::visit( const Nodecl::Assignment& n )
    {
        // Gather LHS info
        Nodecl::NodeclBase lhs = n.get_lhs( );
        bool lhs_is_uniform = walk( lhs );
        // lhs does not affect adjacency

        // Gather RHS info
        Nodecl::NodeclBase rhs = n.get_rhs( );
        bool rhs_is_uniform = walk( rhs );

        return ( rhs_is_uniform && lhs_is_uniform );
    }

    bool ExpressionEvolutionVisitor::visit( const Nodecl::BitwiseShl& n )
    {
        // Gather LHS info
        Nodecl::NodeclBase lhs = n.get_lhs( );
        bool lhs_is_uniform = walk( lhs );
        bool lhs_is_adjacent_access = _is_adjacent_access;

        // Gather RHS info
        Nodecl::NodeclBase rhs = n.get_rhs( );
        bool rhs_is_uniform = walk( rhs );
        bool rhs_is_zero = false;
        if( rhs_is_uniform )
            rhs_is_zero = nodecl_is_zero( rhs );

        // Compute adjacency info
        _is_adjacent_access = lhs_is_adjacent_access && rhs_is_zero;

        return ( lhs_is_uniform && rhs_is_uniform );
    }

    bool ExpressionEvolutionVisitor::visit( const Nodecl::BitwiseShr& n )
    {
        // Gather LHS info
        Nodecl::NodeclBase lhs = n.get_lhs( );
        bool lhs_is_uniform = walk( lhs );
        bool lhs_is_adjacent_access = _is_adjacent_access;

        // Gather RHS info
        Nodecl::NodeclBase rhs = n.get_rhs( );
        bool rhs_is_uniform = walk( rhs );
        bool rhs_is_zero = false;
        if( rhs_is_uniform )
            rhs_is_zero = nodecl_is_zero( rhs );

        // Compute adjacency info
        _is_adjacent_access = lhs_is_adjacent_access && rhs_is_zero;

        return ( lhs_is_uniform && rhs_is_uniform );
    }

    bool ExpressionEvolutionVisitor::visit( const Nodecl::BooleanLiteral& n )
    {
        return true;
    }

    bool ExpressionEvolutionVisitor::visit( const Nodecl::ComplexLiteral& n )
    {
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
        bool lhs_is_uniform = walk( lhs );
        bool lhs_is_adjacent_access = _is_adjacent_access;

        // Gather RHS info
        Nodecl::NodeclBase rhs = n.get_rhs( );
        bool rhs_is_uniform = walk( rhs );
        bool rhs_is_one = false;
        if( rhs_is_uniform )
            rhs_is_one = nodecl_is_one( rhs );

        // Compute adjacency info
        _is_adjacent_access = lhs_is_adjacent_access && rhs_is_one;

        return ( lhs_is_uniform && rhs_is_uniform );
    }

    bool ExpressionEvolutionVisitor::visit( const Nodecl::FloatingLiteral& n )
    {
        return true;
    }

    bool ExpressionEvolutionVisitor::visit( const Nodecl::FunctionCall& n )
    {
        // Traverse arguments to find induction variables
        walk( n.get_arguments( ) );

        _is_adjacent_access = false;      // Reset this value

        return false; // Conservatively assume the result of the function call is not constant
    }

    bool ExpressionEvolutionVisitor::visit( const Nodecl::IntegerLiteral& n )
    {
        return true;
    }

    bool ExpressionEvolutionVisitor::visit( const Nodecl::LowerThan& n )
    {
        // Gather LHS info
        Nodecl::NodeclBase lhs = n.get_lhs( );
        bool lhs_is_uniform = walk( lhs );

        // Gather RHS info
        Nodecl::NodeclBase rhs = n.get_rhs( );
        bool rhs_is_uniform = walk( rhs );

        // Adjacency is not applicable
        _is_adjacent_access = false;

        return ( rhs_is_uniform && lhs_is_uniform );
    }

    bool ExpressionEvolutionVisitor::visit( const Nodecl::MaskLiteral& n )
    {
        return true;
    }

    bool ExpressionEvolutionVisitor::visit( const Nodecl::Minus& n )
    {
        // Gather LHS info
        Nodecl::NodeclBase lhs = n.get_lhs( );
        bool lhs_is_uniform = walk( lhs );
        bool lhs_is_adjacent_access = _is_adjacent_access;

        // Gather RHS info
        Nodecl::NodeclBase rhs = n.get_rhs( );
        bool rhs_is_uniform = walk( rhs );
        bool rhs_is_adjacent_access = _is_adjacent_access;

        // Compute adjacency info
        _is_adjacent_access = ( lhs_is_adjacent_access && rhs_is_uniform )
                           || ( lhs_is_uniform && rhs_is_adjacent_access );

        return ( rhs_is_uniform && lhs_is_uniform );
    }

    bool ExpressionEvolutionVisitor::visit( const Nodecl::Mul& n )
    {
        // Gather LHS info
        Nodecl::NodeclBase lhs = n.get_lhs( );
        bool lhs_is_uniform = walk( lhs );
        bool lhs_is_one = false;
        if( lhs_is_uniform )
            lhs_is_one = nodecl_is_one( lhs );
        bool lhs_is_adjacent_access = _is_adjacent_access;

        // Gather RHS info
        Nodecl::NodeclBase rhs = n.get_rhs( );
        bool rhs_is_uniform = walk( rhs );
        bool rhs_is_one = false;
        if( rhs_is_uniform )
            rhs_is_one = nodecl_is_one( rhs );
        bool rhs_is_adjacent_access = _is_adjacent_access;

        // Compute adjacency info
        _is_adjacent_access = ( lhs_is_adjacent_access && rhs_is_one )
                           || ( rhs_is_adjacent_access && lhs_is_one );

        return ( lhs_is_uniform && rhs_is_uniform );
    }

    bool ExpressionEvolutionVisitor::visit( const Nodecl::Neg& n )
    {
        return walk( n.get_rhs( ) );
    }

    bool ExpressionEvolutionVisitor::visit( const Nodecl::PointerToMember& n )
    {
        // Collect information about the induction variables contained in the node
        bool is_linear = is_linear_internal(_scope, n);
        bool is_linear_stride_one = false;

        if (is_linear)
        {
            //std::set<Nodecl::NodeclBase, Nodecl::Utils::Nodecl_structural_less> lower_bounds
            //        = get_linear_variable_lower_bound_internal(_scope, n);
            Nodecl::NodeclBase step = get_linear_variable_increment_internal(_scope, n);

            //ERROR_CONDITION(lower_bounds.size() != 1,
            //                "Induction variable '%s' has %d lower bounds. "
            //                "Only 1 lower bound supported.\n",
            //                n.prettyprint().c_str(), lower_bounds.size());

            //Nodecl::NodeclBase lower_bound = *lower_bounds.begin();

            if (step.is_constant() && nodecl_is_one(step))
            {
                is_linear_stride_one = true;
            }
        }


        _is_adjacent_access = is_linear_stride_one;

        return !Analysis::Utils::nodecl_set_contains_nodecl( n, _killed );
    }

    bool ExpressionEvolutionVisitor::visit( const Nodecl::Postdecrement& n )
    {
        // Gather information about induction variables
        walk( n.get_rhs( ) );

        _is_adjacent_access = false;

        return false;
    }

    bool ExpressionEvolutionVisitor::visit( const Nodecl::Postincrement& n )
    {
        // Gather information about induction variables
        walk( n.get_rhs( ) );

        _is_adjacent_access = false;

        return false;
    }

    bool ExpressionEvolutionVisitor::visit( const Nodecl::Power& n )
    {
        // Gather LHS info
        Nodecl::NodeclBase lhs = n.get_lhs( );
        bool lhs_is_uniform = walk( lhs );
        bool lhs_is_adjacent_access = _is_adjacent_access;

        // Gather RHS info
        Nodecl::NodeclBase rhs = n.get_rhs( );
        bool rhs_is_uniform = walk( rhs );
        bool rhs_is_one = false;
        if( rhs_is_uniform )
            rhs_is_one = nodecl_is_one( rhs );

        // Compute adjacency info
        _is_adjacent_access = lhs_is_adjacent_access && rhs_is_uniform && rhs_is_one;

        return ( lhs_is_uniform && rhs_is_uniform );
    }

    bool ExpressionEvolutionVisitor::visit( const Nodecl::Predecrement& n )
    {
        walk( n.get_rhs( ) );

        _is_adjacent_access = false;

        return false;
    }

    bool ExpressionEvolutionVisitor::visit( const Nodecl::Preincrement& n )
    {
        walk( n.get_rhs( ) );

        _is_adjacent_access = false;

        return false;
    }

    bool ExpressionEvolutionVisitor::visit( const Nodecl::Reference& n )
    {
        return walk( n.get_rhs( ) );
    }

    bool ExpressionEvolutionVisitor::visit( const Nodecl::Sizeof& n )
    {
        bool n_is_uniform = walk( n.get_expr( ) );

        _is_adjacent_access = false;

        return n_is_uniform;
    }

    bool ExpressionEvolutionVisitor::visit( const Nodecl::StringLiteral& n )
    {
        return true;
    }

    bool ExpressionEvolutionVisitor::visit( const Nodecl::Symbol& n )
    {
        // Collect information about the induction variables contained in the node
        // bool n_is_linear = variable_is_linear( n );

        //std::cerr << "Studying " << n.prettyprint() << std::endl;

        bool n_is_uniform = false;
        // bool reach_defs_are_adjacent = false;
        // bool reach_defs_are_uniform = false;

        TL::tribool is_adjacent_tribool = nodecl_has_property_in_scope(
                    _scope_node, _n_node, _n_node, n, 
                    Nodecl::NodeclBase::null(),_pcfg,
                    adjacency_property(), _adjacency_visited_nodes);

        ERROR_CONDITION(is_adjacent_tribool.is_unknown(), 
                "ExpressionEvolutionVisitor: nodecl_has_property unknown", 0);

        _is_adjacent_access = is_adjacent_tribool.is_true();

        n_is_uniform = is_uniform_internal(
                _scope_node, _n_node, n, _pcfg);
                
        return n_is_uniform;
    }

}
}
