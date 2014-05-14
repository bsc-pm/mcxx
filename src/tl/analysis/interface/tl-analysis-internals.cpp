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
 
#include "tl-analysis-internals.hpp"

#include "tl-tribool.hpp"

//#include "cxx-process.h"
//#include "tl-analysis-utils.hpp"
//#include "tl-analysis-static-info.hpp"
//#include "tl-expression-reduction.hpp"
//#include "tl-use-def.hpp"
 
namespace TL  {
namespace Analysis {

    /*
     *  PROPERTIES
     */

    TL::tribool invariant_property(
            Node* const scope_node,
            Node* const stmt_node,
            const Nodecl::NodeclBase& n,
            ExtensibleGraph* const pcfg,
            std::set<Nodecl::NodeclBase> visited_nodes)
    {
        // Start of base cases for Invariant.
        if(Nodecl::Utils::nodecl_is_literal(n))
            return true;

        // TODO: n instead of stmt_node.
        // If the 'n' is not contained in the scope node,
        // then n is invariant in the scope
        if(!ExtensibleGraph::node_contains_node(
                    scope_node, stmt_node))
            return true;

        if(Nodecl::Utils::nodecl_contains_nodecl_of_kind<Nodecl::FunctionCall>(n)) 
            return false;

        // Check if n contains an IV of the scope
        // TODO: if it's not loop, return empty list?
        if(scope_node->is_loop_node())
        {
            ObjectList<Utils::InductionVariableData *> scope_ivs =
                scope_node->get_induction_variables();

            for(ObjectList<Utils::InductionVariableData *>::iterator it = scope_ivs.begin();
                    it != scope_ivs.end();
                    it++)
            {
                if(Nodecl::Utils::find_nodecl_by_structure(n,
                            (*it)->get_variable().get_nodecl()))
                    return false;
            }
        }

        return TL::tribool::unknown;
    }

    /*
     *  QUERIES
     */

    bool is_invariant_internal(
            Node* const scope_node,
            Node* const stmt_node,
            Node* const n_node,
            const Nodecl::NodeclBase& n,
            ExtensibleGraph* const pcfg,
            std::set<Nodecl::NodeclBase> visited_nodes)
    {
        TL::tribool result = nodecl_has_property_in_scope(scope_node,
                stmt_node, n_node, n, pcfg,
                invariant_property,
                visited_nodes);

        ERROR_CONDITION(result.is_unknown(),
                "is_invariant_internal returns unknown!", 0);

        return result.is_true();
    }

    bool has_been_defined_internal(Node* const n_node,
            const Nodecl::NodeclBase& n,
            const std::set<TL::Symbol>& global_variables)
    {
        bool result = false;

        if( n.is<Nodecl::Symbol>( ) || n.is<Nodecl::ArraySubscript>( )
                || n.is<Nodecl::ClassMemberAccess>( ) )
        {
            Utils::ext_sym_map rd_in = n_node->get_reaching_definitions_in();
            std::pair<Utils::ext_sym_map::iterator, Utils::ext_sym_map::iterator> n_rds =
                rd_in.equal_range(n);
    
            if(n_rds.first != n_rds.second) // n has RDs
            {
                return true;
            }
            else // n doesn't have RDs
            {
                Nodecl::NodeclBase nodecl_base = Utils::get_nodecl_base(n);
                if (nodecl_base.is<Nodecl::Symbol>())
                {
                    Nodecl::Symbol sym = nodecl_base.as<Nodecl::Symbol>();
                    if(global_variables.find(sym.get_symbol()) != 
                                global_variables.end()) // n is a global var
                        result = true;
                }
            }
        }
        else
        {
            WARNING_MESSAGE( "Nodecl '%s' is neither symbol, ArraySubscript or ClassMemberAccess. " \
                    "One of these types required as defined option. Returning false.\n", n.prettyprint( ).c_str( ) );
        }

        return result;
    }

    bool is_iv_internal(Node* const scope_node, const Nodecl::NodeclBase& n)
    { 
        bool result = false;

        ObjectList<Analysis::Utils::InductionVariableData*> ivs =
            scope_node->get_induction_variables();

        for( ObjectList<Analysis::Utils::InductionVariableData*>::const_iterator it = ivs.begin( );
                it != ivs.end( ); ++it )
        {
            if ( Nodecl::Utils::structurally_equal_nodecls(
                        ( *it )->get_variable( ).get_nodecl( ), n,
                        /* skip conversion nodes */ true ) )
            {
                result = ( *it )->is_basic( );
                break;
            }
        }

        return result;
    }

    bool is_non_reduction_basic_iv_internal(Node* const scope_node,
            const Nodecl::NodeclBase& n)
    {
        bool result = false;

        ObjectList<Analysis::Utils::InductionVariableData*> ivs =
            scope_node->get_induction_variables();
        ObjectList<TL::Symbol> reductions =
            scope_node->get_reductions();

        for( ObjectList<Analysis::Utils::InductionVariableData*>::const_iterator it = ivs.begin( );
                it != ivs.end( ); ++it )
        {
            if( !reductions.contains( ( *it )->get_variable( ).get_symbol( ) ) )
            {
                if ( Nodecl::Utils::structurally_equal_nodecls(
                            ( *it )->get_variable( ).get_nodecl( ), n,
                            /* skip conversion nodes */ true ) )
                {
                    result = ( *it )->is_basic( );
                    break;
                }
            }
        }

        return result;
    }

    Nodecl::NodeclBase get_iv_lower_bound_internal(Node* const scope_node,
            const Nodecl::NodeclBase& n)
    {
        Nodecl::NodeclBase result;

        ObjectList<Analysis::Utils::InductionVariableData*> ivs =
            scope_node->get_induction_variables();

        ObjectList<Analysis::Utils::InductionVariableData*>::const_iterator it;
        for( it = ivs.begin( );
             it != ivs.end( ); ++it )
        {
            if ( Nodecl::Utils::structurally_equal_nodecls(
                        ( *it )->get_variable( ).get_nodecl( ), n,
                        /* skip conversion nodes */ true ) )
            {
                result = ( *it )->get_lb( );
                break;
            }
        }

        if( it == ivs.end( ) )
        {
            WARNING_MESSAGE( "You are asking for the lower bound of an Object ( %s ) "\
                             "which is not an Induction Variable\n", n.prettyprint( ).c_str( ) );
        }

        return result;
    }

    Nodecl::NodeclBase get_iv_increment_internal(Node* const scope_node,
            const Nodecl::NodeclBase& n)
    {
        Nodecl::NodeclBase result;

        ObjectList<Analysis::Utils::InductionVariableData*> ivs =
            scope_node->get_induction_variables();

        ObjectList<Analysis::Utils::InductionVariableData*>::const_iterator it;
        for( it = ivs.begin( );
             it != ivs.end( ); ++it )
        {
            if ( Nodecl::Utils::structurally_equal_nodecls(
                        ( *it )->get_variable( ).get_nodecl( ), n,
                        /* skip conversion nodes */ true ) )
            {
                result = ( *it )->get_increment( );
                break;
            }
        }

        if( it == ivs.end( ) )
        {
            WARNING_MESSAGE( "You are asking for the increment bound of an Object ( %s ) "\
                             "which is not an Induction Variable\n", n.prettyprint( ).c_str( ) );
        }

        return result;
    }
}
}
