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
#include "tl-liveness.hpp"
#include "tl-node.hpp"

namespace TL {
namespace Analysis {

    // **************************************************************************************************** //
    // ******************************* Class implementing liveness analysis ******************************* //

    Liveness::Liveness( ExtensibleGraph* graph )
            : _graph( graph )
    {}

    void Liveness::compute_liveness( )
    {
        Node* graph = _graph->get_graph( );

        // Compute initial info (liveness only regarding the current node)
        gather_live_initial_information( graph );
        ExtensibleGraph::clear_visits( graph );

        // Common Liveness analysis
        solve_live_equations( graph );
        ExtensibleGraph::clear_visits( graph );

        // Iterated tasks treatment
        solve_specific_live_in_tasks( graph );
        ExtensibleGraph::clear_visits( graph );
    }

    void Liveness::gather_live_initial_information( Node* current )
    {
        if( !current->is_visited( ) )
        {
            current->set_visited( true );

            if( !current->is_exit_node( ) )
            {
                if( current->is_graph_node( ) )
                {
                    Node* entry = current->get_graph_entry_node( );
                    gather_live_initial_information( entry );
                    set_graph_node_liveness( current );
                }
                else if( !current->is_entry_node( ) )
                {
                    current->set_live_in( current->get_ue_vars( ) );
                }

                ObjectList<Edge*> exit_edges = current->get_exit_edges( );
                for( ObjectList<Edge*>::iterator it = exit_edges.begin( ); it != exit_edges.end( ); ++it )
                {
                    gather_live_initial_information( ( *it )->get_target( ) );
                }
            }
        }
    }

    void Liveness::solve_live_equations( Node* current )
    {
        bool changed = true;
        while( changed )
        {
            changed = false;
            solve_live_equations_rec( current, changed );
            ExtensibleGraph::clear_visits( current );
        }
    }

    void Liveness::solve_live_equations_rec( Node* current, bool& changed )
    {
        if ( !current->is_visited( ) )
        {
            current->set_visited( true );

            if( !current->is_exit_node( ) )
            {
                ObjectList<Node*> children = current->get_children( );

                if( current->is_graph_node( ) )
                {
                    solve_live_equations_rec( current->get_graph_entry_node(), changed );
                    set_graph_node_liveness( current );
                }
                else if( !current->is_entry_node( ) )
                {
                    Utils::ext_sym_set old_live_in = current->get_live_in_vars();
                    Utils::ext_sym_set old_live_out = current->get_live_out_vars();
                    Utils::ext_sym_set live_out, live_in, succ_live_in;

                    // Computing Live out
                    for( ObjectList<Node*>::iterator it = children.begin( ); it != children.end( ); ++it )
                    {
                        bool child_is_exit = ( *it )->is_exit_node( );
                        if( child_is_exit )
                        {
                            // Iterate over outer children while we found an EXIT node
                            Node* exit_outer_node = ( *it )->get_outer_node( );
                            ObjectList<Node*> outer_children;
                            while( child_is_exit )
                            {
                                outer_children = exit_outer_node->get_children( );
                                child_is_exit = ( outer_children.size( ) == 1 ) && outer_children[0]->is_exit_node( );
                                exit_outer_node = ( child_is_exit ? outer_children[0]->get_outer_node( ) : NULL );
                            }
                            // Get the Live in of the current successors
                            for( ObjectList<Node*>::iterator itoc = outer_children.begin( ); itoc != outer_children.end( ); ++itoc )
                            {
                                Utils::ext_sym_set outer_live_in = ( *itoc )->get_live_in_vars( );
                                succ_live_in.insert( outer_live_in.begin( ), outer_live_in.end( ) );
                            }
                        }
                        else
                        {
                            succ_live_in = ( *it )->get_live_in_vars( );
                        }
                        live_out = Utils::ext_sym_set_union( live_out, succ_live_in );
                    }

                    // Computing Live In
                    live_in = Utils::ext_sym_set_union( current->get_ue_vars( ),
                                                        Utils::ext_sym_set_difference( live_out, current->get_killed_vars( ) ) );

                    if( !Utils::ext_sym_set_equivalence( old_live_in, live_in ) ||
                        !Utils::ext_sym_set_equivalence( old_live_out, live_out ) )
                    {
                        current->set_live_in( live_in );
                        current->set_live_out( live_out );
                        changed = true;
                    }
                }

                for( ObjectList<Node*>::iterator it = children.begin( ); it != children.end( ); ++it )
                {
                    solve_live_equations_rec( *it, changed );
                }
            }
        }
    }

    void Liveness::solve_specific_live_in_tasks( Node* current )
    {
        if( !current->is_visited( ) )
        {
            current->set_visited( true );
            if( current->is_graph_node( ) )
            {
                if( current->is_task_node( ) )
                {
                    if( task_is_in_loop( current ) )
                    {
                        Utils::ext_sym_set task_li = current->get_live_in_vars( );
                        Utils::ext_sym_set task_lo = current->get_live_out_vars( );
                        for( Utils::ext_sym_set::iterator it = task_li.begin( ); it != task_li.end( ); ++it )
                        {
                            if( Utils::ext_sym_set_contains_englobed_nodecl( *it, task_lo ) )
                            {
                                delete_englobed_var_from_list( *it, task_lo );
                                current->set_live_out( *it );
                            }
                            else if( !Utils::ext_sym_set_contains_englobing_nodecl( *it, task_lo ) )
                            {
                                current->set_live_out( *it );
                            }
                        }
                    }
                }

                solve_specific_live_in_tasks( current->get_graph_entry_node( ) );
            }

            ObjectList<Node*> children = current->get_children( );
            for( ObjectList<Node*>::iterator it = children.begin( ); it != children.end( ); ++it )
            {
                solve_specific_live_in_tasks( *it );
            }
        }
    }

    bool Liveness::task_is_in_loop( Node* current )
    {
        bool res = false;

        ObjectList<Edge*> entries = current->get_entry_edges( );
        for( ObjectList<Edge*>::iterator it = entries.begin( ); it != entries.end( ); ++it )
        {
            if( ( *it )->is_back_edge( ) )
                return true;
        }

        ObjectList<Node*> parents = current->get_parents( );
        for( ObjectList<Node*>::iterator it = parents.begin( ); it != parents.end( ); ++it )
        {
            res = res || task_is_in_loop( *it );
        }

        return res;
    }

    void Liveness::set_graph_node_liveness( Node* current )
    {
        if( current->is_graph_node( ) )
        {
            // LI(graph) = U LI(inner entries)
            Utils::ext_sym_set graph_li, live_in;
            ObjectList<Node*> entries = current->get_graph_entry_node( )->get_children( );
            for( ObjectList<Node*>::iterator it = entries.begin( ); it != entries.end( ); ++it )
            {
                live_in = Utils::ext_sym_set_union( live_in, ( *it )->get_live_in_vars( ) );
            }
            // Delete those variables which are local to the graph
            Scope sc( current->get_node_scope( ) );
            if( sc.is_valid( ) )
            {
                for( Utils::ext_sym_set::iterator it = live_in.begin( ); it != live_in.end( ); ++it )
                {
                    ObjectList<Symbol> syms = it->get_symbols( );
                    for( ObjectList<Symbol>::iterator its = syms.begin( ); its != syms.end( ); ++its )
                    {   // If one of the symbols in the expression is not local, then the whole symbol is not local
                        if( !its->get_scope( ).scope_is_enclosed_by( sc ) )
                            graph_li.insert( *it );
                    }
                }
            }
            current->set_live_in( graph_li );

            // LO(graph) = U LO(inner exits)
            Utils::ext_sym_set graph_lo;
            ObjectList<Node*> exits = current->get_graph_exit_node( )->get_parents( );
            for( ObjectList<Node*>::iterator it = exits.begin( ); it != exits.end( ); ++it )
            {
                graph_lo = Utils::ext_sym_set_union( graph_lo, ( *it )->get_live_out_vars( ) );
            }
            current->set_live_out( graph_lo );
        }
    }

    // ***************************** END class implementing liveness analysis ***************************** //
    // **************************************************************************************************** //

}
}