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


#include "tl-cfg-constants-analysis.hpp"
#include "tl-node.hpp"

namespace TL
{
namespace Analysis
{
    //! Static methods declarations
    static void mark_pcfg_edges_as_non_executable( ExtensibleGraph* pcfg );
    static void mark_pcfg_edges_as_non_executable_rec( Node* current );
    static void init_all_pcfg_operands_as_undetermined_constants( ExtensibleGraph* pcfg );
    static void init_all_pcfg_operands_as_undetermined_constants_rec ( Node* current );
    
    //! Constructor method
    ConstantsAnalysis::ConstantsAnalysis( )
        : _working_list( )
    {}
    
    //! Destructor method
    ConstantsAnalysis::~ConstantsAnalysis( )
    {
        _working_list.clear( );
    }
    
    //! Initialize pcfg for constant propagation algorithm
    //! marking all edges in the graph as "non-executable"
    static void mark_pcfg_edges_as_non_executable( ExtensibleGraph* pcfg )
    {
        Node* pcfg_node = pcfg->get_graph( );
        mark_pcfg_edges_as_non_executable_rec( pcfg_node->get_graph_entry_node() );
        ExtensibleGraph::clear_visits( pcfg_node );
    }
    static void mark_pcfg_edges_as_non_executable_rec( Node* current )
    {
        if ( !current->is_visited( ) )
        {
            current->set_visited(true);
            
            ObjectList<Edge*> current_exit_edges = current->get_exit_edges( );
            for ( ObjectList<Edge*>::iterator it = current_exit_edges.begin( ); it != current_exit_edges.end( ); ++it )
            {
                (*it)->set_executable( true );
            }
            
            ObjectList<Node*> current_children = current->get_children( );
            for ( ObjectList<Node*>::iterator it = current_children.begin( ); it != current_children.end( ); ++it )
            {
                mark_pcfg_edges_as_non_executable_rec(*it);
            }
        }
    }
    
    //! Initialize pcfg for constant propagation algorithm
    //! marking the LatticeCell of all operands as "undetermined constant"
    static void init_all_pcfg_operands_as_undetermined_constants( ExtensibleGraph* pcfg )
    {
        Node* pcfg_node = pcfg->get_graph( );
        init_all_pcfg_operands_as_undetermined_constants_rec( pcfg_node->get_graph_entry_node() );
        ExtensibleGraph::clear_visits( pcfg_node );
    }
    static void init_all_pcfg_operands_as_undetermined_constants_rec ( Node* current )
    {
        if ( !current->is_visited( ) )
        {
            current->set_visited( true );
            
            if ( current->is_graph_node( ) )
            {
                init_all_pcfg_operands_as_undetermined_constants_rec( current->get_graph_entry_node( ) );
            }
            else
            {
                ObjectList<Nodecl::NodeclBase> stmts = current->get_statements( );
                for( ObjectList<Nodecl::NodeclBase>::iterator it = stmts.begin( ); it != stmts.end( ); ++it )
                {
                    
                }
            }
            
            ObjectList<Node*> current_children = current->get_children();
            for ( ObjectList<Node*>::iterator it = current_children.begin( ); it != current_children.end( ); ++it )
            {
                init_all_pcfg_operands_as_undetermined_constants_rec( *it );
            }
        }
    }
    
    
    /*!Conditional Constant Propagation
     * A 'lattice element' represents compile-time knowledge about the value of a given variable.
     * A 'lattice element' can be one of the three types:
     *                        ⊤                        => Variable may be some (as yet) undetermined constant.
     *              /     /   |       \
     *         ς_i    ς_j    ς_k    ...    ς_M         => Specific constant value.
     *              \     \   |       /
     *                        ⊥                        => Constant value can not be guaranteed.
     * The output of the algorithm is an 'output assignment' of lattice values for each operand that will be either ς_x or ⊥.
     * Progressing through the algorithm means lowering through the lattice following the next rules:
     *      · any ⊓  ⊤  = any
     *      · any ⊓  ⊥  = ⊥
     *      · ς_i ⊓ ς_j = ς_i   , if i = j
     *      · ς_i ⊓ ς_j = ⊥     , if i ≠ j
     * 
     */
    void ConstantsAnalysis::conditional_constant_propagation( ExtensibleGraph* pcfg )
    {
        // Prepare pcfg 
        // - marking as "non-executable" all edges
        mark_pcfg_edges_as_non_executable( pcfg );
        // - initializing as '⊤' all operands of expressions at all nodes except the Entry node
        init_all_pcfg_operands_as_undetermined_constants( pcfg );
        
        // Perform analysis
    }
}
}