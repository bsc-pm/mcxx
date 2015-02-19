/*--------------------------------------------------------------------
 ( C) Copyright 2006-2014 Barcelona Supercomputing Center             *
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

#include "tl-constants-analysis.hpp"

namespace TL {
namespace Analysis {

    // ******************************************************************************************* //
    // ********** Class representing the Lattice Cell of Constant Propagation Algorithm ********** //

    LatticeCellValue::LatticeCellValue( )
        : _ext_sym( Nodecl::NodeclBase::null( ) ), _lattice_val( undetermined_const_val ),
        _const_val( Nodecl::NodeclBase::null( ) )
    {}

    void LatticeCellValue::set_ext_sym( Nodecl::NodeclBase es )
    {
        _ext_sym = es;
    }

    void LatticeCellValue::set_lattice_val( LatticeValue lv )
    {
        _lattice_val = lv;
    }

    void LatticeCellValue::set_const_val( Nodecl::NodeclBase cv )
    {
        _const_val = cv;
    }

    // ******** END class representing the Lattice Cell of Constant Propagation Algorithm ******** //
    // ******************************************************************************************* //



    // ******************************************************************************************* //
    // ******************** Visitor computing the Extensible Symbols modified ******************** //

    AssignedNodeclVisitor::AssignedNodeclVisitor( )
            : _assigned_nodecls( ), _is_lhs( false )
    {}

    ObjectList<Nodecl::NodeclBase> AssignedNodeclVisitor::get_assigned_nodecls()
    {
        return _assigned_nodecls;
    }

    void AssignedNodeclVisitor::visit_assignment( Nodecl::NodeclBase lhs, Nodecl::NodeclBase rhs )
    {
        //! Keep record of the value of \_lhs for nested assignments
        bool is_lhs = _is_lhs;

        // Traverse lhs
        _is_lhs = true;
        walk( lhs );
        // Traverse rhs
        _is_lhs = is_lhs;
        walk( rhs );
    }

    void AssignedNodeclVisitor::visit_xx_crements( Nodecl::NodeclBase n )
    {
        bool is_lhs = _is_lhs;
        _is_lhs = true;
        walk( n );
        _is_lhs = is_lhs;
    }

    AssignedNodeclVisitor::Ret AssignedNodeclVisitor::visit( const Nodecl::AddAssignment& n )
    {
        visit_assignment( n.get_lhs( ), n.get_rhs( ) );
    }

    AssignedNodeclVisitor::Ret AssignedNodeclVisitor::visit( const Nodecl::ArithmeticShrAssignment& n )
    {
        visit_assignment( n.get_lhs( ), n.get_rhs( ) );
    }

    AssignedNodeclVisitor::Ret AssignedNodeclVisitor::visit( const Nodecl::ArraySubscript& n )
    {
        if ( _is_lhs )
            _assigned_nodecls.insert( n );
    }

    AssignedNodeclVisitor::Ret AssignedNodeclVisitor::visit( const Nodecl::Assignment& n )
    {
        visit_assignment( n.get_lhs( ), n.get_rhs( ) );
    }

    AssignedNodeclVisitor::Ret AssignedNodeclVisitor::visit( const Nodecl::BitwiseAndAssignment& n )
    {
        visit_assignment( n.get_lhs( ), n.get_rhs( ) );
    }

    AssignedNodeclVisitor::Ret AssignedNodeclVisitor::visit( const Nodecl::BitwiseOrAssignment& n )
    {
        visit_assignment( n.get_lhs( ), n.get_rhs( ) );
    }

    AssignedNodeclVisitor::Ret AssignedNodeclVisitor::visit( const Nodecl::BitwiseShlAssignment& n )
    {
        visit_assignment( n.get_lhs( ), n.get_rhs( ) );
    }

    AssignedNodeclVisitor::Ret AssignedNodeclVisitor::visit( const Nodecl::BitwiseShrAssignment& n )
    {
        visit_assignment( n.get_lhs( ), n.get_rhs( ) );
    }

    AssignedNodeclVisitor::Ret AssignedNodeclVisitor::visit( const Nodecl::BitwiseXorAssignment& n )
    {
        visit_assignment( n.get_lhs( ), n.get_rhs( ) );
    }

    AssignedNodeclVisitor::Ret AssignedNodeclVisitor::visit( const Nodecl::ClassMemberAccess& n )
    {
        if ( _is_lhs )
            _assigned_nodecls.insert( n );
    }

    AssignedNodeclVisitor::Ret AssignedNodeclVisitor::visit( const Nodecl::DivAssignment& n )
    {
        visit_assignment( n.get_lhs( ), n.get_rhs( ) );
    }

    AssignedNodeclVisitor::Ret AssignedNodeclVisitor::visit( const Nodecl::MinusAssignment& n )
    {
        visit_assignment( n.get_lhs( ), n.get_rhs( ) );
    }

    AssignedNodeclVisitor::Ret AssignedNodeclVisitor::visit( const Nodecl::ModAssignment& n )
    {
        visit_assignment( n.get_lhs( ), n.get_rhs( ) );
    }

    AssignedNodeclVisitor::Ret AssignedNodeclVisitor::visit( const Nodecl::MulAssignment& n )
    {
        visit_assignment( n.get_lhs( ), n.get_rhs( ) );
    }

    AssignedNodeclVisitor::Ret AssignedNodeclVisitor::visit( const Nodecl::Postdecrement& n )
    {
        visit_xx_crements( n.get_rhs( ) );
    }

    AssignedNodeclVisitor::Ret AssignedNodeclVisitor::visit( const Nodecl::Postincrement& n )
    {
        visit_xx_crements( n.get_rhs( ) );
    }

    AssignedNodeclVisitor::Ret AssignedNodeclVisitor::visit( const Nodecl::Predecrement& n )
    {
        visit_xx_crements( n.get_rhs( ) );
    }

    AssignedNodeclVisitor::Ret AssignedNodeclVisitor::visit( const Nodecl::Preincrement& n )
    {
        visit_xx_crements( n.get_rhs( ) );
    }

    AssignedNodeclVisitor::Ret AssignedNodeclVisitor::visit( const Nodecl::Symbol& n )
    {
        if ( _is_lhs )
            _assigned_nodecls.insert( n );
    }

    // ****************** END visitor computing the Extensible Symbols modified ****************** //
    // ******************************************************************************************* //



    // ******************************************************************************************* //
    // ****************************** Conditional Constant Analysis ****************************** //

    //! Static methods declarations
    static void mark_pcfg_edges_as_non_executable( Node* graph );
    static void mark_pcfg_edges_as_non_executable_rec( Node* current );
    static void init_all_pcfg_operands_lattice( Node* graph, Scope sc, bool ipa );
    static void init_all_pcfg_operands_lattice_rec ( Node* current, Scope sc, bool ipa );

    //! Constructor method
    ConditionalConstantAnalysis::ConditionalConstantAnalysis( bool ipa )
        : _work_list( ), _ipa( ipa )
    {}

    //! Destructor method
    ConditionalConstantAnalysis::~ConditionalConstantAnalysis( )
    {
        _work_list.clear( );
    }

    //! Initialize pcfg for constant propagation algorithm
    //! marking all edges in the graph as "non-executable"
    static void mark_pcfg_edges_as_non_executable( Node* graph )
    {
        mark_pcfg_edges_as_non_executable_rec( graph );
        ExtensibleGraph::clear_visits( graph );
    }
    static void mark_pcfg_edges_as_non_executable_rec( Node* current )
    {
        if ( !current->is_visited( ) )
        {
            current->set_visited(true);

            ObjectList<Edge*> current_exit_edges = current->get_exit_edges( );
            for ( ObjectList<Edge*>::iterator it = current_exit_edges.begin( ); it != current_exit_edges.end( ); ++it )
            {
                (*it)->set_executable( false );
            }

            ObjectList<Node*> current_children = current->get_children( );
            for ( ObjectList<Node*>::iterator it = current_children.begin( ); it != current_children.end( ); ++it )
            {
                mark_pcfg_edges_as_non_executable_rec(*it);
            }
        }
    }

    /*!Initialize pcfg for constant propagation algorithm
     * marking the LatticeCell of all operands as "undetermined constant"
     * When this analysis is not IPA:
     * - global variables are initialized as 'overdefined' and
     * - local variables are initialized as 'undetermined'
     * Once a variable is marked as undetermined it can never again be marked with a different value
     */
    static void init_all_pcfg_operands_lattice( Node* graph, Scope sc, bool ipa )
    {
        init_all_pcfg_operands_lattice_rec( graph, sc, ipa );
        ExtensibleGraph::clear_visits( graph );
    }
    static void init_all_pcfg_operands_lattice_rec ( Node* current, Scope sc, bool ipa )
    {
        if ( !current->is_visited( ) )
        {
            current->set_visited( true );

            if ( current->is_graph_node( ) )
            {
                init_all_pcfg_operands_lattice_rec( current->get_graph_entry_node( ), sc, ipa );
            }
            else
            {
                AssignedNodeclVisitor v;
                ObjectList<Nodecl::NodeclBase> stmts;
                ObjectList<Nodecl::NodeclBase>::iterator it_s;
                ObjectList<Nodecl::NodeclBase> assigned_nodecls;
                ObjectList<Nodecl::NodeclBase>::iterator it_es;

                if( ipa )
                {
                    stmts = current->get_statements( );
                    for( it_s = stmts.begin( ); it_s != stmts.end( ); ++it_s )
                    {
                        v.walk( *it_s );
                        assigned_nodecls = v.get_assigned_nodecls( );
                        for( it_es = assigned_nodecls.begin( ); it_es != assigned_nodecls.end( ); ++it_es )
                        {
                            LatticeCellValue lcv;
                            lcv.set_ext_sym( *it_es );

                            current->set_lattice_val(lcv);
                        }
                    }
                }
                else
                {
                    stmts = current->get_statements( );
                    for( it_s = stmts.begin( ); it_s != stmts.end( ); ++it_s )
                    {
                        v.walk( *it_s );
                        assigned_nodecls = v.get_assigned_nodecls( );
                        for( it_es = assigned_nodecls.begin( ); it_es != assigned_nodecls.end( ); ++it_es )
                        {
                            LatticeCellValue lcv;
                            if ( !it_es->get_nodecl().retrieve_context( ).scope_is_enclosed_by( sc ) )
                            {   // Variable is global
                                lcv.set_lattice_val( overdefined_const_val );
                            }
                            lcv.set_ext_sym( *it_es );
                            current->set_lattice_val( lcv );
                        }
                    }
                }
            }

            ObjectList<Node*> current_children = current->get_children( );
            for ( ObjectList<Node*>::iterator it = current_children.begin( ); it != current_children.end( ); ++it )
            {
                init_all_pcfg_operands_lattice_rec( *it, sc, ipa );
            }
        }
    }

    /*!Conditional Constant Propagation
     * The output of the algorithm is an 'output assignment' of lattice values for each operand that will be either ς_x or ⊥.
     * Progressing through the algorithm means lowering through the lattice following the next rules:
     *      · any ⊓  ⊤  = any
     *      · any ⊓  ⊥  = ⊥
     *      · ς_i ⊓ ς_j = ς_i   , if i = j
     *      · ς_i ⊓ ς_j = ⊥     , if i ≠ j
     *
     */
    void ConditionalConstantAnalysis::conditional_constants_evaluation( ExtensibleGraph* pcfg )
    {
        Node* graph = pcfg->get_graph( );

        // Prepare pcfg
        // - marking as "non-executable" all edges
        mark_pcfg_edges_as_non_executable( graph );
        // - initializing as '⊤' all operands of expressions at all nodes except the Entry node
        init_all_pcfg_operands_lattice( graph, pcfg->get_scope( ), _ipa );

        // Perform analysis
        Node* graph_entry = graph->get_graph_entry_node( );
        _work_list.append( graph_entry );
        ObjectList<Edge*> entry_children = graph_entry->get_exit_edges( );
        for (ObjectList<Edge*>::iterator it = entry_children.begin( ); it != entry_children.end( ); ++it)
        {
            (*it)->set_executable( true );
        }


    }

    void ConditionalConstantAnalysis::constant_propagation( ExtensibleGraph* pcfg )
    {
        // Apply Conditional Constant Propagation algorithm without changing any instruction
        conditional_constants_evaluation ( pcfg );

        // Perform the actual modifications in the AST

    }

    void ConditionalConstantAnalysis::constant_folding( ExtensibleGraph* pcfg )
    {

    }

    void ConditionalConstantAnalysis::conditional_constant_propagation( ObjectList<ExtensibleGraph*> pcfgs )
    {
        for( ObjectList<ExtensibleGraph*>::iterator it = pcfgs.begin( ); it != pcfgs.end( ); ++it)
        {
            conditional_constant_propagation( *it );
        }
    }

    void ConditionalConstantAnalysis::conditional_constant_propagation( ExtensibleGraph* pcfg )
    {
        // Substitute variables by constant values when possible
        constant_propagation( pcfg );

        // Apply constant folding
        constant_folding ( pcfg );
    }

    // **************************** END conditional constant analysis **************************** //
    // ******************************************************************************************* //

}
}