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

#ifndef TL_REACHING_DEFINITIONS_HPP
#define TL_REACHING_DEFINITIONS_HPP

#include "tl-extensible-graph.hpp"
#include "tl-nodecl-visitor.hpp"

namespace TL {
namespace Analysis {

    // **************************************************************************************************** //
    // ************************** Class implementing reaching definition analysis ************************* //

    //! Class implementing Reaching Definitions Analysis
    class LIBTL_CLASS ReachingDefinitions
    {
    private:
        ExtensibleGraph* _graph;
        Node* _first_stmt_node;

        void generate_unknown_reaching_definitions( );
        
        //!Computes the reaching definitions of each node regarding only its inner statements
        //!Reach Out (X) = Gen (X)
        void gather_reaching_definitions_initial_information( Node* current );

        //!Computes reaching definition equations for a given node and calls recursively to its children
        /*!
         * Reach in (X) = Union of all Reach Out (Y), for all Y predecessors of X
         * Reach out (X) = Gen (X) + ( Reach In (X) - Killed (X) )
         */
        void solve_reaching_definition_equations( Node* current );
        void solve_reaching_definition_equations_rec( Node* current, bool& changed );

        //! Propagates reaching definitions information from inner to outer nodes
        void set_graph_node_reaching_definitions( Node* current );

        NodeclMap combine_generated_statements(Node* current);

    public:
        //! Constructor
        ReachingDefinitions( ExtensibleGraph* graph );

        //! Method computing the Reaching Definitions on the member #graph
        void compute_reaching_definitions( );
    };

    // *********************** End class implementing reaching definitions analysis *********************** //
    // **************************************************************************************************** //



    // **************************************************************************************************** //
    // ************************ Class implementing a visitor of reaching definition *********************** //

    class LIBTL_CLASS GeneratedStatementsVisitor : public Nodecl::ExhaustiveVisitor< void >
    {
    private:
        /*!Set of statements that are generated in a given node
         * The #gen set is used to compute the Reach Out Definitions of a given Node.
         * Thus, #gen set must contain only downwards exposed definition.
         * Since the statements within a node are parsed in sequential order,
         * we will only store the last generation for a given variable.
         * Example:
         *      Node (X) :=  v = u + w;
         *                   v = w;
         *      Gen (X)  :=  v = w;
         */
        NodeclMap _gen;

    public:

        //! Constructor
        GeneratedStatementsVisitor( );

        // **************** Getters and setters *************** //
        NodeclMap get_gen();

        // ***************** Visiting methods ***************** //
        void visit_assignment( const Nodecl::NodeclBase& lhs, const Nodecl::NodeclBase& rhs, 
                               const Nodecl::NodeclBase& stmt );

        Ret visit( const Nodecl::AddAssignment& n );
        Ret visit( const Nodecl::ArithmeticShrAssignment& n );
        Ret visit( const Nodecl::Assignment& n );
        Ret visit( const Nodecl::BitwiseAndAssignment& n );
        Ret visit( const Nodecl::BitwiseOrAssignment& n );
        Ret visit( const Nodecl::BitwiseShlAssignment& n );
        Ret visit( const Nodecl::BitwiseShrAssignment& n );
        Ret visit( const Nodecl::BitwiseXorAssignment& n );
        Ret visit( const Nodecl::DivAssignment& n );
        Ret visit( const Nodecl::MinusAssignment& n );
        Ret visit( const Nodecl::ModAssignment& n );
        Ret visit( const Nodecl::MulAssignment& n );
        Ret visit( const Nodecl::ObjectInit& n );
        Ret visit( const Nodecl::Postdecrement& n );
        Ret visit( const Nodecl::Postincrement& n );
        Ret visit( const Nodecl::Predecrement& n );
        Ret visit( const Nodecl::Preincrement& n );
        Ret visit(const Nodecl::VectorAssignment& n);
        Ret visit(const Nodecl::VectorMaskAssignment& n);
    };

    // ********************** END class implementing a visitor of reaching definition ********************* //
    // **************************************************************************************************** //

}
}

#endif      // TL_REACHING_DEFINITIONS_HPP
