/*--------------------------------------------------------------------
 ( C) Copyright 2006-2012 Barcelona* Supercomputing Center
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

#ifndef TL_USE_DEF_HPP
#define TL_USE_DEF_HPP


#include "tl-extensible-graph.hpp"
#include "tl-nodecl-calc.hpp"
#include "tl-nodecl-visitor.hpp"

namespace TL {
namespace Analysis {

    // **************************************************************************************************** //
    // **************************** Class implementing use-definition analysis **************************** //

    //! Class implementing Use-Def Analysis
    class LIBTL_CLASS UseDef
    {
    private:
        //!Graph we are analyzing the usage which
        ExtensibleGraph* _graph;

        /*!Method that computes recursively the Use-Definition information from a node
         * \param current Node from which the method begins the computation
         * \param ipa Boolean indicating the Use-Def is only for global variables and referenced parameters
         * \param ipa_arguments List of Nodecl which are reference arguments in an IPA call
         */
        void compute_usage_rec( Node* current, ObjectList<TL::Symbol>& visited_functions,
                                ObjectList<Utils::ExtendedSymbolUsage>& visited_global_vars,
                                bool ipa, Utils::nodecl_set ipa_arguments );

        /*!Recursive method that returns a list with three elements:
         * - The first is the list of upper exposed variables of the graph node;
         * - The second is the list of killed variables of the graph node
         * - The third is the list of undefined variables of the graph
         */
        ObjectList<Utils::ext_sym_set> get_use_def_over_nodes( Node* current );

        //!Propagate the Use-Def information from inner nodes to outer nodes
        void set_graph_node_use_def( Node* graph_node );

    public:
        //! Constructor
        UseDef( ExtensibleGraph* graph );

        /*! Method computing the Use-Definition information on the member #graph
         * \param ipa Boolean indicating the Use-Def is only for global variables and referenced parameters
         * \param ipa_arguments List of Nodecl which are reference arguments in an IPA call
         *                      Only necessary when \ipa is true
         */
        void compute_usage( ObjectList<TL::Symbol> visited_functions,
                            ObjectList<Utils::ExtendedSymbolUsage> visited_global_vars,
                            bool ipa = false, Utils::nodecl_set ipa_arguments = Utils::nodecl_set( )
);
    };

    // ************************** End class implementing use-definition analysis ************************** //
    // **************************************************************************************************** //



    // **************************************************************************************************** //
    // ***************************** Class implementing use-definition visitor **************************** //

    //! This Class implements a Visitor that computes the Use-Definition information of a concrete statement
    //! and attaches this information to the Node in a PCFG which the statements belongs to
    class LIBTL_CLASS UsageVisitor : public Nodecl::ExhaustiveVisitor<void>
    {
    private:

        // *********************** Private members *********************** //

        //! Pointer to the Node in a PCFG where the Nodecl is contained
        //! The results of the analysis performed during the visit will be attached to the node
        Node* _node;

        //! State of the traversal
        /*!
         * This value will be true when the actual expression is a defined value
         * Otherwise, when the value is just used, the value will be false
         * By default this value will be false. Each time we change the value to true for any definition,
         * at the end of the recursion, we turn back the value to false
         */
        bool _define;

        //! Nodecl we are traversing actually
        /*!
         * This attribute stores the actual nodecl when we are traversing class member access.
         * It can be of type reference, dereference or array subscript
         */
        Nodecl::NodeclBase _actual_nodecl;

        //!List of functions visited
        ObjectList<Symbol> _visited_functions;

        //! List of global variables appeared until certain point of the analysis
        ObjectList<Utils::ExtendedSymbolUsage> _visited_global_vars;

        // *** Members for the IPA analysis *** //

        /*!Boolean indicating whether the Use-Def analysis must be IPA or not
         * IPA means that we only care about global variables and referenced parameters
         */
        bool _ipa;

        //! Scope used while IPA to know whether a variable is global or local
        Scope _sc;

        //! List of arguments passed by reference or with pointer type
        Utils::nodecl_set _ipa_arguments;


        // *********************** Private methods *********************** //

        //! This method implements the visitor for any Binary operation
        Ret binary_visit( Nodecl::NodeclBase lhs, Nodecl::NodeclBase rhs );

        //! This method implements the visitor for any Binary Assignment operation
        Ret binary_assignment_visit( Nodecl::NodeclBase lhs, Nodecl::NodeclBase rhs );

        //! This method implements the visitor for any Unary operation
        Ret unary_visit( Nodecl::NodeclBase rhs );

        void function_visit( Nodecl::NodeclBase called_sym, Nodecl::NodeclBase arguments );

        //!Prevents copy construction.
        UsageVisitor( const UsageVisitor& v );

    public:
        // *** Constructors *** //
        UsageVisitor( Node* n,
                      ObjectList<Symbol> visited_functions,
                      ObjectList<Utils::ExtendedSymbolUsage> visited_global_vars,
                      bool ipa, Scope sc, Utils::nodecl_set ipa_arguments = Utils::nodecl_set( ) );

        // *** Getters and Setters *** //
        ObjectList<Symbol> get_visited_functions( ) const;
        ObjectList<Utils::ExtendedSymbolUsage> get_visited_global_variables( ) const;

        // *** Utils *** //
        bool variable_is_in_context( Nodecl::NodeclBase var );

        // *** Modifiers *** //
        void compute_statement_usage( Nodecl::NodeclBase st );

        // *** Visitors *** //
        Ret unhandled_node( const Nodecl::NodeclBase& n );
        Ret visit( const Nodecl::ArithmeticShrAssignment& n );
        Ret visit_pre( const Nodecl::ArraySubscript& n );
        Ret visit( const Nodecl::ArraySubscript& n );
        Ret visit( const Nodecl::Assignment& n );
        Ret visit( const Nodecl::BitwiseAndAssignment& n );
        Ret visit( const Nodecl::BitwiseOrAssignment& n );
        Ret visit( const Nodecl::BitwiseShlAssignment& n );
        Ret visit( const Nodecl::BitwiseShrAssignment& n );
        Ret visit( const Nodecl::BitwiseXorAssignment& n );
        Ret visit_pre( const Nodecl::ClassMemberAccess& n );
        Ret visit( const Nodecl::ClassMemberAccess& n );
        Ret visit_pre( const Nodecl::Dereference& n );
        Ret visit( const Nodecl::Dereference& n );
        Ret visit( const Nodecl::DivAssignment& n );
        Ret visit( const Nodecl::FunctionCall& n );
        Ret visit( const Nodecl::MinusAssignment& n );
        Ret visit( const Nodecl::ModAssignment& n );
        Ret visit( const Nodecl::MulAssignment& n );
        Ret visit( const Nodecl::ObjectInit& n );
        Ret visit( const Nodecl::PointerToMember& n );
        Ret visit( const Nodecl::Postdecrement& n );
        Ret visit( const Nodecl::Postincrement& n );
        Ret visit( const Nodecl::Predecrement& n );
        Ret visit( const Nodecl::Preincrement& n );
        Ret visit( const Nodecl::Range& n );
        Ret visit_pre( const Nodecl::Reference& n );
        Ret visit( const Nodecl::Reference& n );
        Ret visit( const Nodecl::Symbol& n );
        Ret visit( const Nodecl::VirtualFunctionCall& n );
    };

    // *************************** End class implementing use-definition visitor ************************** //
    // **************************************************************************************************** //
}
}

#endif      // TL_USE_DEF_HPP