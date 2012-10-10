/*--------------------------------------------------------------------
  (C) Copyright 2006-2012 Barcelona Supercomputing Center
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

#ifndef TL_IV_ANALYSIS_HPP
#define TL_IV_ANALYSIS_HPP

#include "tl-extensible-graph.hpp"
#include "tl-induction-variables-data.hpp"
#include "tl-nodecl.hpp"
#include "tl-nodecl-utils.hpp"
#include "tl-nodecl-visitor.hpp"
#include "tl-symbol.hpp"

namespace TL {
namespace Analysis {

    // ********************************************************************************************* //
    // ************************** Class for induction variables analysis *************************** //

    class LIBTL_CLASS InductionVariableAnalysis : public Nodecl::ExhaustiveVisitor<bool> {
    private:

        // Variables for Induction Variables analysis
        Utils::InductionVarsPerNode _induction_vars;
        ExtensibleGraph* _graph;

        // Variables for modified Nodecl visitor
        Nodecl::NodeclBase _constant;           /*!< Nodecl to be checked of being constant */
        bool _defining;                         /*!< Boolean used during the visit indicating whether we are in a defining context */

        //! Recursive method that actually computes the induction variables of \_graph
        void compute_induction_variables_rec( Node* current );

        void detect_basic_induction_variables(Node* node, Node* loop);
        void detect_derived_induction_variables( Node* node, Node* loop );

        bool is_there_unique_definition_in_loop( Nodecl::NodeclBase iv_st, Node* iv_node, Node* loop );
        bool is_there_definition_in_loop_(Nodecl::NodeclBase iv_st, Node* iv_node, Node* current, Node* loop );

        //! This method returns true when \iv is defined more than once in the loop
        bool is_false_induction_variable( Nodecl::NodeclBase iv, Nodecl::NodeclBase stmt, Node* node, int id_end );

        //! This method is overloaded to deal with graph visits
        bool is_false_induction_variable_( Nodecl::NodeclBase iv, Nodecl::NodeclBase stmt, Node* node, int id_end );

        bool only_definition_is_in_loop(Nodecl::NodeclBase family, Nodecl::NodeclBase iv_st, Node* iv_node, Node* loop);
        bool only_definition_is_in_loop( Nodecl::NodeclBase iv_st, Node* iv_node, Node* loop );

        /*!Deletes those induction variables included in the list during a previous traverse through the loop control
         * that are redefined within the loop
         * \param node Node in the graph we are analysing
         * \param loop_node Outer loop node where is contained the node we are checking
         */
        void delete_false_induction_vars( Node* node, Node* loop_node );

        bool is_loop_invariant_rec( Node* node, int id_end );

        // ********** Modified Symbol Visitor ********** //
        //!The Visitor returns true in case the symbol is modified, and false otherwise
        bool visit_assignment( Nodecl::NodeclBase lhs, Nodecl::NodeclBase rhs );
        bool visit_function( Symbol func_sym, ObjectList<Type> param_types, Nodecl::List arguments );
        Ret visit( const Nodecl::AddAssignment& n );
        Ret visit( const Nodecl::ArithmeticShrAssignment& n );
        Ret visit( const Nodecl::ArraySubscript& n );
        Ret visit( const Nodecl::Assignment& n );
        Ret visit( const Nodecl::BitwiseAndAssignment& n );
        Ret visit( const Nodecl::BitwiseOrAssignment& n );
        Ret visit( const Nodecl::BitwiseShlAssignment& n );
        Ret visit( const Nodecl::BitwiseShrAssignment& n );
        Ret visit( const Nodecl::BitwiseXorAssignment& n );
        Ret visit( const Nodecl::ClassMemberAccess& n );
        Ret visit( const Nodecl::Dereference& n );
        Ret visit( const Nodecl::DivAssignment& n );
        Ret visit( const Nodecl::FunctionCall& n );
        Ret visit( const Nodecl::MinusAssignment& n );
        Ret visit( const Nodecl::ModAssignment& n );
        Ret visit( const Nodecl::MulAssignment& n );
        Ret visit( const Nodecl::Symbol& n );
        Ret visit( const Nodecl::VirtualFunctionCall& n );
        bool join_list( TL::ObjectList<bool>& list );

    public:

        // **** Constructor **** //

        InductionVariableAnalysis( ExtensibleGraph* graph );

        // **** Induction Variables analysis methods **** //

        void compute_induction_variables( );

        Nodecl::NodeclBase is_basic_induction_variable( Nodecl::NodeclBase st, Node* loop );

        Nodecl::NodeclBase is_derived_induction_variable( Nodecl::NodeclBase st, Node* current,
                                                          Node* loop, Nodecl::NodeclBase& family );

        bool is_loop_invariant( Node* node, int id_end );

        bool induction_vars_l_contains_symbol( Node*, Symbol s ) const;

        std::map<Symbol, Nodecl::NodeclBase> get_induction_vars_mapping( Node* loop_node ) const;

        Utils::InductionVarsPerNode get_all_induction_vars( ) const;


        // ******************* Utils ******************* //

        void print_induction_variables( Node* node );
    };

    // ************************ END class for induction variables analysis ************************* //
    // ********************************************************************************************* //



    // ********************************************************************************************* //
    // **************** Visitor matching nodecls for induction variables analysis ****************** //
    //! Returns true when the Nodecl being visited contains or is equal to \_node_to_find

    class LIBTL_CLASS MatchingVisitor : public Nodecl::ExhaustiveVisitor<bool>
    {
    private:
        Nodecl::NodeclBase _node_to_find;

    public:
        MatchingVisitor( Nodecl::NodeclBase nodecl );
        Ret visit( const Nodecl::Symbol& n );
        Ret visit( const Nodecl::ArraySubscript& n );
        Ret visit( const Nodecl::ClassMemberAccess& n );
        bool join_list( TL::ObjectList<bool>& list );
    };

    // ************** END visitor matching nodecls for induction variables analysis **************** //
    // ********************************************************************************************* //

}
}

#endif          // TL_IV_ANALYSIS_HPP