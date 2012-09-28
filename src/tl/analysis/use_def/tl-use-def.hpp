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
        ExtensibleGraph* _graph;

        void compute_usage_rec( Node* current );

    public:
        //! Constructor
        UseDef( ExtensibleGraph* graph );

        //! Sets the variable represented by a symbol as a killed or an upper exposed variable
        //! depending on @defined attribute
        /*!
            * A variable is killed when it is defined or redefined
            * A variable is upper exposed when it is used before of being killed
            * \param defined Action performed over the symbol: 1 if defined, 0 if not
            * \param n Nodecl containing the whole expression about the use/definition
            */
        void fill_use_def_sets( Nodecl::NodeclBase n, bool defined );

        //! Wrapper method for #fill_use_def_sets when there is more than one symbol to be analyzed
        void fill_use_def_sets( Nodecl::List n_l, bool defined );

        //! Returns a list with two elements. The firs is the list of upper exposed variables of the graph node;
        //! The second is the list of killed variables of the graph node (Used in composite nodes)
        ObjectList<Utils::ext_sym_set> get_use_def_over_nodes( Node* current );

        //!Propagate the Use-Def information from inner nodes to outer nodes
        void set_graph_node_use_def( Node* graph_node );

        // *** Analysis methods *** //

        void compute_usage( );
    };

    // ************************** End class implementing use-definition analysis ************************** //
    // **************************************************************************************************** //



    // **************************************************************************************************** //
    // ***************************** Class implementing use-definition visitor **************************** //

    //! This Class implements a Visitor that computes the Use-Definition information of a concrete statement
    //! and attaches this information to the Node in a PCFG which the statements belongs to
    class LIBTL_CLASS UsageVisitor : public Nodecl::ExhaustiveVisitor<void>
    {
    protected:
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

    private:

        //! This method implements the visitor for any Binary operation
        Ret binary_visit( Nodecl::NodeclBase lhs, Nodecl::NodeclBase rhs );

        //! This method implements the visitor for any Binary Assignment operation
        Ret binary_assignment_visit( Nodecl::NodeclBase lhs, Nodecl::NodeclBase rhs );

        //! This method implements the visitor for any Unary operation
        Ret unary_visit( Nodecl::NodeclBase rhs );

        void function_visit( Nodecl::NodeclBase called_func );

        //!Prevents copy construction.
        UsageVisitor( const UsageVisitor& v );

    public:
        // *** Constructors *** //
        UsageVisitor( Node* n );

        // *** Visitors *** //
        Ret unhandled_node( const Nodecl::NodeclBase& n );

        Ret visit( const Nodecl::Add& n );
        Ret visit( const Nodecl::AddAssignment& n );
        Ret visit( const Nodecl::ArithmeticShr& n );
        Ret visit( const Nodecl::ArithmeticShrAssignment& n );
        Ret visit( const Nodecl::ArraySubscript& n );
        Ret visit( const Nodecl::Assignment& n );
        Ret visit( const Nodecl::BitwiseAnd& n );
        Ret visit( const Nodecl::BitwiseAndAssignment& n );
        Ret visit( const Nodecl::BitwiseOr& n );
        Ret visit( const Nodecl::BitwiseOrAssignment& n );
        Ret visit( const Nodecl::BitwiseShl& n );
        Ret visit( const Nodecl::BitwiseShlAssignment& n );
        Ret visit( const Nodecl::BitwiseShr& n );
        Ret visit( const Nodecl::BitwiseShrAssignment& n );
        Ret visit( const Nodecl::BitwiseXor& n );
        Ret visit( const Nodecl::BitwiseXorAssignment& n );
        Ret visit( const Nodecl::BooleanLiteral& n );
        Ret visit( const Nodecl::Cast& n );
        Ret visit( const Nodecl::ClassMemberAccess& n );
        Ret visit( const Nodecl::Comma& n );
        Ret visit( const Nodecl::ComplexLiteral& n );
        Ret visit( const Nodecl::Concat& n );
        Ret visit( const Nodecl::Dereference& n );
        Ret visit( const Nodecl::Different& n );
        Ret visit( const Nodecl::Div& n );
        Ret visit( const Nodecl::DivAssignment& n );
        Ret visit( const Nodecl::EmptyStatement& n );
        Ret visit( const Nodecl::Equal& n );
        Ret visit( const Nodecl::FloatingLiteral& n );
        Ret visit( const Nodecl::FunctionCall& n );
        Ret visit( const Nodecl::GreaterOrEqualThan& n );
        Ret visit( const Nodecl::GreaterThan& n );
        Ret visit( const Nodecl::IntegerLiteral& n );
        Ret visit( const Nodecl::LogicalAnd& n );
        Ret visit( const Nodecl::LogicalNot& n );
        Ret visit( const Nodecl::LogicalOr& n );
        Ret visit( const Nodecl::LowerOrEqualThan& n );
        Ret visit( const Nodecl::LowerThan& n );
        Ret visit( const Nodecl::Minus& n );
        Ret visit( const Nodecl::MinusAssignment& n );
        Ret visit( const Nodecl::Mod& n );
        Ret visit( const Nodecl::ModAssignment& n );
        Ret visit( const Nodecl::Mul& n );
        Ret visit( const Nodecl::MulAssignment& n );
        Ret visit( const Nodecl::Neg& n );
        Ret visit( const Nodecl::ObjectInit& n );
        Ret visit( const Nodecl::Offset& n );
        Ret visit( const Nodecl::Offsetof& n );
        Ret visit( const Nodecl::Plus& n );
        Ret visit( const Nodecl::PointerToMember& n );
        Ret visit( const Nodecl::Postdecrement& n );
        Ret visit( const Nodecl::Postincrement& n );
        Ret visit( const Nodecl::Power& n );
        Ret visit( const Nodecl::Predecrement& n );
        Ret visit( const Nodecl::Preincrement& n );
        Ret visit( const Nodecl::Range& n );
        Ret visit( const Nodecl::Reference& n );
        Ret visit( const Nodecl::ReturnStatement& n );
        Ret visit( const Nodecl::Sizeof& n );
        Ret visit( const Nodecl::StringLiteral& n );
        Ret visit( const Nodecl::Symbol& n );
        Ret visit( const Nodecl::VirtualFunctionCall& n );
    };

    // *************************** End class implementing use-definition visitor ************************** //
    // **************************************************************************************************** //
}
}

#endif      // TL_USE_DEF_HPP