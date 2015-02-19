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

#ifndef TL_CONSTANTS_ANALYSIS_HPP
#define TL_CONSTANTS_ANALYSIS_HPP

#include <list>

#include "tl-analysis-utils.hpp"
#include "tl-extensible-graph.hpp"
#include "tl-nodecl.hpp"

namespace TL {
namespace Analysis {

    class Node;

    // ******************************************************************************************* //
    // ************************* Class for Constant Propagation Analysis ************************* //

    /*!This class implements analysis and optimizations over constants:
     * - Constants propagation
     * - Constant folding
     * - Unreachable code elimination
     * This is not a IPA analysis
     */
    class LIBTL_CLASS ConditionalConstantAnalysis
    {
    private:

        // ********** Class members ********** //

        //!Boolean that indicates whether the analysis is inter-procedural or not
        bool _ipa;

        //!Temporary list used during Constant Propagation algorithm to store the nodes that must be treated
        ObjectList<Node*> _work_list;

        /*!Method that applies the Conditional Constant algorithm to an Extensible Graph
         * and labels each node with information about the variables and its value in the LatticeCell
         */
        void conditional_constants_evaluation( ExtensibleGraph* pcfg );

    public:
        //! Constructor
        ConditionalConstantAnalysis( bool is_ipa );

        //! Destructor
        ~ConditionalConstantAnalysis( );

        /*!Method that consults LatticeCell values of each node and
         * actually applies changes in the Nodecls.
         * This method requires 'conditional_constants_evaluation' to be applied before
         * otherwise it won't perform any change.
         */
        void constant_propagation( ExtensibleGraph* pcfg );

        //!Method that simplifies constant expressions
        void constant_folding( ExtensibleGraph* pcfg );

        /*!Method that applies Conditional Constant Propagation analysis,
         * substitutes variables by constant values when possible and
         * applies constant folding.
         */
        void conditional_constant_propagation( ExtensibleGraph* pcfg );

        /*!Overloaded method that applies Conditional Constant Propagation analysis
         * over a set of PCFGs separately.
         */
        void conditional_constant_propagation( ObjectList<ExtensibleGraph*> pcfg );
    };

    // *********************** END class for Constant Propagation Analysis *********************** //
    // ******************************************************************************************* //



    // ******************************************************************************************* //
    // ******************** Visitor computing the Extensible Symbols modified ******************** //

    //!Visitor to get the l-values contained in a given nodecl that are modified
    class LIBTL_CLASS AssignedNodeclVisitor : public Nodecl::ExhaustiveVisitor<void>
    {
    private:

        // ******* Class attributes ******* //

        //! List of extended symbols found during the traversal
        ObjectList<Nodecl::NodeclBase> _assigned_ext_syms;

        //! Temporary value used to know whether we are traversing the left-hand side of an assignment
        bool _is_lhs;


        // ******* Private methods ******** //

        void visit_assignment(Nodecl::NodeclBase lhs, Nodecl::NodeclBase rhs);

        void visit_xx_crements(Nodecl::NodeclBase n);

    public:
        //! Constructor
        AssignedNodeclVisitor( );

        //! Getters and setters
        ObjectList<Nodecl::NodeclBase> get_assigned_ext_syms( );

        //! Visiting methods
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
        Ret visit( const Nodecl::DivAssignment& n );
        Ret visit( const Nodecl::MinusAssignment& n );
        Ret visit( const Nodecl::ModAssignment& n );
        Ret visit( const Nodecl::MulAssignment& n );
        Ret visit( const Nodecl::Postdecrement& n );
        Ret visit( const Nodecl::Postincrement& n );
        Ret visit( const Nodecl::Predecrement& n );
        Ret visit( const Nodecl::Preincrement& n );
        Ret visit( const Nodecl::Symbol& n );
    };

    // ****************** END visitor computing the Extensible Symbols modified ****************** //
    // ******************************************************************************************* //



    // ******************************************************************************************* //
    // ********** Class representing the Lattice Cell of Constant Propagation Algorithm ********** //

    enum LatticeValue {
        undetermined_const_val, //! Value is undetermined   =>  ⊤
        constant_val,           //! Value is constant       =>  ς_x
        overdefined_const_val   //! Value is unknown        =>  ⊥
    };

    /*!This class implements the relationship between an entity of the program and its value in the LatticeCell
     * A 'lattice element' represents compile-time knowledge about the value of a given variable.
     * A 'lattice element' can be one of the three types:
     *                        ⊤                        => Variable may be some (as yet) undetermined constant.
     *              /     /   |       \
     *         ς_i    ς_j    ς_k    ...    ς_M         => Specific constant value.
     *              \     \   |       /
     *                        ⊥                        => Constant value can not be guaranteed.
     */
    class LIBTL_CLASS LatticeCellValue {
    private:
        //! Entity of the program
        Nodecl::NodeclBase _ext_sym;

        //! temporary value in the LatticeCell of the entity
        LatticeValue _lattice_val;

        Nodecl::NodeclBase _const_val;

    public:
        /*!Constructor method
         * When a LatticeCell gets the value 'unknown', it can never change again.
         * Because of that fact, the LatticeCellValue is initialized as 'undetermined' by default
         */
        LatticeCellValue( );

        //! Getters and setters
        void set_ext_sym( Nodecl::NodeclBase es );
        void set_lattice_val( LatticeValue lv );
        void set_const_val( Nodecl::NodeclBase cv );
    };

    // ******** END class representing the Lattice Cell of Constant Propagation Algorithm ******** //
    // ******************************************************************************************* //

}
}

#endif      // TL_CONSTANTS_ANALYSIS_HPP