/*--------------------------------------------------------------------
(C) Copyright 2006-2009 Barcelona Supercomputing Center 
Centro Nacional de Supercomputacion

This file is part of Mercurium C/C++ source-to-source compiler.

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



#ifndef TL_CFG_ANALYSIS_VISITOR_HPP
#define TL_CFG_ANALYSIS_VISITOR_HPP

#include "tl-node.hpp"
#include "tl-nodecl-visitor.hpp"

namespace TL
{
    class LIBTL_CLASS CfgAnalysisVisitor : public Nodecl::ExhaustiveVisitor<void>
    {
    protected:
        //! Pointer to the Node in a CFG of type ExtensibleGraph where the Nodecl is contained
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
         * This attribute stores the actual nodecl when we are traversing class member access, reference/dereference or array subscript
         */
        Nodecl::NodeclBase _actual_nodecl;
        
        //! Nodecl containing the initializing expression of the symbol which is being defined in the actual statement
        /*!
         * For example, within an assignment, this Nodecl contain the rhs of the assignment
         */
        Nodecl::NodeclBase _init_expression;
        
    private:
        
        //! This method implements the visitor for any Binary operation
        /*!
         * \param n Nodecl containing the Binary operation
         */
        template <typename T>
        Ret binary_visit(const T& n);

        //! This method implements the visitor for any Binary Assignment operation
        /*!
         * \param n Nodecl containing the Binary Assignment operation
         */        
        template <typename T>
        Ret binary_assignment(const T& n);
        
        template <typename T>
        Ret unary_visit(const T& n);
        
    public:
        // *** Constructors *** //
        CfgAnalysisVisitor(Node* n);
        CfgAnalysisVisitor(const CfgAnalysisVisitor& v);
        
        // *** Visitors *** //
        Ret unhandled_node(const Nodecl::NodeclBase& n);
        // Basic nodes
        Ret visit(const Nodecl::Symbol& n);  
        Ret visit(const Nodecl::ObjectInit& n);
        // Composed nodes
        Ret visit(const Nodecl::ArraySubscript& n);
        Ret visit(const Nodecl::ClassMemberAccess& n);
        // Assignments
        Ret visit(const Nodecl::Assignment& n);
        Ret visit(const Nodecl::AddAssignment& n);
        Ret visit(const Nodecl::SubAssignment& n);
        Ret visit(const Nodecl::DivAssignment& n);
        Ret visit(const Nodecl::MulAssignment& n);
        Ret visit(const Nodecl::ModAssignment& n);
        Ret visit(const Nodecl::BitwiseAndAssignment& n);
        Ret visit(const Nodecl::BitwiseOrAssignment& n);
        Ret visit(const Nodecl::BitwiseXorAssignment& n);
        Ret visit(const Nodecl::ShrAssignment& n);
        Ret visit(const Nodecl::ShlAssignment& n);
        // Binary nodes
        Ret visit(const Nodecl::Comma& n);
        Ret visit(const Nodecl::Concat& n);
        Ret visit(const Nodecl::Add& n);
        Ret visit(const Nodecl::Minus& n);
        Ret visit(const Nodecl::Mul& n);
        Ret visit(const Nodecl::Div& n);
        Ret visit(const Nodecl::Mod& n);
        Ret visit(const Nodecl::Power& n);
        Ret visit(const Nodecl::LogicalAnd& n);
        Ret visit(const Nodecl::LogicalOr& n);
        Ret visit(const Nodecl::BitwiseAnd& n);
        Ret visit(const Nodecl::BitwiseOr& n);
        Ret visit(const Nodecl::BitwiseXor& n);
        Ret visit(const Nodecl::Shr& n);
        Ret visit(const Nodecl::Shl& n);
        Ret visit(const Nodecl::Equal& n);
        Ret visit(const Nodecl::Different& n);
        Ret visit(const Nodecl::LowerThan& n);
        Ret visit(const Nodecl::GreaterThan& n);
        Ret visit(const Nodecl::LowerOrEqualThan& n);
        Ret visit(const Nodecl::GreaterOrEqualThan& n);        
        // Pre-post increments-decrements
        Ret visit(const Nodecl::Predecrement& n);
        Ret visit(const Nodecl::Postdecrement& n);
        Ret visit(const Nodecl::Preincrement& n);
        Ret visit(const Nodecl::Postincrement& n);
        // Unary nodes
        Ret visit(const Nodecl::Derreference& n);
        Ret visit(const Nodecl::Reference& n);
    };
    

    //! This visitor parses a nodecl searching global variables
    //! All symbols are inserted in the list #global_vars
    class LIBTL_CLASS CfgGlobalVarsVisitor : public Nodecl::ExhaustiveVisitor<void>
    {
    private:
        Scope _sc;
        ObjectList<Symbol> _global_vars;
        
    public:
        
        CfgGlobalVarsVisitor(Scope sc);
        
        Ret visit(const Nodecl::Symbol& n);
        
        ObjectList<Symbol> get_global_variables() const;
    };    
}
    
#endif      // TL_CFG_ANALYSIS_VISITOR_HPP