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


#ifndef TL_CFG_VISITOR_HPP
#define TL_CFG_VISITOR_HPP

#include "tl-extensible-graph.hpp"
#include "tl-nodecl.hpp"
#include "tl-nodecl-visitor.hpp"

namespace TL
{
    struct loop_control_nodes {
        Node* init;
        Node* cond;
        Node* next;
        
        loop_control_nodes() 
            : init(NULL), cond(NULL), next(NULL) 
        {}
    };
    
    struct switch_nodes {
        Node* cond;
        Node* case_no_break;
        ObjectList<Node*> cases_break;
        int ncases;
        bool last_case_no_break;
        
        switch_nodes() 
            : cond(NULL), case_no_break(NULL), cases_break(), ncases(-1), last_case_no_break(true)
        {}
        
        void clear()
        {
            cond = NULL;
            case_no_break = NULL;
            cases_break.clear();
            ncases = -1;
            last_case_no_break = true;
        }
    };
    
    struct try_block_nodes {
        ObjectList<Node*> catch_parents;
        int nhandlers;
        
        try_block_nodes()
            : catch_parents(), nhandlers(-1)
        {}
        
        void clear()
        {
            catch_parents.clear();
            nhandlers = -1;
        }
    };
    
    class LIBTL_CLASS CfgVisitor : public Nodecl::NodeclVisitor
    {
    protected:
        ExtensibleGraph* _actual_cfg;
        ScopeLink _sl;
        struct loop_control_nodes _actual_loop_info;
        struct switch_nodes _actual_switch_info;
        struct try_block_nodes _actual_try_info;
        
    private:
        ObjectList<ExtensibleGraph*> _cfgs;
        ObjectList<Nodecl::NodeclBase> _seq_nodecl;
        
        //! This method creates a node from a expression
        /*!
         * \param n Source expression
         * \param connect_node Boolean indicating whether the node must be connected with the last nodes
         *                     created in the graph
         * \return The new node created
         */
        Node* get_expression_node(const Nodecl::NodeclBase& n, bool connect_node = true);
        
        //! This method creates a list with the nodes in an specific subgraph
        /*!
         * \param node First node to be traversed. The method will visit all nodes from here.
         */
        void compute_catch_parents(Node* node);
        
    public:
        CfgVisitor(ScopeLink sl);
        CfgVisitor(const CfgVisitor& visitor);
        
        void unhandled_node(const Nodecl::NodeclBase& n);
        void visit(const Nodecl::TopLevel& n);
        void visit(const Nodecl::FunctionCode& n);
        void visit(const Nodecl::TryBlock& n);
        void visit(const Nodecl::CatchHandler& n);
        void visit(const Nodecl::Throw& n);
        void visit(const Nodecl::CompoundStatement& n);
        void visit(const Nodecl::AnyList& n);
        void visit(const Nodecl::Symbol& n);
        void visit(const Nodecl::ExpressionStatement& n);
        void visit(const Nodecl::ParenthesizedExpression& n);
        void visit(const Nodecl::ErrExpr& n);
        void visit(const Nodecl::ObjectInit& n);
        void visit(const Nodecl::ArraySubscript& n);
        void visit(const Nodecl::ClassMemberAccess& n);
        void visit(const Nodecl::FortranNamedPairSpec& n);
        void visit(const Nodecl::Concat& n);
        void visit(const Nodecl::New& n);
        void visit(const Nodecl::Delete& n);
        void visit(const Nodecl::DeleteArray& n);
        void visit(const Nodecl::Sizeof& n);
        void visit(const Nodecl::Type& n);
        void visit(const Nodecl::Typeid& n);
        void visit(const Nodecl::Cast& n);
        void visit(const Nodecl::Offset& n);
        void visit(const Nodecl::VirtualFunctionCall& n);
        void visit(const Nodecl::FunctionCall& n);
        void visit(const Nodecl::StringLiteral& n);
        void visit(const Nodecl::BooleanLiteral& n);
        void visit(const Nodecl::IntegerLiteral& n);
        void visit(const Nodecl::ComplexLiteral& n);
        void visit(const Nodecl::FloatingLiteral& n);
        void visit(const Nodecl::StructuredLiteral& n);
        void visit(const Nodecl::EmptyStatement& n);
        void visit(const Nodecl::ReturnStatement& n);
        void visit(const Nodecl::BuiltinExpr& n);
        void visit(const Nodecl::BuiltinDecl& n);
        void visit(const Nodecl::PragmaCustomDirective& n);
        void visit(const Nodecl::PragmaCustomConstruct& n);
        void visit(const Nodecl::PragmaCustomClause& n);
        void visit(const Nodecl::PragmaCustomLine& n);
        void visit(const Nodecl::PragmaClauseArg& n);
        void visit(const Nodecl::ForStatement& n);
        void visit(const Nodecl::WhileStatement& n);
        void visit(const Nodecl::IfElseStatement& n);
        void visit(const Nodecl::SwitchStatement& n);
        void visit(const Nodecl::CaseStatement& n);
        void visit(const Nodecl::DefaultStatement& n);
        void visit(const Nodecl::ConditionalExpression& n);
        void visit(const Nodecl::FortranComputedGotoStatement& n);
        void visit(const Nodecl::FortranAssignedGotoStatement& n);
        void visit(const Nodecl::GotoStatement& n);
        void visit(const Nodecl::LabeledStatement& n);
        void visit(const Nodecl::LoopControl& n);
        void visit(const Nodecl::ContinueStatement& n);
        void visit(const Nodecl::BreakStatement& n);
        void visit(const Nodecl::DoStatement& n);
        void visit(const Nodecl::Assignment& n);
        void visit(const Nodecl::AddAssignment& n);
        void visit(const Nodecl::SubAssignment& n);
        void visit(const Nodecl::DivAssignment& n);
        void visit(const Nodecl::MulAssignment& n);
        void visit(const Nodecl::ModAssignment& n);
        void visit(const Nodecl::BitwiseAndAssignment& n);
        void visit(const Nodecl::BitwiseOrAssignment& n);
        void visit(const Nodecl::BitwiseXorAssignment& n);
        void visit(const Nodecl::ShrAssignment& n);
        void visit(const Nodecl::ShlAssignment& n);
        void visit(const Nodecl::Add& n);
        void visit(const Nodecl::Minus& n);
        void visit(const Nodecl::Mul& n);
        void visit(const Nodecl::Div& n);
        void visit(const Nodecl::Mod& n);
        void visit(const Nodecl::Power& n);
        void visit(const Nodecl::LogicalAnd& n);
        void visit(const Nodecl::LogicalOr& n);
        void visit(const Nodecl::BitwiseAnd& n);
        void visit(const Nodecl::BitwiseOr& n);
        void visit(const Nodecl::BitwiseXor& n);
        void visit(const Nodecl::Equal& n);
        void visit(const Nodecl::Different& n);
        void visit(const Nodecl::LowerThan& n);
        void visit(const Nodecl::GreaterThan& n);
        void visit(const Nodecl::LowerOrEqualThan& n);
        void visit(const Nodecl::GreaterOrEqualThan& n);
        void visit(const Nodecl::Shr& n);
        void visit(const Nodecl::Shl& n);
        void visit(const Nodecl::Predecrement& n);
        void visit(const Nodecl::Postdecrement& n);
        void visit(const Nodecl::Preincrement& n);
        void visit(const Nodecl::Postincrement& n);
        void visit(const Nodecl::Plus& n);
        void visit(const Nodecl::Neg& n);     
        void visit(const Nodecl::BitwiseNot& n);
        void visit(const Nodecl::LogicalNot& n);
        void visit(const Nodecl::Derreference& n);
        void visit(const Nodecl::Reference& n);
        void visit(const Nodecl::Text& n);
        void visit(const Nodecl::FortranWhere& n);
        void visit(const Nodecl::FortranWherePair& n);
        void visit(const Nodecl::SubscriptTriplet& n);
        void visit(const Nodecl::FortranLabelAssignStatement& n);
        void visit(const Nodecl::FortranIoSpec& n);
        void visit(const Nodecl::FieldDesignator& n);
        void visit(const Nodecl::IndexDesignator& n);
        void visit(const Nodecl::FortranEquivalence& n);
        void visit(const Nodecl::FortranData& n);
        void visit(const Nodecl::FortranImpliedDo& n);
        void visit(const Nodecl::FortranForall& n);    
        void visit(const Nodecl::FortranArithmeticIfStatement& n);
        void visit(const Nodecl::FortranNullifyStatement& n);
        void visit(const Nodecl::FortranIoStatement& n);  
        void visit(const Nodecl::FortranOpenStatement& n);
        void visit(const Nodecl::FortranCloseStatement& n);
        void visit(const Nodecl::FortranReadStatement& n);
        void visit(const Nodecl::FortranWriteStatement& n);
        void visit(const Nodecl::FortranPrintStatement& n);
        void visit(const Nodecl::FortranStopStatement& n);
        void visit(const Nodecl::FortranAllocateStatement& n);
        void visit(const Nodecl::FortranDeallocateStatement& n);
        void visit(const Nodecl::Comma& n);
    };

    
    //! This visitor traverses Expressions.
    /*!
     * The visitor just figures out whether a expression need to be broken into different nodes.
     * It happens when the expression contains a Function Call or a Conditional Expression.
     */
    class LIBTL_CLASS BreakingExpressionVisitor : public CfgVisitor
    {
        
    public:
        bool _broken_expression;
        int _breakage_type;
        
        BreakingExpressionVisitor(ScopeLink sl);
       
        void visit(const Nodecl::VirtualFunctionCall& n);
        void visit(const Nodecl::FunctionCall& n);
        void visit(const Nodecl::ConditionalExpression& n);
        
//         void visit(const Nodecl::StringLiteral& n);
//         void visit(const Nodecl::BooleanLiteral& n);
//         void visit(const Nodecl::IntegerLiteral& n);
//         void visit(const Nodecl::ComplexLiteral& n);
//         void visit(const Nodecl::FloatingLiteral& n);
//         void visit(const Nodecl::StructuredLiteral& n);
//         void visit(const Nodecl::Symbol& n);
//         void visit(const Nodecl::VirtualFunctionCall& n);
//         void visit(const Nodecl::FunctionCall& n);
//         void visit(const Nodecl::ArraySubscript& n);
//         void visit(const Nodecl::ClassMemberAccess& n);
//         void visit(const Nodecl::Plus& n);
//         void visit(const Nodecl::Neg& n);
//         void visit(const Nodecl::Add& n);
//         void visit(const Nodecl::Minus& n);
//         void visit(const Nodecl::Mul& n);
//         void visit(const Nodecl::Div& n);
//         void visit(const Nodecl::Mod& n);
//         void visit(const Nodecl::Power& n);
//         void visit(const Nodecl::Concat& n);
//         void visit(const Nodecl::Equal& n);
//         void visit(const Nodecl::Different& n);
//         void visit(const Nodecl::LowerThan& n);
//         void visit(const Nodecl::GreaterThan& n);
//         void visit(const Nodecl::LowerOrEqualThan& n);
//         void visit(const Nodecl::GreaterOrEqualThan& n);
//         void visit(const Nodecl::LogicalAnd& n);
//         void visit(const Nodecl::LogicalOr& n);
//         void visit(const Nodecl::LogicalNot& n);
//         void visit(const Nodecl::BitwiseAnd& n);
//         void visit(const Nodecl::BitwiseOr& n);
//         void visit(const Nodecl::BitwiseXor& n);
//         void visit(const Nodecl::BitwiseNot& n);
//         void visit(const Nodecl::Shr& n);
//         void visit(const Nodecl::Shl& n);
//         void visit(const Nodecl::Assignment& n);
//         void visit(const Nodecl::AddAssignment& n);
//         void visit(const Nodecl::SubAssignment& n);
//         void visit(const Nodecl::DivAssignment& n);
//         void visit(const Nodecl::MulAssignment& n);
//         void visit(const Nodecl::ModAssignment& n);        
//         void visit(const Nodecl::BitwiseAndAssignment& n);
//         void visit(const Nodecl::BitwiseOrAssignment& n);
//         void visit(const Nodecl::BitwiseXorAssignment& n);
//         void visit(const Nodecl::ShrAssignment& n);
//         void visit(const Nodecl::ShlAssignment& n);        
//         void visit(const Nodecl::ParenthesizedExpression& n);
//         void visit(const Nodecl::Reference& n);
//         void visit(const Nodecl::Derreference& n);
//         void visit(const Nodecl::Cast& n);
//         void visit(const Nodecl::ConditionalExpression& n);
//         void visit(const Nodecl::Comma& n);
//         void visit(const Nodecl::Throw& n);
//         void visit(const Nodecl::Predecrement& n);
//         void visit(const Nodecl::Postdecrement& n);
//         void visit(const Nodecl::Preincrement& n);
//         void visit(const Nodecl::Postincrement& n);
//         void visit(const Nodecl::Sizeof& n);    
//         void visit(const Nodecl::Typeid& n);
//         void visit(const Nodecl::Offset& n); 
//         void visit(const Nodecl::New& n);
//         void visit(const Nodecl::Delete& n);
//         void visit(const Nodecl::DeleteArray& n);
//         void visit(const Nodecl::CxxRaw& n);
//         void visit(const Nodecl::FortranData& n);
//         void visit(const Nodecl::FortranEquivalence& n);
//         void visit(const Nodecl::ImpliedDo& n);
//         void visit(const Nodecl::ObjectInit& n);
//         void visit(const Nodecl::BuiltinExpr& n);
//         void visit(const Nodecl::Text& n);
//         void visit(const Nodecl::ErrExpr& n);
    }; 
}
#endif  // TL_CFG_VISITOR_HPP

