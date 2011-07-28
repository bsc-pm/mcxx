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
    class LIBTL_CLASS CfgVisitor : public Nodecl::NodeclVisitor
    {
    protected:
        ExtensibleGraph _actual_cfg;
        ScopeLink _sl;
        
    private:
        ObjectList<ExtensibleGraph> _cfgs;
        ObjectList<Nodecl::NodeclBase> _seq_nodecl;
        
    public:
        CfgVisitor(ScopeLink sl);
        
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
        void visit(const Nodecl::NamedPairSpec& n);
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
        void visit(const Nodecl::ComputedGotoStatement& n);
        void visit(const Nodecl::AssignedGotoStatement& n);
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
        void visit(const Nodecl::Where& n);
        void visit(const Nodecl::WherePair& n);
        void visit(const Nodecl::SubscriptTriplet& n);
        void visit(const Nodecl::LabelAssignStatement& n);
        void visit(const Nodecl::FortranIoSpec& n);
        void visit(const Nodecl::FieldDesignator& n);
        void visit(const Nodecl::IndexDesignator& n);
        void visit(const Nodecl::FortranEquivalence& n);
        void visit(const Nodecl::FortranData& n);
        void visit(const Nodecl::ImpliedDo& n);
        void visit(const Nodecl::Forall& n);    
        void visit(const Nodecl::ArithmeticIfStatement& n);
        void visit(const Nodecl::NullifyStatement& n);
        void visit(const Nodecl::IoStatement& n);  
        void visit(const Nodecl::OpenStatement& n);
        void visit(const Nodecl::CloseStatement& n);
        void visit(const Nodecl::ReadStatement& n);
        void visit(const Nodecl::WriteStatement& n);
        void visit(const Nodecl::PrintStatement& n);
        void visit(const Nodecl::StopStatement& n);
        void visit(const Nodecl::AllocateStatement& n);
        void visit(const Nodecl::DeallocateStatement& n);
        void visit(const Nodecl::CxxRaw& n);
        void visit(const Nodecl::Comma& n);
    };


    class LIBTL_CLASS LoopNextVisitor : public CfgVisitor
    {
    private:
        ObjectList<Nodecl::NodeclBase> _next_nodecls;
        
    public:
        LoopNextVisitor(ExtensibleGraph egraph, ScopeLink sl);
        
//         void visit(const Nodecl::StringLiteral& n);
//         void visit(const Nodecl::BooleanLiteral& n);
//         void visit(const Nodecl::IntegerLiteral& n);
//         void visit(const Nodecl::ComplexLiteral& n);
//         void visit(const Nodecl::FloatingLiteral& n);
//         void visit(const Nodecl::StructuredLiteral& n);
//         void visit(const Nodecl::Symbol& n);
        void visit(const Nodecl::ExpressionStatement& n);
        void visit(const Nodecl::VirtualFunctionCall& n);
        void visit(const Nodecl::FunctionCall& n);
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