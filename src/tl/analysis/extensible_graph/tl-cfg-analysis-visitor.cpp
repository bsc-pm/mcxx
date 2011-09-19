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


#include "cxx-codegen.h"
#include "cxx-process.h"

#include "tl-cfg-analysis-visitor.hpp"

namespace TL
{
    CfgAnalysisVisitor::CfgAnalysisVisitor(Node* n)
        : _node(n)
    {}

    CfgAnalysisVisitor::CfgAnalysisVisitor(const CfgAnalysisVisitor& v)
    {
        _node = v._node;
    }

    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::unhandled_node(const Nodecl::NodeclBase& n)
    {
        std::cerr << "Unhandled node during CFG Analysis'" << c_cxx_codegen_to_str(n.get_internal_nodecl())
                  << "' of type '" << ast_print_node_type(n.get_kind()) << "'" << std::endl;
    }

    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Symbol& n)
    {
        _node->fill_use_def_sets(n.get_symbol(), _define);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Text& n)
    {   // do nothing
    }
    
    template <typename T>
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::literal_visit(const T& n)
    {   // do nothing
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::StringLiteral& n)
    {
        literal_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::BooleanLiteral& n)
    {
        literal_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::IntegerLiteral& n)
    {
        literal_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::ComplexLiteral& n)
    {
        literal_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::FloatingLiteral& n)
    {
        literal_visit(n);
    }

    // TODO
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::VirtualFunctionCall& n)
    {
        unhandled_node(n);
    }
    
    // TODO
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::FunctionCall& n)
    {
        Type t = n.get_type();
        
    }

    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::ObjectInit& n)
    {
        _node->fill_use_def_sets(n.get_symbol(), true);
        _define = false;
        walk(n.get_init_expr());
    }

    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Assignment& n)
    {
        _define = true;
        walk(n.get_lhs());
        _define = false;
        walk(n.get_rhs());
    }
    
    template <typename T>
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::binary_assignment(const T& n)
    {
        _define = false;
        walk(n.get_lhs());
        _define = true;
        walk(n.get_lhs());
        _define = false;
        walk(n.get_rhs());
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::AddAssignment& n)
    {
        binary_assignment(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::SubAssignment& n)
    {
        binary_assignment(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::DivAssignment& n)
    {
        binary_assignment(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::MulAssignment& n)
    {
        binary_assignment(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::ModAssignment& n)
    {
        binary_assignment(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::BitwiseAndAssignment& n)
    {
        binary_assignment(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::BitwiseOrAssignment& n)
    {
        binary_assignment(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::BitwiseXorAssignment& n)
    {
        binary_assignment(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::ShrAssignment& n)
    {
        binary_assignment(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::ShlAssignment& n)
    {
        binary_assignment(n);
    }
        
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Throw& n)
    {
        _define = false;
        walk(n.get_rhs());
    }
    
    template<typename T>
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::nested_visit(const T& n)
    {
        _define = false;
        walk(n.get_nest());
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::ExpressionStatement& n)
    {
        nested_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::ParenthesizedExpression& n)
    {
        unhandled_node(n);
//         nested_visit(n);
    }

    template <typename T>
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::binary_visit(const T& n)
    {
        walk(n.get_lhs());
        walk(n.get_rhs());
    }

    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Comma& n)
    {
        binary_visit(n);
    }

    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Concat& n)
    {
        binary_visit(n);
    }

    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Add& n)
    {
        binary_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Minus& n)
    {
        binary_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Mul& n)
    {
        binary_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Div& n)
    {
        binary_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Mod& n)
    {
        binary_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Power& n)
    {
        binary_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::LogicalAnd& n)
    {
        binary_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::LogicalOr& n)
    {
        binary_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::BitwiseAnd& n)
    {
        binary_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::BitwiseOr& n)
    {
        binary_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::BitwiseXor& n)
    {
        binary_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Equal& n)
    {
        binary_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Different& n)
    {
        binary_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::LowerThan& n)
    {
        binary_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::GreaterThan& n)
    {
        binary_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::LowerOrEqualThan& n)
    {
        binary_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::GreaterOrEqualThan& n)
    {
        binary_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Shr& n)
    {
        binary_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Shl& n)
    {
        binary_visit(n);
    }
    
    template <typename T>
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::unary_visit(const T& n)
    {
        walk(n.get_rhs());
    }    
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Predecrement& n)
    {
        unary_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Postdecrement& n)
    {
        unary_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Preincrement& n)
    {
        unary_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Postincrement& n)
    {
        unary_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Plus& n)
    {
        unary_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Neg& n)
    {
        unary_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::BitwiseNot& n)
    {
        unary_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::LogicalNot& n)
    {
        unary_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Derreference& n)
    {
        unary_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Reference& n)
    {
        unary_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::ArraySubscript& n)
    {
        walk(n.get_subscripted());
        walk(n.get_subscripts());
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::ClassMemberAccess& n)
    {
        walk(n.get_lhs());
        walk(n.get_member());
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::New& n)
    {   // do nothing
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Delete& n)
    {   // FIXME We should specify the object destruction
        // walk(n.get_rhs());
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::DeleteArray& n)
    {   // FIXME We should specify the object destruction
        // walk(n.get_rhs());
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Sizeof& n)
    {   // do nothing
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Type& n)
    {   // do nothing
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Typeid& n)
    {   // do nothing
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Cast& n)
    {
        unhandled_node(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Offset& n)
    {   // do nothing
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::StructuredValue& n)
    {
        walk(n.get_items());
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::EmptyStatement& n)
    {   // do nothing
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::ReturnStatement& n)
    {
        _define = false;
        walk(n.get_value());
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::GotoStatement& n)
    {   // do nothing
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::LabeledStatement& n)
    {
        _define = false;
        walk(n.get_statement());
    }
}
