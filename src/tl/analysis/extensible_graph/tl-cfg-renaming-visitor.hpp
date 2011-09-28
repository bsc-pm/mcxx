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


#ifndef TL_CFG_RENAMING_VISITOR_HPP
#define TL_CFG_RENAMING_VISITOR_HPP

#include <map>

#include "tl-nodecl-visitor.hpp"

namespace TL
{
    //! This class traverses a Nodecl and creates a new Nodecl renaming the symbols inside
    //! depending on the renaming map. 
    /*!
     * The list may contain:
     * - Zero elements: no renames has been performed
     * - More element: renamed nodecls
     */
    class LIBTL_CLASS CfgRenamingVisitor : public Nodecl::NodeclVisitor<Nodecl::NodeclBase>
    {
        private:
            std::map<Symbol, Nodecl::NodeclBase> _rename_map;
            const char* _filename;
            int _line;

            
        public:
            // *** Constructors *** //
            CfgRenamingVisitor(std::map<Symbol, Nodecl::NodeclBase> rename_map, const char* _filename, int _line);
            CfgRenamingVisitor(const CfgRenamingVisitor& sym_v);
           
            
            // *** Visitors *** //
            Ret unhandled_node(const Nodecl::NodeclBase& n);
            Ret visit(const Nodecl::Symbol& n);
            Ret visit(const Nodecl::ExpressionStatement& n);        
            Ret visit(const Nodecl::ParenthesizedExpression& n);
            Ret visit(const Nodecl::ObjectInit& n);
            Ret visit(const Nodecl::Throw& n);
            Ret visit(const Nodecl::ArraySubscript& n);
            Ret visit(const Nodecl::ArraySection& n);
            Ret visit(const Nodecl::ClassMemberAccess& n);    
//             Ret visit(const Nodecl::Concat& n);
//             Ret visit(const Nodecl::New& n);
//             Ret visit(const Nodecl::Delete& n);
//             Ret visit(const Nodecl::DeleteArray& n);
//             Ret visit(const Nodecl::Sizeof& n);
//             Ret visit(const Nodecl::Type& n);
//             Ret visit(const Nodecl::Typeid& n);
//             Ret visit(const Nodecl::Cast& n);
//             Ret visit(const Nodecl::Offset& n);
//             Ret visit(const Nodecl::StringLiteral& n);
//             Ret visit(const Nodecl::BooleanLiteral& n);
//             Ret visit(const Nodecl::IntegerLiteral& n);
//             Ret visit(const Nodecl::ComplexLiteral& n);
//             Ret visit(const Nodecl::FloatingLiteral& n);
//             Ret visit(const Nodecl::StructuredValue& n);
//             Ret visit(const Nodecl::EmptyStatement& n);
//             Ret visit(const Nodecl::ReturnStatement& n);  
//             Ret visit(const Nodecl::GotoStatement& n);
//             Ret visit(const Nodecl::LabeledStatement& n);
//             Ret visit(const Nodecl::ContinueStatement& n);
//             Ret visit(const Nodecl::BreakStatement& n);        
//             Ret visit(const Nodecl::Assignment& n);
//             Ret visit(const Nodecl::AddAssignment& n);
//             Ret visit(const Nodecl::SubAssignment& n);
//             Ret visit(const Nodecl::DivAssignment& n);
//             Ret visit(const Nodecl::MulAssignment& n);
//             Ret visit(const Nodecl::ModAssignment& n);
//             Ret visit(const Nodecl::BitwiseAndAssignment& n);
//             Ret visit(const Nodecl::BitwiseOrAssignment& n);
//             Ret visit(const Nodecl::BitwiseXorAssignment& n);
//             Ret visit(const Nodecl::ShrAssignment& n);
//             Ret visit(const Nodecl::ShlAssignment& n);
//             Ret visit(const Nodecl::Add& n);
//             Ret visit(const Nodecl::Minus& n);
//             Ret visit(const Nodecl::Mul& n);
//             Ret visit(const Nodecl::Div& n);
//             Ret visit(const Nodecl::Mod& n);
//             Ret visit(const Nodecl::Power& n);
//             Ret visit(const Nodecl::LogicalAnd& n);
//             Ret visit(const Nodecl::LogicalOr& n);
//             Ret visit(const Nodecl::BitwiseAnd& n);
//             Ret visit(const Nodecl::BitwiseOr& n);
//             Ret visit(const Nodecl::BitwiseXor& n);
//             Ret visit(const Nodecl::Equal& n);
//             Ret visit(const Nodecl::Different& n);
//             Ret visit(const Nodecl::LowerThan& n);
//             Ret visit(const Nodecl::GreaterThan& n);
//             Ret visit(const Nodecl::LowerOrEqualThan& n);
//             Ret visit(const Nodecl::GreaterOrEqualThan& n);
//             Ret visit(const Nodecl::Shr& n);
//             Ret visit(const Nodecl::Shl& n);
//             Ret visit(const Nodecl::Predecrement& n);
//             Ret visit(const Nodecl::Postdecrement& n);
//             Ret visit(const Nodecl::Preincrement& n);
//             Ret visit(const Nodecl::Postincrement& n);
//             Ret visit(const Nodecl::Plus& n);
//             Ret visit(const Nodecl::Neg& n);     
//             Ret visit(const Nodecl::BitwiseNot& n);
//             Ret visit(const Nodecl::LogicalNot& n);
//             Ret visit(const Nodecl::Derreference& n);
//             Ret visit(const Nodecl::Reference& n);
//             Ret visit(const Nodecl::Text& n);
//             Ret visit(const Nodecl::Comma& n);
//             Ret visit(const Nodecl::Conversion& n);
    };
}

#endif      // TL_CFG_RENAMING_VISITOR_HPP