/*--------------------------------------------------------------------
  (C) Copyright 2006-2014 Barcelona Supercomputing Center
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

#ifndef TL_CALCULATOR_HPP
#define TL_CALCULATOR_HPP

#include "cxx-cexpr.h"
#include "tl-nodecl-visitor.hpp"

namespace TL {
namespace Optimizations {
    
    class LIBTL_CLASS Calculator : public Nodecl::NodeclVisitor<TL::ObjectList<const_value_t*> >
    {
    private:
        // *** Visitors *** //
        Ret unhandled_node(const Nodecl::NodeclBase& n);
        Ret visit(const Nodecl::Symbol& n);
        Ret visit(const Nodecl::Conversion& n);
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
        Ret visit(const Nodecl::BitwiseShr& n);
        Ret visit(const Nodecl::ArithmeticShr& n);
        Ret visit(const Nodecl::BitwiseShl& n);
        Ret visit(const Nodecl::Predecrement& n);
        Ret visit(const Nodecl::Postdecrement& n);
        Ret visit(const Nodecl::Preincrement& n);
        Ret visit(const Nodecl::Postincrement& n);
        Ret visit(const Nodecl::Plus& n);
        Ret visit(const Nodecl::Neg& n);     
        Ret visit(const Nodecl::BitwiseNot& n);
        Ret visit(const Nodecl::LogicalNot& n);
        Ret visit(const Nodecl::IntegerLiteral& n);
        Ret visit(const Nodecl::FloatingLiteral& n);
        Ret visit(const Nodecl::ComplexLiteral& n);
        Ret visit(const Nodecl::BooleanLiteral& n);
        Ret visit(const Nodecl::StringLiteral& n);
        Ret visit(const Nodecl::Dereference& n);
        Ret visit(const Nodecl::ArraySubscript& n);
        Ret visit(const Nodecl::ClassMemberAccess& n);
        Ret visit(const Nodecl::FunctionCall& n);
        Ret visit(const Nodecl::VirtualFunctionCall& n);
        Ret visit(const Nodecl::Equal& n);
        Ret visit(const Nodecl::Different& n);
        Ret visit(const Nodecl::LowerThan& n);
        Ret visit(const Nodecl::LowerOrEqualThan& n);
        Ret visit(const Nodecl::GreaterThan& n);
        Ret visit(const Nodecl::GreaterOrEqualThan& n);
        Ret visit(const Nodecl::ConditionalExpression& n);
        Ret visit(const Nodecl::Assignment& n);
        Ret visit(const Nodecl::Sizeof& n);
        Ret visit(const Nodecl::VectorAdd& n);
        Ret visit(const Nodecl::VectorMinus& n);
        Ret visit(const Nodecl::VectorMul& n);
        Ret visit(const Nodecl::VectorDiv& n);
        Ret visit(const Nodecl::VectorMod& n);
        Ret visit(const Nodecl::VectorLiteral& n);
        Ret visit(const Nodecl::VectorPromotion& n);
        Ret visit(const Nodecl::Analysis::PlusInfinity& n);
        Ret visit(const Nodecl::Analysis::MinusInfinity& n);

    public:
        // *** Constructors *** //
        Calculator();
        
        const_value_t* compute_const_value(Nodecl::NodeclBase val);
    };
    
}
}

#endif // TL_CALCULATOR_HPP
