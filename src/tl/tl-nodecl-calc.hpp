
#ifndef TL_CALCULATOR_HPP
#define TL_CALCULATOR_HPP

#include "cxx-cexpr.h"
#include "tl-nodecl-visitor.hpp"

namespace Nodecl
{
    class LIBTL_CLASS Calculator : public Nodecl::NodeclVisitor<const_value_t*>
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
        Ret visit(const Nodecl::Shr& n);
        Ret visit(const Nodecl::Shl& n);
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
        Ret visit(const Nodecl::Derreference& n);
        Ret visit(const Nodecl::ArraySubscript& n);
        Ret visit(const Nodecl::Cast& n);
        
    public:
        // *** Constructors *** //
        Calculator();
        
        const_value_t* compute_const_value(Nodecl::NodeclBase val);
    };
}

#endif // TL_CALCULATOR_HPP