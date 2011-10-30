#include "cxx-codegen.h"

#include "tl-nodecl-calc.hpp"

namespace Nodecl
{
    Calculator::Calculator()
    {}
 
    const_value_t* Calculator::compute_const_value(Nodecl::NodeclBase val)
    {
        TL::ObjectList<const_value_t*> const_val = walk(val);
        if (const_val.empty())
            return NULL;
        else
            return const_val[0];
    }
 
    Calculator::Ret Calculator::unhandled_node(const Nodecl::NodeclBase& n)
    {
        Nodecl::NodeclBase unhandled_n = n;
        std::cerr << "Unhandled node type " << ast_print_node_type(n.get_kind()) << "'"
                  << " while calculating constant value of expression '" << unhandled_n.prettyprint() << std::endl;
        return Ret();
    }
    
    Calculator::Ret Calculator::visit(const Nodecl::Symbol& n)
    {
        if (n.is_constant())
        {
            return TL::ObjectList<const_value_t*>(1, n.get_constant());
        }
        else
        {
            return TL::ObjectList<const_value_t*>();
        }
    }
    
    Calculator::Ret Calculator::visit(const Nodecl::Add& n)
    {
        TL::ObjectList<const_value_t*> lhs = walk(n.get_lhs());
        TL::ObjectList<const_value_t*> rhs = walk(n.get_rhs());
        
        if (lhs.empty() || rhs.empty())
        {
            return TL::ObjectList<const_value_t*>();
        }
        else
        {
            return TL::ObjectList<const_value_t*>(1, const_value_add(lhs[0], rhs[0]));
        }
    }
    
    Calculator::Ret Calculator::visit(const Nodecl::Minus& n)
    {
        TL::ObjectList<const_value_t*> lhs = walk(n.get_lhs());
        TL::ObjectList<const_value_t*> rhs = walk(n.get_rhs());
        
        if (lhs.empty() || rhs.empty())
        {
            return TL::ObjectList<const_value_t*>();
        }
        else
        {
            return TL::ObjectList<const_value_t*>(1, const_value_sub(lhs[0], rhs[0]));
        }
    }
    
    Calculator::Ret Calculator::visit(const Nodecl::Mul& n)
    {
        TL::ObjectList<const_value_t*> lhs = walk(n.get_lhs());
        TL::ObjectList<const_value_t*> rhs = walk(n.get_rhs());
        
        if (lhs.empty() || rhs.empty())
        {
            return TL::ObjectList<const_value_t*>();
        }
        else
        {
            return TL::ObjectList<const_value_t*>(1, const_value_mul(lhs[0], rhs[0]));
        }
    }
    
    Calculator::Ret Calculator::visit(const Nodecl::Div& n)
    {
        TL::ObjectList<const_value_t*> lhs = walk(n.get_lhs());
        TL::ObjectList<const_value_t*> rhs = walk(n.get_rhs());
        
        if (lhs.empty() || rhs.empty())
        {
            return TL::ObjectList<const_value_t*>();
        }
        else
        {
            return TL::ObjectList<const_value_t*>(1, const_value_div(lhs[0], rhs[0]));
        }
    }
    
    Calculator::Ret Calculator::visit(const Nodecl::Mod& n)
    {
        TL::ObjectList<const_value_t*> lhs = walk(n.get_lhs());
        TL::ObjectList<const_value_t*> rhs = walk(n.get_rhs());
        
        if (lhs.empty() || rhs.empty())
        {
            return TL::ObjectList<const_value_t*>();
        }
        else
        {
            return TL::ObjectList<const_value_t*>(1, const_value_mod(lhs[0], rhs[0]));
        }
    }
    
    Calculator::Ret Calculator::visit(const Nodecl::Shr& n)
    {
        TL::ObjectList<const_value_t*> lhs = walk(n.get_lhs());
        TL::ObjectList<const_value_t*> rhs = walk(n.get_rhs());
        
        if (lhs.empty() || rhs.empty())
        {
            return TL::ObjectList<const_value_t*>();
        }
        else
        {
            return TL::ObjectList<const_value_t*>(1, const_value_shr(lhs[0], rhs[0]));
        }
    }
    
    Calculator::Ret Calculator::visit(const Nodecl::Shl& n)
    {
        TL::ObjectList<const_value_t*> lhs = walk(n.get_lhs());
        TL::ObjectList<const_value_t*> rhs = walk(n.get_rhs());
        
        if (lhs.empty() || rhs.empty())
        {
            return TL::ObjectList<const_value_t*>();
        }
        else
        {
            return TL::ObjectList<const_value_t*>(1, const_value_shl(lhs[0], rhs[0]));
        }
    }
    
    Calculator::Ret Calculator::visit(const Nodecl::Power& n)
    {
        TL::ObjectList<const_value_t*> lhs = walk(n.get_lhs());
        TL::ObjectList<const_value_t*> rhs = walk(n.get_rhs());
        
        if (lhs.empty() || rhs.empty())
        {
            return TL::ObjectList<const_value_t*>();
        }
        else
        {
            return TL::ObjectList<const_value_t*>(1, const_value_pow(lhs[0], rhs[0]));
        }            
    }
    
    Calculator::Ret Calculator::visit(const Nodecl::LogicalAnd& n)
    {
        TL::ObjectList<const_value_t*> lhs = walk(n.get_lhs());
        TL::ObjectList<const_value_t*> rhs = walk(n.get_rhs());
        
        if (lhs.empty() || rhs.empty())
        {
            return TL::ObjectList<const_value_t*>();
        }
        else
        {
            return TL::ObjectList<const_value_t*>(1, const_value_and(lhs[0], rhs[0]));
        }
    }
    
    Calculator::Ret Calculator::visit(const Nodecl::LogicalOr& n)
    {
        TL::ObjectList<const_value_t*> lhs = walk(n.get_lhs());
        TL::ObjectList<const_value_t*> rhs = walk(n.get_rhs());
        
        if (lhs.empty() || rhs.empty())
        {
            return TL::ObjectList<const_value_t*>();
        }
        else
        {
            return TL::ObjectList<const_value_t*>(1, const_value_or(lhs[0], rhs[0]));
        }
    }
    
    Calculator::Ret Calculator::visit(const Nodecl::BitwiseAnd& n)
    {
        TL::ObjectList<const_value_t*> lhs = walk(n.get_lhs());
        TL::ObjectList<const_value_t*> rhs = walk(n.get_rhs());
        
        if (lhs.empty() || rhs.empty())
        {
            return TL::ObjectList<const_value_t*>();
        }
        else
        {
            return TL::ObjectList<const_value_t*>(1, const_value_bitand(lhs[0], rhs[0]));
        }
    }
    Calculator::Ret Calculator::visit(const Nodecl::BitwiseOr& n)
    {
        TL::ObjectList<const_value_t*> lhs = walk(n.get_lhs());
        TL::ObjectList<const_value_t*> rhs = walk(n.get_rhs());
        
        if (lhs.empty() || rhs.empty())
        {
            return TL::ObjectList<const_value_t*>();
        }
        else
        {
            return TL::ObjectList<const_value_t*>(1, const_value_bitor(lhs[0], rhs[0]));
        }
    }
    
    Calculator::Ret Calculator::visit(const Nodecl::BitwiseXor& n)
    {
        TL::ObjectList<const_value_t*> lhs = walk(n.get_lhs());
        TL::ObjectList<const_value_t*> rhs = walk(n.get_rhs());
        
        if (lhs.empty() || rhs.empty())
        {
            return TL::ObjectList<const_value_t*>();
        }
        else
        {
            return TL::ObjectList<const_value_t*>(1, const_value_bitxor(lhs[0], rhs[0]));
        }
    }

    Calculator::Ret Calculator::visit(const Nodecl::Plus& n)
    {
        TL::ObjectList<const_value_t*> rhs = walk(n.get_rhs());
        
        if (rhs.empty())
        {
            return TL::ObjectList<const_value_t*>();
        }
        else
        {
            return TL::ObjectList<const_value_t*>(1, const_value_plus(rhs[0]));
        }
    }
    
    Calculator::Ret Calculator::visit(const Nodecl::Neg& n)
    {
        TL::ObjectList<const_value_t*> rhs = walk(n.get_rhs());
        
        if (rhs.empty())
        {
            return TL::ObjectList<const_value_t*>();
        }
        else
        {
            return TL::ObjectList<const_value_t*>(1, const_value_neg(rhs[0]));
        }
    }
    
    Calculator::Ret Calculator::visit(const Nodecl::BitwiseNot& n)
    {
        TL::ObjectList<const_value_t*> rhs = walk(n.get_rhs());
        
        if (rhs.empty())
        {
            return TL::ObjectList<const_value_t*>();
        }
        else
        {
            return TL::ObjectList<const_value_t*>(1, const_value_bitnot(rhs[0]));
        }
    }
    
    Calculator::Ret Calculator::visit(const Nodecl::LogicalNot& n)
    {
        TL::ObjectList<const_value_t*> rhs = walk(n.get_rhs());
        
        if (rhs.empty())
        {
            return TL::ObjectList<const_value_t*>();
        }
        else
        {
            return TL::ObjectList<const_value_t*>(1, const_value_not(rhs[0]));
        }
    }

    Calculator::Ret Calculator::visit(const Nodecl::Predecrement& n)
    {
        TL::ObjectList<const_value_t*> rhs = walk(n.get_rhs());
        
        if (rhs.empty())
        {
            return TL::ObjectList<const_value_t*>();
        }
        else
        {
            return TL::ObjectList<const_value_t*>(1, const_value_sub(rhs[0], const_value_get_unsigned_int((uint64_t) 1)));
        }
    }
    
    Calculator::Ret Calculator::visit(const Nodecl::Postdecrement& n)
    {
        TL::ObjectList<const_value_t*> rhs = walk(n.get_rhs());
        
        if (rhs.empty())
        {
            return TL::ObjectList<const_value_t*>();
        }
        else
        {
            return TL::ObjectList<const_value_t*>(1, const_value_sub(rhs[0], const_value_get_unsigned_int((uint64_t) 1)));
        }
    }
    
    Calculator::Ret Calculator::visit(const Nodecl::Preincrement& n)
    {
        TL::ObjectList<const_value_t*> rhs = walk(n.get_rhs());
        
        if (rhs.empty())
        {
            return TL::ObjectList<const_value_t*>();
        }
        else
        {
            return TL::ObjectList<const_value_t*>(1, const_value_add(rhs[0], const_value_get_unsigned_int((uint64_t) 1)));
        }
    }
    
    Calculator::Ret Calculator::visit(const Nodecl::Postincrement& n)
    {
        TL::ObjectList<const_value_t*> rhs = walk(n.get_rhs());
        
        if (rhs.empty())
        {
            return TL::ObjectList<const_value_t*>();
        }
        else
        {
            return TL::ObjectList<const_value_t*>(1, const_value_add(rhs[0], const_value_get_unsigned_int((uint64_t) 1)));
        }
    }
    
    Calculator::Ret Calculator::visit(const Nodecl::IntegerLiteral& n)
    {
        return TL::ObjectList<const_value_t*>(1, n.get_constant());
    }
    
    Calculator::Ret Calculator::visit(const Nodecl::FloatingLiteral& n)
    {
        return TL::ObjectList<const_value_t*>(1, n.get_constant());
    }
    
    Calculator::Ret Calculator::visit(const Nodecl::ComplexLiteral& n)
    {
        return TL::ObjectList<const_value_t*>();
    }
    
    Calculator::Ret Calculator::visit(const Nodecl::BooleanLiteral& n)
    {
        return TL::ObjectList<const_value_t*>();
    }
    
    Calculator::Ret Calculator::visit(const Nodecl::StringLiteral& n)
    {
        return TL::ObjectList<const_value_t*>();
    }
    
    Calculator::Ret Calculator::visit(const Nodecl::Derreference& n)
    {
        return TL::ObjectList<const_value_t*>();
    }
}