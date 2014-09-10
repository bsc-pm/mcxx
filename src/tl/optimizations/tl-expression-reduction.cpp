
#include "tl-expression-reduction.hpp"
#include "tl-nodecl-utils.hpp"

namespace TL {
namespace Optimizations {

    ReduceExpressionVisitor::ReduceExpressionVisitor()
        : _calc()
    {}

    void ReduceExpressionVisitor::visit_post(const Nodecl::Add& n)
    {
        if(n.is_constant())
        {   // R1
            n.replace(const_value_to_nodecl(n.get_constant()));
        }
        else
        {
            Nodecl::NodeclBase lhs = n.get_lhs();
            Nodecl::NodeclBase rhs = n.get_rhs();
            if(lhs.is_constant() && const_value_is_zero(lhs.get_constant()))
            {   // 0 + t = t
                n.replace(rhs.shallow_copy());
            }
            else if(rhs.is_constant() && const_value_is_zero(rhs.get_constant()))
            {   // t + 0 = t
                n.replace(lhs.shallow_copy());
            }
            else if(rhs.is_constant())
            {
                if(lhs.is<Nodecl::Add>())
                {   // R6a
                    Nodecl::Add lhs_add = lhs.as<Nodecl::Add>();
                    Nodecl::NodeclBase lhs_lhs = lhs_add.get_lhs();
                    Nodecl::NodeclBase lhs_rhs = lhs_add.get_rhs();
                    if(lhs_lhs.is_constant())
                    {
                        Type rhs_type = rhs.get_type();
                        Nodecl::NodeclBase c = Nodecl::Add::make(lhs_lhs.shallow_copy(), rhs.shallow_copy(), rhs_type);
                        const_value_t* c_value = _calc.compute_const_value(c);
                        if(!const_value_is_zero(c_value))
                        {
                            n.replace(Nodecl::Add::make(const_value_to_nodecl(c_value), lhs_rhs.shallow_copy(), 
                                                        rhs_type, n.get_locus()));
                        }
                        else
                        {
                            n.replace(lhs_rhs.shallow_copy());
                        }
                    }
                    else
                    {
                        // R6i
                        //Nodecl::NodeclBase tmp = lhs_lhs.shallow_copy();
                        //lhs_lhs.replace(rhs.shallow_copy());
                        //rhs.replace(tmp);
                    }
                }
                else if(lhs.is<Nodecl::Minus>())
                {   // R6c
                    Nodecl::Minus lhs_minus = lhs.as<Nodecl::Minus>();
                    Nodecl::NodeclBase lhs_lhs = lhs_minus.get_lhs();
                    Nodecl::NodeclBase lhs_rhs = lhs_minus.get_rhs();
                    if(lhs_lhs.is_constant())
                    {
                        Nodecl::NodeclBase c = Nodecl::Add::make(lhs_lhs.shallow_copy(), rhs.shallow_copy(), rhs.get_type());
                        const_value_t* c_value = _calc.compute_const_value(c);
                        if(!const_value_is_zero(c_value))
                        {
                            n.replace(Nodecl::Minus::make(const_value_to_nodecl(c_value), lhs_rhs.shallow_copy(),
                                                        lhs_lhs.get_type(), n.get_locus()));
                        }
                        else
                        {
                            n.replace(lhs_rhs.shallow_copy());
                        }
                    }
                }
                else
                {   // R2
                    n.replace(Nodecl::Add::make(rhs.shallow_copy(), lhs.shallow_copy(), lhs.get_type(), n.get_locus()));
                }
            }
            else if(lhs.is_constant())
            {
                if(rhs.is<Nodecl::Add>())
                {
                    Nodecl::Add rhs_add = rhs.as<Nodecl::Add>();
                    Nodecl::NodeclBase rhs_lhs = rhs_add.get_lhs();
                    Nodecl::NodeclBase rhs_rhs = rhs_add.get_rhs();
                    if(rhs_lhs.is_constant())
                    {   // R6b
                        Nodecl::NodeclBase c = Nodecl::Add::make(lhs.shallow_copy(), rhs_lhs.shallow_copy(), lhs.get_type());
                        const_value_t* c_value = _calc.compute_const_value(c);
                        if(!const_value_is_zero(c_value))
                        {
                            n.replace(Nodecl::Add::make(const_value_to_nodecl(c_value), rhs_rhs.shallow_copy(),
                                                        rhs.get_type(), n.get_locus()));
                        }
                        else
                        {
                            n.replace(rhs_rhs.shallow_copy());
                        }
                    }
                }
                else if(rhs.is<Nodecl::Minus>())
                {
                    Nodecl::Minus rhs_minus = rhs.as<Nodecl::Minus>();
                    Nodecl::NodeclBase rhs_lhs = rhs_minus.get_lhs();
                    Nodecl::NodeclBase rhs_rhs = rhs_minus.get_rhs();
                    if(rhs_lhs.is_constant())
                    {   // R6d
                        Nodecl::NodeclBase c = Nodecl::Minus::make(lhs.shallow_copy(), rhs_lhs.shallow_copy(), lhs.get_type());
                        const_value_t* c_value = _calc.compute_const_value(c);
                        if(!const_value_is_zero(c_value))
                        {
                            n.replace(Nodecl::Minus::make(const_value_to_nodecl(c_value), rhs_rhs.shallow_copy(),
                                                        lhs.get_type(), n.get_locus()));
                        }
                        else
                        {
                            n.replace(rhs_rhs.shallow_copy());
                        }
                    }
                }
            }
        }
    }

    void ReduceExpressionVisitor::visit_post(const Nodecl::BitwiseAnd& n)
    {
        if(n.is_constant())
        {   // R1
            n.replace(const_value_to_nodecl(n.get_constant()));
        }
    }
 
    void ReduceExpressionVisitor::visit_post(const Nodecl::BitwiseOr& n)
    {
        if(n.is_constant())
        {   // R1
            n.replace(const_value_to_nodecl(n.get_constant()));
        }
    }
 
    void ReduceExpressionVisitor::visit_post(const Nodecl::BitwiseShl& n)
    {
        if(n.is_constant())
        {   // R1
            n.replace(const_value_to_nodecl(n.get_constant()));
        }
    }

    void ReduceExpressionVisitor::visit_post(const Nodecl::BitwiseShr& n)
    {
        if(n.is_constant())
        {   // R1
            n.replace(const_value_to_nodecl(n.get_constant()));
        }
    }

    void ReduceExpressionVisitor::visit_post(const Nodecl::Div& n)
    {   
        if(n.is_constant())
        {   // R10
            n.replace(const_value_to_nodecl(n.get_constant()));
        }
    }

    void ReduceExpressionVisitor::visit_post(const Nodecl::LowerOrEqualThan& n)
    {
        if(n.is_constant())
        {
            n.replace(const_value_to_nodecl(n.get_constant()));
        }
        else
        {
            Nodecl::NodeclBase lhs = n.get_lhs();
            Nodecl::NodeclBase rhs = n.get_rhs();
            if(rhs.is_constant())
            {
                if(lhs.is<Nodecl::Add>())
                {   // R20c
                    Nodecl::Add lhs_add = lhs.as<Nodecl::Add>();
                    Nodecl::NodeclBase lhs_lhs = lhs_add.get_lhs();
                    Nodecl::NodeclBase lhs_rhs = lhs_add.get_rhs();
                    if(lhs_lhs.is_constant())
                    {
                        Nodecl::NodeclBase c = Nodecl::Minus::make(lhs_lhs.shallow_copy(), rhs.shallow_copy(), rhs.get_type());
                        const_value_t* c_value = _calc.compute_const_value(c);
                        Nodecl::NodeclBase new_rhs = Nodecl::Neg::make(lhs_rhs.shallow_copy(), lhs_rhs.get_type());
                        n.replace(Nodecl::LowerOrEqualThan::make(const_value_to_nodecl(c_value), new_rhs,
                                                                 rhs.get_type(), n.get_locus()));
                    }
                }
            }
            else if(lhs.is_constant())
            {
                if(rhs.is<Nodecl::Add>())
                {   // R20d
                    Nodecl::Add rhs_add = rhs.as<Nodecl::Add>();
                    Nodecl::NodeclBase rhs_lhs = rhs_add.get_lhs();
                    Nodecl::NodeclBase rhs_rhs = rhs_add.get_rhs();
                    if(rhs_lhs.is_constant())
                    {
                        Nodecl::NodeclBase c = Nodecl::Minus::make(lhs.shallow_copy(), rhs_lhs.shallow_copy(), rhs.get_type());
                        const_value_t* c_value = _calc.compute_const_value(c);
                        n.replace(Nodecl::LowerOrEqualThan::make(const_value_to_nodecl(c_value), rhs_rhs.shallow_copy(),
                                                                 rhs.get_type(), n.get_locus()));
                    }
                }
            }
            else if(lhs.is<Nodecl::Add>() && rhs.is<Nodecl::Add>())
            {   // R21
                Nodecl::Add lhs_add = lhs.as<Nodecl::Add>();
                Nodecl::NodeclBase lhs_lhs = lhs_add.get_lhs();
                Nodecl::NodeclBase lhs_rhs = lhs_add.get_rhs();
                Nodecl::Add rhs_add = rhs.as<Nodecl::Add>();
                Nodecl::NodeclBase rhs_lhs = rhs_add.get_lhs();
                Nodecl::NodeclBase rhs_rhs = rhs_add.get_rhs();
                if(lhs_lhs.is_constant() && rhs_lhs.is_constant())
                {
                    Nodecl::NodeclBase c = Nodecl::Minus::make(lhs_lhs.shallow_copy(), rhs_lhs.shallow_copy(), rhs.get_type());
                    const_value_t* c_value = _calc.compute_const_value(c);
                    Nodecl::NodeclBase new_rhs = Nodecl::Minus::make(rhs_rhs.shallow_copy(), lhs_rhs.shallow_copy(), rhs_rhs.get_type());
                    n.replace(Nodecl::LowerOrEqualThan::make(const_value_to_nodecl(c_value), new_rhs,
                                                             new_rhs.get_type(), n.get_locus()));
                }
            }
        }
    }

    void ReduceExpressionVisitor::visit_post(const Nodecl::LowerThan& n)
    {
        if(n.is_constant())
        {
            n.replace(const_value_to_nodecl(n.get_constant()));
        }
        else
        {
            Nodecl::NodeclBase lhs = n.get_lhs();
            Nodecl::NodeclBase rhs = n.get_rhs();
            if(rhs.is_constant())
            {
                if(lhs.is<Nodecl::Add>())
                {   // R20a
                    Nodecl::Add lhs_add = lhs.as<Nodecl::Add>();
                    Nodecl::NodeclBase lhs_lhs = lhs_add.get_lhs();
                    Nodecl::NodeclBase lhs_rhs = lhs_add.get_rhs();
                    if(lhs_lhs.is_constant())
                    {
                        Nodecl::NodeclBase c = Nodecl::Minus::make(lhs_lhs.shallow_copy(), rhs.shallow_copy(), rhs.get_type());
                        const_value_t* c_value = _calc.compute_const_value(c);
                        Nodecl::NodeclBase new_rhs = Nodecl::Neg::make(lhs_rhs.shallow_copy(), lhs_rhs.get_type());
                        n.replace(Nodecl::LowerThan::make(const_value_to_nodecl(c_value), new_rhs, rhs.get_type(), n.get_locus()));
                    }
                }
            }
            else if(lhs.is_constant())
            {
                if(rhs.is<Nodecl::Add>())
                {   // R20b
                    Nodecl::Add rhs_add = rhs.as<Nodecl::Add>();
                    Nodecl::NodeclBase rhs_lhs = rhs_add.get_lhs();
                    Nodecl::NodeclBase rhs_rhs = rhs_add.get_rhs();
                    if(rhs_lhs.is_constant())
                    {
                        Nodecl::NodeclBase c = Nodecl::Minus::make(lhs.shallow_copy(), rhs_lhs.shallow_copy(), rhs.get_type());
                        const_value_t* c_value = _calc.compute_const_value(c);
                        n.replace(Nodecl::LowerThan::make(const_value_to_nodecl(c_value), rhs_rhs.shallow_copy(),
                                                          rhs.get_type(), n.get_locus()));
                    }
                }
            }
            else if(lhs.is<Nodecl::Add>() && rhs.is<Nodecl::Add>())
            {   // R21
                Nodecl::Add lhs_add = lhs.as<Nodecl::Add>();
                Nodecl::NodeclBase lhs_lhs = lhs_add.get_lhs();
                Nodecl::NodeclBase lhs_rhs = lhs_add.get_rhs();
                Nodecl::Add rhs_add = rhs.as<Nodecl::Add>();
                Nodecl::NodeclBase rhs_lhs = rhs_add.get_lhs();
                Nodecl::NodeclBase rhs_rhs = rhs_add.get_rhs();
                if(lhs_lhs.is_constant() && rhs_lhs.is_constant())
                {
                    Nodecl::NodeclBase c = Nodecl::Minus::make(lhs_lhs.shallow_copy(), rhs_lhs.shallow_copy(), rhs.get_type());
                    const_value_t* c_value = _calc.compute_const_value(c);
                    Nodecl::NodeclBase new_rhs = Nodecl::Minus::make(rhs_rhs.shallow_copy(), lhs_rhs.shallow_copy(), rhs_rhs.get_type());
                    n.replace(Nodecl::LowerThan::make(const_value_to_nodecl(c_value), new_rhs, new_rhs.get_type(), n.get_locus()));
                }
            }
        }
    }

    void ReduceExpressionVisitor::visit_post(const Nodecl::Minus& n)
    {
        if(n.is_constant())
        {   // R3
            n.replace(const_value_to_nodecl(n.get_constant()));
        }
        else
        {
            Nodecl::NodeclBase lhs = n.get_lhs();
            Nodecl::NodeclBase rhs = n.get_rhs();
            if(lhs.is_constant())
            {
                if(rhs.is<Nodecl::Add>())
                {
                    Nodecl::Add rhs_add = rhs.as<Nodecl::Add>();
                    Nodecl::NodeclBase rhs_lhs = rhs_add.get_lhs();
                    Nodecl::NodeclBase rhs_rhs = rhs_add.get_rhs();
                    if(rhs_lhs.is_constant())
                    {   // R6f
                        Nodecl::NodeclBase c = Nodecl::Minus::make(lhs.shallow_copy(), rhs_lhs.shallow_copy(), lhs.get_type());
                        const_value_t* c_value = _calc.compute_const_value(c);
                        if(!const_value_is_zero(c_value))
                        {
                            n.replace(Nodecl::Minus::make(const_value_to_nodecl(c_value), rhs_rhs.shallow_copy(),
                                                        rhs.get_type(), n.get_locus()));
                        }
                        else
                        {
                            n.replace(rhs_rhs.shallow_copy());
                        }
                    }
                }
                else if(rhs.is<Nodecl::Minus>())
                {
                    Nodecl::Minus rhs_minus = rhs.as<Nodecl::Minus>();
                    Nodecl::NodeclBase rhs_lhs = rhs_minus.get_lhs();
                    Nodecl::NodeclBase rhs_rhs = rhs_minus.get_rhs();
                    if(rhs_lhs.is_constant())
                    {   // R6h
                        Nodecl::NodeclBase c = Nodecl::Minus::make(lhs.shallow_copy(), rhs_lhs.shallow_copy(), lhs.get_type());
                        const_value_t* c_value = _calc.compute_const_value(c);
                        if(!const_value_is_zero(c_value))
                        {
                            n.replace(Nodecl::Add::make(const_value_to_nodecl(c_value), rhs_rhs.shallow_copy(),
                                                          rhs.get_type(), n.get_locus()));
                        }
                        else
                        {
                            n.replace(rhs_rhs.shallow_copy());
                        }
                    }
                }
            }
            else if(rhs.is_constant())
            {   // R4
                if(const_value_is_zero(rhs.get_constant()))
                {
                    n.replace(lhs.shallow_copy());
                }
                else if(lhs.is<Nodecl::Add>())
                {
                    Nodecl::Add lhs_add = lhs.as<Nodecl::Add>();
                    Nodecl::NodeclBase lhs_lhs = lhs_add.get_lhs();
                    Nodecl::NodeclBase lhs_rhs = lhs_add.get_rhs();
                    if(lhs_lhs.is_constant())
                    {   // R6e
                        Nodecl::NodeclBase c = Nodecl::Minus::make(lhs_lhs.shallow_copy(), rhs.shallow_copy(), lhs_lhs.get_type());
                        const_value_t* c_value = _calc.compute_const_value(c);
                        if(!const_value_is_zero(c_value))
                        {
                            n.replace(Nodecl::Add::make(const_value_to_nodecl(c_value), lhs_rhs.shallow_copy(),
                                                        rhs.get_type(), n.get_locus()));
                        }
                        else
                        {
                            n.replace(lhs_rhs.shallow_copy());
                        }
                    }
                }
                else if(lhs.is<Nodecl::Minus>())
                {
                    Nodecl::Minus lhs_minus = lhs.as<Nodecl::Minus>();
                    Nodecl::NodeclBase lhs_lhs = lhs_minus.get_lhs();
                    Nodecl::NodeclBase lhs_rhs = lhs_minus.get_rhs();
                    if(lhs_lhs.is_constant())
                    {   // R6g
                        Nodecl::NodeclBase c = Nodecl::Minus::make(lhs_lhs.shallow_copy(), rhs.shallow_copy(), lhs_lhs.get_type());
                        const_value_t* c_value = _calc.compute_const_value(c);
                        if(!const_value_is_zero(c_value))
                        {
                            n.replace(Nodecl::Minus::make(const_value_to_nodecl(c_value), lhs_rhs.shallow_copy(),
                                                          rhs.get_type(), n.get_locus()));
                        }
                        else
                        {
                            n.replace(lhs_rhs.shallow_copy());
                        }

                    }
                }
                else
                {
                    Nodecl::NodeclBase neg_rhs = const_value_to_nodecl(const_value_neg(rhs.get_constant()));
                    n.replace(Nodecl::Add::make(neg_rhs, lhs.shallow_copy(), lhs.get_type(), n.get_locus()));
                }
            }
            else if(Nodecl::Utils::structurally_equal_nodecls(lhs, rhs))
            {
                n.replace(const_value_to_nodecl(const_value_get_zero(/*num_bytes*/ 4, /*sign*/1)));
            }
            // R30
            else if(lhs.is<Nodecl::Add>() && rhs.is<Nodecl::Add>())
            {
                Nodecl::Add lhs_add = lhs.as<Nodecl::Add>();
                Nodecl::NodeclBase lhs_lhs = lhs_add.get_lhs();
                Nodecl::NodeclBase lhs_rhs = lhs_add.get_rhs();
                Nodecl::Add rhs_add = rhs.as<Nodecl::Add>();
                Nodecl::NodeclBase rhs_lhs = rhs_add.get_lhs();
                Nodecl::NodeclBase rhs_rhs = rhs_add.get_rhs();
                
                //R30a
                if(Nodecl::Utils::structurally_equal_nodecls(lhs, rhs_lhs))
                {
                    n.replace(rhs_rhs);
                }
                //R30b
                else if(Nodecl::Utils::structurally_equal_nodecls(rhs, lhs_lhs))
                {
                    n.replace(lhs_rhs);
                }
                //R30c
                else if (Nodecl::Utils::structurally_equal_nodecls(lhs_lhs, rhs_lhs))
                {
                    Nodecl::Minus minus = Nodecl::Minus::make(
                            lhs_rhs, rhs_rhs, lhs_rhs.get_type());

                    if (lhs_rhs.is_constant() && rhs_rhs.is_constant())
                        minus.set_constant(const_value_sub(
                                    lhs_rhs.get_constant(),
                                    rhs_rhs.get_constant()));

                    walk(minus);

                    n.replace(minus);

                }
                //R30d
                else if (Nodecl::Utils::structurally_equal_nodecls(lhs_rhs, rhs_rhs))
                {
                    Nodecl::Minus minus = Nodecl::Minus::make(
                            lhs_lhs, lhs_lhs, lhs_lhs.get_type());

                    if (lhs_lhs.is_constant() && rhs_lhs.is_constant())
                        minus.set_constant(const_value_sub(
                                    lhs_lhs.get_constant(),
                                    rhs_lhs.get_constant()));

                    walk(minus);

                    n.replace(minus);
                }
            }
        }
    }

    void ReduceExpressionVisitor::visit_post(const Nodecl::Mod& n)
    {   
        if(n.is_constant())
        {
            n.replace(const_value_to_nodecl(n.get_constant()));
        }
        else
        {
            Nodecl::NodeclBase lhs = n.get_lhs();
            Nodecl::NodeclBase rhs = n.get_rhs();
            if(Nodecl::Utils::structurally_equal_nodecls(lhs, rhs) || 
               (rhs.is_constant() && const_value_is_one(rhs.get_constant())))
            {   // R11a
                n.replace(const_value_to_nodecl(const_value_get_zero(/*num_bytes*/ 4, /*sign*/1)));
            }
            else if(lhs.is_constant() && const_value_is_one(lhs.get_constant()))
            {   // R11b
                n.replace(const_value_to_nodecl(const_value_get_one(/*num_bytes*/ 4, /*sign*/1)));
            }
            else if (rhs.get_type().is_integral_type() && lhs.get_type().is_integral_type() &&
                    rhs.is_constant() && lhs.is_constant())
            {   
                Nodecl::Utils::replace(n, const_value_to_nodecl(const_value_mod(lhs.get_constant(), rhs.get_constant())));
            }
        }
    }

    void ReduceExpressionVisitor::visit_post(const Nodecl::Mul& n)
    {
        if(n.is_constant())
        {   // R7
            n.replace(const_value_to_nodecl(n.get_constant()));
        }
        else
        {
            Nodecl::NodeclBase lhs = n.get_lhs();
            Nodecl::NodeclBase rhs = n.get_rhs();
            if((lhs.is_constant() && const_value_is_zero(lhs.get_constant()))
                || (rhs.is_constant() && const_value_is_zero(rhs.get_constant())))
            {   // 0 * t = t , t * 0 = t
                n.replace(const_value_to_nodecl(const_value_get_zero(/*num_bytes*/ 4, /*sign*/1)));
            }
            else if((lhs.is_constant() && const_value_is_one(lhs.get_constant())))
            {   // 1 * t = t
                n.replace(rhs.shallow_copy());
            }
            else if((rhs.is_constant() && const_value_is_one(rhs.get_constant())))
            {   // t * 1 = t
                n.replace(lhs.shallow_copy());
            }
            else if (rhs.is_constant())
            {
                if(const_value_is_zero(rhs.get_constant()))
                {
                    n.replace(const_value_to_nodecl(const_value_get_zero(/*num_bytes*/ 4, /*sign*/1)));
                }
                else
                {
                    if(lhs.is<Nodecl::Mul>())
                    {   // R9
                        Nodecl::Mul lhs_mul = lhs.as<Nodecl::Mul>();
                        Nodecl::NodeclBase lhs_lhs = lhs_mul.get_lhs();
                        Nodecl::NodeclBase lhs_rhs = lhs_mul.get_rhs();
                        if(lhs_lhs.is_constant())
                        {
                            if(const_value_is_zero(lhs_lhs.get_constant()))
                            {
                                n.replace(const_value_to_nodecl(const_value_get_zero(/*num_bytes*/ 4, /*sign*/1)));
                            }
                            else
                            {
                                Type rhs_type = rhs.get_type();
                                Nodecl::NodeclBase c = Nodecl::Mul::make(lhs_lhs.shallow_copy(), rhs.shallow_copy(), rhs_type);
                                const_value_t* c_value = _calc.compute_const_value(c);
                                n.replace(Nodecl::Mul::make(const_value_to_nodecl(c_value), lhs_rhs.shallow_copy(), rhs_type, n.get_locus()));
                            }
                        }
                    }
                    else
                    {   // R8
                        n.replace(Nodecl::Mul::make(rhs.shallow_copy(), lhs.shallow_copy(), lhs.get_type(), n.get_locus()));
                    }
                }
            }
        }
    }

    void ReduceExpressionVisitor::visit_post(const Nodecl::ObjectInit& n)
    {
        TL::Symbol sym = n.get_symbol();

        Nodecl::NodeclBase init = sym.get_value();
        if(!init.is_null())
        {
            walk(init);
        }
    }

    void ReduceExpressionVisitor::visit_post(const Nodecl::VectorAdd& n)
    {
        if(n.is_constant())
        {   // R1
            n.replace(const_value_to_nodecl(n.get_constant()));
        }
        else
        {
            Nodecl::NodeclBase lhs = n.get_lhs();
            Nodecl::NodeclBase rhs = n.get_rhs();
            Nodecl::NodeclBase mask = n.get_mask();
            if(lhs.is_constant() && const_value_is_zero(lhs.get_constant()))
            {   // 0 + t = t
                n.replace(rhs.shallow_copy());
            }
            else if(rhs.is_constant() && const_value_is_zero(rhs.get_constant()))
            {   // t + 0 = t
                n.replace(lhs.shallow_copy());
            }
            else if(rhs.is_constant())
            {
                if(lhs.is<Nodecl::VectorAdd>())
                {   // R6a
                    Nodecl::VectorAdd lhs_add = lhs.as<Nodecl::VectorAdd>();
                    Nodecl::NodeclBase lhs_lhs = lhs_add.get_lhs();
                    Nodecl::NodeclBase lhs_rhs = lhs_add.get_rhs();
                    if(lhs_lhs.is_constant())
                    {
                        Nodecl::NodeclBase c = Nodecl::VectorAdd::make(lhs_lhs.shallow_copy(), rhs.shallow_copy(), mask.shallow_copy(), rhs.get_type());
                        const_value_t* c_value = _calc.compute_const_value(c);
                        if(!const_value_is_zero(c_value))
                        {
                            n.replace(Nodecl::VectorAdd::make(const_value_to_nodecl(c_value), lhs_rhs.shallow_copy(), mask.shallow_copy(),
                                                            rhs.get_type(), n.get_locus()));
                        }
                        else
                        {
                            n.replace(lhs_rhs.shallow_copy());
                        }
                    }
                }
                else if(lhs.is<Nodecl::VectorMinus>())
                {   // R6c
                    Nodecl::VectorMinus lhs_minus = lhs.as<Nodecl::VectorMinus>();
                    Nodecl::NodeclBase lhs_lhs = lhs_minus.get_lhs();
                    Nodecl::NodeclBase lhs_rhs = lhs_minus.get_rhs();
                    if(lhs_lhs.is_constant())
                    {
                        Nodecl::NodeclBase c = Nodecl::VectorAdd::make(lhs_lhs.shallow_copy(), rhs.shallow_copy(), mask.shallow_copy(), rhs.get_type());
                        const_value_t* c_value = _calc.compute_const_value(c);
                        if(!const_value_is_zero(c_value))
                        {
                            n.replace(Nodecl::VectorAdd::make(const_value_to_nodecl(c_value), lhs_rhs.shallow_copy(), mask.shallow_copy(),
                                                                lhs_lhs.get_type(), n.get_locus()));
                        }
                        else
                        {
                            n.replace(lhs_rhs.shallow_copy());
                        }
                    }
                }
                else
                {   // R2
                    n.replace(Nodecl::VectorAdd::make(rhs.shallow_copy(), lhs.shallow_copy(), mask.shallow_copy(), lhs.get_type(), n.get_locus()));
                }
            }
            else if(lhs.is_constant())
            {
                if(rhs.is<Nodecl::VectorAdd>())
                {
                    Nodecl::VectorAdd rhs_add = rhs.as<Nodecl::VectorAdd>();
                    Nodecl::NodeclBase rhs_lhs = rhs_add.get_lhs();
                    Nodecl::NodeclBase rhs_rhs = rhs_add.get_rhs();
                    if(rhs_lhs.is_constant())
                    {   // R6b
                        Nodecl::NodeclBase c = Nodecl::VectorAdd::make(lhs.shallow_copy(), rhs_lhs.shallow_copy(), mask.shallow_copy(), lhs.get_type());
                        const_value_t* c_value = _calc.compute_const_value(c);
                        if(!const_value_is_zero(c_value))
                        {
                            n.replace(Nodecl::VectorAdd::make(const_value_to_nodecl(c_value), rhs_rhs.shallow_copy(), mask.shallow_copy(),
                                                            rhs.get_type(), n.get_locus()));
                        }
                        else
                        {
                            n.replace(rhs_rhs.shallow_copy());
                        }
                    }
                }
                else if(rhs.is<Nodecl::VectorMinus>())
                {
                    Nodecl::VectorMinus rhs_minus = rhs.as<Nodecl::VectorMinus>();
                    Nodecl::NodeclBase rhs_lhs = rhs_minus.get_lhs();
                    Nodecl::NodeclBase rhs_rhs = rhs_minus.get_rhs();
                    if(rhs_lhs.is_constant())
                    {   // R6d
                        Nodecl::NodeclBase c = Nodecl::VectorMinus::make(lhs.shallow_copy(), rhs_lhs.shallow_copy(), mask.shallow_copy(), lhs.get_type());
                        const_value_t* c_value = _calc.compute_const_value(c);
                        if(!const_value_is_zero(c_value))
                        {
                            n.replace(Nodecl::VectorMinus::make(const_value_to_nodecl(c_value), rhs_rhs.shallow_copy(), mask.shallow_copy(),
                                                                lhs.get_type(), n.get_locus()));
                        }
                        else
                        {
                            n.replace(rhs_rhs.shallow_copy());
                        }
                    }
                }
            }
        }
    }

    void ReduceExpressionVisitor::visit_post(const Nodecl::VectorBitwiseAnd& n)
    {
        if(n.is_constant())
        {   // R1
            n.replace(const_value_to_nodecl(n.get_constant()));
        }
    }
 
    void ReduceExpressionVisitor::visit_post(const Nodecl::VectorBitwiseOr& n)
    {
        if(n.is_constant())
        {   // R1
            n.replace(const_value_to_nodecl(n.get_constant()));
        }
    }
 
    void ReduceExpressionVisitor::visit_post(const Nodecl::VectorBitwiseShl& n)
    {
        if(n.is_constant())
        {   // R1
            n.replace(const_value_to_nodecl(n.get_constant()));
        }
    }

    void ReduceExpressionVisitor::visit_post(const Nodecl::VectorBitwiseShlI& n)
    {
        if(n.is_constant())
        {   // R1
            n.replace(const_value_to_nodecl(n.get_constant()));
        }
    }

    void ReduceExpressionVisitor::visit_post(const Nodecl::VectorBitwiseShr& n)
    {
        if(n.is_constant())
        {   // R1
            n.replace(const_value_to_nodecl(n.get_constant()));
        }
    }

    void ReduceExpressionVisitor::visit_post(const Nodecl::VectorBitwiseShrI& n)
    {
        if(n.is_constant())
        {   // R1
            n.replace(const_value_to_nodecl(n.get_constant()));
        }
    }

    void ReduceExpressionVisitor::visit_post(const Nodecl::VectorDiv& n)
    {
        if(n.is_constant())
        {   // R10
            n.replace(const_value_to_nodecl(n.get_constant()));
        }
    }

    void ReduceExpressionVisitor::visit_post(const Nodecl::VectorLowerOrEqualThan& n)
    {
        if(n.is_constant())
        {
            n.replace(const_value_to_nodecl(n.get_constant()));
        }
        else
        {
            Nodecl::NodeclBase lhs = n.get_lhs();
            Nodecl::NodeclBase rhs = n.get_rhs();
            Nodecl::NodeclBase mask = n.get_mask();
            if(rhs.is_constant())
            {
                if(lhs.is<Nodecl::VectorAdd>())
                {   // R20c
                    Nodecl::VectorAdd lhs_add = lhs.as<Nodecl::VectorAdd>();
                    Nodecl::NodeclBase lhs_lhs = lhs_add.get_lhs();
                    Nodecl::NodeclBase lhs_rhs = lhs_add.get_rhs();
                    if(lhs_lhs.is_constant())
                    {
                        Nodecl::NodeclBase c = Nodecl::VectorMinus::make(lhs_lhs.shallow_copy(), rhs.shallow_copy(), mask.shallow_copy(), rhs.get_type());
                        const_value_t* c_value = _calc.compute_const_value(c);
                        Nodecl::NodeclBase new_rhs = Nodecl::VectorNeg::make(lhs_rhs.shallow_copy(), mask.shallow_copy(), lhs_rhs.get_type());
                        n.replace(Nodecl::VectorLowerOrEqualThan::make(const_value_to_nodecl(c_value), new_rhs,
                                                                       mask.shallow_copy(), rhs.get_type(), n.get_locus()));
                    }
                }
            }
            else if(lhs.is_constant())
            {
                if(rhs.is<Nodecl::VectorAdd>())
                {   // R20d
                    Nodecl::VectorAdd rhs_add = rhs.as<Nodecl::VectorAdd>();
                    Nodecl::NodeclBase rhs_lhs = rhs_add.get_lhs();
                    Nodecl::NodeclBase rhs_rhs = rhs_add.get_rhs();
                    if(rhs_lhs.is_constant())
                    {
                        Nodecl::NodeclBase c = Nodecl::VectorMinus::make(lhs.shallow_copy(), rhs_lhs.shallow_copy(), mask.shallow_copy(), rhs.get_type());
                        const_value_t* c_value = _calc.compute_const_value(c);
                        n.replace(Nodecl::VectorLowerOrEqualThan::make(const_value_to_nodecl(c_value), rhs_rhs.shallow_copy(),
                                                                       mask.shallow_copy(), rhs.get_type(), n.get_locus()));
                    }
                }
            }
            else if(lhs.is<Nodecl::VectorAdd>() && rhs.is<Nodecl::VectorAdd>())
            {   // R21
                Nodecl::VectorAdd lhs_add = lhs.as<Nodecl::VectorAdd>();
                Nodecl::NodeclBase lhs_lhs = lhs_add.get_lhs();
                Nodecl::NodeclBase lhs_rhs = lhs_add.get_rhs();
                Nodecl::VectorAdd rhs_add = rhs.as<Nodecl::VectorAdd>();
                Nodecl::NodeclBase rhs_lhs = rhs_add.get_lhs();
                Nodecl::NodeclBase rhs_rhs = rhs_add.get_rhs();
                if(lhs_lhs.is_constant() && rhs_lhs.is_constant())
                {
                    Nodecl::NodeclBase c = Nodecl::VectorMinus::make(lhs_lhs.shallow_copy(), rhs_lhs.shallow_copy(), mask.shallow_copy(), rhs.get_type());
                    const_value_t* c_value = _calc.compute_const_value(c);
                    Nodecl::NodeclBase new_rhs = Nodecl::VectorMinus::make(rhs_rhs.shallow_copy(), lhs_rhs.shallow_copy(), mask.shallow_copy(), rhs_rhs.get_type());
                    n.replace(Nodecl::VectorLowerOrEqualThan::make(const_value_to_nodecl(c_value), new_rhs,
                                                                   mask.shallow_copy(), new_rhs.get_type(), n.get_locus()));
                }
            }
        }
    }


    void ReduceExpressionVisitor::visit_post(const Nodecl::VectorLowerThan& n)
    {
        if(n.is_constant())
        {
            n.replace(const_value_to_nodecl(n.get_constant()));
        }
        else
        {
            Nodecl::NodeclBase lhs = n.get_lhs();
            Nodecl::NodeclBase rhs = n.get_rhs();
            Nodecl::NodeclBase mask = n.get_mask();
            if(rhs.is_constant())
            {
                if(lhs.is<Nodecl::VectorAdd>())
                {   // R20a
                    Nodecl::VectorAdd lhs_add = lhs.as<Nodecl::VectorAdd>();
                    Nodecl::NodeclBase lhs_lhs = lhs_add.get_lhs();
                    Nodecl::NodeclBase lhs_rhs = lhs_add.get_rhs();
                    if(lhs_lhs.is_constant())
                    {
                        Nodecl::NodeclBase c = Nodecl::VectorMinus::make(lhs_lhs.shallow_copy(), rhs.shallow_copy(), mask.shallow_copy(), rhs.get_type());
                        const_value_t* c_value = _calc.compute_const_value(c);
                        Nodecl::NodeclBase new_rhs = Nodecl::VectorNeg::make(lhs_rhs.shallow_copy(), mask.shallow_copy(), lhs_rhs.get_type());
                        n.replace(Nodecl::VectorLowerThan::make(const_value_to_nodecl(c_value), new_rhs,
                                                                mask.shallow_copy(), rhs.get_type(), n.get_locus()));
                    }
                }
            }
            else if(lhs.is_constant())
            {
                if(rhs.is<Nodecl::VectorAdd>())
                {   // R20b
                    Nodecl::VectorAdd rhs_add = rhs.as<Nodecl::VectorAdd>();
                    Nodecl::NodeclBase rhs_lhs = rhs_add.get_lhs();
                    Nodecl::NodeclBase rhs_rhs = rhs_add.get_rhs();
                    if(rhs_lhs.is_constant())
                    {
                        Nodecl::NodeclBase c = Nodecl::VectorMinus::make(lhs.shallow_copy(), rhs_lhs.shallow_copy(), mask.shallow_copy(), rhs.get_type());
                        const_value_t* c_value = _calc.compute_const_value(c);
                        n.replace(Nodecl::VectorLowerThan::make(const_value_to_nodecl(c_value), rhs_rhs.shallow_copy(),
                                                                mask.shallow_copy(), rhs.get_type(), n.get_locus()));
                    }
                }
            }
            else if(lhs.is<Nodecl::VectorAdd>() && rhs.is<Nodecl::VectorAdd>())
            {   // R21
                Nodecl::VectorAdd lhs_add = lhs.as<Nodecl::VectorAdd>();
                Nodecl::NodeclBase lhs_lhs = lhs_add.get_lhs();
                Nodecl::NodeclBase lhs_rhs = lhs_add.get_rhs();
                Nodecl::VectorAdd rhs_add = rhs.as<Nodecl::VectorAdd>();
                Nodecl::NodeclBase rhs_lhs = rhs_add.get_lhs();
                Nodecl::NodeclBase rhs_rhs = rhs_add.get_rhs();
                if(lhs_lhs.is_constant() && rhs_lhs.is_constant())
                {
                    Nodecl::NodeclBase c = Nodecl::VectorMinus::make(lhs_lhs.shallow_copy(), rhs_lhs.shallow_copy(), mask.shallow_copy(), rhs.get_type());
                    const_value_t* c_value = _calc.compute_const_value(c);
                    Nodecl::NodeclBase new_rhs = Nodecl::VectorMinus::make(rhs_rhs.shallow_copy(), lhs_rhs.shallow_copy(), mask.shallow_copy(), rhs_rhs.get_type());
                    n.replace(Nodecl::VectorLowerThan::make(const_value_to_nodecl(c_value), new_rhs,
                                                            mask.shallow_copy(), new_rhs.get_type(), n.get_locus()));
                }
            }
        }
    }

    void ReduceExpressionVisitor::visit_post(const Nodecl::VectorMinus& n)
    {    
        if(n.is_constant())
        {   // R3
            n.replace(const_value_to_nodecl(n.get_constant()));
        }
        else
        {
            Nodecl::NodeclBase lhs = n.get_lhs();
            Nodecl::NodeclBase rhs = n.get_rhs();
            Nodecl::NodeclBase mask = n.get_mask();
            if(lhs.is_constant())
            {
                if(rhs.is<Nodecl::VectorAdd>())
                {
                    Nodecl::VectorAdd rhs_add = rhs.as<Nodecl::VectorAdd>();
                    Nodecl::NodeclBase rhs_lhs = rhs_add.get_lhs();
                    Nodecl::NodeclBase rhs_rhs = rhs_add.get_rhs();
                    if(rhs_lhs.is_constant())
                    {   // R6f
                        Nodecl::NodeclBase c = Nodecl::VectorMinus::make(lhs.shallow_copy(), rhs_lhs.shallow_copy(), mask.shallow_copy(), lhs.get_type());
                        const_value_t* c_value = _calc.compute_const_value(c);
                        if(!const_value_is_zero(c_value))
                        {
                            n.replace(Nodecl::VectorMinus::make(const_value_to_nodecl(c_value), rhs_rhs.shallow_copy(), mask.shallow_copy(),
                                                            rhs.get_type(), n.get_locus()));
                        }
                        else
                        {
                            n.replace(rhs_rhs.shallow_copy());
                        }
                    }
                }
                else if(rhs.is<Nodecl::VectorMinus>())
                {
                    Nodecl::VectorMinus rhs_minus = rhs.as<Nodecl::VectorMinus>();
                    Nodecl::NodeclBase rhs_lhs = rhs_minus.get_lhs();
                    Nodecl::NodeclBase rhs_rhs = rhs_minus.get_rhs();
                    if(rhs_lhs.is_constant())
                    {   // R6h
                        Nodecl::NodeclBase c = Nodecl::VectorMinus::make(lhs.shallow_copy(), rhs_lhs.shallow_copy(), mask.shallow_copy(), lhs.get_type());
                        const_value_t* c_value = _calc.compute_const_value(c);
                        if(!const_value_is_zero(c_value))
                        {
                            n.replace(Nodecl::VectorMinus::make(const_value_to_nodecl(c_value), rhs_rhs.shallow_copy(), mask.shallow_copy(),
                                                                rhs.get_type(), n.get_locus()));
                        }
                        else
                        {
                            n.replace(rhs_rhs.shallow_copy());
                        }

                    }
                }
            }
            else if(rhs.is_constant())
            {   // R4
                if(const_value_is_zero(rhs.get_constant()))
                {
                    n.replace(lhs.shallow_copy());
                }
                else if(lhs.is<Nodecl::VectorAdd>())
                {
                    Nodecl::VectorAdd lhs_add = lhs.as<Nodecl::VectorAdd>();
                    Nodecl::NodeclBase lhs_lhs = lhs_add.get_lhs();
                    Nodecl::NodeclBase lhs_rhs = lhs_add.get_rhs();
                    if(lhs_lhs.is_constant())
                    {   // R6e
                        Nodecl::NodeclBase c = Nodecl::VectorMinus::make(lhs_lhs.shallow_copy(), rhs.shallow_copy(), mask.shallow_copy(), lhs_lhs.get_type());
                        const_value_t* c_value = _calc.compute_const_value(c);
                        if(!const_value_is_zero(c_value))
                        {
                            n.replace(Nodecl::VectorAdd::make(const_value_to_nodecl(c_value), lhs_rhs.shallow_copy(), mask.shallow_copy(),
                                                            rhs.get_type(), n.get_locus()));
                        }
                        else
                        {
                            n.replace(lhs_rhs.shallow_copy());
                        }

                    }
                }
                else if(lhs.is<Nodecl::VectorMinus>())
                {
                    Nodecl::VectorMinus lhs_minus = lhs.as<Nodecl::VectorMinus>();
                    Nodecl::NodeclBase lhs_lhs = lhs_minus.get_lhs();
                    Nodecl::NodeclBase lhs_rhs = lhs_minus.get_rhs();
                    if(lhs_lhs.is_constant())
                    {   // R6g
                        Nodecl::NodeclBase c = Nodecl::VectorMinus::make(lhs_lhs.shallow_copy(), rhs.shallow_copy(), mask.shallow_copy(), lhs_lhs.get_type());
                        const_value_t* c_value = _calc.compute_const_value(c);
                        if(!const_value_is_zero(c_value))
                        {
                            n.replace(Nodecl::VectorMinus::make(const_value_to_nodecl(c_value), lhs_rhs.shallow_copy(), mask.shallow_copy(),
                                                                rhs.get_type(), n.get_locus()));
                        }
                        else
                        {
                            n.replace(lhs_rhs.shallow_copy());
                        }

                    }
                }
                else
                {
                    Nodecl::NodeclBase neg_rhs = const_value_to_nodecl(const_value_neg(rhs.get_constant()));
                    n.replace(Nodecl::VectorAdd::make(neg_rhs, lhs.shallow_copy(), mask.shallow_copy(), 
                                                    lhs.get_type(), n.get_locus()));
                }
            }
            else if(Nodecl::Utils::structurally_equal_nodecls(lhs, rhs))
            {
                n.replace(const_value_to_nodecl(const_value_make_vector_from_scalar(
                                n.get_type().vector_num_elements(),
                                const_value_get_zero(/*num_bytes*/ 4, /*sign*/1))));
            }
        }
    }

    void ReduceExpressionVisitor::visit_post(const Nodecl::VectorMod& n)
    {
        if(n.is_constant())
        {
            n.replace(const_value_to_nodecl(n.get_constant()));
        }
        else
        {
            Nodecl::NodeclBase lhs = n.get_lhs();
            Nodecl::NodeclBase rhs = n.get_rhs();
            if(Nodecl::Utils::structurally_equal_nodecls(lhs, rhs) || 
               (rhs.is_constant() && const_value_is_one(rhs.get_constant())))
            {   // R11a
                n.replace(const_value_to_nodecl(const_value_get_zero(/*num_bytes*/ 4, /*sign*/1)));
            }
            else if(lhs.is_constant() && const_value_is_one(lhs.get_constant()))
            {   // R11b
                n.replace(const_value_to_nodecl(const_value_get_one(/*num_bytes*/ 4, /*sign*/1)));
            }
        }
    }

    void ReduceExpressionVisitor::visit_post(const Nodecl::VectorMul& n)
    {
        if(n.is_constant())
        {   // R7
            n.replace(const_value_to_nodecl(n.get_constant()));
        }
        else
        {
            Nodecl::NodeclBase lhs = n.get_lhs();
            Nodecl::NodeclBase rhs = n.get_rhs();
            Nodecl::NodeclBase mask = n.get_mask();
            if((lhs.is_constant() && const_value_is_zero(lhs.get_constant()))
                || (rhs.is_constant() && const_value_is_zero(rhs.get_constant())))
            {   // 0 * t = t , t * 0 = t
                n.replace(const_value_to_nodecl(const_value_make_vector_from_scalar(
                                n.get_type().vector_num_elements(),
                                const_value_get_zero(/*num_bytes*/ 4, /*sign*/1))));
            }
            else if((lhs.is_constant() && const_value_is_one(lhs.get_constant())))
            {   // 1 * t = t
                n.replace(rhs.shallow_copy());
            }
            else if((rhs.is_constant() && const_value_is_one(rhs.get_constant())))
            {   // t * 1 = t
                n.replace(lhs.shallow_copy());
            }
            else if (rhs.is_constant())
            {
                if(const_value_is_zero(rhs.get_constant()))
                {
                    n.replace(const_value_to_nodecl(const_value_make_vector_from_scalar(
                                    n.get_type().vector_num_elements(),
                                    const_value_get_zero(/*num_bytes*/ 4, /*sign*/1))));
                }
                else
                {
                    if(lhs.is<Nodecl::VectorMul>())
                    {   // R9
                        Nodecl::VectorMul lhs_mul = lhs.as<Nodecl::VectorMul>();
                        Nodecl::NodeclBase lhs_lhs = lhs_mul.get_lhs();
                        Nodecl::NodeclBase lhs_rhs = lhs_mul.get_rhs();
                        if(lhs_lhs.is_constant())
                        {
                            if(const_value_is_zero(lhs_lhs.get_constant()))
                            {
                                n.replace(const_value_to_nodecl(const_value_make_vector_from_scalar(
                                                n.get_type().vector_num_elements(), const_value_get_zero(/*num_bytes*/ 4, /*sign*/1))));
                            }
                            else
                            {
                                Nodecl::NodeclBase c = Nodecl::VectorMul::make(lhs_lhs.shallow_copy(), rhs.shallow_copy(), mask.shallow_copy(), rhs.get_type());
                                const_value_t* c_value = _calc.compute_const_value(c);
                                n.replace(Nodecl::VectorMul::make(const_value_to_nodecl(c_value), lhs_rhs.shallow_copy(), mask.shallow_copy(),
                                                                  rhs.get_type(), n.get_locus()));
                            }
                        }
                    }
                    else
                    {   // R8
                        n.replace(Nodecl::VectorMul::make(rhs.shallow_copy(), lhs.shallow_copy(), mask.shallow_copy(), lhs.get_type(), n.get_locus()));
                    }
                }
            }
        }
    }

    UnitaryReductor::UnitaryReductor()
    {
    }

    bool UnitaryReductor::is_leaf_node(
            const Nodecl::NodeclBase& n)
    {
        if (n.is<Nodecl::Symbol>() || n.is<Nodecl::Mul>() ||
                n.is<Nodecl::Div>() || n.is<Nodecl::Mod>())
            return true;

        return false;
    }

    void UnitaryReductor::reduce(
            const Nodecl::Minus& n)
    {
        Nodecl::NodeclBase lhs = n.get_lhs().no_conv();
        Nodecl::NodeclBase rhs = n.get_rhs().no_conv();

        UnitaryDecomposer decomp;
        _unitary_rhss = decomp.walk(rhs);

        walk(n.get_lhs());

        if (rhs.is_constant() && lhs.is_constant())
        {
            n.replace(const_value_to_nodecl(const_value_sub(
                        lhs.get_constant(), rhs.get_constant())));
        }

        TL::Optimizations::ReduceExpressionVisitor reduce_expr_visitor;
        reduce_expr_visitor.walk(n);

        if (n.is<Nodecl::Minus>())
        {
            lhs = n.get_lhs().no_conv();
            rhs = n.get_rhs().no_conv();

            if (rhs.is_constant() && lhs.is_constant())
            {
                n.replace(const_value_to_nodecl(const_value_sub(
                                lhs.get_constant(), rhs.get_constant())));
            }
        }

        _unitary_rhss.clear();
    }

    void UnitaryReductor::visit(const Nodecl::Conversion& n)
    {
        //TODO
        bool is_symbol_pre = n.get_nest().is<Nodecl::Symbol>();

        walk(n.get_nest());

        bool is_integer_post = n.get_nest().is<Nodecl::IntegerLiteral>();

        if (is_symbol_pre && is_integer_post)
            n.replace(n.get_nest());
    }

    void UnitaryReductor::visit(const Nodecl::Add& n)
    {
        Nodecl::NodeclBase lhs = n.get_lhs();
        Nodecl::NodeclBase rhs = n.get_rhs();

        if (is_leaf_node(lhs))
        {
            TL::ObjectList<Nodecl::NodeclBase>::iterator it =
                Nodecl::Utils::list_get_nodecl_by_structure(
                        _unitary_rhss, lhs);

            if (it != _unitary_rhss.end())
            {
                n.replace(rhs);
                it->replace(const_value_to_nodecl(
                            const_value_get_zero(1, 4)));
                _unitary_rhss.erase(it);
            }
        }
        else
        {
            walk(lhs);

            if (is_leaf_node(rhs))
            {
                TL::ObjectList<Nodecl::NodeclBase>::iterator it =
                    Nodecl::Utils::list_get_nodecl_by_structure(
                            _unitary_rhss, rhs);

                if (it != _unitary_rhss.end())
                {
                    n.replace(lhs);
                    it->replace(const_value_to_nodecl(
                                const_value_get_zero(1, 4)));
                    _unitary_rhss.erase(it);
                }
            }
            else
            {
                walk(rhs);
            }
        }

        if (rhs.is_constant() && lhs.is_constant())
        {
            n.replace(const_value_to_nodecl(const_value_add(
                        lhs.get_constant(), rhs.get_constant())));
        }
    }

    void UnitaryReductor::visit(const Nodecl::Minus& n)
    {
        Nodecl::NodeclBase lhs = n.get_lhs();
        Nodecl::NodeclBase rhs = n.get_rhs();

        if (rhs.is_constant() && lhs.is_constant())
        {
            n.replace(const_value_to_nodecl(const_value_sub(
                        lhs.get_constant(), rhs.get_constant())));
        }

    }

    void UnitaryReductor::visit(const Nodecl::Mul& n)
    {
        TL::ObjectList<Nodecl::NodeclBase>::iterator it =
            Nodecl::Utils::list_get_nodecl_by_structure(
                    _unitary_rhss, n);

        if (it != _unitary_rhss.end())
        {
            n.replace(const_value_to_nodecl(
                        const_value_get_zero(1, 4)));
            it->replace(const_value_to_nodecl(
                        const_value_get_zero(1, 4)));
            _unitary_rhss.erase(it);

        }
    }

    void UnitaryReductor::visit(const Nodecl::Div& n)
    {
        TL::ObjectList<Nodecl::NodeclBase>::iterator it =
            Nodecl::Utils::list_get_nodecl_by_structure(
                    _unitary_rhss, n);

        if (it != _unitary_rhss.end())
        {
            n.replace(const_value_to_nodecl(
                        const_value_get_zero(1, 4)));
            it->replace(const_value_to_nodecl(
                        const_value_get_zero(1, 4)));
            _unitary_rhss.erase(it);

        }
    }

    void UnitaryReductor::visit(const Nodecl::Mod& n)
    {
        TL::ObjectList<Nodecl::NodeclBase>::iterator it =
            Nodecl::Utils::list_get_nodecl_by_structure(
                    _unitary_rhss, n);

        if (it != _unitary_rhss.end())
        {
            n.replace(const_value_to_nodecl(
                        const_value_get_zero(1, 4)));
            it->replace(const_value_to_nodecl(
                        const_value_get_zero(1, 4)));
            _unitary_rhss.erase(it);

        }
    }

    void UnitaryReductor::visit(const Nodecl::Symbol& n)
    {
        TL::ObjectList<Nodecl::NodeclBase>::iterator it =
            Nodecl::Utils::list_get_nodecl_by_structure(
                    _unitary_rhss, n);

        if (it != _unitary_rhss.end())
        {
            n.replace(const_value_to_nodecl(
                        const_value_get_zero(1, 4)));
            it->replace(const_value_to_nodecl(
                        const_value_get_zero(1, 4)));
            _unitary_rhss.erase(it);

        }
    }

    void UnitaryReductor::visit(const Nodecl::IntegerLiteral& n)
    {
        TL::ObjectList<Nodecl::NodeclBase>::iterator it =
            Nodecl::Utils::list_get_nodecl_by_structure(
                    _unitary_rhss, n);

        if (it != _unitary_rhss.end())
        {
            n.replace(const_value_to_nodecl(
                        const_value_get_zero(1, 4)));
            it->replace(const_value_to_nodecl(
                        const_value_get_zero(1, 4)));
            _unitary_rhss.erase(it);
        }
    }

    void UnitaryReductor::unhandled_node(
            const Nodecl::NodeclBase& n)
    {
        internal_error("UnitaryReductor: Unhandled node type '%s'\n",
                ast_print_node_type(n.get_kind()));
 
        return Ret();
    }

    UnitaryDecomposer::UnitaryDecomposer()
    {
    }

    UnitaryDecomposer::Ret UnitaryDecomposer::visit(const Nodecl::Conversion& n)
    {
        //TODO
        n.replace(n.get_nest());
        return walk(n);
    }

    UnitaryDecomposer::Ret UnitaryDecomposer::visit(const Nodecl::Add& n)
    {
        return walk(n.get_lhs()).append(walk(n.get_rhs()));
    }

    UnitaryDecomposer::Ret UnitaryDecomposer::visit(const Nodecl::Minus& n)
    {
        return walk(n.get_lhs()).append(walk(n.get_rhs()));
    }

    UnitaryDecomposer::Ret UnitaryDecomposer::visit(const Nodecl::Mul& n)
    {
        TL::ObjectList<Nodecl::NodeclBase> result;
        result.append(n);
        return result;
    }

    UnitaryDecomposer::Ret UnitaryDecomposer::visit(const Nodecl::Div& n)
    {
        TL::ObjectList<Nodecl::NodeclBase> result;
        result.append(n);
        return result;
    }

    UnitaryDecomposer::Ret UnitaryDecomposer::visit(const Nodecl::Mod& n)
    {
        TL::ObjectList<Nodecl::NodeclBase> result;
        result.append(n);
        return result;
    }

    UnitaryDecomposer::Ret UnitaryDecomposer::visit(const Nodecl::Symbol& n)
    {
        TL::ObjectList<Nodecl::NodeclBase> result;
        result.append(n);
        return result;
    }

    UnitaryDecomposer::Ret UnitaryDecomposer::visit(const Nodecl::IntegerLiteral& n)
    {
        TL::ObjectList<Nodecl::NodeclBase> result;
        result.append(n);
        return result;
    }


    UnitaryDecomposer::Ret UnitaryDecomposer::unhandled_node(
            const Nodecl::NodeclBase& n)
    {
        internal_error("UnitaryDecomposer: Unhandled node type '%s'\n",
                ast_print_node_type(n.get_kind()));
 
        return Ret();
    }
 
}
}
