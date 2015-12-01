/*--------------------------------------------------------------------
  (C) Copyright 2006-2015 Barcelona Supercomputing Center
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


#include "tl-expression-reduction.hpp"
#include "tl-optimizations.hpp"

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
                if (lhs.is_constant())
                {   // Ideally this will never happen, but sometimes, the constant value is not propagated to the Minus node
                    n.replace(const_value_to_nodecl(const_value_add(lhs.get_constant(), rhs.get_constant())));
                }
                else if(lhs.is<Nodecl::Add>())
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

                // R2: This rule applys independetly from the kind of lhs
                // Re-check again everything, the node could have changed
                if (n.is<Nodecl::Add>())
                {
                    lhs = n.get_lhs();
                    rhs = n.get_rhs();
                    
                    if (!lhs.is_constant() && rhs.is_constant())
                    {
                        n.replace(Nodecl::Add::make(rhs.shallow_copy(), lhs.shallow_copy(), lhs.get_type(), n.get_locus()));
                    }
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
                        Nodecl::NodeclBase c = Nodecl::Add::make(lhs.shallow_copy(), rhs_lhs.shallow_copy(), lhs.get_type());
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

            // The node could have changed
            if (n.is<Nodecl::Add>())
            {
                lhs = n.get_lhs();
                rhs = n.get_rhs();

                // R50a: 
                // TODO: This rule is changing the order of the operations (as many other).
                //       Maybe it should be apply on floating point data only when fast-math or -O3 is enabled
                if ((lhs.is<Nodecl::Add>()))
                {
                    Nodecl::Add lhs_add = lhs.as<Nodecl::Add>();
                    Nodecl::NodeclBase lhs_lhs = lhs_add.get_lhs();
                    Nodecl::NodeclBase lhs_rhs = lhs_add.get_rhs();

                    if (lhs_lhs.is_constant())
                    {
                        Nodecl::Add n_copy = n.shallow_copy().as<Nodecl::Add>();
                        Nodecl::Add lhs_copy = lhs_add.shallow_copy().as<Nodecl::Add>();

                        n_copy.get_lhs().replace(lhs_rhs.shallow_copy());
                        lhs_copy.get_rhs().replace(n_copy);

                        n.replace(lhs_copy);
                    }
                }
                // R51a: 
                // TODO: This rule is changing the order of the operations (as many other).
                //       Maybe it should be apply on floating point data only when fast-math or -O3 is enabled
                else if ((lhs.is<Nodecl::Minus>()))
                {
                    Nodecl::Minus lhs_sub = lhs.as<Nodecl::Minus>();
                    Nodecl::NodeclBase lhs_lhs = lhs_sub.get_lhs();
                    Nodecl::NodeclBase lhs_rhs = lhs_sub.get_rhs();

                    if (lhs_lhs.is_constant())
                    {
                        Nodecl::Minus lhs_copy = lhs_sub.shallow_copy().as<Nodecl::Minus>();
                        Nodecl::Minus new_rhs = Nodecl::Minus::make(
                                lhs_rhs.shallow_copy(), rhs.shallow_copy(),
                                n.get_type(), n.get_locus());

                        lhs_copy.get_rhs().replace(new_rhs);
                        n.replace(lhs_copy);
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
                if (rhs.is_constant())
                {   // Ideally this will never happen, but sometimes, the constant value is not propagated to the Minus node
                    n.replace(const_value_to_nodecl(const_value_sub(lhs.get_constant(), rhs.get_constant())));
                }
                else if(rhs.is<Nodecl::Add>())
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

            // The node could have changed
            if (n.is<Nodecl::Minus>())
            {
                lhs = n.get_lhs();
                rhs = n.get_rhs();

                // R50b: 
                // TODO: This rule is changing the order of the operations (as many other).
                //       Maybe it should be apply on floating point data only when fast-math or -O3 is enabled
                if ((lhs.is<Nodecl::Minus>()))
                {
                    Nodecl::Minus lhs_sub = lhs.as<Nodecl::Minus>();
                    Nodecl::NodeclBase lhs_lhs = lhs_sub.get_lhs();
                    Nodecl::NodeclBase lhs_rhs = lhs_sub.get_rhs();

                    if (lhs_lhs.is_constant())
                    {
                        Nodecl::Minus lhs_copy = lhs_sub.shallow_copy().as<Nodecl::Minus>();
                        Nodecl::Add new_rhs = Nodecl::Add::make(
                                lhs_rhs.shallow_copy(), rhs.shallow_copy(),
                                n.get_type(), n.get_locus());

                        lhs_copy.get_rhs().replace(new_rhs);
                        n.replace(lhs_copy);
                    }
                }
                // R51b: 
                // TODO: This rule is changing the order of the operations (as many other).
                //       Maybe it should be apply on floating point data only when fast-math or -O3 is enabled
                else if ((lhs.is<Nodecl::Add>()))
                {
                    Nodecl::Add lhs_add = lhs.as<Nodecl::Add>();
                    Nodecl::NodeclBase lhs_lhs = lhs_add.get_lhs();
                    Nodecl::NodeclBase lhs_rhs = lhs_add.get_rhs();

                    if (lhs_lhs.is_constant())
                    {
                        Nodecl::Add lhs_copy = lhs_add.shallow_copy().as<Nodecl::Add>();
                        Nodecl::Minus new_rhs = Nodecl::Minus::make(
                                lhs_rhs.shallow_copy(), rhs.shallow_copy(),
                                n.get_type(), n.get_locus());

                        lhs_copy.get_rhs().replace(new_rhs);
                        n.replace(lhs_copy);
                    }
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
                    if (lhs.is_constant())
                    {   // Ideally this will never happen, but sometimes, the constant value is not propagated to the Minus node
                        n.replace(const_value_to_nodecl(const_value_mul(lhs.get_constant(), rhs.get_constant())));
                    }
                    else if(lhs.is<Nodecl::Mul>())
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

                    // R8: This rule applys independetly from the kind of lhs
                    // Re-check again everything, the node could have changed
                    if (n.is<Nodecl::Mul>())
                    {
                        lhs = n.get_lhs();
                        rhs = n.get_rhs();

                        if (!lhs.is_constant() && rhs.is_constant())
                        {
                            n.replace(Nodecl::Mul::make(rhs.shallow_copy(), lhs.shallow_copy(), lhs.get_type(), n.get_locus()));
                        }
                    }
                }
            }

            // The node could have changed
            if (n.is<Nodecl::Mul>())
            {
                lhs = n.get_lhs();
                rhs = n.get_rhs();

                // R50c: 
                // TODO: This rule is changing the order of the operations (as many other).
                //       Maybe it should be apply on floating point data only when fast-math or -O3 is enabled
                if ((lhs.is<Nodecl::Mul>()))
                {
                    Nodecl::Mul lhs_add = lhs.as<Nodecl::Mul>();
                    Nodecl::NodeclBase lhs_lhs = lhs_add.get_lhs();
                    Nodecl::NodeclBase lhs_rhs = lhs_add.get_rhs();

                    if (lhs_lhs.is_constant())
                    {
                        Nodecl::Mul n_copy = n.shallow_copy().as<Nodecl::Mul>();
                        Nodecl::Mul lhs_copy = lhs_add.shallow_copy().as<Nodecl::Mul>();

                        n_copy.get_lhs().replace(lhs_rhs.shallow_copy());
                        lhs_copy.get_rhs().replace(n_copy);

                        n.replace(lhs_copy);
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

                // R2: This rule applys independetly from the kind of lhs
                // Re-check again everything, the node could have changed
                if (n.is<Nodecl::VectorAdd>())
                {
                    lhs = n.get_lhs();
                    rhs = n.get_rhs();
                    
                    if (!lhs.is_constant() && rhs.is_constant())
                    {
                        n.replace(Nodecl::VectorAdd::make(rhs.shallow_copy(), lhs.shallow_copy(), 
                                    n.get_mask().shallow_copy(), lhs.get_type(), n.get_locus()));
                    }
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
                        Nodecl::NodeclBase c = Nodecl::VectorAdd::make(lhs.shallow_copy(), rhs_lhs.shallow_copy(), mask.shallow_copy(), lhs.get_type());
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

            // The node could have changed
            if (n.is<Nodecl::VectorAdd>())
            {
                lhs = n.get_lhs();
                rhs = n.get_rhs();

                // R50a: 
                // TODO: This rule is changing the order of the operations (as many other).
                //       Maybe it should be apply on floating point data only when fast-math or -O3 is enabled
                if ((lhs.is<Nodecl::VectorAdd>()))
                {
                    Nodecl::VectorAdd lhs_add = lhs.as<Nodecl::VectorAdd>();
                    Nodecl::NodeclBase lhs_lhs = lhs_add.get_lhs();
                    Nodecl::NodeclBase lhs_rhs = lhs_add.get_rhs();

                    if (lhs_lhs.is_constant())
                    {
                        Nodecl::VectorAdd n_copy = n.shallow_copy().as<Nodecl::VectorAdd>();
                        Nodecl::VectorAdd lhs_copy = lhs_add.shallow_copy().as<Nodecl::VectorAdd>();

                        n_copy.get_lhs().replace(lhs_rhs.shallow_copy());
                        lhs_copy.get_rhs().replace(n_copy);

                        n.replace(lhs_copy);
                    }
                }
                // R51a: 
                // TODO: This rule is changing the order of the operations (as many other).
                //       Maybe it should be apply on floating point data only when fast-math or -O3 is enabled
                else if ((lhs.is<Nodecl::VectorMinus>()))
                {
                    Nodecl::VectorMinus lhs_sub = lhs.as<Nodecl::VectorMinus>();
                    Nodecl::NodeclBase lhs_lhs = lhs_sub.get_lhs();
                    Nodecl::NodeclBase lhs_rhs = lhs_sub.get_rhs();

                    if (lhs_lhs.is_constant())
                    {
                        Nodecl::VectorMinus lhs_copy = lhs_sub.shallow_copy().as<Nodecl::VectorMinus>();
                        Nodecl::VectorMinus new_rhs = Nodecl::VectorMinus::make(
                                lhs_rhs.shallow_copy(), rhs.shallow_copy(), n.get_mask(),
                                n.get_type(), n.get_locus());

                        lhs_copy.get_rhs().replace(new_rhs);
                        n.replace(lhs_copy);
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

    void ReduceExpressionVisitor::visit_post(const Nodecl::VectorBitwiseShr& n)
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

            // The node could have changed
            if (n.is<Nodecl::VectorMinus>())
            {
                lhs = n.get_lhs();
                rhs = n.get_rhs();

                // R50b: 
                // TODO: This rule is changing the order of the operations (as many other).
                //       Maybe it should be apply on floating point data only when fast-math or -O3 is enabled
                if ((lhs.is<Nodecl::VectorMinus>()))
                {
                    Nodecl::VectorMinus lhs_sub = lhs.as<Nodecl::VectorMinus>();
                    Nodecl::NodeclBase lhs_lhs = lhs_sub.get_lhs();
                    Nodecl::NodeclBase lhs_rhs = lhs_sub.get_rhs();

                    if (lhs_lhs.is_constant())
                    {
                        Nodecl::VectorMinus lhs_copy = lhs_sub.shallow_copy().as<Nodecl::VectorMinus>();
                        Nodecl::VectorAdd new_rhs = Nodecl::VectorAdd::make(
                                lhs_rhs.shallow_copy(), rhs.shallow_copy(), n.get_mask(),
                                n.get_type(), n.get_locus());

                        lhs_copy.get_rhs().replace(new_rhs);
                        n.replace(lhs_copy);
                    }
                }
                // R51b: 
                // TODO: This rule is changing the order of the operations (as many other).
                //       Maybe it should be apply on floating point data only when fast-math or -O3 is enabled
                else if ((lhs.is<Nodecl::VectorAdd>()))
                {
                    Nodecl::VectorAdd lhs_add = lhs.as<Nodecl::VectorAdd>();
                    Nodecl::NodeclBase lhs_lhs = lhs_add.get_lhs();
                    Nodecl::NodeclBase lhs_rhs = lhs_add.get_rhs();

                    if (lhs_lhs.is_constant())
                    {
                        Nodecl::VectorAdd lhs_copy = lhs_add.shallow_copy().as<Nodecl::VectorAdd>();
                        Nodecl::VectorMinus new_rhs = Nodecl::VectorMinus::make(
                                lhs_rhs.shallow_copy(), rhs.shallow_copy(), n.get_mask(),
                                n.get_type(), n.get_locus());

                        lhs_copy.get_rhs().replace(new_rhs);
                        n.replace(lhs_copy);
                    }
                }
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
                    
                    // R2: This rule applys independetly from the kind of lhs
                    // Re-check again everything, the node could have changed
                    if (n.is<Nodecl::VectorMul>())
                    {
                        lhs = n.get_lhs();
                        rhs = n.get_rhs();

                        if (!lhs.is_constant() && rhs.is_constant())
                        {
                            n.replace(Nodecl::VectorMul::make(rhs.shallow_copy(), lhs.shallow_copy(), 
                                        n.get_mask().shallow_copy(), lhs.get_type(), n.get_locus()));
                        }
                    }
                }
            }

            // The node could have changed
            if (n.is<Nodecl::VectorMul>())
            {

                // R50c: 
                // TODO: This rule is changing the order of the operations (as many other).
                //       Maybe it should be apply on floating point data only when fast-math or -O3 is enabled
                if ((lhs.is<Nodecl::VectorMul>()))
                {
                    Nodecl::VectorMul lhs_add = lhs.as<Nodecl::VectorMul>();
                    Nodecl::NodeclBase lhs_lhs = lhs_add.get_lhs();
                    Nodecl::NodeclBase lhs_rhs = lhs_add.get_rhs();

                    if (lhs_lhs.is_constant())
                    {
                        Nodecl::VectorMul n_copy = n.shallow_copy().as<Nodecl::VectorMul>();
                        Nodecl::VectorMul lhs_copy = lhs_add.shallow_copy().as<Nodecl::VectorMul>();

                        n_copy.get_lhs().replace(lhs_rhs.shallow_copy());
                        lhs_copy.get_rhs().replace(n_copy);

                        n.replace(lhs_copy);
                    }
                }
            }
        }
    }

    UnitaryReductor::UnitaryReductor()
    {
    }

    void UnitaryReductor::print_unitary_rhss()
    {
        std::cerr << "Unitary rhss: " << std::endl;
        for(TL::ObjectList<Nodecl::NodeclBase>::const_iterator it =
                _unitary_rhss.begin();
                it != _unitary_rhss.end();
                it++)
        {
            std::cerr << it->prettyprint() << std::endl;
        }

        std::cerr << std::endl;
    }
 
    bool UnitaryReductor::is_leaf_node(
            const Nodecl::NodeclBase& n)
    {
        if (n.is<Nodecl::Symbol>() || n.is<Nodecl::Mul>() ||
                n.is<Nodecl::Div>() || n.is<Nodecl::Mod>())
            return true;

        return false;
    }

    void UnitaryReductor::nullify_nodecl(
            const Nodecl::NodeclBase& n)
    {
        TL::ObjectList<Nodecl::NodeclBase>::iterator it =
            Nodecl::Utils::list_get_nodecl_by_structure(
                    _unitary_rhss, n);

        if (it != _unitary_rhss.end())
        {
            // Nullify nodecl from lhs (visitor)
            n.replace(const_value_to_nodecl(
                        const_value_get_zero(4, 1)));

            // Nullify nodecl from rhs (list)
            Nodecl::NodeclBase rhs_it = *it;
            _unitary_rhss.erase(it);

            rhs_it.replace(const_value_to_nodecl(
                        const_value_get_zero(4, 1)));

            Nodecl::NodeclBase rhs_it_parent = rhs_it.get_parent();

            // Remove Conversion?
            if ((!rhs_it_parent.is_null()) && rhs_it_parent.is<Nodecl::Conversion>())
            {
                TL::Type dst_type = rhs_it_parent.get_type().no_ref();
                TL::Type src_type = rhs_it.get_type().no_ref(); 

                if (dst_type.is_same_type(src_type))
                    rhs_it_parent.replace(rhs_it);
            }
        }
    }
 
    void UnitaryReductor::reduce(
            const Nodecl::Minus& n)
    {
        Nodecl::NodeclBase lhs = n.get_lhs().no_conv();
        Nodecl::NodeclBase rhs = n.get_rhs().no_conv();

        if (Nodecl::Utils::structurally_equal_nodecls(lhs, rhs, true /*skip conversions*/))
        {
            n.replace(const_value_to_nodecl(const_value_get_zero(/*num_bytes*/ 4, /*sign*/1)));
            return;
        }

        MinusRemover minus_remover;
        minus_remover.walk(lhs);
        minus_remover.walk(rhs);

        UnitaryDecomposer decomp;
        _unitary_rhss = decomp.walk(rhs);

        //print_unitary_rhss();

        walk(lhs);

        if (rhs.is_constant() && lhs.is_constant())
        {
            n.replace(const_value_to_nodecl(const_value_sub(
                        lhs.get_constant(), rhs.get_constant())));
        }

        TL::Optimizations::canonicalize_and_fold(n, false /*fast_math*/);
//        TL::Optimizations::ReduceExpressionVisitor reduce_expr_visitor;
//        reduce_expr_visitor.walk(n);

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

        if (n.is<Nodecl::Add>())
        {
            lhs = n.get_lhs().no_conv();
            rhs = n.get_rhs().no_conv();

            if (rhs.is_constant() && lhs.is_constant())
            {
                n.replace(const_value_to_nodecl(const_value_add(
                                lhs.get_constant(), rhs.get_constant())));
            }
        }

        _unitary_rhss.clear();
    }

    void UnitaryReductor::visit(const Nodecl::Conversion& n)
    {
        walk(n.get_nest());
        
        TL::Type dst_type = n.get_type().no_ref();
        TL::Type src_type = n.get_nest().get_type().no_ref();

        bool is_integer_post = n.get_nest().is<Nodecl::IntegerLiteral>();

        //if (is_symbol_pre && is_integer_post)
        if (is_integer_post && (src_type.is_same_type(dst_type)))
            n.replace(n.get_nest());
    }

    void UnitaryReductor::visit(const Nodecl::Add& n)
    {
        Nodecl::NodeclBase lhs = n.get_lhs();
        Nodecl::NodeclBase rhs = n.get_rhs();

        walk(lhs);
        walk(rhs);

        if (rhs.is_constant() && lhs.is_constant())
        {
            n.replace(const_value_to_nodecl(const_value_add(
                        lhs.get_constant(), rhs.get_constant())));
        }
    }

    void UnitaryReductor::visit(const Nodecl::Neg& n)
    {
        nullify_nodecl(n);
    }

    void UnitaryReductor::visit(const Nodecl::Mul& n)
    {
        nullify_nodecl(n);
    }

    void UnitaryReductor::visit(const Nodecl::BitwiseShl& n)
    {
        nullify_nodecl(n);
    }

    void UnitaryReductor::visit(const Nodecl::Div& n)
    {
        nullify_nodecl(n);
    }

    void UnitaryReductor::visit(const Nodecl::BitwiseShr& n)
    {
        nullify_nodecl(n);
    }

    void UnitaryReductor::visit(const Nodecl::Mod& n)
    {
        nullify_nodecl(n);
    }

    void UnitaryReductor::visit(const Nodecl::Symbol& n)
    {
        nullify_nodecl(n);
    }

    void UnitaryReductor::visit(const Nodecl::IntegerLiteral& n)
    {
        nullify_nodecl(n);
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

    UnitaryDecomposer::Ret UnitaryDecomposer::visit(const Nodecl::Neg& n)
    {
        UnitaryDecomposer::Ret result;
        result.append(n);
        return result;
    }

    UnitaryDecomposer::Ret UnitaryDecomposer::visit(const Nodecl::Mul& n)
    {
        UnitaryDecomposer::Ret result;
        result.append(n);
        return result;
    }

    UnitaryDecomposer::Ret UnitaryDecomposer::visit(const Nodecl::BitwiseShl& n)
    {
        UnitaryDecomposer::Ret result;
        result.append(n);
        return result;
    }

    UnitaryDecomposer::Ret UnitaryDecomposer::visit(const Nodecl::Div& n)
    {
        UnitaryDecomposer::Ret result;
        result.append(n);
        return result;
    }

    UnitaryDecomposer::Ret UnitaryDecomposer::visit(const Nodecl::BitwiseShr& n)
    {
        UnitaryDecomposer::Ret result;
        result.append(n);
        return result;
    }

    UnitaryDecomposer::Ret UnitaryDecomposer::visit(const Nodecl::Mod& n)
    {
        UnitaryDecomposer::Ret result;
        result.append(n);
        return result;
    }

    UnitaryDecomposer::Ret UnitaryDecomposer::visit(const Nodecl::Symbol& n)
    {
        UnitaryDecomposer::Ret result;
        result.append(n);
        return result;
    }

    UnitaryDecomposer::Ret UnitaryDecomposer::visit(const Nodecl::IntegerLiteral& n)
    {
        UnitaryDecomposer::Ret result;
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

    MinusRemover::MinusRemover()
    {
    }

    MinusRemover::Ret MinusRemover::visit(const Nodecl::Minus& n)
    {
        // turn -(a + b - c) into -a + -b - -c

        Nodecl::NodeclBase rhs = n.get_rhs();

        Nodecl::Neg rhs_neg = Nodecl::Neg::make(
                rhs,
                n.get_type(),
                n.get_locus());

        if (rhs.is_constant())
        {
            rhs_neg.set_constant(rhs.get_constant());
        }

        Nodecl::Add add = Nodecl::Add::make(
                n.get_lhs(),
                rhs_neg,
                n.get_type(),
                n.get_locus());

        n.replace(add);

        walk(n);
    }

    MinusRemover::Ret MinusRemover::visit(const Nodecl::Neg& n)
    {
        UnitaryDecomposer decomp;
        UnitaryDecomposer::Ret rhs_decomp = decomp.walk(n.get_rhs());

        // turn -(a + b - c) into -a + -b - -c
        for(UnitaryDecomposer::Ret::iterator it = rhs_decomp.begin();
                it != rhs_decomp.end();
                it++)
        {
            Nodecl::Neg neg_node = Nodecl::Neg::make(
                    it->shallow_copy(), it->get_type());
            
            if (it->is_constant())
                neg_node.set_constant(
                        const_value_neg(it->get_constant()));

            it->replace(neg_node);
        }
    
        // Remove enclosing Neg
        n.replace(n.get_rhs());
    }
}
}
