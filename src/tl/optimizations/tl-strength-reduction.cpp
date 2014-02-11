/*--------------------------------------------------------------------
  (C) Copyright 2006-2013 Barcelona Supercomputing Center
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

#include "cxx-cexpr.h"
#include "tl-nodecl-utils.hpp"
#include "tl-strength-reduction.hpp"
#include "tl-expression-reduction.hpp"

TL::Optimizations::StrengthReduction::StrengthReduction(bool fast_math)
    : _fast_math(fast_math)
{}

void TL::Optimizations::StrengthReduction::visit(const Nodecl::ObjectInit& n)
{
    TL::Symbol sym = n.get_symbol();

    // Visit initialization
    Nodecl::NodeclBase init = sym.get_value();
    if(!init.is_null())
    {
        walk(init);
    }
}

void TL::Optimizations::StrengthReduction::visit(const Nodecl::Mul& node)
{
    Nodecl::NodeclBase lhs = node.get_lhs();
    Nodecl::NodeclBase rhs = node.get_rhs();

    walk(lhs);
    walk(rhs);

    TL::Type lhs_type = lhs.get_type();
    TL::Type rhs_type = rhs.get_type();

    if(lhs.is_constant() && lhs_type.is_integral_type())
    {
        //TODO: It could be a different type than int
        const_value_t * cv = lhs.get_constant(); 
        int integer_value = const_value_cast_to_signed_int(cv);

        // 3 * V
        /*
        if (integer_value == 3)
        {
            Nodecl::Add add3 = 
                Nodecl::Add::make(
                        
                        Nodecl::BitwiseShl::make(
                            rhs.shallow_copy(),
                            const_value_to_nodecl(const_value_get_one(4, 1)),
                            node.get_type(),
                            node.get_locus()),
                        
                        
                        Nodecl::Add::make(
                            rhs.shallow_copy(),
                            rhs.shallow_copy(),
                            node.get_type(),
                            node.get_locus()),
                        
                        rhs.shallow_copy(),
                        node.get_type(),
                        node.get_locus());

            node.replace(add3);
        }
        */

        // C * V, C == Pow2
        if (__builtin_popcount(integer_value) == 1) //Pow2
        {
            int ctz = __builtin_ctz(integer_value);

            // lhs << (cv>>ctz)
            Nodecl::BitwiseShl shl = 
                Nodecl::BitwiseShl::make(
                        rhs.shallow_copy(),
                        const_value_to_nodecl(const_value_get_signed_int(ctz)),
                        node.get_type(),
                        node.get_locus());

            node.replace(shl);
        }
    }
}

void TL::Optimizations::StrengthReduction::visit(const Nodecl::Div& node)
{
    Nodecl::NodeclBase lhs = node.get_lhs();
    Nodecl::NodeclBase rhs = node.get_rhs();

    walk(lhs);
    walk(rhs);

    TL::Type lhs_type = lhs.get_type();
    TL::Type rhs_type = rhs.get_type();

    if(rhs.is_constant() && rhs_type.is_integral_type())
    {
        //TODO: It could be a different type than int
        const_value_t * cv = rhs.get_constant(); 
        int integer_value = const_value_cast_to_signed_int(cv);

        if (__builtin_popcount(integer_value) == 1) //Pow2
        {
            int ctz = __builtin_ctz(integer_value);

            // lhs << (cv>>ctz)
            Nodecl::ArithmeticShr shr = 
                Nodecl::ArithmeticShr::make(
                        lhs.shallow_copy(),
                        const_value_to_nodecl(const_value_get_signed_int(ctz)),
                        node.get_type(),
                        node.get_locus());

            node.replace(shr);
        }
    }
    else if(_fast_math) // RCP
    {
    }
}

void TL::Optimizations::StrengthReduction::visit(const Nodecl::Mod& node)
{
    Nodecl::NodeclBase lhs = node.get_lhs();
    Nodecl::NodeclBase rhs = node.get_rhs();

    walk(lhs);
    walk(rhs);

    TL::Type lhs_type = lhs.get_type();
    TL::Type rhs_type = rhs.get_type();

<<<<<<< HEAD
    if(rhs.is_constant() && rhs_type.is_integral_type()
            && lhs_type.is_unsigned_int())
    {
        //TODO: It could be a different type than int
        const_value_t * cv = rhs.get_constant(); 
        int integer_value = const_value_cast_to_signed_int(cv);

        if (__builtin_popcount(integer_value) == 1) //Pow2
        {
            int mask = integer_value-1;

            // lhs << (cv>>ctz)
            Nodecl::BitwiseAnd bw_and = 
                Nodecl::BitwiseAnd::make(
                        lhs.shallow_copy(),
                        const_value_to_nodecl(const_value_get_signed_int(mask)),
                        node.get_type(),
                        node.get_locus());

            node.replace(bw_and);
        }
    }
}
 


void TL::Optimizations::StrengthReduction::visit(const Nodecl::VectorAdd& node)
{
    Nodecl::NodeclBase lhs = node.get_lhs();
    Nodecl::NodeclBase rhs = node.get_rhs();

    walk(lhs);
    walk(rhs);

    TL::Type lhs_type = lhs.get_type().basic_type();
    TL::Type rhs_type = rhs.get_type().basic_type();

    // Scalarize ops between vector promotions
    if(lhs.is<Nodecl::VectorPromotion>() && rhs.is<Nodecl::VectorPromotion>())
    {
        node.replace(Nodecl::VectorPromotion::make(
                    Nodecl::Add::make(lhs.as<Nodecl::VectorPromotion>().get_rhs().shallow_copy(),
                        rhs.as<Nodecl::VectorPromotion>().get_rhs().shallow_copy(),
                        node.get_type().vector_element(),
                        node.get_locus()),
                    node.get_mask().shallow_copy(),
                    node.get_type(),
                    node.get_locus()));
    }
}

void TL::Optimizations::StrengthReduction::visit(const Nodecl::VectorMinus& node)
{
    Nodecl::NodeclBase lhs = node.get_lhs();
    Nodecl::NodeclBase rhs = node.get_rhs();

    walk(lhs);
    walk(rhs);

    // Scalarize ops between vector promotions
    if(lhs.is<Nodecl::VectorPromotion>() && rhs.is<Nodecl::VectorPromotion>())
    {
        node.replace(Nodecl::VectorPromotion::make(
                    Nodecl::Minus::make(lhs.as<Nodecl::VectorPromotion>().get_rhs().shallow_copy(),
                        rhs.as<Nodecl::VectorPromotion>().get_rhs().shallow_copy(),
                        node.get_type().vector_element(),
                        node.get_locus()),
                    node.get_mask().shallow_copy(),
                    node.get_type(),
                    node.get_locus()));
    }
}

void TL::Optimizations::StrengthReduction::visit(const Nodecl::VectorMul& node)
{
    Nodecl::NodeclBase lhs = node.get_lhs();
    Nodecl::NodeclBase rhs = node.get_rhs();

    walk(lhs);
    walk(rhs);

    TL::Type lhs_type = lhs.get_type().basic_type();
    TL::Type rhs_type = rhs.get_type().basic_type();

    // Scalarize ops between vector promotions
    if(lhs.is<Nodecl::VectorPromotion>() && rhs.is<Nodecl::VectorPromotion>())
    {
        node.replace(Nodecl::VectorPromotion::make(
                    Nodecl::Mul::make(lhs.as<Nodecl::VectorPromotion>().get_rhs().shallow_copy(),
                        rhs.as<Nodecl::VectorPromotion>().get_rhs().shallow_copy(),
                        node.get_type().vector_element(),
                        node.get_locus()),
                    node.get_mask().shallow_copy(),
                    node.get_type(),
                    node.get_locus()));
    }
    else if(lhs.is_constant() && lhs_type.is_integral_type())
    {
        if (lhs.is<Nodecl::VectorPromotion>())
        {
            //TODO: It could be a different type than int
            const_value_t * cv = lhs.as<Nodecl::VectorPromotion>().get_rhs().get_constant(); 
            int integer_value = const_value_cast_to_signed_int(cv);

            // C * V, C == Pow2
            if (__builtin_popcount(integer_value) == 1) //Pow2
            {
                int ctz = __builtin_ctz(integer_value);

                // lhs << (cv>>ctz)
                Nodecl::VectorBitwiseShlI shl = 
                    Nodecl::VectorBitwiseShlI::make(
                            rhs.shallow_copy(),
                            const_value_to_nodecl(const_value_get_signed_int(ctz)),
                            node.get_mask().shallow_copy(),
                            node.get_type(),
                            node.get_locus());

                node.replace(shl);
            }
        }
        else if (lhs.is<Nodecl::VectorLiteral>())
        {
            //TODO: It could be a different type than int
            Nodecl::List const_list = lhs.as<Nodecl::VectorLiteral>().get_scalar_values().as<Nodecl::List>();
            Nodecl::List result_list;

            bool all_pow2 = true;

            for(Nodecl::List::const_iterator it = const_list.begin();
                    it != const_list.end();
                    it++)
            {
                int integer_value = const_value_cast_to_signed_int(it->as<Nodecl::IntegerLiteral>().get_constant());

                // C * V, C == Pow2
                if (__builtin_popcount(integer_value) == 1) //Pow2
                {
                    int ctz = __builtin_ctz(integer_value);

                    result_list.prepend(Nodecl::IntegerLiteral::make(
                                TL::Type::get_int_type(),
                                const_value_get_signed_int(ctz),
                                it->get_locus()));
                }
                else
                {
                    all_pow2 = false;
                    break;
                }
            }

            if (all_pow2)
            {
                // lhs << (cv>>ctz)
                Nodecl::VectorBitwiseShl shl = 
                    Nodecl::VectorBitwiseShl::make(
                            rhs.shallow_copy(),
                            Nodecl::VectorLiteral::make(result_list,
                                node.get_mask().shallow_copy(),
                                node.get_type()),
                            node.get_mask().shallow_copy(),
                            node.get_type(),
                            node.get_locus());

                node.replace(shl);
            }

        }
    }
}
=======
        }
    }
}
 


void TL::Optimizations::StrengthReduction::visit(const Nodecl::VectorAdd& node)
{
    Nodecl::NodeclBase lhs = node.get_lhs();
    Nodecl::NodeclBase rhs = node.get_rhs();

    walk(lhs);
    walk(rhs);

    TL::Type lhs_type = lhs.get_type().basic_type();
    TL::Type rhs_type = rhs.get_type().basic_type();

    // Scalarize ops between vector promotions
    if(lhs.is<Nodecl::VectorPromotion>() && rhs.is<Nodecl::VectorPromotion>())
    {
        node.replace(Nodecl::VectorPromotion::make(
                    Nodecl::Add::make(lhs.as<Nodecl::VectorPromotion>().get_rhs().shallow_copy(),
                        rhs.as<Nodecl::VectorPromotion>().get_rhs().shallow_copy(),
                        node.get_type().vector_element(),
                        node.get_locus()),
                    node.get_mask().shallow_copy(),
                    node.get_type(),
                    node.get_locus()));
    }
}

void TL::Optimizations::StrengthReduction::visit(const Nodecl::VectorMinus& node)
{
    Nodecl::NodeclBase lhs = node.get_lhs();
    Nodecl::NodeclBase rhs = node.get_rhs();

    walk(lhs);
    walk(rhs);

    // Scalarize ops between vector promotions
    if(lhs.is<Nodecl::VectorPromotion>() && rhs.is<Nodecl::VectorPromotion>())
    {
        node.replace(Nodecl::VectorPromotion::make(
                    Nodecl::Minus::make(lhs.as<Nodecl::VectorPromotion>().get_rhs().shallow_copy(),
                        rhs.as<Nodecl::VectorPromotion>().get_rhs().shallow_copy(),
                        node.get_type().vector_element(),
                        node.get_locus()),
                    node.get_mask().shallow_copy(),
                    node.get_type(),
                    node.get_locus()));
    }
}

void TL::Optimizations::StrengthReduction::visit(const Nodecl::VectorMul& node)
{
    Nodecl::NodeclBase lhs = node.get_lhs();
    Nodecl::NodeclBase rhs = node.get_rhs();

    walk(lhs);
    walk(rhs);

    TL::Type lhs_type = lhs.get_type().basic_type();
    TL::Type rhs_type = rhs.get_type().basic_type();

    // Scalarize ops between vector promotions
    if(lhs.is<Nodecl::VectorPromotion>() && rhs.is<Nodecl::VectorPromotion>())
    {
        node.replace(Nodecl::VectorPromotion::make(
                    Nodecl::Mul::make(lhs.as<Nodecl::VectorPromotion>().get_rhs().shallow_copy(),
                        rhs.as<Nodecl::VectorPromotion>().get_rhs().shallow_copy(),
                        node.get_type().vector_element(),
                        node.get_locus()),
                    node.get_mask().shallow_copy(),
                    node.get_type(),
                    node.get_locus()));
    }
    else if(lhs.is_constant() && lhs_type.is_integral_type())
    {
        if (lhs.is<Nodecl::VectorPromotion>())
        {
            //TODO: It could be a different type than int
            const_value_t * cv = lhs.as<Nodecl::VectorPromotion>().get_rhs().get_constant(); 
            int integer_value = const_value_cast_to_signed_int(cv);

            // C * V, C == Pow2
            if (__builtin_popcount(integer_value) == 1) //Pow2
            {
                int ctz = __builtin_ctz(integer_value);

                // lhs << (cv>>ctz)
                Nodecl::VectorBitwiseShlI shl = 
                    Nodecl::VectorBitwiseShlI::make(
                            rhs.shallow_copy(),
                            const_value_to_nodecl(const_value_get_signed_int(ctz)),
                            node.get_mask().shallow_copy(),
                            node.get_type(),
                            node.get_locus());

                node.replace(shl);
            }
        }
        else if (lhs.is<Nodecl::VectorLiteral>())
        {
            //TODO: It could be a different type than int
            Nodecl::List const_list = lhs.as<Nodecl::VectorLiteral>().get_scalar_values().as<Nodecl::List>();
            Nodecl::List result_list;

            bool all_pow2 = true;

            for(Nodecl::List::const_iterator it = const_list.begin();
                    it != const_list.end();
                    it++)
            {
                int integer_value = const_value_cast_to_signed_int(it->as<Nodecl::IntegerLiteral>().get_constant());

                // C * V, C == Pow2
                if (__builtin_popcount(integer_value) == 1) //Pow2
                {
                    int ctz = __builtin_ctz(integer_value);

                    result_list.prepend(Nodecl::IntegerLiteral::make(
                                TL::Type::get_int_type(),
                                const_value_get_signed_int(ctz),
                                it->get_locus()));
                }
                else
                {
                    all_pow2 = false;
                    break;
                }
            }

            if (all_pow2)
            {
                // lhs << (cv>>ctz)
                Nodecl::VectorBitwiseShl shl = 
                    Nodecl::VectorBitwiseShl::make(
                            rhs.shallow_copy(),
                            Nodecl::VectorLiteral::make(result_list,
                                node.get_mask().shallow_copy(),
                                node.get_type()),
                            node.get_mask().shallow_copy(),
                            node.get_type(),
                            node.get_locus());

                node.replace(shl);
            }

        }
    }
}
>>>>>>> analysis


void TL::Optimizations::StrengthReduction::visit(const Nodecl::VectorDiv& node)
{
    Nodecl::NodeclBase lhs = node.get_lhs();
    Nodecl::NodeclBase rhs = node.get_rhs();

    walk(lhs);
    walk(rhs);

    TL::Type lhs_type = lhs.get_type().basic_type();
    TL::Type rhs_type = rhs.get_type().basic_type();
 
    // Scalarize ops between vector promotions
    if(lhs.is<Nodecl::VectorPromotion>() && rhs.is<Nodecl::VectorPromotion>())
    {
        node.replace(Nodecl::VectorPromotion::make(
                    Nodecl::Div::make(lhs.as<Nodecl::VectorPromotion>().get_rhs().shallow_copy(),
                        rhs.as<Nodecl::VectorPromotion>().get_rhs().shallow_copy(),
                        node.get_type().vector_element(),
                        node.get_locus()),
                    node.get_mask().shallow_copy(),
                    node.get_type(),
                    node.get_locus()));
    }
    else if(rhs.is_constant() && rhs_type.is_integral_type())
    {
        if (rhs.is<Nodecl::VectorPromotion>())
        {
            //TODO: It could be a different type than int
            const_value_t * cv = rhs.as<Nodecl::VectorPromotion>().get_rhs().get_constant(); 
            int integer_value = const_value_cast_to_signed_int(cv);

            // C * V, C == Pow2
            if (__builtin_popcount(integer_value) == 1) //Pow2
            {
                int ctz = __builtin_ctz(integer_value);

                // lhs >> (cv>>ctz)
                Nodecl::VectorArithmeticShrI shl = 
                    Nodecl::VectorArithmeticShrI::make(
                            lhs.shallow_copy(),
                            const_value_to_nodecl(const_value_get_signed_int(ctz)),
                            node.get_mask().shallow_copy(),
                            node.get_type(),
                            node.get_locus());

                node.replace(shl);
            }
        }
        else if (rhs.is<Nodecl::VectorLiteral>())
        {
            //TODO: It could be a different type than int
            Nodecl::List const_list = rhs.as<Nodecl::VectorLiteral>().get_scalar_values().as<Nodecl::List>();
            Nodecl::List result_list;

            bool all_pow2 = true;

            for(Nodecl::List::const_iterator it = const_list.begin();
                    it != const_list.end();
                    it++)
            {
                int integer_value = const_value_cast_to_signed_int(it->as<Nodecl::IntegerLiteral>().get_constant());

                // C * V, C == Pow2
                if (__builtin_popcount(integer_value) == 1) //Pow2
                {
                    int ctz = __builtin_ctz(integer_value);

                    result_list.prepend(Nodecl::IntegerLiteral::make(
                                TL::Type::get_int_type(),
                                const_value_get_signed_int(ctz),
                                it->get_locus()));
                }
                else
                {
                    all_pow2 = false;
                    break;
                }
            }

            if (all_pow2)
            {
                // lhs >> (cv>>ctz)
<<<<<<< HEAD
                Nodecl::VectorArithmeticShr shl = 
                    Nodecl::VectorArithmeticShr::make(
                            lhs.shallow_copy(),
                            Nodecl::VectorLiteral::make(result_list,
                                node.get_mask().shallow_copy(),
                                node.get_type()),
                            node.get_mask().shallow_copy(),
                            node.get_type(),
                            node.get_locus());

                node.replace(shl);
            }
        }
    }
    else if(_fast_math) // RSQRT
    {
        if (rhs.is<Nodecl::VectorSqrt>())
        {
            Nodecl::VectorSqrt vsqrt = rhs.as<Nodecl::VectorSqrt>();

            Nodecl::VectorMul mul = 
                Nodecl::VectorMul::make(
                        lhs.shallow_copy(),
                        Nodecl::VectorRsqrt::make(
                            vsqrt.get_rhs().shallow_copy(),
                            node.get_mask().shallow_copy(),
                            vsqrt.get_type()),
                        node.get_mask().shallow_copy(),
                        node.get_type(),
                        node.get_locus());

            node.replace(mul);

            // Optimize 1 * rsqrt
            walk(mul);
        }
        /*
        else
        {
            Nodecl::VectorMul mul = 
                Nodecl::VectorMul::make(
                        lhs.shallow_copy(),
                        Nodecl::VectorRcp::make(
                            rhs.shallow_copy(),
                            node.get_mask().shallow_copy(),
                            rhs.get_type()),
                        node.get_mask().shallow_copy(),
                        node.get_type(),
                        node.get_locus());

            node.replace(mul);
        }
        */
    }
}

void TL::Optimizations::StrengthReduction::visit(const Nodecl::VectorMod& node)
{
    Nodecl::NodeclBase lhs = node.get_lhs();
    Nodecl::NodeclBase rhs = node.get_rhs();

    walk(lhs);
    walk(rhs);

    TL::Type lhs_type = lhs.get_type().basic_type();
    TL::Type rhs_type = rhs.get_type().basic_type();
    
    // Scalarize ops between vector promotions
    if(lhs.is<Nodecl::VectorPromotion>() && rhs.is<Nodecl::VectorPromotion>())
    {
        node.replace(Nodecl::VectorPromotion::make(
                    Nodecl::Mod::make(lhs.as<Nodecl::VectorPromotion>().get_rhs().shallow_copy(),
                        rhs.as<Nodecl::VectorPromotion>().get_rhs().shallow_copy(),
                        node.get_type().vector_element(),
                        node.get_locus()),
                    node.get_mask().shallow_copy(),
                    node.get_type(),
                    node.get_locus()));
    }
}
 
