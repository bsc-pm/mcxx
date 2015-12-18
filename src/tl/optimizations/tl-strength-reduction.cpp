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

#include "tl-strength-reduction.hpp"
#include "tl-expression-reduction.hpp"

#include "tl-nodecl-utils.hpp"
#include "cxx-cexpr.h"
#include <math.h>

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
    else if(rhs.is_constant() && rhs_type.is_floating_type())
    {
        const_value_t * cv = rhs.get_constant();
        float const_float = const_value_cast_to_float(rhs.get_constant());

        int exp;
        double mantissa = frexp(const_float, &exp);

        if (mantissa == 0x1p-1 ||   // If mantissa is power of 2, the transformation is exact
                _fast_math)          // 0x1p-1 == 1 * 2^-1
        {
            // a / c --> a * 1/c
            Nodecl::Mul mul =
                Nodecl::Mul::make(
                        lhs.shallow_copy(),
                        const_value_to_nodecl(const_value_div(
                                const_value_get_one(4, 1), cv)),
                        node.get_type(),
                        node.get_locus());

            node.replace(mul);
        }
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

    if(node.get_type().is_unsigned_int()
            && rhs.is_constant())
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
                Nodecl::VectorBitwiseShl shl =
                    Nodecl::VectorBitwiseShl::make(
                            rhs.shallow_copy(),
                            Nodecl::VectorPromotion::make(
                                const_value_to_nodecl(const_value_get_signed_int(ctz)),
                                node.get_mask().shallow_copy(),
                                node.get_type()),
                            node.get_mask().shallow_copy(),
                            node.get_type(),
                            node.get_locus());

                node.replace(shl);
            }
        }
        else if (lhs.is<Nodecl::VectorLiteral>())
        {
            //TODO: It could be a different type than int
            Nodecl::List const_list = lhs.as<Nodecl::VectorLiteral>().
                get_scalar_values().as<Nodecl::List>();
            int size = const_list.size();

            bool all_pow2 = true;
            const_value_t** value_set = new const_value_t*[size];

            int i = 0;
            for(Nodecl::List::const_iterator it = const_list.begin();
                    it != const_list.end();
                    it++, i++)
            {
                int integer_value = const_value_cast_to_signed_int(
                        it->as<Nodecl::IntegerLiteral>().get_constant());

                // C * V, C == Pow2
                if (__builtin_popcount(integer_value) == 1) //Pow2
                {
                    int ctz = __builtin_ctz(integer_value);

                    value_set[i] = const_value_get_signed_int(ctz);
                }
                else
                {
                    all_pow2 = false;
                    break;
                }
            }

            if (all_pow2)
            {
                const_value_t* const_result = const_value_make_vector(size, value_set);
                Nodecl::List offset_list = const_value_to_nodecl(const_result);

                // lhs << (cv>>ctz)
                Nodecl::VectorBitwiseShl shl =
                    Nodecl::VectorBitwiseShl::make(
                            rhs.shallow_copy(),
                            Nodecl::VectorLiteral::make(offset_list,
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
                Nodecl::VectorArithmeticShr shl =
                    Nodecl::VectorArithmeticShr::make(
                            lhs.shallow_copy(),
                            Nodecl::VectorPromotion::make(
                                const_value_to_nodecl(const_value_get_signed_int(ctz)),
                                node.get_mask().shallow_copy(),
                                node.get_type()),
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
            // Optimize 1 * rcp
            walk(mul);
        }
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


void TL::Optimizations::StrengthReduction::visit(const Nodecl::VectorSqrt& node)
{
    walk(node.get_rhs());

    TL::Type type = node.get_type().basic_type();

    if(_fast_math) // RSQRT
    {
        Nodecl::NodeclBase one_vector_node;
        if (type.is_floating_type())
        {
            Nodecl::NodeclBase one_node = const_value_to_nodecl(
                    const_value_cast_to_floating_type_value(
                        const_value_get_double(1.0), type.get_internal_type()));

            one_vector_node = Nodecl::VectorPromotion::make(one_node,
                    node.get_mask().shallow_copy(),
                    node.get_type(),
                    node.get_locus());

            one_vector_node.set_constant(const_value_make_vector_from_scalar(
                        node.get_type().vector_num_elements(),
                        one_node.get_constant()));
        }
        else
        {
            internal_error("Unsupported type\n", 0);
        }

        
        Nodecl::VectorDiv div =
            Nodecl::VectorDiv::make(
                    one_vector_node,
                    Nodecl::VectorRsqrt::make(
                        node.get_rhs().shallow_copy(),
                        node.get_mask().shallow_copy(),
                        node.get_type()),
                    node.get_mask().shallow_copy(),
                    node.get_type(),
                    node.get_locus());

        node.replace(div);

        // Optimize 1 * rsqrt
        walk(div);
    }
}
