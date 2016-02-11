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

#include "tl-vector-backend-avx2.hpp"

#include "tl-vectorization-utils.hpp"
#include "tl-source.hpp"
#include "tl-nodecl-utils.hpp"
#include "cxx-cexpr.h"
#include "cxx-intelsupport.h"


#include "cxx-cexpr.h"
#include "cxx-diagnostic.h"

#define AVX2_VECTOR_BIT_SIZE 256
#define AVX2_VECTOR_BYTE_SIZE 32
#define AVX2_INTRIN_PREFIX "_mm256"
#define AVX2_MASK_BIT_SIZE 0


namespace AVX2Comparison
{

const int EQ_OQ    = 0x00;
const int LT_OS    = 0x01;
const int LE_OS    = 0x02;
const int UNORD_Q  = 0x03;
const int NEQ_UQ   = 0x04;
const int NLT_US   = 0x05;
const int NLE_US   = 0x06;
const int ORD_Q    = 0x07;
const int EQ_UQ    = 0x08;
const int NGE_US   = 0x09;
const int NGT_US   = 0x0A;
const int FALSE_OQ = 0x0B;
const int NEQ_OQ   = 0x0C;
const int GE_OS    = 0x0D;
const int GT_OS    = 0x0E;
const int TRUE_UQ  = 0x0F;
const int EQ_OS    = 0x10;
const int LT_OQ    = 0x11;
const int LE_OQ    = 0x12;
const int UNORD_S  = 0x13;
const int NEQ_US   = 0x14;
const int NLT_UQ   = 0x15;
const int NLE_UQ   = 0x16;
const int ORD_S    = 0x17;
const int EQ_US    = 0x18;
const int NGE_UQ   = 0x19;
const int NGT_UQ   = 0x1A;
const int FALSE_OS = 0x1B;
const int NEQ_OS   = 0x1C;
const int GE_OQ    = 0x1D;
const int GT_OQ    = 0x1E;
const int TRUE_US  = 0x1F;

};

namespace AVX2MM
{
const int HINT_NONE = 0x0;
const int HINT_NT   = 0x1;

const int FROUND_CUR_DIRECTION  = 0x04;
};

namespace TL
{

namespace
{
TL::Symbol get_m256i_symbol()
{
    TL::Symbol s
        = TL::Scope::get_global_scope().get_symbol_from_name("__m256i");
    ERROR_CONDITION(!s.is_valid(), "_m256i not found", 0);
    return s;
}
}

namespace Vectorization
{
    namespace {
        TL::Type avx2_comparison_type()
        {
            return TL::Type::get_int_type().get_vector_of_elements(8);
        }
    }

    AVX2VectorLowering::AVX2VectorLowering()
        : _vectorizer(TL::Vectorization::Vectorizer::get_vectorizer()),
        _vector_length(AVX2_VECTOR_BYTE_SIZE)
    {
        std::cerr << "--- AVX2 backend phase ---" << std::endl;
    }

    std::string AVX2VectorLowering::get_casting_intrinsic(const TL::Type& type_from,
            const TL::Type& type_to,
            const locus_t* locus)
    {
        std::stringstream result;

        if (type_from.is_float())
        {
            if(!type_to.is_float())
            {
                if (type_to.is_double())
                {
                    result << AVX2_INTRIN_PREFIX << "_castps_pd";
                }
                else if (type_to.is_signed_int() || type_to.is_unsigned_int())
                {
                    result << AVX2_INTRIN_PREFIX << "_castps_si" <<
                        AVX2_VECTOR_BIT_SIZE;
                }
            }
        }
        else if (type_from.is_signed_int() || type_from.is_unsigned_int())
        {
            if ((!type_to.is_signed_int()) && (!type_to.is_unsigned_int()))
            {

                if (type_to.is_float())
                {
                    result << AVX2_INTRIN_PREFIX << "_castsi" <<
                        AVX2_VECTOR_BIT_SIZE << "_ps";
                }
                else if (type_to.is_double())
                {
                    result << AVX2_INTRIN_PREFIX << "_castsi" <<
                        AVX2_VECTOR_BIT_SIZE << "_pd";
                }
            }
        }
        else
        {
            fatal_printf_at(locus,
                    "AVX2 Lowering: casting from %s to %s is not supported",
                    print_type_str(type_from.get_internal_type(), CURRENT_COMPILED_FILE->global_decl_context),
                    print_type_str(type_to.get_internal_type(), CURRENT_COMPILED_FILE->global_decl_context));
        }

        return result.str();
    }

    std::string AVX2VectorLowering::get_casting_to_scalar_pointer(const TL::Type& type_to)
    {
        std::stringstream result;

        result << "("
            << print_type_str(
                    type_to.get_pointer_to().get_internal_type(),
                    CURRENT_COMPILED_FILE->global_decl_context)
            << ")";

        return result.str();
    }

    void AVX2VectorLowering::visit(const Nodecl::ObjectInit& node)
    {
        TL::Source intrin_src;

        if(node.has_symbol())
        {
            TL::Symbol sym = node.get_symbol();

            // Vectorizing initialization
            Nodecl::NodeclBase init = sym.get_value();
            if(!init.is_null())
            {
                walk(init);
            }
        }
    }

#define UNSUPPORTED_MASK(node) \
        fatal_printf_at(node.get_locus(), \
                "AVX2 Backend: Vector masks are not supported in AVX2 (node=%s).", \
                ast_print_node_type((node).get_kind()))


    void AVX2VectorLowering::common_binary_op_lowering(const Nodecl::NodeclBase& node,
            const std::string& intrin_op_name)
    {
        const Nodecl::VectorAdd& binary_node = node.as<Nodecl::VectorAdd>();

        const Nodecl::NodeclBase lhs = binary_node.get_lhs();
        const Nodecl::NodeclBase rhs = binary_node.get_rhs();
        const Nodecl::NodeclBase mask = binary_node.get_mask();
        if (!mask.is_null())
        {
            UNSUPPORTED_MASK(node);
        }

        TL::Type type = binary_node.get_type().basic_type();

        TL::Source intrin_src, intrin_name, intrin_type_suffix, args;

        intrin_src << intrin_name
            << "("
            << args
            << ")"
            ;

        intrin_name << AVX2_INTRIN_PREFIX
            << "_"
            << intrin_op_name
            << "_"
            << intrin_type_suffix
            ;

        Source cast_operand;

        if (type.is_float())
        {
            intrin_type_suffix << "ps";
        }
        else if (type.is_double())
        {
            intrin_type_suffix << "pd";
        }
        else if (type.is_signed_int() ||
                type.is_unsigned_int())
        {
            cast_operand << "("
                         << as_type(get_m256i_symbol().get_user_defined_type())
                         << ")";
            intrin_type_suffix << "epi32";
        }
        else if (type.is_signed_long_long_int() ||
                type.is_unsigned_long_long_int())
        {
            intrin_type_suffix << "epi64";
        }
        else
        {
            fatal_printf_at(node.get_locus(),
                    "AVX2 Lowering: Node %s at %s has an unsupported type: %s.",
                    ast_print_node_type(binary_node.get_kind()),
                    locus_to_str(binary_node.get_locus()),
                    type.get_simple_declaration(node.retrieve_context(), "").c_str());
        }

        walk(lhs);
        walk(rhs);

        args << cast_operand << as_expression(lhs) << ", " << cast_operand
             << as_expression(rhs);

        Nodecl::NodeclBase function_call =
            intrin_src.parse_expression(node.retrieve_context());

        node.replace(function_call);
    }

    void AVX2VectorLowering::common_unary_op_lowering(const Nodecl::NodeclBase& node,
            const std::string& intrin_op_name)
    {
        const Nodecl::VectorRsqrt& unary_node = node.as<Nodecl::VectorRsqrt>();

        const Nodecl::NodeclBase rhs = unary_node.get_rhs();
        const Nodecl::NodeclBase mask = unary_node.get_mask();

        if (!mask.is_null())
        {
            UNSUPPORTED_MASK(node);
        }

        TL::Type type = unary_node.get_type().basic_type();

        TL::Source intrin_src, intrin_name, intrin_type_suffix,
            args;

        intrin_src << intrin_name
            << "("
            << args
            << ")"
            ;

        intrin_name << AVX2_INTRIN_PREFIX
            << "_"
            << intrin_op_name
            << "_"
            << intrin_type_suffix
            ;

        if (type.is_float())
        {
            intrin_type_suffix << "ps";
        }
        else if (type.is_double())
        {
            intrin_type_suffix << "pd";
        }
        else if (type.is_signed_int() ||
                type.is_unsigned_int())
        {
            intrin_type_suffix << "si" << AVX2_VECTOR_BIT_SIZE;
        }
        else
        {
            fatal_printf_at(node.get_locus(),
                    "AVX2 Lowering: Node %s at %s has an unsupported type: %s.",
                    ast_print_node_type(unary_node.get_kind()),
                    locus_to_str(unary_node.get_locus()),
                    type.get_simple_declaration(node.retrieve_context(), "").c_str());
        }

        walk(rhs);

        args << as_expression(rhs);

        Nodecl::NodeclBase function_call =
            intrin_src.parse_expression(node.retrieve_context());

        node.replace(function_call);
    }

    void AVX2VectorLowering::bitwise_binary_op_lowering(const Nodecl::NodeclBase& node,
            const std::string& intrin_op_name)
    {
        const Nodecl::VectorBitwiseAnd& binary_node = node.as<Nodecl::VectorBitwiseAnd>();

        const Nodecl::NodeclBase lhs = binary_node.get_lhs();
        const Nodecl::NodeclBase rhs = binary_node.get_rhs();
        const Nodecl::NodeclBase mask = binary_node.get_mask();

        if (!mask.is_null())
        {
            UNSUPPORTED_MASK(node);
        }

        TL::Type type = binary_node.get_type().basic_type();

        TL::Source intrin_src, intrin_name, intrin_type_suffix,
            args;

        intrin_src 
            << intrin_name
            << "("
            << args
            << ")"
            ;

        intrin_name << AVX2_INTRIN_PREFIX
            << "_"
            << intrin_op_name
            << "_"
            << intrin_type_suffix
            ;

        if (type.is_float())
        {
            intrin_type_suffix << "ps";
        }
        else if (type.is_double())
        {
            intrin_type_suffix << "pd";
        }
        else if (type.is_integral_type())
        {
            intrin_type_suffix << "si" << AVX2_VECTOR_BIT_SIZE;
        }
        else
        {
            fatal_printf_at(node.get_locus(),
                    "AVX2 Lowering: Node %s at %s has an unsupported type.",
                    ast_print_node_type(binary_node.get_kind()),
                    locus_to_str(binary_node.get_locus()));
        }

        walk(lhs);
        walk(rhs);

        args << "(" << as_expression(lhs) << ")"
            << ", "
            << "(" << as_expression(rhs) << ")"
            ;

        Nodecl::NodeclBase function_call =
            intrin_src.parse_expression(node.retrieve_context());

        node.replace(function_call);
    }

    void AVX2VectorLowering::visit(const Nodecl::VectorAdd& node)
    {
        common_binary_op_lowering(node, "add");
    }

    void AVX2VectorLowering::visit(const Nodecl::VectorMinus& node)
    {
        common_binary_op_lowering(node, "sub");
    }

    void AVX2VectorLowering::visit(const Nodecl::VectorMul& node)
    {
        TL::Type type = node.get_type().basic_type();

        if (type.is_integral_type())
            common_binary_op_lowering(node, "mullo");
        else
            common_binary_op_lowering(node, "mul");
    }

    void AVX2VectorLowering::visit(const Nodecl::VectorDiv& node)
    {
        common_binary_op_lowering(node, "div");
    }

    void AVX2VectorLowering::visit(const Nodecl::VectorMod& node)
    {
        common_binary_op_lowering(node, "rem");
    }

    void AVX2VectorLowering::visit(const Nodecl::VectorSqrt& node)
    {
        common_unary_op_lowering(node, "sqrt");
    }

    void AVX2VectorLowering::visit(const Nodecl::VectorRsqrt& node)
    {
        common_unary_op_lowering(node, "invsqrt");
    }

    void AVX2VectorLowering::visit(const Nodecl::VectorFmadd& node)
    {
        const Nodecl::NodeclBase first_op = node.get_first_op();
        const Nodecl::NodeclBase second_op = node.get_second_op();
        const Nodecl::NodeclBase third_op = node.get_third_op();
        const Nodecl::NodeclBase mask = node.get_mask();

        if (!mask.is_null())
        {
            UNSUPPORTED_MASK(node);
        }

        TL::Type type = node.get_type().basic_type();

        TL::Source intrin_src, intrin_name, intrin_type_suffix,
            mask_prefix, args, mask_args;

        intrin_src << intrin_name
            << "("
            << args
            << ")"
            ;

        intrin_name << AVX2_INTRIN_PREFIX
            << mask_prefix
            << "_"
            << "fmadd_round"
            << "_"
            << intrin_type_suffix
            ;

        if (type.is_float())
        {
            intrin_type_suffix << "ps";
        }
        else if (type.is_double())
        {
            intrin_type_suffix << "pd";
        }
        else if (type.is_signed_int() ||
                type.is_unsigned_int())
        {
            intrin_type_suffix << "si" << AVX2_VECTOR_BIT_SIZE;
        }
        else
        {
            fatal_printf_at(node.get_locus(),
                    "AVX2 Lowering: Node %s at %s has an unsupported type.",
                    ast_print_node_type(node.get_kind()),
                    locus_to_str(node.get_locus()));
        }

        walk(first_op);
        walk(second_op);
        walk(third_op);

        args << as_expression(first_op)
            << ", "
            << mask_args
            << as_expression(second_op)
            << ", "
            << as_expression(third_op)
            << ", "
            << AVX2MM::FROUND_CUR_DIRECTION
            ;

        Nodecl::NodeclBase function_call =
            intrin_src.parse_expression(node.retrieve_context());

        node.replace(function_call);
    }

    void AVX2VectorLowering::visit(const Nodecl::VectorLowerThan& node)
    {
        const TL::Type type = node.get_lhs().get_type().basic_type();
        bool turn;

        TL::Source intrin_src, intrin_name;
        TL::Source cmp_flavor;

        // Intrinsic name
        intrin_name << AVX2_INTRIN_PREFIX << "_cmp";

        if (type.is_float())
        {
            intrin_name << "_ps";
            cmp_flavor << ", " << AVX2Comparison::LT_OS;
            turn = false;
        }
        else if (type.is_signed_int() ||
                type.is_unsigned_int())
        {
            intrin_name << "gt_epi32";
            turn = true;
        }
        else
        {
            fatal_printf_at(node.get_locus(),
                    "AVX2 Lowering: Node %s at %s has an unsupported type.",
                    ast_print_node_type(node.get_kind()),
                    locus_to_str(node.get_locus()));
        }

        walk(node.get_lhs());
        walk(node.get_rhs());

        if (turn)
        {
            intrin_src
                << intrin_name
                << "("
                << as_expression(node.get_rhs())
                << ", "
                << as_expression(node.get_lhs())
                << cmp_flavor
                << ")";
        }
        else
        {
            intrin_src
                << get_casting_intrinsic(type, TL::Type::get_int_type(), node.get_locus())
                << "("
                << intrin_name
                << "("
                << as_expression(node.get_lhs())
                << ", "
                << as_expression(node.get_rhs())
                << cmp_flavor
                << "))";
        }

        Nodecl::NodeclBase function_call =
            intrin_src.parse_expression(node.retrieve_context());

        node.replace(function_call);
    }

    void AVX2VectorLowering::visit(const Nodecl::VectorLowerOrEqualThan& node)
    {
        const TL::Type type = node.get_lhs().get_type().basic_type();

        TL::Source intrin_src, cmp_intrin_suffix;
        TL::Source cmp_flavor;
        bool turn;

        if (type.is_float())
        {
            cmp_intrin_suffix << "_ps";
            cmp_flavor << ", " << AVX2Comparison::LE_OS;
            turn = false;
        }
        else if (type.is_signed_int() ||
                type.is_unsigned_int())
        {
            cmp_intrin_suffix << "_epi32";
            turn = true;
        }
        else
        {
            fatal_printf_at(node.get_locus(),
                    "AVX2 Lowering: Node %s at %s has an unsupported type.",
                    ast_print_node_type(node.get_kind()),
                    locus_to_str(node.get_locus()));
        }

        walk(node.get_lhs());
        walk(node.get_rhs());

        if (turn)
        {
            intrin_src
                << AVX2_INTRIN_PREFIX << "_or_si" << AVX2_VECTOR_BIT_SIZE
                << "("
                << AVX2_INTRIN_PREFIX << "_cmpgt" << cmp_intrin_suffix
                << "("
                << as_expression(node.get_rhs())
                << ", "
                << as_expression(node.get_lhs())
                << "),"
                << AVX2_INTRIN_PREFIX << "_cmpeq" << cmp_intrin_suffix
                << "("
                << as_expression(node.get_lhs())
                << ", "
                << as_expression(node.get_rhs())
                << "))"
                ;
        }
        else
        {
            intrin_src
                << get_casting_intrinsic(type, TL::Type::get_int_type(), node.get_locus())
                << "("
                << AVX2_INTRIN_PREFIX << "_cmp" << cmp_intrin_suffix
                << "("
                << as_expression(node.get_lhs())
                << ", "
                << as_expression(node.get_rhs())
                << cmp_flavor
                << "))";
        }

        Nodecl::NodeclBase function_call =
            intrin_src.parse_expression(node.retrieve_context());

        node.replace(function_call);
    }

    void AVX2VectorLowering::visit(const Nodecl::VectorGreaterThan& node)
    {
        const TL::Type type = node.get_lhs().get_type().basic_type();

        TL::Source intrin_src, intrin_name;
        TL::Source cmp_flavor;

        // Intrinsic name
        intrin_name << AVX2_INTRIN_PREFIX << "_cmp";

        if (type.is_float())
        {
            intrin_name << "_ps";
            cmp_flavor << ", " << AVX2Comparison::GT_OS;
        }
        else if (type.is_signed_int() ||
                type.is_unsigned_int())
        {
            intrin_name << "gt_epi32";
        }
        else
        {
            fatal_printf_at(node.get_locus(),
                    "AVX2 Lowering: Node %s at %s has an unsupported type.",
                    ast_print_node_type(node.get_kind()),
                    locus_to_str(node.get_locus()));
        }

        walk(node.get_lhs());
        walk(node.get_rhs());

        intrin_src
            << get_casting_intrinsic(type, TL::Type::get_int_type(), node.get_locus())
            << "("
            << intrin_name
            << "("
            << as_expression(node.get_lhs())
            << ", "
            << as_expression(node.get_rhs())
            << cmp_flavor
            << "))";

        Nodecl::NodeclBase function_call =
            intrin_src.parse_expression(node.retrieve_context());

        node.replace(function_call);
    }

    void AVX2VectorLowering::visit(const Nodecl::VectorGreaterOrEqualThan& node)
    {
        const TL::Type type = node.get_lhs().get_type().basic_type();

        TL::Source intrin_src, cmp_intrin_suffix;
        TL::Source cmp_flavor;
        bool turn;

        // Intrinsic name
        if (type.is_float())
        {
            cmp_intrin_suffix << "_ps";
            cmp_flavor << ", " << AVX2Comparison::GE_OS;
            turn = false;
        }
        else if (type.is_signed_int() ||
                type.is_unsigned_int())
        {
            cmp_intrin_suffix << "_epi32";
            turn = true;
        }
        else
        {
            fatal_printf_at(node.get_locus(),
                    "AVX2 Lowering: Node %s at %s has an unsupported type.",
                    ast_print_node_type(node.get_kind()),
                    locus_to_str(node.get_locus()));
        }

        walk(node.get_lhs());
        walk(node.get_rhs());

        if (turn)
        {
            intrin_src
                << AVX2_INTRIN_PREFIX << "_or_si" << AVX2_VECTOR_BIT_SIZE
                << "("
                << AVX2_INTRIN_PREFIX << "_cmpgt" << cmp_intrin_suffix
                << "("
                << as_expression(node.get_lhs())
                << ", "
                << as_expression(node.get_rhs())
                << "),"
                << AVX2_INTRIN_PREFIX << "_cmpeq" << cmp_intrin_suffix
                << "("
                << as_expression(node.get_lhs())
                << ", "
                << as_expression(node.get_rhs())
                << "))"
                ;
        }
        else
        {
            intrin_src
                << get_casting_intrinsic(type, TL::Type::get_int_type(), node.get_locus())
                << "("
                << AVX2_INTRIN_PREFIX << "_cmp" << cmp_intrin_suffix
                << "("
                << as_expression(node.get_lhs())
                << ", "
                << as_expression(node.get_rhs())
                << cmp_flavor
                << "))";
        }

        Nodecl::NodeclBase function_call =
            intrin_src.parse_expression(node.retrieve_context());

        node.replace(function_call);
    }

    void AVX2VectorLowering::visit(const Nodecl::VectorEqual& node)
    {
        const TL::Type type = node.get_lhs().get_type().basic_type();

        TL::Source intrin_src, intrin_name;
        TL::Source cmp_flavor;

        // Intrinsic name
        intrin_name << AVX2_INTRIN_PREFIX << "_cmp";

        if (type.is_float())
        {
            intrin_name << "_ps";
            cmp_flavor << ", " << AVX2Comparison::EQ_OQ;
        }
        else if (type.is_signed_int() ||
                type.is_unsigned_int())
        {
            intrin_name << "eq_epi32";
        }
        else
        {
            fatal_printf_at(node.get_locus(),
                    "AVX2 Lowering: Node %s at %s has an unsupported type.",
                    ast_print_node_type(node.get_kind()),
                    locus_to_str(node.get_locus()));
        }

        walk(node.get_lhs());
        walk(node.get_rhs());

        intrin_src
            << get_casting_intrinsic(type, TL::Type::get_int_type(), node.get_locus())
            << "("
            << intrin_name
            << "("
            << as_expression(node.get_lhs())
            << ", "
            << as_expression(node.get_rhs())
            << cmp_flavor
            << "))"
            ;

        Nodecl::NodeclBase function_call =
            intrin_src.parse_expression(node.retrieve_context());

        node.replace(function_call);
    }

    void AVX2VectorLowering::visit(const Nodecl::VectorDifferent& node)
    {
        const TL::Type type = node.get_lhs().get_type().basic_type();

        TL::Source intrin_src, cmp_intrin_suffix;
        TL::Source cmp_flavor;
        bool turn;

        if (type.is_float())
        {
            cmp_intrin_suffix << "_ps";
            cmp_flavor << ", " << AVX2Comparison::NEQ_UQ;
            turn = false;
        }
        else if (type.is_signed_int() ||
                type.is_unsigned_int())
        {
            cmp_intrin_suffix << "_epi32";
            turn = true;
        }
        else
        {
            fatal_printf_at(node.get_locus(),
                    "AVX2 Lowering: Node %s at %s has an unsupported type.",
                    ast_print_node_type(node.get_kind()),
                    locus_to_str(node.get_locus()));
        }

        walk(node.get_lhs());
        walk(node.get_rhs());

        if (turn)
        {
            intrin_src
                << AVX2_INTRIN_PREFIX << "_xor_si" << AVX2_VECTOR_BIT_SIZE
                << "("
                << AVX2_INTRIN_PREFIX << "_cmpeq" << cmp_intrin_suffix
                << "("
                << as_expression(node.get_lhs())
                << ", "
                << as_expression(node.get_rhs())
                << "),"
                << AVX2_INTRIN_PREFIX << "_cmpeq" << cmp_intrin_suffix
                << "("
                << as_expression(node.get_lhs())
                << ", "
                << as_expression(node.get_lhs())
                << "))"
                ;
        }
        else
        {
            intrin_src
                << get_casting_intrinsic(type, TL::Type::get_int_type(), node.get_locus())
                << "("
                << AVX2_INTRIN_PREFIX << "_cmp" << cmp_intrin_suffix
                << "("
                << as_expression(node.get_lhs())
                << ", "
                << as_expression(node.get_rhs())
                << cmp_flavor
                << "))";
        }

        Nodecl::NodeclBase function_call =
            intrin_src.parse_expression(node.retrieve_context());

        node.replace(function_call);
    }

    void AVX2VectorLowering::visit(const Nodecl::VectorBitwiseAnd& node)
    {
        bitwise_binary_op_lowering(node, "and");
    }

    void AVX2VectorLowering::visit(const Nodecl::VectorBitwiseOr& node)
    {
        bitwise_binary_op_lowering(node, "or");
    }

    void AVX2VectorLowering::visit(const Nodecl::VectorBitwiseXor& node)
    {
        bitwise_binary_op_lowering(node, "xor");
    }

    void AVX2VectorLowering::visit(const Nodecl::VectorBitwiseShl& node)
    {
        const Nodecl::NodeclBase lhs = node.get_lhs();
        const Nodecl::NodeclBase rhs = node.get_rhs();
        const Nodecl::NodeclBase mask = node.get_mask();

        if (!mask.is_null())
        {
            UNSUPPORTED_MASK(node);
        }

        TL::Type type = node.get_type().basic_type();

        TL::Source intrin_src, intrin_name, intrin_type_suffix, intrin_op_name,
            mask_prefix, args, mask_args, rhs_expression;

        intrin_src
            << "("
            << intrin_name
            << "("
            << args
            << "))"
            ;

        intrin_name << AVX2_INTRIN_PREFIX
            << mask_prefix
            << "_"
            << intrin_op_name
            << "_"
            << intrin_type_suffix
            ;

        if (type.is_signed_int() ||
                type.is_unsigned_int())
        {
            intrin_type_suffix << "si" << AVX2_VECTOR_BIT_SIZE;
        }
        else
        {
            fatal_printf_at(node.get_locus(),
                    "AVX2 Lowering: Node %s at %s has an unsupported type.",
                    ast_print_node_type(node.get_kind()),
                    locus_to_str(node.get_locus()));
        }

        walk(lhs);

        //RHS
        if (rhs.is<Nodecl::VectorPromotion>())
        {
            intrin_op_name << "slli";
            rhs_expression << as_expression(rhs.as<Nodecl::VectorPromotion>().get_rhs());
        }
        else
        {
            intrin_op_name << "sllv";
            walk(rhs);
            rhs_expression << as_expression(rhs);
        }

        args << mask_args
            << "(" << as_expression(lhs) << ")"
            << ", "
            << "(" << rhs_expression << ")"
            ;

        Nodecl::NodeclBase function_call =
            intrin_src.parse_expression(node.retrieve_context());

        node.replace(function_call);
    }

    void AVX2VectorLowering::visit(const Nodecl::VectorArithmeticShr& node)
    {
        const Nodecl::NodeclBase lhs = node.get_lhs();
        const Nodecl::NodeclBase rhs = node.get_rhs();
        const Nodecl::NodeclBase mask = node.get_mask();

        if (!mask.is_null())
        {
            UNSUPPORTED_MASK(node);
        }

        TL::Type type = node.get_type().basic_type();

        TL::Source intrin_src, intrin_name, intrin_type_suffix, intrin_op_name,
            mask_prefix, args, mask_args, rhs_expression;

        intrin_src
            << "("
            << intrin_name
            << "("
            << args
            << "))"
            ;

        intrin_name << AVX2_INTRIN_PREFIX
            << mask_prefix
            << "_"
            << intrin_op_name
            << "_"
            << intrin_type_suffix
            ;

        if (type.is_signed_int() ||
                type.is_unsigned_int())
        {
            intrin_type_suffix << "epi32";
        }
        else
        {
            fatal_printf_at(node.get_locus(),
                    "AVX2 Lowering: Node %s at %s has an unsupported type.",
                    ast_print_node_type(node.get_kind()),
                    locus_to_str(node.get_locus()));
        }

        walk(lhs);

        //RHS
        Nodecl::NodeclBase rhs_without_conversions = Nodecl::Utils::advance_conversions(rhs);
        if (rhs_without_conversions.is<Nodecl::VectorPromotion>())
        {
            intrin_op_name << "srai";
            rhs_expression << as_expression(rhs_without_conversions.as<Nodecl::VectorPromotion>().get_rhs());
        }
        else
        {
            intrin_op_name << "srav";
            walk(rhs);
            rhs_expression << as_expression(rhs);
        }

        args << mask_args
            << "(" << as_expression(lhs) << ")"
            << ", "
            << "(" << rhs_expression << ")"
            ;

        Nodecl::NodeclBase function_call =
            intrin_src.parse_expression(node.retrieve_context());

        node.replace(function_call);
    }


    void AVX2VectorLowering::visit(const Nodecl::VectorBitwiseShr& node)
    {
        const Nodecl::NodeclBase lhs = node.get_lhs();
        const Nodecl::NodeclBase rhs = node.get_rhs();
        const Nodecl::NodeclBase mask = node.get_mask();

        if (!mask.is_null())
        {
            UNSUPPORTED_MASK(node);
        }

        TL::Type type = node.get_type().basic_type();

        TL::Source intrin_src, intrin_name, intrin_type_suffix, intrin_op_name,
            mask_prefix, args, mask_args, rhs_expression;

        intrin_src
            << "("
            << intrin_name
            << "("
            << args
            << "))"
            ;

        intrin_name << AVX2_INTRIN_PREFIX
            << mask_prefix
            << "_"
            << intrin_op_name
            << "_"
            << intrin_type_suffix
            ;

        if (type.is_signed_int() ||
                type.is_unsigned_int())
        {
            intrin_type_suffix << "si" << AVX2_VECTOR_BIT_SIZE;
        }
        else
        {
            fatal_printf_at(node.get_locus(),
                    "AVX2 Lowering: Node %s at %s has an unsupported type.",
                    ast_print_node_type(node.get_kind()),
                    locus_to_str(node.get_locus()));
        }

        walk(lhs);

        //RHS
        Nodecl::NodeclBase rhs_without_conversions = Nodecl::Utils::advance_conversions(rhs);
        if (rhs_without_conversions.is<Nodecl::VectorPromotion>())
        {
            intrin_op_name << "srli";
            rhs_expression << as_expression(rhs_without_conversions.as<Nodecl::VectorPromotion>().get_rhs());
        }
        else
        {
            intrin_op_name << "srlv";
            walk(rhs);
            rhs_expression << as_expression(rhs);
        }

        args << mask_args
            << "(" << as_expression(lhs) << ")"
            << ", "
            << "(" << rhs_expression << ")"
            ;

        Nodecl::NodeclBase function_call =
            intrin_src.parse_expression(node.retrieve_context());

        node.replace(function_call);
    }

    void AVX2VectorLowering::visit(const Nodecl::VectorLogicalOr& node)
    {
        fatal_printf_at(node.get_locus(),
                "AVX2 Lowering %s: 'logical or' operation (i.e., operator '||') is not "
                "supported in AVX2. Try using 'bitwise or' operations (i.e., operator '|') instead if possible.",
                locus_to_str(node.get_locus()));
    }

    void AVX2VectorLowering::visit(const Nodecl::VectorAlignRight& node)
    {

        //ALIGNR_256 macro works with arbitrary offset, eg ALIGNR_256(ret, a, b, 1, 4)
        // r: result. (v0, v1): array. offs: offset of 1st element. size: element size in bytes.
#if 0
#define ALIGNR_256(r, v0, v1, offs, size) \
        if (offs == 0) \
        r = v0; \
        else if (offs == 32 / size) \
        r = v1; \
        else \
        { \
            r = _mm256_permute2x128_si256(v0, v1, 0x21); \
            \
            if (offs > 16 / size) \
            r = _mm256_alignr_epi8(v1, r, offs * size & ~16); \
            else if (offs < 16 / size) \
            r = _mm256_alignr_epi8(r, v0, offs * size); \
        }
#endif
        const Nodecl::NodeclBase left_vector = node.get_left_vector();
        const Nodecl::NodeclBase right_vector = node.get_right_vector();
        const Nodecl::NodeclBase num_elements = node.get_num_elements();
        const Nodecl::NodeclBase mask = node.get_mask();

        if (!mask.is_null())
        {
            UNSUPPORTED_MASK(node);
        }

        TL::Type type = node.get_type().basic_type();

        TL::Source intrin_src, intrin_name, intrin_type_suffix, intrin_op_name,
            mask_prefix, args, mask_args, rhs_expression;

        intrin_src << get_casting_intrinsic(TL::Type::get_int_type(), type, node.get_locus())
            << "("
            << intrin_name
            << "("
            << args
            << "))"
            ;

        intrin_name << AVX2_INTRIN_PREFIX
            << mask_prefix
            << "_"
            << intrin_op_name
            << "_"
            << intrin_type_suffix
            ;

        intrin_op_name << "alignr";
        intrin_type_suffix << "epi8";

        walk(left_vector);
        walk(right_vector);
        walk(num_elements);

        args << mask_args
            << get_casting_intrinsic(type, TL::Type::get_int_type(), node.get_locus()) << "(" << as_expression(left_vector) << ")"
            << ", "
            << get_casting_intrinsic(type, TL::Type::get_int_type(), node.get_locus()) << "(" << as_expression(right_vector) << ")"
            << ", "
            << as_expression(num_elements)
            ;

        Nodecl::NodeclBase function_call =
            intrin_src.parse_expression(node.retrieve_context());

        node.replace(function_call);
    }

    void AVX2VectorLowering::visit(const Nodecl::VectorNeg& node)
    {
        Nodecl::NodeclBase rhs = node.get_rhs();
        Nodecl::NodeclBase mask = node.get_mask();

        TL::Type type = node.get_type().basic_type();

        TL::Source intrin_src, neg_op;

        intrin_src << get_casting_intrinsic(TL::Type::get_int_type(), type, node.get_locus())
            << "("
            << neg_op
            << ")"
            ;

        if (type.is_float())
        {
            TL::Type vector_int_type =
                TL::Type::get_int_type().get_vector_of_elements(_vector_length);

            Nodecl::VectorBitwiseXor vector_xor =
                Nodecl::VectorBitwiseXor::make(
                        Nodecl::VectorPromotion::make(
                            Nodecl::IntegerLiteral::make(
                                TL::Type::get_int_type(),
                                const_value_get_signed_int(0x80000000)),
                            mask.shallow_copy(),
                            vector_int_type),
                        Nodecl::VectorCast::make(
                            rhs.shallow_copy(),
                            mask.shallow_copy(),
                            vector_int_type),
                        mask.shallow_copy(),
                        vector_int_type);

            walk(vector_xor);

            neg_op << as_expression(vector_xor);
        }
        else if (type.is_double())
        {
            TL::Type vector_int_type =
                TL::Type::get_long_long_int_type().get_vector_of_bytes(_vector_length);

            Nodecl::VectorBitwiseXor vector_xor =
                Nodecl::VectorBitwiseXor::make(
                        Nodecl::VectorPromotion::make(
                            Nodecl::IntegerLiteral::make(
                                TL::Type::get_long_long_int_type(),
                                const_value_get_signed_int(0x8000000000000000LL)),
                            mask.shallow_copy(),
                            vector_int_type),
                        Nodecl::VectorCast::make(
                            rhs.shallow_copy(),
                            mask.shallow_copy(),
                            vector_int_type),
                        mask.shallow_copy(),
                        vector_int_type);

            walk(vector_xor);

            neg_op << as_expression(vector_xor);
        }
        else if (type.is_signed_int() ||
                type.is_unsigned_int())
        {
            TL::Type vector_int_type =
                TL::Type::get_int_type().get_vector_of_bytes(_vector_length);

            Nodecl::VectorMinus vector_minus =
                Nodecl::VectorMinus::make(
                        Nodecl::VectorPromotion::make(
                            Nodecl::IntegerLiteral::make(
                                TL::Type::get_int_type(),
                                const_value_get_zero(4 , 1)),
                            mask.shallow_copy(),
                            vector_int_type),
                        rhs.shallow_copy(),
                        mask.shallow_copy(),
                        vector_int_type);

            walk(vector_minus);

            neg_op << as_expression(vector_minus);
        }
        else
        {
            fatal_printf_at(node.get_locus(),
                    "AVX2 Lowering: Node %s at %s has an unsupported type.",
                    ast_print_node_type(node.get_kind()),
                    locus_to_str(node.get_locus()));
        }

        Nodecl::NodeclBase function_call =
            intrin_src.parse_expression(node.retrieve_context());

        node.replace(function_call);
    }

    void AVX2VectorLowering::visit(const Nodecl::VectorConversion& node)
    {
        const TL::Type& src_type = node.get_nest().get_type().basic_type().get_unqualified_type();
        const TL::Type& dst_type = node.get_type().basic_type().get_unqualified_type();

        TL::Source intrin_src;

        walk(node.get_nest());

        if (src_type.is_same_type(dst_type))
        {
            node.replace(node.get_nest());
            return;
        }
        else if (src_type.is_integral_type() && dst_type.is_integral_type()
                 && src_type.get_size() == dst_type.get_size())
        {
            // This is a no-op conversion
            node.replace(node.get_nest());
            return;
        }
        else if (src_type.is_signed_int() &&
                dst_type.is_float())
        {
            intrin_src << AVX2_INTRIN_PREFIX << "_cvtepi32_ps"
                << "("
                << as_expression(node.get_nest())
                << ")";
        }
        else if (src_type.is_float() &&
                dst_type.is_signed_int())
        {
            // C/C++ requires truncated conversion
            intrin_src << AVX2_INTRIN_PREFIX << "_cvttps_epi32"
                << "("
                << as_expression(node.get_nest())
                << ")";
        }
        else
        {
            fprintf(stderr,
                    "AVX2 Lowering: Conversion at '%s' is not supported yet: "
                    "%s -> %s\n",
                    locus_to_str(node.get_locus()),
                    print_declarator(src_type.get_internal_type()),
                    print_declarator(dst_type.get_internal_type()));
        }

        Nodecl::NodeclBase function_call =
            intrin_src.parse_expression(node.retrieve_context());

        node.replace(function_call);
    }

    void AVX2VectorLowering::visit(const Nodecl::VectorCast& node)
    {
        const Nodecl::NodeclBase rhs = node.get_rhs();

        const TL::Type& dst_vector_type = node.get_type().get_unqualified_type().no_ref();
        const TL::Type& dst_type = dst_vector_type.basic_type().get_unqualified_type();
        const TL::Type& src_type = rhs.get_type().basic_type().get_unqualified_type();

        TL::Source intrin_src, args;

        walk(rhs);

        intrin_src
            << get_casting_intrinsic(src_type, dst_type, node.get_locus())
            << "("
            << as_expression(rhs)
            << ")"
            ;

        Nodecl::NodeclBase function_call =
            intrin_src.parse_expression(node.retrieve_context());

        node.replace(function_call);
    }

    void AVX2VectorLowering::visit(const Nodecl::VectorPromotion& node)
    {
        TL::Type type = node.get_type().basic_type();

        TL::Source intrin_src;

        // Intrinsic name
        intrin_src << AVX2_INTRIN_PREFIX << "_set1";

        if (type.is_float())
        {
            intrin_src << "_ps";
        }
        else if (type.is_double())
        {
            intrin_src << "_pd";
        }
        else if (type.is_signed_int() ||
                type.is_unsigned_int())
        {
            intrin_src << "_epi32";
        }
        else
        {
            fatal_printf_at(node.get_locus(),
                    "AVX2 Lowering: Node %s at %s has an unsupported type.",
                    ast_print_node_type(node.get_kind()),
                    locus_to_str(node.get_locus()));
        }

        walk(node.get_rhs());

        intrin_src << "(";
        intrin_src << as_expression(node.get_rhs());
        intrin_src << ")";

        Nodecl::NodeclBase function_call =
            intrin_src.parse_expression(node.retrieve_context());

        node.replace(function_call);
    }

    void AVX2VectorLowering::visit(const Nodecl::VectorLiteral& node)
    {
        TL::Type vector_type = node.get_type();
        TL::Type scalar_type = vector_type.basic_type();

        TL::Source intrin_src, intrin_name, undefined_value, values;

        intrin_src << intrin_name
            << "("
            << values
            << ")"
            ;

        // Intrinsic name
        intrin_name << AVX2_INTRIN_PREFIX << "_set";

        if (scalar_type.is_float())
        {
            intrin_name << "_ps";
            undefined_value << "0.0f";
        }
        else if (scalar_type.is_double())
        {
            intrin_name << "_pd";
            undefined_value << "0.0";
        }
        else if (scalar_type.is_signed_int() ||
                scalar_type.is_unsigned_int())
        {
            intrin_name << "_epi32";
            undefined_value << "0";
        }
        else
        {
            fatal_printf_at(node.get_locus(),
                    "AVX2 Lowering: Node %s at %s has an unsupported scalar_type (%s).",
                    ast_print_node_type(node.get_kind()),
                    locus_to_str(node.get_locus()),
                    scalar_type.get_simple_declaration(node.retrieve_context(), "").c_str());
        }

        Nodecl::List scalar_values =
            node.get_scalar_values().as<Nodecl::List>();


        unsigned int num_undefined_values =
            _vector_length/scalar_type.get_size() - vector_type.vector_num_elements();

        for (unsigned int i=0; i<num_undefined_values; i++)
        {
            values.append_with_separator(undefined_value, ",");
        }

        for (Nodecl::List::const_iterator it = scalar_values.begin();
                it != scalar_values.end();
                it++)
        {
            walk((*it));
            values.append_with_separator(as_expression(*it), ",");
        }

        Nodecl::NodeclBase function_call =
            intrin_src.parse_expression(node.retrieve_context());

        node.replace(function_call);
    }

    void AVX2VectorLowering::visit(const Nodecl::VectorConditionalExpression& node)
    {
        TL::Type type = node.get_type().basic_type();

        TL::Source intrin_src;

        Nodecl::NodeclBase true_node = node.get_true();
        Nodecl::NodeclBase false_node = node.get_false();
        Nodecl::NodeclBase condition_node = node.get_condition();

        TL::Type true_type = true_node.get_type().basic_type();
        TL::Type false_type = false_node.get_type().basic_type();
        TL::Type condition_type = condition_node.get_type();

        std::string casting;

        // Intrinsic name
        intrin_src << AVX2_INTRIN_PREFIX << "_blend";

        // Postfix
        if (true_type.is_integral_type()
                && false_type.is_integral_type())
        {
            // TODO _epi16
            intrin_src << "v_epi8";
        }
        else if (true_type.is_float()
                && false_type.is_float())
        {
            // TODO _ps
            intrin_src << "v_ps";
        }
        else if (true_type.is_double()
                && false_type.is_double())
        {
            // TODO _pd
            intrin_src << "v_pd";
        }
        else
        {
            fatal_printf_at(node.get_locus(),
                    "AVX2 Lowering: Node %s at %s has an unsupported type.",
                    ast_print_node_type(node.get_kind()),
                    locus_to_str(node.get_locus()));
        }

        walk(false_node);
        walk(true_node);
        walk(condition_node);

        intrin_src << "("
            << as_expression(false_node) // False first!
            << ", "
            << as_expression(true_node)
            << ", "
            << get_casting_intrinsic(TL::Type::get_int_type(),
                    true_type.basic_type(), node.get_locus())
            << "("
            << as_expression(condition_node)
            << "))";

        Nodecl::NodeclBase function_call =
            intrin_src.parse_expression(node.retrieve_context());

        node.replace(function_call);
    }

    void AVX2VectorLowering::visit(const Nodecl::VectorAssignment& node)
    {
        Nodecl::NodeclBase lhs = node.get_lhs();
        Nodecl::NodeclBase rhs = node.get_rhs();
        Nodecl::NodeclBase mask = node.get_mask();

        TL::Type type = node.get_type().basic_type();

        TL::Source intrin_src, intrin_name, intrin_type_suffix, intrin_op_name,
            mask_prefix, args, mask_args;

        intrin_src << as_expression(lhs)
            << " = "
            << intrin_name
            << "("
            << args
            << ")"
            ;

        walk(lhs);

        //            if (mask.is_null())
        {
            walk(rhs);
            args << as_expression(rhs);
        }

        Nodecl::NodeclBase function_call =
            intrin_src.parse_expression(node.retrieve_context());

        node.replace(function_call);
    }

    void AVX2VectorLowering::visit(const Nodecl::VectorLoad& node)
    {
        Nodecl::List flags = node.get_flags().as<Nodecl::List>();

        bool aligned = !flags.find_first<Nodecl::AlignedFlag>().
            is_null();

        if (aligned)
            visit_aligned_vector_load(node);
        else
            visit_unaligned_vector_load(node);
    }

    void AVX2VectorLowering::visit_aligned_vector_load(const Nodecl::VectorLoad& node)
    {
        Nodecl::NodeclBase rhs = node.get_rhs();
        Nodecl::NodeclBase mask = node.get_mask();

        if (!mask.is_null())
        {
            UNSUPPORTED_MASK(node);
        }

        TL::Type vtype = node.get_type();
        TL::Type type = vtype.basic_type();

        TL::Source intrin_src, intrin_name, intrin_type_suffix, intrin_op_name,
            mask_prefix, casting_args, args, mask_args, extra_args;

        intrin_src << intrin_name
            << "("
            << args
            << ")"
            ;

        intrin_name << AVX2_INTRIN_PREFIX
            << mask_prefix
            << "_"
            << intrin_op_name
            << "_"
            << intrin_type_suffix
            ;

        intrin_op_name << "load";

        if (type.is_float())
        {
            intrin_type_suffix << "ps";
        }
        else if (type.is_double())
        {
            intrin_type_suffix << "pd";
        }
        else if (type.is_integral_type())
        {
            intrin_type_suffix << "epi32";
        }
        else
        {
            fatal_printf_at(node.get_locus(),
                    "AVX2 Lowering: Node %s at %s has an unsupported type.",
                    ast_print_node_type(node.get_kind()),
                    locus_to_str(node.get_locus()));
        }

        casting_args << get_casting_to_scalar_pointer(type);

        walk(rhs);

        args << casting_args
            << as_expression(rhs)
            << mask_args
            << extra_args
            ;

        Nodecl::NodeclBase function_call =
            intrin_src.parse_expression(node.retrieve_context());

        node.replace(function_call);
    }

    void AVX2VectorLowering::visit_unaligned_vector_load(
            const Nodecl::VectorLoad& node)
    {
        Nodecl::NodeclBase rhs = node.get_rhs();
        Nodecl::NodeclBase mask = node.get_mask();

        if (!mask.is_null())
        {
            UNSUPPORTED_MASK(node);
        }

        TL::Type vtype = node.get_type();
        TL::Type type = vtype.basic_type();

        TL::Source intrin_src, intrin_name, intrin_type_suffix, intrin_op_name,
            mask_prefix, casting_args, args, mask_args, extra_args;

        intrin_src << intrin_name << "(" << args << ")";

        intrin_name << AVX2_INTRIN_PREFIX << mask_prefix << "_"
                    << intrin_op_name << "_" << intrin_type_suffix;

        intrin_op_name << "loadu";

        if (type.is_float())
        {
            intrin_type_suffix << "ps";
        }
        else if (type.is_double())
        {
            intrin_type_suffix << "pd";
        }
        else if (type.is_integral_type())
        {
            intrin_type_suffix << "si" << AVX2_VECTOR_BIT_SIZE;
            TL::Symbol s = get_m256i_symbol();
            casting_args << "("
                         << as_type(s.get_user_defined_type().get_const_type().get_pointer_to())
                         << ")";
        }
        else
        {
            fatal_printf_at(node.get_locus(),
                    "AVX2 Lowering: Node %s at %s has an unsupported type.",
                    ast_print_node_type(node.get_kind()),
                    locus_to_str(node.get_locus()));
        }

        walk(rhs);

        args << mask_args
            << casting_args
            << as_expression(rhs)
            << extra_args
            ;

        Nodecl::NodeclBase function_call =
            intrin_src.parse_expression(node.retrieve_context());

        node.replace(function_call);
    }

    void AVX2VectorLowering::visit(const Nodecl::VectorStore& node)
    {
        Nodecl::List flags = node.get_flags().as<Nodecl::List>();

        bool aligned = !flags.find_first<Nodecl::AlignedFlag>().
            is_null();

        if (aligned)
            visit_aligned_vector_store(node);
        else
            visit_unaligned_vector_store(node);
    }

    void AVX2VectorLowering::visit_aligned_vector_store(
            const Nodecl::VectorStore& node)
    {
        Nodecl::NodeclBase lhs = node.get_lhs();
        Nodecl::NodeclBase rhs = node.get_rhs();
        Nodecl::NodeclBase mask = node.get_mask();

        if (!mask.is_null())
        {
            UNSUPPORTED_MASK(node);
        }

        TL::Type type = node.get_lhs().get_type().basic_type();

        TL::Source intrin_src, intrin_name, intrin_op_name, args,
            intrin_type_suffix, mask_prefix, mask_args, casting_args;


        intrin_src << intrin_name
            << "("
            << args
            << ")"
            ;

        intrin_name << AVX2_INTRIN_PREFIX
            << mask_prefix
            << "_"
            << intrin_op_name
            << "_"
            << intrin_type_suffix
            ;

        intrin_op_name << "store";

        if (type.is_float())
        {
            intrin_type_suffix << "ps";
        }
        else if (type.is_double())
        {
            intrin_type_suffix << "pd";
        }
        else if (type.is_integral_type())
        {
            intrin_type_suffix << "epi32";
        }
        else
        {
            fatal_printf_at(node.get_locus(),
                    "AVX2 Lowering: Node %s at %s has an unsupported type.",
                    ast_print_node_type(node.get_kind()),
                    locus_to_str(node.get_locus()));
        }
        
        casting_args << get_casting_to_scalar_pointer(type);

        walk(lhs);
        walk(rhs);

        args << "("
            << casting_args
            << as_expression(lhs)
            << ")"
            << mask_args
            << ", "
            << as_expression(rhs)
            ;

        Nodecl::NodeclBase function_call =
            intrin_src.parse_expression(node.retrieve_context());

        node.replace(function_call);
    }

    void AVX2VectorLowering::visit_unaligned_vector_store(
            const Nodecl::VectorStore& node)
    {
        Nodecl::NodeclBase lhs = node.get_lhs();
        Nodecl::NodeclBase rhs = node.get_rhs();
        Nodecl::NodeclBase mask = node.get_mask();

        if (!mask.is_null())
        {
            UNSUPPORTED_MASK(node);
        }

        TL::Type type = node.get_lhs().get_type().basic_type();

        TL::Source intrin_src, intrin_name, intrin_op_name, args,
            intrin_type_suffix, mask_prefix, mask_args, casting_args;


        intrin_src << intrin_name
            << "("
            << args
            << ")"
            ;

        intrin_name << AVX2_INTRIN_PREFIX << mask_prefix << "_"
                    << intrin_op_name << "_" << intrin_type_suffix;

        intrin_op_name << "storeu";

        if (type.is_float())
        {
            intrin_type_suffix << "ps";
        }
        else if (type.is_double())
        {
            intrin_type_suffix << "pd";
        }
        else if (type.is_integral_type())
        {
            intrin_type_suffix << "si" << AVX2_VECTOR_BIT_SIZE;
            casting_args << get_casting_to_scalar_pointer(
                    TL::Type::get_void_type());
        }
        else
        {
            fatal_printf_at(node.get_locus(),
                    "AVX2 Lowering: Node %s at %s has an unsupported type.",
                    ast_print_node_type(node.get_kind()),
                    locus_to_str(node.get_locus()));
        }

        walk(lhs);
        walk(rhs);

        args << "("
            << casting_args
            << as_expression(lhs)
            << ")"
            << ", "
            << mask_args
            << as_expression(rhs)
            ;

        Nodecl::NodeclBase function_call =
            intrin_src.parse_expression(node.retrieve_context());

        node.replace(function_call);
    }

    void AVX2VectorLowering::visit(const Nodecl::VectorGather& node)
    {
        const Nodecl::NodeclBase base = node.get_base();
        const Nodecl::NodeclBase strides = node.get_strides();
        const Nodecl::NodeclBase mask = node.get_mask();

        if (!mask.is_null())
        {
            UNSUPPORTED_MASK(node);
        }

        TL::Type type = node.get_type().basic_type();
        TL::Type index_type = strides.get_type().basic_type();

        TL::Source intrin_src, intrin_name, intrin_op_name, intrin_type_suffix,
            mask_prefix, args, mask_args;

        intrin_src << intrin_name
            << "("
            << args
            << ")"
            ;

        intrin_name << AVX2_INTRIN_PREFIX
            << mask_prefix
            << "_"
            << intrin_op_name
            << "_"
            << intrin_type_suffix
            ;

        intrin_op_name << "i32gather";

        if (type.is_float())
        {
            intrin_type_suffix << "ps";
        }
        else if (type.is_signed_int() || type.is_unsigned_int())
        {
            intrin_type_suffix << "si" << AVX2_VECTOR_BIT_SIZE;
        }
        else
        {
            fatal_printf_at(node.get_locus(),
                    "AVX2 Lowering: Node %s at %s has an unsupported source type.",
                    ast_print_node_type(node.get_kind()),
                    locus_to_str(node.get_locus()));
        }

        if ((!index_type.is_signed_int()) && (!index_type.is_unsigned_int()))
        {
            fatal_printf_at(node.get_locus(),
                    "AVX2 Lowering: Node %s at %s has an unsupported index type.",
                    ast_print_node_type(node.get_kind()),
                    locus_to_str(node.get_locus()));
        }

        walk(base);
        walk(strides);

        args << mask_args
            << as_expression(base)
            << ", "
            << as_expression(strides)
            << ", "
            << type.get_size()
            ;

        Nodecl::NodeclBase function_call =
            intrin_src.parse_expression(node.retrieve_context());

        node.replace(function_call);
    }

    void AVX2VectorLowering::visit(const Nodecl::VectorScatter& node)
    {
        fatal_printf_at(node.get_locus(), "AVX2 Lowering: Scatter operations are not supported");
    }

    void AVX2VectorLowering::visit(const Nodecl::VectorFunctionCall& node)
    {
        Nodecl::FunctionCall function_call =
            node.get_function_call().as<Nodecl::FunctionCall>();

        const Nodecl::NodeclBase mask = node.get_mask();
        TL::Type vector_type = node.get_type();
        TL::Type scalar_type = vector_type.basic_type();
        Nodecl::List arguments = function_call.get_arguments().as<Nodecl::List>();

        if (!mask.is_null())
        {
            UNSUPPORTED_MASK(node);
        }

        walk(arguments);
        node.replace(function_call);
    }

    void AVX2VectorLowering::visit(const Nodecl::VectorFabs& node)
    {
        const Nodecl::NodeclBase mask = node.get_mask();
        const Nodecl::NodeclBase argument = node.get_argument();

        if (!mask.is_null())
        {
            UNSUPPORTED_MASK(node);
        }

        TL::Type type = node.get_type().basic_type();

        TL::Source intrin_src, mask_prefix, mask_args;

        walk(argument);

        // Handcoded implementations for float and double
        if (type.is_float())
        {
            intrin_src << get_casting_intrinsic(TL::Type::get_int_type(), type, node.get_locus()) << "("
                << AVX2_INTRIN_PREFIX << mask_prefix << "_and_si" << AVX2_VECTOR_BIT_SIZE << "("
                << mask_args
                << get_casting_intrinsic(type, TL::Type::get_int_type(), node.get_locus()) << "("
                << as_expression(argument)
                << "), " << AVX2_INTRIN_PREFIX << "_set1_epi32(0x7FFFFFFF)))";
        }
        else if (type.is_double())
        {
            intrin_src << get_casting_intrinsic(TL::Type::get_int_type(), type, node.get_locus()) << "("
                << AVX2_INTRIN_PREFIX << mask_prefix << "_and_si" << AVX2_VECTOR_BIT_SIZE << "("
                << mask_args
                << get_casting_intrinsic(type, TL::Type::get_int_type(), node.get_locus()) << "("
                << as_expression(argument)
                << "), " << AVX2_INTRIN_PREFIX << "_set1_epi64(0x7FFFFFFFFFFFFFFFLL)))";
        }
        else
        {
            fatal_printf_at(node.get_locus(),
                    "AVX2 Lowering: Node %s at %s has an unsupported type.",
                    ast_print_node_type(node.get_kind()),
                    locus_to_str(node.get_locus()));
        }

        Nodecl::NodeclBase function_call =
            intrin_src.parse_expression(node.retrieve_context());

        node.replace(function_call);
    }

    void AVX2VectorLowering::visit(const Nodecl::VectorSincos& node)
    {
        const Nodecl::NodeclBase mask = node.get_mask();
        const Nodecl::NodeclBase source = node.get_source();
        const Nodecl::NodeclBase sin_pointer = node.get_sin_pointer();
        const Nodecl::NodeclBase cos_pointer = node.get_cos_pointer();

        if (!mask.is_null())
        {
            UNSUPPORTED_MASK(node);
        }

        TL::Type type = node.get_type().basic_type();

        TL::Source intrin_src, mask_prefix, mask_args;

        walk(source);
        walk(sin_pointer);
        walk(cos_pointer);

        fatal_printf_at(node.get_locus(), "AVX2 Lowering: Sincos is unsupported.");

        if (type.is_float())
        {
            if(mask.is_null())
            {
                intrin_src << "(*" << as_expression(sin_pointer) << ")"
                    << " = " << AVX2_INTRIN_PREFIX << "_mask_sincos_ps"
                    << "("
                    << as_expression(sin_pointer.children().front())
                    << ", 0xFFFF, "
                    << as_expression(cos_pointer)
                    << ", "
                    << as_expression(source)
                    << ")";
            }
            else
            {
                intrin_src << "(*" << as_expression(sin_pointer) << ")"
                    << " = " << AVX2_INTRIN_PREFIX << "_mask_sincos_ps"
                    << "("
                    << mask_args
                    << as_expression(cos_pointer)
                    << ", "
                    << as_expression(source)
                    << ")";
            }
        }
        else
        {
            fatal_printf_at(node.get_locus(),
                    "AVX2 Lowering: Node %s at %s has an unsupported type: %s.",
                    ast_print_node_type(node.get_kind()),
                    locus_to_str(node.get_locus()),
                    type.get_simple_declaration(node.retrieve_context(), "").c_str());
        }

        Nodecl::NodeclBase function_call =
            intrin_src.parse_expression(node.retrieve_context());

        node.replace(function_call);
    }

    void AVX2VectorLowering::visit(const Nodecl::ParenthesizedExpression& node)
    {
        walk(node.get_nest());

        Nodecl::NodeclBase n(node.shallow_copy());
        n.set_type(node.get_nest().get_type());
        node.replace(n);
    }

    void AVX2VectorLowering::visit_reduction_add_4bytes_elements(
            const Nodecl::VectorReductionAdd& node)
    {
        Nodecl::NodeclBase vector_src = node.get_vector_src();
        TL::Type vtype = vector_src.get_type().no_ref();
        TL::Type type = node.get_type().no_ref();

        walk(vector_src);

        TL::Source intrin_src, horizontal_256_op_src, horizontal_128_op_src,
            add_128_op_src, horizontal_256_intrin_src,horizontal_128_intrin_src,
            extract_op_src, extract_intrin_src, sse_suffix_type, avx_suffix_type,
            sse_suffix_size, avx_suffix_size,
            temporal_128red_var, temporal_256red_var;

        TL::Type v256_type = vtype;
        TL::Type v128_type = type.get_vector_of_elements(
                v256_type.vector_num_elements()/2);

        intrin_src             
            << "({"
            << print_type_str(v256_type.get_internal_type(),
                    node.retrieve_context().get_decl_context())
            << " " << temporal_256red_var << ";"
            << print_type_str(v128_type.get_internal_type(),
                    node.retrieve_context().get_decl_context())
            << " " << temporal_128red_var << ";"
            << horizontal_256_op_src
            << add_128_op_src
            << horizontal_128_op_src
            << extract_op_src
            << "})";

        if (type.is_float())
        {
            extract_intrin_src << "_mm_cvtss_f32";
            sse_suffix_type << "ps";
            sse_suffix_size << "";
            avx_suffix_type << "ps";
            avx_suffix_size << "";
        }
        else if (type.is_signed_int() ||
                type.is_unsigned_int())
        {
            extract_intrin_src << "_mm_cvtsi128_si32";
            sse_suffix_type << "epi";
            sse_suffix_size << "32";
            avx_suffix_type << "si";
            avx_suffix_size << "256";
        }
        else
        {
            fatal_printf_at(node.get_locus(),
                    "AVX2 Lowering: Node %s at %s has an unsupported type.",
                    ast_print_node_type(node.get_kind()),
                    locus_to_str(node.get_locus()));
        }
        //std::cerr << node.get_lhs().prettyprint() << " " << node.get_rhs().prettyprint();

        temporal_256red_var << "__rtmp256";
        temporal_128red_var << "__rtmp128";

        horizontal_128_intrin_src
            << "_mm_hadd_"
            << sse_suffix_type
            << sse_suffix_size
            ;

        horizontal_256_intrin_src
            << "_mm256_hadd_"
            << sse_suffix_type
            << sse_suffix_size
            ;

        horizontal_256_op_src
            << temporal_256red_var
            << " = "
            << horizontal_256_intrin_src
            << "("
            << as_expression(vector_src)
            << ", "
            << as_expression(vector_src)
            << ");";

        add_128_op_src
            << temporal_128red_var
            << " = "
            << "_mm_add_" << sse_suffix_type << sse_suffix_size
            << "("
            << "_mm256_cast" << avx_suffix_type << "256_" << avx_suffix_type << "128("
            << temporal_256red_var
            << ")"
            << ", "
            << "_mm256_extractf128_" << avx_suffix_type << avx_suffix_size
            << "("
            << temporal_256red_var
            << ", "
            << "1"
            << "));";
        ;

        horizontal_128_op_src
            << temporal_128red_var
            << " = "
            << horizontal_128_intrin_src
            << "("
            << temporal_128red_var
            << ", "
            << temporal_128red_var
            << ");";

        extract_op_src
            << extract_intrin_src
            << "("
            << temporal_128red_var
            << ");";

        Nodecl::NodeclBase function_call =
            intrin_src.parse_expression(node.retrieve_context());

        node.replace(function_call);
    }

    void AVX2VectorLowering::visit_reduction_add_8bytes_elements(
            const Nodecl::VectorReductionAdd& node)
    {
        Nodecl::NodeclBase vector_src = node.get_vector_src();
        TL::Type vtype = vector_src.get_type().no_ref();
        TL::Type type = node.get_type().no_ref();

        walk(vector_src);

        TL::Source intrin_src, horizontal_256_op_src, 
            add_128_op_src, horizontal_256_intrin_src,
            extract_op_src, extract_intrin_src, sse_suffix_type, avx_suffix_type,
            sse_suffix_size, avx_suffix_size,
            temporal_128red_var, temporal_256red_var;

        TL::Type v256_type = vtype;
        TL::Type v128_type = type.get_vector_of_elements(
                v256_type.vector_num_elements()/2);

        intrin_src             
            << "({"
            << print_type_str(v256_type.get_internal_type(),
                    node.retrieve_context().get_decl_context())
            << " " << temporal_256red_var << ";"
            << print_type_str(v128_type.get_internal_type(),
                    node.retrieve_context().get_decl_context())
            << " " << temporal_128red_var << ";"
            << horizontal_256_op_src
            << add_128_op_src
            << extract_op_src
            << "})";

        if (type.is_double())
        {
            extract_intrin_src << "_mm_cvtsd_f64";
            sse_suffix_type << "pd";
            sse_suffix_type << "";
            avx_suffix_type << "pd";
            avx_suffix_type << "";
        }
        else
        {
            fatal_error("AVX2 Lowering: Node %s at %s has an unsupported type.",
                    ast_print_node_type(node.get_kind()),
                    locus_to_str(node.get_locus()));
        }
        //std::cerr << node.get_lhs().prettyprint() << " " << node.get_rhs().prettyprint();

        temporal_256red_var << "__rtmp256";
        temporal_128red_var << "__rtmp128";

        horizontal_256_intrin_src
            << "_mm256_hadd_"
            << avx_suffix_type
            ;

        horizontal_256_op_src
            << temporal_256red_var
            << " = "
            << horizontal_256_intrin_src
            << "("
            << as_expression(vector_src)
            << ", "
            << as_expression(vector_src)
            << ");";

        add_128_op_src
            << temporal_128red_var
            << " = "
            << "_mm_add_" << sse_suffix_type << sse_suffix_size
            << "("
            << "_mm256_cast" << avx_suffix_type << "256_" << avx_suffix_type << "128("
            << temporal_256red_var
            << ")"
            << ", "
            << "_mm256_extractf128_" << avx_suffix_type << avx_suffix_size
            << "("
            << temporal_256red_var
            << ", "
            << "1"
            << "));";

        extract_op_src
            << extract_intrin_src
            << "("
            << temporal_128red_var
            << ");";

        Nodecl::NodeclBase function_call =
            intrin_src.parse_expression(node.retrieve_context());

        node.replace(function_call);
    }


    void AVX2VectorLowering::visit(const Nodecl::VectorReductionAdd& node)
    {
        Nodecl::NodeclBase vector_src = node.get_vector_src();

        TL::Type vtype = vector_src.get_type().no_ref();
        TL::Type type = node.get_type().no_ref();
        int type_size = type.get_size();

        if (type_size == 4)
        {
            visit_reduction_add_4bytes_elements(node);
        }
        else if (type_size == 8)
        {
            visit_reduction_add_8bytes_elements(node);
        }
        else
        {
            fatal_error("AVX2 Lowering: Node %s at %s with type size %d not implemented yet.",
                    ast_print_node_type(node.get_kind()),
                    locus_to_str(node.get_locus()),
                    type.get_size());
        }
    }

// OLD. It didn't compile?
//    void AVX2VectorLowering::visit(const Nodecl::VectorReductionAdd& node)
//    {
//        Nodecl::NodeclBase vector_src = node.get_vector_src();
//
//        TL::Type vtype = vector_src.get_type().no_ref();
//        TL::Type type = node.get_type().no_ref();
//
//        walk(vector_src);
//
//        TL::Source intrin_src, horizontal_256_op_src, horizontal_128_op_src,
//            horizontal_128_intrin_src, extract_op_src, extract_intrin_src,
//            sse_suffix_elem, avx_suffix_type;
//
//        intrin_src             
//            << "({"
//            << horizontal_256_op_src
//            << horizontal_128_op_src
//            << horizontal_128_op_src
//            << extract_op_src
//            << "})";
//
//        if (type.is_float())
//        {
//            extract_intrin_src << "_mm_cvtss_f32";
//            sse_suffix_elem << "ps";
//            avx_suffix_type << "ps";
//        }
//        else if (type.is_double())
//        {
//            extract_intrin_src << "_mm_cvtsd_f64";
//            sse_suffix_elem << "pd";
//            avx_suffix_type << "pd";
//        }
//        else if (type.is_signed_int() ||
//                type.is_unsigned_int())
//        {
//            extract_intrin_src << "_mm_cvtsi128_si32";
//            sse_suffix_elem << "epi32";
//            avx_suffix_type << "si256";
//        }
//        else
//        {
//            fatal_error("AVX2 Lowering: Node %s at %s has an unsupported type.",
//                    ast_print_node_type(node.get_kind()),
//                    locus_to_str(node.get_locus()));
//        }
//        //std::cerr << node.get_lhs().prettyprint() << " " << node.get_rhs().prettyprint();
//
//        horizontal_128_intrin_src
//            << "_mm_hadd_"
//            << sse_suffix_elem
//            ;
//
//        horizontal_256_op_src
//            << print_type_str(vtype.basic_type().get_vector_of_elements(
//                        vtype.vector_num_elements()/2).get_internal_type(),
//                    node.retrieve_context().get_decl_context())
//            << " __rtmp0"
//            << " = "
//            << "_mm_add_" << sse_suffix_elem
//            << "("
//            << "_mm256_extractf128_" << avx_suffix_type
//            << "("
//            << as_expression(vector_src)
//            << ", "
//            << "0"
//            << "),"
//            << "_mm256_extractf128_" << avx_suffix_type
//            << "("
//            << as_expression(vector_src)
//            << ", "
//            << "1"
//            << "));";
//        ;
//
//        horizontal_128_op_src
//            << "__rtmp0 = "
//            << horizontal_128_intrin_src
//            << "("
//            << "__rtmp0"
//            << ", "
//            << "__rtmp0"
//            << ");";
//
//        extract_op_src
//            << extract_intrin_src
//            << "("
//            << "__rtmp0"
//            << ");";
//
//        Nodecl::NodeclBase function_call =
//            intrin_src.parse_expression(node.retrieve_context());
//
//        node.replace(function_call);
//    }

    void AVX2VectorLowering::visit(const Nodecl::VectorReductionMinus& node)
    {
        // OpenMP defines reduction(-:a) in the same way as reduction(+:a)
        visit(node.as<Nodecl::VectorReductionAdd>());
    }

    void AVX2VectorLowering::visit(const Nodecl::VectorMaskAssignment& node)
    {
        walk(node.get_lhs());
        walk(node.get_rhs());

        // Emit this is as a plain assignment
        node.replace(
                Nodecl::Assignment::make(
                    node.get_lhs(),
                    node.get_rhs(),
                    avx2_comparison_type().get_lvalue_reference_to()));
    }

    void AVX2VectorLowering::visit(const Nodecl::VectorMaskNot& node)
    {
        TL::Type type = node.get_rhs().get_type().basic_type();

        TL::Source intrin_src;

        walk(node.get_rhs());

        intrin_src
            << "_mm256_andnot_si256("
            << as_expression(node.get_rhs()) << ","
            << "_mm256_set1_epi32(~0))"
            ;

        Nodecl::NodeclBase function_call =
            intrin_src.parse_expression(node.retrieve_context());

        node.replace(function_call);
    }

    void AVX2VectorLowering::visit(const Nodecl::VectorMaskConversion& node)
    {
        UNSUPPORTED_MASK(node);
    }

    void AVX2VectorLowering::visit(const Nodecl::VectorMaskAnd& node)
    {
        UNSUPPORTED_MASK(node);
    }

    void AVX2VectorLowering::visit(const Nodecl::VectorMaskOr& node)
    {
        UNSUPPORTED_MASK(node);
    }

    void AVX2VectorLowering::visit(const Nodecl::VectorMaskAnd1Not& node)
    {
        UNSUPPORTED_MASK(node);
    }

    void AVX2VectorLowering::visit(const Nodecl::VectorMaskAnd2Not& node)
    {
        UNSUPPORTED_MASK(node);
    }

    void AVX2VectorLowering::visit(const Nodecl::VectorMaskXor& node)
    {
        UNSUPPORTED_MASK(node);
    }

    void AVX2VectorLowering::visit(const Nodecl::MaskLiteral& node)
    {
        UNSUPPORTED_MASK(node);
    }

}
}
