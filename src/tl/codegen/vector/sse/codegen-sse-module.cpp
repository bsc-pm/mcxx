/*--------------------------------------------------------------------
  (C) Copyright 2006-2012 Barcelona Supercomputing Center
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

#include "codegen-sse-module.hpp"
#include "codegen-cxx.hpp"

namespace Codegen 
{
    SSEModuleVisitor::SSEModuleVisitor(CodegenVisitor* base_codegen) 
        : CodegenModuleVisitor(base_codegen)
    {
    }

    void SSEModuleVisitor::visit(const Nodecl::VectorAdd& node) 
    { 
        TL::Type type = node.get_type().basic_type();

        // Intrinsic name
        file << "_mm_add";
        
        // Postfix
        if (type.is_float()) 
        { 
            file << "_ps"; 
        } 
        else if (type.is_double()) 
        { 
            file << "_pd"; 
        } 
        else if (type.is_signed_int() ||
            type.is_unsigned_int()) 
        { 
            file << "_epi32"; 
        } 
        else if (type.is_signed_short_int() ||
            type.is_unsigned_short_int()) 
        { 
            file << "_epi16"; 
        } 
        else if (type.is_char() || 
            type.is_signed_char() ||
            type.is_unsigned_char()) 
        { 
            file << "_epi8"; 
        } 
        else
        {
            running_error("SSE Codegen: Node %s at %s has an unsupported type.", 
                    ast_print_node_type(node.get_kind()),
                    node.get_locus().c_str());
        }      

        file << "("; 
        walk(node.get_lhs());
        file << ", ";
        walk(node.get_rhs());
        file << ")"; 
    }                                                 

    void SSEModuleVisitor::visit(const Nodecl::VectorMinus& node) 
    { 
        TL::Type type = node.get_type().basic_type();

        // Intrinsic name
        file << "_mm_sub";
        
        // Postfix
        if (type.is_float()) 
        { 
            file << "_ps"; 
        } 
        else if (type.is_double()) 
        { 
            file << "_pd"; 
        } 
        else if (type.is_signed_int() ||
            type.is_unsigned_int()) 
        { 
            file << "_epi32"; 
        } 
        else if (type.is_signed_short_int() ||
            type.is_unsigned_short_int()) 
        { 
            file << "_epi16"; 
        } 
        else if (type.is_char() || 
            type.is_signed_char() ||
            type.is_unsigned_char()) 
        { 
            file << "_epi8"; 
        } 
        else
        {
            running_error("SSE Codegen: Node %s at %s has an unsupported type.", 
                    ast_print_node_type(node.get_kind()),
                    node.get_locus().c_str());
        }      

        file << "("; 
        walk(node.get_lhs());
        file << ", ";
        walk(node.get_rhs());
        file << ")"; 
    }                                                 

    void SSEModuleVisitor::visit(const Nodecl::VectorMul& node) 
    { 
        TL::Type result_type = node.get_type().basic_type();
        TL::Type first_op_type = node.get_rhs().get_type().basic_type();
        TL::Type second_op_type = node.get_lhs().get_type().basic_type();

        // Intrinsic name
        file << "_mm_mul";
        
        // Postfix
        if (result_type.is_float() &&
                first_op_type.is_float() &&
                second_op_type.is_float())
        {
           file << "_ps"; 
        } 
        else if (result_type.is_double() &&
                first_op_type.is_double() &&
                second_op_type.is_double())
        { 
            file << "_pd"; 
        }
        else if (result_type.is_signed_int() &&
                first_op_type.is_signed_int() &&
                second_op_type.is_signed_int())
        {
            file << "lo_epi32"; 
        } 
        else if (result_type.is_unsigned_int() &&
                first_op_type.is_unsigned_int() &&
                second_op_type.is_unsigned_int())
        {
            file << "lo_epi32"; 
        } 
 
       /* 
        else if (type.is_signed_short_int() ||
            type.is_unsigned_short_int()) 
        { 
            file << "_epi16"; 
        } 
        else if (type.is_char() || 
            type.is_signed_char() ||
            type.is_unsigned_char()) 
        { 
            file << "_epi8"; 
        }
       */ 
        else
        {
            running_error("SSE Codegen: Node %s at %s has an unsupported type.", 
                    ast_print_node_type(node.get_kind()),
                    node.get_locus().c_str());
        }      

        file << "("; 
        walk(node.get_lhs());
        file << ", ";
        walk(node.get_rhs());
        file << ")"; 
    }    

    void SSEModuleVisitor::visit(const Nodecl::VectorDiv& node) 
    { 
        TL::Type type = node.get_type().basic_type();

        // Intrinsic name
        file << "_mm_div";
        
        // Postfix
        if (type.is_float()) 
        { 
            file << "_ps"; 
        } 
        else if (type.is_double()) 
        { 
            file << "_pd"; 
        } 
        else if (type.is_signed_int() ||
            type.is_unsigned_int()) 
        { 
            file << "_epi32"; 
        } 
        else if (type.is_signed_short_int() ||
            type.is_unsigned_short_int()) 
        { 
            file << "_epi16"; 
        } 
        else if (type.is_char() || 
            type.is_signed_char() ||
            type.is_unsigned_char()) 
        { 
            file << "_epi8"; 
        } 
        else
        {
            running_error("SSE Codegen: Node %s at %s has an unsupported type.", 
                    ast_print_node_type(node.get_kind()),
                    node.get_locus().c_str());
        }      

        file << "("; 
        walk(node.get_lhs());
        file << ", ";
        walk(node.get_rhs());
        file << ")"; 
    }                                                 

    void SSEModuleVisitor::visit(const Nodecl::VectorLowerThan& node) 
    { 
        TL::Type type = node.get_lhs().get_type().basic_type();

        // Intrinsic name
        file << "_mm_cmplt";
        
        // Postfix
        if (type.is_float()) 
        { 
            file << "_ps"; 
        } 
        else if (type.is_signed_int() ||
            type.is_unsigned_int()) 
        { 
            file << "_epi32"; 
        } 
        else if (type.is_signed_short_int() ||
            type.is_unsigned_short_int()) 
        { 
            file << "_epi16"; 
        } 
        else if (type.is_char() || 
            type.is_signed_char() ||
            type.is_unsigned_char()) 
        { 
            file << "_epi8"; 
        } 
        else
        {
            running_error("SSE Codegen: Node %s at %s has an unsupported type.", 
                    ast_print_node_type(node.get_kind()),
                    node.get_locus().c_str());
        }      

        file << "("; 
        walk(node.get_lhs());
        file << ", ";
        walk(node.get_rhs());
        file << ")"; 
    }                                                 

    void SSEModuleVisitor::visit(const Nodecl::VectorGreaterThan& node) 
    { 
        TL::Type type = node.get_lhs().get_type().basic_type();

        // Intrinsic name
        file << "_mm_cmpgt";
        
        // Postfix
        if (type.is_float()) 
        { 
            file << "_ps"; 
        } 
        else if (type.is_signed_int() ||
            type.is_unsigned_int()) 
        { 
            file << "_epi32"; 
        } 
        else if (type.is_signed_short_int() ||
            type.is_unsigned_short_int()) 
        { 
            file << "_epi16"; 
        } 
        else if (type.is_char() || 
            type.is_signed_char() ||
            type.is_unsigned_char()) 
        { 
            file << "_epi8"; 
        } 
        else
        {
            running_error("SSE Codegen: Node %s at %s has an unsupported type.", 
                    ast_print_node_type(node.get_kind()),
                    node.get_locus().c_str());
        }      

        file << "("; 
        walk(node.get_lhs());
        file << ", ";
        walk(node.get_rhs());
        file << ")"; 
    }                                                 

    void SSEModuleVisitor::visit(const Nodecl::VectorEqual& node) 
    { 
        TL::Type type = node.get_lhs().get_type().basic_type();

        // Intrinsic name
        file << "_mm_cmpeq";
        
        // Postfix
        if (type.is_float()) 
        { 
            file << "_ps"; 
        } 
        else if (type.is_signed_int() ||
            type.is_unsigned_int()) 
        { 
            file << "_epi32"; 
        } 
        else if (type.is_signed_short_int() ||
            type.is_unsigned_short_int()) 
        { 
            file << "_epi16"; 
        } 
        else if (type.is_char() || 
            type.is_signed_char() ||
            type.is_unsigned_char()) 
        { 
            file << "_epi8"; 
        } 
        else
        {
            running_error("SSE Codegen: Node %s at %s has an unsupported type.", 
                    ast_print_node_type(node.get_kind()),
                    node.get_locus().c_str());
        }      

        file << "("; 
        walk(node.get_lhs());
        file << ", ";
        walk(node.get_rhs());
        file << ")"; 
    }                                                 

    void SSEModuleVisitor::visit(const Nodecl::VectorBitwiseAnd& node) 
    { 
        TL::Type type = node.get_type().basic_type();

        // Intrinsic name
        file << "_mm_and";
        
        // Postfix
        if (type.is_float()) 
        { 
            file << "_ps"; 
        } 
        else if (type.is_integral_type())
        { 
            file << "_si128"; 
        } 
        else
        {
            running_error("SSE Codegen: Node %s at %s has an unsupported type.", 
                    ast_print_node_type(node.get_kind()),
                    node.get_locus().c_str());
        }      

        file << "("; 
        walk(node.get_lhs());
        file << ", ";
        walk(node.get_rhs());
        file << ")"; 
    }                                                 

    void SSEModuleVisitor::visit(const Nodecl::VectorBitwiseOr& node) 
    { 
        TL::Type type = node.get_type().basic_type();

        // Intrinsic name
        file << "_mm_or";
        
        // Postfix
        if (type.is_float()) 
        { 
            file << "_ps"; 
        } 
        else if (type.is_integral_type())
        { 
            file << "_si128"; 
        } 
        else
        {
            running_error("SSE Codegen: Node %s at %s has an unsupported type.", 
                    ast_print_node_type(node.get_kind()),
                    node.get_locus().c_str());
        }      

        file << "("; 
        walk(node.get_lhs());
        file << ", ";
        walk(node.get_rhs());
        file << ")"; 
    }                                                 

    void SSEModuleVisitor::visit(const Nodecl::VectorBitwiseXor& node) 
    { 
        TL::Type type = node.get_type().basic_type();

        // Intrinsic name
        file << "_mm_xor";
        
        // Postfix
        if (type.is_float()) 
        { 
            file << "_ps"; 
        } 
        else if (type.is_integral_type())
        { 
            file << "_si128"; 
        }
        else
        {
            running_error("SSE Codegen: Node %s at %s has an unsupported type.", 
                    ast_print_node_type(node.get_kind()),
                    node.get_locus().c_str());
        }      

        file << "("; 
        walk(node.get_lhs());
        file << ", ";
        walk(node.get_rhs());
        file << ")"; 
    }   

    void SSEModuleVisitor::visit(const Nodecl::VectorLogicalOr& node) 
    { 
        fprintf(stderr, "Warning: Logical Or operation in '%s' is not "
                "supported in SSE. Bitwise Or operation will be used instead. "
                "This might change the expected behaviour of the application.\n",
                node.get_locus().c_str()); 

        visit(node.as<Nodecl::VectorBitwiseOr>());
    }                                                 

    void SSEModuleVisitor::visit(const Nodecl::VectorNeg& node) 
    { 
        TL::Type type = node.get_type().basic_type();
        
        if (type.is_float()) 
        { 
            file << "_mm_xor_ps(_mm_castsi128_ps(_mm_set1_epi32(0x80000000)), ";
            walk(node.get_rhs());
            file << ")";
        } 
        else if (type.is_double()) 
        { 
            file << "_mm_xor_pd(_mm_castsi128_pd(_mm_set1_epi64(0x8000000000000000LL)), ";
            walk(node.get_rhs());
            file << ")";
        }
        else if (type.is_signed_int() ||
                type.is_unsigned_int())
        {
            file << "(_mm_sub_epi32( _mm_setzero_si128(0),";
            walk(node.get_rhs());
            file << "))";
        }
        else if (type.is_signed_short_int() ||
                type.is_unsigned_short_int())
        {
            file << "(_mm_sub_epi16( _mm_setzero_si128(0),";
            walk(node.get_rhs());
            file << "))";
        }
        else if (type.is_char() ||
                type.is_signed_char() ||
                type.is_unsigned_char())
        {
            file << "(_mm_sub_epi8( _mm_setzero_si128(0),";
            walk(node.get_rhs());
            file << "))";
        }
        else
        {
            running_error("SSE Codegen: Node %s at %s has an unsupported type.", 
                    ast_print_node_type(node.get_kind()),
                    node.get_locus().c_str());
        }      
    }                                                 

    void SSEModuleVisitor::visit(const Nodecl::VectorConversion& node) 
    {
        const TL::Type& src_type = node.get_nest().get_type().basic_type().get_unqualified_type();
        const TL::Type& dst_type = node.get_type().basic_type().get_unqualified_type();

        if (src_type.is_same_type(dst_type))
        {
            walk(node.get_nest());
            return;
        }
        else if (src_type.is_signed_int() &&
                dst_type.is_float()) 
        { 
            file << "_mm_cvtepi32_ps("; 
            walk(node.get_nest());
            file << ")"; 

        } 
        else if (src_type.is_float() &&
                dst_type.is_signed_int()) 
        { 
            // C/C++ requires truncated conversion
            file << "_mm_cvttps_epi32("; 
            walk(node.get_nest());
            file << ")"; 
        } 
        else if (src_type.is_float() &&
                (dst_type.is_signed_char() ||
                 dst_type.is_char())) 
        {
            // Saturated conversion
            file << "_mm_packs_epi16("; 
            file << "_mm_packs_epi32("; 
            file << "_mm_cvttps_epi32("; 
            walk(node.get_nest());
            file << "),"; 
            file << "_mm_castps_si128(";
            walk(node.get_nest());
            file << ")";
            //file << "_mm_undefined_si128()"; 
            file << "),"; 
            file << "_mm_castps_si128(";
            walk(node.get_nest());
            file << ")";
            //file << "_mm_undefined_si128()"; 
            file << ")"; 
        } 
        else if (src_type.is_float() &&
                dst_type.is_unsigned_char()) 
        {
            // Saturated conversion
            file << "_mm_packus_epi16("; 
            file << "_mm_packus_epi32("; 
            file << "_mm_cvttps_epi32("; 
            walk(node.get_nest());
            file << "),"; 
            file << "_mm_castps_si128(";
            walk(node.get_nest());
            file << ")";
            //file << "_mm_undefined_si128()"; 
            file << "),"; 
            file << "_mm_castps_si128(";
            walk(node.get_nest());
            //file << "_mm_undefined_si128()"; 
            file << "))"; 
        } 
        else if (src_type.is_signed_int() &&
                (dst_type.is_signed_char() ||
                 dst_type.is_char())) 
        {
            // Saturated conversion
            file << "_mm_packs_epi16("; 
            file << "_mm_packs_epi32("; 
            walk(node.get_nest());
            file << ","; 
            walk(node.get_nest());
            //file << "_mm_undefined_si128()"; 
            file << "),"; 
            walk(node.get_nest());
            //file << "_mm_undefined_si128()"; 
            file << ")"; 
        } 
        else if (src_type.is_signed_int() &&
                dst_type.is_unsigned_char()) 
        {
            // Saturated conversion
            file << "_mm_packus_epi16("; 
            file << "_mm_packus_epi32("; 
            walk(node.get_nest());
            file << ","; 
            walk(node.get_nest());
            //file << "_mm_undefined_si128()"; 
            file << ")"; 
            file << ",";
            walk(node.get_nest());
            //file << "_mm_undefined_si128()"; 
            file << ")"; 
        }
        /*
        else if (src_type.is_float() &&
                dst_type.is_double()) 
        { 
            file << "ps_pd"; 
        } 
        else if (src_type.is_double() &&
                dst_type.is_float()) 
        { 
            file << "pd_ps"; 
        } 
        */
 
        else
        {
            fprintf(stderr, "SSE Codegen: Conversion at '%s' is not supported yet.\n", 
                    node.get_locus().c_str());
        }      
    }

    void SSEModuleVisitor::visit(const Nodecl::ConstantVectorPromotion& node) 
    { 
        TL::Type type = node.get_type().basic_type();

        // Intrinsic name
        file << "_mm_set1";
        
        // Postfix
        if (type.is_float()) 
        { 
            file << "_ps"; 
        } 
        else if (type.is_double()) 
        { 
            file << "_pd"; 
        } 
        else if (type.is_signed_int() ||
            type.is_unsigned_int()) 
        { 
            file << "_epi32"; 
        } 
        else if (type.is_signed_short_int() ||
            type.is_unsigned_short_int()) 
        { 
            file << "_epi16"; 
        } 
        else if (type.is_char() || 
                type.is_signed_char() ||
                type.is_unsigned_char()) 
        { 
            file << "_epi8"; 
        } 
        else
        {
            running_error("SSE Codegen: Node %s at %s has an unsupported type.", 
                    ast_print_node_type(node.get_kind()),
                    node.get_locus().c_str());
        }      

        file << "("; 
        walk(node.get_rhs());
        file << ")"; 
    }        

    void SSEModuleVisitor::visit(const Nodecl::VectorConditionalExpression& node) 
    { 
        TL::Type type = node.get_type().basic_type();

        Nodecl::NodeclBase true_node = node.get_true();
        Nodecl::NodeclBase false_node = node.get_false();
        Nodecl::NodeclBase condition_node = node.get_condition();

        TL::Type true_type = true_node.get_type().basic_type();
        TL::Type false_type = false_node.get_type().basic_type();
        TL::Type condiition_type = condition_node.get_type();

        std::string casting;

        // Intrinsic name
        file << "_mm_blend";

        // Postfix
        if (true_type.is_integral_type()
                && false_type.is_integral_type())
        {
            // TODO _epi16
            file << "v_epi8";
        }
        else if (true_type.is_float()
                && false_type.is_float())
        {
            // TODO _ps
            file << "v_ps";
            casting = "(__m128)";
        }
        else if (true_type.is_double()
                && false_type.is_double())
        {
            // TODO _pd
            file << "v_pd";
            casting = "(__m128d)";
        }
        else
        {
            running_error("SSE Codegen: Node %s at %s has an unsupported type.", 
                    ast_print_node_type(node.get_kind()),
                    node.get_locus().c_str());
        }

        file << "("; 
        walk(true_node);
        file << ", ";
        walk(false_node);
        file << ", "
            << casting;
        walk(condition_node);
        file << ")"; 
    }        

    void SSEModuleVisitor::visit(const Nodecl::VectorAssignment& node) 
    { 
        walk(node.get_lhs());
        file << " = ";
        walk(node.get_rhs());
    }                                                 

    void SSEModuleVisitor::visit(const Nodecl::VectorLoad& node) 
    { 
        TL::Type type = node.get_type().basic_type();

        // Intrinsic name
        file << "_mm_load";

        // Postfix
        if (type.is_float()) 
        { 
            file << "_ps"; 
        } 
        else if (type.is_double()) 
        { 
            file << "_pd"; 
        } 
        else if (type.is_integral_type()) 
        { 
            file << "_si128"; 
        } 
        else
        {
            running_error("SSE Codegen: Node %s at %s has an unsupported type.", 
                    ast_print_node_type(node.get_kind()),
                    node.get_locus().c_str());
        }

        file << "("; 

        if (type.is_integral_type())
        {
            // TODO
            //file << "(__m
        }

        walk(node.get_rhs());
        file << ")"; 
    }

    void SSEModuleVisitor::visit(const Nodecl::VectorStore& node) 
    { 
        TL::Type type = node.get_lhs().get_type().basic_type();

        // Intrinsic name
        file << "_mm_store";

        // Postfix
        if (type.is_float()) 
        { 
            file << "_ps("; 
        } 
        else if (type.is_double()) 
        { 
            file << "_pd("; 
        } 
        else if (type.is_integral_type()) 
        { 
            file << "_si128((__m128i *)"; 
        } 
        else
        {
            running_error("SSE Codegen: Node %s at %s has an unsupported type.", 
                    ast_print_node_type(node.get_kind()),
                    node.get_locus().c_str());
        }

        walk(node.get_lhs());
        file << ", ";
        walk(node.get_rhs());
        file << ")"; 
    }

    void SSEModuleVisitor::visit(const Nodecl::VectorFunctionCall& node) 
    {
        ((CxxBase*)_modular_visitor)->visit_function_call(
            node.as<Nodecl::FunctionCall>(), false);
    }

    void SSEModuleVisitor::visit(const Nodecl::VectorFabs& node) 
    {
        TL::Type type = node.get_type().basic_type();

        // Handcoded implementations for float and double
        if (type.is_float()) 
        { 
            file << "(_mm_and_ps(";
            walk(node.get_arguments());
            file << ", _mm_castsi128_ps(_mm_set1_epi32(0x7FFFFFFF))))"; 
        } 
        else if (type.is_double()) 
        { 
            file << "(_mm_and_pd(";
            walk(node.get_arguments());
            file << ", _mm_castsi128_pd(_mm_set1_epi64(0x7FFFFFFFFFFFFFFFLL))))"; 
        }
        else
        {
            // Intrinsic name
            file << "_mm_abs";

            // Postfix
            if (type.is_signed_int() ||
                    type.is_unsigned_int()) 
            { 
                file << "_epi32"; 
            } 
            else if (type.is_signed_short_int() ||
                    type.is_unsigned_short_int()) 
            { 
                file << "_epi16"; 
            } 
            else if (type.is_char() || 
                    type.is_signed_char() ||
                    type.is_unsigned_char()) 
            { 
                file << "_epi8"; 
            } 
            else
            {
                running_error("SSE Codegen: Node %s at %s has an unsupported type.", 
                        ast_print_node_type(node.get_kind()),
                        node.get_locus().c_str());
            }
        }
    }

    Nodecl::NodeclVisitor<void>::Ret SSEModuleVisitor::unhandled_node(const Nodecl::NodeclBase& n) 
    { 
        fprintf(stderr, "SSE Codegen: Unknown node %s at %s.\n",
                ast_print_node_type(n.get_kind()),
                n.get_locus().c_str()); 
        /*
           running_error("SSE Codegen: Unknown node %s at %s.",
           ast_print_node_type(n.get_kind()),
           n.get_locus().c_str()); 
         */
        return Ret(); 
    }
}
