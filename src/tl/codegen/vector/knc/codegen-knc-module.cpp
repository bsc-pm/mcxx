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

#include "codegen-knc-module.hpp"
#include "codegen-cxx.hpp"

namespace Codegen 
{
    KNCModuleVisitor::KNCModuleVisitor(CodegenVisitor* base_codegen) 
        : CodegenModuleVisitor(base_codegen)
    {
    }

    void KNCModuleVisitor::visit(const Nodecl::VectorAdd& node) 
    { 
        TL::Type type = node.get_type().basic_type();

        // Intrinsic name
        file << "_mm512_add";
        
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
            fatal_error("KNC Codegen: Node %s at %s has an unsupported type.", 
                    ast_print_node_type(node.get_kind()),
                    node.get_locus_str().c_str());
        }      

        file << "("; 
        walk(node.get_lhs());
        file << ", ";
        walk(node.get_rhs());
        file << ")"; 
    }                                                 

    void KNCModuleVisitor::visit(const Nodecl::VectorMinus& node) 
    { 
        TL::Type type = node.get_type().basic_type();

        // Intrinsic name
        file << "_mm512_sub";
        
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
            fatal_error("KNC Codegen: Node %s at %s has an unsupported type.", 
                    ast_print_node_type(node.get_kind()),
                    node.get_locus_str().c_str());
        }      

        file << "("; 
        walk(node.get_lhs());
        file << ", ";
        walk(node.get_rhs());
        file << ")"; 
    }                                                 

    void KNCModuleVisitor::visit(const Nodecl::VectorMul& node) 
    { 
        TL::Type result_type = node.get_type().basic_type();
        TL::Type first_op_type = node.get_rhs().get_type().basic_type();
        TL::Type second_op_type = node.get_lhs().get_type().basic_type();

        // Intrinsic name
        file << "_mm512_mul";
        
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
            fatal_error("KNC Codegen: Node %s at %s has an unsupported type.", 
                    ast_print_node_type(node.get_kind()),
                    node.get_locus_str().c_str());
        }      

        file << "("; 
        walk(node.get_lhs());
        file << ", ";
        walk(node.get_rhs());
        file << ")"; 
    }    

    void KNCModuleVisitor::visit(const Nodecl::VectorDiv& node) 
    { 
        TL::Type type = node.get_type().basic_type();

        // Intrinsic name
        file << "_mm512_div";
        
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
            fatal_error("KNC Codegen: Node %s at %s has an unsupported type.", 
                    ast_print_node_type(node.get_kind()),
                    node.get_locus_str().c_str());
        }      

        file << "("; 
        walk(node.get_lhs());
        file << ", ";
        walk(node.get_rhs());
        file << ")"; 
    }                                                 

    void KNCModuleVisitor::visit(const Nodecl::VectorLowerThan& node) 
    { 
        TL::Type type = node.get_lhs().get_type().basic_type();

        // Intrinsic name
        file << "_mm512_cmplt";
        
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
            fatal_error("KNC Codegen: Node %s at %s has an unsupported type.", 
                    ast_print_node_type(node.get_kind()),
                    node.get_locus_str().c_str());
        }      

        file << "("; 
        walk(node.get_lhs());
        file << ", ";
        walk(node.get_rhs());
        file << ")"; 
    }                                                 

    void KNCModuleVisitor::visit(const Nodecl::VectorGreaterThan& node) 
    { 
        TL::Type type = node.get_lhs().get_type().basic_type();

        // Intrinsic name
        file << "_mm512_cmpgt";
        
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
            fatal_error("KNC Codegen: Node %s at %s has an unsupported type.", 
                    ast_print_node_type(node.get_kind()),
                    node.get_locus_str().c_str());
        }      

        file << "("; 
        walk(node.get_lhs());
        file << ", ";
        walk(node.get_rhs());
        file << ")"; 
    }                                                 

    void KNCModuleVisitor::visit(const Nodecl::VectorEqual& node) 
    { 
        TL::Type type = node.get_lhs().get_type().basic_type();

        // Intrinsic name
        file << "_mm512_cmpeq";
        
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
            fatal_error("KNC Codegen: Node %s at %s has an unsupported type.", 
                    ast_print_node_type(node.get_kind()),
                    node.get_locus_str().c_str());
        }      

        file << "("; 
        walk(node.get_lhs());
        file << ", ";
        walk(node.get_rhs());
        file << ")"; 
    }                                                 

    void KNCModuleVisitor::visit(const Nodecl::VectorBitwiseAnd& node) 
    { 
        TL::Type type = node.get_type().basic_type();

        // Intrinsic name
        file << "_mm512_and";
        
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
            fatal_error("KNC Codegen: Node %s at %s has an unsupported type.", 
                    ast_print_node_type(node.get_kind()),
                    node.get_locus_str().c_str());
        }      

        file << "("; 
        walk(node.get_lhs());
        file << ", ";
        walk(node.get_rhs());
        file << ")"; 
    }                                                 

    void KNCModuleVisitor::visit(const Nodecl::VectorBitwiseOr& node) 
    { 
        TL::Type type = node.get_type().basic_type();

        // Intrinsic name
        file << "_mm512_or";
        
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
            fatal_error("KNC Codegen: Node %s at %s has an unsupported type.", 
                    ast_print_node_type(node.get_kind()),
                    node.get_locus_str().c_str());
        }      

        file << "("; 
        walk(node.get_lhs());
        file << ", ";
        walk(node.get_rhs());
        file << ")"; 
    }                                                 

    void KNCModuleVisitor::visit(const Nodecl::VectorBitwiseXor& node) 
    { 
        TL::Type type = node.get_type().basic_type();

        // Intrinsic name
        file << "_mm512_xor";
        
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
            fatal_error("KNC Codegen: Node %s at %s has an unsupported type.", 
                    ast_print_node_type(node.get_kind()),
                    node.get_locus_str().c_str());
        }      

        file << "("; 
        walk(node.get_lhs());
        file << ", ";
        walk(node.get_rhs());
        file << ")"; 
    }   

    void KNCModuleVisitor::visit(const Nodecl::VectorLogicalOr& node) 
    { 
<<<<<<< HEAD:src/tl/codegen/vector/knc/codegen-knc-module.cpp
        fatal_error("KNC Codegen %s: 'logical or' operation (i.e., operator '||') is not supported in KNC. Try using 'bitwise or' operations (i.e., operator '|') instead if possible.",
                node.get_locus().c_str());
=======
        fprintf(stderr, "Warning: Logical Or operation in '%s' is not "
                "supported in SSE. Bitwise Or operation will be used instead. "
                "This might change the expected behaviour of the application.\n",
                node.get_locus_str().c_str()); 

        visit(node.as<Nodecl::VectorBitwiseOr>());
>>>>>>> master:src/tl/codegen/vector/sse/codegen-sse-module.cpp
    }                                                 

    void KNCModuleVisitor::visit(const Nodecl::VectorNeg& node) 
    { 
        TL::Type type = node.get_type().basic_type();
        
        if (type.is_float()) 
        { 
            file << "_mm512_xor_ps(_mm512_castsi128_ps(_mm512_set1_epi32(0x80000000)), ";
            walk(node.get_rhs());
            file << ")";
        } 
        else if (type.is_double()) 
        { 
            file << "_mm512_xor_pd(_mm512_castsi128_pd(_mm512_set1_epi64(0x8000000000000000LL)), ";
            walk(node.get_rhs());
            file << ")";
        }
        else if (type.is_signed_int() ||
                type.is_unsigned_int())
        {
            file << "(_mm512_sub_epi32( _mm512_setzero_si128(),";
            walk(node.get_rhs());
            file << "))";
        }
        else if (type.is_signed_short_int() ||
                type.is_unsigned_short_int())
        {
            file << "(_mm512_sub_epi16( _mm512_setzero_si128(),";
            walk(node.get_rhs());
            file << "))";
        }
        else if (type.is_char() ||
                type.is_signed_char() ||
                type.is_unsigned_char())
        {
            file << "(_mm512_sub_epi8( _mm512_setzero_si128(),";
            walk(node.get_rhs());
            file << "))";
        }
        else
        {
            fatal_error("KNC Codegen: Node %s at %s has an unsupported type.", 
                    ast_print_node_type(node.get_kind()),
                    node.get_locus_str().c_str());
        }      
    }                                                 

    void KNCModuleVisitor::visit(const Nodecl::VectorConversion& node) 
    {
        const TL::Type& src_type = node.get_nest().get_type().basic_type().get_unqualified_type();
        const TL::Type& dst_type = node.get_type().basic_type().get_unqualified_type();

        if (src_type.is_same_type(dst_type))
        {
            walk(node.get_nest());
            return;
        }
        else if ((src_type.is_signed_int() && dst_type.is_unsigned_int()) ||
                (dst_type.is_signed_int() && src_type.is_unsigned_int()) ||
                (src_type.is_signed_short_int() && dst_type.is_unsigned_short_int()) ||
                (dst_type.is_signed_short_int() && src_type.is_unsigned_short_int()))
        {
            walk(node.get_nest());
            return;
        }
        else if (src_type.is_signed_int() &&
                dst_type.is_float()) 
        { 
            file << "_mm512_cvtepi32_ps("; 
            walk(node.get_nest());
            file << ")"; 
        } 
        else if (src_type.is_float() &&
                dst_type.is_signed_int()) 
        { 
            // C/C++ requires truncated conversion
            file << "_mm512_cvttps_epi32("; 
            walk(node.get_nest());
            file << ")"; 
        } 
        else if (src_type.is_float() &&
                (dst_type.is_signed_char() ||
                 dst_type.is_char())) 
        {
            // Saturated conversion
            file << "_mm512_packs_epi16("; 
            file << "_mm512_packs_epi32("; 
            file << "_mm512_cvttps_epi32("; 
            walk(node.get_nest());
            file << "),"; 
            file << "_mm512_castps_si128(";
            walk(node.get_nest());
            file << ")";
            //file << "_mm512_undefined_si128()"; 
            file << "),"; 
            file << "_mm512_castps_si128(";
            walk(node.get_nest());
            file << ")";
            //file << "_mm512_undefined_si128()"; 
            file << ")"; 
        } 
        else if (src_type.is_float() &&
                dst_type.is_unsigned_char()) 
        {
            // Saturated conversion
            file << "_mm512_packus_epi16("; 
            file << "_mm512_packus_epi32("; 
            file << "_mm512_cvttps_epi32("; 
            walk(node.get_nest());
            file << "),"; 
            file << "_mm512_castps_si128(";
            walk(node.get_nest());
            file << ")";
            //file << "_mm512_undefined_si128()"; 
            file << "),"; 
            file << "_mm512_castps_si128(";
            walk(node.get_nest());
            //file << "_mm512_undefined_si128()"; 
            file << "))"; 
        } 
        else if (src_type.is_signed_int() &&
                (dst_type.is_signed_char() ||
                 dst_type.is_char())) 
        {
            // Saturated conversion
            file << "_mm512_packs_epi16("; 
            file << "_mm512_packs_epi32("; 
            walk(node.get_nest());
            file << ","; 
            walk(node.get_nest());
            //file << "_mm512_undefined_si128()"; 
            file << "),"; 
            walk(node.get_nest());
            //file << "_mm512_undefined_si128()"; 
            file << ")"; 
        } 
        else if (src_type.is_signed_int() &&
                dst_type.is_unsigned_char()) 
        {
            // Saturated conversion
            file << "_mm512_packus_epi16("; 
            file << "_mm512_packus_epi32("; 
            walk(node.get_nest());
            file << ","; 
            walk(node.get_nest());
            //file << "_mm512_undefined_si128()"; 
            file << ")"; 
            file << ",";
            walk(node.get_nest());
            //file << "_mm512_undefined_si128()"; 
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
<<<<<<< HEAD:src/tl/codegen/vector/knc/codegen-knc-module.cpp
            fprintf(stderr, "KNC Codegen: Conversion at '%s' is not supported yet: %s\n", 
                    node.get_locus().c_str(),
                    node.get_nest().prettyprint().c_str());
=======
            fprintf(stderr, "SSE Codegen: Conversion at '%s' is not supported yet.\n", 
                    node.get_locus_str().c_str());
>>>>>>> master:src/tl/codegen/vector/sse/codegen-sse-module.cpp
        }      
    }

    void KNCModuleVisitor::visit(const Nodecl::VectorPromotion& node) 
    { 
        TL::Type type = node.get_type().basic_type();

        // Intrinsic name
        file << "_mm512_set1";
        
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
            fatal_error("KNC Codegen: Node %s at %s has an unsupported type.", 
                    ast_print_node_type(node.get_kind()),
                    node.get_locus_str().c_str());
        }      

        file << "("; 
        walk(node.get_rhs());
        file << ")"; 
    }        

    void KNCModuleVisitor::visit(const Nodecl::VectorLiteral& node) 
    { 
        TL::Type type = node.get_type().basic_type();

        // Intrinsic name
        file << "_mm512_set";
        
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
            fatal_error("KNC Codegen: Node %s at %s has an unsupported type.", 
                    ast_print_node_type(node.get_kind()),
                    node.get_locus().c_str());
        }      

        file << "(";

        Nodecl::List scalar_values =
            node.get_scalar_values().as<Nodecl::List>();

        Nodecl::List::const_iterator it = scalar_values.begin();
        walk((*it));
        it++;

        for (; it != scalar_values.end();
            it++)
        {
            file << ", ";
            walk((*it));
        }

        file << ")"; 
    }        


    void KNCModuleVisitor::visit(const Nodecl::VectorConditionalExpression& node) 
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
        file << "_mm512_blend";

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

            casting = "(";
            casting += print_type_str(TL::Type::get_float_type().get_vector_to(16).get_internal_type(),
                    node.retrieve_context().get_decl_context());
            casting += ")";
        }
        else if (true_type.is_double()
                && false_type.is_double())
        {
            // TODO _pd
            file << "v_pd";
            casting = "(";
            casting += print_type_str(TL::Type::get_double_type().get_vector_to(16).get_internal_type(),
                    node.retrieve_context().get_decl_context());
            casting += ")";
        }
        else
        {
            fatal_error("KNC Codegen: Node %s at %s has an unsupported type.", 
                    ast_print_node_type(node.get_kind()),
                    node.get_locus_str().c_str());
        }

        file << "("; 
        walk(false_node); // False first!
        file << ", ";
        walk(true_node);
        file << ", "
            << casting;
        walk(condition_node);
        file << ")"; 
    }        

    void KNCModuleVisitor::visit(const Nodecl::VectorAssignment& node) 
    { 
        walk(node.get_lhs());
        file << " = ";
        walk(node.get_rhs());
    }                                                 

    void KNCModuleVisitor::visit(const Nodecl::VectorLoad& node) 
    { 
        TL::Type type = node.get_type().basic_type();

        // Intrinsic name
        file << "_mm512_load";

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
            fatal_error("KNC Codegen: Node %s at %s has an unsupported type.", 
                    ast_print_node_type(node.get_kind()),
                    node.get_locus_str().c_str());
        }

        file << "("; 

        if (type.is_integral_type())
        {
            file << "(" << print_type_str(TL::Type::get_long_long_int_type().get_vector_to(16).get_pointer_to().get_internal_type(),
                    node.retrieve_context().get_decl_context()) << ")";
        }

        walk(node.get_rhs());
        file << ")"; 
    }

    void KNCModuleVisitor::visit(const Nodecl::VectorStore& node) 
    { 
        TL::Type type = node.get_lhs().get_type().basic_type();

        // Intrinsic name
        file << "_mm512_store";

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
            file << "_si128((" << print_type_str(TL::Type::get_long_long_int_type().get_vector_to(16).get_pointer_to().get_internal_type(),
                    node.retrieve_context().get_decl_context()) << ")";
        } 
        else
        {
            fatal_error("KNC Codegen: Node %s at %s has an unsupported type.", 
                    ast_print_node_type(node.get_kind()),
                    node.get_locus_str().c_str());
        }

        walk(node.get_lhs());
        file << ", ";
        walk(node.get_rhs());
        file << ")"; 
    }

    void KNCModuleVisitor::visit(const Nodecl::VectorGather& node) 
    { 
        TL::Type type = node.get_type().basic_type();
        TL::Type index_type = node.get_strides().get_type().basic_type();

        // Intrinsic name
        file << "_mm512_set";

        std::string extract;

        // Postfix
        if (type.is_float()) 
        { 
            file << "_ps";
        } 
        else if (type.is_signed_int() || type.is_unsigned_int()) 
        { 
            file << "_epi32";
        }
        else if (type.is_signed_short_int() || type.is_unsigned_short_int()) 
        { 
            file << "_epi16";
        }
        else if (type.is_signed_char() || type.is_char() || type.is_unsigned_char())
        {
            file << "_epi8";
        }
        else
        {
            fatal_error("KNC Codegen: Node %s at %s has an unsupported type.", 
                    ast_print_node_type(node.get_kind()),
                    node.get_locus().c_str());
        }

        // Indexes
        if (index_type.is_signed_int() || index_type.is_unsigned_int()) 
        { 
            extract = "_mm512_extract_epi32";
        }
        else if (index_type.is_signed_short_int() || index_type.is_unsigned_short_int()) 
        { 
            extract = "_mm512_extract_epi16";
        }
        else if (index_type.is_signed_char() || index_type.is_char() || index_type.is_unsigned_char())
        {
            extract = "_mm512_extract_epi8";
        }
        else
        {
            fatal_error("KNC Codegen: Node %s at %s has an unsupported type.", 
                    ast_print_node_type(node.get_kind()),
                    node.get_locus().c_str());
        }


        file << "("; 

        unsigned int i = 0;
        
        walk(node.get_base());
        file << "[";

        file << extract;
        file << "(";
        walk(node.get_strides());
        file << ", " << i << ")";

        file << "]";

        i++;

        for (; i < type.get_size(); i++)
        {
            file << ", ";

            walk(node.get_base());
            file << "[";

            file << extract;
            file << "(";
            walk(node.get_strides());
            file << ", " << i << ")";

            file << "]";
        }

        file << ")"; 
    }

    void KNCModuleVisitor::visit(const Nodecl::VectorScatter& node) 
    { 
        TL::Type type = node.get_source().get_type().basic_type();
        TL::Type index_type = node.get_strides().get_type().basic_type();

        std::string extract_index;
        std::string extract_source;

        // Indexes
        if (index_type.is_signed_int() || index_type.is_unsigned_int()) 
        { 
            extract_index = "_mm512_extract_epi32";
        }
        else if (index_type.is_signed_short_int() || index_type.is_unsigned_short_int()) 
        { 
            extract_index = "_mm512_extract_epi16";
        }
        else if (index_type.is_signed_char() || index_type.is_char() || index_type.is_unsigned_char())
        {
            extract_index = "_mm512_extract_epi8";
        }
        else
        {
            fatal_error("KNC Codegen: Node %s at %s has an unsupported index type.", 
                    ast_print_node_type(node.get_kind()),
                    node.get_locus().c_str());
        }

        // Sourcei
        if (type.is_float())
        {
            extract_source = "_mm512_extract_ps";
        }
        else if (type.is_signed_int() || type.is_unsigned_int()) 
        { 
            extract_source = "_mm512_extract_epi32";
        }
        else if (type.is_signed_short_int() || type.is_unsigned_short_int()) 
        { 
            extract_source = "_mm512_extract_epi16";
        }
        else if (type.is_signed_char() || type.is_char() || type.is_unsigned_char())
        {
            extract_source = "_mm512_extract_epi8";
        }
        else
        {
            fatal_error("KNC Codegen: Node %s at %s has an unsupported source type.", 
                    ast_print_node_type(node.get_kind()),
                    node.get_locus().c_str());
        }


        for (unsigned int i=0; i < type.get_size(); i++)
        {
            walk(node.get_base());
            file << "[";

            file << extract_index;
            file << "(";
            walk(node.get_strides());
            file << ", ";
            file << i;
            file << ")";

            file << "]";

            file << " = " << extract_source << "(";
            walk(node.get_source());
            file << ", ";
            file << i;
            file << ")";
 
            file << ";" << std::endl;
        }
    }

    void KNCModuleVisitor::visit(const Nodecl::VectorFunctionCall& node) 
    {
        ((CxxBase*)_modular_visitor)->visit_function_call(
            node.as<Nodecl::FunctionCall>(), false);
    }

    void KNCModuleVisitor::visit(const Nodecl::VectorFabs& node) 
    {
        TL::Type type = node.get_type().basic_type();

        // Handcoded implementations for float and double
        if (type.is_float()) 
        { 
            file << "(_mm512_and_ps(";
            walk(node.get_arguments());
            file << ", _mm512_castsi128_ps(_mm512_set1_epi32(0x7FFFFFFF))))"; 
        } 
        else if (type.is_double()) 
        { 
            file << "(_mm512_and_pd(";
            walk(node.get_arguments());
            file << ", _mm512_castsi128_pd(_mm512_set1_epi64(0x7FFFFFFFFFFFFFFFLL))))"; 
        }
        else
        {
            // Intrinsic name
            file << "_mm512_abs";

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
                fatal_error("KNC Codegen: Node %s at %s has an unsupported type.", 
                        ast_print_node_type(node.get_kind()),
                        node.get_locus_str().c_str());
            }
        }
    }

    Nodecl::NodeclVisitor<void>::Ret KNCModuleVisitor::unhandled_node(const Nodecl::NodeclBase& n) 
    { 
        fprintf(stderr, "KNC Codegen: Unknown node %s at %s.\n",
                ast_print_node_type(n.get_kind()),
                n.get_locus_str().c_str()); 
        /*
           fatal_error("KNC Codegen: Unknown node %s at %s.",
           ast_print_node_type(n.get_kind()),
           n.get_locus_str().c_str()); 
         */
        return Ret(); 
    }
}
