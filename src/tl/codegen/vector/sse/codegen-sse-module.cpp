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
        TL::Type type = node.get_type().basic_type();

        // Intrinsic name
        file << "_mm_mul";
        
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

    void SSEModuleVisitor::visit(const Nodecl::VectorNeg& node) 
    { 
        TL::Type type = node.get_type().basic_type();
        
        if (type.is_float()) 
        { 
            file << "_mm_xor_ps((__m128) _mm_set1_epi32(0x80000000), ";
            walk(node.get_rhs());
            file << ")";
        } 
        else if (type.is_double()) 
        { 
            file << "_mm_xor_pd((__m128d) _mm_set1_epi64(0x8000000000000000LL), ";
            walk(node.get_rhs());
            file << ")";
        }
        else if (type.is_signed_int() ||
                type.is_unsigned_int())
        {
            file << "(_mm_sub_epi32( _mm_set1_epi32(0),";
            walk(node.get_rhs());
            file << "))";
        }
        else if (type.is_signed_short_int() ||
                type.is_unsigned_short_int())
        {
            file << "(_mm_sub_epi16( _mm_set1_epi32(0),";
            walk(node.get_rhs());
            file << "))";
        }
        else if (type.is_char() ||
                type.is_signed_char() ||
                type.is_unsigned_char())
        {
            file << "(_mm_sub_epi8( _mm_set1_epi32(0),";
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
        TL::Type condition_type = condition_node.get_type();

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
        }
        else if (true_type.is_double()
                && false_type.is_double())
        {
            // TODO _pd
            file << "v_pd";
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
    file << ", ";
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
            file << ", (__m128) _mm_set1_epi32(0x7FFFFFFF)))"; 
        } 
        else if (type.is_double()) 
        { 
            file << "(_mm_and_pd(";
            walk(node.get_arguments());
            file << ", (__m128) _mm_set1_epi64(0x7FFFFFFFFFFFFFFFLL)))"; 
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
        fprintf(stderr, "SSE Codegen: Unknown node %s at %s.",
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
