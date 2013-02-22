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

#include "tl-vector-lowering-sse.hpp"

namespace TL 
{
    namespace Vectorization
    {
        SSEVectorLowering::SSEVectorLowering() 
        {
        }

        void SSEVectorLowering::visit(const Nodecl::VectorAdd& node) 
        { 
            TL::Type type = node.get_type().basic_type();

            std::stringstream intrin_name;

            intrin_name << "_mm_add";

            // Postfix
            if (type.is_float()) 
            { 
                intrin_name << "_ps"; 
            } 
            else if (type.is_double()) 
            { 
                intrin_name << "_pd"; 
            } 
            else if (type.is_signed_int() ||
                    type.is_unsigned_int()) 
            { 
                intrin_name << "_epi32"; 
            } 
            else if (type.is_signed_short_int() ||
                    type.is_unsigned_short_int()) 
            { 
                intrin_name << "_epi16"; 
            } 
            else if (type.is_char() || 
                    type.is_signed_char() ||
                    type.is_unsigned_char()) 
            { 
                intrin_name << "_epi8"; 
            } 
            else
            {
                running_error("SSE Lowering: Node %s at %s has an unsupported type.", 
                        ast_print_node_type(node.get_kind()),
                        node.get_locus().c_str());
            }      

            walk(node.get_lhs());
            walk(node.get_rhs());

            Nodecl::Symbol intrin_sym = node.retrieve_context().get_symbol_from_name(
                    intrin_name.str()).make_nodecl(node.get_filename(), node.get_line());

            Nodecl::FunctionCall intrin_node =
                Nodecl::FunctionCall::make(
                        intrin_sym,
                        Nodecl::List::make( /*Arguments*/
                            node.get_lhs(),
                            node.get_rhs()),
                        nodecl_null(),
                        nodecl_null(),
                        intrin_sym.get_symbol().get_type(),
                        node.get_filename(),
                        node.get_line());

            node.replace(intrin_node);
        }                                                 

        void SSEVectorLowering::visit(const Nodecl::VectorMinus& node) 
        { 
            TL::Type type = node.get_type().basic_type();

            std::stringstream intrin_name;

            intrin_name << "_mm_sub";

            // Postfix
            if (type.is_float()) 
            { 
                intrin_name << "_ps"; 
            } 
            else if (type.is_double()) 
            { 
                intrin_name << "_pd"; 
            } 
            else if (type.is_signed_int() ||
                    type.is_unsigned_int()) 
            { 
                intrin_name << "_epi32"; 
            } 
            else if (type.is_signed_short_int() ||
                    type.is_unsigned_short_int()) 
            { 
                intrin_name << "_epi16"; 
            } 
            else if (type.is_char() || 
                    type.is_signed_char() ||
                    type.is_unsigned_char()) 
            { 
                intrin_name << "_epi8"; 
            } 
            else
            {
                running_error("SSE Lowering: Node %s at %s has an unsupported type.", 
                        ast_print_node_type(node.get_kind()),
                        node.get_locus().c_str());
            }      

            walk(node.get_lhs());
            walk(node.get_rhs());

            Nodecl::Symbol intrin_sym = node.retrieve_context().get_symbol_from_name(
                    intrin_name.str()).make_nodecl(node.get_filename(), node.get_line());

            Nodecl::FunctionCall intrin_node =
                Nodecl::FunctionCall::make(
                        intrin_sym,
                        Nodecl::List::make( /*Arguments*/
                            node.get_lhs(),
                            node.get_rhs()),
                        nodecl_null(),
                        nodecl_null(),
                        intrin_sym.get_symbol().get_type(),
                        node.get_filename(),
                        node.get_line());

            node.replace(intrin_node);
        }                                                 

        void SSEVectorLowering::visit(const Nodecl::VectorMul& node) 
        {
            TL::Type result_type = node.get_type().basic_type();
            TL::Type first_op_type = node.get_rhs().get_type().basic_type();
            TL::Type second_op_type = node.get_lhs().get_type().basic_type();

            std::stringstream intrin_name;

            intrin_name << "_mm_mul";

            // Postfix
            if (result_type.is_float() &&
                    first_op_type.is_float() &&
                    second_op_type.is_float())
            {
                intrin_name << "_ps"; 
            } 
            else if (result_type.is_double() &&
                    first_op_type.is_double() &&
                    second_op_type.is_double())
            { 
                intrin_name << "_pd"; 
            }
            else if (result_type.is_signed_int() &&
                    first_op_type.is_signed_int() &&
                    second_op_type.is_signed_int())
            {
                intrin_name << "lo_epi32"; 
            } 
            else if (result_type.is_unsigned_int() &&
                    first_op_type.is_unsigned_int() &&
                    second_op_type.is_unsigned_int())
            {
                intrin_name << "lo_epi32"; 
            } 

            /* 
               else if (type.is_signed_short_int() ||
               type.is_unsigned_short_int()) 
               { 
               intrin_name << "_epi16"; 
               } 
               else if (type.is_char() || 
               type.is_signed_char() ||
               type.is_unsigned_char()) 
               { 
               intrin_name << "_epi8"; 
               }
             */ 
            else
            {
                running_error("SSE Lowering: Node %s at %s has an unsupported type.", 
                        ast_print_node_type(node.get_kind()),
                        node.get_locus().c_str());
            }      

            walk(node.get_lhs());
            walk(node.get_rhs());

            Nodecl::Symbol intrin_sym = node.retrieve_context().get_symbol_from_name(
                    intrin_name.str()).make_nodecl(node.get_filename(), node.get_line());

            Nodecl::FunctionCall intrin_node =
                Nodecl::FunctionCall::make(
                        intrin_sym,
                        Nodecl::List::make( /*Arguments*/
                            node.get_lhs(),
                            node.get_rhs()),
                        nodecl_null(),
                        nodecl_null(),
                        intrin_sym.get_symbol().get_type(),
                        node.get_filename(),
                        node.get_line());

            node.replace(intrin_node);
        }    

        void SSEVectorLowering::visit(const Nodecl::VectorDiv& node) 
        { 
            TL::Type type = node.get_type().basic_type();

            std::stringstream intrin_name;

            // Intrinsic name
            intrin_name << "_mm_div";

            // Postfix
            if (type.is_float()) 
            { 
                intrin_name << "_ps"; 
            } 
            else if (type.is_double()) 
            { 
                intrin_name << "_pd"; 
            } 
            else if (type.is_signed_int() ||
                    type.is_unsigned_int()) 
            { 
                intrin_name << "_epi32"; 
            } 
            else if (type.is_signed_short_int() ||
                    type.is_unsigned_short_int()) 
            { 
                intrin_name << "_epi16"; 
            } 
            else if (type.is_char() || 
                    type.is_signed_char() ||
                    type.is_unsigned_char()) 
            { 
                intrin_name << "_epi8"; 
            } 
            else
            {
                running_error("SSE Lowering: Node %s at %s has an unsupported type.", 
                        ast_print_node_type(node.get_kind()),
                        node.get_locus().c_str());
            }      

            walk(node.get_lhs());
            walk(node.get_rhs());

            Nodecl::Symbol intrin_sym = node.retrieve_context().get_symbol_from_name(
                    intrin_name.str()).make_nodecl(node.get_filename(), node.get_line());

            Nodecl::FunctionCall intrin_node =
                Nodecl::FunctionCall::make(
                        intrin_sym,
                        Nodecl::List::make( /*Arguments*/
                            node.get_lhs(),
                            node.get_rhs()),
                        nodecl_null(),
                        nodecl_null(),
                        intrin_sym.get_symbol().get_type(),
                        node.get_filename(),
                        node.get_line());

            node.replace(intrin_node);
        }                                                 

        void SSEVectorLowering::visit(const Nodecl::VectorLowerThan& node) 
        { 
            TL::Type type = node.get_lhs().get_type().basic_type();

            std::stringstream intrin_name;

            // Intrinsic name
            intrin_name << "_mm_cmplt";

            // Postfix
            if (type.is_float()) 
            { 
                intrin_name << "_ps"; 
            } 
            else if (type.is_signed_int() ||
                    type.is_unsigned_int()) 
            { 
                intrin_name << "_epi32"; 
            } 
            else if (type.is_signed_short_int() ||
                    type.is_unsigned_short_int()) 
            { 
                intrin_name << "_epi16"; 
            } 
            else if (type.is_char() || 
                    type.is_signed_char() ||
                    type.is_unsigned_char()) 
            { 
                intrin_name << "_epi8"; 
            } 
            else
            {
                running_error("SSE Lowering: Node %s at %s has an unsupported type.", 
                        ast_print_node_type(node.get_kind()),
                        node.get_locus().c_str());
            }      

            walk(node.get_lhs());
            walk(node.get_rhs());

            Nodecl::Symbol intrin_sym = node.retrieve_context().get_symbol_from_name(
                    intrin_name.str()).make_nodecl(node.get_filename(), node.get_line());

            Nodecl::FunctionCall intrin_node =
                Nodecl::FunctionCall::make(
                        intrin_sym,
                        Nodecl::List::make( /*Arguments*/
                            node.get_lhs(),
                            node.get_rhs()),
                        nodecl_null(),
                        nodecl_null(),
                        intrin_sym.get_symbol().get_type(),
                        node.get_filename(),
                        node.get_line());

            node.replace(intrin_node);
        }                                                 

        void SSEVectorLowering::visit(const Nodecl::VectorGreaterThan& node) 
        { 
            TL::Type type = node.get_lhs().get_type().basic_type();

            std::stringstream intrin_name;

            // Intrinsic name
            intrin_name << "_mm_cmpgt";

            // Postfix
            if (type.is_float()) 
            { 
                intrin_name << "_ps"; 
            } 
            else if (type.is_signed_int() ||
                    type.is_unsigned_int()) 
            { 
                intrin_name << "_epi32"; 
            } 
            else if (type.is_signed_short_int() ||
                    type.is_unsigned_short_int()) 
            { 
                intrin_name << "_epi16"; 
            } 
            else if (type.is_char() || 
                    type.is_signed_char() ||
                    type.is_unsigned_char()) 
            { 
                intrin_name << "_epi8"; 
            } 
            else
            {
                running_error("SSE Lowering: Node %s at %s has an unsupported type.", 
                        ast_print_node_type(node.get_kind()),
                        node.get_locus().c_str());
            }      

            walk(node.get_lhs());
            walk(node.get_rhs());

            Nodecl::Symbol intrin_sym = node.retrieve_context().get_symbol_from_name(
                    intrin_name.str()).make_nodecl(node.get_filename(), node.get_line());

            Nodecl::FunctionCall intrin_node =
                Nodecl::FunctionCall::make(
                        intrin_sym,
                        Nodecl::List::make( /*Arguments*/
                            node.get_lhs(),
                            node.get_rhs()),
                        nodecl_null(),
                        nodecl_null(),
                        intrin_sym.get_symbol().get_type(),
                        node.get_filename(),
                        node.get_line());

            node.replace(intrin_node);
        }                                                 

        void SSEVectorLowering::visit(const Nodecl::VectorEqual& node) 
        { 
            TL::Type type = node.get_lhs().get_type().basic_type();

            std::stringstream intrin_name;

            // Intrinsic name
            intrin_name << "_mm_cmpeq";

            // Postfix
            if (type.is_float()) 
            { 
                intrin_name << "_ps"; 
            } 
            else if (type.is_signed_int() ||
                    type.is_unsigned_int()) 
            { 
                intrin_name << "_epi32"; 
            } 
            else if (type.is_signed_short_int() ||
                    type.is_unsigned_short_int()) 
            { 
                intrin_name << "_epi16"; 
            } 
            else if (type.is_char() || 
                    type.is_signed_char() ||
                    type.is_unsigned_char()) 
            { 
                intrin_name << "_epi8"; 
            } 
            else
            {
                running_error("SSE Lowering: Node %s at %s has an unsupported type.", 
                        ast_print_node_type(node.get_kind()),
                        node.get_locus().c_str());
            }      

            walk(node.get_lhs());
            walk(node.get_rhs());

            Nodecl::Symbol intrin_sym = node.retrieve_context().get_symbol_from_name(
                    intrin_name.str()).make_nodecl(node.get_filename(), node.get_line());

            Nodecl::FunctionCall intrin_node =
                Nodecl::FunctionCall::make(
                        intrin_sym,
                        Nodecl::List::make( /*Arguments*/
                            node.get_lhs(),
                            node.get_rhs()),
                        nodecl_null(),
                        nodecl_null(),
                        intrin_sym.get_symbol().get_type(),
                        node.get_filename(),
                        node.get_line());

            node.replace(intrin_node);
        }                                                 

        void SSEVectorLowering::visit(const Nodecl::VectorBitwiseAnd& node) 
        { 
            TL::Type type = node.get_type().basic_type();

            std::stringstream intrin_name;

            // Intrinsic name
            intrin_name << "_mm_and";

            // Postfix
            if (type.is_float()) 
            { 
                intrin_name << "_ps"; 
            } 
            else if (type.is_integral_type())
            { 
                intrin_name << "_si128"; 
            } 
            else
            {
                running_error("SSE Lowering: Node %s at %s has an unsupported type.", 
                        ast_print_node_type(node.get_kind()),
                        node.get_locus().c_str());
            }      

            walk(node.get_lhs());
            walk(node.get_rhs());

            Nodecl::Symbol intrin_sym = node.retrieve_context().get_symbol_from_name(
                    intrin_name.str()).make_nodecl(node.get_filename(), node.get_line());

            Nodecl::FunctionCall intrin_node =
                Nodecl::FunctionCall::make(
                        intrin_sym,
                        Nodecl::List::make( /*Arguments*/
                            node.get_lhs(),
                            node.get_rhs()),
                        nodecl_null(),
                        nodecl_null(),
                        intrin_sym.get_symbol().get_type(),
                        node.get_filename(),
                        node.get_line());

            node.replace(intrin_node);
        }                                                 

        void SSEVectorLowering::visit(const Nodecl::VectorBitwiseOr& node) 
        { 
            TL::Type type = node.get_type().basic_type();

            std::stringstream intrin_name;

            // Intrinsic name
            intrin_name << "_mm_or";

            // Postfix
            if (type.is_float()) 
            { 
                intrin_name << "_ps"; 
            } 
            else if (type.is_integral_type())
            { 
                intrin_name << "_si128"; 
            } 
            else
            {
                running_error("SSE Lowering: Node %s at %s has an unsupported type.", 
                        ast_print_node_type(node.get_kind()),
                        node.get_locus().c_str());
            }      

            walk(node.get_lhs());
            walk(node.get_rhs());

            Nodecl::Symbol intrin_sym = node.retrieve_context().get_symbol_from_name(
                    intrin_name.str()).make_nodecl(node.get_filename(), node.get_line());

            Nodecl::FunctionCall intrin_node =
                Nodecl::FunctionCall::make(
                        intrin_sym,
                        Nodecl::List::make( /*Arguments*/
                            node.get_lhs(),
                            node.get_rhs()),
                        nodecl_null(),
                        nodecl_null(),
                        intrin_sym.get_symbol().get_type(),
                        node.get_filename(),
                        node.get_line());

            node.replace(intrin_node);
        }                                                 

        void SSEVectorLowering::visit(const Nodecl::VectorBitwiseXor& node) 
        { 
            TL::Type type = node.get_type().basic_type();

            std::stringstream intrin_name;

            // Intrinsic name
            intrin_name << "_mm_xor";

            // Postfix
            if (type.is_float()) 
            { 
                intrin_name << "_ps"; 
            } 
            else if (type.is_integral_type())
            { 
                intrin_name << "_si128"; 
            }
            else
            {
                running_error("SSE Lowering: Node %s at %s has an unsupported type.", 
                        ast_print_node_type(node.get_kind()),
                        node.get_locus().c_str());
            }      

            walk(node.get_lhs());
            walk(node.get_rhs());

            Nodecl::Symbol intrin_sym = node.retrieve_context().get_symbol_from_name(
                    intrin_name.str()).make_nodecl(node.get_filename(), node.get_line());

            Nodecl::FunctionCall intrin_node =
                Nodecl::FunctionCall::make(
                        intrin_sym,
                        Nodecl::List::make( /*Arguments*/
                            node.get_lhs(),
                            node.get_rhs()),
                        nodecl_null(),
                        nodecl_null(),
                        intrin_sym.get_symbol().get_type(),
                        node.get_filename(),
                        node.get_line());

            node.replace(intrin_node);
        }   

        void SSEVectorLowering::visit(const Nodecl::VectorLogicalOr& node) 
        { 
            running_error("SSE Lowering %s: 'logical or' operation (i.e., operator '||') is not supported in SSE. Try using 'bitwise or' operations (i.e., operator '|') instead if possible.",
                    node.get_locus().c_str());
        }                                                 

        void SSEVectorLowering::visit(const Nodecl::VectorNeg& node) 
        {
            /* 
            TL::Type type = node.get_type().basic_type();

            std::stringstream intrin_name;

            if (type.is_float()) 
            { 
                intrin_name << "_mm_xor_ps(_mm_castsi128_ps(_mm_set1_epi32(0x80000000)), ";
                walk(node.get_rhs());
                intrin_name << ")";
            } 
            else if (type.is_double()) 
            { 
                intrin_name << "_mm_xor_pd(_mm_castsi128_pd(_mm_set1_epi64(0x8000000000000000LL)), ";
                walk(node.get_rhs());
                intrin_name << ")";
            }
            else if (type.is_signed_int() ||
                    type.is_unsigned_int())
            {
                intrin_name << "(_mm_sub_epi32( _mm_setzero_si128(),";
                walk(node.get_rhs());
                intrin_name << "))";
            }
            else if (type.is_signed_short_int() ||
                    type.is_unsigned_short_int())
            {
                intrin_name << "(_mm_sub_epi16( _mm_setzero_si128(),";
                walk(node.get_rhs());
                intrin_name << "))";
            }
            else if (type.is_char() ||
                    type.is_signed_char() ||
                    type.is_unsigned_char())
            {
                intrin_name << "(_mm_sub_epi8( _mm_setzero_si128(),";
                walk(node.get_rhs());
                intrin_name << "))";
            }
            else
            {
                running_error("SSE Lowering: Node %s at %s has an unsupported type.", 
                        ast_print_node_type(node.get_kind()),
                        node.get_locus().c_str());
            } 
       */     
        }                                                 

        void SSEVectorLowering::visit(const Nodecl::VectorConversion& node) 
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
            /*
            else if (src_type.is_signed_int() &&
                    dst_type.is_float()) 
            { 
                intrin_name << "_mm_cvtepi32_ps("; 
                walk(node.get_nest());
                intrin_name << ")"; 
            } 
            else if (src_type.is_float() &&
                    dst_type.is_signed_int()) 
            { 
                // C/C++ requires truncated conversion
                intrin_name << "_mm_cvttps_epi32("; 
                walk(node.get_nest());
                intrin_name << ")"; 
            } 
            else if (src_type.is_float() &&
                    (dst_type.is_signed_char() ||
                     dst_type.is_char())) 
            {
                // Saturated conversion
                intrin_name << "_mm_packs_epi16("; 
                intrin_name << "_mm_packs_epi32("; 
                intrin_name << "_mm_cvttps_epi32("; 
                walk(node.get_nest());
                intrin_name << "),"; 
                intrin_name << "_mm_castps_si128(";
                walk(node.get_nest());
                intrin_name << ")";
                //intrin_name << "_mm_undefined_si128()"; 
                intrin_name << "),"; 
                intrin_name << "_mm_castps_si128(";
                walk(node.get_nest());
                intrin_name << ")";
                //intrin_name << "_mm_undefined_si128()"; 
                intrin_name << ")"; 
            } 
            else if (src_type.is_float() &&
                    dst_type.is_unsigned_char()) 
            {
                // Saturated conversion
                intrin_name << "_mm_packus_epi16("; 
                intrin_name << "_mm_packus_epi32("; 
                intrin_name << "_mm_cvttps_epi32("; 
                walk(node.get_nest());
                intrin_name << "),"; 
                intrin_name << "_mm_castps_si128(";
                walk(node.get_nest());
                intrin_name << ")";
                //intrin_name << "_mm_undefined_si128()"; 
                intrin_name << "),"; 
                intrin_name << "_mm_castps_si128(";
                walk(node.get_nest());
                //intrin_name << "_mm_undefined_si128()"; 
                intrin_name << "))"; 
            } 
            else if (src_type.is_signed_int() &&
                    (dst_type.is_signed_char() ||
                     dst_type.is_char())) 
            {
                // Saturated conversion
                intrin_name << "_mm_packs_epi16("; 
                intrin_name << "_mm_packs_epi32("; 
                walk(node.get_nest());
                intrin_name << ","; 
                walk(node.get_nest());
                //intrin_name << "_mm_undefined_si128()"; 
                intrin_name << "),"; 
                walk(node.get_nest());
                //intrin_name << "_mm_undefined_si128()"; 
                intrin_name << ")"; 
            } 
            else if (src_type.is_signed_int() &&
                    dst_type.is_unsigned_char()) 
            {
                // Saturated conversion
                intrin_name << "_mm_packus_epi16("; 
                intrin_name << "_mm_packus_epi32("; 
                walk(node.get_nest());
                intrin_name << ","; 
                walk(node.get_nest());
                //intrin_name << "_mm_undefined_si128()"; 
                intrin_name << ")"; 
                intrin_name << ",";
                walk(node.get_nest());
                //intrin_name << "_mm_undefined_si128()"; 
                intrin_name << ")"; 
            }
            */
            /*
               else if (src_type.is_float() &&
               dst_type.is_double()) 
               { 
               intrin_name << "ps_pd"; 
               } 
               else if (src_type.is_double() &&
               dst_type.is_float()) 
               { 
               intrin_name << "pd_ps"; 
               } 
             */
/*
            else
            {
                fprintf(stderr, "SSE Lowering: Conversion at '%s' is not supported yet: %s\n", 
                        node.get_locus().c_str(),
                        node.get_nest().prettyprint().c_str());
            }      
            */
        }

        void SSEVectorLowering::visit(const Nodecl::VectorPromotion& node) 
        { 
            TL::Type type = node.get_type().basic_type();

            std::stringstream intrin_name;

            // Intrinsic name
            intrin_name << "_mm_set1";

            // Postfix
            if (type.is_float()) 
            { 
                intrin_name << "_ps"; 
            } 
            else if (type.is_double()) 
            { 
                intrin_name << "_pd"; 
            } 
            else if (type.is_signed_int() ||
                    type.is_unsigned_int()) 
            { 
                intrin_name << "_epi32"; 
            } 
            else if (type.is_signed_short_int() ||
                    type.is_unsigned_short_int()) 
            { 
                intrin_name << "_epi16"; 
            } 
            else if (type.is_char() || 
                    type.is_signed_char() ||
                    type.is_unsigned_char()) 
            { 
                intrin_name << "_epi8"; 
            } 
            else
            {
                running_error("SSE Lowering: Node %s at %s has an unsupported type.", 
                        ast_print_node_type(node.get_kind()),
                        node.get_locus().c_str());
            }      

            walk(node.get_rhs());

            Nodecl::Symbol intrin_sym = node.retrieve_context().get_symbol_from_name(
                    intrin_name.str()).make_nodecl(node.get_filename(), node.get_line());

            Nodecl::FunctionCall intrin_node =
                Nodecl::FunctionCall::make(
                        intrin_sym,
                        Nodecl::List::make( /*Arguments*/
                            node.get_rhs()),
                        nodecl_null(),
                        nodecl_null(),
                        intrin_sym.get_symbol().get_type(),
                        node.get_filename(),
                        node.get_line());

            node.replace(intrin_node);
        }        

        void SSEVectorLowering::visit(const Nodecl::VectorLiteral& node) 
        {
            /* 
            TL::Type type = node.get_type().basic_type();

            std::stringstream intrin_name;

            // Intrinsic name
            intrin_name << "_mm_set";

            // Postfix
            if (type.is_float()) 
            { 
                intrin_name << "_ps"; 
            } 
            else if (type.is_double()) 
            { 
                intrin_name << "_pd"; 
            } 
            else if (type.is_signed_int() ||
                    type.is_unsigned_int()) 
            { 
                intrin_name << "_epi32"; 
            } 
            else if (type.is_signed_short_int() ||
                    type.is_unsigned_short_int()) 
            { 
                intrin_name << "_epi16"; 
            } 
            else if (type.is_char() || 
                    type.is_signed_char() ||
                    type.is_unsigned_char()) 
            { 
                intrin_name << "_epi8"; 
            } 
            else
            {
                running_error("SSE Lowering: Node %s at %s has an unsupported type.", 
                        ast_print_node_type(node.get_kind()),
                        node.get_locus().c_str());
            }      

            intrin_name << "(";

            Nodecl::List scalar_values =
                node.get_scalar_values().as<Nodecl::List>();

            Nodecl::List::const_iterator it = scalar_values.begin();
            walk((*it));
            it++;

            for (; it != scalar_values.end();
                    it++)
            {
                intrin_name << ", ";
                walk((*it));
            }

            intrin_name << ")"; 
            */
        }        


        void SSEVectorLowering::visit(const Nodecl::VectorConditionalExpression& node) 
        { 
            /*
            TL::Type type = node.get_type().basic_type();

            Nodecl::NodeclBase true_node = node.get_true();
            Nodecl::NodeclBase false_node = node.get_false();
            Nodecl::NodeclBase condition_node = node.get_condition();

            TL::Type true_type = true_node.get_type().basic_type();
            TL::Type false_type = false_node.get_type().basic_type();
            TL::Type condiition_type = condition_node.get_type();

            std::string casting;

            // Intrinsic name
            intrin_name << "_mm_blend";

            // Postfix
            if (true_type.is_integral_type()
                    && false_type.is_integral_type())
            {
                // TODO _epi16
                intrin_name << "v_epi8";
            }
            else if (true_type.is_float()
                    && false_type.is_float())
            {
                // TODO _ps
                intrin_name << "v_ps";

                casting = "(";
                casting += print_type_str(TL::Type::get_float_type().get_vector_to(16).get_internal_type(),
                        node.retrieve_context().get_decl_context());
                casting += ")";
            }
            else if (true_type.is_double()
                    && false_type.is_double())
            {
                // TODO _pd
                intrin_name << "v_pd";
                casting = "(";
                casting += print_type_str(TL::Type::get_double_type().get_vector_to(16).get_internal_type(),
                        node.retrieve_context().get_decl_context());
                casting += ")";
            }
            else
            {
                running_error("SSE Lowering: Node %s at %s has an unsupported type.", 
                        ast_print_node_type(node.get_kind()),
                        node.get_locus().c_str());
            }

            intrin_name << "("; 
            walk(false_node); // False first!
            intrin_name << ", ";
            walk(true_node);
            intrin_name << ", "
                << casting;
            walk(condition_node);
            intrin_name << ")"; 
            */
        }        

        void SSEVectorLowering::visit(const Nodecl::VectorAssignment& node) 
        {
            walk(node.get_lhs());
            walk(node.get_rhs());

            Nodecl::Assignment assig =
                Nodecl::Assignment::make(
                        node.get_lhs(),
                        node.get_rhs(),
                        node.get_rhs().get_type(),
                        node.get_filename(),
                        node.get_line());

            node.replace(assig);
        }                                                 

        void SSEVectorLowering::visit(const Nodecl::VectorLoad& node) 
        { 
            TL::Type type = node.get_type().basic_type();

            std::stringstream intrin_name;

            // Intrinsic name
            intrin_name << "_mm_load";

            // Postfix
            if (type.is_float()) 
            { 
                intrin_name << "_ps"; 
            } 
            else if (type.is_double()) 
            { 
                intrin_name << "_pd"; 
            } 
            else if (type.is_integral_type()) 
            { 
                intrin_name << "_si128"; 
            } 
            else
            {
                running_error("SSE Lowering: Node %s at %s has an unsupported type.", 
                        ast_print_node_type(node.get_kind()),
                        node.get_locus().c_str());
            }

            walk(node.get_rhs());

            Nodecl::NodeclBase new_rhs;

            if (type.is_integral_type())
            {
                new_rhs = Nodecl::Cast::make(
                        node.get_rhs(),
                        TL::Type::get_long_long_int_type().get_vector_to(16).get_pointer_to().get_internal_type(),
                        NULL,
                        node.get_filename(),
                        node.get_line());
            }
            else
            {
                new_rhs = node.get_rhs();
            }


            Nodecl::Symbol intrin_sym = node.retrieve_context().get_symbol_from_name(
                    intrin_name.str()).make_nodecl(node.get_filename(), node.get_line());

            Nodecl::FunctionCall intrin_node =
                Nodecl::FunctionCall::make(
                        intrin_sym,
                        Nodecl::List::make( /*Arguments*/
                            new_rhs),
                        nodecl_null(),
                        nodecl_null(),
                        intrin_sym.get_symbol().get_type(),
                        node.get_filename(),
                        node.get_line());

            node.replace(intrin_node);
        }

        void SSEVectorLowering::visit(const Nodecl::VectorStore& node) 
        { 
            TL::Type type = node.get_lhs().get_type().basic_type();

            std::stringstream intrin_name;

            // Intrinsic name
            intrin_name << "_mm_store";

            // Postfix
            if (type.is_float()) 
            { 
                intrin_name << "_ps"; 
            } 
            else if (type.is_double()) 
            { 
                intrin_name << "_pd"; 
            } 
            else if (type.is_integral_type()) 
            { 
                intrin_name << "_si128";
            } 
            else
            {
                running_error("SSE Lowering: Node %s at %s has an unsupported type.", 
                        ast_print_node_type(node.get_kind()),
                        node.get_locus().c_str());
            }

            
            walk(node.get_lhs());
            walk(node.get_rhs());

            Nodecl::NodeclBase new_lhs;

            if (type.is_integral_type())
            {
                new_lhs = Nodecl::Cast::make(
                        node.get_lhs(),
                        TL::Type::get_long_long_int_type().get_vector_to(16).get_pointer_to().get_internal_type(),
                        NULL,
                        node.get_filename(),
                        node.get_line());
            }
            else
            {
                new_lhs = node.get_lhs();
            }


            Nodecl::Symbol intrin_sym = node.retrieve_context().get_symbol_from_name(
                    intrin_name.str()).make_nodecl(node.get_filename(), node.get_line());

            Nodecl::FunctionCall intrin_node =
                Nodecl::FunctionCall::make(
                        intrin_sym,
                        Nodecl::List::make( /*Arguments*/
                            new_lhs,
                            node.get_rhs()),
                        nodecl_null(),
                        nodecl_null(),
                        intrin_sym.get_symbol().get_type(),
                        node.get_filename(),
                        node.get_line());

            node.replace(intrin_node);
        }

        void SSEVectorLowering::visit(const Nodecl::VectorGather& node) 
        { 
/*
            TL::Type type = node.get_type().basic_type();
            TL::Type index_type = node.get_strides().get_type().basic_type();

            // Intrinsic name
            intrin_name << "_mm_set";

            std::string extract;

            // Postfix
            if (type.is_float()) 
            { 
                intrin_name << "_ps";
            } 
            else if (type.is_signed_int() || type.is_unsigned_int()) 
            { 
                intrin_name << "_epi32";
            }
            else if (type.is_signed_short_int() || type.is_unsigned_short_int()) 
            { 
                intrin_name << "_epi16";
            }
            else if (type.is_signed_char() || type.is_char() || type.is_unsigned_char())
            {
                intrin_name << "_epi8";
            }
            else
            {
                running_error("SSE Lowering: Node %s at %s has an unsupported type.", 
                        ast_print_node_type(node.get_kind()),
                        node.get_locus().c_str());
            }

            // Indexes
            if (index_type.is_signed_int() || index_type.is_unsigned_int()) 
            { 
                extract = "_mm_extract_epi32";
            }
            else if (index_type.is_signed_short_int() || index_type.is_unsigned_short_int()) 
            { 
                extract = "_mm_extract_epi16";
            }
            else if (index_type.is_signed_char() || index_type.is_char() || index_type.is_unsigned_char())
            {
                extract = "_mm_extract_epi8";
            }
            else
            {
                running_error("SSE Lowering: Node %s at %s has an unsupported type.", 
                        ast_print_node_type(node.get_kind()),
                        node.get_locus().c_str());
            }


            intrin_name << "("; 

            unsigned int i = 0;

            walk(node.get_base());
            intrin_name << "[";

            intrin_name << extract;
            intrin_name << "(";
            walk(node.get_strides());
            intrin_name << ", " << i << ")";

            intrin_name << "]";

            i++;

            for (; i < type.get_size(); i++)
            {
                intrin_name << ", ";

                walk(node.get_base());
                intrin_name << "[";

                intrin_name << extract;
                intrin_name << "(";
                walk(node.get_strides());
                intrin_name << ", " << i << ")";

                intrin_name << "]";
            }

            intrin_name << ")"; 
            */
        }

        void SSEVectorLowering::visit(const Nodecl::VectorScatter& node) 
        { 
            /*
            TL::Type type = node.get_source().get_type().basic_type();
            TL::Type index_type = node.get_strides().get_type().basic_type();

            std::string extract_index;
            std::string extract_source;

            // Indexes
            if (index_type.is_signed_int() || index_type.is_unsigned_int()) 
            { 
                extract_index = "_mm_extract_epi32";
            }
            else if (index_type.is_signed_short_int() || index_type.is_unsigned_short_int()) 
            { 
                extract_index = "_mm_extract_epi16";
            }
            else if (index_type.is_signed_char() || index_type.is_char() || index_type.is_unsigned_char())
            {
                extract_index = "_mm_extract_epi8";
            }
            else
            {
                running_error("SSE Lowering: Node %s at %s has an unsupported index type.", 
                        ast_print_node_type(node.get_kind()),
                        node.get_locus().c_str());
            }

            // Sourcei
            if (type.is_float())
            {
                extract_source = "_mm_extract_ps";
            }
            else if (type.is_signed_int() || type.is_unsigned_int()) 
            { 
                extract_source = "_mm_extract_epi32";
            }
            else if (type.is_signed_short_int() || type.is_unsigned_short_int()) 
            { 
                extract_source = "_mm_extract_epi16";
            }
            else if (type.is_signed_char() || type.is_char() || type.is_unsigned_char())
            {
                extract_source = "_mm_extract_epi8";
            }
            else
            {
                running_error("SSE Lowering: Node %s at %s has an unsupported source type.", 
                        ast_print_node_type(node.get_kind()),
                        node.get_locus().c_str());
            }


            for (unsigned int i=0; i < type.get_size(); i++)
            {
                walk(node.get_base());
                intrin_name << "[";

                intrin_name << extract_index;
                intrin_name << "(";
                walk(node.get_strides());
                intrin_name << ", ";
                intrin_name << i;
                intrin_name << ")";

                intrin_name << "]";

                intrin_name << " = " << extract_source << "(";
                walk(node.get_source());
                intrin_name << ", ";
                intrin_name << i;
                intrin_name << ")";

                intrin_name << ";" << std::endl;
            }
        */
        }

        void SSEVectorLowering::visit(const Nodecl::VectorFunctionCall& node) 
        {
          //  ((CxxBase*)_modular_visitor)->visit_function_call(
          //      node.as<Nodecl::FunctionCall>(), false);
        }

        void SSEVectorLowering::visit(const Nodecl::VectorFabs& node) 
        {
            /*
            TL::Type type = node.get_type().basic_type();

            // Handcoded implementations for float and double
            if (type.is_float()) 
            { 
                intrin_name << "(_mm_and_ps(";
                walk(node.get_arguments());
                intrin_name << ", _mm_castsi128_ps(_mm_set1_epi32(0x7FFFFFFF))))"; 
            } 
            else if (type.is_double()) 
            { 
                intrin_name << "(_mm_and_pd(";
                walk(node.get_arguments());
                intrin_name << ", _mm_castsi128_pd(_mm_set1_epi64(0x7FFFFFFFFFFFFFFFLL))))"; 
            }
            else
            {
                // Intrinsic name
          
                intrin_name << "_mm_abs";

                // Postfix
                if (type.is_signed_int() ||
                        type.is_unsigned_int()) 
                { 
                    intrin_name << "_epi32"; 
                } 
                else if (type.is_signed_short_int() ||
                        type.is_unsigned_short_int()) 
                { 
                    intrin_name << "_epi16"; 
                } 
                else if (type.is_char() || 
                        type.is_signed_char() ||
                        type.is_unsigned_char()) 
                { 
                    intrin_name << "_epi8"; 
                } 
                else
                {
                    running_error("SSE Lowering: Node %s at %s has an unsupported type.", 
                            ast_print_node_type(node.get_kind()),
                            node.get_locus().c_str());
                }
            }
            */
        }

        Nodecl::NodeclVisitor<void>::Ret SSEVectorLowering::unhandled_node(const Nodecl::NodeclBase& n) 
        { 
            fprintf(stderr, "SSE Lowering: Unknown node %s at %s.\n",
                    ast_print_node_type(n.get_kind()),
                    n.get_locus().c_str()); 
            /*
               running_error("SSE Lowering: Unknown node %s at %s.",
               ast_print_node_type(n.get_kind()),
               n.get_locus().c_str()); 
             */
            return Ret(); 
        }
    }
}
