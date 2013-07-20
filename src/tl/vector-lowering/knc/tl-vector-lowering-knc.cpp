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

#include "tl-vector-lowering-knc.hpp"
#include "tl-source.hpp"

namespace TL 
{
    namespace Vectorization
    {
        KNCVectorLowering::KNCVectorLowering() 
            : _vectorizer(TL::Vectorization::Vectorizer::get_vectorizer()), _vector_length(64) 
        {
        }

        void KNCVectorLowering::visit(const Nodecl::ObjectInit& node) 
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

        void KNCVectorLowering::visit(const Nodecl::VectorAdd& node) 
        { 
            TL::Type type = node.get_type().basic_type();

            TL::Source intrin_src;

            intrin_src << "_mm512_add";

            // Postfix
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
                running_error("KNC Lowering: Node %s at %s has an unsupported type.", 
                        ast_print_node_type(node.get_kind()),
                        locus_to_str(node.get_locus()));
            }      

            walk(node.get_lhs());
            walk(node.get_rhs());

            intrin_src << "(";
            intrin_src << as_expression(node.get_lhs());
            intrin_src << ", ";
            intrin_src << as_expression(node.get_rhs());
            intrin_src << ")";

            Nodecl::NodeclBase function_call = 
                    intrin_src.parse_expression(node.retrieve_context());

            node.replace(function_call);
        }                                                 

        void KNCVectorLowering::visit(const Nodecl::VectorAddMask& node) 
        { 
            TL::Type type = node.get_type().basic_type();
            TL::Source intrin_src, undef;

            intrin_src << "_mm512_mask_add";

            // Postfix
            if (type.is_float()) 
            { 
                intrin_src << "_ps("; 
                undef << "_mm512_undefined()";
            } 
            else if (type.is_double()) 
            { 
                intrin_src << "_pd("; 
                undef << "_mm512_undefined_pd()";
            } 
            else if (type.is_signed_int() ||
                    type.is_unsigned_int()) 
            { 
                intrin_src << "_epi32("; 
                undef << "_mm512_undefined_epi32()";
            } 
            else
            {
                running_error("KNC Lowering: Node %s at %s has an unsupported type.", 
                        ast_print_node_type(node.get_kind()),
                        locus_to_str(node.get_locus()));
            }      
            
            if (_old_m512.empty())
            {
                intrin_src << undef;
            }
            else
            {
                intrin_src << as_expression(_old_m512.back());
                _old_m512.pop_back();
            }

            walk(node.get_lhs());
            walk(node.get_rhs());
            walk(node.get_mask());

            intrin_src << ", "
                << as_expression(node.get_mask())
                << ", "
                << as_expression(node.get_lhs())
                << ", "
                << as_expression(node.get_rhs())
                << ")"
                ;

            Nodecl::NodeclBase function_call = 
                    intrin_src.parse_expression(node.retrieve_context());

            node.replace(function_call);
        }                                                 


        void KNCVectorLowering::visit(const Nodecl::VectorMinus& node) 
        { 
            TL::Type type = node.get_type().basic_type();
            TL::Source intrin_src;

            intrin_src << "_mm512_sub";

            // Postfix
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
                running_error("KNC Lowering: Node %s at %s has an unsupported type.", 
                        ast_print_node_type(node.get_kind()),
                        locus_to_str(node.get_locus()));
            }      

            walk(node.get_lhs());
            walk(node.get_rhs());

            intrin_src << "(";
            intrin_src << as_expression(node.get_lhs());
            intrin_src << ", ";
            intrin_src << as_expression(node.get_rhs());
            intrin_src << ")";

            Nodecl::NodeclBase function_call = 
                    intrin_src.parse_expression(node.retrieve_context());

            node.replace(function_call);
        }                                                 

        void KNCVectorLowering::visit(const Nodecl::VectorMinusMask& node) 
        { 
            TL::Type type = node.get_type().basic_type();
            TL::Source intrin_src, undef;

            intrin_src << "_mm512_mask_sub";

            // Postfix
            if (type.is_float()) 
            { 
                intrin_src << "_ps(";
                undef << "_mm512_undefined()";
            } 
            else if (type.is_double()) 
            { 
                intrin_src << "_pd("; 
                undef << "_mm512_undefined_pd()";
            } 
            else if (type.is_signed_int() ||
                    type.is_unsigned_int()) 
            { 
                intrin_src << "_epi32("; 
                undef << "_mm512_undefined_epi32()";
            } 
            else
            {
                running_error("KNC Lowering: Node %s at %s has an unsupported type.", 
                        ast_print_node_type(node.get_kind()),
                        locus_to_str(node.get_locus()));
            }      

            if (_old_m512.empty())
            {
                intrin_src << undef;
            }
            else
            {
                intrin_src << as_expression(_old_m512.back());
                _old_m512.pop_back();
            }

            walk(node.get_lhs());
            walk(node.get_rhs());
            walk(node.get_mask());

            intrin_src << ", "
                << as_expression(node.get_mask())
                << ", "
                << as_expression(node.get_lhs())
                << ", "
                << as_expression(node.get_rhs())
                << ")"
                ;

            Nodecl::NodeclBase function_call = 
                    intrin_src.parse_expression(node.retrieve_context());

            node.replace(function_call);
        }                                                 

        void KNCVectorLowering::visit(const Nodecl::VectorMul& node) 
        {
            TL::Type result_type = node.get_type().basic_type();
            TL::Type first_op_type = node.get_rhs().get_type().basic_type();
            TL::Type second_op_type = node.get_lhs().get_type().basic_type();

            TL::Source intrin_src, intrin_name, args;

            intrin_src << intrin_name
                << "("
                << args
                << ")"
                ;

            intrin_name << "_mm512_mul";

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
            /* 
               else if (type.is_signed_short_int() ||
               type.is_unsigned_short_int()) 
               { 
               intrin_src << "_epi16"; 
               } 
               else if (type.is_char() || 
               type.is_signed_char() ||
               type.is_unsigned_char()) 
               { 
               intrin_src << "_epi8"; 
               }
             */ 
            else
            {
                running_error("KNC Lowering: Node %s at %s has an unsupported type.", 
                        ast_print_node_type(node.get_kind()),
                        locus_to_str(node.get_locus()));
            }      

            walk(node.get_rhs());
            walk(node.get_lhs());
            args << as_expression(node.get_lhs())
                << ", "
                << as_expression(node.get_rhs());

            Nodecl::NodeclBase function_call = 
                    intrin_src.parse_expression(node.retrieve_context());

            node.replace(function_call);
        }    

        void KNCVectorLowering::visit(const Nodecl::VectorMulMask& node) 
        {
            TL::Type result_type = node.get_type().basic_type();
            TL::Type first_op_type = node.get_rhs().get_type().basic_type();
            TL::Type second_op_type = node.get_lhs().get_type().basic_type();

            TL::Source intrin_src, undef;

            intrin_src << "_mm512_mask_mul";

            // Postfix
            if (result_type.is_float() &&
                    first_op_type.is_float() &&
                    second_op_type.is_float())
            {
                intrin_src << "_ps("; 
                undef << "_mm512_undefined()";
            } 
            else if (result_type.is_double() &&
                    first_op_type.is_double() &&
                    second_op_type.is_double())
            { 
                intrin_src << "_pd("; 
                undef << "_mm512_undefined_pd()";
            }
            else if (result_type.is_signed_int() &&
                    first_op_type.is_signed_int() &&
                    second_op_type.is_signed_int())
            {
                intrin_src << "lo_epi32("; 
                undef << "_mm512_undefined_epi32()";
            } 
            /* 
               else if (type.is_signed_short_int() ||
               type.is_unsigned_short_int()) 
               { 
               intrin_src << "_epi16"; 
               } 
               else if (type.is_char() || 
               type.is_signed_char() ||
               type.is_unsigned_char()) 
               { 
               intrin_src << "_epi8"; 
               }
             */ 
            else
            {
                running_error("KNC Lowering: Node %s at %s has an unsupported type.", 
                        ast_print_node_type(node.get_kind()),
                        locus_to_str(node.get_locus()));
            }      

            if (_old_m512.empty())
            {
                intrin_src << undef;
            }
            else
            {
                intrin_src << as_expression(_old_m512.back());
                _old_m512.pop_back();
            }

            walk(node.get_rhs());
            walk(node.get_lhs());
            walk(node.get_mask());

            intrin_src << ", "
                << as_expression(node.get_mask())
                << ", "
                << as_expression(node.get_lhs())
                << ", "
                << as_expression(node.get_rhs())
                << ")"
                ;

            Nodecl::NodeclBase function_call = 
                    intrin_src.parse_expression(node.retrieve_context());

            node.replace(function_call);
        }    

        void KNCVectorLowering::visit(const Nodecl::VectorDiv& node) 
        { 
            TL::Type type = node.get_type().basic_type();
            TL::Source intrin_src;

            intrin_src << "_mm512_div";

            // Postfix
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
                running_error("KNC Lowering: Node %s at %s has an unsupported type.", 
                        ast_print_node_type(node.get_kind()),
                        locus_to_str(node.get_locus()));
            }      

            walk(node.get_lhs());
            walk(node.get_rhs());

            intrin_src << "(";
            intrin_src << as_expression(node.get_lhs());
            intrin_src << ", ";
            intrin_src << as_expression(node.get_rhs());
            intrin_src << ")";

            Nodecl::NodeclBase function_call = 
                    intrin_src.parse_expression(node.retrieve_context());

            node.replace(function_call);
        }                                                 

        void KNCVectorLowering::visit(const Nodecl::VectorDivMask& node) 
        { 
            TL::Type type = node.get_type().basic_type();
            TL::Source intrin_src, undef;

            intrin_src << "_mm512_mask_div";

            // Postfix
            if (type.is_float()) 
            { 
                intrin_src << "_ps("; 
                undef << "_mm512_undefined()";
            } 
            else if (type.is_double()) 
            { 
                intrin_src << "_pd("; 
                undef << "_mm512_undefined_pd()";
            } 
            else if (type.is_signed_int() ||
                    type.is_unsigned_int()) 
            { 
                intrin_src << "_epi32("; 
                undef << "_mm512_undefined_epi32()";
            } 
            else
            {
                running_error("KNC Lowering: Node %s at %s has an unsupported type.", 
                        ast_print_node_type(node.get_kind()),
                        locus_to_str(node.get_locus()));
            }      

            if (_old_m512.empty())
            {
                intrin_src << undef;
            }
            else
            {
                intrin_src << as_expression(_old_m512.back());
                _old_m512.pop_back();
            }

            walk(node.get_lhs());
            walk(node.get_rhs());
            walk(node.get_mask());

            intrin_src << ", "
                << as_expression(node.get_mask())
                << ", "
                << as_expression(node.get_lhs())
                << ", "
                << as_expression(node.get_rhs())
                << ")"
                ;

            Nodecl::NodeclBase function_call = 
                    intrin_src.parse_expression(node.retrieve_context());

            node.replace(function_call);
        }    

        void KNCVectorLowering::visit(const Nodecl::VectorLowerThan& node) 
        { 
            TL::Type type = node.get_lhs().get_type().basic_type();

            TL::Source intrin_src;
            TL::Source cmp_flavor;

            // Intrinsic name
            intrin_src << "_mm512_cmp";

            // Postfix
            if (type.is_float()) 
            { 
                intrin_src << "_ps"; 
                cmp_flavor << _CMP_LT_OS;
            } 
            else if (type.is_signed_int() ||
                    type.is_unsigned_int()) 
            { 
                intrin_src << "_epi32"; 
                cmp_flavor << "_MM_CMPINT_LT";
            } 
            else
            {
                running_error("KNC Lowering: Node %s at %s has an unsupported type.", 
                        ast_print_node_type(node.get_kind()),
                        locus_to_str(node.get_locus()));
            }      

            walk(node.get_lhs());
            walk(node.get_rhs());

            intrin_src << "_mask(";
            intrin_src << as_expression(node.get_rhs());
            intrin_src << ", ";
            intrin_src << as_expression(node.get_lhs());
            intrin_src << ", ";
            intrin_src << cmp_flavor;
            intrin_src << ")";

            Nodecl::NodeclBase function_call = 
                    intrin_src.parse_expression(node.retrieve_context());

            node.replace(function_call);
        }                                                 

        void KNCVectorLowering::visit(const Nodecl::VectorGreaterThan& node) 
        { 
            TL::Type type = node.get_lhs().get_type().basic_type();

            TL::Source intrin_src;
            TL::Source cmp_flavor;

            // Intrinsic name
            intrin_src << "_mm512_cmp";
            cmp_flavor << "_MM_CMPINT_LT";

            // Postfix
            if (type.is_float()) 
            { 
                intrin_src << "_ps"; 
                cmp_flavor << _CMP_LT_OS;
            } 
            else if (type.is_signed_int() ||
                    type.is_unsigned_int()) 
            { 
                intrin_src << "_epi32"; 
                cmp_flavor << "_MM_CMPINT_LT";
            } 
            else
            {
                running_error("KNC Lowering: Node %s at %s has an unsupported type.", 
                        ast_print_node_type(node.get_kind()),
                        locus_to_str(node.get_locus()));
            }     
            
            walk(node.get_lhs());
            walk(node.get_rhs());

            intrin_src << "_mask(";
            intrin_src << as_expression(node.get_rhs());
            intrin_src << ", ";
            intrin_src << as_expression(node.get_lhs());
            intrin_src << ", ";
            intrin_src << cmp_flavor;
            intrin_src << ")";

            Nodecl::NodeclBase function_call = 
                    intrin_src.parse_expression(node.retrieve_context());

            node.replace(function_call);
        }                                                 

        void KNCVectorLowering::visit(const Nodecl::VectorEqual& node) 
        { 
            TL::Type type = node.get_lhs().get_type().basic_type();

            TL::Source intrin_src;
            TL::Source cmp_flavor;

            // Intrinsic name
            intrin_src << "_mm512_cmp";

            // Postfix
            if (type.is_float()) 
            { 
                intrin_src << "_ps"; 
                cmp_flavor << _CMP_EQ_OQ;
            } 
            else if (type.is_signed_int() ||
                    type.is_unsigned_int()) 
            { 
                intrin_src << "_epi32"; 
                cmp_flavor << "_MM_CMPINT_EQ";
            } 
            else
            {
                running_error("KNC Lowering: Node %s at %s has an unsupported type.", 
                        ast_print_node_type(node.get_kind()),
                        locus_to_str(node.get_locus()));
            }      

            walk(node.get_lhs());
            walk(node.get_rhs());

            intrin_src << "_mask("
                << as_expression(node.get_lhs())
                << ", "
                << as_expression(node.get_rhs())
                << ", "
                << cmp_flavor
                << ")"
                ;

            Nodecl::NodeclBase function_call =
                intrin_src.parse_expression(node.retrieve_context());

            node.replace(function_call);
        }                                                 

        void KNCVectorLowering::visit(const Nodecl::VectorBitwiseAnd& node) 
        { 
            TL::Type type = node.get_type().basic_type();

            TL::Source intrin_src;
            TL::Source casting_src;

            // Postfix
            if (type.is_float()) 
            { 
                casting_src << "_mm512_castps_si512i(";
            } 
            else if (type.is_integral_type())
            { 
                casting_src << "(";
            } 
            else
            {
                running_error("KNC Lowering: Node %s at %s has an unsupported type.", 
                        ast_print_node_type(node.get_kind()),
                        locus_to_str(node.get_locus()));
            }      

            walk(node.get_lhs());
            walk(node.get_rhs());
 
            // Intrinsic name
            intrin_src << casting_src << "_mm512_and_epi32";

           
            intrin_src << "(";
            intrin_src << casting_src << as_expression(node.get_lhs()) << ")";
            intrin_src << ", ";
            intrin_src << casting_src << as_expression(node.get_rhs()) << ")";
            intrin_src << "))";

            Nodecl::NodeclBase function_call =
                intrin_src.parse_expression(node.retrieve_context());

            node.replace(function_call);
        }                                                 

        void KNCVectorLowering::visit(const Nodecl::VectorBitwiseAndMask& node) 
        { 
            TL::Type type = node.get_type().basic_type();

            TL::Source intrin_src;
            TL::Source casting_src;

            // Postfix
            if (type.is_float()) 
            { 
                casting_src << "_mm512_castps_si512i(";
            } 
            else if (type.is_integral_type())
            { 
                casting_src << "(";
            } 
            else
            {
                running_error("KNC Lowering: Node %s at %s has an unsupported type.", 
                        ast_print_node_type(node.get_kind()),
                        locus_to_str(node.get_locus()));
            }      


            // Intrinsic name
            intrin_src << casting_src << "_mm512_mask_and_epi32(";

            if (_old_m512.empty())
            {
                intrin_src << "_mm512_undefined_epi32()";
            }
            else
            {
                intrin_src << as_expression(_old_m512.back());
                _old_m512.pop_back();
            }

            walk(node.get_lhs());
            walk(node.get_rhs());

            intrin_src << ", ";
            intrin_src << casting_src << as_expression(node.get_lhs()) << ")";
            intrin_src << ", ";
            intrin_src << casting_src << as_expression(node.get_rhs()) << ")";
            intrin_src << "))";

            Nodecl::NodeclBase function_call =
                intrin_src.parse_expression(node.retrieve_context());

            node.replace(function_call);
        }                                                 

       void KNCVectorLowering::visit(const Nodecl::VectorBitwiseOr& node) 
        { 
            TL::Type type = node.get_type().basic_type();

            TL::Source intrin_src;
            TL::Source casting_src;

            // Postfix
            if (type.is_float()) 
            { 
                casting_src << "_mm512_castps_si512(";
            } 
            else if (type.is_integral_type())
            { 
            } 
            else
            {
                running_error("KNC Lowering: Node %s at %s has an unsupported type.", 
                        ast_print_node_type(node.get_kind()),
                        locus_to_str(node.get_locus()));
            }      

            walk(node.get_lhs());
            walk(node.get_rhs());

            // Intrinsic name
            intrin_src << casting_src << "_mm512_or_epi32";

            intrin_src << "(";
            intrin_src << casting_src << as_expression(node.get_lhs()) << ")";
            intrin_src << ", ";
            intrin_src << casting_src << as_expression(node.get_rhs()) << ")";
            intrin_src << "))";

            Nodecl::NodeclBase function_call =
                intrin_src.parse_expression(node.retrieve_context());

            node.replace(function_call);
        }                                                 

        void KNCVectorLowering::visit(const Nodecl::VectorBitwiseOrMask& node) 
        { 
            TL::Type type = node.get_type().basic_type();

            TL::Source intrin_src;
            TL::Source casting_src;

            // Postfix
            if (type.is_float()) 
            { 
                casting_src << "_mm512_castps_si512(";
            } 
            else if (type.is_integral_type())
            { 
            } 
            else
            {
                running_error("KNC Lowering: Node %s at %s has an unsupported type.", 
                        ast_print_node_type(node.get_kind()),
                        locus_to_str(node.get_locus()));
            }      


            // Intrinsic name
            intrin_src << casting_src << "_mm512_mask_or_epi32(";

            if (_old_m512.empty())
            {
                intrin_src << "_mm512_undefined_epi32()";
            }
            else
            {
                intrin_src << as_expression(_old_m512.back());
                _old_m512.pop_back();
            }

            walk(node.get_lhs());
            walk(node.get_rhs());

            intrin_src << ", ";
            intrin_src << casting_src << as_expression(node.get_lhs()) << ")";
            intrin_src << ", ";
            intrin_src << casting_src << as_expression(node.get_rhs()) << ")";
            intrin_src << "))";

            Nodecl::NodeclBase function_call =
                intrin_src.parse_expression(node.retrieve_context());

            node.replace(function_call);
        }                                                 

 
        void KNCVectorLowering::visit(const Nodecl::VectorBitwiseXor& node) 
        { 
            TL::Type type = node.get_type().basic_type();

            TL::Source intrin_src;
            TL::Source casting_src;

            // Postfix
            if (type.is_float()) 
            { 
                casting_src << "_mm512_castps_si512(";
            } 
            else if (type.is_integral_type())
            { 
                casting_src << "(";
            }
            else
            {
                running_error("KNC Lowering: Node %s at %s has an unsupported type.", 
                        ast_print_node_type(node.get_kind()),
                        locus_to_str(node.get_locus()));
            }      

            walk(node.get_lhs());
            walk(node.get_rhs());

            // Intrinsic name
            intrin_src << casting_src << "_mm512_xor_epi32";

            intrin_src << ", ";
            intrin_src << "(";
            intrin_src << casting_src << as_expression(node.get_lhs()) << ")";
            intrin_src << ", ";
            intrin_src << casting_src << as_expression(node.get_rhs()) << ")";
            intrin_src << "))";

            Nodecl::NodeclBase function_call =
                intrin_src.parse_expression(node.retrieve_context());

            node.replace(function_call);
        }   


        void KNCVectorLowering::visit(const Nodecl::VectorBitwiseXorMask& node) 
        { 
            TL::Type type = node.get_type().basic_type();

            TL::Source intrin_src;
            TL::Source casting_src;

            // Postfix
            if (type.is_float()) 
            { 
                casting_src << "_mm512_castps_si512(";
            } 
            else if (type.is_integral_type())
            { 
                casting_src << "(";
            }
            else
            {
                running_error("KNC Lowering: Node %s at %s has an unsupported type.", 
                        ast_print_node_type(node.get_kind()),
                        locus_to_str(node.get_locus()));
            }      


            // Intrinsic name
            intrin_src << casting_src << "_mm512_xor_epi32(";

            if (_old_m512.empty())
            {
                intrin_src << "_mm512_undefined_epi32()";
            }
            else
            {
                intrin_src << as_expression(_old_m512.back());
                _old_m512.pop_back();
            }

            walk(node.get_lhs());
            walk(node.get_rhs());


            intrin_src << ", ";
            intrin_src << casting_src << as_expression(node.get_lhs()) << ")";
            intrin_src << ", ";
            intrin_src << casting_src << as_expression(node.get_rhs()) << ")";
            intrin_src << "))";

            Nodecl::NodeclBase function_call =
                intrin_src.parse_expression(node.retrieve_context());

            node.replace(function_call);
        }   


        void KNCVectorLowering::visit(const Nodecl::VectorLogicalOr& node) 
        { 
            running_error("KNC Lowering %s: 'logical or' operation (i.e., operator '||') is not supported in KNC. Try using 'bitwise or' operations (i.e., operator '|') instead if possible.",
                    locus_to_str(node.get_locus()));
        }                                                 

        void KNCVectorLowering::visit(const Nodecl::VectorNeg& node) 
        {
            TL::Type type = node.get_type().basic_type();

            TL::Source intrin_src;

            if (type.is_float()) 
            { 
                intrin_src << "_mm512_castsi512_ps(_mm512_xor_epi32(_mm512_set1_epi32(0x80000000), _mm512_castps_si512( ";
            } 
            else if (type.is_double()) 
            { 
                intrin_src << "_mm512_castsi512_pd(_mm512_xor_epi32(_mm512_set1_epi64(0x8000000000000000LL), _mm512_castpd_si512(";
            }
            else if (type.is_signed_int() ||
                    type.is_unsigned_int())
            {
                intrin_src << "(_mm512_sub_epi32( _mm512_setzero_si512(),";
            }
            else if (type.is_signed_short_int() ||
                    type.is_unsigned_short_int())
            {
                intrin_src << "(_mm512_sub_epi16( _mm512_setzero_si512(),";
            }
            else if (type.is_char() ||
                    type.is_signed_char() ||
                    type.is_unsigned_char())
            {
                intrin_src << "(_mm512_sub_epi8( _mm512_setzero_si512(),";
            }
            else
            {
                running_error("KNC Lowering: Node %s at %s has an unsupported type.", 
                        ast_print_node_type(node.get_kind()),
                        locus_to_str(node.get_locus()));
            } 

            walk(node.get_rhs());

            intrin_src << as_expression(node.get_rhs());
            intrin_src << ")))";

            Nodecl::NodeclBase function_call =
                intrin_src.parse_expression(node.retrieve_context());

            node.replace(function_call);
        }                                                 

        void KNCVectorLowering::visit(const Nodecl::VectorConversion& node) 
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
            else if ((src_type.is_signed_int() && dst_type.is_unsigned_int()) ||
                    (dst_type.is_signed_int() && src_type.is_unsigned_int()) ||
                    (src_type.is_signed_short_int() && dst_type.is_unsigned_short_int()) ||
                    (dst_type.is_signed_short_int() && src_type.is_unsigned_short_int()))
            {
                node.replace(node.get_nest());
                return;
            }
            else if (src_type.is_signed_int() &&
                    dst_type.is_float()) 
            { 
                intrin_src << "_mm512_cvtepi32_ps("; 
                intrin_src << as_expression(node.get_nest());
                intrin_src << ")"; 
            } 
            else if (src_type.is_float() &&
                    dst_type.is_signed_int()) 
            { 
                // C/C++ requires truncated conversion
                intrin_src << "_mm512_cvttps_epi32("; 
                intrin_src << as_expression(node.get_nest());
                intrin_src << ")"; 
            }
            /*
            else if (src_type.is_float() &&
                    dst_type.is_double()) 
            { 
                intrin_src << "ps_pd"; 
            } 
            else if (src_type.is_double() &&
                    dst_type.is_float()) 
            { 
                intrin_src << "pd_ps"; 
            } 
            else if (src_type.is_float() &&
                    (dst_type.is_signed_char() ||
                     dst_type.is_char())) 
            {
                // Saturated conversion
                intrin_src << "_mm512_packs_epi16("; 
                intrin_src << "_mm512_packs_epi32("; 
                intrin_src << "_mm512_cvttps_epi32("; 
                walk(node.get_nest());
                intrin_src << "),"; 
                intrin_src << "_mm512_castps_si512(";
                walk(node.get_nest());
                intrin_src << ")";
                //intrin_src << "_mm512_undefined_si512()"; 
                intrin_src << "),"; 
                intrin_src << "_mm512_castps_si512(";
                walk(node.get_nest());
                intrin_src << ")";
                //intrin_src << "_mm512_undefined_si512()"; 
                intrin_src << ")"; 
            } 
            else if (src_type.is_float() &&
                    dst_type.is_unsigned_char()) 
            {
                // Saturated conversion
                intrin_src << "_mm512_packus_epi16("; 
                intrin_src << "_mm512_packus_epi32("; 
                intrin_src << "_mm512_cvttps_epi32("; 
                walk(node.get_nest());
                intrin_src << "),"; 
                intrin_src << "_mm512_castps_si512(";
                walk(node.get_nest());
                intrin_src << ")";
                //intrin_src << "_mm512_undefined_si512()"; 
                intrin_src << "),"; 
                intrin_src << "_mm512_castps_si512(";
                walk(node.get_nest());
                //intrin_src << "_mm512_undefined_si512()"; 
                intrin_src << "))"; 
            } 
            else if (src_type.is_signed_int() &&
                    (dst_type.is_signed_char() ||
                     dst_type.is_char())) 
            {
                // Saturated conversion
                intrin_src << "_mm512_packs_epi16("; 
                intrin_src << "_mm512_packs_epi32("; 
                walk(node.get_nest());
                intrin_src << ","; 
                walk(node.get_nest());
                //intrin_src << "_mm512_undefined_si512()"; 
                intrin_src << "),"; 
                walk(node.get_nest());
                //intrin_src << "_mm512_undefined_si512()"; 
                intrin_src << ")"; 
            } 
            else if (src_type.is_signed_int() &&
                    dst_type.is_unsigned_char()) 
            {
                // Saturated conversion
                intrin_src << "_mm512_packus_epi16("; 
                intrin_src << "_mm512_packus_epi32("; 
                walk(node.get_nest());
                intrin_src << ","; 
                walk(node.get_nest());
                //intrin_src << "_mm512_undefined_si512()"; 
                intrin_src << ")"; 
                intrin_src << ",";
                walk(node.get_nest());
                //intrin_src << "_mm512_undefined_si512()"; 
                intrin_src << ")"; 
            }
            */
            else
            {
                fprintf(stderr, "KNC Lowering: Conversion at '%s' is not supported yet: %s\n", 
                        locus_to_str(node.get_locus()),
                        node.get_nest().prettyprint().c_str());
            }   

            Nodecl::NodeclBase function_call =
                intrin_src.parse_expression(node.retrieve_context());

            node.replace(function_call);
        }

        void KNCVectorLowering::visit(const Nodecl::VectorPromotion& node) 
        { 
            TL::Type type = node.get_type().basic_type();

            TL::Source intrin_src;

            // Intrinsic name
            intrin_src << "_mm512_set1";

            // Postfix
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
            else if (type.is_signed_short_int() ||
                    type.is_unsigned_short_int()) 
            { 
                intrin_src << "_epi16"; 
            } 
            else if (type.is_char() || 
                    type.is_signed_char() ||
                    type.is_unsigned_char()) 
            { 
                intrin_src << "_epi8"; 
            } 
            else
            {
                running_error("KNC Lowering: Node %s at %s has an unsupported type.", 
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

        void KNCVectorLowering::visit(const Nodecl::VectorLiteral& node) 
        {
            TL::Type type = node.get_type().basic_type();

            TL::Source intrin_src;

            // Intrinsic name
            intrin_src << "_mm512_set";

            // Postfix
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
            else if (type.is_signed_short_int() ||
                    type.is_unsigned_short_int()) 
            { 
                intrin_src << "_epi16"; 
            } 
            else if (type.is_char() || 
                    type.is_signed_char() ||
                    type.is_unsigned_char()) 
            { 
                intrin_src << "_epi8"; 
            } 
            else
            {
                running_error("KNC Lowering: Node %s at %s has an unsupported type.", 
                        ast_print_node_type(node.get_kind()),
                        locus_to_str(node.get_locus()));
            }      

            intrin_src << "(";

            Nodecl::List scalar_values =
                node.get_scalar_values().as<Nodecl::List>();

            Nodecl::List::const_iterator it = scalar_values.begin();
            walk((*it));
            intrin_src << as_expression(*it);
            it++;

            for (; it != scalar_values.end();
                    it++)
            {
                intrin_src << ", ";
                walk((*it));
                intrin_src << as_expression(*it);
            }

            intrin_src << ")"; 

            Nodecl::NodeclBase function_call =
                intrin_src.parse_expression(node.retrieve_context());

            node.replace(function_call);
        }        

        void KNCVectorLowering::visit(const Nodecl::VectorConditionalExpression& node) 
        { 
            TL::Type type = node.get_type().basic_type();

            TL::Source intrin_src, swizzle;

            Nodecl::NodeclBase true_node = node.get_true();
            Nodecl::NodeclBase false_node = node.get_false();
            Nodecl::NodeclBase condition_node = node.get_condition();

            TL::Type true_type = true_node.get_type().basic_type();
            TL::Type false_type = false_node.get_type().basic_type();
            TL::Type condiition_type = condition_node.get_type();

            // Postfix
            if (true_type.is_float()
                    && false_type.is_float())
            {
                intrin_src << "_mm512_mask_mov_ps";
            }
            else if (true_type.is_double()
                    && false_type.is_double())
            {
                intrin_src << "_mm512_mask_mov_pd";
            }
            else if (true_type.is_integral_type()
                    && false_type.is_integral_type())
            {
                intrin_src << "_mm512_mask_swizzle_epi32";
                swizzle << ", _MM_SWIZ_REG_NONE";
            }
            else
            {
                running_error("KNC Lowering: Node %s at %s has an unsupported type.", 
                        ast_print_node_type(node.get_kind()),
                        locus_to_str(node.get_locus()));
            }

            walk(false_node);
            walk(true_node);
            walk(condition_node);

            intrin_src << "(" 
                << as_expression(false_node) // False first!
                << ", "
                << as_expression(condition_node)
                << ", "
                << as_expression(true_node)
                << swizzle
                << ")"; 

            Nodecl::NodeclBase function_call =
                intrin_src.parse_expression(node.retrieve_context());

            node.replace(function_call);
        }        

        void KNCVectorLowering::visit(const Nodecl::VectorAssignment& node) 
        {
            TL::Source intrin_src;

            walk(node.get_lhs());
            walk(node.get_rhs());

            intrin_src << as_expression(node.get_lhs());
            intrin_src << " = ";
            intrin_src << as_expression(node.get_rhs());

            Nodecl::NodeclBase function_call =
                intrin_src.parse_expression(node.retrieve_context());

            node.replace(function_call);
        }                                                 

        void KNCVectorLowering::visit(const Nodecl::VectorAssignmentMask& node) 
        {
            TL::Source intrin_src;

            walk(node.get_lhs());
            _old_m512.push_back(node.get_lhs());
            walk(node.get_rhs());

            intrin_src << as_expression(node.get_lhs());
            intrin_src << " = ";
            intrin_src << as_expression(node.get_rhs());

            Nodecl::NodeclBase function_call =
                intrin_src.parse_expression(node.retrieve_context());

            node.replace(function_call);
        }                                                 

        void KNCVectorLowering::visit(const Nodecl::VectorLoad& node) 
        { 
            TL::Type type = node.get_type().basic_type();
            TL::Source intrin_src, intrin_name, args;

            intrin_src << intrin_name
                << "("
                << args
                << ")"
                ;

            intrin_name << "_mm512_load";

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
                intrin_name << "_epi32"; 
/*
                args << "(" 
                    << print_type_str(
                            TL::Type::get_void_type().get_pointer_to().get_internal_type(),
                            node.retrieve_context().get_decl_context())
                    << ")"
                    ; 
*/
            } 
            else
            {
                running_error("KNC Lowering: Node %s at %s has an unsupported type.", 
                        ast_print_node_type(node.get_kind()),
                        locus_to_str(node.get_locus()));
            }

            walk(node.get_rhs());
            
            args << as_expression(node.get_rhs());

            Nodecl::NodeclBase function_call =
                intrin_src.parse_expression(node.retrieve_context());
            
            node.replace(function_call);
        }

        void KNCVectorLowering::visit(const Nodecl::VectorLoadMask& node) 
        { 
            TL::Type type = node.get_type().basic_type();
            TL::Source intrin_src, undef, casting;

            intrin_src << "_mm512_mask_load";

            // Postfix
            if (type.is_float()) 
            { 
                intrin_src << "_ps("; 
                undef << "_mm512_undefined()";
            } 
            else if (type.is_double()) 
            { 
                intrin_src << "_pd(";
                undef << "_mm512_undefined_pd()";
            } 
            else if (type.is_integral_type()) 
            { 
                intrin_src << "_epi32(";
/*
                casting << "(" 
                    << print_type_str(
                        TL::Type::get_void_type().get_pointer_to().get_internal_type(),
                        node.retrieve_context().get_decl_context())
                    << ")"
                    ; 
*/
                undef << "_mm512_undefined_epi32()";
            } 
            else
            {
                running_error("KNC Lowering: Node %s at %s has an unsupported type.", 
                        ast_print_node_type(node.get_kind()),
                        locus_to_str(node.get_locus()));
            }

            if (_old_m512.empty())
            {
                intrin_src << undef;
            }
            else
            {
                intrin_src << as_expression(_old_m512.back());
                _old_m512.pop_back();
            }

            walk(node.get_rhs());
            walk(node.get_mask());

            intrin_src << ", "
                << as_expression(node.get_mask())
                << ", "
                << casting
                << as_expression(node.get_rhs())
                << ")";

            Nodecl::NodeclBase function_call =
                intrin_src.parse_expression(node.retrieve_context());

            node.replace(function_call);
        }

        void KNCVectorLowering::visit(const Nodecl::UnalignedVectorLoad& node) 
        { 
            TL::Type type = node.get_type().basic_type();
            TL::Source intrin_src, intrin_name_hi, intrin_name_lo, args_lo, args_hi, undef, ext;

            intrin_src << intrin_name_hi
                << "("
                << intrin_name_lo
                << "("
                << args_lo
                << ")"
                << ", "
                << args_hi
                << ")"
                ;

            intrin_name_hi << "_mm512_extloadunpackhi";
            intrin_name_lo << "_mm512_extloadunpacklo";

            if (type.is_float()) 
            { 
                intrin_name_hi << "_ps"; 
                intrin_name_lo << "_ps"; 
                undef << "_mm512_undefined()";
                ext << "_MM_UPCONV_PS_NONE";
            } 
            else if (type.is_double()) 
            { 
                intrin_name_hi << "_pd";
                intrin_name_lo << "_pd";
                ext << "_MM_UPCONV_PD_NONE";
                undef << "_mm512_undefined_pd()";
            } 
            else if (type.is_integral_type()) 
            { 
                intrin_name_hi << "_epi32";
                intrin_name_lo << "_epi32";
                ext << "_MM_UPCONV_EPI32_NONE";
                undef << "_mm512_undefined_epi32()";
/*
                casting << "(" 
                    << print_type_str(
                        TL::Type::get_void_type().get_pointer_to().get_internal_type(),
                        node.retrieve_context().get_decl_context())
                    << ")"
                    ; 
*/
            } 
            else
            {
                running_error("KNC Lowering: Node %s at %s has an unsupported type.", 
                        ast_print_node_type(node.get_kind()),
                        locus_to_str(node.get_locus()));
            }


            walk(node.get_rhs());

            args_lo << undef
                << ", "
                << as_expression(node.get_rhs())
                << ", "
                << ext
                << ", "
                << _MM_HINT_NONE
                ;

            args_hi << "("
                << as_expression(node.get_rhs())
                << ") + "
                << _vector_length
                << ", "
                << ext
                << ", "
                << _MM_HINT_NONE
                ;

            Nodecl::NodeclBase function_call =
                intrin_src.parse_expression(node.retrieve_context());
            
            node.replace(function_call);
        }

        void KNCVectorLowering::visit(const Nodecl::UnalignedVectorLoadMask& node) 
        { 
            TL::Type type = node.get_type().basic_type();
            TL::Source intrin_src, intrin_name_hi, intrin_name_lo, args_lo, args_hi, undef, ext;
            intrin_src << intrin_name_hi
                << "("
                << intrin_name_lo
                << "("
                << args_lo
                << ")"
                << ", "
                << args_hi
                << ")"
                ;

            intrin_name_hi << "_mm512_mask_extloadunpackhi";
            intrin_name_lo << "_mm512_mask_extloadunpacklo";

            if (type.is_float()) 
            { 
                intrin_name_hi << "_ps"; 
                intrin_name_lo << "_ps"; 
                undef << "_mm512_undefined()";
                ext << "_MM_UPCONV_PS_NONE";
            } 
            else if (type.is_double()) 
            { 
                intrin_name_hi << "_pd";
                intrin_name_lo << "_pd";
                undef << "_mm512_undefined_pd()";
                ext << "_MM_UPCONV_PD_NONE";
            } 
            else if (type.is_integral_type()) 
            { 
                intrin_name_hi << "_epi32";
                intrin_name_lo << "_epi32";
                ext << "_MM_UPCONV_EPI32_NONE";
/*
                casting << "(" 
                    << print_type_str(
                        TL::Type::get_void_type().get_pointer_to().get_internal_type(),
                        node.retrieve_context().get_decl_context())
                    << ")"
                    ; 
*/
                undef << "_mm512_undefined_epi32()";
            } 
            else
            {
                running_error("KNC Lowering: Node %s at %s has an unsupported type.", 
                        ast_print_node_type(node.get_kind()),
                        locus_to_str(node.get_locus()));
            }

            if (_old_m512.empty())
            {
                args_lo << undef;
            }
            else
            {
                args_lo << as_expression(_old_m512.back());
                _old_m512.pop_back();
            }

            walk(node.get_rhs());
            walk(node.get_mask());


            args_lo << ", "
                << as_expression(node.get_mask())
                << ", "
                << as_expression(node.get_rhs())
                << ", "
                << ext
                << ", "
                << _MM_HINT_NONE
 
                ;

            args_hi << "("
                << as_expression(node.get_mask())
                << ", "
                << as_expression(node.get_rhs())
                << ") + "
                << _vector_length
                << ", "
                << ext
                << ", "
                << _MM_HINT_NONE
                ;

            Nodecl::NodeclBase function_call =
                intrin_src.parse_expression(node.retrieve_context());
            
            node.replace(function_call);
        }

        void KNCVectorLowering::visit(const Nodecl::VectorStore& node) 
        { 
            TL::Type type = node.get_lhs().get_type().basic_type();
            TL::Source intrin_src;

            intrin_src << "_mm512_store";

            // Postfix
            if (type.is_float()) 
            { 
                intrin_src << "_ps("; 
            } 
            else if (type.is_double()) 
            { 
                intrin_src << "_pd("; 
            } 
            else if (type.is_integral_type()) 
            { 
                intrin_src << "_epi32((";
                intrin_src << print_type_str(
                        TL::Type::get_void_type().get_pointer_to().get_internal_type(),
                        node.retrieve_context().get_decl_context());
                intrin_src << ")";
            } 
            else
            {
                running_error("KNC Lowering: Node %s at %s has an unsupported type.", 
                        ast_print_node_type(node.get_kind()),
                        locus_to_str(node.get_locus()));
            }

            walk(node.get_lhs());
            walk(node.get_rhs());

            intrin_src << as_expression(node.get_lhs());
            intrin_src << ", ";
            intrin_src << as_expression(node.get_rhs());
            intrin_src << ")"; 

            Nodecl::NodeclBase function_call =
                intrin_src.parse_expression(node.retrieve_context());

            node.replace(function_call);
        }

        void KNCVectorLowering::visit(const Nodecl::VectorStoreMask& node) 
        { 
            TL::Type type = node.get_lhs().get_type().basic_type();
            TL::Source intrin_src;

            intrin_src << "_mm512_mask_store";

            // Postfix
            if (type.is_float()) 
            { 
                intrin_src << "_ps(";
            } 
            else if (type.is_double()) 
            { 
                intrin_src << "_pd("; 
            } 
            else if (type.is_integral_type()) 
            { 
                intrin_src << "_epi32((";
                intrin_src << print_type_str(
                        TL::Type::get_void_type().get_pointer_to().get_internal_type(),
                        node.retrieve_context().get_decl_context());
                intrin_src << ")";
            } 
            else
            {
                running_error("KNC Lowering: Node %s at %s has an unsupported type.", 
                        ast_print_node_type(node.get_kind()),
                        locus_to_str(node.get_locus()));
            }

            walk(node.get_lhs());
            walk(node.get_rhs());
            walk(node.get_mask());

            intrin_src << as_expression(node.get_lhs())
                << ", " 
                << as_expression(node.get_mask())
                << ", " 
                << as_expression(node.get_rhs()) 
                << ")"; 

            Nodecl::NodeclBase function_call =
                intrin_src.parse_expression(node.retrieve_context());

            node.replace(function_call);
        }

        void KNCVectorLowering::visit(const Nodecl::UnalignedVectorStore& node) 
        { 
            TL::Source intrin_src, intrin_name_hi, intrin_name_lo, args_lo, args_hi, tmp_var, ext;
            TL::Type type = node.get_lhs().get_type().basic_type();

            intrin_src 
                << "({"
                << tmp_var << ";"
                << intrin_name_lo << "(" << args_lo << ");"
                << intrin_name_hi << "(" << args_hi << ");"
                << "})"
                ;

            intrin_name_lo << "_mm512_extpackstorelo";
            intrin_name_hi << "_mm512_extpackstorehi";

            if (type.is_float()) 
            { 
                intrin_name_hi << "_ps"; 
                intrin_name_lo << "_ps"; 
                tmp_var << "__m512 ";
                ext << "_MM_DOWNCONV_PS_NONE";
            } 
            else if (type.is_double()) 
            { 
                intrin_name_hi << "_pd";
                intrin_name_lo << "_pd";
                tmp_var << "__m512d ";
                ext << "_MM_DOWNCONV_PD_NONE";
            } 
            else if (type.is_integral_type()) 
            { 
                intrin_name_hi << "_epi32";
                intrin_name_lo << "_epi32";
                tmp_var << "__m512i ";
                ext << "_MM_DOWNCONV_EPI32_NONE";

/*
                casting << "(" 
                    << print_type_str(
                        TL::Type::get_void_type().get_pointer_to().get_internal_type(),
                        node.retrieve_context().get_decl_context())
                    << ")"
                    ; 
*/
            } 
            else
            {
                running_error("KNC Lowering: Node %s at %s has an unsupported type.", 
                        ast_print_node_type(node.get_kind()),
                        locus_to_str(node.get_locus()));
            }


            walk(node.get_rhs());

            tmp_var << "__vtmp = "
                << as_expression(node.get_rhs())
                ;

            args_lo << as_expression(node.get_lhs())
                << ", "
                << "__vtmp"
                << ", "
                << ext
                << ", "
                << _MM_HINT_NONE
                ;

            args_hi << "("
                << as_expression(node.get_lhs())
                << ") + "
                << _vector_length
                << ", "
                << "__vtmp"
                << ", "
                << ext
                << ", "
                << _MM_HINT_NONE
                ;

            Nodecl::NodeclBase function_call =
                intrin_src.parse_expression(node.retrieve_context());
            
            node.replace(function_call);
        }

        void KNCVectorLowering::visit(const Nodecl::UnalignedVectorStoreMask& node) 
        { 
            TL::Source intrin_src, intrin_name_hi, intrin_name_lo, args_lo, args_hi, tmp_var, ext;
            TL::Type type = node.get_lhs().get_type().basic_type();

            intrin_src 
                << "({"
                << tmp_var << ";"
                << intrin_name_lo << "(" << args_lo << ");"
                << intrin_name_hi << "(" << args_hi << ");"
                << "})"
                ;

            intrin_name_lo << "_mm512_mask_extpackstorelo";
            intrin_name_hi << "_mm512_mask_extpackstorehi";

            if (type.is_float()) 
            { 
                intrin_name_hi << "_ps"; 
                intrin_name_lo << "_ps"; 
                tmp_var << "__m512 ";
                ext << "_MM_DOWNONV_PS_NONE";
            } 
            else if (type.is_double()) 
            { 
                intrin_name_hi << "_pd";
                intrin_name_lo << "_pd";
                tmp_var << "__m512d ";
                ext << "_MM_DOWNONV_PD_NONE";
            } 
            else if (type.is_integral_type()) 
            { 
                intrin_name_hi << "_epi32";
                intrin_name_lo << "_epi32";
                tmp_var << "__m512i ";
                ext << "_MM_DOWNONV_EPI32_NONE";
/*
                casting << "(" 
                    << print_type_str(
                        TL::Type::get_void_type().get_pointer_to().get_internal_type(),
                        node.retrieve_context().get_decl_context())
                    << ")"
                    ; 
*/
            } 
            else
            {
                running_error("KNC Lowering: Node %s at %s has an unsupported type.", 
                        ast_print_node_type(node.get_kind()),
                        locus_to_str(node.get_locus()));
            }


            walk(node.get_rhs());
            walk(node.get_mask());

            tmp_var << "__vtmp = "
                << as_expression(node.get_rhs())
                ;

            args_lo << as_expression(node.get_lhs())
                << ", "
                << as_expression(node.get_mask())
                << ", "
                << "__vtmp"
                << ", "
                << ext
                << ", "
                << _MM_HINT_NONE
                ;

            args_hi << "("
                << as_expression(node.get_lhs())
                << ") + "
                << _vector_length
                << ", "
                << as_expression(node.get_mask())
                << ", "
                << "__vtmp"
                << ", "
                << ext
                << ", "
                << _MM_HINT_NONE
                ;

            Nodecl::NodeclBase function_call =
                intrin_src.parse_expression(node.retrieve_context());
            
            node.replace(function_call);
        }

        void KNCVectorLowering::visit(const Nodecl::VectorGather& node) 
        { 
            TL::Type type = node.get_type().basic_type();
            TL::Type index_type = node.get_strides().get_type().basic_type();
            
            TL::Source intrin_src;

            intrin_src << "_mm512_i32gather";

            std::string extract;

            // Postfix
            if (type.is_float()) 
            { 
                intrin_src << "_ps";
            } 
            else if (type.is_signed_int() || type.is_unsigned_int()) 
            { 
                intrin_src << "_epi32";
            }
            else
            {
                running_error("KNC Lowering: Node %s at %s has an unsupported type.", 
                        ast_print_node_type(node.get_kind()),
                        locus_to_str(node.get_locus()));
            }

            if (!index_type.is_signed_int() && !index_type.is_unsigned_int()) 
            {
                running_error("KNC Lowering: Node %s at %s has an unsupported type.", 
                        ast_print_node_type(node.get_kind()),
                        locus_to_str(node.get_locus()));
            }

            walk(node.get_base());
            walk(node.get_strides());

            intrin_src << "("; 

            intrin_src << as_expression(node.get_base());
            intrin_src << ", ";
            intrin_src << as_expression(node.get_strides());
            intrin_src << ", ";
            intrin_src << type.get_size();

            intrin_src << ")";

            
            Nodecl::NodeclBase function_call =
                intrin_src.parse_expression(node.retrieve_context());

            node.replace(function_call);
        }

        void KNCVectorLowering::visit(const Nodecl::VectorGatherMask& node) 
        { 
            TL::Type type = node.get_type().basic_type();
            TL::Type index_type = node.get_strides().get_type().basic_type();
            
            TL::Source intrin_src, undef;

            intrin_src << "_mm512_mask_i32gather";

            std::string extract;

            // Postfix
            if (type.is_float()) 
            { 
                intrin_src << "_ps(";
                undef << "_mm512_undefined()";
            } 
            else if (type.is_signed_int() || type.is_unsigned_int()) 
            { 
                intrin_src << "_epi32(";
                undef << "_mm512_undefined_epi32()";
            }
            else
            {
                running_error("KNC Lowering: Node %s at %s has an unsupported type.", 
                        ast_print_node_type(node.get_kind()),
                        locus_to_str(node.get_locus()));
            }

            if (!index_type.is_signed_int() && !index_type.is_unsigned_int()) 
            {
                running_error("KNC Lowering: Node %s at %s has an unsupported type.", 
                        ast_print_node_type(node.get_kind()),
                        locus_to_str(node.get_locus()));
            }

            if (_old_m512.empty())
            {
                intrin_src << undef;
            }
            else
            {
                intrin_src << as_expression(_old_m512.back());
                _old_m512.pop_back();
            }


            walk(node.get_base());
            walk(node.get_strides());
            walk(node.get_mask());

            intrin_src << as_expression(node.get_mask())
               << ", "
               << as_expression(node.get_base())
               << ", "
               << as_expression(node.get_strides())
               << ", "
               << type.get_size()
               << ")";

            
            Nodecl::NodeclBase function_call =
                intrin_src.parse_expression(node.retrieve_context());

            node.replace(function_call);
        }

        void KNCVectorLowering::visit(const Nodecl::VectorScatter& node) 
        { 
            TL::Type type = node.get_source().get_type().basic_type();
            TL::Type index_type = node.get_strides().get_type().basic_type();

            std::string extract_index;
            std::string extract_source;

            TL::Source intrin_src;

            intrin_src << "_mm512_i32scatter";

            // Indexes
            if (!index_type.is_signed_int() && !index_type.is_unsigned_int()) 
            { 
                running_error("KNC Lowering: Node %s at %s has an unsupported index type.", 
                        ast_print_node_type(node.get_kind()),
                        locus_to_str(node.get_locus()));
            }

            // Source
            if (type.is_float()) 
            { 
                intrin_src << "_ps";
            } 
            else if (type.is_signed_int() || type.is_unsigned_int()) 
            { 
                intrin_src << "_epi32";
            }
            else
            {
                running_error("KNC Lowering: Node %s at %s has an unsupported source type.", 
                        ast_print_node_type(node.get_kind()),
                        locus_to_str(node.get_locus()));
            }

            walk(node.get_base());
            walk(node.get_strides());
            walk(node.get_source());

            intrin_src << "("
                << as_expression(node.get_base())
                << ", "
                << as_expression(node.get_strides())
                << ", "
                << as_expression(node.get_source())
                << ", "
                << type.get_size()
                << ")";


            Nodecl::NodeclBase function_call =
                intrin_src.parse_expression(node.retrieve_context());

            node.replace(function_call);
        }

        void KNCVectorLowering::visit(const Nodecl::VectorScatterMask& node) 
        { 
            TL::Type type = node.get_source().get_type().basic_type();
            TL::Type index_type = node.get_strides().get_type().basic_type();

            std::string extract_index;
            std::string extract_source;

            TL::Source intrin_src;

            intrin_src << "_mm512_mask_i32scatter";

            // Indexes
            if (!index_type.is_signed_int() && !index_type.is_unsigned_int()) 
            { 
                running_error("KNC Lowering: Node %s at %s has an unsupported index type.", 
                        ast_print_node_type(node.get_kind()),
                        locus_to_str(node.get_locus()));
            }

            // Source
            if (type.is_float()) 
            { 
                intrin_src << "_ps";
            } 
            else if (type.is_signed_int() || type.is_unsigned_int()) 
            { 
                intrin_src << "_epi32";
            }
            else
            {
                running_error("KNC Lowering: Node %s at %s has an unsupported source type.", 
                        ast_print_node_type(node.get_kind()),
                        locus_to_str(node.get_locus()));
            }

            walk(node.get_base());
            walk(node.get_strides());
            walk(node.get_source());
            walk(node.get_mask());

            intrin_src << "("
                << as_expression(node.get_mask())
                << ", "
                << as_expression(node.get_base())
                << ", "
                << as_expression(node.get_strides())
                << ", "
                << as_expression(node.get_source())
                << ", "
                << type.get_size()
                << ")";


            Nodecl::NodeclBase function_call =
                intrin_src.parse_expression(node.retrieve_context());

            node.replace(function_call);
        }
 
        void KNCVectorLowering::visit(const Nodecl::VectorFunctionCall& node) 
        {
            Nodecl::FunctionCall function_call =
                node.get_function_call().as<Nodecl::FunctionCall>();

            walk(function_call.get_arguments());

            node.replace(function_call);
        }

        void KNCVectorLowering::visit(const Nodecl::VectorFunctionCallMask& node) 
        {
            Nodecl::FunctionCall function_call =
                node.get_function_call().as<Nodecl::FunctionCall>();

            TL::Type type = node.get_type().basic_type();
            Nodecl::List arguments = function_call.get_arguments().as<Nodecl::List>();
            Nodecl::NodeclBase mask = node.get_mask();
            
            walk(mask);

            TL::Symbol scalar_sym =
                node.get_scalar_symbol().as<Nodecl::Symbol>().get_symbol();

            if(_vectorizer.is_svml_function(scalar_sym.get_name(),
                        "knc",
                        64,
                        NULL,
                        true)) // TODO
            {
                arguments.prepend(mask.shallow_copy());

                if (_old_m512.empty())
                {
                    TL::Source undef;

                    if (type.is_float()) 
                    { 
                        undef << "_mm512_undefined()";
                    } 
                    else if (type.is_double()) 
                    { 
                        undef << "_mm512_undefined_pd()";
                    } 
                    else if (type.is_integral_type()) 
                    { 
                        undef << "_mm512_undefined_epi32()";
                    }

                    Nodecl::NodeclBase undef_function_call =
                        undef.parse_expression(node.retrieve_context());

                    walk(arguments);
                    arguments.prepend(undef_function_call);
                }
                else
                {
                    Nodecl::NodeclBase old_m512 = _old_m512.back();
                    _old_m512.pop_back();

                    walk(arguments);
                    arguments.prepend(old_m512);
                }

                function_call.set_arguments(arguments);
            }
            else
            {
                walk(arguments);

                arguments.append(mask.shallow_copy());
                function_call.set_arguments(arguments);
            }
                    
            node.replace(function_call);
        }

        void KNCVectorLowering::visit(const Nodecl::VectorFabs& node) 
        {
            TL::Type type = node.get_type().basic_type();

            TL::Source intrin_src;

            // Handcoded implementations for float and double
            if (type.is_float()) 
            { 
                intrin_src << "_mm512_castsi512_ps(_mm512_and_epi32(_mm512_castps_si512(";
                intrin_src << as_expression(node.get_argument());
                intrin_src << "), _mm512_set1_epi32(0x7FFFFFFF)))"; 
            } 
            else if (type.is_double()) 
            { 
                intrin_src << "_mm512_castsi512_pd(_mm512_and_epi32(_mm512_castpd_si512(";
                intrin_src << as_expression(node.get_argument());
                intrin_src << "), _mm512_set1_epi64(0x7FFFFFFFFFFFFFFFLL)))"; 
            }
            else
            {
                /*
                // Intrinsic name
                intrin_src << "_mm512_abs";

                // Postfix
                if (type.is_signed_int() ||
                        type.is_unsigned_int()) 
                { 
                    intrin_src << "_epi32"; 
                } 
                else if (type.is_signed_short_int() ||
                        type.is_unsigned_short_int()) 
                { 
                    intrin_src << "_epi16"; 
                } 
                else if (type.is_char() || 
                        type.is_signed_char() ||
                        type.is_unsigned_char()) 
                { 
                    intrin_src << "_epi8"; 
                } 
                else
                {
                */
                    running_error("KNC Lowering: Node %s at %s has an unsupported type.", 
                            ast_print_node_type(node.get_kind()),
                            locus_to_str(node.get_locus()));
                //}
            }
            
            Nodecl::NodeclBase function_call =
                intrin_src.parse_expression(node.retrieve_context());
            
            node.replace(function_call);
        }

        void KNCVectorLowering::visit(const Nodecl::VectorFabsMask& node) 
        {
            TL::Type type = node.get_type().basic_type();

            TL::Source intrin_src, old_m512;

            if (_old_m512.empty())
            {
                old_m512 << "_mm512_undefined_epi32()";
            }
            else
            {
                TL::Type old_m512_type = _old_m512.back().get_type().basic_type();
                if (old_m512_type.is_float()) 
                { 
                    old_m512 << "_mm512_castps_si512";
                }

                else if (old_m512_type.is_double()) 
                {
                    old_m512 << "_mm512_castpd_si512";
                } 
                else
                {
                    running_error("KNC Lowering: Node %s at %s has an unsupported type.", 
                            ast_print_node_type(node.get_kind()),
                            locus_to_str(node.get_locus()));
                }

                old_m512 << "("
                    << as_expression(_old_m512.back())
                    << ")";

                _old_m512.pop_back();
            }
 
            walk(node.get_argument());
            walk(node.get_mask());

            // Handcoded implementations for float and double
            if (type.is_float()) 
            { 
                intrin_src << "_mm512_castsi512_ps(_mm512_mask_and_epi32("
                    << old_m512 << ", "
                    << as_expression(node.get_mask()) << ", "
                    << "_mm512_castps_si512("
                    << as_expression(node.get_argument())
                    << "), _mm512_set1_epi32(0x7FFFFFFF)))"; 
            } 
            else if (type.is_double()) 
            { 
                intrin_src << "_mm512_castsi512_pd(_mm512_mask_and_epi32("
                    << old_m512 << ", "
                    << as_expression(node.get_mask()) << ", "
                    << "_mm512_castpd_si512("
                    << as_expression(node.get_argument())
                    << "), _mm512_set1_epi64(0x7FFFFFFFFFFFFFFFLL)))"; 
            }
            else
            {
                /*
                // Intrinsic name
                intrin_src << "_mm512_abs";

                // Postfix
                if (type.is_signed_int() ||
                        type.is_unsigned_int()) 
                { 
                    intrin_src << "_epi32"; 
                } 
                else if (type.is_signed_short_int() ||
                        type.is_unsigned_short_int()) 
                { 
                    intrin_src << "_epi16"; 
                } 
                else if (type.is_char() || 
                        type.is_signed_char() ||
                        type.is_unsigned_char()) 
                { 
                    intrin_src << "_epi8"; 
                } 
                else
                {
                */
                    running_error("KNC Lowering: Node %s at %s has an unsupported type.", 
                            ast_print_node_type(node.get_kind()),
                            locus_to_str(node.get_locus()));
                //}
            }
            
            Nodecl::NodeclBase function_call =
                intrin_src.parse_expression(node.retrieve_context());
            
            node.replace(function_call);
        }

        void KNCVectorLowering::visit(const Nodecl::ParenthesizedExpression& node)
        {
            walk(node.get_nest());

            Nodecl::NodeclBase n(node.shallow_copy());
            n.set_type(node.get_nest().get_type());
            node.replace(n);
        }

        void KNCVectorLowering::visit(const Nodecl::VectorMaskAssignment& node)
        {
            TL::Source intrin_src;

            walk(node.get_lhs());
            walk(node.get_rhs());

            intrin_src << as_expression(node.get_lhs())
                << " = "
                << "_mm512_kmov("
                << as_expression(node.get_rhs())
                << ")"
                ;

            Nodecl::NodeclBase function_call =
                intrin_src.parse_expression(node.retrieve_context());

            node.replace(function_call);
        }

        void KNCVectorLowering::visit(const Nodecl::VectorMaskNot& node)
        {
            TL::Source intrin_src;

            walk(node.get_rhs());

            intrin_src << "_mm512_knot("
                << as_expression(node.get_rhs())
                << ")"
                ;

            Nodecl::NodeclBase function_call =
                intrin_src.parse_expression(node.retrieve_context());

            node.replace(function_call);
        }

        void KNCVectorLowering::visit(const Nodecl::VectorMaskAnd& node)
        {
            TL::Source intrin_src;

            walk(node.get_lhs());
            walk(node.get_rhs());

            intrin_src << "_mm512_kand("
                << as_expression(node.get_lhs())
                << ", "
                << as_expression(node.get_rhs())
                << ")"
                ;

            Nodecl::NodeclBase function_call =
                intrin_src.parse_expression(node.retrieve_context());

            node.replace(function_call);
        }

        void KNCVectorLowering::visit(const Nodecl::VectorMaskAnd1Not& node)
        {
            TL::Source intrin_src;

            walk(node.get_lhs());
            walk(node.get_rhs());

            intrin_src << "_mm512_kandn("
                << as_expression(node.get_lhs())
                << ", "
                << as_expression(node.get_rhs())
                << ")"
                ;

            Nodecl::NodeclBase function_call =
                intrin_src.parse_expression(node.retrieve_context());

            node.replace(function_call);
        }

        void KNCVectorLowering::visit(const Nodecl::VectorMaskAnd2Not& node)
        {
            TL::Source intrin_src;

            walk(node.get_lhs());
            walk(node.get_rhs());

            intrin_src << "_mm512_kandnr("
                << as_expression(node.get_lhs())
                << ", "
                << as_expression(node.get_rhs())
                << ")"
                ;

            Nodecl::NodeclBase function_call =
                intrin_src.parse_expression(node.retrieve_context());

            node.replace(function_call);
        }


        Nodecl::NodeclVisitor<void>::Ret KNCVectorLowering::unhandled_node(const Nodecl::NodeclBase& n) 
        { 
            running_error("KNC Lowering:º Unknown node %s at %s.",
                    ast_print_node_type(n.get_kind()),
                    locus_to_str(n.get_locus())); 

            return Ret(); 
        }
    }
}
