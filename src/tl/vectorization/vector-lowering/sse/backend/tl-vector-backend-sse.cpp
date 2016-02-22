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

#include "tl-vector-backend-sse.hpp"
#include "tl-nodecl-utils.hpp"
#include "tl-source.hpp"

#include "cxx-diagnostic.h"

#define SSE_VECTOR_BIT_SIZE 128
#define SSE_VECTOR_BYTE_SIZE 16
#define SSE_INTRIN_PREFIX "_mm"
#define SSE_MASK_BIT_SIZE 0

namespace TL 
{
    namespace Vectorization
    {
        SSEVectorBackend::SSEVectorBackend()
        {
            std::cerr << "--- SSE backend phase ---" << std::endl;
        }

        std::string SSEVectorBackend::get_casting_intrinsic(const TL::Type& type_from,
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
                        result << SSE_INTRIN_PREFIX << "_castps_pd";
                    }
                    else if (type_to.is_signed_int() || type_to.is_unsigned_int())
                    {
                        result << SSE_INTRIN_PREFIX << "_castps_si" << 
                            SSE_VECTOR_BIT_SIZE;
                    }
                }
            }
            else if (type_from.is_signed_int() || type_from.is_unsigned_int())
            {
                if ((!type_to.is_signed_int()) && (!type_to.is_unsigned_int()))
                {
                    if (type_to.is_float())
                    {
                        result << SSE_INTRIN_PREFIX << "_castsi" << 
                            SSE_VECTOR_BIT_SIZE << "_ps";
                    }
                    else if (type_to.is_double())
                    {
                        result << SSE_INTRIN_PREFIX << "_castsi" <<
                            SSE_VECTOR_BIT_SIZE << "_pd";
                    }
                }
            }
            else
            {            
                fatal_printf_at(locus, "SSE Backend: casting intrinsic not supported");
            }

            return result.str(); 
        }

        void SSEVectorBackend::visit(const Nodecl::ObjectInit& node) 
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

        void SSEVectorBackend::visit(const Nodecl::VectorAdd& node) 
        { 
            TL::Type type = node.get_type().basic_type();

            TL::Source intrin_src;

            intrin_src << "_mm_add";

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
                fatal_printf_at(node.get_locus(),
                        "SSE Backend: Node %s at %s has an unsupported type.", 
                        ast_print_node_type(node.get_kind()),
                        locus_to_str(node.get_locus()));
            }      
            //std::cerr << node.get_lhs().prettyprint() << " " << node.get_rhs().prettyprint();
            //

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

        void SSEVectorBackend::visit(const Nodecl::VectorMinus& node) 
        { 
            TL::Type type = node.get_type().basic_type();

            TL::Source intrin_src;

            intrin_src << "_mm_sub";

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
                fatal_printf_at(node.get_locus(),
                        "SSE Backend: Node %s at %s has an unsupported type.", 
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

        void SSEVectorBackend::visit(const Nodecl::VectorMul& node) 
        {
            TL::Type result_type = node.get_type().basic_type();
            TL::Type first_op_type = node.get_rhs().get_type().basic_type();
            TL::Type second_op_type = node.get_lhs().get_type().basic_type();

            TL::Source intrin_src;

            intrin_src << "_mm_mul";

            // Postfix
            if (result_type.is_float() &&
                    first_op_type.is_float() &&
                    second_op_type.is_float())
            {
                intrin_src << "_ps"; 
            } 
            else if (result_type.is_double() &&
                    first_op_type.is_double() &&
                    second_op_type.is_double())
            { 
                intrin_src << "_pd"; 
            }
            else if (result_type.is_signed_int() &&
                    first_op_type.is_signed_int() &&
                    second_op_type.is_signed_int())
            {
                intrin_src << "lo_epi32"; 
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
                fatal_printf_at(node.get_locus(),
                        "SSE Backend: Node %s at %s has an unsupported type.", 
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

        void SSEVectorBackend::visit(const Nodecl::VectorDiv& node) 
        { 
            TL::Type type = node.get_type().basic_type();

            TL::Source intrin_src;

            // Intrinsic name
            intrin_src << "_mm_div";

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
            else
            {
                fatal_printf_at(node.get_locus(),
                        "SSE Backend: Node %s at %s has an unsupported type.", 
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

        void SSEVectorBackend::visit(const Nodecl::VectorLowerThan& node) 
        { 
            TL::Type type = node.get_lhs().get_type().basic_type();

            TL::Source casting, intrin_src;

            // Intrinsic name
            intrin_src << casting << "_mm_cmplt";

            // Postfix
            if (type.is_float()) 
            { 
                casting << "(" << as_type(TL::Type::get_int_type().get_vector_of_elements(4)) << ")";
                intrin_src << "_ps"; 
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
                fatal_printf_at(node.get_locus(),
                        "SSE Backend: Node %s at %s has an unsupported type.", 
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

        void SSEVectorBackend::visit(const Nodecl::VectorLowerOrEqualThan& node) 
        { 
            TL::Type type = node.get_lhs().get_type().basic_type();

            TL::Source intrin_src, casting;

            // Intrinsic name
            intrin_src << casting <<"_mm_cmpgt";

            // Postfix
            if (type.is_float()) 
            { 
                casting << "(" << as_type(TL::Type::get_int_type().get_vector_of_elements(4)) << ")";
                intrin_src << "_ps"; 
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
                fatal_printf_at(node.get_locus(),
                        "SSE Backend: Node %s at %s has an unsupported type.", 
                        ast_print_node_type(node.get_kind()),
                        locus_to_str(node.get_locus()));
            }      

            walk(node.get_lhs());
            walk(node.get_rhs());

            intrin_src << "(";
            intrin_src << as_expression(node.get_rhs());
            intrin_src << ", ";
            intrin_src << as_expression(node.get_lhs());
            intrin_src << ")";

            Nodecl::NodeclBase function_call = 
                    intrin_src.parse_expression(node.retrieve_context());

            node.replace(function_call);
        }                                                 

        void SSEVectorBackend::visit(const Nodecl::VectorGreaterThan& node) 
        { 
            TL::Type type = node.get_lhs().get_type().basic_type();

            TL::Source intrin_src, casting;

            // Intrinsic name
            intrin_src << casting << "_mm_cmpgt";

            // Postfix
            if (type.is_float()) 
            { 
                casting << "(" << as_type(TL::Type::get_int_type().get_vector_of_elements(4)) << ")";
                intrin_src << "_ps"; 
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
                fatal_printf_at(node.get_locus(),
                        "SSE Backend: Node %s at %s has an unsupported type.", 
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

        void SSEVectorBackend::visit(const Nodecl::VectorGreaterOrEqualThan& node) 
        { 
            TL::Type type = node.get_lhs().get_type().basic_type();

            TL::Source intrin_src, casting;

            // Intrinsic name
            intrin_src << casting << "_mm_cmplt";

            // Postfix
            if (type.is_float()) 
            { 
                casting << "(" << as_type(TL::Type::get_int_type().get_vector_of_elements(4)) << ")";
                intrin_src << "_ps"; 
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
                fatal_printf_at(node.get_locus(),
                        "SSE Backend: Node %s at %s has an unsupported type.", 
                        ast_print_node_type(node.get_kind()),
                        locus_to_str(node.get_locus()));
            }      

            walk(node.get_lhs());
            walk(node.get_rhs());

            intrin_src << "(";
            intrin_src << as_expression(node.get_rhs());
            intrin_src << ", ";
            intrin_src << as_expression(node.get_lhs());
            intrin_src << ")";

            Nodecl::NodeclBase function_call = 
                    intrin_src.parse_expression(node.retrieve_context());

            node.replace(function_call);
        }

        void SSEVectorBackend::visit(const Nodecl::VectorEqual& node) 
        { 
            TL::Type type = node.get_lhs().get_type().basic_type();

            TL::Source intrin_src, casting;

            // Intrinsic name
            intrin_src << casting << "_mm_cmpeq";

            // Postfix
            if (type.is_float()) 
            { 
                casting << "(" << as_type(TL::Type::get_int_type().get_vector_of_elements(4)) << ")";
                intrin_src << "_ps"; 
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
                fatal_printf_at(node.get_locus(),
                        "SSE Backend: Node %s at %s has an unsupported type.", 
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

        void SSEVectorBackend::visit(const Nodecl::VectorDifferent& node) 
        { 
            TL::Type type = node.get_lhs().get_type().basic_type();

            TL::Source intrin_src, casting;

            // Intrinsic name
            intrin_src
                << "(" << as_type(TL::Type::get_int_type().get_vector_of_elements(4)) << ")"
                << "_mm_andnot_ps("
                << casting << "_mm_cmpeq";

            // Postfix
            if (type.is_float()) 
            { 
                intrin_src << "_ps"; 
            } 
            else if (type.is_signed_int() ||
                    type.is_unsigned_int()) 
            { 
                casting << "(" << as_type(TL::Type::get_float_type().get_vector_of_elements(4)) << ")";
                intrin_src << "_epi32"; 
            } 
            else if (type.is_signed_short_int() ||
                    type.is_unsigned_short_int()) 
            { 
                casting << "(" << as_type(TL::Type::get_float_type().get_vector_of_elements(4)) << ")";
                intrin_src << "_epi16"; 
            } 
            else if (type.is_char() || 
                    type.is_signed_char() ||
                    type.is_unsigned_char()) 
            { 
                casting << "(" << as_type(TL::Type::get_float_type().get_vector_of_elements(4)) << ")";
                intrin_src << "_epi8"; 
            } 
            else
            {
                fatal_printf_at(node.get_locus(),
                        "SSE Backend: Node %s at %s has an unsupported type.", 
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

            intrin_src << ", (" << as_type(TL::Type::get_float_type().get_vector_of_elements(4)) << ") _mm_set1_epi32(~0))";

            Nodecl::NodeclBase function_call =
                intrin_src.parse_expression(node.retrieve_context());

            node.replace(function_call);
        }                                                 

        void SSEVectorBackend::visit(const Nodecl::VectorBitwiseAnd& node) 
        { 
            TL::Type type = node.get_type().basic_type();

            TL::Source intrin_src;

            // Intrinsic name
            intrin_src << "_mm_and";

            // Postfix
            if (type.is_float()) 
            { 
                intrin_src << "_ps"; 
            } 
            else if (type.is_integral_type())
            { 
                intrin_src << "_si128"; 
            } 
            else
            {
                fatal_printf_at(node.get_locus(),
                        "SSE Backend: Node %s at %s has an unsupported type.", 
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

        void SSEVectorBackend::visit(const Nodecl::VectorBitwiseOr& node) 
        { 
            TL::Type type = node.get_type().basic_type();

            TL::Source intrin_src;

            // Intrinsic name
            intrin_src << "_mm_or";

            // Postfix
            if (type.is_float()) 
            { 
                intrin_src << "_ps"; 
            } 
            else if (type.is_integral_type())
            { 
                intrin_src << "_si128"; 
            } 
            else
            {
                fatal_printf_at(node.get_locus(),
                        "SSE Backend: Node %s at %s has an unsupported type.", 
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

        void SSEVectorBackend::visit(const Nodecl::VectorBitwiseXor& node) 
        { 
            TL::Type type = node.get_type().basic_type();

            TL::Source intrin_src;

            // Intrinsic name
            intrin_src << "_mm_xor";

            // Postfix
            if (type.is_float()) 
            { 
                intrin_src << "_ps"; 
            } 
            else if (type.is_integral_type())
            { 
                intrin_src << "_si128"; 
            }
            else
            {
                fatal_printf_at(node.get_locus(),
                        "SSE Backend: Node %s at %s has an unsupported type.", 
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

        void SSEVectorBackend::visit(const Nodecl::VectorAlignRight& node) 
        { 
            const Nodecl::NodeclBase left_vector = node.get_left_vector();
            const Nodecl::NodeclBase right_vector = node.get_right_vector();
            const Nodecl::NodeclBase num_elements = node.get_num_elements();
            const Nodecl::NodeclBase mask = node.get_mask();

            TL::Type type = node.get_type().basic_type();

            TL::Source intrin_src, casting_intrin, intrin_name, intrin_type_suffix, intrin_op_name,
                mask_prefix, casting_args, args, mask_args, rhs_expression;

            intrin_src << casting_intrin
                << "("
                << intrin_name
                << "("
                << args
                << "))"
                ;

            intrin_name << "_mm"
                << mask_prefix
                << "_"
                << intrin_op_name
                << "_"
                << intrin_type_suffix
                ;

            intrin_op_name << "alignr";
            intrin_type_suffix << "epi8"; 

            //process_mask_component(mask, mask_prefix, mask_args, type);

            if (type.is_float()) 
            { 
                casting_intrin << "_mm_castsi128_ps";
                casting_args << "_mm_castps_si128";
            } 
            else if (type.is_signed_int() ||
                    type.is_unsigned_int()) 
            { 
            } 
            else
            {
                internal_error("SSE Backend: Node %s at %s has an unsupported type (%s).", 
                        ast_print_node_type(node.get_kind()),
                        locus_to_str(node.get_locus()),
                        type.get_simple_declaration(node.retrieve_context(), "").c_str());
            }      

            walk(left_vector);
            walk(right_vector);
            walk(num_elements);

            args << mask_args
                << casting_args << "(" << as_expression(left_vector) << ")"
                << ", "
                << casting_args << "(" << as_expression(right_vector) << ")"
                << ", "
                << as_expression(num_elements) << "* 4"
                ;

            Nodecl::NodeclBase function_call =
                intrin_src.parse_expression(node.retrieve_context());

            node.replace(function_call);
        }

        void SSEVectorBackend::visit(const Nodecl::VectorLogicalOr& node) 
        { 
            fatal_printf_at(node.get_locus(),
                    "SSE Backend %s: 'logical or' operation (i.e., operator '||') is not supported in SSE. Try using 'bitwise or' operations (i.e., operator '|') instead if possible.",
                    locus_to_str(node.get_locus()));
        }                                                 

        void SSEVectorBackend::visit(const Nodecl::VectorNeg& node) 
        {
            TL::Type type = node.get_type().basic_type();

            TL::Source intrin_src;

            if (type.is_float()) 
            { 
                intrin_src << "_mm_xor_ps(_mm_castsi128_ps(_mm_set1_epi32(0x80000000)), ";
            } 
            else if (type.is_double()) 
            { 
                intrin_src << "_mm_xor_pd(_mm_castsi128_pd(_mm_set1_epi64(0x8000000000000000LL)), ";
            }
            else if (type.is_signed_int() ||
                    type.is_unsigned_int())
            {
                intrin_src << "_mm_sub_epi32( _mm_setzero_si128(),";
            }
            else if (type.is_signed_short_int() ||
                    type.is_unsigned_short_int())
            {
                intrin_src << "_mm_sub_epi16( _mm_setzero_si128(),";
            }
            else if (type.is_char() ||
                    type.is_signed_char() ||
                    type.is_unsigned_char())
            {
                intrin_src << "_mm_sub_epi8( _mm_setzero_si128(),";
            }
            else
            {
                fatal_printf_at(node.get_locus(),
                        "SSE Backend: Node %s at %s has an unsupported type.", 
                        ast_print_node_type(node.get_kind()),
                        locus_to_str(node.get_locus()));
            } 

            walk(node.get_rhs());

            intrin_src << as_expression(node.get_rhs());
            intrin_src << ")";

            Nodecl::NodeclBase function_call =
                intrin_src.parse_expression(node.retrieve_context());

            node.replace(function_call);
        }                                                 

        void SSEVectorBackend::visit(const Nodecl::VectorConversion& node) 
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
                    (src_type.is_signed_int() && dst_type.is_signed_long_int()) ||
                    (src_type.is_signed_int() && dst_type.is_unsigned_long_int()) ||
                    (src_type.is_unsigned_int() && dst_type.is_signed_long_int()) ||
                    (src_type.is_unsigned_int() && dst_type.is_unsigned_long_int()) ||
                    (src_type.is_signed_int() && src_type.is_signed_long_int()) ||
                    (src_type.is_signed_int() && src_type.is_unsigned_long_int()) ||
                    (src_type.is_unsigned_int() && src_type.is_signed_long_int()) ||
                    (src_type.is_unsigned_int() && src_type.is_unsigned_long_int()) ||
                    (src_type.is_signed_short_int() && dst_type.is_unsigned_short_int()) ||
                    (dst_type.is_signed_short_int() && src_type.is_unsigned_short_int()))
            {
                node.replace(node.get_nest());
                return;
            }
            else if (src_type.is_signed_int() &&
                    dst_type.is_float()) 
            { 
                intrin_src << "_mm_cvtepi32_ps("; 
                intrin_src << as_expression(node.get_nest());
                intrin_src << ")"; 
            } 
            else if (src_type.is_float() &&
                    dst_type.is_signed_int()) 
            { 
                // C/C++ requires truncated conversion
                intrin_src << "_mm_cvttps_epi32("; 
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
                intrin_src << "_mm_packs_epi16("; 
                intrin_src << "_mm_packs_epi32("; 
                intrin_src << "_mm_cvttps_epi32("; 
                walk(node.get_nest());
                intrin_src << "),"; 
                intrin_src << "_mm_castps_si128(";
                walk(node.get_nest());
                intrin_src << ")";
                //intrin_src << "_mm_undefined_si128()"; 
                intrin_src << "),"; 
                intrin_src << "_mm_castps_si128(";
                walk(node.get_nest());
                intrin_src << ")";
                //intrin_src << "_mm_undefined_si128()"; 
                intrin_src << ")"; 
            } 
            else if (src_type.is_float() &&
                    dst_type.is_unsigned_char()) 
            {
                // Saturated conversion
                intrin_src << "_mm_packus_epi16("; 
                intrin_src << "_mm_packus_epi32("; 
                intrin_src << "_mm_cvttps_epi32("; 
                walk(node.get_nest());
                intrin_src << "),"; 
                intrin_src << "_mm_castps_si128(";
                walk(node.get_nest());
                intrin_src << ")";
                //intrin_src << "_mm_undefined_si128()"; 
                intrin_src << "),"; 
                intrin_src << "_mm_castps_si128(";
                walk(node.get_nest());
                //intrin_src << "_mm_undefined_si128()"; 
                intrin_src << "))"; 
            } 
            else if (src_type.is_signed_int() &&
                    (dst_type.is_signed_char() ||
                     dst_type.is_char())) 
            {
                // Saturated conversion
                intrin_src << "_mm_packs_epi16("; 
                intrin_src << "_mm_packs_epi32("; 
                walk(node.get_nest());
                intrin_src << ","; 
                walk(node.get_nest());
                //intrin_src << "_mm_undefined_si128()"; 
                intrin_src << "),"; 
                walk(node.get_nest());
                //intrin_src << "_mm_undefined_si128()"; 
                intrin_src << ")"; 
            } 
            else if (src_type.is_signed_int() &&
                    dst_type.is_unsigned_char()) 
            {
                // Saturated conversion
                intrin_src << "_mm_packus_epi16("; 
                intrin_src << "_mm_packus_epi32("; 
                walk(node.get_nest());
                intrin_src << ","; 
                walk(node.get_nest());
                //intrin_src << "_mm_undefined_si128()"; 
                intrin_src << ")"; 
                intrin_src << ",";
                walk(node.get_nest());
                //intrin_src << "_mm_undefined_si128()"; 
                intrin_src << ")"; 
            }
            */
            else
            {
                fprintf(stderr, "SSE Backend: Conversion at '%s' is not supported yet: %s\n", 
                        locus_to_str(node.get_locus()),
                        node.get_nest().prettyprint().c_str());
            }   

            Nodecl::NodeclBase function_call =
                intrin_src.parse_expression(node.retrieve_context());

            node.replace(function_call);
        }

        void SSEVectorBackend::visit(const Nodecl::VectorPromotion& node) 
        { 
            TL::Type type = node.get_type().basic_type();

            TL::Source intrin_src;

            // Intrinsic name
            intrin_src << "_mm_set";

            // Postfix
            if (type.is_float()) 
            { 
                intrin_src << "_ps1"; 
            } 
            else if (type.is_double()) 
            { 
                intrin_src << "1_pd"; 
            } 
            else if (type.is_signed_int() ||
                    type.is_unsigned_int()) 
            { 
                intrin_src << "1_epi32"; 
            } 
            else if (type.is_signed_short_int() ||
                    type.is_unsigned_short_int()) 
            { 
                intrin_src << "1_epi16"; 
            } 
            else if (type.is_char() || 
                    type.is_signed_char() ||
                    type.is_unsigned_char()) 
            { 
                intrin_src << "1_epi8"; 
            } 
            else
            {
                fatal_printf_at(node.get_locus(),
                        "SSE Backend: Node %s at %s has an unsupported type.", 
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

        void SSEVectorBackend::visit(const Nodecl::VectorLiteral& node) 
        {
            TL::Type type = node.get_type().basic_type();

            TL::Source intrin_src;

            // Intrinsic name
            intrin_src << "_mm_set";

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
                fatal_printf_at(node.get_locus(),
                        "SSE Backend: Node %s at %s has an unsupported type.", 
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


        void SSEVectorBackend::visit(const Nodecl::VectorConditionalExpression& node) 
        { 
            TL::Type type = node.get_type().basic_type();

            TL::Source intrin_src;

            Nodecl::NodeclBase true_node = node.get_true();
            Nodecl::NodeclBase false_node = node.get_false();
            Nodecl::NodeclBase condition_node = node.get_condition();

            TL::Type true_type = true_node.get_type().basic_type();
            TL::Type false_type = false_node.get_type().basic_type();
            TL::Type condiition_type = condition_node.get_type();

            std::string casting;

            // Intrinsic name
            intrin_src << "_mm_blend";

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

                casting = "(";
                casting += print_type_str(TL::Type::get_float_type().get_vector_of_bytes(16).get_internal_type(),
                        node.retrieve_context().get_decl_context());
                casting += ")";
            }
            else if (true_type.is_double()
                    && false_type.is_double())
            {
                // TODO _pd
                intrin_src << "v_pd";
                casting = "(";
                casting += print_type_str(TL::Type::get_double_type().get_vector_of_bytes(16).get_internal_type(),
                        node.retrieve_context().get_decl_context());
                casting += ")";
            }
            else
            {
                fatal_printf_at(node.get_locus(),
                        "SSE Backend: Node %s at %s has an unsupported type.", 
                        ast_print_node_type(node.get_kind()),
                        locus_to_str(node.get_locus()));
            }

            walk(false_node);
            walk(true_node);
            walk(condition_node);

            intrin_src << "("; 
            intrin_src << as_expression(false_node); // False first!
            intrin_src << ", ";
            intrin_src << as_expression(true_node);
            intrin_src << ", "
                << casting;
            intrin_src << as_expression(condition_node);
            intrin_src << ")"; 

            Nodecl::NodeclBase function_call =
                intrin_src.parse_expression(node.retrieve_context());

            node.replace(function_call);
        }        

        void SSEVectorBackend::visit(const Nodecl::VectorAssignment& node) 
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

        void SSEVectorBackend::visit(const Nodecl::VectorLoad& node) 
        { 
            Nodecl::List flags = node.get_flags().as<Nodecl::List>();

            bool aligned = !flags.find_first<Nodecl::AlignedFlag>().
                is_null();

            if (aligned)
                visit_aligned_vector_load(node);
            else
                visit_unaligned_vector_load(node);
        }

        void SSEVectorBackend::visit_aligned_vector_load(
                const Nodecl::VectorLoad& node) 
        {
            TL::Type type = node.get_type().basic_type();

            TL::Source intrin_src;

            // Intrinsic name
            intrin_src << "_mm_load";

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
                intrin_src << "_si128(("; 
                intrin_src << print_type_str(
                        TL::Type::get_long_long_int_type().get_vector_of_bytes(16).get_pointer_to().get_internal_type(),
                        node.retrieve_context().get_decl_context());

                intrin_src << ")"; 
            } 
            else
            {
                fatal_printf_at(node.get_locus(),
                        "SSE Backend: Node %s at %s has an unsupported type.", 
                        ast_print_node_type(node.get_kind()),
                        locus_to_str(node.get_locus()));
            }

            walk(node.get_rhs());

            intrin_src << as_expression(node.get_rhs());
            intrin_src << ")"; 

            Nodecl::NodeclBase function_call =
                intrin_src.parse_expression(node.retrieve_context());

            node.replace(function_call);
        }

        void SSEVectorBackend::visit_unaligned_vector_load(
                const Nodecl::VectorLoad& node) 
        { 
            TL::Type type = node.get_type().basic_type();

            TL::Source intrin_src;

            // Intrinsic name
            intrin_src << "_mm_loadu";

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
                intrin_src << "_si128(("; 
                intrin_src << print_type_str(
                        TL::Type::get_long_long_int_type().get_vector_of_bytes(16).get_pointer_to().get_internal_type(),
                        node.retrieve_context().get_decl_context());

                intrin_src << ")"; 
            } 
            else
            {
                fatal_printf_at(node.get_locus(),
                        "SSE Backend: Node %s at %s has an unsupported type.", 
                        ast_print_node_type(node.get_kind()),
                        locus_to_str(node.get_locus()));
            }

            walk(node.get_rhs());

            intrin_src << as_expression(node.get_rhs());
            intrin_src << ")"; 

            Nodecl::NodeclBase function_call =
                intrin_src.parse_expression(node.retrieve_context());

            node.replace(function_call);
        }

        void SSEVectorBackend::visit(const Nodecl::VectorStore& node) 
        {
            Nodecl::List flags = node.get_flags().as<Nodecl::List>();

            bool aligned = !flags.find_first<Nodecl::AlignedFlag>().
                is_null();

            if (aligned)
                visit_aligned_vector_store(node);
            else
                visit_unaligned_vector_store(node);
        }

        void SSEVectorBackend::visit_aligned_vector_store(
                const Nodecl::VectorStore& node) 
        {
            TL::Type type = node.get_lhs().get_type().basic_type();

            TL::Source intrin_src;

            // Intrinsic name
            intrin_src << "_mm_store";

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
                intrin_src << "_si128((";
                intrin_src << print_type_str(
                        TL::Type::get_long_long_int_type().get_vector_of_bytes(16).get_pointer_to().get_internal_type(),
                        node.retrieve_context().get_decl_context());
                intrin_src << ")";
            } 
            else
            {
                fatal_printf_at(node.get_locus(),
                        "SSE Backend: Node %s at %s has an unsupported type.", 
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

        void SSEVectorBackend::visit_unaligned_vector_store(
                const Nodecl::VectorStore& node) 
        { 
            TL::Type type = node.get_lhs().get_type().basic_type();

            TL::Source intrin_src;

            // Intrinsic name
            intrin_src << "_mm_storeu";

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
                intrin_src << "_si128((";
                intrin_src << print_type_str(
                        TL::Type::get_long_long_int_type().get_vector_of_bytes(16).get_pointer_to().get_internal_type(),
                        node.retrieve_context().get_decl_context());
                intrin_src << ")";
            } 
            else
            {
                fatal_printf_at(node.get_locus(),
                        "SSE Backend: Node %s at %s has an unsupported type.", 
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

        void SSEVectorBackend::visit(const Nodecl::VectorGather& node) 
        { 
            TL::Type type = node.get_type().basic_type();
            TL::Type index_type = node.get_strides().get_type().basic_type();

            TL::Source intrin_src;

            // Intrinsic name
            intrin_src << "_mm_set";

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
            else if (type.is_signed_short_int() || type.is_unsigned_short_int()) 
            { 
                intrin_src << "_epi16";
            }
            else if (type.is_signed_char() || type.is_char() || type.is_unsigned_char())
            {
                intrin_src << "_epi8";
            }
            else
            {
                fatal_printf_at(node.get_locus(),
                        "SSE Backend: Node %s at %s has an unsupported type.", 
                        ast_print_node_type(node.get_kind()),
                        locus_to_str(node.get_locus()));
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
                fatal_printf_at(node.get_locus(),
                        "SSE Backend: Node %s at %s has an unsupported type.", 
                        ast_print_node_type(node.get_kind()),
                        locus_to_str(node.get_locus()));
            }

            walk(node.get_base());
            walk(node.get_strides());

            intrin_src << "("; 

            unsigned int i = 0;

            intrin_src << as_expression(node.get_base());
            intrin_src << "[";

            intrin_src << extract;
            intrin_src << "(";
            intrin_src << as_expression(node.get_strides());
            intrin_src << ", " << i << ")";

            intrin_src << "]";

            i++;

            for (; i < type.get_size(); i++)
            {
                intrin_src << ", ";

                intrin_src << as_expression(node.get_base());
                intrin_src << "[";

                intrin_src << extract;
                intrin_src << "(";

                intrin_src << as_expression(node.get_strides());
                intrin_src << ", " << i << ")";

                intrin_src << "]";
            }

            intrin_src << ")"; 

            Nodecl::NodeclBase function_call =
                intrin_src.parse_expression(node.retrieve_context());

            node.replace(function_call);
        }

        void SSEVectorBackend::visit(const Nodecl::VectorScatter& node) 
        { 
            TL::Type type = node.get_source().get_type().basic_type();
            TL::Type index_type = node.get_strides().get_type().basic_type();

            TL::Source intrin_src;
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
                fatal_printf_at(node.get_locus(),
                        "SSE Backend: Node %s at %s has an unsupported index type.", 
                        ast_print_node_type(node.get_kind()),
                        locus_to_str(node.get_locus()));
            }

            // Source
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
                fatal_printf_at(node.get_locus(),
                        "SSE Backend: Node %s at %s has an unsupported source type.", 
                        ast_print_node_type(node.get_kind()),
                        locus_to_str(node.get_locus()));
            }

            walk(node.get_base());
            walk(node.get_strides());
            walk(node.get_source());

            for (unsigned int i=0; i < type.get_size(); i++)
            {
                intrin_src << as_expression(node.get_base());
                intrin_src << "[";

                intrin_src << extract_index;
                intrin_src << "(";

                intrin_src << as_expression(node.get_strides());
                intrin_src << ", ";
                intrin_src << i;
                intrin_src << ")";

                intrin_src << "]";

                intrin_src << " = " << extract_source << "(";
                intrin_src << as_expression(node.get_source());
                intrin_src << ", ";
                intrin_src << i;
                intrin_src << ")";

                intrin_src << ";\n";
            }

            Nodecl::NodeclBase function_call =
                intrin_src.parse_expression(node.retrieve_context());

            node.replace(function_call);
        }

        void SSEVectorBackend::visit(const Nodecl::VectorFunctionCall& node) 
        {
            Nodecl::FunctionCall function_call =
                node.get_function_call().as<Nodecl::FunctionCall>();

            walk(function_call.get_arguments());

            node.replace(function_call);
        }

        void SSEVectorBackend::visit(const Nodecl::VectorFabs& node) 
        {
            TL::Type type = node.get_type().basic_type();

            TL::Source intrin_src;

            walk(node.get_argument());

            // Handcoded implementations for float and double
            if (type.is_float()) 
            { 
                intrin_src << "(_mm_and_ps(";
                intrin_src << as_expression(node.get_argument());
                intrin_src << ", _mm_castsi128_ps(_mm_set1_epi32(0x7FFFFFFF))))"; 
            } 
            else if (type.is_double()) 
            { 
                intrin_src << "(_mm_and_pd(";
                intrin_src << as_expression(node.get_argument());
                intrin_src << ", _mm_castsi128_pd(_mm_set1_epi64(0x7FFFFFFFFFFFFFFFLL))))"; 
            }
            else
            {
                /*
                // Intrinsic name
                intrin_src << "_mm_abs";

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
                    fatal_printf_at(node.get_locus(),
                            "SSE Backend: Node %s at %s has an unsupported type.", 
                            ast_print_node_type(node.get_kind()),
                            locus_to_str(node.get_locus()));
                //}
            }
            
            Nodecl::NodeclBase function_call =
                intrin_src.parse_expression(node.retrieve_context());
            
            node.replace(function_call);
        }

        void SSEVectorBackend::visit(const Nodecl::ParenthesizedExpression& node)
        {
            walk(node.get_nest());
            Nodecl::NodeclBase n(node);
        
            n.set_type(node.get_nest().get_type());
        }

        void SSEVectorBackend::visit(const Nodecl::VectorReductionAdd& node) 
        { 
            Nodecl::NodeclBase vector_src = node.get_vector_src();

            TL::Type vtype = vector_src.get_type().no_ref();
            TL::Type type = node.get_type().no_ref();
            
            walk(vector_src);

            TL::Source intrin_src, horizontal_op_src, horizontal_intrin_src, extract_op_src, extract_intrin_src;

            intrin_src 
                << "({"
                << horizontal_op_src
                << horizontal_op_src
                << extract_op_src
                << "})";

            // Postfix
            if (type.is_float()) 
            { 
                horizontal_intrin_src << "_mm_hadd_ps"; 
                extract_intrin_src << "_mm_cvtss_f32";
            } 
            else if (type.is_double()) 
            { 
                horizontal_intrin_src << "_mm_hadd_pd"; 
                extract_intrin_src << "_mm_cvtsd_f64";
            } 
            else if (type.is_signed_int() ||
                    type.is_unsigned_int()) 
            { 
                horizontal_intrin_src << "_mm_hadd_epi32"; 
                extract_intrin_src << "_mm_cvtsi128_si32";
            } 
            else
            {
                fatal_printf_at(node.get_locus(),
                        "SSE Backend: Node %s at %s has an unsupported type.", 
                        ast_print_node_type(node.get_kind()),
                        locus_to_str(node.get_locus()));
            }      
            //std::cerr << node.get_lhs().prettyprint() << " " << node.get_rhs().prettyprint();

            horizontal_op_src
                << as_expression(vector_src)
                << " = "
                << horizontal_intrin_src
                << "(" 
                << as_expression(vector_src)
                << ", "
                << as_expression(vector_src)
                << ");";

            extract_op_src
                << extract_intrin_src
                << "(" 
                << as_expression(vector_src)
                << ");";

            Nodecl::NodeclBase function_call = 
                    intrin_src.parse_expression(node.retrieve_context());

            node.replace(function_call);
        }

        void SSEVectorBackend::visit(const Nodecl::VectorReductionMinus& node)
        {
            // A famous OpenMP blunder
            visit(node.as<Nodecl::VectorReductionAdd>());
        }

        namespace {
            TL::Type sse_comparison_type()
            {
                return TL::Type::get_int_type().get_vector_of_elements(4);
            }
        }

        void SSEVectorBackend::visit(const Nodecl::VectorMaskAssignment& node)
        {
            walk(node.get_lhs());
            walk(node.get_rhs());

            // Emit this is as a plain assignment
            node.replace(
                    Nodecl::Assignment::make(
                        node.get_lhs(),
                        node.get_rhs(),
                        sse_comparison_type().get_lvalue_reference_to()));
        }

        void SSEVectorBackend::visit(const Nodecl::VectorMaskNot& node)
        {
            TL::Type type = node.get_rhs().get_type().basic_type();

            TL::Source intrin_src;

            walk(node.get_rhs());

            intrin_src
                << "_mm_andnot_si128("
                << as_expression(node.get_rhs()) << ","
                << "_mm_set1_epi32(~0))"
                ;

            Nodecl::NodeclBase function_call =
                intrin_src.parse_expression(node.retrieve_context());

            node.replace(function_call);
        }

#define UNSUPPORTED_MASK(node) \
        fatal_printf_at(node.get_locus(), \
                "SSE Backend: Vector masks are not supported in SSE (node=%s).", \
                ast_print_node_type((node).get_kind()))

        void SSEVectorBackend::visit(const Nodecl::VectorMaskConversion& node)
        {
            UNSUPPORTED_MASK(node);
        }

        void SSEVectorBackend::visit(const Nodecl::VectorMaskAnd& node)
        {
            UNSUPPORTED_MASK(node);
        }

        void SSEVectorBackend::visit(const Nodecl::VectorMaskOr& node)
        {
            UNSUPPORTED_MASK(node);
        }

        void SSEVectorBackend::visit(const Nodecl::VectorMaskAnd1Not& node)
        {
            UNSUPPORTED_MASK(node);
        }

        void SSEVectorBackend::visit(const Nodecl::VectorMaskAnd2Not& node)
        {
            UNSUPPORTED_MASK(node);
        }

        void SSEVectorBackend::visit(const Nodecl::VectorMaskXor& node)
        {
            UNSUPPORTED_MASK(node);
        }

        void SSEVectorBackend::visit(const Nodecl::MaskLiteral& node)
        {
            UNSUPPORTED_MASK(node);
        }

        void SSEVectorBackend::visit(const Nodecl::VectorSqrt& node)
        {
            TL::Type type = node.get_type().basic_type();

            if (!node.get_mask().is_null())
                fatal_printf_at(node.get_locus(),
                        "SSE Backend: Node %s at %s has an unsupported mask", 
                        ast_print_node_type(node.get_kind()),
                        locus_to_str(node.get_locus()));

            TL::Source intrin_src;

            intrin_src << "_mm_sqrt";

            // Postfix
            if (type.is_float()) 
            { 
                intrin_src << "_ps"; 
            } 
            else if (type.is_double()) 
            { 
                intrin_src << "_pd"; 
            } 
            else
            {
                fatal_printf_at(node.get_locus(),
                        "SSE Backend: Node %s at %s has an unsupported type.", 
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

        void SSEVectorBackend::visit(const Nodecl::VectorRcp& node)
        {
            TL::Type type = node.get_type().basic_type();

            if (!node.get_mask().is_null())
                fatal_printf_at(node.get_locus(),
                        "SSE Backend: Node %s at %s has an unsupported mask", 
                        ast_print_node_type(node.get_kind()),
                        locus_to_str(node.get_locus()));

            TL::Source intrin_src, one;

            // Intrinsic name
            intrin_src << "_mm_div";

            // Postfix
            if (type.is_float()) 
            { 
                one << "_mm_set_ps1(1.0f)";
                intrin_src << "_ps"; 
            } 
            else if (type.is_double()) 
            { 
                one << "_mm_set1_pd(1.0)";
                intrin_src << "_pd"; 
            } 
            else if (type.is_signed_int() ||
                    type.is_unsigned_int()) 
            { 
                one << "_mm_set1_epi32(1)";
                intrin_src << "_epi32"; 
            } 
            else if (type.is_signed_short_int() ||
                    type.is_unsigned_short_int()) 
            { 
                one << "_mm_set1_epi16(1)";
                intrin_src << "_epi16"; 
            } 
            else
            {
                fatal_printf_at(node.get_locus(),
                        "SSE Backend: Node %s at %s has an unsupported type.", 
                        ast_print_node_type(node.get_kind()),
                        locus_to_str(node.get_locus()));
            }      

            walk(node.get_rhs());

            intrin_src << "(";
            intrin_src << one;
            intrin_src << ", ";
            intrin_src << as_expression(node.get_rhs());
            intrin_src << ")";

            Nodecl::NodeclBase function_call = 
                    intrin_src.parse_expression(node.retrieve_context());

            node.replace(function_call);
        }

        void SSEVectorBackend::visit(const Nodecl::VectorRsqrt& node)
        {
            TL::Type type = node.get_type().basic_type();

            if (!node.get_mask().is_null())
                fatal_printf_at(node.get_locus(),
                        "SSE Backend: Node %s at %s has an unsupported mask", 
                        ast_print_node_type(node.get_kind()),
                        locus_to_str(node.get_locus()));

            TL::Source intrin_src;

            intrin_src << "_mm_rsqrt";

            // Postfix
            if (type.is_float()) 
            { 
                intrin_src << "_ps"; 
            } 
            else if (type.is_double()) 
            { 
                intrin_src << "_pd"; 
            } 
            else
            {
                fatal_printf_at(node.get_locus(),
                        "SSE Backend: Node %s at %s has an unsupported type.", 
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


    }
}
