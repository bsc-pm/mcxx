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

#include "tl-vector-backend-knl.hpp"

#define KNL_VECTOR_BIT_SIZE 512
#define KNL_VECTOR_BYTE_SIZE 64
#define KNL_INTRIN_PREFIX "_mm512"
#define KNL_MASK_BIT_SIZE 16


namespace TL
{
namespace Vectorization
{
    KNLVectorBackend::KNLVectorBackend()
        : KNCVectorBackend()
    {
        std::cerr << "--- KNL backend phase ---" << std::endl;
    }

    void KNLVectorBackend::visit(const Nodecl::VectorLoad& n)
    {
        Nodecl::NodeclBase rhs = n.get_rhs();
        Nodecl::NodeclBase mask = n.get_mask();
        Nodecl::List flags = n.get_flags().as<Nodecl::List>();
        TL::Type type = n.get_type().basic_type();

        bool aligned = !flags.find_first<Nodecl::AlignedFlag>().is_null();


        TL::Source intrin_src, intrin_name, intrin_type_suffix, intrin_op_name,
            mask_prefix, args, mask_args, extra_args;

        intrin_src << intrin_name
            << "("
            << args
            << ")"
            ;

        intrin_name << KNL_INTRIN_PREFIX
            << mask_prefix
            << "_"
            << intrin_op_name
            << "_"
            << intrin_type_suffix
            ;

        process_mask_component(mask, mask_prefix, mask_args, type);

        if (aligned)
            intrin_op_name << "load";
        else
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
            intrin_type_suffix << "epi32";
        }
        else
        {
            internal_error("KNL Backend: Node %s at %s has an unsupported type.",
                    ast_print_node_type(n.get_kind()),
                    locus_to_str(n.get_locus()));
        }

        walk(rhs);

        args << mask_args
            << as_expression(rhs)
            << extra_args
            ;

        Nodecl::NodeclBase function_call =
            intrin_src.parse_expression(n.retrieve_context());

        n.replace(function_call);
    }

    void KNLVectorBackend::visit_common_vector_store(
            const Nodecl::VectorStore& n,
            const bool aligned)
    {
        Nodecl::NodeclBase lhs = n.get_lhs();
        Nodecl::NodeclBase rhs = n.get_rhs();
        Nodecl::NodeclBase mask = n.get_mask();

        TL::Type type = n.get_lhs().get_type().basic_type();

        TL::Source intrin_src, intrin_name, intrin_op_name, args,
            intrin_type_suffix, mask_prefix, mask_args, casting_args;

        intrin_src << intrin_name
            << "("
            << args
            << ")"
            ;

        intrin_name << KNL_INTRIN_PREFIX
            << mask_prefix
            << "_"
            << intrin_op_name
            << "_"
            << intrin_type_suffix
            ;

        process_mask_component(mask, mask_prefix, mask_args, type,
                KNCConfigMaskProcessing::ONLY_MASK );

        if (aligned)
            intrin_op_name << "store";
        else
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
            intrin_type_suffix << "si512";
            casting_args << get_casting_to_scalar_pointer(
                    TL::Type::get_void_type());
        }
        else
        {
            internal_error("KNL Backend: Node %s at %s has an unsupported type.",
                    ast_print_node_type(n.get_kind()),
                    locus_to_str(n.get_locus()));
        }

        walk(lhs);
        walk(rhs);

        args << "("
            << casting_args
            << as_expression(lhs)
            << "), "
            << mask_args
            << as_expression(rhs)
            ;

        Nodecl::NodeclBase function_call =
            intrin_src.parse_expression(n.retrieve_context());

        n.replace(function_call);
    }

    void KNLVectorBackend::visit_aligned_vector_stream_store(
            const Nodecl::VectorStore& n)
    {
        Nodecl::NodeclBase mask = n.get_mask();

        // Stream store with mask is not supported
        // Emit store with hint instead
        if(!mask.is_null())
        {
            visit_common_vector_store(n.as<Nodecl::VectorStore>(),
                    true /*aligned*/);
            return;
        }

        Nodecl::NodeclBase lhs = n.get_lhs();
        Nodecl::NodeclBase rhs = n.get_rhs();

        TL::Type type = n.get_lhs().get_type().basic_type();

        Nodecl::List ss_flags = n.get_flags().as<Nodecl::List>();

        bool relaxed_store = !ss_flags.find_first<Nodecl::RelaxedFlag>().
           is_null();
        bool cache_eviction = !ss_flags.find_first<Nodecl::EvictFlag>().
            is_null();

        TL::Source stream_store_src, cache_evict_src, intrin_src, intrin_name,
            intrin_op_name, args,intrin_type_suffix, mask_prefix, mask_args;

        stream_store_src
            << "({"
            << intrin_src << ";"
            << cache_evict_src << ";"
            << "})"
            ;

        intrin_src << intrin_name
            << "("
            << args
            << ")"
            ;

        intrin_name << KNL_INTRIN_PREFIX
            << mask_prefix
            << "_"
            << intrin_op_name
            << "_"
            << intrin_type_suffix
            ;

        process_mask_component(mask, mask_prefix, mask_args, type,
                KNCConfigMaskProcessing::ONLY_MASK );

        if (relaxed_store)
            intrin_op_name << "storenrngo";
        else
            intrin_op_name << "storenr";


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
            internal_error("KNC Backend: Node %s at %s has an unsupported type.",
                    ast_print_node_type(n.get_kind()),
                    locus_to_str(n.get_locus()));
        }

        walk(lhs);
        walk(rhs);

        args << "("
            << get_casting_to_scalar_pointer(
                    TL::Type::get_void_type())
            << as_expression(lhs)
            << ")"
            << ", "
            << mask_args
            << as_expression(rhs)
            ;

        if (cache_eviction)
        {
            cache_evict_src << "_mm_clevict("
                << as_expression(lhs)
                << ", 1)"   //L1 = 0, L2 = 1
                ;
        }

        Nodecl::NodeclBase function_call =
            stream_store_src.parse_expression(n.retrieve_context());

        n.replace(function_call);
    }


    void KNLVectorBackend::visit(const Nodecl::VectorStore& n)
    {
        Nodecl::List flags = n.get_flags().as<Nodecl::List>();

        bool aligned = !flags.find_first<Nodecl::AlignedFlag>().
            is_null();
        bool stream = !flags.find_first<Nodecl::NontemporalFlag>().
            is_null();

        if (aligned)
        {
            if (stream)
                visit_aligned_vector_stream_store(n);
            else
                visit_common_vector_store(n, true /*aligned*/);
        }
        else
        {
            if (stream)
                visit_common_vector_store(n, false /*aligned*/);
            else
                visit_common_vector_store(n, false /*aligned*/);
        }
    }
}
}
