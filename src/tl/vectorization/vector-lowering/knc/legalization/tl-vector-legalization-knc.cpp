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

#include "tl-source.hpp"

#include "tl-vectorization-common.hpp"
#include "tl-vectorization-utils.hpp"
#include "tl-vector-legalization-knc.hpp"

#include "tl-nodecl-utils.hpp"

#define NUM_8B_ELEMENTS 8
#define NUM_4B_ELEMENTS 16

namespace TL
{
namespace Vectorization
{
    KNCVectorLegalization::KNCVectorLegalization(bool prefer_gather_scatter,
            bool prefer_mask_gather_scatter)
        : _vector_length(KNC_VECTOR_LENGTH),
        _prefer_gather_scatter(prefer_gather_scatter),
        _prefer_mask_gather_scatter(prefer_mask_gather_scatter)
    {
        std::cerr << "--- KNC legalization phase ---" << std::endl;
    }

    void KNCVectorLegalization::visit(const Nodecl::ObjectInit& n)
    {
        TL::Source intrin_src;

        if(n.has_symbol())
        {
            TL::Symbol sym = n.get_symbol();

            // Vectorizing initialization
            Nodecl::NodeclBase init = sym.get_value();
            if(!init.is_null())
            {
                walk(init);
            }
        }
    }

    void KNCVectorLegalization::visit(const Nodecl::VectorConversion& n)
    {
        walk(n.get_nest());

        const TL::Type& src_vector_type = n.get_nest().get_type().get_unqualified_type().no_ref();
        const TL::Type& dst_vector_type = n.get_type().get_unqualified_type().no_ref();
        const TL::Type& src_type = src_vector_type.basic_type().get_unqualified_type();
        const TL::Type& dst_type = dst_vector_type.basic_type().get_unqualified_type();
       
        // If mask type, conversion is not needed
        if (dst_vector_type.is_mask() && src_type.is_integral_type())
        {
            n.replace(n.get_nest());
        }
        else
        {
            /*
               printf("Conversion from %s(%s) to %s(%s)\n",
               src_vector_type.get_simple_declaration(n.retrieve_context(), "").c_str(),
               src_type.get_simple_declaration(n.retrieve_context(), "").c_str(),
               dst_vector_type.get_simple_declaration(n.retrieve_context(), "").c_str(),
               dst_type.get_simple_declaration(n.retrieve_context(), "").c_str());
             */
            const unsigned int src_num_elements = src_vector_type.vector_num_elements();
            const unsigned int dst_num_elements = dst_vector_type.vector_num_elements();

            // 4-byte element vector type
            if ((src_type.is_float() ||
                        src_type.is_signed_int() ||
                        src_type.is_unsigned_int())
                    && (src_num_elements < NUM_4B_ELEMENTS))
            {
                // If src type is float8, int8, ... then it will be converted to float16, int16
                if(src_num_elements == 8)
                {
                    n.get_nest().set_type(
                            src_type.get_vector_of_elements(NUM_4B_ELEMENTS));
                }

                // If dst type is float8, int8, ... then it will be converted to float16, int16
                if ((dst_type.is_float() ||
                            dst_type.is_signed_int() ||
                            dst_type.is_unsigned_int())
                        && (dst_num_elements < NUM_4B_ELEMENTS))
                {
                    if(dst_num_elements == 8)
                    {
                        Nodecl::NodeclBase new_n = n.shallow_copy();
                        new_n.set_type(dst_type.get_vector_of_elements(NUM_4B_ELEMENTS));
                        n.replace(new_n);
                    }

                }
            }
        }
    }

    void KNCVectorLegalization::visit(const Nodecl::VectorLoad& n)
    {


        const Nodecl::NodeclBase rhs = n.get_rhs();
        const Nodecl::NodeclBase mask = n.get_mask();

        walk(rhs);
        walk(mask);

        // Turn unaligned load into gather
        if (_prefer_gather_scatter ||
                (_prefer_mask_gather_scatter && !mask.is_null()))
        {
            VECTORIZATION_DEBUG()
            {
                fprintf(stderr, "KNC Legalization: Turn unaligned load '%s' into "\
                        "adjacent gather\n", rhs.prettyprint().c_str());
            }

            Nodecl::VectorGather vector_gather = n.get_flags().
                as<Nodecl::List>().find_first<Nodecl::VectorGather>();

            ERROR_CONDITION(vector_gather.is_null(), "Gather is null in "\
                    "legalization of unaligned load with mask", 0);

            // Visit Gather
            walk(vector_gather);

            VECTORIZATION_DEBUG()
            {
/*                fprintf(stderr, "    Gather '%s' "
//                        "(base: %s strides: %s\n", n.prettyprint().c_str(),
//                        vector_gather.get_base().prettyprint().c_str(),
                        vector_gather.get_strides().prettyprint().c_str());
*/          }

            n.replace(vector_gather);
        }
    }

    void KNCVectorLegalization::visit(
            const Nodecl::VectorStore& n)
    {
        TL::ObjectList<Nodecl::NodeclBase> flags = 
            n.get_flags().as<Nodecl::List>().to_object_list();

        bool aligned =
            Nodecl::Utils::list_contains_nodecl_by_structure(
                flags, Nodecl::AlignedFlag());

        if (!aligned)
        {
            const Nodecl::NodeclBase lhs = n.get_lhs();
            const Nodecl::NodeclBase rhs = n.get_rhs();
            const Nodecl::NodeclBase mask = n.get_mask();

            walk(lhs);
            walk(rhs);
            walk(mask);

            // Turn unaligned store into scatter
            if (_prefer_gather_scatter ||
                    (_prefer_mask_gather_scatter && !mask.is_null()))
            {
                VECTORIZATION_DEBUG()
                {
                    fprintf(stderr, "KNC Legalization: Turn unaligned store '%s'"\
                            "into adjacent scatter\n",
                            lhs.prettyprint().c_str());
                }

                Nodecl::VectorScatter vector_scatter = n.get_flags().
                    as<Nodecl::List>().find_first<Nodecl::VectorScatter>();

                ERROR_CONDITION(vector_scatter.is_null(), "Scatter is null in "\
                        "legalization of unaligned load with mask", 0);

                // Visit Scatter
                walk(vector_scatter);

                VECTORIZATION_DEBUG()
                {
                    /*                fprintf(stderr, "    Scatter '%s' "\
                                      "(base: %s strides: %s\n",
                                      lhs.prettyprint().c_str(),
                                      vector_scatter.get_base().prettyprint().c_str(),
                                      vector_scatter.get_strides().prettyprint().c_str());
                     */          }

                n.replace(vector_scatter);
            }
        }
    }

    void KNCVectorLegalization::visit(const Nodecl::VectorGather& n)
    {
        const Nodecl::NodeclBase base = n.get_base();

        walk(base);

        TL::Type index_vector_type = n.get_strides().get_type();
        TL::Type index_type = index_vector_type.basic_type();

        if (index_type.is_signed_long_int() || index_type.is_unsigned_long_int())
        {
            TL::Type dst_vector_type = TL::Type::get_int_type().get_vector_of_elements(
                    index_vector_type.vector_num_elements());

            VECTORIZATION_DEBUG()
            {
                fprintf(stderr, "KNC Legalization: '%s' gather indexes conversion from %s(%s) to %s(%s)\n",
                        base.prettyprint().c_str(),
                        index_vector_type.get_simple_declaration(n.retrieve_context(), "").c_str(),
                        index_type.get_simple_declaration(n.retrieve_context(), "").c_str(),
                        dst_vector_type.get_simple_declaration(n.retrieve_context(), "").c_str(),
                        dst_vector_type.basic_type().get_simple_declaration(n.retrieve_context(), "").c_str());
            }

            Nodecl::VectorGather new_gather = n.shallow_copy().as<Nodecl::VectorGather>();
            KNCStrideVisitorConv stride_visitor(index_vector_type.vector_num_elements());
            stride_visitor.walk(new_gather.get_strides());

            new_gather.get_strides().set_type(dst_vector_type);

            n.replace(new_gather);
        }
        else
        {
            walk(n.get_strides());
        }
    }

    void KNCVectorLegalization::visit(const Nodecl::VectorScatter& n)
    {
        const Nodecl::NodeclBase base = n.get_base();

        walk(base);
        walk(n.get_source());

        TL::Type index_vector_type = n.get_strides().get_type();
        TL::Type index_type = index_vector_type.basic_type();

        if (index_type.is_signed_long_int() || index_type.is_unsigned_long_int())
        {
            TL::Type dst_vector_type = TL::Type::get_int_type().get_vector_of_elements(
                    n.get_strides().get_type().vector_num_elements());

            VECTORIZATION_DEBUG()
            {
                fprintf(stderr, "KNC Lowering: '%s' scatter indexes conversion from %s(%s) to %s(%s)\n",
                        base.prettyprint().c_str(),
                        index_vector_type.get_simple_declaration(n.retrieve_context(), "").c_str(),
                        index_type.get_simple_declaration(n.retrieve_context(), "").c_str(),
                        dst_vector_type.get_simple_declaration(n.retrieve_context(), "").c_str(),
                        dst_vector_type.basic_type().get_simple_declaration(n.retrieve_context(), "").c_str());
            }

            Nodecl::VectorScatter new_scatter = n.shallow_copy().as<Nodecl::VectorScatter>();
            KNCStrideVisitorConv stride_visitor(index_vector_type.vector_num_elements());
            stride_visitor.walk(new_scatter.get_strides());

            new_scatter.get_strides().set_type(dst_vector_type);

            n.replace(new_scatter);
        }
        else
        {
            walk(n.get_strides());
        }
    }

    Nodecl::NodeclVisitor<void>::Ret KNCVectorLegalization::unhandled_node(
            const Nodecl::NodeclBase& n)
    {
        running_error("KNC Legalization: Unknown n %s at %s.",
                ast_print_node_type(n.get_kind()),
                locus_to_str(n.get_locus()));

        return Ret();
    }


    KNCStrideVisitorConv::KNCStrideVisitorConv(unsigned int vector_num_elements)
        : _vector_num_elements(vector_num_elements)
    {
    }

    /*
       void KNCStrideVisitorConv::visit(const Nodecl::VectorConversion& n)
       {
       walk(n.get_nest());

       Nodecl::VectorConversion new_n = n.shallow_copy().as<Nodecl::VectorConversion>();

    // TODO better
    new_n.set_type(TL::Type::get_int_type().get_vector_of_elements(
    _vector_num_elements));

    n.replace(new_n);
    }
     */

    Nodecl::NodeclVisitor<void>::Ret KNCStrideVisitorConv::unhandled_node(
            const Nodecl::NodeclBase& n)
    {
        //printf("Unsupported %d: %s\n", _vector_num_elements, n.prettyprint().c_str());

        if (n.get_type().is_vector())
        {

            Nodecl::NodeclBase new_n = n.shallow_copy().as<Nodecl::NodeclBase>();

            new_n.set_type(TL::Type::get_int_type().get_vector_of_elements(
                        _vector_num_elements));

            // TODO better
            n.replace(new_n);

            TL::ObjectList<Nodecl::NodeclBase> children = n.children();
            for(TL::ObjectList<Nodecl::NodeclBase>::iterator it = children.begin();
                    it != children.end();
                    it ++)
            {
                walk(*it);
            }
        }
        return Ret();
    }
}
}
