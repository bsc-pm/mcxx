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

#include "tl-vector-legalization-sse.hpp"
#include "tl-source.hpp"

#include "cxx-graphviz.h"

namespace TL 
{
    namespace Vectorization
    {
        SSEVectorLegalization::SSEVectorLegalization() 
        {
            std::cerr << "--- SSE legalization phase ---" << std::endl;
        }

        void SSEVectorLegalization::visit(const Nodecl::FunctionCode& node)
        {
            ast_dump_graphviz(::nodecl_get_ast(node.get_internal_nodecl()), stderr);
            std::cerr << "\n\n\n\n";
        }

        void SSEVectorLegalization::visit(const Nodecl::ObjectInit& node) 
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

        void SSEVectorLegalization::visit(const Nodecl::VectorConversion& node) 
        {
#if 0
            const TL::Type& src_vector_type = node.get_nest().get_type().get_unqualified_type().no_ref();
            const TL::Type& dst_vector_type = node.get_type().get_unqualified_type().no_ref();
            const TL::Type& src_type = src_vector_type.basic_type().get_unqualified_type();
            const TL::Type& dst_type = dst_vector_type.basic_type().get_unqualified_type();
//            const int src_type_size = src_type.get_size();
//            const int dst_type_size = dst_type.get_size();
/*
            printf("Conversion from %s(%s) to %s(%s)\n",
                    src_vector_type.get_simple_declaration(node.retrieve_context(), "").c_str(),
                    src_type.get_simple_declaration(node.retrieve_context(), "").c_str(),
                    dst_vector_type.get_simple_declaration(node.retrieve_context(), "").c_str(),
                    dst_type.get_simple_declaration(node.retrieve_context(), "").c_str());
*/
            const unsigned int src_num_elements = src_vector_type.vector_num_elements();
            const unsigned int dst_num_elements = dst_vector_type.vector_num_elements();

            walk(node.get_nest());

            // 4-byte element vector type
            if ((src_type.is_float() || 
                        src_type.is_signed_int() ||
                        src_type.is_unsigned_int()) 
                    && (src_num_elements < NUM_4B_ELEMENTS))
            {
                // If src type is float8, int8, ... then it will be converted to float16, int16
                if(src_num_elements == 8)
                {
                    node.get_nest().set_type(
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
                        Nodecl::NodeclBase new_node = node.shallow_copy();
                        new_node.set_type(dst_type.get_vector_of_elements(NUM_4B_ELEMENTS));
                        node.replace(new_node);
                    }

                } 
            } 
#endif
        }

        void SSEVectorLegalization::visit(const Nodecl::VectorGather& node) 
        {
#if 0
            walk(node.get_base());

            TL::Type index_vector_type = node.get_strides().get_type();
            TL::Type index_type = index_vector_type.basic_type();

            if (index_type.is_signed_long_int() || index_type.is_unsigned_long_int()) 
            {
                TL::Type dst_vector_type = TL::Type::get_int_type().get_vector_of_elements(
                        index_vector_type.vector_num_elements());
/*
                printf("Gather indexes conversion from %s(%s) to %s(%s)\n",
                    index_vector_type.get_simple_declaration(node.retrieve_context(), "").c_str(),
                    index_type.get_simple_declaration(node.retrieve_context(), "").c_str(),
                    dst_vector_type.get_simple_declaration(node.retrieve_context(), "").c_str(),
                    dst_vector_type.basic_type().get_simple_declaration(node.retrieve_context(), "").c_str());
*/
                Nodecl::VectorGather new_gather = node.shallow_copy().as<Nodecl::VectorGather>();
                SSEStrideVisitorConv stride_visitor(index_vector_type.vector_num_elements());
                stride_visitor.walk(new_gather.get_strides());

                new_gather.get_strides().set_type(dst_vector_type);

                node.replace(new_gather);
            }
            else
            {
                walk(node.get_strides());
            }
#endif
        }

        void SSEVectorLegalization::visit(const Nodecl::VectorScatter& node) 
        { 
#if 0
            walk(node.get_base());
            walk(node.get_source());

            TL::Type index_vector_type = node.get_strides().get_type();
            TL::Type index_type = index_vector_type.basic_type();

            if (index_type.is_signed_long_int() || index_type.is_unsigned_long_int()) 
            {
                TL::Type dst_vector_type = TL::Type::get_int_type().get_vector_of_elements(
                        node.get_strides().get_type().vector_num_elements());
/*
                printf("Scatter indexes conversion from %s(%s) to %s(%s)\n",
                    index_vector_type.get_simple_declaration(node.retrieve_context(), "").c_str(),
                    index_type.get_simple_declaration(node.retrieve_context(), "").c_str(),
                    dst_vector_type.get_simple_declaration(node.retrieve_context(), "").c_str(),
                    dst_vector_type.basic_type().get_simple_declaration(node.retrieve_context(), "").c_str());
*/
                Nodecl::VectorScatter new_scatter = node.shallow_copy().as<Nodecl::VectorScatter>();
                SSEStrideVisitorConv stride_visitor(index_vector_type.vector_num_elements());
                stride_visitor.walk(new_scatter.get_strides());

                new_scatter.get_strides().set_type(dst_vector_type);

                node.replace(new_scatter);
            }
            else
            {
                walk(node.get_strides());
            }
#endif
        }

        Nodecl::NodeclVisitor<void>::Ret SSEVectorLegalization::unhandled_node(const Nodecl::NodeclBase& n) 
        { 
            fatal_error("SSE Legalization: Unknown node %s at %s.",
                    ast_print_node_type(n.get_kind()),
                    locus_to_str(n.get_locus())); 

            return Ret(); 
        }


        SSEStrideVisitorConv::SSEStrideVisitorConv(unsigned int vector_num_elements)
            : _vector_num_elements(vector_num_elements)
        {
        }

        /*
        void SSEStrideVisitorConv::visit(const Nodecl::VectorConversion& node)
        {
            walk(node.get_nest());

            Nodecl::VectorConversion new_node = node.shallow_copy().as<Nodecl::VectorConversion>();

            // TODO better
            new_node.set_type(TL::Type::get_int_type().get_vector_of_elements(
                        _vector_num_elements));

            node.replace(new_node);
        }
        */

        Nodecl::NodeclVisitor<void>::Ret SSEStrideVisitorConv::unhandled_node(const Nodecl::NodeclBase& node) 
        {
            //printf("Unsupported %d: %s\n", _vector_num_elements, node.prettyprint().c_str()); 
            
            if (node.get_type().is_vector())
            {

                Nodecl::NodeclBase new_node = node.shallow_copy().as<Nodecl::NodeclBase>();

                new_node.set_type(TL::Type::get_int_type().get_vector_of_elements(
                            _vector_num_elements));

                // TODO better
                node.replace(new_node);

                Nodecl::NodeclBase::Children children = node.children();
                for(Nodecl::NodeclBase::Children::iterator it = children.begin();
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
