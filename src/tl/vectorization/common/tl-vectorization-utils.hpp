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

#ifndef TL_VECTORIZATION_UTILS_HPP
#define TL_VECTORIZATION_UTILS_HPP

#include "tl-vectorization-common.hpp"
#include "tl-vector-isa-descriptor.hpp"

#include "tl-nodecl-visitor.hpp"
#include <map>


namespace TL
{
    namespace Vectorization
    {
        namespace Utils
        {
            bool is_vector_node(Nodecl::NodeclBase n);
            bool contains_vector_nodes(Nodecl::NodeclBase n);

            class MaskCheckCostEstimation : public Nodecl::ExhaustiveVisitor<void>
            {
                private:
                    const unsigned int _add_cost;
                    const unsigned int _minus_cost;
                    const unsigned int _mul_cost;
                    const unsigned int _div_cost;
                    const unsigned int _return_cost;
                    const unsigned int _if_statement_cost;
                    const unsigned int _else_statement_cost;
                    const unsigned int _static_for_statement_cost;
                    //const unsigned int _masked_for_statement_cost;
                    const unsigned int _function_call_cost;

                    const unsigned int _nesting_threshold;

                    unsigned int _nesting_level;
                    unsigned int _cost;

                    void binary_operation(const Nodecl::NodeclBase& n,
                            const unsigned int cost);

                public:
                    MaskCheckCostEstimation();

                    unsigned int get_mask_check_cost(
                            const Nodecl::NodeclBase& n,
                            unsigned int initial_cost,
                            const unsigned int cost_threshold);

                    virtual void visit(const Nodecl::IfElseStatement& n);
                    virtual void visit(const Nodecl::ForStatement& n);
                    virtual void visit(const Nodecl::FunctionCall& n);
                    virtual void visit(const Nodecl::Add& n);
                    virtual void visit(const Nodecl::Minus& n);
                    virtual void visit(const Nodecl::Mul& n);
                    virtual void visit(const Nodecl::Div& n);
                    virtual void visit(const Nodecl::ReturnStatement& n);
            };

            Nodecl::NodeclBase get_new_mask_symbol(const Nodecl::NodeclBase& n,
                    const int masks_size,
                    const bool ref_type);

//            bool is_declared_in_scope(const scope_t *const  target_scope,
//                    const scope_t *const symbol_scope);
            bool is_declared_in_inner_scope(const Nodecl::NodeclBase& target_node,
                    const TL::Symbol& tl_symbol);

            bool is_all_one_mask(const Nodecl::NodeclBase& n);
            Nodecl::MaskLiteral get_all_one_mask(const int num_elements);

            Nodecl::NodeclBase get_proper_mask(const Nodecl::NodeclBase& mask);

            Nodecl::NodeclBase get_null_mask();

            TL::Type get_qualified_vector_to(TL::Type src_type,
                    const unsigned int vec_factor);

            std::string get_var_counter();

            bool class_type_can_be_vectorized(TL::Type);
            bool is_class_of_vector_fields(TL::Type type);
            TL::Type get_class_of_vector_fields_for_isa(TL::Type src_type,
                    const unsigned int vec_factor,
                    bool &is_new,
                    const VectorIsaDescriptor &vec_isa_desc);
            TL::Type get_class_of_vector_fields_for_isa(TL::Type src_type,
                    const unsigned int vec_factor,
                    const VectorIsaDescriptor &vec_isa_desc);

            Nodecl::NodeclBase get_if_mask_is_not_zero_nodecl(
                    const Nodecl::NodeclBase& mask,
                    const Nodecl::NodeclBase& then);
            Nodecl::MaskLiteral get_contiguous_mask_literal(
                    const int size,
                    const int num_active_lanes);

            Nodecl::List get_vector_offset_list(const int start_value,
                                                 const int increment,
                                                 const int vec_factor);

            const_value_t *get_vector_const_value(
                const TL::ObjectList<Nodecl::NodeclBase> &list);

            Nodecl::NodeclBase get_denormalized_ub(Nodecl::ForStatement for_statement);

            template <typename ScalarNode, typename Functor>
                ScalarNode make_scalar_binary_node(
                        const Nodecl::NodeclBase& lhs,
                        const Nodecl::NodeclBase& rhs,
                        const TL::Type& type,
                        const Functor const_operation)
                {
                    ScalarNode result = ScalarNode::make(lhs, rhs, type);

                    if (lhs.is_constant() && rhs.is_constant())
                    {
                        result.set_constant(const_operation(lhs.get_constant(),
                                    rhs.get_constant()));
                    }

                    return result;
                }

            template <typename VectorNode, typename Functor>
                VectorNode make_vector_binary_node(
                        const Nodecl::NodeclBase& lhs,
                        const Nodecl::NodeclBase& rhs,
                        const Nodecl::NodeclBase& mask,
                        const TL::Type& type,
                        const Functor const_operation)
                {
                    VectorNode result = VectorNode::make(lhs, rhs, mask, type);

                    if (lhs.is_constant() && rhs.is_constant())
                    {
                        result.set_constant(const_operation(lhs.get_constant(),
                                    rhs.get_constant()));
                    }

                    return result;
                }


            class RemovePrefetchIntrinsics : public Nodecl::ExhaustiveVisitor<void>
            {
                public:
                    RemovePrefetchIntrinsics(void) {};
                    void visit(const Nodecl::FunctionCall& node);
            };
            
            TL::Symbol get_subscripted_symbol(const Nodecl::NodeclBase& subscripted);
            Nodecl::NodeclBase get_scalar_memory_access(
                    const Nodecl::NodeclBase& n);
            Nodecl::NodeclBase get_vector_load_subscripted(
                    const Nodecl::VectorLoad& vectori_load);
            Nodecl::NodeclBase get_vector_load_subscript(
                    const Nodecl::VectorLoad& vector_load);

            objlist_nodecl_t get_nodecls_not_contained_in(
                    const objlist_nodecl_t& contained_list,
                    const objlist_nodecl_t& container_list);

            typedef std::map<TL::Symbol, TL::Symbol> class_of_vector_field_map_t;
            class_of_vector_field_map_t class_of_vector_fields_get_map_field(TL::Type class_of_vector);
        }
    }
}

#endif //TL_VECTORIZATION_UTILS_VERSIONING_HPP

