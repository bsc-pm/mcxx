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

#include "tl-vectorizer-visitor-expression.hpp"
#include "tl-vectorizer-utils.hpp"

namespace TL
{
    namespace Vectorization
    {
        VectorizerVisitorExpression::VectorizerVisitorExpression(
                VectorizerEnvironment& environment) :
            _environment(environment)
        {
        }

        void VectorizerVisitorExpression::visit(const Nodecl::Add& n)
        {
            walk(n.get_lhs());
            walk(n.get_rhs());

            if(Utils::is_all_one_mask(_environment._mask_list.back()))
            {
                const Nodecl::VectorAdd vector_add =
                    Nodecl::VectorAdd::make(
                            n.get_lhs().shallow_copy(),
                            n.get_rhs().shallow_copy(),
                            get_qualified_vector_to(n.get_type(), _environment._vector_length),
                            n.get_locus());

                n.replace(vector_add);
            }
            else
            {
                const Nodecl::VectorAddMask vector_add =
                    Nodecl::VectorAddMask::make(
                            n.get_lhs().shallow_copy(),
                            n.get_rhs().shallow_copy(),
                            _environment._mask_list.back().shallow_copy(),
                            get_qualified_vector_to(n.get_type(), _environment._vector_length),
                            n.get_locus());

                n.replace(vector_add);
            }
        }

        void VectorizerVisitorExpression::visit(const Nodecl::Minus& n)
        {
            walk(n.get_lhs());
            walk(n.get_rhs());

            if(Utils::is_all_one_mask(_environment._mask_list.back()))
            {
                const Nodecl::VectorMinus vector_minus =
                    Nodecl::VectorMinus::make(
                            n.get_lhs().shallow_copy(),
                            n.get_rhs().shallow_copy(),
                            get_qualified_vector_to(n.get_type(), _environment._vector_length),
                            n.get_locus());

                n.replace(vector_minus);
            }
            else
            {
                const Nodecl::VectorMinusMask vector_minus =
                    Nodecl::VectorMinusMask::make(
                            n.get_lhs().shallow_copy(),
                            n.get_rhs().shallow_copy(),
                            _environment._mask_list.back().shallow_copy(),
                            get_qualified_vector_to(n.get_type(), _environment._vector_length),
                            n.get_locus());

                n.replace(vector_minus);
            }
        }

        void VectorizerVisitorExpression::visit(const Nodecl::Mul& n)
        {
            walk(n.get_lhs());
            walk(n.get_rhs());

            if(Utils::is_all_one_mask(_environment._mask_list.back()))
            {
                const Nodecl::VectorMul vector_mul =
                    Nodecl::VectorMul::make(
                            n.get_lhs().shallow_copy(),
                            n.get_rhs().shallow_copy(),
                            get_qualified_vector_to(n.get_type(), _environment._vector_length),
                            n.get_locus());

                n.replace(vector_mul);
            }
            else
            {
                const Nodecl::VectorMulMask vector_mul =
                    Nodecl::VectorMulMask::make(
                            n.get_lhs().shallow_copy(),
                            n.get_rhs().shallow_copy(),
                            _environment._mask_list.back().shallow_copy(),
                            get_qualified_vector_to(n.get_type(), _environment._vector_length),
                            n.get_locus());

                n.replace(vector_mul);
            }
        }

        void VectorizerVisitorExpression::visit(const Nodecl::Div& n)
        {
            walk(n.get_lhs());
            walk(n.get_rhs());


            if(Utils::is_all_one_mask(_environment._mask_list.back()))
            {
                const Nodecl::VectorDiv vector_div =
                    Nodecl::VectorDiv::make(
                            n.get_lhs().shallow_copy(),
                            n.get_rhs().shallow_copy(),
                            get_qualified_vector_to(n.get_type(), _environment._vector_length),
                            n.get_locus());

                n.replace(vector_div);
            }
            else
            {
                const Nodecl::VectorDivMask vector_div =
                    Nodecl::VectorDivMask::make(
                            n.get_lhs().shallow_copy(),
                            n.get_rhs().shallow_copy(),
                            _environment._mask_list.back().shallow_copy(),
                            get_qualified_vector_to(n.get_type(), _environment._vector_length),
                            n.get_locus());

                n.replace(vector_div);
            }
        }

        void VectorizerVisitorExpression::visit(const Nodecl::Neg& n)
        {
            Nodecl::NodeclBase rhs = n.get_rhs();

            if (rhs.is<Nodecl::IntegerLiteral>() || // -1
                rhs.is<Nodecl::FloatingLiteral>())
            {
                const Nodecl::VectorPromotion vector_prom =
                    Nodecl::VectorPromotion::make(
                            n.shallow_copy(),
                            get_qualified_vector_to(n.get_type(), _environment._vector_length),
                            n.get_locus());

                n.replace(vector_prom);
            }
            else // -a
            {
                walk(rhs);

                const Nodecl::VectorNeg vector_neg =
                    Nodecl::VectorNeg::make(
                            n.get_rhs().shallow_copy(),
                            get_qualified_vector_to(n.get_type(), _environment._vector_length),
                            n.get_locus());

                n.replace(vector_neg);
            }
        }

        void VectorizerVisitorExpression::visit(const Nodecl::LowerThan& n)
        {
            walk(n.get_lhs());
            walk(n.get_rhs());

            const Nodecl::VectorLowerThan vector_lt =
                Nodecl::VectorLowerThan::make(
                        n.get_lhs().shallow_copy(),
                        n.get_rhs().shallow_copy(),
                        get_qualified_vector_to(n.get_type(), _environment._vector_length),
                        n.get_locus());

            n.replace(vector_lt);
        }

        void VectorizerVisitorExpression::visit(const Nodecl::LowerOrEqualThan& n)
        {
            walk(n.get_lhs());
            walk(n.get_rhs());

            const Nodecl::VectorLowerOrEqualThan vector_lt =
                Nodecl::VectorLowerOrEqualThan::make(
                        n.get_lhs().shallow_copy(),
                        n.get_rhs().shallow_copy(),
                        get_qualified_vector_to(n.get_type(), _environment._vector_length),
                        n.get_locus());

            n.replace(vector_lt);
        }

        void VectorizerVisitorExpression::visit(const Nodecl::GreaterThan& n)
        {
            walk(n.get_lhs());
            walk(n.get_rhs());

            const Nodecl::VectorGreaterThan vector_gt =
                Nodecl::VectorGreaterThan::make(
                        n.get_lhs().shallow_copy(),
                        n.get_rhs().shallow_copy(),
                        get_qualified_vector_to(n.get_type(), _environment._vector_length),
                        n.get_locus());

            n.replace(vector_gt);
        }

        void VectorizerVisitorExpression::visit(const Nodecl::GreaterOrEqualThan& n)
        {
            walk(n.get_lhs());
            walk(n.get_rhs());

            const Nodecl::VectorGreaterOrEqualThan vector_gt =
                Nodecl::VectorGreaterOrEqualThan::make(
                        n.get_lhs().shallow_copy(),
                        n.get_rhs().shallow_copy(),
                        get_qualified_vector_to(n.get_type(), _environment._vector_length),
                        n.get_locus());

            n.replace(vector_gt);
        }

        void VectorizerVisitorExpression::visit(const Nodecl::Equal& n)
        {
            walk(n.get_lhs());
            walk(n.get_rhs());

            const Nodecl::VectorEqual vector_eq =
                Nodecl::VectorEqual::make(
                        n.get_lhs().shallow_copy(),
                        n.get_rhs().shallow_copy(),
                        get_qualified_vector_to(n.get_type(), _environment._vector_length),
                        n.get_locus());

            n.replace(vector_eq);
        }

        void VectorizerVisitorExpression::visit(const Nodecl::Different& n)
        {
            walk(n.get_lhs());
            walk(n.get_rhs());

            const Nodecl::VectorDifferent vector_dif =
                Nodecl::VectorDifferent::make(
                        n.get_lhs().shallow_copy(),
                        n.get_rhs().shallow_copy(),
                        get_qualified_vector_to(n.get_type(), _environment._vector_length),
                        n.get_locus());

            n.replace(vector_dif);
        }

        void VectorizerVisitorExpression::visit(const Nodecl::BitwiseAnd& n)
        {
            walk(n.get_lhs());
            walk(n.get_rhs());

            if(Utils::is_all_one_mask(_environment._mask_list.back()))
            {
                const Nodecl::VectorBitwiseAnd vector_ba =
                    Nodecl::VectorBitwiseAnd::make(
                            n.get_lhs().shallow_copy(),
                            n.get_rhs().shallow_copy(),
                            get_qualified_vector_to(n.get_type(), _environment._vector_length),
                            n.get_locus());

                n.replace(vector_ba);
            }
            else
            {
                const Nodecl::VectorBitwiseAndMask vector_ba =
                    Nodecl::VectorBitwiseAndMask::make(
                            n.get_lhs().shallow_copy(),
                            n.get_rhs().shallow_copy(),
                            _environment._mask_list.back().shallow_copy(),
                            get_qualified_vector_to(n.get_type(), _environment._vector_length),
                            n.get_locus());

                n.replace(vector_ba);
            }

        }

        void VectorizerVisitorExpression::visit(const Nodecl::BitwiseOr& n)
        {
            walk(n.get_lhs());
            walk(n.get_rhs());

            if(Utils::is_all_one_mask(_environment._mask_list.back()))
            {
                const Nodecl::VectorBitwiseOr vector_bo =
                    Nodecl::VectorBitwiseOr::make(
                            n.get_lhs().shallow_copy(),
                            n.get_rhs().shallow_copy(),
                            get_qualified_vector_to(n.get_type(), _environment._vector_length),
                            n.get_locus());

                n.replace(vector_bo);
            }
            else
            {
                const Nodecl::VectorBitwiseOrMask vector_bo =
                    Nodecl::VectorBitwiseOrMask::make(
                            n.get_lhs().shallow_copy(),
                            n.get_rhs().shallow_copy(),
                            _environment._mask_list.back().shallow_copy(),
                            get_qualified_vector_to(n.get_type(), _environment._vector_length),
                            n.get_locus());

                n.replace(vector_bo);
            }
        }

        void VectorizerVisitorExpression::visit(const Nodecl::LogicalAnd& n)
        {
            walk(n.get_lhs());
            walk(n.get_rhs());

            const Nodecl::VectorLogicalAnd vector_lo =
                Nodecl::VectorLogicalAnd::make(
                        n.get_lhs().shallow_copy(),
                        n.get_rhs().shallow_copy(),
                        get_qualified_vector_to(n.get_type(), _environment._vector_length),
                        n.get_locus());

            n.replace(vector_lo);
        }

        void VectorizerVisitorExpression::visit(const Nodecl::LogicalOr& n)
        {
            walk(n.get_lhs());
            walk(n.get_rhs());

            const Nodecl::VectorLogicalOr vector_lo =
                Nodecl::VectorLogicalOr::make(
                        n.get_lhs().shallow_copy(),
                        n.get_rhs().shallow_copy(),
                        get_qualified_vector_to(n.get_type(), _environment._vector_length),
                        n.get_locus());

            n.replace(vector_lo);
        }

        void VectorizerVisitorExpression::visit(const Nodecl::ConditionalExpression& n)
        {
            Nodecl::NodeclBase condition = n.get_condition();

            walk(condition);

            Nodecl::NodeclBase prev_mask =
                _environment._mask_list.back();

            if(Utils::is_all_one_mask(prev_mask))
            {
                _environment._mask_list.push_back(condition);
                _environment._local_scope_list.push_back(n.get_true().retrieve_context());
                walk(n.get_true());
                _environment._mask_list.pop_back();
                _environment._local_scope_list.pop_back();

                Nodecl::VectorMaskNot neg_condition =
                    Nodecl::VectorMaskNot::make(condition,
                            condition.get_type(),
                            condition.get_locus());

                _environment._mask_list.push_back(neg_condition);
                _environment._local_scope_list.push_back(n.get_false().retrieve_context());
                walk(n.get_false());
                _environment._local_scope_list.pop_back();
                _environment._mask_list.pop_back();
            }
            else
            {
                TL::Scope scope = n.retrieve_context();
                // True Mask
                TL::Symbol true_mask_sym = scope.new_symbol("__mask_" + 
                        Vectorizer::_vectorizer->get_var_counter());
                true_mask_sym.get_internal_symbol()->kind = SK_VARIABLE;
                true_mask_sym.get_internal_symbol()->entity_specs.is_user_declared = 1;
                true_mask_sym.set_type(TL::Type::get_mask_type(_environment._mask_size));

                Nodecl::Symbol true_mask_nodecl_sym = true_mask_sym.make_nodecl(true, n.get_locus());
                Nodecl::NodeclBase true_mask_value = 
                    Nodecl::VectorMaskAnd::make(
                            prev_mask.shallow_copy(),
                            condition.shallow_copy(),
                            true_mask_sym.get_type(),
                            n.get_locus());

                Nodecl::ExpressionStatement true_mask_exp =
                    Nodecl::ExpressionStatement::make(
                            Nodecl::VectorMaskAssignment::make(true_mask_nodecl_sym, 
                                true_mask_value,
                                true_mask_sym.get_type(),
                                n.get_locus()));

                // Visit True
                _environment._mask_list.push_back(true_mask_nodecl_sym.shallow_copy());
                _environment._local_scope_list.push_back(n.get_true().retrieve_context());
                walk(n.get_true());
                _environment._local_scope_list.pop_back();
                _environment._mask_list.pop_back();

                n.prepend_sibling(true_mask_exp);

                    
                // False Mask
                TL::Symbol false_mask_sym = scope.new_symbol("__mask_" + 
                        Vectorizer::_vectorizer->get_var_counter());
                false_mask_sym.get_internal_symbol()->kind = SK_VARIABLE;
                false_mask_sym.get_internal_symbol()->entity_specs.is_user_declared = 1;
                false_mask_sym.set_type(TL::Type::get_mask_type(_environment._mask_size));

                Nodecl::Symbol false_mask_nodecl_sym = false_mask_sym.make_nodecl(true, n.get_locus());

                Nodecl::NodeclBase false_mask_value =
                    Nodecl::VectorMaskAnd2Not::make(
                            prev_mask.shallow_copy(),
                            condition.shallow_copy(),
                            false_mask_sym.get_type(),
                            n.get_locus());

                Nodecl::ExpressionStatement else_mask_exp =
                    Nodecl::ExpressionStatement::make(
                            Nodecl::VectorMaskAssignment::make(false_mask_nodecl_sym.shallow_copy(), 
                                false_mask_value,
                                false_mask_sym.get_type(),
                                n.get_locus()));

                // Visit False
                _environment._mask_list.push_back(false_mask_nodecl_sym);
                _environment._local_scope_list.push_back(n.get_false().retrieve_context());
                walk(n.get_false());
                _environment._local_scope_list.pop_back();
                _environment._mask_list.pop_back();

                n.prepend_sibling(else_mask_exp);
            }

            const Nodecl::VectorConditionalExpression vector_cond =
                Nodecl::VectorConditionalExpression::make(
                        condition.shallow_copy(),
                        n.get_true().shallow_copy(),
                        n.get_false().shallow_copy(),
                        get_qualified_vector_to(n.get_type(), _environment._vector_length),
                        n.get_locus());

            n.replace(vector_cond);
        }

        void VectorizerVisitorExpression::visit(const Nodecl::Assignment& n)
        {
            Nodecl::NodeclBase lhs = n.get_lhs();
            walk(n.get_rhs());

            // Computing new vector type
            TL::Type vector_type = n.get_type();
            
            vector_type = get_qualified_vector_to(vector_type, _environment._vector_length);

            if(lhs.is<Nodecl::ArraySubscript>())
            {
                // Vector Store
                if(Vectorizer::_analysis_info->is_adjacent_access(
                            _environment._analysis_scopes.back(),
                            lhs))
                {
                    TL::Type basic_type = lhs.get_type();
                    if (basic_type.is_lvalue_reference())
                    {
                        basic_type = basic_type.references_to();
                    }

                    // Aligned
                    if(Vectorizer::_analysis_info->is_simd_aligned_access(
                            _environment._analysis_scopes.back(),
                            lhs,
                            _environment._suitable_expr_list,
                            _environment._unroll_factor,
                            _environment._vector_length))
                    {
                        printf("VECTORIZER: Store access '%s' is ALIGNED\n",
                                lhs.prettyprint().c_str());

                        if(Utils::is_all_one_mask(_environment._mask_list.back()))
                        {
                            const Nodecl::VectorStore vector_store =
                                Nodecl::VectorStore::make(
                                        Nodecl::Reference::make(
                                            Nodecl::ParenthesizedExpression::make(
                                                lhs.shallow_copy(),
                                                basic_type,
                                                n.get_locus()),
                                            basic_type.get_pointer_to(),
                                            n.get_locus()),
                                        n.get_rhs().shallow_copy(),
                                        vector_type,
                                        n.get_locus());

                            n.replace(vector_store);
                        }
                        else
                        {
                            const Nodecl::VectorStoreMask vector_store =
                                Nodecl::VectorStoreMask::make(
                                        Nodecl::Reference::make(
                                            Nodecl::ParenthesizedExpression::make(
                                                lhs.shallow_copy(),
                                                basic_type,
                                                n.get_locus()),
                                            basic_type.get_pointer_to(),
                                            n.get_locus()),
                                        n.get_rhs().shallow_copy(),
                                        _environment._mask_list.back().shallow_copy(),
                                        vector_type,
                                        n.get_locus());

                            n.replace(vector_store);
                        }
                    }
                    else // Unaligned
                    {
                        printf("VECTORIZER: Store access '%s' is UNALIGNED\n",
                                lhs.prettyprint().c_str());

                        if(Utils::is_all_one_mask(_environment._mask_list.back()))
                        {
                            const Nodecl::UnalignedVectorStore vector_store =
                                Nodecl::UnalignedVectorStore::make(
                                        Nodecl::Reference::make(
                                            Nodecl::ParenthesizedExpression::make(
                                                lhs.shallow_copy(),
                                                basic_type,
                                                n.get_locus()),
                                            basic_type.get_pointer_to(),
                                            n.get_locus()),
                                        n.get_rhs().shallow_copy(),
                                        vector_type,
                                        n.get_locus());

                            n.replace(vector_store);
                        }
                        else
                        {
                            const Nodecl::UnalignedVectorStoreMask vector_store =
                                Nodecl::UnalignedVectorStoreMask::make(
                                        Nodecl::Reference::make(
                                            Nodecl::ParenthesizedExpression::make(
                                                lhs.shallow_copy(),
                                                basic_type,
                                                n.get_locus()),
                                            basic_type.get_pointer_to(),
                                            n.get_locus()),
                                        n.get_rhs().shallow_copy(),
                                        _environment._mask_list.back().shallow_copy(),
                                        vector_type,
                                        n.get_locus());

                            n.replace(vector_store);
                        }
                    } 
                }
                else // Vector Scatter
                {
                    const Nodecl::ArraySubscript lhs_array = lhs.as<Nodecl::ArraySubscript>();

                    const Nodecl::NodeclBase base = lhs_array.get_subscripted();
                    const Nodecl::List subscripts = lhs_array.get_subscripts().as<Nodecl::List>();

                    std::cerr << "Scatter: " << lhs_array.prettyprint() << "\n";

                    ERROR_CONDITION(subscripts.size() > 1,
                            "Vectorizer: Scatter on multidimensional array is not supported yet!", 0);

                    Nodecl::NodeclBase strides = *subscripts.begin();
                    walk(strides);

                    if(Utils::is_all_one_mask(_environment._mask_list.back()))
                    {
                        const Nodecl::VectorScatter vector_scatter =
                            Nodecl::VectorScatter::make(
                                    base.shallow_copy(),
                                    strides,
                                    n.get_rhs().shallow_copy(),
                                    vector_type,
                                    n.get_locus());

                        n.replace(vector_scatter);
                    }
                    else
                    {
                        const Nodecl::VectorScatterMask vector_scatter =
                            Nodecl::VectorScatterMask::make(
                                    base.shallow_copy(),
                                    strides,
                                    n.get_rhs().shallow_copy(),
                                    _environment._mask_list.back().shallow_copy(),
                                    vector_type,
                                    n.get_locus());

                        n.replace(vector_scatter);
                    }
                }
            }
            else // Register
            {
                walk(lhs);

                if(Utils::is_all_one_mask(_environment._mask_list.back()))
                {
                    const Nodecl::VectorAssignment vector_assignment =
                        Nodecl::VectorAssignment::make(
                                lhs.shallow_copy(),
                                n.get_rhs().shallow_copy(),
                                vector_type,
                                n.get_locus());

                    n.replace(vector_assignment);
                }
                else
                {
                    const Nodecl::VectorAssignmentMask vector_assignment =
                        Nodecl::VectorAssignmentMask::make(
                                lhs.shallow_copy(),
                                n.get_rhs().shallow_copy(),
                                _environment._mask_list.back().shallow_copy(),
                                vector_type,
                                n.get_locus());

                    n.replace(vector_assignment);
                }
            }
        }

        void VectorizerVisitorExpression::visit(const Nodecl::AddAssignment& n)
        {
            const Nodecl::Assignment assignment =
                Nodecl::Assignment::make(
                        n.get_lhs().shallow_copy(),
                        Nodecl::Add::make(
                            n.get_lhs().shallow_copy(),
                            n.get_rhs().shallow_copy(),
                            n.get_type(),
                            n.get_locus()),
                        n.get_type(),
                        n.get_locus());

            n.replace(assignment);

            // Visit standard assignment
            walk(n);
        }

        void VectorizerVisitorExpression::visit(const Nodecl::MinusAssignment& n)
        {
            const Nodecl::Assignment assignment =
                Nodecl::Assignment::make(
                        n.get_lhs().shallow_copy(),
                        Nodecl::Minus::make(
                            n.get_lhs().shallow_copy(),
                            n.get_rhs().shallow_copy(),
                            n.get_type(),
                            n.get_locus()),
                        n.get_type(),
                        n.get_locus());

            n.replace(assignment);

            // Visit standard assignment
            walk(n);
        }

        void VectorizerVisitorExpression::visit(const Nodecl::MulAssignment& n)
        {
            const Nodecl::Assignment assignment =
                Nodecl::Assignment::make(
                        n.get_lhs().shallow_copy(),
                        Nodecl::Mul::make(
                            n.get_lhs().shallow_copy(),
                            n.get_rhs().shallow_copy(),
                            n.get_type(),
                            n.get_locus()),
                        n.get_type(),
                        n.get_locus());

            n.replace(assignment);

            // Visit standard assignment
            walk(n);
        }

        void VectorizerVisitorExpression::visit(const Nodecl::DivAssignment& n)
        {
            const Nodecl::Assignment assignment =
                Nodecl::Assignment::make(
                        n.get_lhs().shallow_copy(),
                        Nodecl::Div::make(
                            n.get_lhs().shallow_copy(),
                            n.get_rhs().shallow_copy(),
                            n.get_type(),
                            n.get_locus()),
                        n.get_type(),
                        n.get_locus());

            n.replace(assignment);

            // Visit standard assignment
            walk(n);
        }

        void VectorizerVisitorExpression::visit(const Nodecl::Conversion& n)
        {
            walk(n.get_nest());

            const Nodecl::VectorConversion vector_conv =
                Nodecl::VectorConversion::make(
                        n.get_nest().shallow_copy(),
                        get_qualified_vector_to(n.get_type(), _environment._vector_length),
                        n.get_locus());

            n.replace(vector_conv);
        }

        void VectorizerVisitorExpression::visit(const Nodecl::Cast& n)
        {
            walk(n.get_rhs());

            const Nodecl::VectorConversion vector_conv =
                Nodecl::VectorConversion::make(
                        n.get_rhs().shallow_copy(),
                        get_qualified_vector_to(n.get_type(), _environment._vector_length),
                        n.get_locus());

            n.replace(vector_conv);
        }

        void VectorizerVisitorExpression::visit(const Nodecl::ArraySubscript& n)
        {
            // Computing new vector type
            TL::Type vector_type = n.get_type();
            if (vector_type.is_lvalue_reference())
            {
                vector_type = vector_type.references_to();
            }
            vector_type = get_qualified_vector_to(vector_type, _environment._vector_length);

            TL::Type basic_type = n.get_type();
            if (basic_type.is_lvalue_reference())
            {
                basic_type = basic_type.references_to();
            }

            // Vector Promotion from ArraySubscript
            if (!Vectorizer::_analysis_info->is_induction_variable_dependent_access(
                        _environment._analysis_scopes.back(),
                        n))
            {

                std::cerr << "No IV dependent access: " << n.prettyprint() << "\n";

                const Nodecl::VectorPromotion vector_prom =
                    Nodecl::VectorPromotion::make(
                            n.shallow_copy(),
                            vector_type,
                            n.get_locus());

                n.replace(vector_prom);
            }
            // Vector Load
            else if (Vectorizer::_analysis_info->is_adjacent_access(
                        _environment._analysis_scopes.back(),
                        n))
            {
                // Aligned
                if(Vectorizer::_analysis_info->is_simd_aligned_access(
                            _environment._analysis_scopes.back(),
                            n,
                            _environment._suitable_expr_list,
                            _environment._unroll_factor,
                            _environment._vector_length))
                {
                    printf("VECTORIZER: Load access '%s' is ALIGNED\n",
                            n.prettyprint().c_str());

                    if(Utils::is_all_one_mask(_environment._mask_list.back()))
                    {
                        const Nodecl::VectorLoad vector_load =
                            Nodecl::VectorLoad::make(
                                    Nodecl::Reference::make(
                                        Nodecl::ParenthesizedExpression::make(
                                            n.shallow_copy(),
                                            basic_type,
                                            n.get_locus()),
                                        basic_type.get_pointer_to(),
                                        n.get_locus()),
                                    vector_type,
                                    n.get_locus());

                        n.replace(vector_load);
                    }
                    else
                    {
                        const Nodecl::VectorLoadMask vector_load =
                            Nodecl::VectorLoadMask::make(
                                    Nodecl::Reference::make(
                                        Nodecl::ParenthesizedExpression::make(
                                            n.shallow_copy(),
                                            basic_type,
                                            n.get_locus()),
                                        basic_type.get_pointer_to(),
                                        n.get_locus()),
                                    _environment._mask_list.back().shallow_copy(),
                                    vector_type,
                                    n.get_locus());

                        n.replace(vector_load);
                    }
                }
                else // Unaligned
                {
                    printf("VECTORIZER: Load access '%s' is UNALIGNED\n",
                            n.prettyprint().c_str());

                    if(Utils::is_all_one_mask(_environment._mask_list.back()))
                    {
                        const Nodecl::UnalignedVectorLoad vector_load =
                            Nodecl::UnalignedVectorLoad::make(
                                    Nodecl::Reference::make(
                                        Nodecl::ParenthesizedExpression::make(
                                            n.shallow_copy(),
                                            basic_type,
                                            n.get_locus()),
                                        basic_type.get_pointer_to(),
                                        n.get_locus()),
                                    vector_type,
                                    n.get_locus());

                        n.replace(vector_load);
                    }
                    else
                    {
                        const Nodecl::UnalignedVectorLoadMask vector_load =
                            Nodecl::UnalignedVectorLoadMask::make(
                                    Nodecl::Reference::make(
                                        Nodecl::ParenthesizedExpression::make(
                                            n.shallow_copy(),
                                            basic_type,
                                            n.get_locus()),
                                        basic_type.get_pointer_to(),
                                        n.get_locus()),
                                    _environment._mask_list.back().shallow_copy(),
                                    vector_type,
                                    n.get_locus());

                        n.replace(vector_load);
                    }
                } 
            }
            else // Vector Gather
            {
                const Nodecl::NodeclBase base = n.get_subscripted();
                const Nodecl::List subscripts = n.get_subscripts().as<Nodecl::List>();

                ERROR_CONDITION(subscripts.size() > 1,
                    "Vectorizer: Gather on multidimensional array is not supported yet!", 0);

                std::cerr << "Gather: " << n.prettyprint() << "\n";

                Nodecl::NodeclBase strides = *subscripts.begin();
                walk(strides);

                if(Utils::is_all_one_mask(_environment._mask_list.back()))
                {
                    const Nodecl::VectorGather vector_gather =
                        Nodecl::VectorGather::make(
                                base.shallow_copy(),
                                strides,
                                vector_type,
                                n.get_locus());

                    n.replace(vector_gather);
                }
                else
                {
                    const Nodecl::VectorGatherMask vector_gather =
                        Nodecl::VectorGatherMask::make(
                                base.shallow_copy(),
                                strides,
                                _environment._mask_list.back().shallow_copy(),
                                vector_type,
                                n.get_locus());

                    n.replace(vector_gather);
                }
            }
        }

        void VectorizerVisitorExpression::visit(const Nodecl::FunctionCall& n)
        {
            Nodecl::NodeclBase called = n.get_called();
            ERROR_CONDITION(!called.is<Nodecl::Symbol>(),
                    "Vectorizer: %s found. This kind of function call is not supported yet",
                    ast_print_node_type(called.get_kind()));

            Nodecl::Symbol called_sym = called.as<Nodecl::Symbol>();

            // Vectorizing arguments
            walk(n.get_arguments());

            // Special functions
            if(Utils::is_all_one_mask(_environment._mask_list.back()))
            {
                if (called_sym.get_symbol().get_name() == "fabsf")
                {
                    const Nodecl::VectorFabs vector_fabs_call =
                        Nodecl::VectorFabs::make(
                                n.get_arguments().as<Nodecl::List>().front().shallow_copy(),
                                get_qualified_vector_to(n.get_type(), _environment._vector_length),
                                n.get_locus());

                    n.replace(vector_fabs_call);
                }
                else //Common functions
                {
                    // Get the best vector version of the function available
                    Nodecl::NodeclBase best_version =
                        TL::Vectorization::Vectorizer::_function_versioning.get_best_version(
                                called_sym.get_symbol().get_name(), 
                                _environment._device, 
                                _environment._vector_length, 
                                _environment._target_type,
                                false);

                    ERROR_CONDITION(best_version.is_null(), "Vectorizer: the best vector function for '%s' is null",
                            called_sym.get_symbol().get_name().c_str());

                    // Create new called symbol
                    Nodecl::Symbol new_called;
                    if (best_version.is<Nodecl::FunctionCode>())
                    {
                        new_called = best_version.as<Nodecl::FunctionCode>().get_symbol().
                            make_nodecl(true, n.get_locus());
                    }
                    else if (best_version.is<Nodecl::Symbol>())
                    {
                        new_called = best_version.as<Nodecl::Symbol>().get_symbol().
                            make_nodecl(true, n.get_locus());
                    }
                    else
                    {
                        running_error("Vectorizer: %s found as vector function version in function versioning.",
                                ast_print_node_type(best_version.get_kind()));
                    }

                    const Nodecl::VectorFunctionCall vector_function_call =
                        Nodecl::VectorFunctionCall::make(
                                Nodecl::FunctionCall::make(
                                    new_called,
                                    n.get_arguments().shallow_copy(),
                                    n.get_alternate_name().shallow_copy(),
                                    n.get_function_form().shallow_copy(),
                                    get_qualified_vector_to(n.get_type(), _environment._vector_length),
                                    n.get_locus()),
                                called_sym.shallow_copy(),
                                get_qualified_vector_to(n.get_type(), _environment._vector_length),
                                n.get_locus());

                    n.replace(vector_function_call);
                }
            }
            else
            {

                if (called_sym.get_symbol().get_name() == "fabsf")
                {
                    const Nodecl::VectorFabsMask vector_fabs_call =
                        Nodecl::VectorFabsMask::make(
                                n.get_arguments().as<Nodecl::List>().front().shallow_copy(),
                                _environment._mask_list.back().shallow_copy(),
                                get_qualified_vector_to(n.get_type(), _environment._vector_length),
                                n.get_locus());

                    n.replace(vector_fabs_call);
                }
                else //Common functions
                {
                    // Get the best vector version of the function available
                    Nodecl::NodeclBase best_version =
                        TL::Vectorization::Vectorizer::_function_versioning.get_best_version(
                                called_sym.get_symbol().get_name(), 
                                _environment._device, 
                                _environment._vector_length, 
                                _environment._target_type,
                                true);

                    ERROR_CONDITION(best_version.is_null(), "Vectorizer: the best vector function for '%s' is null",
                            called_sym.get_symbol().get_name().c_str());

                    // Create new called symbol
                    Nodecl::Symbol new_called;
                    if (best_version.is<Nodecl::FunctionCode>())
                    {
                        new_called = best_version.as<Nodecl::FunctionCode>().get_symbol().
                            make_nodecl(true, n.get_locus());
                    }
                    else if (best_version.is<Nodecl::Symbol>())
                    {
                        new_called = best_version.as<Nodecl::Symbol>().get_symbol().
                            make_nodecl(true, n.get_locus());
                    }
                    else
                    {
                        running_error("Vectorizer: %s found as vector function version in function versioning.",
                                ast_print_node_type(best_version.get_kind()));
                    }

                    const Nodecl::VectorFunctionCallMask vector_function_call =
                        Nodecl::VectorFunctionCallMask::make(
                                Nodecl::FunctionCall::make(
                                    new_called,
                                    n.get_arguments().shallow_copy(),
                                    n.get_alternate_name().shallow_copy(),
                                    n.get_function_form().shallow_copy(),
                                    get_qualified_vector_to(n.get_type(), _environment._vector_length),
                                    n.get_locus()),
                                called_sym.shallow_copy(),
                                _environment._mask_list.back().shallow_copy(),
                                get_qualified_vector_to(n.get_type(), _environment._vector_length),
                                n.get_locus());

                    n.replace(vector_function_call);
                }
            }
        }

        void VectorizerVisitorExpression::visit(const Nodecl::Symbol& n)
        {
            TL::Type sym_type = n.get_type();
            TL::Symbol tl_sym = n.get_symbol();
            TL::Type tl_sym_type = tl_sym.get_type();

            //std::cerr << "scalar_type: " << n.prettyprint() << std::endl;

            if (!sym_type.is_vector())
            {
               // Vectorize BASIC induction variable
                if (Vectorizer::_analysis_info->is_basic_induction_variable(
                            _environment._analysis_scopes.back(),
                            n))
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr,"VECTORIZER: '%s' is IV and will be PROMOTED with OFFSET\n",
                                n.prettyprint().c_str()); 
                    }

                    // Computing IV offset {0, 1, 2, 3}
                    TL::ObjectList<Nodecl::NodeclBase> literal_list;

                    const_value_t *ind_var_increment = Vectorizer::_analysis_info->get_induction_variable_increment(
                            _environment._analysis_scopes.back(), n);
                    
                    const_value_t *i = const_value_get_zero(4, 0);
                    for(unsigned int j = 0;
                            j < _environment._unroll_factor;
                            i = const_value_add(i, ind_var_increment), j++)
                    {
                        literal_list.prepend(const_value_to_nodecl(i));
                    }

                    Nodecl::List offset = Nodecl::List::make(literal_list);

                    // IV cannot be a reference
                    TL::Type ind_var_type = get_qualified_vector_to(n.get_type(), _environment._vector_length).no_ref();

                    TL::Type offset_type = ind_var_type;
                    Nodecl::ParenthesizedExpression vector_induction_var =
                        Nodecl::ParenthesizedExpression::make(
                                Nodecl::VectorAdd::make(
                                    Nodecl::VectorPromotion::make(
                                        n.shallow_copy(),
                                        ind_var_type,
                                        n.get_locus()),
                                    Nodecl::VectorLiteral::make(
                                        offset,
                                        offset_type,
                                        n.get_locus()),
                                    get_qualified_vector_to(n.get_type(), _environment._vector_length),
                                    n.get_locus()),
                                get_qualified_vector_to(n.get_type(), _environment._vector_length),
                                n.get_locus());

                    n.replace(vector_induction_var);
                }
                // Vectorize symbols declared in the SIMD scope
                else if (Utils::is_declared_in_scope(
                            _environment._local_scope_list.back().get_decl_context().current_scope,
                            n.get_symbol().get_scope().get_decl_context().current_scope))
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr,"VECTORIZER: '%s' is DECLARED INSIDE OF THE SIMD SCOPE. Its TYPE will be vectorized\n", 
                                n.prettyprint().c_str()); 
                    }

                    //std::cerr << "NS scalar_type: " << n.prettyprint() << std::endl;

                    //TL::Symbol
                    if (tl_sym_type.is_scalar_type())
                    {
                        //std::cerr << "TS scalar_type: " << n.prettyprint() << std::endl;
                        tl_sym.set_type(get_qualified_vector_to(tl_sym_type, _environment._vector_length));
                        tl_sym_type = tl_sym.get_type();
                    }

                    //Nodecl::Symbol
                    Nodecl::Symbol new_sym =
                        Nodecl::Symbol::make(tl_sym,
                                n.get_locus());

                    new_sym.set_type(tl_sym_type.get_lvalue_reference_to());

                    n.replace(new_sym);
                }
                // Non local Nodecl::Symbol with scalar type whose TL::Symbol has vector_type
                else if(tl_sym_type.is_vector())
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr,"VECTORIZER: '%s' is a scalar NON-LOCAL Nodecl::Symbol whose TL::Symbol has vector type. Its Nodecl TYPE will be vectorized\n", 
                                n.prettyprint().c_str()); 
                    }

                    //Nodecl::Symbol
                    Nodecl::Symbol new_sym =
                        Nodecl::Symbol::make(tl_sym,
                                n.get_locus());

                    new_sym.set_type(tl_sym_type.get_lvalue_reference_to());

                    n.replace(new_sym);
                }
                // Vectorize constants
                else if (Vectorizer::_analysis_info->is_constant(
                            _environment._analysis_scopes.back(),
                            n))
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr,"VECTORIZER: '%s' is CONSTANT and will be PROMOTED to vector\n", 
                                n.prettyprint().c_str()); 
                    }

                    const Nodecl::VectorPromotion vector_prom =
                        Nodecl::VectorPromotion::make(
                                n.shallow_copy(),
                                get_qualified_vector_to(sym_type, _environment._vector_length),
                                n.get_locus());

                    n.replace(vector_prom);
                }
                else if(_environment._reduction_list != NULL)
                {
                    if(_environment._reduction_list->contains(tl_sym))
                    {
                        // If symbol is in the map
                        Nodecl::Symbol new_red_symbol;

                        std::map<TL::Symbol, TL::Symbol>::iterator it =
                            _environment._new_external_vector_symbol_map->find(tl_sym);

                        new_red_symbol = it->second.make_nodecl(true, n.get_locus());

                        n.replace(new_red_symbol);
                    }
                }
                else
                {
                    //TODO: If you are from outside of the loop -> Vector local copy.
                    running_error("Vectorizer: Loop is not vectorizable. '%s' is not IV, Constant, Local, Reduction or LastPrivate.",
                            n.get_symbol().get_name().c_str());
                }
            }
        }

        void VectorizerVisitorExpression::visit(const Nodecl::IntegerLiteral& n)
        {
            const Nodecl::VectorPromotion vector_prom =
                Nodecl::VectorPromotion::make(
                        n.shallow_copy(),
                        get_qualified_vector_to(n.get_type(), _environment._vector_length),
                        n.get_locus());

            n.replace(vector_prom);
        }

        void VectorizerVisitorExpression::visit(const Nodecl::FloatingLiteral& n)
        {
            const Nodecl::VectorPromotion vector_prom =
                Nodecl::VectorPromotion::make(
                        n.shallow_copy(),
                        get_qualified_vector_to(n.get_type(), _environment._vector_length),
                        n.get_locus());

            n.replace(vector_prom);
        }

        void VectorizerVisitorExpression::visit(const Nodecl::Reference& n)
        {
            walk(n.get_rhs());

            const Nodecl::Reference reference =
                Nodecl::Reference::make(
                        n.get_rhs().shallow_copy(),
                        get_qualified_vector_to(n.get_type(), _environment._vector_length),
                        n.get_locus());

            n.replace(reference);
        } 

        Nodecl::NodeclVisitor<void>::Ret VectorizerVisitorExpression::unhandled_node(
                const Nodecl::NodeclBase& n)
        {
            std::cerr << "Vectorizer: Unknown 'Expression' node "
                << ast_print_node_type(n.get_kind())
                //<< "(" << n.prettyprint() << ")"
                << " at " << n.get_locus()
                << std::endl;

            return Ret();
        }
    }
}
