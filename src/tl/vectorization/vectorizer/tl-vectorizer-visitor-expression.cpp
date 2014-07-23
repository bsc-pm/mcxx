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

//#include "tl-vectorizer-gather-scatter-info.hpp"
#include "tl-vectorization-utils.hpp"
#include "tl-vectorization-analysis-interface.hpp"

#include "cxx-cexpr.h"
#include "tl-nodecl-utils.hpp"


namespace TL
{
namespace Vectorization
{
    VectorizerVisitorExpression::VectorizerVisitorExpression(
            VectorizerEnvironment& environment,
            const bool cache_enabled) :
        _environment(environment), _cache_enabled(cache_enabled)
    {
    }

    void VectorizerVisitorExpression::symbol_type_promotion(
            const Nodecl::Symbol& n)
    {
        TL::Symbol tl_sym = n.get_symbol();
        TL::Type tl_sym_type = tl_sym.get_type().no_ref();
        TL::Type vector_type;

        //TL::Symbol
        if (tl_sym_type.is_mask())
        {
            vector_type = tl_sym_type;

            VECTORIZATION_DEBUG()
            {
                fprintf(stderr,"VECTORIZER: '%s' mask type vectorization "\
                        "(size %d)\n", n.prettyprint().c_str(),
                       vector_type.get_mask_num_elements());
            }
        }
        else if (tl_sym_type.is_scalar_type())
        {
            vector_type = Utils::get_qualified_vector_to(tl_sym_type,
                    _environment._vectorization_factor);

            VECTORIZATION_DEBUG()
            {
                fprintf(stderr,"VECTORIZER: TL::Type promotion '%s' from '%s'"\
                       " to '%s'\n", n.prettyprint().c_str(),
                       tl_sym_type.get_simple_declaration(
                           n.retrieve_context(), "").c_str(),
                       vector_type.get_simple_declaration(
                           n.retrieve_context(), "").c_str());
            }

            tl_sym.set_type(vector_type);
            tl_sym_type = tl_sym.get_type();
        }

        //Nodecl::Symbol
        Nodecl::Symbol new_sym = Nodecl::Symbol::make(tl_sym, n.get_locus());
        new_sym.set_type(tl_sym_type.get_lvalue_reference_to());

        n.replace(new_sym);
    }

    bool VectorizerVisitorExpression::process_fmul_op(
            const Nodecl::NodeclBase&  n)
    {
        Nodecl::Add binary_op = n.as<Nodecl::Add>();

        Nodecl::NodeclBase lhs = binary_op.get_lhs();
        Nodecl::NodeclBase rhs = binary_op.get_rhs();
        Nodecl::NodeclBase mask = Utils::get_proper_mask(
                _environment._mask_list.back());

        if (lhs.is<Nodecl::Mul>())
        {
            /*
               if (n.is<Nodecl::Add>())
               {
               Nodecl::Mul mul = lhs.as<Nodecl::Mul>();

               walk(rhs);
               walk(mul.get_lhs());
               walk(mul.get_rhs());

               Nodecl::VectorFmadd fmadd =
               Nodecl::VectorFmadd::make(mul.get_lhs().shallow_copy(),
               mul.get_rhs().shallow_copy(),
               rhs.shallow_copy(),
               mask,
               n.get_type(),
               n.get_locus());

               std::cerr << "FMADD detected" << std::endl;

               n.replace(fmadd);

               return true;
               }
               else if (n.is<Nodecl::Minus>())
               {
            // TODO
            }
             */
        }
        else if (rhs.is<Nodecl::Mul>())
        {
            if (n.is<Nodecl::Add>())
            {
                /*
                   Nodecl::Mul mul = rhs.as<Nodecl::Mul>();

                   walk(lhs);
                   walk(mul.get_lhs());
                   walk(mul.get_rhs());

                   Nodecl::VectorFmadd fmadd =
                   Nodecl::VectorFmadd::make(mul.get_lhs().shallow_copy(),
                   mul.get_rhs().shallow_copy(),
                   lhs.shallow_copy(),
                   mask,
                   n.get_type(),
                   n.get_locus());

                   std::cerr << "FMADD detected" << std::endl;

                   n.replace(fmadd);

                   return true;
                 */
            }
            else if (n.is<Nodecl::Minus>())
            {
                // TODO
            }
        }

        return false;
    }

    template <typename ScalarNode, typename VectorNode>
    void VectorizerVisitorExpression::visit_binary_op(const ScalarNode& n)
    {
        if(!((n.template is<Nodecl::Add>() || n.template is<Nodecl::Minus>()) &&
                    process_fmul_op(n)))
        {
            Nodecl::NodeclBase mask = Utils::get_proper_mask(
                    _environment._mask_list.back());

            Nodecl::NodeclBase lhs = n.get_lhs();
            Nodecl::NodeclBase rhs = n.get_rhs();

            walk(lhs);
            walk(rhs);

            VectorNode vector_node =
                VectorNode::make(
                        lhs.shallow_copy(),
                        rhs.shallow_copy(),
                        mask,
                        Utils::get_qualified_vector_to(n.get_type(),
                            _environment._vectorization_factor),
                        n.get_locus());

            if (n.is_constant())
                vector_node.set_constant(
                        const_value_make_vector_from_scalar(
                            _environment._vectorization_factor,
                            n.get_constant()));

            n.replace(vector_node);
        }
    }

    void VectorizerVisitorExpression::visit(const Nodecl::Add& n)
    {
        visit_binary_op<Nodecl::Add, Nodecl::VectorAdd>(n);
    }

    void VectorizerVisitorExpression::visit(const Nodecl::Minus& n)
    {
        visit_binary_op<Nodecl::Minus, Nodecl::VectorMinus>(n);
    }

    void VectorizerVisitorExpression::visit(const Nodecl::Mul& n)
    {
        visit_binary_op<Nodecl::Mul, Nodecl::VectorMul>(n);
    }

    void VectorizerVisitorExpression::visit(const Nodecl::Div& n)
    {
        visit_binary_op<Nodecl::Div, Nodecl::VectorDiv>(n);
    }

    void VectorizerVisitorExpression::visit(const Nodecl::Mod& n)
    {
        visit_binary_op<Nodecl::Mod, Nodecl::VectorMod>(n);
    }

    void VectorizerVisitorExpression::visit(const Nodecl::Neg& n)
    {
        Nodecl::NodeclBase mask = Utils::get_proper_mask(
                _environment._mask_list.back());

        Nodecl::NodeclBase rhs = n.get_rhs();

        if (rhs.is<Nodecl::IntegerLiteral>() || // -1
                rhs.is<Nodecl::FloatingLiteral>())
        {
            Nodecl::VectorPromotion vector_prom =
                Nodecl::VectorPromotion::make(
                        n.shallow_copy(),
                        mask,
                        Utils::get_qualified_vector_to(n.get_type(),
                            _environment._vectorization_factor),
                        n.get_locus());

            vector_prom.set_constant(const_value_make_vector_from_scalar(
                        _environment._vectorization_factor,
                        n.get_constant()));

            n.replace(vector_prom);
        }
        else // -a
        {
            walk(rhs);

            Nodecl::VectorNeg vector_neg =
                Nodecl::VectorNeg::make(
                        n.get_rhs().shallow_copy(),
                        mask,
                        Utils::get_qualified_vector_to(n.get_type(),
                            _environment._vectorization_factor),
                        n.get_locus());

            if(n.is_constant())
                vector_neg.set_constant(const_value_make_vector_from_scalar(
                            _environment._vectorization_factor,
                            n.get_constant()));

            n.replace(vector_neg);
        }
    }

    void VectorizerVisitorExpression::visit(const Nodecl::LowerThan& n)
    {
        visit_binary_op<Nodecl::LowerThan, Nodecl::VectorLowerThan>(n);
    }

    void VectorizerVisitorExpression::visit(const Nodecl::LowerOrEqualThan& n)
    {
        visit_binary_op<Nodecl::LowerOrEqualThan,
            Nodecl::VectorLowerOrEqualThan>(n);
    }

    void VectorizerVisitorExpression::visit(const Nodecl::GreaterThan& n)
    {
        visit_binary_op<Nodecl::GreaterThan, Nodecl::VectorGreaterThan>(n);
    }

    void VectorizerVisitorExpression::visit(const Nodecl::GreaterOrEqualThan& n)
    {
        visit_binary_op<Nodecl::GreaterOrEqualThan,
            Nodecl::VectorGreaterOrEqualThan>(n);
    }

    void VectorizerVisitorExpression::visit(const Nodecl::Equal& n)
    {
        visit_binary_op<Nodecl::Equal, Nodecl::VectorEqual>(n);
    }

    void VectorizerVisitorExpression::visit(const Nodecl::Different& n)
    {
        visit_binary_op<Nodecl::Different, Nodecl::VectorDifferent>(n);
    }

    void VectorizerVisitorExpression::visit(const Nodecl::BitwiseAnd& n)
    {
        visit_binary_op<Nodecl::BitwiseAnd, Nodecl::VectorBitwiseAnd>(n);
    }

    void VectorizerVisitorExpression::visit(const Nodecl::BitwiseOr& n)
    {
        visit_binary_op<Nodecl::BitwiseOr, Nodecl::VectorBitwiseOr>(n);
    }

    void VectorizerVisitorExpression::visit(const Nodecl::BitwiseShl& n)
    {
        visit_binary_op<Nodecl::BitwiseShl, Nodecl::VectorBitwiseShl>(n);
    }

    void VectorizerVisitorExpression::visit(const Nodecl::ArithmeticShr& n)
    {
        visit_binary_op<Nodecl::ArithmeticShr, Nodecl::VectorArithmeticShr>(n);
    }

    void VectorizerVisitorExpression::visit(const Nodecl::BitwiseShr& n)
    {
        visit_binary_op<Nodecl::BitwiseShr, Nodecl::VectorBitwiseShr>(n);
    }

    void VectorizerVisitorExpression::visit(const Nodecl::LogicalAnd& n)
    {
        visit_binary_op<Nodecl::LogicalAnd, Nodecl::VectorLogicalAnd>(n);
    }

    void VectorizerVisitorExpression::visit(const Nodecl::LogicalOr& n)
    {
        visit_binary_op<Nodecl::LogicalOr, Nodecl::VectorLogicalOr>(n);
    }

    void VectorizerVisitorExpression::visit(
            const Nodecl::ConditionalExpression& n)
    {
        Nodecl::NodeclBase condition = n.get_condition();

        walk(condition);

        if(_environment._support_masking)
        {
            Nodecl::NodeclBase prev_mask =
                _environment._mask_list.back();

            if(Utils::is_all_one_mask(prev_mask))
            {
                // ConditionalExpression doesn't allow new contexts 
                _environment._mask_list.push_back(condition);
                walk(n.get_true());
                _environment._mask_list.pop_back();

                Nodecl::VectorMaskNot neg_condition =
                    Nodecl::VectorMaskNot::make(condition,
                            condition.get_type(),
                            condition.get_locus());

                // ConditionalExpression doesn't allow new contexts 
                _environment._mask_list.push_back(neg_condition);
                walk(n.get_false());
                _environment._mask_list.pop_back();
            }
            else
            {
                // True Mask
                Nodecl::NodeclBase true_mask_nodecl_sym = 
                    Utils::get_new_mask_symbol(
                            _environment._analysis_simd_scope,
                            _environment._vectorization_factor,
                            true /*ref_type*/);

                Nodecl::NodeclBase true_mask_value =
                    Nodecl::VectorMaskAnd::make(
                            prev_mask.shallow_copy(),
                            condition.shallow_copy(),
                            true_mask_nodecl_sym.get_type().no_ref(),
                            n.get_locus());

                Nodecl::ExpressionStatement true_mask_exp =
                    Nodecl::ExpressionStatement::make(
                            Nodecl::VectorMaskAssignment::make(
                                true_mask_nodecl_sym,
                                true_mask_value,
                                true_mask_nodecl_sym.get_type().no_ref(),
                                n.get_locus()));

                // Visit True
                _environment._mask_list.push_back(
                        true_mask_nodecl_sym.shallow_copy());
                walk(n.get_true());
                _environment._mask_list.pop_back();

                n.prepend_sibling(true_mask_exp);


                // False Mask
                Nodecl::NodeclBase false_mask_nodecl_sym =
                    Utils::get_new_mask_symbol(
                            _environment._analysis_simd_scope, 
                            _environment._vectorization_factor,
                            true /*ref_type*/);

                Nodecl::NodeclBase false_mask_value =
                    Nodecl::VectorMaskAnd2Not::make(
                            prev_mask.shallow_copy(),
                            condition.shallow_copy(),
                            false_mask_nodecl_sym.get_type().no_ref(),
                            n.get_locus());

                Nodecl::ExpressionStatement else_mask_exp =
                    Nodecl::ExpressionStatement::make(
                            Nodecl::VectorMaskAssignment::make(
                                false_mask_nodecl_sym.shallow_copy(),
                                false_mask_value,
                                false_mask_nodecl_sym.get_type().no_ref(),
                                n.get_locus()));

                // Visit False
                _environment._mask_list.push_back(false_mask_nodecl_sym);
                walk(n.get_false());
                _environment._mask_list.pop_back();

                n.prepend_sibling(else_mask_exp);
            }
        }
        // NO MASKING SUPPORT
        else
        {
            walk(n.get_true());
            walk(n.get_false());
        }

        const Nodecl::VectorConditionalExpression vector_cond =
            Nodecl::VectorConditionalExpression::make(
                    condition.shallow_copy(),
                    n.get_true().shallow_copy(),
                    n.get_false().shallow_copy(),
                    Utils::get_qualified_vector_to(n.get_type(),
                        _environment._vectorization_factor),
                    n.get_locus());

        n.replace(vector_cond);
    }

    void VectorizerVisitorExpression::visit(const Nodecl::Assignment& n)
    {
        TL::Type assignment_type = n.get_type().no_ref();
        Nodecl::NodeclBase lhs = n.get_lhs();
        Nodecl::NodeclBase rhs = n.get_rhs();
        Nodecl::NodeclBase mask = Utils::get_proper_mask(
                _environment._mask_list.back());

        /*
        bool has_vector_type = false;
        // Look for vector types in the assignment 
        const objlist_nodecl_t mem_accesses = 
            Nodecl::Utils::get_all_memory_accesses(n);

        for(objlist_nodecl_t::const_iterator it = mem_accesses.begin();
                it != mem_accesses.end();
                it++)
        {
            if ((it->get_type().is_vector()) || 
                    (it->is<Nodecl::Symbol>() && 
                     it->as<Nodecl::Symbol>().get_symbol().get_type().is_vector()))
            {
                has_vector_type = true;
                break;
            }
        }
        */

        // If lhs or rhs aren't uniform, vectorize
        if((lhs.is<Nodecl::Symbol>() && 
                    lhs.as<Nodecl::Symbol>().get_symbol().get_type().is_vector()) ||
                //TODO: is_uniform on lhs won't be eventually necessary
                !VectorizationAnalysisInterface::_vectorizer_analysis->
                is_uniform(_environment._analysis_simd_scope, lhs, lhs) ||
                !VectorizationAnalysisInterface::_vectorizer_analysis->
                is_uniform(_environment._analysis_simd_scope, rhs, rhs))
        {
            // Computing new vector type
            TL::Type vector_type = Utils::get_qualified_vector_to(assignment_type,
                    _environment._vectorization_factor);

            // IV vectorization: i = i + 3 --> i = i + (vectorization_factor * 3)
            if(VectorizationAnalysisInterface::_vectorizer_analysis->
                    is_linear(_environment._analysis_simd_scope, lhs))
            {
                VECTORIZATION_DEBUG()
                {
                    fprintf(stderr, "VECTORIZER: Vectorizing linear update '%s'\n",
                            lhs.prettyprint().c_str());
                }

                if (rhs.is<Nodecl::Add>())
                {
                    Nodecl::Add rhs_add = rhs.as<Nodecl::Add>();

                    Nodecl::NodeclBase add_lhs = rhs_add.get_lhs();
                    Nodecl::NodeclBase add_lhs_no_conv = add_lhs.no_conv();
                    Nodecl::NodeclBase add_rhs = rhs_add.get_rhs();
                    Nodecl::NodeclBase add_rhs_no_conv = add_rhs.no_conv();

                    // = 3 + i | n + i
                    if (Nodecl::Utils::structurally_equal_nodecls(lhs, add_rhs_no_conv))
                    {
                        Nodecl::Add new_add = Nodecl::Add::make(
                                Nodecl::Mul::make(
                                    add_lhs, //Do not shallow copy!
                                    Nodecl::IntegerLiteral::make(
                                        TL::Type::get_int_type(),
                                        const_value_get_signed_int(
                                            _environment._vectorization_factor),
                                        n.get_locus()),
                                    rhs_add.get_type(),
                                    n.get_locus()),
                                add_rhs,    //Do not shallow copy!
                                rhs_add.get_type(),
                                n.get_locus());

                        rhs_add.replace(new_add);
                    }
                    // = i + n
                    else if (Nodecl::Utils::structurally_equal_nodecls(lhs, add_lhs_no_conv))
                    {
                        Nodecl::Add new_add = Nodecl::Add::make(
                                Nodecl::Mul::make(
                                    add_rhs, //Do not shallow copy!
                                    Nodecl::IntegerLiteral::make(
                                        TL::Type::get_int_type(),
                                        const_value_get_signed_int(
                                            _environment._vectorization_factor),
                                        n.get_locus()),
                                    rhs_add.get_type(),
                                    n.get_locus()),
                                add_lhs,    //Do not shallow copy!
                                rhs_add.get_type(),
                                n.get_locus());

                        rhs_add.replace(new_add);
                    }
                    else
                    {
                        running_error("Vectorizer: This linear update is not supported yet"\
                                "(%s).", n.prettyprint().c_str());
                    }

                    /*
                       Nodecl::NodeclBase step = VectorizationAnalysisInterface::
                       _vectorizer_analysis->get_induction_variable_increment(
                       _environment._analysis_simd_scope, lhs);

                    //VectorizationAnalysisInterface::_vectorizer_analysis->get_induction_variable_increment(
                    //    _environment._analysis_simd_scope, lhs);

                    objlist_nodecl_t step_list =
                    Nodecl::Utils::get_all_nodecl_occurrences(step, lhs);

                    for(objlist_nodecl_t::iterator it = step_list.begin();
                    it != step_list.end();
                    it ++)
                    {
                    Nodecl::Mul new_step =
                    Nodecl::Mul::make(
                    it->shallow_copy(),
                    Nodecl::IntegerLiteral::make(
                    TL::Type::get_int_type(),
                    const_value_get_signed_int(
                    _environment._vectorization_factor),
                    it->get_locus()),
                    TL::Type::get_int_type(),
                    it->get_locus());

                    it->replace(new_step);
                    }
                     */
                }
            }
            else if(lhs.is<Nodecl::ArraySubscript>())
            {
                Nodecl::NodeclBase subscripted = lhs.as<Nodecl::ArraySubscript>().
                    get_subscripted().no_conv();

                ERROR_CONDITION(lhs.as<Nodecl::ArraySubscript>().
                        get_subscripts().as<Nodecl::List>().size() > 1,
                        "Vectorizer: ArraySubscript has not been linearized: %s",
                        lhs.prettyprint().c_str());

                if (subscripted.is<Nodecl::Cast>())
                {
                    subscripted = Nodecl::Utils::advance_conversions(
                            subscripted.as<Nodecl::Cast>().get_rhs());
                }

                ERROR_CONDITION(!subscripted.is<Nodecl::Symbol>(),
                        "Vectorizer: ArraySubscript form not supported yet: %s",
                        lhs.prettyprint().c_str());

                Nodecl::Symbol subscripted_symbol = subscripted.as<Nodecl::Symbol>();

                walk(rhs);

                // Vector Store
                // Constant ArraySubscript, nothing to do
                if (VectorizationAnalysisInterface::_vectorizer_analysis->
                        is_uniform(_environment._analysis_simd_scope,
                            lhs, lhs))
                {
                    std::cerr << "Vectorizer: Constant store: "
                        << lhs.prettyprint()
                        << std::endl;

                    running_error("Vectorizer: Extract operation is not "\
                            "supported yet (%s).", lhs.prettyprint().c_str());

                }
                // ArraySubscript indexed by nested IV, nothing to do
                /*
                   else if (VectorizationAnalysisInterface::_vectorizer_analysis->
                   is_nested_induction_variable_dependent_access(
                   _environment, lhs) &&
                   !VectorizationAnalysisInterface::_vectorizer_analysis->
                   is_induction_variable_dependent_expression(
                   _environment._analysis_simd_scope, lhs))
                   {
                   std::cerr << "Nested IV dependent store: " << lhs.prettyprint()
                   << std::endl;
                   running_error("Vectorizer: Extract operation is not "\
                   "supported yet (%s).", lhs.prettyprint().c_str());
                   }
                 */
                else
                {
                    // Get a scatter for real scatter or unaligned store extra flag
                    const Nodecl::ArraySubscript lhs_array_copy =
                        VectorizationAnalysisInterface::_vectorizer_analysis->shallow_copy(
                                lhs).as<Nodecl::ArraySubscript>();

                    /*VectorizerGatherScatterInfo scatter_access(_environment);

                      const Nodecl::NodeclBase base =
                      scatter_access.get_base(lhs_array_copy);
                      Nodecl::NodeclBase strides =
                      scatter_access.get_strides(lhs_array_copy);
                     */
                    const Nodecl::NodeclBase base = 
                        lhs_array_copy.get_subscripted();
                    // The array must have been linearized
                    Nodecl::NodeclBase strides = lhs_array_copy.as<Nodecl::ArraySubscript>().
                        get_subscripts().as<Nodecl::List>().front();
                       
                    // Vectorize strides
                    walk(strides);

                    const Nodecl::VectorScatter vector_scatter =
                        Nodecl::VectorScatter::make(
                                base,
                                strides,
                                rhs.shallow_copy(),
                                mask.shallow_copy(),
                                vector_type,
                                n.get_locus());


                    // Adjacent access
                    if(VectorizationAnalysisInterface::_vectorizer_analysis->
                            is_adjacent_access(_environment._analysis_simd_scope, lhs))
                    {
                        TL::Type basic_type = lhs.get_type();
                        if (basic_type.is_lvalue_reference())
                        {
                            basic_type = basic_type.references_to();
                        }

                        nontmp_expr_map_t::const_iterator nontemporal_it =
                            _environment._nontemporal_exprs_map.find(
                                    subscripted_symbol.get_symbol());

                        bool nontemporal_store = (nontemporal_it !=
                                _environment._nontemporal_exprs_map.end());

                        // Aligned
                        if(VectorizationAnalysisInterface::_vectorizer_analysis->
                                is_simd_aligned_access(
                                    _environment._analysis_simd_scope,
                                    lhs,
                                    _environment._aligned_symbols_map,
                                    _environment._suitable_exprs_list,
                                    _environment._vectorization_factor,
                                    _environment._vectorization_factor * assignment_type.get_size()))
                        {
                            if (nontemporal_store)
                            {
                                VECTORIZATION_DEBUG()
                                {
                                    fprintf(stderr, "VECTORIZER: Aligned stream store '%s'\n",
                                            lhs.prettyprint().c_str());
                                }

                                const Nodecl::VectorStreamStore vector_stream_store =
                                    Nodecl::VectorStreamStore::make(
                                            Nodecl::Reference::make(
                                                lhs.shallow_copy(),
                                                basic_type.get_pointer_to(),
                                                n.get_locus()),
                                            rhs.shallow_copy(),
                                            mask.shallow_copy(),
                                            Nodecl::List::make(
                                                nontemporal_it->second).shallow_copy(),
                                            vector_type,
                                            n.get_locus());

                                n.replace(vector_stream_store);
                            }
                            else
                            {
                                VECTORIZATION_DEBUG()
                                {
                                    fprintf(stderr, "VECTORIZER: Aligned store  '%s'\n",
                                            lhs.prettyprint().c_str());
                                }

                                const Nodecl::VectorStore vector_store =
                                    Nodecl::VectorStore::make(
                                            Nodecl::Reference::make(
                                                lhs.shallow_copy(),
                                                basic_type.get_pointer_to(),
                                                n.get_locus()),
                                            rhs.shallow_copy(),
                                            mask.shallow_copy(),
                                            Nodecl::List::make(vector_scatter),
                                            vector_type,
                                            n.get_locus());

                                n.replace(vector_store);
                            }
                        }
                        else // Unaligned
                        {
                            if (nontemporal_store)
                            {
                                VECTORIZATION_DEBUG()
                                {
                                    fprintf(stderr, "VECTORIZER: Unaligned stream store '%s'\n",
                                            lhs.prettyprint().c_str());
                                }

                                const Nodecl::UnalignedVectorStreamStore vector_stream_store =
                                    Nodecl::UnalignedVectorStreamStore::make(
                                            Nodecl::Reference::make(
                                                lhs.shallow_copy(),
                                                basic_type.get_pointer_to(),
                                                n.get_locus()),
                                            rhs.shallow_copy(),
                                            mask.shallow_copy(),
                                            Nodecl::List::make(
                                                nontemporal_it->second).shallow_copy(),
                                            vector_type,
                                            n.get_locus());

                                n.replace(vector_stream_store);
                            }
                            else
                            {
                                VECTORIZATION_DEBUG()
                                {
                                    fprintf(stderr, "VECTORIZER: Unaligned store '%s'\n",
                                            lhs.prettyprint().c_str());
                                }

                                const Nodecl::UnalignedVectorStore vector_store =
                                    Nodecl::UnalignedVectorStore::make(
                                            Nodecl::Reference::make(
                                                lhs.shallow_copy(),
                                                basic_type.get_pointer_to(),
                                                n.get_locus()),
                                            rhs.shallow_copy(),
                                            mask.shallow_copy(),
                                            Nodecl::List::make(vector_scatter),
                                            vector_type,
                                            n.get_locus());

                                n.replace(vector_store);
                            }
                        }
                    }
                    else // Vector Scatter
                    {
                        VECTORIZATION_DEBUG()
                        {
                            fprintf(stderr, "VECTORIZER: Scatter '%s'\n",
                                    lhs.prettyprint().c_str());
                        }

                        n.replace(vector_scatter);
                    }
                }
            }
            else if (lhs.is<Nodecl::Symbol>())
            {
                Nodecl::Symbol sym = lhs.as<Nodecl::Symbol>();

                walk(lhs); // This only works because lhs is a symbol.
                // walk is only to visit RHS but in this
                // case LHS and RHS visits seems to be
                // equivalent

                TL::Type tl_lhs_sym_type = sym.get_type().no_ref();

                if (tl_lhs_sym_type.is_vector())  // Register
                {
                    walk(rhs);

                    const Nodecl::VectorAssignment vector_assignment =
                        Nodecl::VectorAssignment::make(
                                lhs.shallow_copy(),
                                rhs.shallow_copy(),
                                mask.shallow_copy(),
                                vector_type,
                                n.get_locus());

                    n.replace(vector_assignment);
                }
                else if (tl_lhs_sym_type.is_mask())
                {
                    walk(rhs);

                    const Nodecl::VectorMaskAssignment vector_mask_assignment =
                        Nodecl::VectorMaskAssignment::make(
                                lhs.shallow_copy(),
                                rhs.shallow_copy(),
                                vector_type,
                                n.get_locus());

                    n.replace(vector_mask_assignment);
                }
                else
                {
                    VECTORIZATION_DEBUG()
                    {
                        fprintf(stderr, "VECTORIZER: Keep assignment scalar '%s'\n",
                                n.prettyprint().c_str());
                    }
                }
            }
            else if (lhs.is<Nodecl::ClassMemberAccess>())
            {
                Nodecl::ClassMemberAccess cma = lhs.as<Nodecl::ClassMemberAccess>();

                Nodecl::NodeclBase class_object = cma.get_lhs();
                Nodecl::NodeclBase member = cma.get_member();

                ERROR_CONDITION(!member.is<Nodecl::Symbol>(), 
                        "Member is not a Symbol. Unsupported case", 0);
                ERROR_CONDITION(!class_object.is<Nodecl::ArraySubscript>(),
                        "Lhs is not an array. Unsupported case", 0);

                int access_size = cma.get_type().no_ref().get_size();
                int class_size = class_object.get_type().no_ref().get_size();
                int member_offset = member.as<Nodecl::Symbol>().get_symbol().get_offset();

                //TODO a.x[i] = 

                // a[i].x --> Scatter

                // Remove CMA and visit again
                lhs.replace(class_object.shallow_copy());

                walk(n);

                // Update lhs
                lhs = n.get_lhs();

                if(lhs.is<Nodecl::VectorStore>() ||
                        lhs.is<Nodecl::UnalignedVectorStore>())
                {
                    Nodecl::VectorStore vstore = lhs.as<Nodecl::VectorStore>();

                    Nodecl::VectorScatter vector_scatter = vstore.get_flags().
                        as<Nodecl::List>().find_first<Nodecl::VectorScatter>();

                    // Add member to strides
                    Nodecl::NodeclBase strides = vector_scatter.get_strides();

                    strides.replace(Nodecl::Add::make(
                                Nodecl::Mul::make(strides.shallow_copy(),
                                    Nodecl::IntegerLiteral::make(
                                        TL::Type::get_int_type(),
                                        const_value_get_signed_int(
                                            class_size/access_size)),
                                    strides.get_type()),
                                Nodecl::IntegerLiteral::make(
                                    TL::Type::get_int_type(),
                                    const_value_get_signed_int(
                                        member_offset/access_size)),
                                strides.get_type()));

                    n.replace(vector_scatter);
                }
                else
                {
                    running_error("Vectorizer: ClassMemberAccess type is not "\
                            "supported yet: '%s'", n.prettyprint().c_str());
                }
            }
            else
            {
                internal_error("Vectorizer: Unsupported assignment on %s at %s.\n",
                        lhs.prettyprint().c_str(), n.get_locus());
            }
        } 
        else
        {
            VECTORIZATION_DEBUG()
            {
                fprintf(stderr, "VECTORIZER: Keep scalar %s\n",
                        lhs.prettyprint().c_str());
            }
        }
    }

    void VectorizerVisitorExpression::visit(const Nodecl::Conversion& n)
    {
        Nodecl::NodeclBase mask = Utils::get_proper_mask(
                _environment._mask_list.back());

        Nodecl::NodeclBase nest = n.get_nest();
        walk(nest);

        // If false, someone (TL::Symbol) moves/replaces my tree.
        // Therefore do nothing, I'm no longer a Conversion!!
        if (n.is<Nodecl::Conversion>())
        {
            TL::Type src_type = n.get_nest().get_type();
            TL::Type dst_type = n.get_type();

            if (src_type.no_ref().is_vector())
            {
                // Remove rvalue conversions. In a vector code they are
                // explicit loads ops.
                //                    if (src_type != dst_type)

                TL::Type dst_vec_type;

                if (dst_type.is_bool())
                    dst_vec_type = TL::Type::get_mask_type(
                            _environment._vectorization_factor);
                else
                    dst_vec_type = Utils::get_qualified_vector_to(dst_type,
                            _environment._vectorization_factor);


                Nodecl::VectorConversion vector_conv =
                    Nodecl::VectorConversion::make(
                            n.get_nest().shallow_copy(),
                            mask,
                            dst_vec_type,
                            n.get_locus());

                vector_conv.set_constant(const_value_convert_to_type(
                            n.get_nest().get_constant(),
                            dst_type.get_internal_type()));

                n.replace(vector_conv);
            }
        }
    }

    void VectorizerVisitorExpression::visit(const Nodecl::Cast& n)
    {
        Nodecl::NodeclBase mask = Utils::get_proper_mask(
                _environment._mask_list.back());

        Nodecl::NodeclBase rhs = n.get_rhs();
        walk(rhs);

        Nodecl::VectorConversion vector_conv =
            Nodecl::VectorConversion::make(
                    rhs.shallow_copy(),
                    mask,
                    Utils::get_qualified_vector_to(n.get_type(),
                        _environment._vectorization_factor),
                    n.get_locus());

        if(rhs.is_constant())
            vector_conv.set_constant(rhs.get_constant());

        /*
           printf("Casting %s %s\n",
           Utils::get_qualified_vector_to(n.get_type(), _environment._vectorization_factor).get_simple_declaration(n.retrieve_context(), "").c_str(),
           n.get_rhs().get_type().get_simple_declaration(n.retrieve_context(), "").c_str());
         */
        n.replace(vector_conv);
    }

    void VectorizerVisitorExpression::visit(const Nodecl::ArraySubscript& n)
    {
        Nodecl::NodeclBase mask = Utils::get_proper_mask(
                _environment._mask_list.back());

        TL::Type basic_type = n.get_type().no_ref();

        const_value_t* const_value = NULL;
        if (n.is_constant())
        {
            std::cerr << "TODO: CONSTANT ARRAYSUBSCRIPT: "
                << n.prettyprint() << "\n";
        }


        // Computing new vector type
        TL::Type vector_type = Utils::get_qualified_vector_to(basic_type,
                _environment._vectorization_factor);

        // Vector Promotion from constant ArraySubscript
        if (VectorizationAnalysisInterface::_vectorizer_analysis->
                is_uniform(_environment._analysis_simd_scope, n, n))
        {
            VECTORIZATION_DEBUG()
            {
                std::cerr << "VECTORIZER: Constant load: " << n.prettyprint() << "\n";
            }

            // Deal with Nodecl::Conversions
            Nodecl::NodeclBase encapsulated_symbol = n;
            while (!encapsulated_symbol.get_parent().is_null() &&
                    encapsulated_symbol.get_parent().is<Nodecl::Conversion>())
                encapsulated_symbol = encapsulated_symbol.get_parent();

            TL::Type encapsulated_symbol_type = encapsulated_symbol.get_type();

            Nodecl::VectorPromotion vector_prom =
                Nodecl::VectorPromotion::make(
                        encapsulated_symbol.shallow_copy(),
                        mask,
                        Utils::get_qualified_vector_to(encapsulated_symbol_type,
                            _environment._vectorization_factor),
                        n.get_locus());

            vector_prom.set_constant(const_value);

            encapsulated_symbol.replace(vector_prom);
        }
        /* This is no longer necessary due to the new query variable_is_constant_at_statement
        // Vector promotion from ArraySubscript indexed by nested IV
        else if (VectorizationAnalysisInterface::_vectorizer_analysis->
                is_nested_induction_variable_dependent_access(
                    _environment, n) &&
                !VectorizationAnalysisInterface::_vectorizer_analysis->
                is_induction_variable_dependent_expression(
                    _environment._analysis_simd_scope,
                    n))
        {
            VECTORIZATION_DEBUG()
            {
                fprintf(stderr, "VECTORIZER: Nested IV dependent load '%s'\n",
                        n.prettyprint().c_str());
            }

            // Deal with Nodecl::Conversions
            Nodecl::NodeclBase encapsulated_symbol = n;
            while (!encapsulated_symbol.get_parent().is_null() &&
                    encapsulated_symbol.get_parent().is<Nodecl::Conversion>())
                encapsulated_symbol = encapsulated_symbol.get_parent();

            TL::Type encapsulated_symbol_type = encapsulated_symbol.get_type();

            Nodecl::VectorPromotion vector_prom =
                Nodecl::VectorPromotion::make(
                        encapsulated_symbol.shallow_copy(),
                        mask,
                        Utils::get_qualified_vector_to(encapsulated_symbol_type,
                            _environment._vectorization_factor),
                        n.get_locus());

            vector_prom.set_constant(const_value);

            encapsulated_symbol.replace(vector_prom);
        }
        */
        // Cached access
        else if (_cache_enabled &&
                _environment._vectorizer_cache.is_cached_access(n))
        {
            std::cerr << "CACHED ACCESS: " << n.prettyprint() << " IS ";
            n.replace(_environment._vectorizer_cache.get_load_access(n));
            std::cerr << n.prettyprint() << std::endl;
        }
        // Vector Load
        else
        {
            Nodecl::ArraySubscript array_copy = VectorizationAnalysisInterface::
                _vectorizer_analysis->shallow_copy(n).as<Nodecl::ArraySubscript>();

            // Get a gather for real gather or unaligned load extra flag
            /*VectorizerGatherScatterInfo gather_access(_environment);

            Nodecl::NodeclBase base = gather_access.get_base(array_copy);
            Nodecl::NodeclBase strides = gather_access.get_strides(array_copy);
            */
            Nodecl::NodeclBase base = array_copy.get_subscripted();
                             // The array must have been linearized
            Nodecl::NodeclBase strides = array_copy.get_subscripts().
                as<Nodecl::List>().front();
 
            // Vectorize strides
            walk(strides);

            Nodecl::VectorGather vector_gather =
                Nodecl::VectorGather::make(
                        base,
                        strides,
                        mask.shallow_copy(),
                        vector_type,
                        n.get_locus());

            vector_gather.set_constant(const_value);

            // Adjacent access
            if (VectorizationAnalysisInterface::_vectorizer_analysis->
                    is_adjacent_access(
                        _environment._analysis_simd_scope,
                        n))
            {
                // Aligned
                if(VectorizationAnalysisInterface::_vectorizer_analysis->
                        is_simd_aligned_access(
                            _environment._analysis_simd_scope,
                            n,
                            _environment._aligned_symbols_map,
                            _environment._suitable_exprs_list,
                            _environment._vectorization_factor,
                            _environment._vector_length))
                {
                    VECTORIZATION_DEBUG()
                    {
                        fprintf(stderr, "VECTORIZER: Aligned load   '%s'\n",
                                n.prettyprint().c_str());
                    }

                    Nodecl::VectorLoad vector_load =
                        Nodecl::VectorLoad::make(
                                Nodecl::Reference::make(
                                    n.shallow_copy(),
                                    basic_type.get_pointer_to(),
                                    n.get_locus()),
                                mask,
                                Nodecl::List::make(vector_gather),
                                vector_type,
                                n.get_locus());

                    vector_load.set_constant(const_value);

                    n.replace(vector_load);
                }
                else // Unaligned
                {
                    VECTORIZATION_DEBUG()
                    {
                        fprintf(stderr, "VECTORIZER: Unaligned load '%s'\n",
                                n.prettyprint().c_str());
                    }

                    Nodecl::UnalignedVectorLoad vector_load =
                        Nodecl::UnalignedVectorLoad::make(
                                Nodecl::Reference::make(
                                    n.shallow_copy(),
                                    basic_type.get_pointer_to(),
                                    n.get_locus()),
                                mask,
                                Nodecl::List::make(vector_gather),
                                vector_type,
                                n.get_locus());

                    vector_load.set_constant(const_value);

                    n.replace(vector_load);
                }
            }
            else // Vector Gather
            {
                VECTORIZATION_DEBUG()
                {
                    fprintf(stderr, "VECTORIZER: Gather '%s'\n", 
                            n.prettyprint().c_str());
                }

                n.replace(vector_gather);
            }
        }
    }

    void VectorizerVisitorExpression::visit(const Nodecl::FunctionCall& n)
    {
        Nodecl::NodeclBase mask = Utils::get_proper_mask(
                _environment._mask_list.back());

        Nodecl::NodeclBase called = n.get_called();
        ERROR_CONDITION(!called.is<Nodecl::Symbol>(),
                "Vectorizer: %s found. This kind of function call is not "\
                "supported yet", ast_print_node_type(called.get_kind()));

        Nodecl::Symbol called_sym = called.as<Nodecl::Symbol>();
        TL::Type call_type = n.get_type();
        std::string func_name = called_sym.get_symbol().get_name();

        if (func_name == "_mm_prefetch" || func_name == "_mm_prefetche")
        {
            VECTORIZATION_DEBUG()
            {
                std::cerr << "Warning: preventing prefetch function call "\
                    "from being vectorized: " << n.get_locus() << std::endl;
            }

            return;
        }

        if (func_name == "_mm_clevict")
        {
            VECTORIZATION_DEBUG()
            {
                std::cerr << "Warning: preventing eviction function call "\
                    "from being vectorized: " << n.get_locus() << std::endl;
            }

            return;
        }


        // Vectorizing arguments
        Nodecl::List arguments_list = n.get_arguments().as<Nodecl::List>();
        bool need_vector_function = false;

        for(Nodecl::List::iterator it = arguments_list.begin();
                it != arguments_list.end();
                it++)
        {
            if (!VectorizationAnalysisInterface::_vectorizer_analysis->
                    is_uniform(_environment._analysis_simd_scope, *it, *it))
            {
                if(!VectorizationAnalysisInterface::_vectorizer_analysis->
                    is_linear(_environment._analysis_simd_scope, *it))
                {
                    VECTORIZATION_DEBUG()
                    {
                        std::cerr << "VECTORIZER: Vectorizing argument '" << it->prettyprint()
                            << "'" << std::endl;
                    }
                    
                    walk(*it);
                }
                else
                {
                    VECTORIZATION_DEBUG()
                    {
                        std::cerr << "VECTORIZER: Argument '" << it->prettyprint()
                            << "' is kept scalar because is linear" << std::endl;
                    }
                }
 
                need_vector_function = true;
            }
            else
            {
                VECTORIZATION_DEBUG()
                {
                    std::cerr << "VECTORIZER: Argument '" << it->prettyprint()
                        << "' is kept scalar because is uniform" << std::endl;
                }
            }
        }

        // Vectorize version only if it uses vector arguments
        // TODO: return type
        if (need_vector_function)
        {
            VECTORIZATION_DEBUG()
            {
                std::cerr << "VECTORIZER: Vectorizing function call '"
                    << func_name << "'" << std::endl;
            }

            if (func_name == "fabsf" ||
                    func_name == "fabs")
            {
                const Nodecl::VectorFabs vector_fabs_call =
                    Nodecl::VectorFabs::make(
                            n.get_arguments().as<Nodecl::List>().
                            front().shallow_copy(),
                            mask,
                            Utils::get_qualified_vector_to(
                                call_type, _environment._vectorization_factor),
                            n.get_locus());

                n.replace(vector_fabs_call);
            }
            else if (func_name == "sqrtf" ||
                    func_name == "sqrt")
            {
                const Nodecl::VectorSqrt vector_sqrt_call =
                    Nodecl::VectorSqrt::make(
                            n.get_arguments().as<Nodecl::List>().
                            front().shallow_copy(),
                            mask,
                            Utils::get_qualified_vector_to(call_type,
                                _environment._vectorization_factor),
                            n.get_locus());

                n.replace(vector_sqrt_call);
            }
            else if (func_name == "sincosf")
            {
                Nodecl::List::iterator args = n.get_arguments().
                    as<Nodecl::List>().begin();

                Nodecl::NodeclBase source = *args;
                args++;
                Nodecl::NodeclBase sin_p = *args;
                args++;
                Nodecl::NodeclBase cos_p = *args;
                args++;

                const Nodecl::VectorSincos vector_sincos_call =
                    Nodecl::VectorSincos::make(
                            source.shallow_copy(),
                            sin_p.shallow_copy(),
                            cos_p.shallow_copy(),
                            mask,
                            Utils::get_qualified_vector_to(
                                TL::Type::get_float_type(),
                                _environment._vectorization_factor),
                            n.get_locus());

                n.replace(vector_sincos_call);
            }
            else //Common functions
            {
                TL::Type function_target_type = _environment._target_type;
                int function_target_type_size = function_target_type.get_size();

                // If _target_type and call_type have the same size, we use call_type as
                // this function should have been registered with this type
                if (call_type.is_void() || 
                        _environment._target_type.get_size() == call_type.get_size())
                {
                    function_target_type = call_type;
                    function_target_type_size = function_target_type.get_size();
                }

                // Get the best vector version of the function available
                Nodecl::NodeclBase best_version =
                    Vectorizer::_function_versioning.get_best_version(
                            func_name,
                            _environment._device,
                            _environment._vectorization_factor * function_target_type_size,
                            function_target_type,
                            !mask.is_null());

                ERROR_CONDITION(best_version.is_null(), "Vectorizer: the best "\
                        "vector function for '%s' is null", func_name.c_str());

                // Create new called symbol
                Nodecl::Symbol new_called;
                if (best_version.is<Nodecl::FunctionCode>())
                {
                    new_called = best_version.as<Nodecl::FunctionCode>().
                        get_symbol().make_nodecl(true, n.get_locus());
                }
                else if (best_version.is<Nodecl::Symbol>())
                {
                    new_called = best_version.as<Nodecl::Symbol>().get_symbol().
                        make_nodecl(true, n.get_locus());
                }
                else
                {
                    running_error("Vectorizer: %s found as vector function "\
                            "version in function versioning.",
                            ast_print_node_type(best_version.get_kind()));
                }

                const Nodecl::VectorFunctionCall vector_function_call =
                    Nodecl::VectorFunctionCall::make(
                            Nodecl::FunctionCall::make(
                                new_called,
                                n.get_arguments().shallow_copy(),
                                n.get_alternate_name().shallow_copy(),
                                n.get_function_form().shallow_copy(),
                                Utils::get_qualified_vector_to(call_type,
                                    _environment._vectorization_factor),
                                n.get_locus()),
                            called_sym.shallow_copy(),
                            mask,
                            Utils::get_qualified_vector_to(call_type,
                                _environment._vectorization_factor),
                            n.get_locus());

                n.replace(vector_function_call);
            }
        }
        else
        {
            VECTORIZATION_DEBUG()
            {
                std::cerr << "VECTORIZER: Function call '"
                    << func_name << "' is kept scalar" << std::endl;
            }
        }
    }

    void VectorizerVisitorExpression::visit(const Nodecl::Symbol& n)
    {
        TL::Type sym_type = n.get_type().no_ref();
        TL::Symbol tl_sym = n.get_symbol();
        TL::Type tl_sym_type = tl_sym.get_type().no_ref();

        // Deal with Nodecl::Conversions
        Nodecl::NodeclBase encapsulated_symbol = n;
        while ((!encapsulated_symbol.get_parent().is_null()) &&
                encapsulated_symbol.get_parent().is<Nodecl::Conversion>())
            encapsulated_symbol = encapsulated_symbol.get_parent();

        TL::Type encapsulated_symbol_type = encapsulated_symbol.get_type();

        //std::cerr << "scalar_type: " << n.prettyprint() << std::endl;

        if(!sym_type.is_vector() && !sym_type.is_mask())
        {
           // Vectorize BASIC induction variable  // visiting RHS of an assignment
            if (!encapsulated_symbol_type.is_lvalue_reference() &&
                    VectorizationAnalysisInterface::_vectorizer_analysis->
                    is_linear(_environment._analysis_simd_scope, n))
            {
                vectorize_basic_induction_variable(n);
            }
            /*
            // Vectorize symbols declared in the SIMD scope
            else if (Utils::is_declared_in_inner_scope(
            _environment._analysis_simd_scope,
            n.get_symbol()))
            {
            symbol_type_promotion(n);
            }


            // Vectorize NESTED IV
            else if (// Is nested IV and
            VectorizationAnalysisInterface::_vectorizer_analysis->
            is_nested_non_reduction_basic_induction_variable(
            _environment, n)
            &&
            // Lb doesn't depend on SIMD IV and
            !VectorizationAnalysisInterface::_vectorizer_analysis->
            iv_lb_depends_on_ivs_from_scope(
            _environment._analysis_scopes.back(),
            n,
            _environment._analysis_simd_scope)
            &&
            // Step doesn't depend on SIMD IV
            !VectorizationAnalysisInterface::_vectorizer_analysis->
            iv_step_depends_on_ivs_from_scope(
            _environment._analysis_scopes.back(),
            n,
            _environment._analysis_simd_scope))
            {
            VECTORIZATION_DEBUG()
            {
            fprintf(stderr,"VECTORIZER: Promotion '%s' (nested IV)\n",
            n.prettyprint().c_str());
            }

            const Nodecl::VectorPromotion vector_prom =
            Nodecl::VectorPromotion::make(
            encapsulated_symbol.shallow_copy(),
            Utils::get_null_mask(),
            Utils::get_qualified_vector_to(encapsulated_symbol_type,
            _environment._vectorization_factor),
            n.get_locus());

            encapsulated_symbol.replace(vector_prom);
            }
             */
            // Is a reduction symbol
            else if(_environment._reduction_list != NULL &&
                    _environment._reduction_list->contains(tl_sym))
            {
                // If symbol is in the map
                Nodecl::Symbol new_red_symbol;

                std::map<TL::Symbol, TL::Symbol>::iterator it =
                    _environment._new_external_vector_symbol_map->
                    find(tl_sym);

                new_red_symbol = it->second.make_nodecl(
                        true, n.get_locus());

                VECTORIZATION_DEBUG()
                {
                    fprintf(stderr,"VECTORIZER: Reduction symbol '%s'\n",
                            n.prettyprint().c_str());
                }

                n.replace(new_red_symbol);
            }
            // Nodecl::Symbol with scalar type whose TL::Symbol has vector_type
            else if(tl_sym_type.is_vector() || tl_sym_type.is_mask())
            {
                symbol_type_promotion(n);
            }
            // Promotion: scalar_type and visiting RHS of an assignment
            else if (!encapsulated_symbol_type.is_lvalue_reference())
            {
                VECTORIZATION_DEBUG()
                {
                    fprintf(stderr,"VECTORIZER: Promotion '%s'\n",
                            n.prettyprint().c_str());
                }

                Nodecl::VectorPromotion vector_prom =
                    Nodecl::VectorPromotion::make(
                            encapsulated_symbol.shallow_copy(),
                            Utils::get_null_mask(),
                            Utils::get_qualified_vector_to(
                                encapsulated_symbol_type,
                                _environment._vectorization_factor),
                            n.get_locus());

                if(encapsulated_symbol.is_constant())
                    vector_prom.set_constant(
                            const_value_make_vector_from_scalar(
                                _environment._vectorization_factor,
                                encapsulated_symbol.get_constant()));

                encapsulated_symbol.replace(vector_prom);
            }

            // Vectorize constants
            /*
               else if (VectorizationAnalysisInterface::_vectorizer_analysis->
               variable_is_constant_at_statement(_environment._analysis_simd_scope, n))
               {
               VECTORIZATION_DEBUG()
               {
               fprintf(stderr,"VECTORIZER: Promotion '%s'\n",
               n.prettyprint().c_str());
               }

               Nodecl::VectorPromotion vector_prom =
               Nodecl::VectorPromotion::make(
               encapsulated_symbol.shallow_copy(),
               Utils::get_null_mask(),
               Utils::get_qualified_vector_to(
               encapsulated_symbol_type,
               _environment._vectorization_factor),
               n.get_locus());

               if(encapsulated_symbol.is_constant())
               vector_prom.set_constant(
               const_value_make_vector_from_scalar(
               _environment._vectorization_factor,
               encapsulated_symbol.get_constant()));

               encapsulated_symbol.replace(vector_prom);
               }
             */
               else
               {
                   //TODO: If you are from outside of the loop -> Vector local copy.
                   running_error("Vectorizer: Loop is not vectorizable. '%s' "\
                           "is not IV, Invariant, Reduction or LastPrivate.",
                           n.get_symbol().get_name().c_str());
               }
        }
        else
        {
            VECTORIZATION_DEBUG()
            {
                fprintf(stderr,"VECTORIZER: '%s' already has vector type\n",
                        n.prettyprint().c_str());
            }
        }
    }

    void VectorizerVisitorExpression::visit(const Nodecl::ClassMemberAccess& n)
    {
        Nodecl::NodeclBase n_original = n.shallow_copy();
        Nodecl::NodeclBase class_object = n.get_lhs();
        Nodecl::NodeclBase member = n.get_member();
        Nodecl::NodeclBase mask = Utils::get_proper_mask(
                _environment._mask_list.back());

        ERROR_CONDITION(!member.is<Nodecl::Symbol>(), 
                "Member is not a Symbol. Unsupported case", 0);
        ERROR_CONDITION(!class_object.is<Nodecl::ArraySubscript>(),
                "Lhs is not an array. Unsupported case", 0);

        int access_size = n.get_type().no_ref().get_size();
        int class_size = class_object.get_type().no_ref().get_size();
        int member_offset = member.as<Nodecl::Symbol>().get_symbol().get_offset();

        //TODO a.x[i]

        // a[i].x --> Gather

        walk(class_object);

        // Gather
        if(class_object.is<Nodecl::VectorLoad>() ||
                class_object.is<Nodecl::UnalignedVectorLoad>())
        {
            Nodecl::VectorLoad vload = class_object.as<Nodecl::VectorLoad>();

            Nodecl::VectorGather vector_gather = vload.get_flags().
                as<Nodecl::List>().find_first<Nodecl::VectorGather>();

            // Add pointer casting to base --> (float *) &a[i].fp
            Nodecl::NodeclBase base = vector_gather.get_base();

            base.replace(Nodecl::Cast::make(base.shallow_copy(),
                        n_original.get_type().no_ref().get_pointer_to(), ""));

            // Add member to strides
            Nodecl::NodeclBase strides = vector_gather.get_strides();

            strides.replace(Nodecl::Add::make(
                        Nodecl::Mul::make(strides.shallow_copy(),
                            Nodecl::IntegerLiteral::make(
                                TL::Type::get_int_type(),
                                const_value_get_signed_int(
                                    class_size/access_size)),
                            strides.get_type()),
                        Nodecl::IntegerLiteral::make(
                            TL::Type::get_int_type(),
                            const_value_get_signed_int(
                                member_offset/access_size)),
                        strides.get_type()));

            // Set new type
            vector_gather.set_type(Utils::get_qualified_vector_to(
                        n_original.get_type(),
                        _environment._vectorization_factor));

            n.replace(vector_gather);
        }
        // Vector Promotion
        else if (class_object.is<Nodecl::VectorPromotion>())
        {
            Nodecl::VectorPromotion vprom = class_object.
                as<Nodecl::VectorPromotion>();

            vprom.get_rhs().replace(n_original.shallow_copy());
            
            // Set new type
            vprom.set_type(Utils::get_qualified_vector_to(
                        n_original.get_type(),
                        _environment._vectorization_factor));

            n.replace(vprom);
        }
        else
        {
                running_error("Vectorizer: ClassMemberAccess type is not "\
                        "supported yet: '%s'", n.prettyprint().c_str());
        }
    }

    void VectorizerVisitorExpression::visit(const Nodecl::IntegerLiteral& n)
    {
        VECTORIZATION_DEBUG()
        {
            fprintf(stderr,"VECTORIZER: IntegerLiteral promotion '%s'\n",
                    n.prettyprint().c_str());
        }

        Nodecl::VectorPromotion vector_prom =
            Nodecl::VectorPromotion::make(
                    n.shallow_copy(),
                    Utils::get_null_mask(),
                    Utils::get_qualified_vector_to(n.get_type(),
                        _environment._vectorization_factor),
                    n.get_locus());

        vector_prom.set_constant(const_value_make_vector_from_scalar(
                    _environment._vectorization_factor,
                    n.get_constant()));


        n.replace(vector_prom);
    }

    void VectorizerVisitorExpression::visit(const Nodecl::FloatingLiteral& n)
    {
        VECTORIZATION_DEBUG()
        {
            fprintf(stderr,"VECTORIZER: FloatingLiteral promotion '%s'\n",
                    n.prettyprint().c_str());
        }

        Nodecl::VectorPromotion vector_prom =
            Nodecl::VectorPromotion::make(
                    n.shallow_copy(),
                    Utils::get_null_mask(),
                    Utils::get_qualified_vector_to(n.get_type(),
                        _environment._vectorization_factor),
                    n.get_locus());

        vector_prom.set_constant(const_value_make_vector_from_scalar(
                    _environment._vectorization_factor,
                    n.get_constant()));

        n.replace(vector_prom);
    }

    void VectorizerVisitorExpression::visit(
            const Nodecl::ParenthesizedExpression& n)
    {
        walk(n.get_nest());

        Nodecl::ParenthesizedExpression parent = n.shallow_copy().
            as<Nodecl::ParenthesizedExpression>();

        parent.set_type(n.get_nest().get_type());
        if(n.is_constant())
            parent.set_constant(const_value_make_vector_from_scalar(
                        _environment._vectorization_factor,
                        n.get_constant()));

        n.replace(parent);
    }

    void VectorizerVisitorExpression::visit(const Nodecl::Reference& n)
    {
        walk(n.get_rhs());

        const Nodecl::Reference reference =
            Nodecl::Reference::make(
                    n.get_rhs().shallow_copy(),
                    n.get_rhs().get_type().no_ref().get_pointer_to(),
                    n.get_locus());

        n.replace(reference);
    }

    void VectorizerVisitorExpression::visit(const Nodecl::Dereference& n)
    {
        walk(n.get_rhs());

        const Nodecl::Dereference dereference =
            Nodecl::Dereference::make(
                    n.get_rhs().shallow_copy(),
                    Utils::get_qualified_vector_to(n.get_type(),
                        _environment._vectorization_factor),
                    n.get_locus());

        n.replace(dereference);
    }

    void VectorizerVisitorExpression::visit(const Nodecl::VectorLaneId& n)
    {
        // Offset list
        Nodecl::List offset_list =
            Vectorization::Utils::get_vector_offset_list(
                    /* start value */ 0, /* stride value */ 1,
                    /* size */ _environment._vectorization_factor);

        // VectorLiteral {0, 1, 2, ..., VL-1}
        Nodecl::VectorLiteral offset_vector_literal =
            Nodecl::VectorLiteral::make(
                    offset_list,
                    Utils::get_null_mask(),
                    Utils::get_qualified_vector_to(n.get_type(),
                        _environment._vectorization_factor),
                    n.get_locus());

        offset_vector_literal.set_constant(
                offset_list.get_constant());

        n.replace(offset_vector_literal);
    }

    void VectorizerVisitorExpression::vectorize_basic_induction_variable(
            const Nodecl::Symbol& n)
    {
        VECTORIZATION_DEBUG()
        {
            fprintf(stderr,"VECTORIZER: Linear promotion '%s'\n",
                    n.prettyprint().c_str());
        }

        // Computing IV offset {0, 1, 2, 3}
        Nodecl::NodeclBase ind_var_increment = VectorizationAnalysisInterface::
            _vectorizer_analysis->get_linear_step(
                    _environment._analysis_simd_scope, n);

        if (ind_var_increment.is_constant())
        {
            int iv_increment = const_value_cast_to_4(ind_var_increment.get_constant());
            // Offset list
            Nodecl::List offset_list =
                Vectorization::Utils::get_vector_offset_list(0, iv_increment,
                        _environment._vectorization_factor);

            Nodecl::NodeclBase vector_induction_var;
            TL::Type ind_var_type;

            // If Conversion node
            if(n.get_parent().is<Nodecl::Conversion>())
            {
                Nodecl::Conversion conversion = n.get_parent().
                    as<Nodecl::Conversion>();

                TL::Type dest_type = conversion.get_type();
                TL::Type source_type = conversion.get_nest().get_type();

                // No conversion of values is needed
                if (dest_type.is_same_type(source_type.no_ref().
                            get_unqualified_type()))
                {
                    ind_var_type = Utils::get_qualified_vector_to(
                            conversion.get_type().no_ref(),
                            _environment._vectorization_factor);

                    // VectorLiteral offset
                    Nodecl::VectorLiteral offset_vector_literal =
                        Nodecl::VectorLiteral::make(
                                offset_list,
                                Utils::get_null_mask(),
                                ind_var_type,
                                n.get_locus());

                    offset_vector_literal.set_constant(
                            offset_list.get_constant());

                    // Pomoted Conversion
                    Nodecl::VectorPromotion promoted_conversion =
                        Nodecl::VectorPromotion::make(
                                conversion.shallow_copy(),
                                Utils::get_null_mask(),
                                ind_var_type,
                                n.get_locus());

                    if (conversion.is_constant())
                    {
                        promoted_conversion.set_constant(
                                const_value_make_vector_from_scalar(
                                    _environment._vectorization_factor,
                                    conversion.get_constant()));
                    }

                    vector_induction_var =
                        Nodecl::VectorAdd::make(
                                promoted_conversion,
                                offset_vector_literal,
                                Utils::get_null_mask(),
                                Utils::get_qualified_vector_to(ind_var_type,
                                    _environment._vectorization_factor),
                                n.get_locus());

                    if (promoted_conversion.is_constant() &&
                            offset_vector_literal.is_constant())
                        vector_induction_var.set_constant(
                            const_value_add(promoted_conversion.get_constant(),
                                offset_vector_literal.get_constant()));

                }
                else // Conversion of values
                {
                    // IV cannot be a reference
                    ind_var_type = Utils::get_qualified_vector_to(
                            n.get_type(), _environment._vectorization_factor).no_ref();

                    // VectorLiteral offset
                    Nodecl::VectorLiteral offset_vector_literal =
                        Nodecl::VectorLiteral::make(
                                offset_list,
                                Utils::get_null_mask(),
                                ind_var_type,
                                n.get_locus());

                    offset_vector_literal.set_constant(
                            offset_list.get_constant());

                    // Pomoted Conversion
                    Nodecl::VectorPromotion promoted_nest =
                        Nodecl::VectorPromotion::make(
                                conversion.get_nest().shallow_copy(),
                                Utils::get_null_mask(),
                                ind_var_type,
                                n.get_locus());

                    if (conversion.get_nest().is_constant())
                    {
                        promoted_nest.set_constant(
                                const_value_make_vector_from_scalar(
                                    _environment._vectorization_factor,
                                    conversion.get_nest().get_constant()));
                    }

                    // IV + Offset
                    Nodecl::VectorAdd iv_plus_offset =
                        Nodecl::VectorAdd::make(
                                    promoted_nest,
                                    offset_vector_literal,
                                    Utils::get_null_mask(),
                                    ind_var_type,
                                    n.get_locus());

                    if (promoted_nest.is_constant() && 
                            offset_vector_literal.is_constant())
                    {
                        iv_plus_offset.set_constant(
                                const_value_add(promoted_nest.get_constant(),
                                    offset_vector_literal.get_constant()));
                    }


                    vector_induction_var =
                        Nodecl::VectorConversion::make(
                                iv_plus_offset,
                                Utils::get_null_mask(),
                                Utils::get_qualified_vector_to(dest_type,
                                    _environment._vectorization_factor),
                                n.get_locus());
                }

                conversion.replace(vector_induction_var);
            }
            else // There is no conversion
            {
                ind_var_type = Utils::get_qualified_vector_to(
                        n.get_type(), _environment._vectorization_factor).no_ref();

                // VectorLiteral offset
                Nodecl::VectorLiteral offset_vector_literal =
                    Nodecl::VectorLiteral::make(
                            offset_list,
                            Utils::get_null_mask(),
                            ind_var_type,
                            n.get_locus());

                offset_vector_literal.set_constant(offset_list.get_constant());

                vector_induction_var =
                    Nodecl::VectorAdd::make(
                            Nodecl::VectorPromotion::make(
                                n.shallow_copy(),
                                Utils::get_null_mask(),
                                ind_var_type,
                                n.get_locus()),
                            offset_vector_literal,
                            Utils::get_null_mask(),
                            ind_var_type,
                            n.get_locus()),

                    n.replace(vector_induction_var);
            }
        }
        else
        {
            running_error("Vectorizer: Linear step is not constant: %s.",
                    ind_var_increment.prettyprint().c_str());
        }
    }

    Nodecl::NodeclVisitor<void>::Ret VectorizerVisitorExpression::
        unhandled_node(const Nodecl::NodeclBase& n)
    {
        internal_error("Vectorizer: Unknown 'Expression' node %s at %s.\n%s",
                ast_print_node_type(n.get_kind()),
                n.get_locus(),
                n.prettyprint().c_str());
        /*
           std::cerr << "Vectorizer: Unknown 'Expression' node "
           << ast_print_node_type(n.get_kind())
        //<< "(" << n.prettyprint() << ")"
        << " at " << n.get_locus() << "." << std::endl
        << n.prettyprint()
        << std::endl;
         */
        return Ret();
    }
}
}
