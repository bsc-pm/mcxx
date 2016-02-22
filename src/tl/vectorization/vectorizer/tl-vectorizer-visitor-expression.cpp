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
            VectorizerEnvironment& environment) :
        _environment(environment)
    {
    }

    void VectorizerVisitorExpression::symbol_type_promotion(
            const Nodecl::Symbol& n)
    {
        TL::Symbol tl_sym = n.get_symbol();
        TL::Type tl_sym_type = tl_sym.get_type().no_ref();
        TL::Type vector_type = tl_sym_type;

        //TL::Symbol
        if (tl_sym_type.is_mask())
        {
            VECTORIZATION_DEBUG()
            {
                fprintf(stderr,"VECTORIZER: '%s' mask type vectorization "\
                        "(size %d)\n", n.prettyprint().c_str(),
                       vector_type.get_mask_num_elements());
            }
        }
        else if (tl_sym_type.is_scalar_type())
        {
            vector_type = Utils::get_qualified_vector_to(
                tl_sym_type,
                _environment._vec_isa_desc.get_vec_factor_for_type(
                    tl_sym_type, _environment._vec_factor));

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
        }

        //Nodecl::Symbol
        Nodecl::Symbol new_sym = Nodecl::Symbol::make(tl_sym, n.get_locus());
        new_sym.set_type(vector_type.get_lvalue_reference_to());

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
    void VectorizerVisitorExpression::visit_binary_op(const ScalarNode& n,
            const bool returns_mask_type)
    {
        if(!((n.template is<Nodecl::Add>() || n.template is<Nodecl::Minus>()) &&
                    process_fmul_op(n)))
        {
            Nodecl::NodeclBase mask = Utils::get_proper_mask(
                    _environment._mask_list.back());

            TL::Type n_type = n.get_type();
            Nodecl::NodeclBase lhs = n.get_lhs();
            Nodecl::NodeclBase rhs = n.get_rhs();

            walk(lhs);
            walk(rhs);

            unsigned int isa_vec_factor
                = _environment._vec_isa_desc.get_vec_factor_for_type(
                    n_type, _environment._vec_factor);

            TL::Type vector_type
                = returns_mask_type ?
                      TL::Type::get_mask_type(isa_vec_factor) :
                      Utils::get_qualified_vector_to(n.get_type(),
                                                     isa_vec_factor);

            VectorNode vector_node = VectorNode::make(lhs.shallow_copy(),
                                                      rhs.shallow_copy(),
                                                      mask,
                                                      vector_type,
                                                      n.get_locus());

            if (n.is_constant())
                vector_node.set_constant(const_value_make_vector_from_scalar(
                    isa_vec_factor, n.get_constant()));

            n.replace(vector_node);
        }
    }

    template <typename ScalarNode, typename VectorRegularNode, typename VectorMaskNode>
    void VectorizerVisitorExpression::visit_bitwise_binary_op(const ScalarNode& n)
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

            bool returns_mask_type;
            if (lhs.get_type().is_mask() && rhs.get_type().is_mask())
            {
                returns_mask_type = true;
            }
            else if (!lhs.get_type().is_mask() && !rhs.get_type().is_mask())
            {
                returns_mask_type = false;
            }
            else
            {
                internal_error("Vectorizer: Bitwise binary operation with wrong data types: %s",
                        n.prettyprint().c_str());
            }
               
            unsigned int isa_vec_factor
                = _environment._vec_isa_desc.get_vec_factor_for_type(
                    n.get_type(), _environment._vec_factor);

            TL::Type vector_type
                = returns_mask_type ?
                      TL::Type::get_mask_type(isa_vec_factor) :
                      Utils::get_qualified_vector_to(n.get_type(),
                                                     isa_vec_factor);

            if (returns_mask_type)
            {
                VectorMaskNode vector_node =
                    VectorMaskNode::make(
                            lhs.shallow_copy(),
                            rhs.shallow_copy(),
                            vector_type,
                            n.get_locus());

                if (n.is_constant())
                    vector_node.set_constant(
                            const_value_make_vector_from_scalar(
                                isa_vec_factor,
                                n.get_constant()));

                n.replace(vector_node);
            }
            else
            {
                VectorRegularNode vector_node =
                    VectorRegularNode::make(
                            lhs.shallow_copy(),
                            rhs.shallow_copy(),
                            mask,
                            vector_type,
                            n.get_locus());

                if (n.is_constant())
                    vector_node.set_constant(
                            const_value_make_vector_from_scalar(
                                isa_vec_factor,
                                n.get_constant()));

                n.replace(vector_node);
            }
        }
    }

    void VectorizerVisitorExpression::visit(const Nodecl::Add& n)
    {
        visit_binary_op<Nodecl::Add, Nodecl::VectorAdd>(n, false /* returns_mask_type */);
    }

    void VectorizerVisitorExpression::visit(const Nodecl::Minus& n)
    {
        visit_binary_op<Nodecl::Minus, Nodecl::VectorMinus>(n, false /* returns_mask_type */);
    }

    void VectorizerVisitorExpression::visit(const Nodecl::Mul& n)
    {
        visit_binary_op<Nodecl::Mul, Nodecl::VectorMul>(n, false /* returns_mask_type */);
    }

    void VectorizerVisitorExpression::visit(const Nodecl::Div& n)
    {
        visit_binary_op<Nodecl::Div, Nodecl::VectorDiv>(n, false /* returns_mask_type */);
    }

    void VectorizerVisitorExpression::visit(const Nodecl::Mod& n)
    {
        visit_binary_op<Nodecl::Mod, Nodecl::VectorMod>(n, false /* returns_mask_type */);
    }

    void VectorizerVisitorExpression::visit(const Nodecl::Neg& n)
    {
        Nodecl::NodeclBase mask = Utils::get_proper_mask(
                _environment._mask_list.back());

        Nodecl::NodeclBase rhs = n.get_rhs();

        TL::Type scalar_type = n.get_type();
        unsigned int isa_vec_factor
            = _environment._vec_isa_desc.get_vec_factor_for_type(
                n.get_type(), _environment._vec_factor);
        TL::Type vector_type
            = Utils::get_qualified_vector_to(scalar_type, isa_vec_factor);

        if (rhs.is<Nodecl::IntegerLiteral>() || // -1
                rhs.is<Nodecl::FloatingLiteral>())
        {
            Nodecl::VectorPromotion vector_prom =
                Nodecl::VectorPromotion::make(
                        n.shallow_copy(),
                        mask,
                        vector_type,
                        n.get_locus());

            vector_prom.set_constant(const_value_make_vector_from_scalar(
                        isa_vec_factor, 
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
                        vector_type,
                        n.get_locus());

            if(n.is_constant())
                vector_neg.set_constant(const_value_make_vector_from_scalar(
                            isa_vec_factor,
                            n.get_constant()));

            n.replace(vector_neg);
        }
    }

    void VectorizerVisitorExpression::visit(const Nodecl::LowerThan& n)
    {
        visit_binary_op<Nodecl::LowerThan, Nodecl::VectorLowerThan>(n, true /* returns_mask_type */);
    }

    void VectorizerVisitorExpression::visit(const Nodecl::LowerOrEqualThan& n)
    {
        visit_binary_op<Nodecl::LowerOrEqualThan,
            Nodecl::VectorLowerOrEqualThan>(n, true /* returns_mask_type */);
    }

    void VectorizerVisitorExpression::visit(const Nodecl::GreaterThan& n)
    {
        visit_binary_op<Nodecl::GreaterThan, Nodecl::VectorGreaterThan>(n, true /* returns_mask_type */);
    }

    void VectorizerVisitorExpression::visit(const Nodecl::GreaterOrEqualThan& n)
    {
        visit_binary_op<Nodecl::GreaterOrEqualThan,
            Nodecl::VectorGreaterOrEqualThan>(n, true /* returns_mask_type */);
    }

    void VectorizerVisitorExpression::visit(const Nodecl::Equal& n)
    {
        visit_binary_op<Nodecl::Equal, Nodecl::VectorEqual>(n, true /* returns_mask_type */);
    }

    void VectorizerVisitorExpression::visit(const Nodecl::Different& n)
    {
        visit_binary_op<Nodecl::Different, Nodecl::VectorDifferent>(n, true /* returns_mask_type */);
    }

    void VectorizerVisitorExpression::visit(const Nodecl::BitwiseAnd& n)
    {
        visit_bitwise_binary_op<Nodecl::BitwiseAnd, Nodecl::VectorBitwiseAnd, Nodecl::VectorMaskAnd>(n);
    }

    void VectorizerVisitorExpression::visit(const Nodecl::BitwiseOr& n)
    {
        visit_bitwise_binary_op<Nodecl::BitwiseOr, Nodecl::VectorBitwiseOr, Nodecl::VectorMaskOr>(n);
    }

    void VectorizerVisitorExpression::visit(const Nodecl::BitwiseShl& n)
    {
        visit_binary_op<Nodecl::BitwiseShl, Nodecl::VectorBitwiseShl>(n, false /* returns_mask_type */);
    }

    void VectorizerVisitorExpression::visit(const Nodecl::ArithmeticShr& n)
    {
        visit_binary_op<Nodecl::ArithmeticShr, Nodecl::VectorArithmeticShr>(n, false /* returns_mask_type */);
    }

    void VectorizerVisitorExpression::visit(const Nodecl::BitwiseShr& n)
    {
        visit_binary_op<Nodecl::BitwiseShr, Nodecl::VectorBitwiseShr>(n, false /* returns_mask_type */);
    }

    void VectorizerVisitorExpression::visit(const Nodecl::LogicalAnd& n)
    {
        visit_binary_op<Nodecl::LogicalAnd, Nodecl::VectorLogicalAnd>(n, false /* returns_mask_type */);
    }

    void VectorizerVisitorExpression::visit(const Nodecl::LogicalOr& n)
    {
        visit_binary_op<Nodecl::LogicalOr, Nodecl::VectorLogicalOr>(n, false /* returns_mask_type */);
    }

    void VectorizerVisitorExpression::vectorize_regular_class_member_access(const Nodecl::ClassMemberAccess &n)
    {
        Nodecl::NodeclBase class_object = n.get_lhs();
        Nodecl::NodeclBase member = n.get_member();

        TL::Type class_type = class_object.get_type().no_ref().get_unqualified_type();
        TL::Type vector_class_type = Utils::get_class_of_vector_fields_for_isa(
                class_type,
                _environment._vec_factor,
                _environment._vec_isa_desc);

        VECTORIZATION_DEBUG()
        {
            std::cerr << "TYPE " << vector_class_type.print_declarator() << std::endl;
        }
        Utils::class_of_vector_field_map_t field_map = Utils::class_of_vector_fields_get_map_field(vector_class_type);

        TL::Symbol orig_member = member.get_symbol();
        ERROR_CONDITION(!orig_member.is_valid(), "Expecting a symbol here", 0);

        TL::Symbol new_member = field_map[orig_member];
        ERROR_CONDITION(!new_member.is_valid(), "Field has not been mapped", 0);


        Nodecl::NodeclBase new_lhs = class_object.shallow_copy();
        new_lhs.set_type(vector_class_type); // Fix LHS

        Nodecl::NodeclBase new_rhs = new_member.make_nodecl(/* is_lvalue_ref */ true);

        Nodecl::NodeclBase new_class_member_acces = 
            Nodecl::ClassMemberAccess::make(
                    new_lhs,
                    new_rhs,
                    Nodecl::NodeclBase::null(), // member-access-form 
                    new_member.get_type().get_lvalue_reference_to(),
                    n.get_locus());

        n.replace(new_class_member_acces);
    }

    void VectorizerVisitorExpression::visit(
            const Nodecl::ConditionalExpression& n)
    {
        Nodecl::NodeclBase condition = n.get_condition();

        unsigned int isa_vec_factor
            = _environment._vec_isa_desc.get_vec_factor_for_type(
                n.get_type(), _environment._vec_factor);

        walk(condition);

        if(_environment._vec_isa_desc.support_masking())
        {
            TL::Type mask_type = TL::Type::get_mask_type(isa_vec_factor);

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
                            mask_type,
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
                            isa_vec_factor,
                            true /*ref_type*/);

                Nodecl::NodeclBase true_mask_value =
                    Nodecl::VectorMaskAnd::make(
                            prev_mask.shallow_copy(),
                            condition.shallow_copy(),
                            mask_type.no_ref(),
                            n.get_locus());

                Nodecl::ExpressionStatement true_mask_exp =
                    Nodecl::ExpressionStatement::make(
                            Nodecl::VectorMaskAssignment::make(
                                true_mask_nodecl_sym,
                                true_mask_value,
                                mask_type,
                                n.get_locus()));

                // Visit True
                _environment._mask_list.push_back(
                        true_mask_nodecl_sym.shallow_copy());
                walk(n.get_true());
                _environment._mask_list.pop_back();

                CXX_LANGUAGE()
                {
                    n.prepend_sibling(
                            Nodecl::CxxDef::make(
                                Nodecl::NodeclBase::null(),
                                true_mask_nodecl_sym.get_symbol(),
                                true_mask_nodecl_sym.get_locus()));
                }
                n.prepend_sibling(true_mask_exp);


                // False Mask
                Nodecl::NodeclBase false_mask_nodecl_sym =
                    Utils::get_new_mask_symbol(
                            _environment._analysis_simd_scope, 
                            isa_vec_factor,
                            true /*ref_type*/);

                Nodecl::NodeclBase false_mask_value =
                    Nodecl::VectorMaskAnd2Not::make(
                            prev_mask.shallow_copy(),
                            condition.shallow_copy(),
                            mask_type,
                            n.get_locus());

                Nodecl::ExpressionStatement else_mask_exp =
                    Nodecl::ExpressionStatement::make(
                            Nodecl::VectorMaskAssignment::make(
                                false_mask_nodecl_sym.shallow_copy(),
                                false_mask_value,
                                mask_type,
                                n.get_locus()));

                // Visit False
                _environment._mask_list.push_back(false_mask_nodecl_sym);
                walk(n.get_false());
                _environment._mask_list.pop_back();

                CXX_LANGUAGE()
                {
                    n.prepend_sibling(
                            Nodecl::CxxDef::make(
                                Nodecl::NodeclBase::null(),
                                false_mask_nodecl_sym.get_symbol(),
                                false_mask_nodecl_sym.get_locus()));
                }
                n.prepend_sibling(else_mask_exp);
            }
        }
        // NO MASKING SUPPORT
        else
        {
            walk(n.get_true());
            walk(n.get_false());
        }

        const Nodecl::VectorConditionalExpression vector_cond
            = Nodecl::VectorConditionalExpression::make(
                condition.shallow_copy(),
                n.get_true().shallow_copy(),
                n.get_false().shallow_copy(),
                Utils::get_qualified_vector_to(n.get_type(), isa_vec_factor),
                n.get_locus());

        n.replace(vector_cond);
    }

    Nodecl::NodeclBase VectorizerVisitorExpression::get_memory_vector_read(const Nodecl::NodeclBase& n)
    {
        Nodecl::NodeclBase mask = Utils::get_proper_mask(
                _environment._mask_list.back());

        TL::Type n_type = n.get_type().no_ref();
        unsigned int isa_vec_factor
            = _environment._vec_isa_desc.get_vec_factor_for_type(
                n_type, _environment._vec_factor);
        TL::Type vector_type = Utils::get_qualified_vector_to(
            n_type, isa_vec_factor);

        Nodecl::NodeclBase gather_copy;
        Nodecl::NodeclBase base;
        Nodecl::NodeclBase strides;
        Nodecl::NodeclBase vaccess;

        if (n.is<Nodecl::ArraySubscript>())
        {
            Nodecl::ArraySubscript array = n.as<Nodecl::ArraySubscript>();
            Nodecl::NodeclBase subscripted = array.get_subscripted().no_conv();
            vaccess = Nodecl::Reference::make(
                    n.no_conv().shallow_copy(),
                    n_type.get_pointer_to(),
                    n.get_locus());

            ERROR_CONDITION(n.as<Nodecl::ArraySubscript>().
                    get_subscripts().as<Nodecl::List>().size() > 1,
                    "Vectorizer: ArraySubscript has not been linearized: %s",
                    n.prettyprint().c_str());

            // Get a scatter for real scatter or unaligned store extra flag
            gather_copy = Vectorizer::_vectorizer_analysis->shallow_copy(array);
            base = gather_copy.as<Nodecl::ArraySubscript>().get_subscripted();
            // The array must have been linearized
            strides = gather_copy.as<Nodecl::ArraySubscript>().
                get_subscripts().as<Nodecl::List>().front();

            // Vectorize strides
            walk(strides);
        }
        else if (n.is<Nodecl::Dereference>())
        {
            // Get a scatter for real scatter or unaligned store extra flag
            Nodecl::NodeclBase deref_element = n.as<Nodecl::Dereference>().get_rhs();
            vaccess = deref_element;

            // TODO
            bool is_linear = Vectorizer::_vectorizer_analysis->
                    is_linear(_environment._analysis_simd_scope, vaccess.no_conv());
            bool is_adjacent = Vectorizer::_vectorizer_analysis->
                    is_adjacent_access(_environment._analysis_simd_scope, n);

            if (is_linear && is_adjacent)
            {
                gather_copy = Vectorizer::_vectorizer_analysis->shallow_copy(deref_element);
                base = gather_copy;

                // Computing IV offset {0, 1, 2, 3}
                Nodecl::NodeclBase linear_increment = Vectorizer::
                    _vectorizer_analysis->get_linear_step(_environment._analysis_simd_scope, vaccess);

                if (linear_increment.is_constant())
                {
                    // If *p is adjacent, the step will be sizeof(*p)
                    int iv_increment = const_value_cast_to_4(linear_increment.get_constant()) / n_type.get_size();
                    
                    // Offset list
                    Nodecl::List offset_list
                        = Vectorization::Utils::get_vector_offset_list(
                            0 /*start value*/,
                            iv_increment,
                            isa_vec_factor);

                    // VectorLiteral {0, 1, 2, ..., VL-1}
                    Nodecl::VectorLiteral offset_vector_literal
                        = Nodecl::VectorLiteral::make(
                            offset_list,
                            Utils::get_null_mask(),
                            Utils::get_qualified_vector_to(
                                TL::Type::get_int_type(), isa_vec_factor),
                            n.get_locus());

                    offset_vector_literal.set_constant(
                            offset_list.get_constant());
                    
                    strides = offset_vector_literal;
                }
                else
                {
                    internal_error("Vectorizer: Dereferences with step %s are not supported yet: %s",
                            linear_increment.prettyprint().c_str(),
                            n.prettyprint().c_str());
                }
            }
            else
            {
                internal_error("Vectorizer: Linear (%d) Adjacent (%d) Dereferences are not supported yet: %s",
                        is_linear, is_adjacent,
                        n.prettyprint().c_str());
            }
        }


        Nodecl::VectorGather vector_gather =
            Nodecl::VectorGather::make(
                    base,
                    strides,
                    mask.shallow_copy(),
                    vector_type,
                    n.get_locus());

        vector_gather.set_constant(n.get_constant()); // TODO?

        // Adjacent access
        if (Vectorizer::_vectorizer_analysis->is_adjacent_access(
                    _environment._analysis_simd_scope, n))
        {
            Nodecl::List load_flags;
            load_flags.append(vector_gather);

            VECTORIZATION_DEBUG()
            {
                fprintf(stderr, "VECTORIZER: Load   '%s'",
                        n.prettyprint().c_str());
            }

            // Aligned
            int alignment_output;
            if (Vectorizer::_vectorizer_analysis->is_simd_aligned_access(
                    _environment._analysis_simd_scope,
                    n,
                    _environment._aligned_symbols_map,
                    _environment._suitable_exprs_list,
                    _environment._vec_factor,
                    _environment._vec_isa_desc.get_memory_alignment_in_bytes(),
                    alignment_output))
            {
                load_flags.append(Nodecl::AlignedFlag::make());

                VECTORIZATION_DEBUG()
                {
                    fprintf(stderr, " (aligned)");
                }

            }
            else
            {
                ERROR_CONDITION(Vectorizer::_unaligned_accesses_disabled,
                        "%s is an unaligned vector load. Unaligned accesses are disabled",
                        n.prettyprint().c_str());
                if (alignment_output != -1) // a known unaligned load
                {
                    load_flags.append(Nodecl::AlignmentInfo::make(
                                const_value_get_signed_int(alignment_output)));

                    VECTORIZATION_DEBUG()
                    {
                        fprintf(stderr, " (alignment info = %d)",
                                alignment_output);
                    }
                }
            }

            VECTORIZATION_DEBUG()
            {
                fprintf(stderr, "\n");
            }

            Nodecl::VectorLoad vector_load =
                Nodecl::VectorLoad::make(
                        vaccess.shallow_copy(),
                        mask,
                        load_flags,
                        vector_type,
                        n.get_locus());

            vector_load.set_constant(n.get_constant());

            return vector_load;
        }
        else // Vector Gather
        {
            ERROR_CONDITION(Vectorizer::_gathers_scatters_disabled,
                    "%s is a non-adjacent vector load. Gather/scatter are disabled.",
                    n.prettyprint().c_str());

            VECTORIZATION_DEBUG()
            {
                fprintf(stderr, "VECTORIZER: Gather '%s'\n", 
                        n.prettyprint().c_str());
            }

            return vector_gather;
        }
    }

    Nodecl::NodeclBase VectorizerVisitorExpression::get_memory_vector_write(const Nodecl::NodeclBase& lhs,
            const Nodecl::NodeclBase& rhs,
            const Nodecl::NodeclBase& mask,
            const TL::Type type)
    {
        TL::Type assignment_type = type.no_ref();
        TL::Type lhs_type = lhs.get_type().no_ref();

        unsigned int isa_vec_factor
            = _environment._vec_isa_desc.get_vec_factor_for_type(
                lhs_type, _environment._vec_factor);
        TL::Type vector_type
            = Utils::get_qualified_vector_to(assignment_type,
                                            isa_vec_factor);
        Nodecl::Symbol lhs_symbol;

        Nodecl::NodeclBase lhs_scatter_copy;
        Nodecl::NodeclBase base;
        Nodecl::NodeclBase strides;
        Nodecl::NodeclBase lhs_vaccess;

        walk(rhs);

        if (lhs.is<Nodecl::ArraySubscript>())
        {
            Nodecl::ArraySubscript array = lhs.as<Nodecl::ArraySubscript>();
            Nodecl::NodeclBase subscripted = array.get_subscripted().no_conv();
            lhs_vaccess = Nodecl::Reference::make(
                    lhs.no_conv().shallow_copy(),
                    lhs_type.get_pointer_to(),
                    lhs.get_locus());

            ERROR_CONDITION(array.get_subscripts().as<Nodecl::List>().size() > 1,
                    "Vectorizer: ArraySubscript has not been linearized: %s",
                    lhs.prettyprint().c_str());


            while (subscripted.is<Nodecl::ArraySubscript>())
            {
                subscripted = subscripted.as<Nodecl::ArraySubscript>().
                    get_subscripted().no_conv();
            }

            ERROR_CONDITION(!subscripted.is<Nodecl::Symbol>(),
                    "Vectorizer: ArraySubscript form not supported yet: %s",
                    lhs.prettyprint().c_str());

            lhs_symbol = subscripted.as<Nodecl::Symbol>();

            // Get a scatter for real scatter or unaligned store extra flag
            lhs_scatter_copy = Vectorizer::_vectorizer_analysis->shallow_copy(array);

            if (subscripted.is<Nodecl::Symbol>())
            {
                lhs_symbol = subscripted.as<Nodecl::Symbol>();

                base = lhs_scatter_copy.as<Nodecl::ArraySubscript>().get_subscripted();
                // The array must have been linearized
                strides = lhs_scatter_copy.as<Nodecl::ArraySubscript>().
                    get_subscripts().as<Nodecl::List>().front();
            }
            else if (subscripted.is<Nodecl::ArraySubscript>())
            {
                base = lhs_scatter_copy.as<Nodecl::ArraySubscript>().get_subscripted();
                strides = lhs_scatter_copy.as<Nodecl::ArraySubscript>().
                    get_subscripts().as<Nodecl::List>().front();

                while (subscripted.is<Nodecl::ArraySubscript>() &&
                        !Vectorizer::_vectorizer_analysis->
                        is_uniform(_environment._analysis_simd_scope, subscripted, subscripted))
                {
                    subscripted = subscripted.as<Nodecl::ArraySubscript>().get_subscripted();
                    
                    // TODO: Fix. Gather/statter is not ready for multi-dimensional arrays
                    // The array must have been linearized
                    strides = base.as<Nodecl::ArraySubscript>().
                        get_subscripts().as<Nodecl::List>().front();
                   
                    base = base.as<Nodecl::ArraySubscript>().get_subscripted();
                }

                Nodecl::NodeclBase subscripted_sym = subscripted;

                while (subscripted_sym.is<Nodecl::ArraySubscript>())
                {
                    subscripted_sym = subscripted_sym.as<Nodecl::ArraySubscript>().
                        get_subscripted().no_conv();
                }

                if (subscripted_sym.is<Nodecl::Symbol>())
                {
                    lhs_symbol = subscripted_sym.as<Nodecl::Symbol>();
                }
                else
                {
                    internal_error("Vectorizer: Symbol not found in base of ArraySubscript. ArraySubscript form not supported yet: %s",
                            lhs.prettyprint().c_str());
                }
            }
            else
            {
                internal_error("Vectorizer: ArraySubscript form not supported yet: %s",
                    lhs.prettyprint().c_str());
            }

            // Vectorize strides
            walk(strides);
        }
        else if (lhs.is<Nodecl::Dereference>())
        {
            // Get a scatter for real scatter or unaligned store extra flag
            Nodecl::NodeclBase deref_element = lhs.as<Nodecl::Dereference>().get_rhs();
            lhs_vaccess = deref_element;

            // TODO
            bool is_linear = Vectorizer::_vectorizer_analysis->
                    is_linear(_environment._analysis_simd_scope, lhs_vaccess.no_conv());
            bool is_adjacent = Vectorizer::_vectorizer_analysis->
                    is_adjacent_access(_environment._analysis_simd_scope, lhs);

            if (is_linear && is_adjacent)
            {
                lhs_scatter_copy = Vectorizer::_vectorizer_analysis->shallow_copy(deref_element);
                base = lhs_scatter_copy;

                // Computing IV offset {0, 1, 2, 3}
                Nodecl::NodeclBase linear_increment = Vectorizer::
                    _vectorizer_analysis->get_linear_step(_environment._analysis_simd_scope, lhs_vaccess);

                if (linear_increment.is_constant())
                {
                    // If *p is adjacent, the step will be sizeof(*p)
                    int iv_increment = const_value_cast_to_4(linear_increment.get_constant()) / lhs_type.get_size();
                    
                    // Offset list
                    Nodecl::List offset_list
                        = Vectorization::Utils::get_vector_offset_list(
                            0 /*start value*/,
                            iv_increment,
                            isa_vec_factor);

                    // VectorLiteral {0, 1, 2, ..., VL-1}
                    Nodecl::VectorLiteral offset_vector_literal =
                        Nodecl::VectorLiteral::make(
                                offset_list,
                                Utils::get_null_mask(),
                                Utils::get_qualified_vector_to(TL::Type::get_int_type(),
                                    isa_vec_factor),
                                lhs.get_locus());

                    offset_vector_literal.set_constant(
                            offset_list.get_constant());
                    
                    strides = offset_vector_literal;
                }
                else
                {
                    internal_error("Vectorizer: Dereferences with step %s are not supported yet: %s",
                            linear_increment.prettyprint().c_str(),
                            lhs.prettyprint().c_str());
                }
            }
            else
            {
                internal_error("Vectorizer: Linear (%d) Adjacent (%d) Dereferences are not supported yet: %s",
                        is_linear, is_adjacent,
                        lhs.prettyprint().c_str());
            }
        }


        const Nodecl::VectorScatter vector_scatter =
            Nodecl::VectorScatter::make(
                    base,
                    strides,
                    rhs.shallow_copy(),
                    mask.shallow_copy(),
                    vector_type,
                    lhs.get_locus());


        // Adjacent access
        if(Vectorizer::_vectorizer_analysis->
                is_adjacent_access(_environment._analysis_simd_scope, lhs))
        {
            TL::Type basic_type = lhs_type;
            if (basic_type.is_lvalue_reference())
            {
                basic_type = basic_type.references_to();
            }

            map_tlsym_objlist_t::const_iterator nontemporal_it =
                _environment._nontemporal_exprs_map.find(
                        lhs_symbol.get_symbol());

            bool nontemporal_store = (nontemporal_it !=
                    _environment._nontemporal_exprs_map.end());

            Nodecl::List store_flags;
            store_flags.append(vector_scatter);

            VECTORIZATION_DEBUG()
            {
                fprintf(stderr, "VECTORIZER: Store '%s'",
                        lhs.prettyprint().c_str());
            }


            // Aligned
            int alignment_output;
            if (Vectorizer::_vectorizer_analysis->is_simd_aligned_access(
                    _environment._analysis_simd_scope,
                    lhs,
                    _environment._aligned_symbols_map,
                    _environment._suitable_exprs_list,
                    _environment._vec_factor,
                    _environment._vec_isa_desc.get_memory_alignment_in_bytes(),
                    alignment_output))
            {
                store_flags.append(Nodecl::AlignedFlag::make());

                VECTORIZATION_DEBUG()
                {
                    fprintf(stderr, " (aligned)");
                }
            }
            else
            {
                ERROR_CONDITION(Vectorizer::_unaligned_accesses_disabled,
                        "%s is an unaligned vector store. Unaligned accesses are disabled",
                        lhs.prettyprint().c_str());
                if (alignment_output != -1) // a known unaligned store
                {
                    store_flags.append(Nodecl::AlignmentInfo::make(
                                const_value_get_signed_int(alignment_output)));

                    VECTORIZATION_DEBUG()
                    {
                        fprintf(stderr, " (alignment info = %d)",
                                alignment_output);
                    }
                }
            }

            if (nontemporal_store)
            {
                store_flags.append(Nodecl::NontemporalFlag::make());

                VECTORIZATION_DEBUG()
                {
                    fprintf(stderr, " (nontemporal)");
                }

                Nodecl::List nontemporal_flags = Nodecl::List::make(nontemporal_it->second);

                for (auto tflag : nontemporal_flags)
                {
                    std::cerr << "--> " << tflag.prettyprint() << std::endl;
                }
                
                bool relaxed = !nontemporal_flags.find_first<Nodecl::RelaxedFlag>().is_null();
                bool evict = !nontemporal_flags.find_first<Nodecl::EvictFlag>().is_null();
                
                if (relaxed) 
                {
                    fprintf(stderr, "RELAXED\n");

                    VECTORIZATION_DEBUG()
                    {
                        fprintf(stderr, " (relaxed)");
                    }

                    store_flags.append(Nodecl::RelaxedFlag::make());
                }

                if(evict)
                {
                    VECTORIZATION_DEBUG()
                    {
                        fprintf(stderr, " (evict)");
                    }

                    store_flags.append(Nodecl::EvictFlag::make());
                }
            }

            VECTORIZATION_DEBUG()
            {
                fprintf(stderr, "\n");
            }

            const Nodecl::VectorStore vector_store =
                Nodecl::VectorStore::make(
                        lhs_vaccess.shallow_copy(),
                        rhs.shallow_copy(),
                        mask.shallow_copy(),
                        store_flags,
                        vector_type,
                        lhs.get_locus());

            return vector_store;
        }
        else // Vector Scatter
        {
            ERROR_CONDITION(Vectorizer::_gathers_scatters_disabled,
                    "%s is a non-adjacent vector store. Gather/scatter are disabled.",
                    lhs.prettyprint().c_str());

            VECTORIZATION_DEBUG()
            {
                fprintf(stderr, "VECTORIZER: Scatter '%s'\n",
                        lhs.prettyprint().c_str());
            }

            return vector_scatter;
        }
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

        unsigned int isa_vec_factor
            = _environment._vec_isa_desc.get_vec_factor_for_type(
                assignment_type, _environment._vec_factor);

        // If lhs or rhs aren't uniform, vectorize
        if((lhs.is<Nodecl::Symbol>() && 
                    lhs.as<Nodecl::Symbol>().get_symbol().get_type().is_vector()) ||
                //TODO: is_uniform on lhs won't be eventually necessary
                !Vectorizer::_vectorizer_analysis->
                is_uniform(_environment._analysis_simd_scope, lhs, lhs) ||
                !Vectorizer::_vectorizer_analysis->
                is_uniform(_environment._analysis_simd_scope, rhs, rhs))
        {
            // Computing new vector type
            TL::Type vector_type
                = Utils::get_qualified_vector_to(assignment_type,
                        isa_vec_factor);

            // IV vectorization: i = i + 3 --> i = i + (vec_factor * 3)
            if(Vectorizer::_vectorizer_analysis->
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
                                            _environment._vec_factor),
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
                                            _environment._vec_factor),
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
                        fatal_error("Vectorizer: This linear update is not supported yet"\
                                "(%s).", n.prettyprint().c_str());
                    }

                    /*
                       Nodecl::NodeclBase step = Vectorizer::
                       _vectorizer_analysis->get_induction_variable_increment(
                       _environment._analysis_simd_scope, lhs);

                    //Vectorizer::_vectorizer_analysis->get_induction_variable_increment(
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
                    _environment._vec_factor),
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
                // Vector Store
                // Constant ArraySubscript, nothing to do
                if (Vectorizer::_vectorizer_analysis->
                        is_uniform(_environment._analysis_simd_scope,
                            lhs, lhs))
                {
                    VECTORIZATION_DEBUG()
                    {
                        std::cerr << "Vectorizer: Constant store: "
                            << lhs.prettyprint()
                            << std::endl;
                    }

                    fatal_error("Vectorizer: Extract operation is not "\
                            "supported yet (%s).", lhs.prettyprint().c_str());

                }
                // ArraySubscript indexed by nested IV, nothing to do
                /*
                   else if (Vectorizer::_vectorizer_analysis->
                   is_nested_induction_variable_dependent_access(
                   _environment, lhs) &&
                   !Vectorizer::_vectorizer_analysis->
                   is_induction_variable_dependent_expression(
                   _environment._analysis_simd_scope, lhs))
                   {
                   std::cerr << "Nested IV dependent store: " << lhs.prettyprint()
                   << std::endl;
                   fatal_error("Vectorizer: Extract operation is not "\
                   "supported yet (%s).", lhs.prettyprint().c_str());
                   }
                 */
                else
                {
                    Nodecl::NodeclBase memory_write = 
                        get_memory_vector_write(lhs, rhs, mask, assignment_type);

                    n.replace(memory_write);
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
                                Nodecl::NodeclBase::null(), // HasBeenDefinedFlag
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
                if (class_object.is<Nodecl::ArraySubscript>())
                {

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

                    if(lhs.is<Nodecl::VectorStore>())
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
                        fatal_error("Vectorizer: ClassMemberAccess type is not "\
                                "supported yet: '%s'", n.prettyprint().c_str());
                    }
                }
                else if (class_object.is<Nodecl::Symbol>())
                {
                    if (Vectorizer::_vectorizer_analysis
                            ->is_uniform(_environment._analysis_simd_scope, class_object, class_object)
                            && !Utils::is_class_of_vector_fields(class_object.get_symbol().get_type()))
                    {
                        // Do nothing in this case
                    }
                    else
                    {
                        vectorize_regular_class_member_access(lhs.as<Nodecl::ClassMemberAccess>());
                    }
                    walk(rhs);
                }
                else
                {
                    fatal_error("Vectorizer: ClassMemberAccess type is not "\
                            "supported yet: '%s'", n.prettyprint().c_str());
                }
            }
            else if (lhs.is<Nodecl::Dereference>())
            {
                n.replace(get_memory_vector_write(lhs, rhs, mask, assignment_type));
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
        walk(n.get_nest());

        // If false, someone (TL::Symbol) moves/replaces my tree.
        // Therefore do nothing, I'm no longer a Conversion!!
        if (!n.is<Nodecl::Conversion>())
            return;

        Nodecl::NodeclBase nest = n.get_nest();

        TL::Type src_vector_type = nest.get_type();
        TL::Type src_vector_type_noref = src_vector_type.no_ref();
        TL::Type dst_type = n.get_type();

        if (src_vector_type_noref.is_vector())
        {
            // Remove lvalue conversions.
            // In a vector code they are explicit loads ops.
            if (src_vector_type_noref.basic_type().is_same_type(dst_type) &&
                    // FIXME - is this next check too restrictive?
                    (nest.is<Nodecl::VectorLoad>() ||
                     nest.is<Nodecl::VectorGather>()))
            {
                // There is no conversion
                n.replace(nest);
            }
            else if (src_vector_type_noref.basic_type().is_same_type(dst_type))
            {
                // There is no conversion
                n.replace(nest);
            }
            else
            {
                unsigned int isa_vec_factor
                    = _environment._vec_isa_desc.get_vec_factor_for_type(
                        dst_type, _environment._vec_factor);
                TL::Type dst_vec_type;

                if (dst_type.is_bool())
                    dst_vec_type = TL::Type::get_mask_type(
                            isa_vec_factor);
                else
                    dst_vec_type = Utils::get_qualified_vector_to(
                        dst_type,
                        isa_vec_factor);

                Nodecl::VectorConversion vector_conv =
                    Nodecl::VectorConversion::make(
                            n.get_nest().shallow_copy(),
                            mask,
                            dst_vec_type,
                            n.get_locus());

                vector_conv.set_constant(const_value_convert_to_type(
                            n.get_nest().get_constant(),
                            dst_vec_type.get_internal_type()));

                n.replace(vector_conv);
            }
        }
        else if (src_vector_type_noref.is_mask())
        {
            // Update the type of the conversion. A mask type can be generated
            // from a bool type
            if (src_vector_type.is_lvalue_reference())
            {
                Nodecl::VectorMaskConversion mask_conv
                    = Nodecl::VectorMaskConversion::make(
                        n.get_nest().shallow_copy(),
                        src_vector_type_noref,
                        n.get_locus());

                mask_conv.set_constant(n.get_nest().get_constant());
                n.replace(mask_conv);
            }
            else
            {
                // TODO: Conversion between different mask types?
                fatal_error(
                    "Vectorizer: Conversions between different mask types are "
                    "not supported yet");
            }
        }
    }

    void VectorizerVisitorExpression::visit(const Nodecl::ArraySubscript& n)
    {
        Nodecl::NodeclBase mask = Utils::get_proper_mask(
                _environment._mask_list.back());

        TL::Type basic_type = n.get_type().no_ref();

        // Vector Promotion from constant ArraySubscript
        if (Vectorizer::_vectorizer_analysis->
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

            const_value_t* const_value = encapsulated_symbol.get_constant();

            TL::Type encapsulated_symbol_type = encapsulated_symbol.get_type();

            unsigned int isa_vec_factor
                = _environment._vec_isa_desc.get_vec_factor_for_type(
                    encapsulated_symbol_type, _environment._vec_factor);

            Nodecl::VectorPromotion vector_prom = Nodecl::VectorPromotion::make(
                encapsulated_symbol.shallow_copy(),
                mask,
                Utils::get_qualified_vector_to(encapsulated_symbol_type,
                                               isa_vec_factor),
                n.get_locus());

            vector_prom.set_constant(const_value);

            encapsulated_symbol.replace(vector_prom);
        }
        /* This is no longer necessary due to the new query
        variable_is_constant_at_statement
        // Vector promotion from ArraySubscript indexed by nested IV
        else if (Vectorizer::_vectorizer_analysis->
                is_nested_induction_variable_dependent_access(
                    _environment, n) &&
                !Vectorizer::_vectorizer_analysis->
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
                            _environment._vec_factor,
        _environment._vec_isa_desc),
                        n.get_locus());

            vector_prom.set_constant(const_value);

            encapsulated_symbol.replace(vector_prom);
        }
        */
        // Cached access
        /*
                else if (_cache_enabled &&
                        _environment._vectorizer_cache.is_cached_access(n))
                {
                    std::cerr << "CACHED ACCESS: " << n.prettyprint() << " IS ";
                    n.replace(_environment._vectorizer_cache.get_load_access(n));
                    std::cerr << n.prettyprint() << std::endl;
                }
        */
        // Vector Load or Gather
        else
        {
            n.replace(get_memory_vector_read(n));
        }
    }

    bool is_compiler_node_function_call(TL::Symbol func_name)
    {
        TL::Scope global_scope(CURRENT_COMPILED_FILE->global_decl_context);
        if (func_name == global_scope.get_symbol_from_name("fabsf")
                || func_name == global_scope.get_symbol_from_name("sqrtf")
                || func_name == global_scope.get_symbol_from_name("fabs")
                || func_name == global_scope.get_symbol_from_name("sqrt")
                || func_name == global_scope.get_symbol_from_name("sincosf")
                || func_name == global_scope.get_symbol_from_name("sincos"))
        {
            return true;
        }
        return false;
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
        TL::Type vec_func_type = Utils::get_qualified_vector_to(
            call_type,
            _environment._vec_isa_desc.get_vec_factor_for_type(
                call_type, _environment._vec_factor));

        TL::Symbol func_name = called_sym.get_symbol();

        TL::Scope global_scope(CURRENT_COMPILED_FILE->global_decl_context);
        if (func_name == global_scope.get_symbol_from_name("_mm_prefetch") 
                || func_name == global_scope.get_symbol_from_name("_mm_prefetche"))
        {
            VECTORIZATION_DEBUG()
            {
                std::cerr << "Warning: preventing prefetch function call "\
                    "from being vectorized: " << n.get_locus() << std::endl;
            }

            return;
        }

        if (func_name == global_scope.get_symbol_from_name("_mm_clevict"))
        {
            VECTORIZATION_DEBUG()
            {
                std::cerr << "Warning: preventing eviction function call "\
                    "from being vectorized: " << n.get_locus() << std::endl;
            }

            return;
        }


        // Vectorizing arguments
        TL::ObjectList<Nodecl::NodeclBase> arguments_list = n.get_arguments().as<Nodecl::List>().to_object_list();
        TL::ObjectList<TL::Symbol> param_list;
        bool need_vector_function = false;

        // Get list of params;
        TL::Type function_target_type = call_type.no_ref();

        // Get the best vector version of the function available
        Nodecl::NodeclBase best_version = vec_func_versioning.get_best_version(
            func_name,
            _environment._vec_isa_desc.get_id(),
            _environment._vec_factor,
            !mask.is_null());

        bool is_svml = false;
        if (!best_version.is_null())
        {
            is_svml = std::find(vec_math_library_funcs.begin(),
                                vec_math_library_funcs.end(),
                                best_version.get_symbol())
                      != vec_math_library_funcs.end();
        }

        bool vectorize_all_arguments = false;
        if (!best_version.is_null())
        {
            if (best_version.is<Nodecl::FunctionCode>())
            {
                param_list = best_version.as<Nodecl::FunctionCode>().get_symbol().
                    get_related_symbols();

                need_vector_function = true;
                VECTORIZATION_DEBUG()
                {
                    fprintf(stderr, "func: %s\n", best_version.as<Nodecl::FunctionCode>().get_symbol().get_name().c_str());
                }
            }
            else if (best_version.is<Nodecl::Symbol>())
            {
                param_list = best_version.as<Nodecl::Symbol>().get_symbol().
                    get_related_symbols();

                need_vector_function = true;

                VECTORIZATION_DEBUG()
                {
                    fprintf(stderr, "func: %s\n", best_version.as<Nodecl::Symbol>().get_symbol().get_name().c_str());
                }
            }
            else
            {
                internal_error("Code unreachable", 0);
            }
        }
        else if (is_compiler_node_function_call(func_name))
        {
            need_vector_function = true;
            // FIXME: we are assuming that inline expanded vector functions
            // have all their arguments as vectors
            vectorize_all_arguments = true;
        }
        else
        {
            // Do nothing
        }


        if (!param_list.empty())
        {
            auto param_it = param_list.begin();
            for(const auto& argument_it : arguments_list)
            {
                // If the parameter has no vector type,
                // it will be linear or uniform in the function.
                VECTORIZATION_DEBUG()
                {
                std::cerr << "Param: " << param_it->get_name() << " " << print_declarator(param_it->get_type().get_internal_type()) << std::endl;
                }
                TL::Type param_type = param_it->get_type().no_ref();
                if (param_type.is_vector()
                        || Utils::is_class_of_vector_fields(param_type.get_unqualified_type()))
                {
                    VECTORIZATION_DEBUG()
                    {
                        std::cerr << "VECTORIZER: Vectorizing argument '" << argument_it.prettyprint()
                            << "'" << std::endl;
                    }
                    walk(argument_it);
                }
                else
                {
                    VECTORIZATION_DEBUG()
                    {
                        std::cerr << "VECTORIZER: Argument '" << argument_it.prettyprint()
                            << "' is kept scalar because is uniform or linear" << std::endl;
                    }
                }
                param_it++;
            }
        }
        else if (vectorize_all_arguments)
        {
            for(const auto& argument_it : arguments_list)
            {
                // If the parameter has no vector type,
                // it will be linear or uniform in the function.
                VECTORIZATION_DEBUG()
                {
                    std::cerr << "VECTORIZER: Vectorizing argument '" << argument_it.prettyprint()
                        << "'" << std::endl;
                }
                walk(argument_it);
            }
        }

        // auto argument_it = arguments_list.begin();
        // for(const auto& param_it : param_list)
        // {
        //     // If the parameter has no vector type,
        //     // it will be linear or uniform in the function.
        //     if (param_it.get_type().is_vector())
        //     {
        //         // VECTORIZATION_DEBUG()
        //         {
        //             std::cerr << "VECTORIZER: Vectorizing argument '" << argument_it->prettyprint()
        //                 << "'" << std::endl;
        //         }
        //         walk(*argument_it);
        //     }
        //     else
        //     {
        //         // VECTORIZATION_DEBUG()
        //         {
        //             std::cerr << "VECTORIZER: Argument '" << argument_it->prettyprint()
        //                 << "' is kept scalar because is uniform or linear" << std::endl;
        //         }
        //     }
        //     argument_it++;
        // }

        // Vectorize version only if it uses vector arguments
        // TODO: return type
        if (need_vector_function)
        {
            VECTORIZATION_DEBUG()
            {
                std::cerr << "VECTORIZER: Vectorizing function call '"
                    << func_name.get_qualified_name() << "'" << std::endl;
            }

            if (func_name == global_scope.get_symbol_from_name("fabsf") ||
                    func_name == global_scope.get_symbol_from_name("fabs"))
            {
                const Nodecl::VectorFabs vector_fabs_call
                    = Nodecl::VectorFabs::make(n.get_arguments()
                                                   .as<Nodecl::List>()
                                                   .front()
                                                   .shallow_copy(),
                                               mask,
                                               vec_func_type,
                                               n.get_locus());

                n.replace(vector_fabs_call);
            }
            else if (func_name == global_scope.get_symbol_from_name("sqrtf") ||
                    func_name == global_scope.get_symbol_from_name("sqrt"))
            {
                const Nodecl::VectorSqrt vector_sqrt_call
                    = Nodecl::VectorSqrt::make(
                        n.get_arguments()
                            .as<Nodecl::List>()
                            .front()
                            .shallow_copy(),
                        mask,
                        vec_func_type,
                        n.get_locus());

                n.replace(vector_sqrt_call);
            }
            else if (func_name == global_scope.get_symbol_from_name("sincosf"))
            {
                Nodecl::List::iterator args = n.get_arguments().
                    as<Nodecl::List>().begin();

                Nodecl::NodeclBase source = *args;
                args++;
                Nodecl::NodeclBase sin_p = *args;
                args++;
                Nodecl::NodeclBase cos_p = *args;
                args++;

                const Nodecl::VectorSincos vector_sincos_call
                    = Nodecl::VectorSincos::make(
                        source.shallow_copy(),
                        sin_p.shallow_copy(),
                        cos_p.shallow_copy(),
                        mask,
                        Utils::get_qualified_vector_to(
                            TL::Type::get_float_type(),
                            _environment._vec_isa_desc.get_vec_factor_for_type(
                                TL::Type::get_float_type(),
                                _environment._vec_factor)),
                        n.get_locus());

                n.replace(vector_sincos_call);
            }
            else //Common functions
            {
                // If _target_type and call_type have the same size, we use call_type as
                // this function should have been registered with this type
                // if (call_type.is_void() || 
                //         _environment._target_type.get_size() == call_type.get_size())
                // {
                //     function_target_type = call_type;
                //     function_target_type_size =
                //         (function_target_type.is_void() ? 1 : function_target_type.get_size());
                // }

                ERROR_CONDITION(best_version.is_null(), "Vectorizer: the best "\
                        "vector function for '%s' is null", func_name.get_qualified_name().c_str());

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
                    fatal_error("Vectorizer: %s found as vector function "\
                            "version in function versioning.",
                            ast_print_node_type(best_version.get_kind()));
                }

                Nodecl::List arguments = n.get_arguments().as<Nodecl::List>();

                if (!mask.is_null())
                {
                    arguments.append(mask.shallow_copy());
                    if (is_svml)
                    {
                        VECTORIZATION_DEBUG()
                        {
                            std::cerr << "SPECIAL CASE FOR SVML"  << new_called.prettyprint() << std::endl;
                        }
                        arguments.append(arguments[0].shallow_copy());
                    }
                }

                const Nodecl::VectorFunctionCall vector_function_call
                    = Nodecl::VectorFunctionCall::make(
                        Nodecl::FunctionCall::make(
                            new_called,
                            arguments,
                            n.get_alternate_name().shallow_copy(),
                            n.get_function_form().shallow_copy(),
                            vec_func_type,
                            n.get_locus()),
                        mask,
                        vec_func_type,
                        n.get_locus());

                n.replace(vector_function_call);
            }
        }
        else
        {
            VECTORIZATION_DEBUG()
            {
                std::cerr << "VECTORIZER: Function call '"
                    << func_name.get_qualified_name() << "' is kept scalar" << std::endl;
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
                    Vectorizer::_vectorizer_analysis->
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
            Vectorizer::_vectorizer_analysis->
            is_nested_non_reduction_basic_induction_variable(
            _environment, n)
            &&
            // Lb doesn't depend on SIMD IV and
            !Vectorizer::_vectorizer_analysis->
            iv_lb_depends_on_ivs_from_scope(
            _environment._analysis_scopes.back(),
            n,
            _environment._analysis_simd_scope)
            &&
            // Step doesn't depend on SIMD IV
            !Vectorizer::_vectorizer_analysis->
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
            _environment._vec_factor, _environment._vec_isa_desc),
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
            else if(tl_sym_type.is_vector()
                    || tl_sym_type.is_mask()
                    || Utils::is_class_of_vector_fields(tl_sym_type.get_unqualified_type()))
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
                unsigned int isa_vec_factor
                    = _environment._vec_isa_desc.get_vec_factor_for_type(
                        encapsulated_symbol_type, _environment._vec_factor);

                Nodecl::VectorPromotion vector_prom
                    = Nodecl::VectorPromotion::make(
                        encapsulated_symbol.shallow_copy(),
                        Utils::get_null_mask(),
                        Utils::get_qualified_vector_to(encapsulated_symbol_type,
                                                       isa_vec_factor),
                        n.get_locus());

                if(encapsulated_symbol.is_constant())
                    vector_prom.set_constant(
                            const_value_make_vector_from_scalar(
                                isa_vec_factor,
                                encapsulated_symbol.get_constant()));

                encapsulated_symbol.replace(vector_prom);
            }

            // Vectorize constants
            /*
               else if (Vectorizer::_vectorizer_analysis->
               variable_is_constant_at_statement(_environment._analysis_simd_scope,
               n))
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
               _environment._vec_factor, _environment._vec_isa_desc),
               n.get_locus());

               if(encapsulated_symbol.is_constant())
               vector_prom.set_constant(
               const_value_make_vector_from_scalar(
               _environment._vec_factor,
               encapsulated_symbol.get_constant()));

               encapsulated_symbol.replace(vector_prom);
               }
             */
            else
            {
                // TODO: If you are from outside of the loop -> Vector local
                // copy.
                fatal_error(
                    "Vectorizer: Loop is not vectorizable. '%s' "
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
        if (class_object.is<Nodecl::ArraySubscript>())
        {

            int access_size = n.get_type().no_ref().get_size();
            int class_size = class_object.get_type().no_ref().get_size();
            int member_offset = member.as<Nodecl::Symbol>().get_symbol().get_offset();

            //TODO a.x[i]

            // a[i].x --> Gather

            walk(class_object);

            unsigned int isa_vec_factor
                = _environment._vec_isa_desc.get_vec_factor_for_type(
                    n_original.get_type(), _environment._vec_factor);

            TL::Type vector_type = Utils::get_qualified_vector_to(
                n_original.get_type(), isa_vec_factor);

            // Gather
            if(class_object.is<Nodecl::VectorLoad>())
            {
                Nodecl::VectorLoad vload = class_object.as<Nodecl::VectorLoad>();

                Nodecl::VectorGather vector_gather = vload.get_flags().
                    as<Nodecl::List>().find_first<Nodecl::VectorGather>();

                // Add pointer casting to base --> (float *) &a[i].fp
                Nodecl::NodeclBase base = vector_gather.get_base();

                base.replace(Nodecl::Conversion::make(base.shallow_copy(),
                            n_original.get_type().no_ref().get_pointer_to()));
                base.set_text("C");

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
                vector_gather.set_type(vector_type);

                n.replace(vector_gather);
            }
            // Vector Promotion
            else if (class_object.is<Nodecl::VectorPromotion>())
            {
                Nodecl::VectorPromotion vprom = class_object.
                    as<Nodecl::VectorPromotion>();

                vprom.get_rhs().replace(n_original.shallow_copy());

                // Set new type
                vprom.set_type(vector_type);

                n.replace(vprom);
            }
            else
            {
                fatal_error("Vectorizer: ClassMemberAccess type is not "\
                        "supported yet: '%s'", n.prettyprint().c_str());
            }
        }
        else if (class_object.is<Nodecl::Symbol>())
        {
            if (Vectorizer::_vectorizer_analysis
                    ->is_uniform(_environment._analysis_simd_scope, class_object, class_object)
                    && !Utils::is_class_of_vector_fields(class_object.get_symbol().get_type()))
            {
                if (!member.get_type().no_ref().is_pointer())
                {
                    Nodecl::VectorPromotion vector_prom
                        = Nodecl::VectorPromotion::make(
                            n.shallow_copy(),
                            Utils::get_null_mask(),
                            Utils::get_qualified_vector_to(
                                n.get_type(),
                                _environment._vec_isa_desc
                                    .get_vec_factor_for_type(
                                        n.get_type(),
                                        _environment._vec_factor)),
                            n.get_locus());
                    n.replace(vector_prom);
                }
                else
                {
                    fatal_error("Vectorizer: ClassMemberAccess type is not "\
                            "supported yet: '%s'", n.prettyprint().c_str());
                }
            }
            else
            {
                vectorize_regular_class_member_access(n);
            }
        }
        else
        {
            fatal_error("Vectorizer: ClassMemberAccess type is not "\
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

        unsigned int isa_vec_factor
            = _environment._vec_isa_desc.get_vec_factor_for_type(
                n.get_type(), _environment._vec_factor);

        Nodecl::VectorPromotion vector_prom = Nodecl::VectorPromotion::make(
            n.shallow_copy(),
            Utils::get_null_mask(),
            Utils::get_qualified_vector_to(n.get_type(),
                                          isa_vec_factor),
            n.get_locus());

        vector_prom.set_constant(const_value_make_vector_from_scalar(
                    isa_vec_factor,
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

        unsigned int isa_vec_factor
            = _environment._vec_isa_desc.get_vec_factor_for_type(
                n.get_type(), _environment._vec_factor);

        Nodecl::VectorPromotion vector_prom = Nodecl::VectorPromotion::make(
            n.shallow_copy(),
            Utils::get_null_mask(),
            Utils::get_qualified_vector_to(n.get_type(), isa_vec_factor),
            n.get_locus());

        vector_prom.set_constant(const_value_make_vector_from_scalar(
                    isa_vec_factor,
                    n.get_constant()));

        n.replace(vector_prom);
    }

//    void VectorizerVisitorExpression::visit(
//            const Nodecl::ParenthesizedExpression& n)
//    {
//        walk(n.get_nest());
//
//        Nodecl::ParenthesizedExpression parent = n.shallow_copy().
//            as<Nodecl::ParenthesizedExpression>();
//
//        parent.set_type(n.get_nest().get_type());
//        if(n.is_constant())
//            parent.set_constant(const_value_make_vector_from_scalar(
//                        _environment._vec_factor,
//                        n.get_constant()));
//
//        n.replace(parent);
//    }

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
        n.replace(get_memory_vector_read(n));

        //walk(n.get_rhs());

        //const Nodecl::Dereference dereference =
        //    Nodecl::Dereference::make(
        //            n.get_rhs().shallow_copy(),
        //            Utils::get_qualified_vector_to(n.get_type(),
        //                _environment._vec_factor),
        //            n.get_locus());

        //n.replace(dereference);
    }

    void VectorizerVisitorExpression::visit(const Nodecl::VectorLaneId& n)
    {
        TL::Type n_type = n.get_type();

        unsigned int isa_vec_factor
            = _environment._vec_isa_desc.get_vec_factor_for_type(
                n_type, _environment._vec_factor);

        // Offset list
        Nodecl::List offset_list
            = Vectorization::Utils::get_vector_offset_list(
                /* start value */ 0,
                /* stride value */ 1,
                /* size */ isa_vec_factor);

        // VectorLiteral {0, 1, 2, ..., VL-1}
        Nodecl::VectorLiteral offset_vector_literal
            = Nodecl::VectorLiteral::make(
                offset_list,
                Utils::get_null_mask(),
                Utils::get_qualified_vector_to(n_type,
                                              isa_vec_factor),
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
        Nodecl::NodeclBase ind_var_increment = Vectorizer::
            _vectorizer_analysis->get_linear_step(
                    _environment._analysis_simd_scope, n);

        if (ind_var_increment.is_constant())
        {
            int iv_increment = const_value_cast_to_4(ind_var_increment.get_constant());
            // Offset list
            Nodecl::List offset_list
                = Vectorization::Utils::get_vector_offset_list(
                    0 /*start value*/,
                    iv_increment,
                    _environment._vec_isa_desc.get_vec_factor_for_type(
                        n.get_symbol().get_type(), _environment._vec_factor));

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
                    TL::Type conv_type = conversion.get_type().no_ref();

                    ind_var_type = Utils::get_qualified_vector_to(
                        conv_type,
                        _environment._vec_isa_desc.get_vec_factor_for_type(
                            conv_type, _environment._vec_factor));

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
                                    _environment._vec_factor,
                                    conversion.get_constant()));
                    }

                    vector_induction_var = Nodecl::VectorAdd::make(
                        promoted_conversion,
                        offset_vector_literal,
                        Utils::get_null_mask(),
                        Utils::get_qualified_vector_to(
                            ind_var_type,
                            _environment._vec_isa_desc.get_vec_factor_for_type(
                                ind_var_type, _environment._vec_factor)),
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
                                       n.get_type(),
                                       _environment._vec_isa_desc.get_vec_factor_for_type(n.get_type(),
                                       _environment._vec_factor))
                                       .no_ref();

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
                                    _environment._vec_factor,
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

                    vector_induction_var = Nodecl::VectorConversion::make(
                        iv_plus_offset,
                        Utils::get_null_mask(),
                        Utils::get_qualified_vector_to(
                            dest_type,
                            _environment._vec_isa_desc.get_vec_factor_for_type(
                                dest_type, _environment._vec_factor)),
                        n.get_locus());
                }

                conversion.replace(vector_induction_var);
            }
            else // There is no conversion
            {
                ind_var_type
                    = Utils::get_qualified_vector_to(
                          n.get_type(),
                          _environment._vec_isa_desc.get_vec_factor_for_type(
                              n.get_type(), _environment._vec_factor))
                          .no_ref();

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
        else if (Vectorizer::_vectorizer_analysis->is_uniform(
                    _environment._analysis_simd_scope, ind_var_increment,
                    ind_var_increment))
        {
            fatal_error("Vectorizer: Linear step is not constant but uniform: %s. Not supported",
                    ind_var_increment.prettyprint().c_str());
        }
        else
        {
            fatal_error("Vectorizer: Linear step is not constant: %s.",
                    ind_var_increment.prettyprint().c_str());
        }
    }

    void VectorizerVisitorExpression::visit(
            const Nodecl::IntelAssume& n)
    {
        // Do nothing
    }

    void VectorizerVisitorExpression::visit(
            const Nodecl::IntelAssumeAligned& n)
    {
        // Do nothing
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
