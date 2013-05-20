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

namespace TL
{
    namespace Vectorization
    {
        VectorizerVisitorExpression::VectorizerVisitorExpression(
                const VectorizerEnvironment& environment) :
            _environment(environment)
        {
        }

        void VectorizerVisitorExpression::visit(const Nodecl::Add& n)
        {
            walk(n.get_lhs());
            walk(n.get_rhs());

            const Nodecl::VectorAdd vector_add =
                Nodecl::VectorAdd::make(
                        n.get_lhs().shallow_copy(),
                        n.get_rhs().shallow_copy(),
                        get_qualified_vector_to(n.get_type(), _environment._vector_length),
                        n.get_locus());
            
            n.replace(vector_add);
        }

        void VectorizerVisitorExpression::visit(const Nodecl::Minus& n)
        {
            walk(n.get_lhs());
            walk(n.get_rhs());

            const Nodecl::VectorMinus vector_minus =
                Nodecl::VectorMinus::make(
                        n.get_lhs().shallow_copy(),
                        n.get_rhs().shallow_copy(),
                        get_qualified_vector_to(n.get_type(), _environment._vector_length),
                        n.get_locus());

            n.replace(vector_minus);
        }

        void VectorizerVisitorExpression::visit(const Nodecl::Mul& n)
        {
            walk(n.get_lhs());
            walk(n.get_rhs());

            const Nodecl::VectorMul vector_mul =
                Nodecl::VectorMul::make(
                        n.get_lhs().shallow_copy(),
                        n.get_rhs().shallow_copy(),
                        get_qualified_vector_to(n.get_type(), _environment._vector_length),
                        n.get_locus());

            n.replace(vector_mul);
        }

        void VectorizerVisitorExpression::visit(const Nodecl::Div& n)
        {
            walk(n.get_lhs());
            walk(n.get_rhs());

            const Nodecl::VectorDiv vector_div =
                Nodecl::VectorDiv::make(
                        n.get_lhs().shallow_copy(),
                        n.get_rhs().shallow_copy(),
                        get_qualified_vector_to(n.get_type(), _environment._vector_length),
                        n.get_locus());

            n.replace(vector_div);
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

        void VectorizerVisitorExpression::visit(const Nodecl::BitwiseAnd& n)
        {
            walk(n.get_lhs());
            walk(n.get_rhs());

            const Nodecl::VectorBitwiseAnd vector_ba =
                Nodecl::VectorBitwiseAnd::make(
                        n.get_lhs().shallow_copy(),
                        n.get_rhs().shallow_copy(),
                        get_qualified_vector_to(n.get_type(), _environment._vector_length),
                        n.get_locus());

            n.replace(vector_ba);
        }

        void VectorizerVisitorExpression::visit(const Nodecl::BitwiseOr& n)
        {
            walk(n.get_lhs());
            walk(n.get_rhs());

            const Nodecl::VectorBitwiseOr vector_bo =
                Nodecl::VectorBitwiseOr::make(
                        n.get_lhs().shallow_copy(),
                        n.get_rhs().shallow_copy(),
                        get_qualified_vector_to(n.get_type(), _environment._vector_length),
                        n.get_locus());

            n.replace(vector_bo);
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
            walk(n.get_condition());
            walk(n.get_true());
            walk(n.get_false());

            const Nodecl::VectorConditionalExpression vector_cond =
                Nodecl::VectorConditionalExpression::make(
                        n.get_condition().shallow_copy(),
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
                            Vectorizer::_analysis_scopes->back(),
                            lhs))
                {
                    TL::Type basic_type = lhs.get_type();
                    if (basic_type.is_lvalue_reference())
                    {
                        basic_type = basic_type.references_to();
                    }

                    // Aligned
                    if(Vectorizer::_analysis_info->is_simd_aligned_access(
                            Vectorizer::_analysis_scopes->back(),
                            lhs,
                            _environment._suitable_expr_list,
                            _environment._unroll_factor,
                            _environment._vector_length))
                    {
                        printf("VECTORIZER: Store access '%s' is ALIGNED\n",
                                lhs.prettyprint().c_str());

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
                    else // Unaligned
                    {
                        printf("VECTORIZER: Store access '%s' is UNALIGNED\n",
                                lhs.prettyprint().c_str());

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

                    const Nodecl::VectorScatter vector_scatter =
                        Nodecl::VectorScatter::make(
                                base.shallow_copy(),
                                strides,
                                n.get_rhs().shallow_copy(),
                                vector_type,
                                n.get_locus());

                    n.replace(vector_scatter);
                }
            }
            else // Register
            {
                walk(lhs);

                const Nodecl::VectorAssignment vector_assignment =
                    Nodecl::VectorAssignment::make(
                            lhs.shallow_copy(),
                            n.get_rhs().shallow_copy(),
                            vector_type,
                            n.get_locus());

                n.replace(vector_assignment);
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
            if (Vectorizer::_analysis_info->is_constant_access(
                        Vectorizer::_analysis_scopes->back(),
                        n))
            {

                std::cerr << "Constant access: " << n.prettyprint() << "\n";

                const Nodecl::VectorPromotion vector_prom =
                    Nodecl::VectorPromotion::make(
                            n.shallow_copy(),
                            vector_type,
                            n.get_locus());

                n.replace(vector_prom);
            }
            // Vector Load
            else if (Vectorizer::_analysis_info->is_adjacent_access(
                        Vectorizer::_analysis_scopes->back(),
                        n))
            {
                // Aligned
                if(Vectorizer::_analysis_info->is_simd_aligned_access(
                            Vectorizer::_analysis_scopes->back(),
                            n,
                            _environment._suitable_expr_list,
                            _environment._unroll_factor,
                            _environment._vector_length))
                {
                    printf("VECTORIZER: Load access '%s' is ALIGNED\n",
                            n.prettyprint().c_str());

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
                else // Unaligned
                {
                    printf("VECTORIZER: Load access '%s' is UNALIGNED\n",
                            n.prettyprint().c_str());

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

                const Nodecl::VectorGather vector_gather =
                    Nodecl::VectorGather::make(
                            base.shallow_copy(),
                            strides,
                            vector_type,
                            n.get_locus());

                n.replace(vector_gather);
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
                            _environment._target_type);

                ERROR_CONDITION(best_version.is_null(), "Vectorizer: the best vector function for '%s' is null",
                        called_sym.get_symbol().get_name().c_str());

                // Create new called symbol
                Nodecl::Symbol new_called;
                if (best_version.is<Nodecl::FunctionCode>())
                {
                    new_called = best_version.as<Nodecl::FunctionCode>().get_symbol().
                        make_nodecl(n.get_locus());
                }
                else if (best_version.is<Nodecl::Symbol>())
                {
                    new_called = best_version.as<Nodecl::Symbol>().get_symbol().
                        make_nodecl(n.get_locus());
                }
                else
                {
                    running_error("Vectorizer: %s found as vector function version in function versioning.",
                            ast_print_node_type(best_version.get_kind()));
                }

                const Nodecl::VectorFunctionCall vector_function_call =
                    Nodecl::VectorFunctionCall::make(
                            new_called,
                            n.get_arguments().shallow_copy(),
                            n.get_alternate_name().shallow_copy(),
                            n.get_function_form().shallow_copy(),
                            get_qualified_vector_to(n.get_type(), _environment._vector_length),
                            n.get_locus());

                n.replace(vector_function_call);
            }
        }

        void VectorizerVisitorExpression::visit(const Nodecl::Symbol& n)
        {
            TL::Type sym_type = n.get_type();

            //std::cerr << "scalar_type: " << n.prettyprint() << std::endl;

            if (!sym_type.is_vector())
            {
               // Vectorize BASIC induction variable
                if (Vectorizer::_analysis_info->is_basic_induction_variable(
                            Vectorizer::_analysis_scopes->back(),
                            n))
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr,"VECTORIZER: '%s' is IV and will be PROMOTED with OFFSET\n", n.prettyprint().c_str()); 
                    }

                    // Computing IV offset {0, 1, 2, 3}
                    TL::ObjectList<Nodecl::NodeclBase> literal_list;

                    const_value_t *ind_var_increment = Vectorizer::_analysis_info->get_induction_variable_increment(
                            Vectorizer::_analysis_scopes->back(), n);

                    for(const_value_t *i = const_value_get_zero(4, 0);
                            const_value_is_nonzero(const_value_lt(i, const_value_get_unsigned_int(_environment._unroll_factor)));
                            i = const_value_add(i, ind_var_increment))
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
                else if (is_declared_in_scope(
                            _environment._simd_body_scope.get_decl_context().current_scope,
                            n.get_symbol().get_scope().get_decl_context().current_scope))
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr,"VECTORIZER: '%s' is DECLARED INSIDE OF THE SIMD SCOPE. Its TYPE will be vectorized\n", 
                                n.prettyprint().c_str()); 
                    }

                    //std::cerr << "NS scalar_type: " << n.prettyprint() << std::endl;

                    TL::Symbol tl_sym = n.get_symbol();
                    TL::Type tl_sym_type = tl_sym.get_type();

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
                // Vectorize constants
                else if (Vectorizer::_analysis_info->is_constant(
                            Vectorizer::_analysis_scopes->back(),
                            n))
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr,"VECTORIZER: '%s' is CONSTANT and will be PROMOTED to vector\n", n.prettyprint().c_str()); 
                    }

                    const Nodecl::VectorPromotion vector_prom =
                        Nodecl::VectorPromotion::make(
                                n.shallow_copy(),
                                get_qualified_vector_to(sym_type, _environment._vector_length),
                                n.get_locus());

                    n.replace(vector_prom);
                }
                else
                {
                    //TODO: If you are from outside of the loop -> Vector local copy.
                    running_error("Vectorizer: Loop is not vectorizable. '%s' is not IV or Constant or Local.",
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

        Nodecl::NodeclVisitor<void>::Ret VectorizerVisitorExpression::unhandled_node(
                const Nodecl::NodeclBase& n)
        {
            std::cerr << "Unknown 'Expression' node "
                << ast_print_node_type(n.get_kind())
                << " at " << n.get_locus()
                << std::endl;

            return Ret();
        }

        bool VectorizerVisitorExpression::is_declared_in_scope(const scope_t *const  target_scope,
                const scope_t *const symbol_scope) const
        {
            if (symbol_scope == NULL)
                return false;
            else if (target_scope == NULL)
                return false;
            else if (target_scope == symbol_scope)
                return true;
            else
            {
                return false;
            }
        }
    }
}
