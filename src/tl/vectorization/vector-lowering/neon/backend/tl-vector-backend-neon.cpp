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

#include "tl-vector-backend-neon.hpp"
#include "tl-vectorization-utils.hpp"

#include "tl-source.hpp"
#include "tl-nodecl-utils.hpp"
#include "tl-optimizations.hpp"
#include "cxx-cexpr.h"


namespace TL
{
    namespace Vectorization
    {
        NeonVectorBackend::NeonVectorBackend()
            : _vectorizer(TL::Vectorization::Vectorizer::get_vectorizer())
        {
            std::cerr << "--- NEON backend phase ---" << std::endl;
        }

        void NeonVectorBackend::visit(const Nodecl::FunctionCode& n)
        {
            bool contains_vector_nodes = TL::Vectorization::Utils::contains_vector_nodes(n);

            if (contains_vector_nodes)
            {
                // Initialize analisys
                TL::Optimizations::canonicalize_and_fold(
                        n, /*_fast_math_enabled*/ false);

                walk(n.get_statements());
            }
        }

        void NeonVectorBackend::visit(const Nodecl::ObjectInit& n)
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

        void NeonVectorBackend::visit(const Nodecl::VectorAdd& n)
        {
            walk(n.get_lhs());
            walk(n.get_rhs());

            TL::Type t = n.get_type();
            ERROR_CONDITION(!t.is_vector(), "Invalid type", 0);
            TL::Type element = t.vector_element();
            ERROR_CONDITION(!element.is_float(), "Not implemented: %s", print_declarator(element.get_internal_type()));

            TL::Symbol builtin_fun = TL::Scope::get_global_scope().get_symbol_from_name("vaddq_f32");
            ERROR_CONDITION(!builtin_fun.is_valid(), "Symbol not found", 0);

            n.replace(
                    Nodecl::FunctionCall::make(
                        builtin_fun.make_nodecl(/* set_ref_type */ true),
                        Nodecl::List::make(
                            n.get_lhs(),
                            n.get_rhs()),
                        /* alternate-name */ Nodecl::NodeclBase::null(),
                        /* function-form */ Nodecl::NodeclBase::null(),
                        n.get_type(),
                        n.get_locus()
                        )
                    );
        }

        void NeonVectorBackend::visit(const Nodecl::VectorAlignRight& n)
        {
        }

        void NeonVectorBackend::visit(const Nodecl::VectorArithmeticShr& n)
        {
        }

        void NeonVectorBackend::visit(const Nodecl::VectorAssignment& n)
        {
        }

        void NeonVectorBackend::visit(const Nodecl::VectorBitwiseAnd& n)
        {
        }

        void NeonVectorBackend::visit(const Nodecl::VectorBitwiseOr& n)
        {
        }

        void NeonVectorBackend::visit(const Nodecl::VectorBitwiseShl& n)
        {
        }

        void NeonVectorBackend::visit(const Nodecl::VectorBitwiseShr& n)
        {
        }

        void NeonVectorBackend::visit(const Nodecl::VectorBitwiseXor& n)
        {
        }

        void NeonVectorBackend::visit(const Nodecl::VectorCast& n)
        {
        }

        void NeonVectorBackend::visit(const Nodecl::VectorConditionalExpression& n)
        {
        }

        void NeonVectorBackend::visit(const Nodecl::VectorConversion& n)
        {
        }

        void NeonVectorBackend::visit(const Nodecl::VectorDifferent& n)
        {
        }

        void NeonVectorBackend::visit(const Nodecl::VectorDiv& n)
        {
        }

        void NeonVectorBackend::visit(const Nodecl::VectorEqual& n)
        {
        }

        void NeonVectorBackend::visit(const Nodecl::VectorFabs& n)
        {
        }

        void NeonVectorBackend::visit(const Nodecl::VectorFmadd& n)
        {
        }

        void NeonVectorBackend::visit(const Nodecl::VectorFunctionCall& n)
        {
        }

        void NeonVectorBackend::visit(const Nodecl::VectorGather& n)
        {
        }

        void NeonVectorBackend::visit(const Nodecl::VectorGreaterOrEqualThan& n)
        {
        }

        void NeonVectorBackend::visit(const Nodecl::VectorGreaterThan& n)
        {
        }

        void NeonVectorBackend::visit(const Nodecl::VectorLiteral& n)
        {
        }

        void NeonVectorBackend::visit(const Nodecl::VectorLoad& n)
        {
            walk(n.get_rhs());

            TL::Type t = n.get_type();
            ERROR_CONDITION(!t.is_vector(), "Invalid type", 0);
            TL::Type element = t.vector_element();
            ERROR_CONDITION(!element.is_float(), "Not implemented: %s", print_declarator(element.get_internal_type()));

            TL::Symbol builtin_fun = TL::Scope::get_global_scope().get_symbol_from_name("vld1q_f32");
            ERROR_CONDITION(!builtin_fun.is_valid(), "Symbol not found", 0);

            n.replace(
                    Nodecl::FunctionCall::make(
                        builtin_fun.make_nodecl(/* set_ref_type */ true),
                        Nodecl::List::make(n.get_rhs()),
                        /* alternate-name */ Nodecl::NodeclBase::null(),
                        /* function-form */ Nodecl::NodeclBase::null(),
                        n.get_type(),
                        n.get_locus()
                        )
                    );

        }

        void NeonVectorBackend::visit(const Nodecl::VectorLogicalOr& n)
        {
        }

        void NeonVectorBackend::visit(const Nodecl::VectorLowerOrEqualThan& n)
        {
        }

        void NeonVectorBackend::visit(const Nodecl::VectorLowerThan& n)
        {
        }

        void NeonVectorBackend::visit(const Nodecl::VectorMinus& n)
        {
        }

        void NeonVectorBackend::visit(const Nodecl::VectorMod& n)
        {
        }

        void NeonVectorBackend::visit(const Nodecl::VectorMul& n)
        {
        }

        void NeonVectorBackend::visit(const Nodecl::VectorNeg& n)
        {
        }

        void NeonVectorBackend::visit(const Nodecl::VectorPromotion& n)
        {
        }

        void NeonVectorBackend::visit(const Nodecl::VectorRcp& n)
        {
        }

        void NeonVectorBackend::visit(const Nodecl::VectorRsqrt& n)
        {
        }

        void NeonVectorBackend::visit(const Nodecl::VectorScatter& n)
        {
        }

        void NeonVectorBackend::visit(const Nodecl::VectorSqrt& n)
        {
        }

        void NeonVectorBackend::visit(const Nodecl::VectorStore& n)
        {
            walk(n.get_lhs());
            walk(n.get_rhs());

            TL::Type t = n.get_type();
            ERROR_CONDITION(!t.is_vector(), "Invalid type", 0);
            TL::Type element = t.vector_element();
            ERROR_CONDITION(!element.is_float(), "Not implemented: %s", print_declarator(element.get_internal_type()));

            TL::Symbol builtin_fun = TL::Scope::get_global_scope().get_symbol_from_name("vst1q_f32");
            ERROR_CONDITION(!builtin_fun.is_valid(), "Symbol not found", 0);

            n.replace(
                    Nodecl::FunctionCall::make(
                        builtin_fun.make_nodecl(/* set_ref_type */ true),
                        Nodecl::List::make(
                            n.get_lhs(), // dest
                            n.get_rhs()),
                        /* alternate-name */ Nodecl::NodeclBase::null(),
                        /* function-form */ Nodecl::NodeclBase::null(),
                        n.get_type(),
                        n.get_locus()
                        )
                    );
        }

        // void NeonVectorBackend::visit(const Nodecl::VectorPrefetch& n)
        // void NeonVectorBackend::visit(const Nodecl::VectorSincos& n);
        // void NeonVectorBackend::visit(const Nodecl::ParenthesizedExpression& n);
        // void NeonVectorBackend::visit(const Nodecl::VectorReductionAdd& n);
        // void NeonVectorBackend::visit(const Nodecl::VectorReductionMinus& n);
        // void NeonVectorBackend::visit(const Nodecl::VectorMaskAssignment& n);
        // void NeonVectorBackend::visit(const Nodecl::VectorMaskConversion& n);
        // void NeonVectorBackend::visit(const Nodecl::VectorMaskOr& n);
        // void NeonVectorBackend::visit(const Nodecl::VectorMaskAnd& n);
        // void NeonVectorBackend::visit(const Nodecl::VectorMaskNot& n);
        // void NeonVectorBackend::visit(const Nodecl::VectorMaskAnd1Not& n);
        // void NeonVectorBackend::visit(const Nodecl::VectorMaskAnd2Not& n);
        // void NeonVectorBackend::visit(const Nodecl::VectorMaskXor& n);

        // void NeonVectorBackend::visit(const Nodecl::MaskLiteral& n);

        Nodecl::NodeclVisitor<void>::Ret NeonVectorBackend::unhandled_node(const Nodecl::NodeclBase& n)
        {
            internal_error("NEON Backend: Unknown node %s at %s.",
                    ast_print_node_type(n.get_kind()),
                    locus_to_str(n.get_locus()));

            return Ret();
        }
    }
}
