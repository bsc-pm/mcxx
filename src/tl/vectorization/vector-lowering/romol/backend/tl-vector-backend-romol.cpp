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

#include "tl-vector-backend-romol.hpp"

#include "tl-source.hpp"
#include "tl-nodecl-utils.hpp"
#include "tl-optimizations.hpp"
#include "cxx-cexpr.h"
#include "cxx-diagnostic.h"


namespace TL { namespace Vectorization {

    // Use RAII to restore the old value of a variable
    struct AssignAndKeep
    {
        private:
            Nodecl::NodeclBase &n;
            Nodecl::NodeclBase old_val;

        public:
            AssignAndKeep(Nodecl::NodeclBase& n_, Nodecl::NodeclBase new_val)
                : n(n_), old_val(n)
            {
                n = new_val;
            }

            ~AssignAndKeep()
            {
                n = old_val;
            }
    };

    namespace {
        Nodecl::NodeclBase assig_get_lhs(Nodecl::NodeclBase n)
        {
            if (n.is<Nodecl::Assignment>())
            {
                return n.as<Nodecl::Assignment>().get_lhs();
            }
            else
            {
                internal_error("Code unreachable", 0);
            }
        }
    }

    RomolVectorBackend::RomolVectorBackend()
        : _vectorizer(TL::Vectorization::Vectorizer::get_vectorizer())
    {
        std::cerr << "--- RoMoL backend phase ---" << std::endl;
    }

    void RomolVectorBackend::visit(const Nodecl::FunctionCode& n)
    {
        // TODO: Do it more efficiently!
        bool contains_vector_nodes =
            Nodecl::Utils::nodecl_contains_nodecl_of_kind<Nodecl::VectorAssignment>(n) ||
            Nodecl::Utils::nodecl_contains_nodecl_of_kind<Nodecl::VectorAdd>(n) ||
            Nodecl::Utils::nodecl_contains_nodecl_of_kind<Nodecl::VectorMul>(n) ||
            Nodecl::Utils::nodecl_contains_nodecl_of_kind<Nodecl::VectorConversion>(n) ||
            Nodecl::Utils::nodecl_contains_nodecl_of_kind<Nodecl::VectorLiteral>(n) ||
            Nodecl::Utils::nodecl_contains_nodecl_of_kind<Nodecl::VectorFunctionCode>(n) ||
            Nodecl::Utils::nodecl_contains_nodecl_of_kind<Nodecl::VectorMaskAssignment>(n) ||
            Nodecl::Utils::nodecl_contains_nodecl_of_kind<Nodecl::VectorLoad>(n) ||
            Nodecl::Utils::nodecl_contains_nodecl_of_kind<Nodecl::VectorStore>(n) ||
            Nodecl::Utils::nodecl_contains_nodecl_of_kind<Nodecl::VectorReductionAdd>(n) ||
            Nodecl::Utils::nodecl_contains_nodecl_of_kind<Nodecl::VectorConditionalExpression>(n) ||
            Nodecl::Utils::nodecl_contains_nodecl_of_kind<Nodecl::VectorPromotion>(n) ||
            Nodecl::Utils::nodecl_contains_nodecl_of_kind<Nodecl::VectorFunctionCall>(n);

        if (contains_vector_nodes)
        {
            // Initialize analisys
            TL::Optimizations::canonicalize_and_fold(
                    n, /*_fast_math_enabled*/ false);

            walk(n.get_statements());
        }
    }

    void RomolVectorBackend::visit(const Nodecl::ObjectInit& n)
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

    void RomolVectorBackend::visit(const Nodecl::Assignment& n)
    {
        AssignAndKeep k(current_assig, n);

        Nodecl::NodeclBase rhs = n.get_rhs();

        walk(rhs);
    }

    void RomolVectorBackend::visit(const Nodecl::VectorAdd& n)
    {
        if (current_assig.is_null())
        {
            warn_printf_at(n.get_locus(), "discarding expression without side effects");
            return;
        }

        TL::Type t = n.get_type();
        ERROR_CONDITION(!t.is_vector(), "Invalid type", 0);
        TL::Type element = t.vector_element();
        ERROR_CONDITION(!element.is_float(), "Not implemented: %s", print_declarator(element.get_internal_type()));

        TL::Symbol builtin_fun = TL::Scope::get_global_scope().get_symbol_from_name("valib_add_fl_fl_fl");
        ERROR_CONDITION(!builtin_fun.is_valid(), "Symbol not found", 0);

        current_assig.replace(
                Nodecl::FunctionCall::make(
                    builtin_fun.make_nodecl(/* set_ref_type */ true),
                    Nodecl::List::make(
                        assig_get_lhs(current_assig),
                        n.get_lhs(),
                        n.get_rhs()),
                    /* alternate-name */ Nodecl::NodeclBase::null(),
                    /* function-form */ Nodecl::NodeclBase::null(),
                    n.get_type(),
                    n.get_locus()
                    )
                );
    }

    void RomolVectorBackend::visit(const Nodecl::VectorAlignRight& n)
    {
    }

    void RomolVectorBackend::visit(const Nodecl::VectorArithmeticShr& n)
    {
    }

    void RomolVectorBackend::visit(const Nodecl::VectorAssignment& n)
    {
    }

    void RomolVectorBackend::visit(const Nodecl::VectorBitwiseAnd& n)
    {
    }

    void RomolVectorBackend::visit(const Nodecl::VectorBitwiseOr& n)
    {
    }

    void RomolVectorBackend::visit(const Nodecl::VectorBitwiseShl& n)
    {
    }

    void RomolVectorBackend::visit(const Nodecl::VectorBitwiseShr& n)
    {
    }

    void RomolVectorBackend::visit(const Nodecl::VectorBitwiseXor& n)
    {
    }

    void RomolVectorBackend::visit(const Nodecl::VectorCast& n)
    {
    }

    void RomolVectorBackend::visit(const Nodecl::VectorConditionalExpression& n)
    {
    }

    void RomolVectorBackend::visit(const Nodecl::VectorConversion& n)
    {
    }

    void RomolVectorBackend::visit(const Nodecl::VectorDifferent& n)
    {
    }

    void RomolVectorBackend::visit(const Nodecl::VectorDiv& n)
    {
    }

    void RomolVectorBackend::visit(const Nodecl::VectorEqual& n)
    {
    }

    void RomolVectorBackend::visit(const Nodecl::VectorFabs& n)
    {
    }

    void RomolVectorBackend::visit(const Nodecl::VectorFmadd& n)
    {
    }

    void RomolVectorBackend::visit(const Nodecl::VectorFunctionCall& n)
    {
    }

    void RomolVectorBackend::visit(const Nodecl::VectorGather& n)
    {
    }

    void RomolVectorBackend::visit(const Nodecl::VectorGreaterOrEqualThan& n)
    {
    }

    void RomolVectorBackend::visit(const Nodecl::VectorGreaterThan& n)
    {
    }

    void RomolVectorBackend::visit(const Nodecl::VectorLiteral& n)
    {
    }

    void RomolVectorBackend::visit(const Nodecl::VectorLoad& n)
    {
        if (current_assig.is_null())
        {
            warn_printf_at(n.get_locus(), "discarding expression without side effects");
            return;
        }

        TL::Type t = n.get_type();
        ERROR_CONDITION(!t.is_vector(), "Invalid type", 0);
        TL::Type element = t.vector_element();
        ERROR_CONDITION(!element.is_float(), "Not implemented: %s", print_declarator(element.get_internal_type()));

        TL::Symbol builtin_fun = TL::Scope::get_global_scope().get_symbol_from_name("valib_ld_fl");
        ERROR_CONDITION(!builtin_fun.is_valid(), "Symbol not found", 0);

        current_assig.replace(
                Nodecl::FunctionCall::make(
                    builtin_fun.make_nodecl(/* set_ref_type */ true),
                    Nodecl::List::make(
                        assig_get_lhs(current_assig),
                        n.get_rhs()),
                    /* alternate-name */ Nodecl::NodeclBase::null(),
                    /* function-form */ Nodecl::NodeclBase::null(),
                    n.get_type(),
                    n.get_locus()
                    )
                );

    }

    void RomolVectorBackend::visit(const Nodecl::VectorLogicalOr& n)
    {
    }

    void RomolVectorBackend::visit(const Nodecl::VectorLowerOrEqualThan& n)
    {
    }

    void RomolVectorBackend::visit(const Nodecl::VectorLowerThan& n)
    {
    }

    void RomolVectorBackend::visit(const Nodecl::VectorMinus& n)
    {
    }

    void RomolVectorBackend::visit(const Nodecl::VectorMod& n)
    {
    }

    void RomolVectorBackend::visit(const Nodecl::VectorMul& n)
    {
    }

    void RomolVectorBackend::visit(const Nodecl::VectorNeg& n)
    {
    }

    void RomolVectorBackend::visit(const Nodecl::VectorPromotion& n)
    {
    }

    void RomolVectorBackend::visit(const Nodecl::VectorRcp& n)
    {
    }

    void RomolVectorBackend::visit(const Nodecl::VectorRsqrt& n)
    {
    }

    void RomolVectorBackend::visit(const Nodecl::VectorScatter& n)
    {
    }

    void RomolVectorBackend::visit(const Nodecl::VectorSqrt& n)
    {
    }

    void RomolVectorBackend::visit(const Nodecl::VectorStore& n)
    {
        TL::Type t = n.get_type();
        ERROR_CONDITION(!t.is_vector(), "Invalid type", 0);
        TL::Type element = t.vector_element();
        ERROR_CONDITION(!element.is_float(), "Not implemented: %s", print_declarator(element.get_internal_type()));

        TL::Symbol builtin_fun = TL::Scope::get_global_scope().get_symbol_from_name("valib_st_fl");
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

    // void RomolVectorBackend::visit(const Nodecl::VectorPrefetch& n)
    // void RomolVectorBackend::visit(const Nodecl::VectorSincos& n);
    // void RomolVectorBackend::visit(const Nodecl::ParenthesizedExpression& n);
    // void RomolVectorBackend::visit(const Nodecl::VectorReductionAdd& n);
    // void RomolVectorBackend::visit(const Nodecl::VectorReductionMinus& n);
    // void RomolVectorBackend::visit(const Nodecl::VectorMaskAssignment& n);
    // void RomolVectorBackend::visit(const Nodecl::VectorMaskConversion& n);
    // void RomolVectorBackend::visit(const Nodecl::VectorMaskOr& n);
    // void RomolVectorBackend::visit(const Nodecl::VectorMaskAnd& n);
    // void RomolVectorBackend::visit(const Nodecl::VectorMaskNot& n);
    // void RomolVectorBackend::visit(const Nodecl::VectorMaskAnd1Not& n);
    // void RomolVectorBackend::visit(const Nodecl::VectorMaskAnd2Not& n);
    // void RomolVectorBackend::visit(const Nodecl::VectorMaskXor& n);

    // void RomolVectorBackend::visit(const Nodecl::MaskLiteral& n);

    Nodecl::NodeclVisitor<void>::Ret RomolVectorBackend::unhandled_node(const Nodecl::NodeclBase& n)
    {
        internal_error("NEON Backend: Unknown node %s at %s.",
                ast_print_node_type(n.get_kind()),
                locus_to_str(n.get_locus()));

        return Ret();
    }
} }
