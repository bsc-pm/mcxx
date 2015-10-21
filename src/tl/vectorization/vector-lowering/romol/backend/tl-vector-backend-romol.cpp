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
            else if (n.is<Nodecl::VectorMaskAssignment>())
            {
                return n.as<Nodecl::VectorMaskAssignment>().get_lhs();
            }
            else
            {
                internal_error("Code unreachable", 0);
            }
        }

        std::string type_name(TL::Type t)
        {
            if (t.is_float())
            {
                return "fl";
            }
            else if (t.is_double())
            {
                return "db";
            }
            else if (::is_any_signed_int_type(t.get_internal_type()))
            {
                int size = t.get_size();
                std::stringstream ss;
                ss << "int" << (size * 8);
                return ss.str();
            }
                else if (::is_any_unsigned_int_type(t.get_internal_type()))
            {
                int size = t.get_size();
                std::stringstream ss;
                ss << "uint" << (size * 8);
                return ss.str();
            }
            else
            {
                internal_error("Unsupported type '%s'\n", print_declarator(t.get_internal_type()));
            }
        }

        std::string binary_operation_name(const std::string& op, TL::Type t)
        {
            std::stringstream ss;
            std::string tname = type_name(t);
            ss << "valib_" << op << "_" << tname << "_" << tname << "_" << tname;

            return ss.str();
        }

        std::string unary_operation_name(const std::string& op, TL::Type t)
        {
            std::stringstream ss;
            std::string tname = type_name(t);
            ss << "valib_" << op << "_" << tname << "_" << tname;

            return ss.str();
        }

        std::string relational_operation_name(const std::string& op, TL::Type t)
        {
            std::stringstream ss;
            std::string tname = type_name(t);
            ss << "valib_" << op << "_" << tname << "_" << tname;

            return ss.str();
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

    void RomolVectorBackend::emit_mask_is_zero(Nodecl::NodeclBase n,
            Nodecl::NodeclBase mask_tmp)
    {
        const std::string mask_is_zero = "valib_mask_is_zero";
        TL::Symbol builtin_fun = TL::Scope::get_global_scope().get_symbol_from_name(mask_is_zero);
        ERROR_CONDITION(!builtin_fun.is_valid(), "Symbol not found '%s'", mask_is_zero.c_str());

        n.replace(
                Nodecl::FunctionCall::make(
                    builtin_fun.make_nodecl(/* set_ref_type */ true),
                    Nodecl::List::make(mask_tmp),
                    /* alternate-name */ Nodecl::NodeclBase::null(),
                    /* function-form */ Nodecl::NodeclBase::null(),
                    n.get_type(),
                    n.get_locus()
                    )
                );
    }

    void RomolVectorBackend::emit_mask_is_nonzero(Nodecl::NodeclBase n,
            Nodecl::NodeclBase mask_tmp)
    {
        Nodecl::NodeclBase t = n.shallow_copy();
        emit_mask_is_zero(t, mask_tmp);

        n.replace(
                Nodecl::LogicalNot::make(
                    t,
                    TL::Type::get_bool_type(),
                    n.get_locus())
                );
    }

    namespace {
        bool is_symbol_of_mask_type(Nodecl::NodeclBase n)
        {
            n = n.no_conv();

            return n.is<Nodecl::Symbol>()
                && n.get_symbol().get_type().is_mask();
        }

        bool is_constant_zero(Nodecl::NodeclBase n)
        {
            return n.is_constant()
                && const_value_is_zero(n.get_constant());
        }
    }

    template <typename Node>
    void RomolVectorBackend::emit_mask_comparison(const Node& n,
            void (RomolVectorBackend::* emit_cmp_fun)(Nodecl::NodeclBase, Nodecl::NodeclBase))
    {
        if (is_symbol_of_mask_type(n.get_lhs())
                && is_constant_zero(n.get_rhs()))
        {
            // mask <?> 0
            (this->*emit_cmp_fun)(n, n.get_lhs());
        }
        else if (is_symbol_of_mask_type(n.get_rhs())
                && is_constant_zero(n.get_lhs()))
        {
            // 0 <?> mask
            (this->*emit_cmp_fun)(n, n.get_rhs());
        }
        else
        {
            internal_error("unsupported mask operation at %s", n.get_locus_str().c_str());
        }
    }

    void RomolVectorBackend::visit(const Nodecl::Different& n)
    {
        if (n.get_lhs().get_type().no_ref().is_mask()
                || n.get_rhs().get_type().no_ref().is_mask())
            emit_mask_comparison(n, &RomolVectorBackend::emit_mask_is_nonzero);
    }

    void RomolVectorBackend::visit(const Nodecl::Equal& n)
    {
        if (n.get_lhs().get_type().no_ref().is_mask()
                || n.get_rhs().get_type().no_ref().is_mask())
            emit_mask_comparison(n, &RomolVectorBackend::emit_mask_is_zero);
    }

    template <typename Node>
    void RomolVectorBackend::visit_elementwise_binary_expression(const Node& n, const std::string& name)
    {
        if (current_assig.is_null())
        {
            warn_printf_at(n.get_locus(), "discarding expression without side effects");
            return;
        }

        TL::Type t = n.get_type();
        ERROR_CONDITION(!t.is_vector(), "Invalid type", 0);

        TL::Type element_type = t.vector_element();
        std::string binary_operation = binary_operation_name(name, element_type);
        TL::Symbol builtin_fun = TL::Scope::get_global_scope().get_symbol_from_name(
                binary_operation);
        ERROR_CONDITION(!builtin_fun.is_valid(), "Symbol not found '%s'", binary_operation.c_str());

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

    template <typename Node>
    void RomolVectorBackend::visit_elementwise_unary_expression(const Node& n, const std::string& name)
    {
        if (current_assig.is_null())
        {
            warn_printf_at(n.get_locus(), "discarding expression without side effects");
            return;
        }

        TL::Type t = n.get_type();
        ERROR_CONDITION(!t.is_vector(), "Invalid type", 0);

        TL::Type element_type = t.vector_element();
        std::string binary_operation = unary_operation_name(name, element_type);
        TL::Symbol builtin_fun = TL::Scope::get_global_scope().get_symbol_from_name(
                binary_operation);
        ERROR_CONDITION(!builtin_fun.is_valid(), "Symbol not found '%s'", binary_operation.c_str());

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

    template <typename Node>
    void RomolVectorBackend::visit_relational_expression(const Node& n, const std::string& name)
    {
        if (current_assig.is_null())
        {
            warn_printf_at(n.get_locus(), "discarding expression without side effects");
            return;
        }

        TL::Type t = n.get_type();
        ERROR_CONDITION(!t.is_vector(), "Invalid type", 0);

        TL::Type element_type = t.vector_element();
        std::string binary_operation = relational_operation_name(name, element_type);
        TL::Symbol builtin_fun = TL::Scope::get_global_scope().get_symbol_from_name(
                binary_operation);
        ERROR_CONDITION(!builtin_fun.is_valid(), "Symbol not found '%s'", binary_operation.c_str());

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

    void RomolVectorBackend::visit(const Nodecl::VectorAdd& n)
    {
        visit_elementwise_binary_expression(n, "add");
    }

    void RomolVectorBackend::visit(const Nodecl::VectorAlignRight& n)
    {
    }

    void RomolVectorBackend::visit(const Nodecl::VectorArithmeticShr& n)
    {
        visit_elementwise_binary_expression(n, "asr");
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
        visit_elementwise_binary_expression(n, "lsl");
    }

    void RomolVectorBackend::visit(const Nodecl::VectorBitwiseShr& n)
    {
        visit_elementwise_binary_expression(n, "lsr");
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
        visit_relational_expression(n, "ne");
    }

    void RomolVectorBackend::visit(const Nodecl::VectorDiv& n)
    {
        // FIXME: there is no 'div' yet
        // visit_elementwise_binary_expression(n, "div");
    }

    void RomolVectorBackend::visit(const Nodecl::VectorEqual& n)
    {
        visit_relational_expression(n, "eq");
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
        visit_relational_expression(n, "ge");
    }

    void RomolVectorBackend::visit(const Nodecl::VectorGreaterThan& n)
    {
        visit_relational_expression(n, "gt");
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
        TL::Type element_type = t.vector_element();

        std::string load_operation = "valib_ld_";
        load_operation += type_name(element_type);

        TL::Symbol builtin_fun = TL::Scope::get_global_scope().get_symbol_from_name(load_operation);
        ERROR_CONDITION(!builtin_fun.is_valid(), "Symbol not found '%s'", load_operation.c_str());

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
        visit_relational_expression(n, "le");
    }

    void RomolVectorBackend::visit(const Nodecl::VectorLowerThan& n)
    {
        visit_relational_expression(n, "lt");
    }

    void RomolVectorBackend::visit(const Nodecl::VectorMinus& n)
    {
        visit_elementwise_binary_expression(n, "sub");
    }

    void RomolVectorBackend::visit(const Nodecl::VectorMod& n)
    {
        // FIXME: there is no 'mod' yet
        // visit_elementwise_binary_expression(n, "mod");
    }

    void RomolVectorBackend::visit(const Nodecl::VectorMul& n)
    {
        visit_elementwise_binary_expression(n, "mul");
    }

    void RomolVectorBackend::visit(const Nodecl::VectorNeg& n)
    {
        visit_elementwise_unary_expression(n, "neg");
    }

    void RomolVectorBackend::visit(const Nodecl::VectorPromotion& n)
    {
        if (current_assig.is_null())
        {
            warn_printf_at(n.get_locus(), "discarding expression without side effects");
            return;
        }

        TL::Type t = n.get_type();
        ERROR_CONDITION(!t.is_vector(), "Invalid type", 0);
        TL::Type element_type = t.vector_element();

        std::string vector_promotion_operation = "valib_set_";
        vector_promotion_operation += type_name(element_type);

        TL::Symbol builtin_fun = TL::Scope::get_global_scope().get_symbol_from_name(vector_promotion_operation);
        ERROR_CONDITION(!builtin_fun.is_valid(), "Symbol not found '%s'", vector_promotion_operation.c_str());

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
        TL::Type element_type = t.vector_element();

        std::string store_operation = "valib_st_";
        store_operation += type_name(element_type);

        TL::Symbol builtin_fun = TL::Scope::get_global_scope().get_symbol_from_name(store_operation);
        ERROR_CONDITION(!builtin_fun.is_valid(), "Symbol not found '%s'", store_operation.c_str());

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

    void RomolVectorBackend::visit(const Nodecl::VectorMaskAssignment& n)
    {
        AssignAndKeep k(current_assig, n);

        Nodecl::NodeclBase rhs = n.get_rhs();

        walk(rhs);
    }

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
