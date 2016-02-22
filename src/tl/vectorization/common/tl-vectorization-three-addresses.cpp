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

#include "tl-vectorization-three-addresses.hpp"
#include "tl-vectorization-utils.hpp"
#include "tl-nodecl-utils.hpp"

namespace TL
{
namespace Vectorization
{
    unsigned long long int VectorizationThreeAddresses::sym_counter = 0;

    VectorizationThreeAddresses::VectorizationThreeAddresses()
    {
    }


    bool needs_decomposition(const Nodecl::NodeclBase& n)
    {
        if (n.is<Nodecl::VectorAdd>() || 
                n.is<Nodecl::VectorMinus>() ||
                n.is<Nodecl::VectorMul>() ||
                n.is<Nodecl::VectorDiv>() ||
                n.is<Nodecl::VectorMod>() ||
                n.is<Nodecl::VectorNeg>() ||
                n.is<Nodecl::VectorFmadd>() ||
                n.is<Nodecl::VectorFmminus>() ||
                n.is<Nodecl::VectorBitwiseShr>() ||
                n.is<Nodecl::VectorBitwiseShl>() ||
                n.is<Nodecl::VectorArithmeticShr>() ||
                n.is<Nodecl::VectorPromotion>() ||
                n.is<Nodecl::VectorLiteral>() ||
                n.is<Nodecl::VectorLoad>() ||
                n.is<Nodecl::VectorGather>() ||
                n.is<Nodecl::VectorEqual>() ||
                n.is<Nodecl::VectorDifferent>() ||
                n.is<Nodecl::VectorLowerThan>() ||
                n.is<Nodecl::VectorLowerOrEqualThan>() ||
                n.is<Nodecl::VectorGreaterThan>() ||
                n.is<Nodecl::VectorGreaterOrEqualThan>() ||
                n.is<Nodecl::VectorLogicalNot>() ||
                n.is<Nodecl::VectorLogicalAnd>() ||
                n.is<Nodecl::VectorLogicalOr>() ||
                n.is<Nodecl::VectorBitwiseNot>() ||
                n.is<Nodecl::VectorBitwiseAnd>() ||
                n.is<Nodecl::VectorBitwiseOr>() ||
                n.is<Nodecl::VectorBitwiseXor>() ||
                n.is<Nodecl::VectorSqrt>() ||
                n.is<Nodecl::VectorRsqrt>() ||
                n.is<Nodecl::VectorFabs>() ||
                n.is<Nodecl::VectorSincos>() ||
                n.is<Nodecl::VectorFunctionCall>() ||
                n.is<Nodecl::VectorConversion>() ||
                n.is<Nodecl::VectorConditionalExpression>() ||
                n.is<Nodecl::VectorAlignRight>() ||
                n.is<Nodecl::VectorMaskNot>() ||
                n.is<Nodecl::VectorMaskAnd>() ||
                n.is<Nodecl::VectorMaskOr>() ||
                n.is<Nodecl::VectorMaskXor>() ||
                n.is<Nodecl::VectorMaskAnd1Not>() ||
                n.is<Nodecl::VectorMaskAnd2Not>()
                )
            return true;

        return false;
    }

    TL::Symbol VectorizationThreeAddresses::get_temporal_symbol(
            const Nodecl::NodeclBase& reference)
    {
        TL::Scope scope = _object_init.is_null() ? 
            reference.retrieve_context() : _object_init.retrieve_context();

        std::stringstream new_sym_name;
        new_sym_name << "_v3atmp" << sym_counter;

        TL::Symbol tl_sym = scope.new_symbol(new_sym_name.str());
        tl_sym.get_internal_symbol()->kind = SK_VARIABLE;
        symbol_entity_specs_set_is_user_declared(tl_sym.get_internal_symbol(), 1);
        tl_sym.set_type(reference.get_type().no_ref().get_unqualified_type());

// DEBUG __m1024 types
//
//        if (tl_sym.get_type().is_vector() &&
//                (tl_sym.get_type().basic_type().is_signed_long_int() ||
//                tl_sym.get_type().basic_type().is_unsigned_long_int()))
//        {
//            abort();
//        }

        sym_counter++;

        return tl_sym;
    }

    void VectorizationThreeAddresses::decomp(
            const Nodecl::NodeclBase& n)
    {
        TL::Symbol tmp_sym = get_temporal_symbol(n);

        auto new_stmt = Nodecl::ExpressionStatement::make(
                Nodecl::Assignment::make(
                    tmp_sym.make_nodecl(false /*ref_type*/),
                    n.shallow_copy(),
                    n.get_type(),
                    n.get_locus()));

        n.replace(Nodecl::Conversion::make(
                    tmp_sym.make_nodecl(true /*ref_type*/),
                    tmp_sym.get_type()));


        Nodecl::NodeclBase cxx_def;
        if (IS_CXX_LANGUAGE)
        {
            cxx_def = Nodecl::CxxDef::make(
                    /* context of def */ Nodecl::NodeclBase::null(),
                    tmp_sym,
                    n.get_locus());
        }


        if (!_object_init.is_null())
        {
            if (IS_CXX_LANGUAGE)
            {
                Nodecl::Utils::prepend_sibling_statement(_object_init, cxx_def);
            }
            Nodecl::Utils::prepend_sibling_statement(_object_init, new_stmt);
        }
        else
        {
            if (IS_CXX_LANGUAGE)
            {
                Nodecl::Utils::prepend_sibling_statement(n, cxx_def);
            }
            Nodecl::Utils::prepend_sibling_statement(n, new_stmt);
        }
    }
 
    void VectorizationThreeAddresses::visit_vector_unary(
            const Nodecl::NodeclBase& n)
    {
        auto children = n.children();

        auto lhs = children[0];

        walk(lhs);

        if (needs_decomposition(lhs))
        {
            decomp(lhs);
        }
    }

    void VectorizationThreeAddresses::visit_vector_binary(
            const Nodecl::NodeclBase& n)
    {
        auto children = n.children();

        auto lhs = children[0];
        auto rhs = children[1];

        walk(lhs);
        walk(rhs);

        if (needs_decomposition(lhs))
        {
            decomp(lhs);
        }

        if (needs_decomposition(rhs))
        {
            decomp(rhs);
        }
    }

    void VectorizationThreeAddresses::visit_vector_ternary(
            const Nodecl::NodeclBase& n)
    {
        auto children = n.children();

        auto first = children[0];
        auto second = children[1];
        auto third = children[2];

        walk(first);
        walk(second);
        walk(third);

        if (needs_decomposition(first))
        {
            decomp(first);
        }

        if (needs_decomposition(second))
        {
            decomp(second);
        }

        if (needs_decomposition(third))
        {
            decomp(third);
        }
    }

    void VectorizationThreeAddresses::visit(const Nodecl::Comma &n)
    {
        if (TL::Vectorization::Utils::contains_vector_nodes(n.get_lhs()))
        {
            visit_expression(n.get_lhs());
        }
        if (TL::Vectorization::Utils::contains_vector_nodes(n.get_rhs()))
        {
            visit_expression(n.get_rhs());
        }
    }

    void VectorizationThreeAddresses::visit(const Nodecl::ObjectInit& n)
    {
        _object_init = n;

        TL::Symbol sym = n.get_symbol();
        Nodecl::NodeclBase init = sym.get_value();

        if(!init.is_null())
        {
            walk(init);
        }

        _object_init = Nodecl::NodeclBase::null(); 
    }

    void VectorizationThreeAddresses::visit(const Nodecl::VectorAdd& n)
    {
        visit_vector_binary(n);
    }
    void VectorizationThreeAddresses::visit(const Nodecl::VectorMinus& n)
    {
        visit_vector_binary(n);
    }
    void VectorizationThreeAddresses::visit(const Nodecl::VectorMul& n)
    {
        visit_vector_binary(n);
    }
    void VectorizationThreeAddresses::visit(const Nodecl::VectorDiv& n)
    {
        visit_vector_binary(n);
    }
    void VectorizationThreeAddresses::visit(const Nodecl::VectorMod& n)
    {
        visit_vector_binary(n);
    }
    void VectorizationThreeAddresses::visit(const Nodecl::VectorNeg& n)
    {
        visit_vector_unary(n);
    }
    void VectorizationThreeAddresses::visit(const Nodecl::VectorSqrt& n)
    {
        visit_vector_unary(n);
    }
    void VectorizationThreeAddresses::visit(const Nodecl::VectorRsqrt& n)
    {
        visit_vector_unary(n);
    }
    void VectorizationThreeAddresses::visit(const Nodecl::VectorFabs& n)
    {
        visit_vector_unary(n);
    }
    void VectorizationThreeAddresses::visit(const Nodecl::VectorSincos& n)
    {
        visit_vector_ternary(n);
    }
    void VectorizationThreeAddresses::visit(const Nodecl::VectorFunctionCall& n)
    {
        ERROR_CONDITION(!n.get_function_call().is<Nodecl::FunctionCall>(), "Invalid node", 0);
        Nodecl::FunctionCall call = n.get_function_call().as<Nodecl::FunctionCall>();

        Nodecl::List args = call.get_arguments().as<Nodecl::List>();
        for (Nodecl::List::iterator it = args.begin(); it != args.end(); it++)
        {
            if (TL::Vectorization::Utils::contains_vector_nodes(*it))
                visit_expression(*it);
        }
    }
    void VectorizationThreeAddresses::visit(const Nodecl::VectorAlignRight& n)
    {
        visit_vector_ternary(n);
    }
    void VectorizationThreeAddresses::visit(const Nodecl::VectorLoad& n)
    {
        visit_vector_unary(n);
    }
    void VectorizationThreeAddresses::visit(const Nodecl::VectorGather& n)
    {
        visit_vector_binary(n);
    }
    void VectorizationThreeAddresses::visit(const Nodecl::VectorStore& n)
    {
        visit_vector_binary(n);
    }
    void VectorizationThreeAddresses::visit(const Nodecl::VectorScatter& n)
    {
        visit_vector_ternary(n);
    }
    void VectorizationThreeAddresses::visit(const Nodecl::VectorFmadd& n)
    {
        visit_vector_ternary(n);
    }
    void VectorizationThreeAddresses::visit(const Nodecl::VectorFmminus& n)
    {
        visit_vector_ternary(n);
    }
    void VectorizationThreeAddresses::visit(const Nodecl::VectorArithmeticShr& n)
    {
        visit_vector_binary(n);
    }
    void VectorizationThreeAddresses::visit(const Nodecl::VectorBitwiseShr& n)
    {
        visit_vector_binary(n);
    }
    void VectorizationThreeAddresses::visit(const Nodecl::VectorBitwiseShl& n)
    {
        visit_vector_binary(n);
    }
    void VectorizationThreeAddresses::visit(const Nodecl::VectorReductionAdd& n)
    {
        visit_vector_binary(n);
    }
    void VectorizationThreeAddresses::visit(const Nodecl::VectorReductionMinus& n)
    {
        visit_vector_binary(n);
    }
    void VectorizationThreeAddresses::visit(const Nodecl::VectorReductionMul& n)
    {
        visit_vector_binary(n);
    }
    void VectorizationThreeAddresses::visit(const Nodecl::VectorEqual& n)
    {
        visit_vector_binary(n);
    }
    void VectorizationThreeAddresses::visit(const Nodecl::VectorDifferent& n)
    {
        visit_vector_binary(n);
    }
    void VectorizationThreeAddresses::visit(const Nodecl::VectorLowerThan& n)
    {
        visit_vector_binary(n);
    }
    void VectorizationThreeAddresses::visit(const Nodecl::VectorLowerOrEqualThan& n)
    {
        visit_vector_binary(n);
    }
    void VectorizationThreeAddresses::visit(const Nodecl::VectorGreaterThan& n)
    {
        visit_vector_binary(n);
    }
    void VectorizationThreeAddresses::visit(const Nodecl::VectorGreaterOrEqualThan& n)
    {
        visit_vector_binary(n);
    }
    void VectorizationThreeAddresses::visit(const Nodecl::VectorLogicalNot& n)
    {
        visit_vector_unary(n);
    }
    void VectorizationThreeAddresses::visit(const Nodecl::VectorLogicalAnd& n)
    {
        visit_vector_binary(n);
    }
    void VectorizationThreeAddresses::visit(const Nodecl::VectorLogicalOr& n)
    {
        visit_vector_binary(n);
    }
    void VectorizationThreeAddresses::visit(const Nodecl::VectorBitwiseNot& n)
    {
        visit_vector_unary(n);
    }
    void VectorizationThreeAddresses::visit(const Nodecl::VectorBitwiseAnd& n)
    {
        visit_vector_binary(n);
    }
    void VectorizationThreeAddresses::visit(const Nodecl::VectorBitwiseOr& n)
    {
        visit_vector_binary(n);
    }
    void VectorizationThreeAddresses::visit(const Nodecl::VectorBitwiseXor& n)
    {
        visit_vector_binary(n);
    }
           
    void VectorizationThreeAddresses::visit(const Nodecl::VectorMaskNot& n)
    {
        visit_vector_unary(n);
    }
    void VectorizationThreeAddresses::visit(const Nodecl::VectorMaskAnd& n)
    {
        visit_vector_binary(n);
    }
    void VectorizationThreeAddresses::visit(const Nodecl::VectorMaskOr& n)
    {
        visit_vector_binary(n);
    }
    void VectorizationThreeAddresses::visit(const Nodecl::VectorMaskXor& n)
    {
        visit_vector_binary(n);
    }
    void VectorizationThreeAddresses::visit(const Nodecl::VectorMaskAnd1Not& n)
    {
        visit_vector_binary(n);
    }
    void VectorizationThreeAddresses::visit(const Nodecl::VectorMaskAnd2Not& n)
    {
        visit_vector_binary(n);
    }

    void VectorizationThreeAddresses::visit(const Nodecl::VectorConversion& n)
    {
        visit_vector_unary(n);
    }
    void VectorizationThreeAddresses::visit(const Nodecl::VectorConditionalExpression& n)
    {
        visit_vector_ternary(n);
    }
    void VectorizationThreeAddresses::visit(const Nodecl::LoopControl& n)
    {
        if (TL::Vectorization::Utils::contains_vector_nodes(n.get_init()))
            visit_expression(n.get_init());
        if (TL::Vectorization::Utils::contains_vector_nodes(n.get_cond()))
            visit_expression(n.get_cond());
        if (TL::Vectorization::Utils::contains_vector_nodes(n.get_next()))
            visit_expression(n.get_next());
    }
    void VectorizationThreeAddresses::visit(const Nodecl::WhileStatement& n)
    {
        if (TL::Vectorization::Utils::contains_vector_nodes(n.get_condition()))
            visit_expression(n.get_condition());

        walk(n.get_statement());
    }

    void VectorizationThreeAddresses::visit_expression(const Nodecl::NodeclBase &n)
    {
        // Wrap the expression inside a new CompoundExpression and walk it
        TL::Scope sc = n.retrieve_context();
        TL::Scope new_scope = new_block_context(sc.get_decl_context());
        Nodecl::NodeclBase new_expr = n.shallow_copy();
        Nodecl::NodeclBase compound_expr =
            Nodecl::CompoundExpression::make(
                    Nodecl::Context::make(
                        Nodecl::List::make(
                            Nodecl::CompoundStatement::make(
                                Nodecl::List::make(
                                    Nodecl::ExpressionStatement::make(new_expr, n.get_locus())
                                    ),
                                Nodecl::NodeclBase::null(),
                                n.get_locus())),
                        new_scope,
                        n.get_locus()),
                    n.get_type(),
                    n.get_locus());
        compound_expr.set_constant(n.get_constant());

        walk(new_expr);

        Nodecl::List new_list = compound_expr
            .as<Nodecl::CompoundExpression>().get_nest()
            .as<Nodecl::Context>().get_in_context().as<Nodecl::List>()[0]
            .as<Nodecl::CompoundStatement>().get_statements()
            .as<Nodecl::List>();

        // Leave this expression alone
        if (new_list.size() == 1)
        {
            // Shallow-copied node could have be turned into a
            // CompoundExpression
            n.replace(new_expr.shallow_copy());
            nodecl_free(compound_expr.get_internal_nodecl());
        }
        else
        {
            // Otherwise replace it
            // Force a side effect
            if (!n.get_type().is_void())
            {
                Nodecl::NodeclBase last_stmt = new_list.back();
                if (last_stmt.is<Nodecl::ExpressionStatement>())
                {
                    Nodecl::NodeclBase last_expr =
                        last_stmt.as<Nodecl::ExpressionStatement>().get_nest();
                    if (needs_decomposition(last_expr))
                    {
                        decomp(last_expr);
                    }
                }
            }
            n.replace(compound_expr);
        }
    }
}
}
