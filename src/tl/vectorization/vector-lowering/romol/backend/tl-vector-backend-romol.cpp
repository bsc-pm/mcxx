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
#include "tl-vectorization-utils.hpp"
#include "tl-counters.hpp"

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
            else if (n.is<Nodecl::VectorAssignment>())
            {
                return n.as<Nodecl::VectorAssignment>().get_lhs();
            }
            else
            {
                internal_error("Code unreachable %s", ast_print_node_type(n.get_kind()));
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

        std::string binary_operation_name(const std::string& op, TL::Type t, bool has_mask)
        {
            std::stringstream ss;
            std::string tname = type_name(t);
            ss << "valib_" << op << (has_mask ? "m" : "") << "_" << tname << "_" << tname << "_" << tname;

            return ss.str();
        }

        std::string unary_operation_name(const std::string& op, TL::Type t, bool has_mask)
        {
            std::stringstream ss;
            std::string tname = type_name(t);
            ss << "valib_" << op << (has_mask ? "m" : "") << "_" << tname << "_" << tname;

            return ss.str();
        }

        std::string relational_operation_name(const std::string& op, TL::Type t)
        {
            std::stringstream ss;
            std::string tname = type_name(t);
            ss << "valib_" << op << "_" << tname << "_" << tname;

            return ss.str();
        }

        std::string mask_operation_name(const std::string& op)
        {
            return std::string("valib_mask_") + op;
        }

        TL::Type get_array_of_vector(TL::Type t)
        {
            ERROR_CONDITION(!t.is_vector(), "Invalid type", 0);

            return t.vector_element().get_array_to(
                    const_value_to_nodecl(
                        const_value_get_signed_int(t.vector_num_elements())
                        ),
                    TL::Scope::get_global_scope());
        }

        TL::Type get_array_of_mask(TL::Type t)
        {
            ERROR_CONDITION(!t.is_mask(), "Invalid type", 0);

            int n = (t.get_mask_num_elements() / 8)
                + !!(t.get_mask_num_elements() % 8);

            return TL::Type::get_unsigned_char_type().get_array_to(
                    const_value_to_nodecl(
                        const_value_get_signed_int(n)
                        ),
                    TL::Scope::get_global_scope());
        }
    }

    RomolVectorBackend::RomolVectorBackend()
        : _vectorizer(TL::Vectorization::Vectorizer::get_vectorizer())
    {
        std::cerr << "--- RoMoL backend phase ---" << std::endl;
    }

    bool RomolVectorBackend::contains_vector_nodes(Nodecl::NodeclBase n)
    {
        return TL::Vectorization::Utils::contains_vector_nodes(n);
    }

    void RomolVectorBackend::visit(const Nodecl::FunctionCode& n)
    {
        if (contains_vector_nodes(n))
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
            if(!init.is_null()
                    && contains_vector_nodes(init))
            {
                // Remove the initialization
                sym.set_value(Nodecl::NodeclBase::null());

                Nodecl::Assignment assig = Nodecl::Assignment::make(
                        sym.make_nodecl(/* set_ref_type */ true, n.get_locus()),
                        init,
                        sym.get_type().get_lvalue_reference_to(),
                        init.get_locus());

                AssignAndKeep k(current_assig, assig);
                walk(assig.get_rhs());

                Nodecl::ExpressionStatement expr_stmt =
                    Nodecl::ExpressionStatement::make(assig,
                            assig.get_locus());

                n.append_sibling(expr_stmt);
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
                && n.get_type().no_ref().is_mask();
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

    bool RomolVectorBackend::does_not_have_side_effects(const Nodecl::NodeclBase& n)
    {
        if (current_assig.is_null())
        {
            warn_printf_at(n.get_locus(), "discarding expression without side effects\n");
            return true;
        }
        return false;
    }

    template <typename Node>
    void RomolVectorBackend::visit_elementwise_binary_expression(const Node& n, const std::string& name)
    {
        if (does_not_have_side_effects(n))
            return;

        TL::Type t = n.get_type();
        ERROR_CONDITION(!t.is_vector(), "Invalid type", 0);

        Nodecl::NodeclBase mask = n.get_mask();

        TL::Type element_type = t.vector_element();
        std::string binary_operation = binary_operation_name(name, element_type, !mask.is_null());
        TL::Symbol builtin_fun = TL::Scope::get_global_scope().get_symbol_from_name(
                binary_operation);
        ERROR_CONDITION(!builtin_fun.is_valid(), "Symbol not found '%s'", binary_operation.c_str());

        Nodecl::List args =
            Nodecl::List::make(
                    assig_get_lhs(current_assig),
                    n.get_lhs(),
                    n.get_rhs());
        if (!mask.is_null())
        {
            args.append(mask);
        }

        current_assig.replace(
                Nodecl::FunctionCall::make(
                    builtin_fun.make_nodecl(/* set_ref_type */ true),
                    args,
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
        if (does_not_have_side_effects(n))
            return;

        TL::Type t = n.get_type();
        ERROR_CONDITION(!t.is_vector(), "Invalid type", 0);

        Nodecl::NodeclBase mask = n.get_mask();

        TL::Type element_type = t.vector_element();
        std::string unary_operation = unary_operation_name(name, element_type, !mask.is_null());
        TL::Symbol builtin_fun = TL::Scope::get_global_scope().get_symbol_from_name(
                unary_operation);
        ERROR_CONDITION(!builtin_fun.is_valid(), "Symbol not found '%s'", unary_operation.c_str());

        Nodecl::List args = Nodecl::List::make(
                assig_get_lhs(current_assig),
                n.get_rhs());
        if (!mask.is_null())
        {
            args.append(mask);
        }

        current_assig.replace(
                Nodecl::FunctionCall::make(
                    builtin_fun.make_nodecl(/* set_ref_type */ true),
                    args,
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
        if (does_not_have_side_effects(n))
            return;

        TL::Type t = n.get_lhs().get_type().no_ref();
        ERROR_CONDITION(!t.is_vector(), "Invalid type '%s'", print_declarator(t.get_internal_type()));
        {
            // Sanity check
            TL::Type t1 = n.get_rhs().get_type().no_ref();
            ERROR_CONDITION(!t.is_same_type(t1),
                    "Inconsistent types in relational expression (lhs='%s', rhs='%s)\n",
                    print_declarator(t.get_internal_type()),
                    print_declarator(t1.get_internal_type()));
        }

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

    template <typename Node>
    void RomolVectorBackend::visit_mask_binary_expression(const Node& n, const std::string& name)
    {
        if (does_not_have_side_effects(n))
            return;

        TL::Type t = n.get_type();
        ERROR_CONDITION(!t.is_mask(), "Invalid type '%s'", print_declarator(t.get_internal_type()));

        std::string mask_operation = mask_operation_name(name);
        TL::Symbol builtin_fun = TL::Scope::get_global_scope().get_symbol_from_name(
                mask_operation);
        ERROR_CONDITION(!builtin_fun.is_valid(), "Symbol not found '%s'", mask_operation.c_str());

        Nodecl::List args =
            Nodecl::List::make(
                    assig_get_lhs(current_assig),
                    n.get_lhs(),
                    n.get_rhs());

        current_assig.replace(
                Nodecl::FunctionCall::make(
                    builtin_fun.make_nodecl(/* set_ref_type */ true),
                    args,
                    /* alternate-name */ Nodecl::NodeclBase::null(),
                    /* function-form */ Nodecl::NodeclBase::null(),
                    n.get_type(),
                    n.get_locus()
                    )
                );
    }

    template <typename Node>
    void RomolVectorBackend::visit_mask_unary_expression(const Node& n, const std::string& name)
    {
        if (does_not_have_side_effects(n))
            return;

        TL::Type t = n.get_type();
        ERROR_CONDITION(!t.is_mask(), "Invalid type '%s'", print_declarator(t.get_internal_type()));

        std::string mask_operation = mask_operation_name(name);
        TL::Symbol builtin_fun = TL::Scope::get_global_scope().get_symbol_from_name(
                mask_operation);
        ERROR_CONDITION(!builtin_fun.is_valid(), "Symbol not found '%s'", mask_operation.c_str());

        Nodecl::List args =
            Nodecl::List::make(
                    assig_get_lhs(current_assig),
                    n.get_rhs());

        current_assig.replace(
                Nodecl::FunctionCall::make(
                    builtin_fun.make_nodecl(/* set_ref_type */ true),
                    args,
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
        AssignAndKeep k(current_assig, n);

        walk(n.get_rhs());
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
        if (does_not_have_side_effects(n))
            return;

        TL::Type t = n.get_true().get_type().no_ref();
        ERROR_CONDITION(!t.is_vector(), "Invalid type '%s'", print_declarator(t.get_internal_type()));
        {
            // Sanity check
            TL::Type t1 = n.get_false().get_type().no_ref();
            ERROR_CONDITION(!t.is_same_type(t1),
                    "Inconsistent types in conditional expression (lhs='%s', rhs='%s)\n",
                    print_declarator(t.get_internal_type()),
                    print_declarator(t1.get_internal_type()));
        }
        TL::Type element_type = t.vector_element();

        std::string builtin_name = "valib_select_" + type_name(element_type);
        TL::Symbol builtin_fun = TL::Scope::get_global_scope().get_symbol_from_name(builtin_name);
        ERROR_CONDITION(!builtin_fun.is_valid(), "Symbol not found '%s'", builtin_name.c_str());

        Nodecl::List args =
            Nodecl::List::make(
                    assig_get_lhs(current_assig),
                    n.get_condition(),
                    n.get_true(),
                    n.get_false());

        current_assig.replace(
                Nodecl::FunctionCall::make(
                    builtin_fun.make_nodecl(/* set_ref_type */ true),
                    args,
                    /* alternate-name */ Nodecl::NodeclBase::null(),
                    /* function-form */ Nodecl::NodeclBase::null(),
                    n.get_type(),
                    n.get_locus()
                    )
                );
    }

    void RomolVectorBackend::visit(const Nodecl::VectorConversion& n)
    {
        if (does_not_have_side_effects(n))
            return;

        Nodecl::NodeclBase nest = n.get_nest();
        Nodecl::NodeclBase mask = n.get_mask();

        TL::Type dest = n.get_type().no_ref().vector_element();
        TL::Type orig = nest.get_type().no_ref().vector_element();

        std::stringstream conv_name;
        conv_name << "valib_cv";
        if (!mask.is_null())
        {
            conv_name << "m";
        }
        conv_name << "_" << type_name(orig) << "_" << type_name(dest);

        TL::Symbol builtin_fun = TL::Scope::get_global_scope().get_symbol_from_name(conv_name.str());
        ERROR_CONDITION(!builtin_fun.is_valid(), "Symbol not found '%s' ('%s' => '%s')",
                conv_name.str().c_str(),
                print_declarator(nest.get_type().no_ref().get_internal_type()),
                print_declarator(n.get_type().no_ref().get_internal_type())
                );

        Nodecl::List args = Nodecl::List::make(
                assig_get_lhs(current_assig),
                n.get_nest());

        if (!mask.is_null())
        {
            args.append(mask);
        }

        current_assig.replace(
                Nodecl::FunctionCall::make(
                    builtin_fun.make_nodecl(/* set_ref_type */ true),
                    args,
                    /* alternate-name */ Nodecl::NodeclBase::null(),
                    /* function-form */ Nodecl::NodeclBase::null(),
                    n.get_type(),
                    n.get_locus()
                    )
                );
    }

    void RomolVectorBackend::visit(const Nodecl::VectorDifferent& n)
    {
        visit_relational_expression(n, "ne");
    }

    void RomolVectorBackend::visit(const Nodecl::VectorDiv& n)
    {
        visit_elementwise_binary_expression(n, "div");
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
        Nodecl::FunctionCall function_call = n.get_function_call().as<Nodecl::FunctionCall>();
        ERROR_CONDITION(!function_call.is<Nodecl::FunctionCall>(), "Invalid node", 0);

        Nodecl::List call_args = function_call.get_arguments().as<Nodecl::List>();
        for (Nodecl::List::iterator it = call_args.begin(); it != call_args.end(); it++)
        {
            if (contains_vector_nodes(*it))
            {
                // Remove the current assignment LHS, otherwise we will try to assign the argument to it
                AssignAndKeep k(current_assig, Nodecl::NodeclBase::null());
                walk(*it);
            }
        }

        if (current_assig.is_null())
        {
            n.replace(n.get_function_call());
        }
        else
        {
            if (current_assig.is<Nodecl::VectorAssignment>()
                    || (current_assig.is<Nodecl::Assignment>()
                        && n.get_type().no_ref().is_vector()))
            {
                Nodecl::NodeclBase mask;

                if (current_assig.is<Nodecl::VectorAssignment>())
                {
                    Nodecl::VectorAssignment assig = current_assig.as<Nodecl::VectorAssignment>();
                    mask = assig.get_mask();
                }

                std::string builtin_name;
                if (mask.is_null())
                    builtin_name = "valib_mov";
                else
                    builtin_name = "valib_movm";

                TL::Symbol builtin_fun = TL::Scope::get_global_scope().get_symbol_from_name(builtin_name);
                ERROR_CONDITION(!builtin_fun.is_valid(), "Symbol not found '%s", builtin_name.c_str());

                Nodecl::List args = Nodecl::List::make(
                        assig_get_lhs(current_assig),
                        n.get_function_call());

                if (!mask.is_null())
                    args.append(mask);

                current_assig.replace(
                        Nodecl::FunctionCall::make(
                            builtin_fun.make_nodecl(/* set_ref_type */ true),
                            args,
                            /* alternate-name */ Nodecl::NodeclBase::null(),
                            /* function-form */ Nodecl::NodeclBase::null(),
                            TL::Type::get_void_type(),
                            n.get_locus()
                            )
                        );
            }
            else if (current_assig.is<Nodecl::Assignment>())
            {
                // Leave it as is
            }
            else
            {
                internal_error("Invalid node '%s' at '%s'\n",
                        ast_print_node_type(current_assig.get_kind()),
                        current_assig.get_locus_str().c_str());
            }
        }
    }

    void RomolVectorBackend::visit(const Nodecl::VectorGather& n)
    {
        if (does_not_have_side_effects(n))
            return;

        TL::Type t = n.get_type().no_ref();
        ERROR_CONDITION(!t.is_vector(), "Invalid type '%s'", print_declarator(t.get_internal_type()));
        TL::Type element_type = t.vector_element();

        Nodecl::NodeclBase mask = n.get_mask();

        std::string gather_name;
        if (mask.is_null())
            gather_name = "valib_gather_offset_" + type_name(element_type);
        else
            gather_name = "valib_gather_offset_mask_" + type_name(element_type);

        TL::Symbol builtin_fun = TL::Scope::get_global_scope().get_symbol_from_name(gather_name);
        ERROR_CONDITION(!builtin_fun.is_valid(), "Symbol not found '%s'", gather_name.c_str());

        Nodecl::List args = Nodecl::List::make(
                assig_get_lhs(current_assig),
                n.get_base(),
                n.get_strides());

        if (!mask.is_null())
        {
            args.append(mask);
        }

        current_assig.replace(
                Nodecl::FunctionCall::make(
                    builtin_fun.make_nodecl(/* set_ref_type */ true),
                    args,
                    /* alternate-name */ Nodecl::NodeclBase::null(),
                    /* function-form */ Nodecl::NodeclBase::null(),
                    n.get_type(),
                    n.get_locus()
                    )
                );
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
        if (does_not_have_side_effects(n))
            return;

        TL::Counter& counter = TL::CounterManager::get_counter("vector-literal-id");
        std::stringstream ss;
        ss << "_vliteral_" << (int)(counter);
        counter++;

        TL::Symbol sym;
        vector_literal_pool_t::iterator vpool_it = vector_literal_pool.find(n.get_constant());
        if (vpool_it == vector_literal_pool.end())
        {
            sym = TL::Scope::get_global_scope().new_symbol(ss.str());
            sym.get_internal_symbol()->kind = SK_VARIABLE;
            sym.get_internal_symbol()->type_information = get_array_of_vector(n.get_type()).get_internal_type();
            symbol_entity_specs_set_is_user_declared(sym.get_internal_symbol(), 1);
            symbol_entity_specs_set_is_static(sym.get_internal_symbol(), 1);

            Nodecl::NodeclBase scalar_values = n.get_scalar_values().shallow_copy();

            // Get the appropiate field
            TL::Symbol valib_vector_type = TL::Scope::get_global_scope().get_symbol_from_name("valib_vector_t");
            ERROR_CONDITION(!valib_vector_type.is_valid(), "valib_vector_t not found", 0);

            Nodecl::NodeclBase value =
                Nodecl::StructuredValue::make(
                        scalar_values,
                        Nodecl::StructuredValueBracedTypecast::make(),
                        sym.get_type(),
                        n.get_locus());
            value.set_constant(n.get_constant());
            sym.set_value(value);

            Nodecl::Utils::prepend_to_enclosing_top_level_location(
                    n,
                    Nodecl::ObjectInit::make(sym, n.get_locus())
                    );
            vector_literal_pool.insert(std::make_pair(n.get_constant(), sym));
        }
        else
        {
            sym = vpool_it->second;
        }

        // Refer the symbol
        Nodecl::NodeclBase ref_to_literal =
                Nodecl::Conversion::make(
                    sym.make_nodecl(/* set_ref_type*/ true, n.get_locus()),
                    TL::Type::get_void_type(),
                    n.get_locus());

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
                        ref_to_literal),
                    /* alternate-name */ Nodecl::NodeclBase::null(),
                    /* function-form */ Nodecl::NodeclBase::null(),
                    TL::Type::get_void_type(),
                    n.get_locus()
                    )
                );
    }

    void RomolVectorBackend::visit(const Nodecl::VectorLoad& n)
    {
        if (does_not_have_side_effects(n))
            return;

        TL::Type t = n.get_type();
        ERROR_CONDITION(!t.is_vector(), "Invalid type", 0);
        TL::Type element_type = t.vector_element();

        Nodecl::NodeclBase mask = n.get_mask();

        std::string load_operation;
        if (!mask.is_null())
            load_operation = "valib_ldm_";
        else
            load_operation = "valib_ld_";

        load_operation += type_name(element_type);

        TL::Symbol builtin_fun = TL::Scope::get_global_scope().get_symbol_from_name(load_operation);
        ERROR_CONDITION(!builtin_fun.is_valid(), "Symbol not found '%s'", load_operation.c_str());

        Nodecl::List args =
            Nodecl::List::make(
                    assig_get_lhs(current_assig),
                    n.get_rhs());
        if (!mask.is_null())
        {
            args.append(mask);
        }

        current_assig.replace(
                Nodecl::FunctionCall::make(
                    builtin_fun.make_nodecl(/* set_ref_type */ true),
                    args,
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
        visit_elementwise_binary_expression(n, "mod");
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
        if (does_not_have_side_effects(n))
            return;

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
        TL::Type t = n.get_type().no_ref();
        ERROR_CONDITION(!t.is_vector(), "Invalid type '%s'", print_declarator(t.get_internal_type()));
        TL::Type element_type = t.vector_element();

        Nodecl::NodeclBase mask = n.get_mask();

        std::string scatter_name;
        if (mask.is_null())
            scatter_name = "valib_scatter_offset_" + type_name(element_type);
        else
            scatter_name = "valib_scatter_offset_mask_" + type_name(element_type);

        TL::Symbol builtin_fun = TL::Scope::get_global_scope().get_symbol_from_name(scatter_name);
        ERROR_CONDITION(!builtin_fun.is_valid(), "Symbol not found '%s'", scatter_name.c_str());

        Nodecl::List args = Nodecl::List::make(
                n.get_source(),
                n.get_base(),
                n.get_strides());

        if (!mask.is_null())
        {
            args.append(mask);
        }

        n.replace(
                Nodecl::FunctionCall::make(
                    builtin_fun.make_nodecl(/* set_ref_type */ true),
                    args,
                    /* alternate-name */ Nodecl::NodeclBase::null(),
                    /* function-form */ Nodecl::NodeclBase::null(),
                    n.get_type(),
                    n.get_locus()
                    )
                );
    }

    void RomolVectorBackend::visit(const Nodecl::VectorSqrt& n)
    {
    }

    void RomolVectorBackend::visit(const Nodecl::VectorStore& n)
    {
        TL::Type t = n.get_type();
        ERROR_CONDITION(!t.is_vector(), "Invalid type", 0);
        TL::Type element_type = t.vector_element();

        Nodecl::NodeclBase mask = n.get_mask();

        std::string store_operation;
        if (!mask.is_null())
            store_operation = "valib_stm_";
        else
            store_operation = "valib_st_";

        store_operation += type_name(element_type);

        TL::Symbol builtin_fun = TL::Scope::get_global_scope().get_symbol_from_name(store_operation);
        ERROR_CONDITION(!builtin_fun.is_valid(), "Symbol not found '%s'", store_operation.c_str());

        Nodecl::List args =
            Nodecl::List::make(
                    n.get_rhs(),
                    n.get_lhs() /* dest */);

        if (!mask.is_null())
        {
            args.append(mask);
        }

        n.replace(
                Nodecl::FunctionCall::make(
                    builtin_fun.make_nodecl(/* set_ref_type */ true),
                    args,
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
    void RomolVectorBackend::visit(const Nodecl::VectorReductionAdd& n)
    {
        TL::Type element_type = n.get_type();

        std::string red_operation = "valib_red_add_";
        red_operation += type_name(element_type);

        TL::Symbol builtin_fun = TL::Scope::get_global_scope().get_symbol_from_name(red_operation);
        ERROR_CONDITION(!builtin_fun.is_valid(), "Symbol not found '%s'", red_operation.c_str());

        n.replace(
                Nodecl::FunctionCall::make(
                    builtin_fun.make_nodecl(/* set_ref_type */ true),
                    Nodecl::List::make(
                        n.get_vector_src()
                        ),
                    /* alternate-name */ Nodecl::NodeclBase::null(),
                    /* function-form */ Nodecl::NodeclBase::null(),
                    n.get_type(),
                    n.get_locus())
                );
    }

    void RomolVectorBackend::visit(const Nodecl::VectorReductionMinus& n)
    {
        visit(n.as<Nodecl::VectorReductionAdd>());
    }

    void RomolVectorBackend::visit(const Nodecl::Symbol& n)
    {
        if (current_assig.is_null())
            return;

        // This is a bit special since we have to leave Nodecl::Assignment as is
        if (current_assig.is<Nodecl::VectorMaskAssignment>())
        {
            TL::Symbol builtin_fun = TL::Scope::get_global_scope().get_symbol_from_name("valib_mask_mov");
            ERROR_CONDITION(!builtin_fun.is_valid(), "Symbol not found 'valib_mask_mov", 0);

            current_assig.replace(
                    Nodecl::FunctionCall::make(
                        builtin_fun.make_nodecl(/* set_ref_type */ true),
                        Nodecl::List::make(
                            assig_get_lhs(current_assig),
                            n.get_symbol().make_nodecl(/* set_ref_type */ true, n.get_locus())),
                        /* alternate-name */ Nodecl::NodeclBase::null(),
                        /* function-form */ Nodecl::NodeclBase::null(),
                        TL::Type::get_void_type(),
                        n.get_locus()
                        )
                    );
        }
        else if (current_assig.is<Nodecl::VectorAssignment>()
                || (current_assig.is<Nodecl::Assignment>()
                    && n.get_type().no_ref().is_vector()))
        {
            Nodecl::NodeclBase mask;

            if (current_assig.is<Nodecl::VectorAssignment>())
            {
                Nodecl::VectorAssignment assig = current_assig.as<Nodecl::VectorAssignment>();
                mask = assig.get_mask();
            }

            std::string builtin_name;
            if (mask.is_null())
                builtin_name = "valib_mov";
            else
                builtin_name = "valib_movm";

            TL::Symbol builtin_fun = TL::Scope::get_global_scope().get_symbol_from_name(builtin_name);
            ERROR_CONDITION(!builtin_fun.is_valid(), "Symbol not found '%s", builtin_name.c_str());

            Nodecl::List args = Nodecl::List::make(
                    assig_get_lhs(current_assig),
                    n.get_symbol().make_nodecl(/* set_ref_type */ true, n.get_locus()));

            if (!mask.is_null())
                args.append(mask);

            current_assig.replace(
                    Nodecl::FunctionCall::make(
                        builtin_fun.make_nodecl(/* set_ref_type */ true),
                        args,
                        /* alternate-name */ Nodecl::NodeclBase::null(),
                        /* function-form */ Nodecl::NodeclBase::null(),
                        TL::Type::get_void_type(),
                        n.get_locus()
                        )
                    );
        }
        else if (current_assig.is<Nodecl::Assignment>())
        {
            // Leave it as is
        }
        else
        {
            internal_error("Invalid node '%s' at '%s'\n",
                    ast_print_node_type(current_assig.get_kind()),
                    current_assig.get_locus_str().c_str());
        }
    }

    void RomolVectorBackend::visit(const Nodecl::VectorMaskAssignment& n)
    {
        AssignAndKeep k(current_assig, n);

        Nodecl::NodeclBase rhs = n.get_rhs();

        walk(rhs);
    }

    // void RomolVectorBackend::visit(const Nodecl::VectorMaskConversion& n);
    void RomolVectorBackend::visit(const Nodecl::VectorMaskOr& n)
    {
        if (does_not_have_side_effects(n))
            return;
        visit_mask_binary_expression(n, "or");
    }

    void RomolVectorBackend::visit(const Nodecl::VectorMaskAnd& n)
    {
        if (does_not_have_side_effects(n))
            return;
        visit_mask_binary_expression(n, "and");
    }

    void RomolVectorBackend::visit(const Nodecl::VectorMaskXor& n)
    {
        if (does_not_have_side_effects(n))
            return;
        visit_mask_binary_expression(n, "xor");
    }

    void RomolVectorBackend::visit(const Nodecl::VectorMaskNot& n)
    {
        if (does_not_have_side_effects(n))
            return;
        visit_mask_unary_expression(n, "not");
    }

    // void RomolVectorBackend::visit(const Nodecl::VectorMaskAnd1Not& n);
    // void RomolVectorBackend::visit(const Nodecl::VectorMaskAnd2Not& n);

    void RomolVectorBackend::visit(const Nodecl::MaskLiteral& n)
    {
        TL::Counter& counter = TL::CounterManager::get_counter("mask-literal-id");
        std::stringstream ss;
        ss << "_mliteral_" << (int)(counter);
        counter++;

        // FIXME - Cache these in a literal pool
        TL::Symbol sym = TL::Scope::get_global_scope().new_symbol(ss.str());
        sym.get_internal_symbol()->kind = SK_VARIABLE;
        sym.get_internal_symbol()->type_information = get_array_of_mask(n.get_type()).get_internal_type();
        symbol_entity_specs_set_is_user_declared(sym.get_internal_symbol(), 1);
        symbol_entity_specs_set_is_static(sym.get_internal_symbol(), 1);

        // Sanity check
        int num_elements = n.get_type().get_mask_num_elements();
        int num_bytes = num_elements / 8 + !!(num_elements % 8);
        ERROR_CONDITION((size_t)num_bytes > sizeof(cvalue_uint_t),
                "Cannot compute a mask correctly: too many bytes", 0);

        cvalue_uint_t v = const_value_cast_to_cvalue_uint(n.get_constant());
        Nodecl::List l;
        int i;
        for (i = 0; i < num_bytes; i++)
        {
            l.append(const_value_to_nodecl(const_value_get_integer(
                            ((v >> 8*i) & 0xff),
                            /* bytes */ 1,
                            /* sign */ 0)));
        }

        Nodecl::NodeclBase value =
            Nodecl::StructuredValue::make(
                    l,
                    Nodecl::StructuredValueBracedTypecast::make(),
                    sym.get_type());
        sym.set_value(value);

        Nodecl::Utils::prepend_to_enclosing_top_level_location(
                n,
                Nodecl::ObjectInit::make(sym, n.get_locus())
                );

        // Refer the symbol
        Nodecl::NodeclBase ref_to_literal =
            Nodecl::Conversion::make(
                    sym.make_nodecl(/* set_ref_type*/ true, n.get_locus()),
                    TL::Type::get_void_type(),
                    n.get_locus());

        TL::Symbol builtin_fun = TL::Scope::get_global_scope().get_symbol_from_name("valib_mask_ld");
        ERROR_CONDITION(!builtin_fun.is_valid(), "Symbol not found 'valib_mask_ld", 0);

        current_assig.replace(
                Nodecl::FunctionCall::make(
                    builtin_fun.make_nodecl(/* set_ref_type */ true),
                    Nodecl::List::make(
                        assig_get_lhs(current_assig),
                        ref_to_literal),
                    /* alternate-name */ Nodecl::NodeclBase::null(),
                    /* function-form */ Nodecl::NodeclBase::null(),
                    TL::Type::get_void_type(),
                    n.get_locus()
                    )
                );
    }
} }
