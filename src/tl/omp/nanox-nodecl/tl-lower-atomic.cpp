/*--------------------------------------------------------------------
  (C) Copyright 2006-2012 Barcelona Supercomputing Center
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


#include "tl-lowering-visitor.hpp"

#include "tl-nodecl-alg.hpp"
#include "cxx-diagnostic.h"


namespace TL { namespace Nanox {

    namespace {

        Nodecl::NodeclBase inefficient_atomic(Nodecl::NodeclBase node)
        {
            internal_error("Not yet implemented", 0);
            return node;
        }

        // FIXME - Part of this logic must be moved to OpenMP::Core
        bool allowed_expressions_critical(Nodecl::NodeclBase expr, bool &using_builtin)
        {
            node_t op_kind = expr.get_kind();

            switch (op_kind)
            {
                case NODECL_PREINCREMENT :
                case NODECL_POSTINCREMENT :
                case NODECL_PREDECREMENT :
                case NODECL_POSTDECREMENT:
                    {
                        Nodecl::NodeclBase operand = expr.as<Nodecl::Preincrement>().get_rhs();

                        bool is_lvalue = false;
                        Type t = operand.get_type();

                        if (t.is_any_reference())
                            t = t.references_to();

                        if (!is_lvalue
                                || !(t.is_integral_type()
                                    || t.is_floating_type()))
                            return false;

                        // Only integer types can use a gcc builtin
                        using_builtin = t.is_integral_type();

                        // No objections
                        return true;
                    }
                case NODECL_ADD_ASSIGNMENT:
                case NODECL_MINUS_ASSIGNMENT:
                case NODECL_MUL_ASSIGNMENT:
                case NODECL_DIV_ASSIGNMENT:
                case NODECL_BITWISE_AND_ASSIGNMENT:
                case NODECL_BITWISE_OR_ASSIGNMENT:
                case NODECL_BITWISE_XOR_ASSIGNMENT:
                case NODECL_SHL_ASSIGNMENT:
                case NODECL_SHR_ASSIGNMENT:
                    {
                        Nodecl::NodeclBase lhs = expr.as<Nodecl::AddAssignment>().get_lhs();

                        Type t = lhs.get_type();

                        bool is_lvalue = t.is_lvalue_reference();

                        if (t.is_any_reference())
                            t = t.references_to();

                        bool lhs_is_integral = t.is_integral_type();
                        if (!is_lvalue
                                || !(lhs_is_integral
                                    || t.is_floating_type()))
                            return false;


                        // Likewise for rhs
                        Nodecl::NodeclBase rhs = expr.as<Nodecl::AddAssignment>().get_rhs();

                        t = rhs.get_type();

                        is_lvalue = t.is_lvalue_reference();

                        if (t.is_any_reference())
                            t = t.references_to();

                        if (!(t.is_integral_type()
                                    || t.is_floating_type()))
                            return false;

                        using_builtin =
                            lhs_is_integral
                            && (op_kind == NODECL_ADD_ASSIGNMENT                 // x += y
                                    || op_kind == NODECL_MINUS_ASSIGNMENT        // x -= y
                                    || op_kind == NODECL_BITWISE_AND_ASSIGNMENT  // x &= y
                                    || op_kind == NODECL_BITWISE_OR_ASSIGNMENT   // x |= y
                                    || op_kind == NODECL_BITWISE_XOR_ASSIGNMENT  // x ^= y
                               );

                        return true;
                    }
                default:
                    return false;
            }

            return false;
        }

        Nodecl::NodeclBase compare_and_exchange(Nodecl::NodeclBase expr)
        {
            node_t op_kind = expr.get_kind();
            Source critical_source;
            Source type, lhs, rhs, op, bytes, proper_int_type, temporary;

            Type expr_type = expr.get_type();

            if (expr_type.is_any_reference())
            {
                expr_type = expr_type.references_to();
            }

            critical_source 
                << "{"
                <<   type << " __oldval;"
                <<   type << " __newval;"

                <<   temporary

                <<   "do {"
                <<      "__oldval = (" << lhs << ");"
                <<      "__newval = __oldval " << op << " (" << rhs << ");"
                <<      "__sync_synchronize();"
                <<   "} while (!__sync_bool_compare_and_swap_" << bytes << "( &(" << lhs << ") ,"
                <<                 "*(" << proper_int_type << "*)&__oldval,"
                <<                 "*(" << proper_int_type << "*)&__newval ));"
                << "}"
                ;

            if (op_kind == NODECL_PREINCREMENT  // ++x
                    || op_kind == NODECL_POSTINCREMENT // x++
                    || op_kind == NODECL_PREDECREMENT // --x
                    || op_kind == NODECL_POSTDECREMENT) // x--
            {
                lhs << as_expression(expr.as<Nodecl::Preincrement>().get_rhs().shallow_copy());
                rhs << "1";

                if (op_kind == NODECL_PREDECREMENT
                        || op_kind == NODECL_POSTDECREMENT)
                {
                    op << "-";
                }
                else
                {
                    op << "+";
                }

            }
            else
            {
                lhs << as_expression(expr.as<Nodecl::AddAssignment>().get_lhs().shallow_copy());
                op << Nodecl::Utils::get_elemental_operator_of_binary_expression(expr);

                temporary
                    << type << " __temp = " << as_expression(expr.as<Nodecl::AddAssignment>().get_rhs().shallow_copy()) << ";";
                rhs << "__temp";
            }

            type << as_type(expr_type);
            bytes << expr_type.get_size();

            // FIXME - We should be choosing the proper size using the environment
            if (expr_type.get_size() == 4)
            {
                Type int_type(::get_unsigned_int_type());
                if (int_type.get_size() == 4)
                {
                    proper_int_type << "unsigned int";
                }
                else
                {
                    internal_error("Code unreachable", 0);
                }
            }
            else if (expr_type.get_size() == 8)
            {
                // FIXME - Why are we using a low level op?
                Type long_type(::get_unsigned_long_int_type());
                Type long_long_type(::get_unsigned_long_long_int_type());

                if (long_type.get_size() == 8)
                {
                    proper_int_type << "unsigned long";
                }
                else if (long_long_type.get_size() == 8)
                {
                    proper_int_type << "unsigned long long";
                }
                else
                {
                    internal_error("Code unreachable", 0);
                }
            }

            return critical_source.parse_statement(expr);
        }

        Nodecl::NodeclBase builtin_atomic_int_op(Nodecl::NodeclBase expr)
        {
            node_t op_kind = expr.get_kind();
            Source critical_source;
            if (op_kind == NODECL_PREINCREMENT  // ++x
                    || op_kind == NODECL_POSTINCREMENT // x++
                    || op_kind == NODECL_PREDECREMENT // --x
                    || op_kind == NODECL_POSTDECREMENT) // x--
            {
                // FIXME: __sync_add_and_fetch or __sync_fetch_and_add? I think
                // the former would be better for a dumb compiler
                std::string intrinsic_function_name;

                switch (op_kind)
                {
                    case NODECL_PREINCREMENT:
                    case NODECL_POSTINCREMENT:
                        {
                            intrinsic_function_name = "__sync_add_and_fetch";
                            break;
                        }
                    case NODECL_PREDECREMENT:
                    case NODECL_POSTDECREMENT:
                        {
                            intrinsic_function_name = "__sync_sub_and_fetch";
                            break;
                        }
                    default:
                        internal_error("Code unreachable", 0);
                }

                critical_source << intrinsic_function_name << "(&(" << as_expression(expr.as<Nodecl::Preincrement>().get_rhs().shallow_copy()) << "), 1);"
                    ;
            }
            // No need to check the other case as allowed_expressions_critical
            // already did this for us
            else
            {
                std::string intrinsic_function_name;
                switch ((int)op_kind)
                {
                    case NODECL_ADD_ASSIGNMENT : // x += y
                        {
                            intrinsic_function_name = "__sync_add_and_fetch";
                            break;
                        }
                    case NODECL_MINUS_ASSIGNMENT : // x -= y
                        {
                            intrinsic_function_name = "__sync_sub_and_fetch";
                            break;
                        }
                        // case NODECL_MUL_ASSIGNMENT:  // x *= y
                        //     {
                        //         // This one does not seem to exist
                        //         intrinsic_function_name = "__sync_mul_and_fetch";
                        //         break;
                        //     }
                        // case NODECL_DIV_ASSIGNMENT:  // x *= y
                        //     {
                        //         // This one does not seem to exist
                        //         intrinsic_function_name = "__sync_div_and_fetch";
                        //         break;
                        //     }
                    case NODECL_BITWISE_AND : // x &= y
                        {
                            intrinsic_function_name = "__sync_and_and_fetch";
                            break;
                        }
                    case NODECL_BITWISE_OR : // x |= y
                        {
                            intrinsic_function_name = "__sync_or_and_fetch";
                            break;
                        }
                    case NODECL_BITWISE_XOR : // x ^= y
                        {
                            intrinsic_function_name = "__sync_xor_and_fetch";
                            break;
                        }
                        // case NODECL_SHL_ASSIGNMENT : // x <<= y
                        //     {
                        //         // This one does not seem to exist
                        //         intrinsic_function_name = "__sync_shl_and_fetch";
                        //         break;
                        //     }
                        // case NODECL_SHR_ASSIGNMENT : // x >>= y
                        //     {
                        //         // This one does not seem to exist
                        //         intrinsic_function_name = "__sync_shr_and_fetch";
                        //         break;
                        //     }
                    default:
                        internal_error("Code unreachable", 0);
                }

                critical_source
                    << "{"
                    << as_type(expr.as<Nodecl::AddAssignment>().get_rhs().get_type()) << "__tmp = "
                        << as_expression(expr.as<Nodecl::AddAssignment>().get_rhs().shallow_copy()) << ";"
                    << intrinsic_function_name << "(&(" << as_expression(expr.as<Nodecl::AddAssignment>().get_lhs().shallow_copy()) << "), __tmp);"
                    << "}"
                    ;
            }

            return critical_source.parse_statement(expr);
        }
    }

    void LoweringVisitor::visit(const Nodecl::Parallel::Atomic& construct)
    {
        Nodecl::List statements = construct.get_statements().as<Nodecl::List>();

        walk(statements);

        // Get the new statements
        statements = construct.get_statements().as<Nodecl::List>();

        Nodecl::List replacements;

        for (Nodecl::List::iterator it = statements.begin(); it != statements.end(); it++)
        {
            Nodecl::NodeclBase stmt(*it);
            if (!stmt.is<Nodecl::ExpressionStatement>())
            {
                error_printf("%s: error: 'atomic' directive requires an expression statement\n", 
                        stmt.get_locus().c_str());
            }
            else
            {
                Nodecl::NodeclBase expr = stmt.as<Nodecl::ExpressionStatement>().get_nest();
                Nodecl::NodeclBase atomic_tree;

                bool using_builtin = false;
                if (!allowed_expressions_critical(expr, using_builtin))
                {
                    warn_printf("%s: warning: 'atomic' expression cannot be implemented efficiently\n", 
                            expr.get_locus().c_str());
                    atomic_tree = inefficient_atomic(expr);
                }
                else
                {
                    if (using_builtin)
                    {
                        atomic_tree = builtin_atomic_int_op(expr);
                        info_printf("%s: info: 'atomic' directive implemented using GCC atomic builtins\n",
                                expr.get_locus().c_str());
                    }
                    else
                    {
                        atomic_tree = compare_and_exchange(expr);
                        info_printf("%s: info: 'atomic' directive implemented using compare and exchange\n",
                                expr.get_locus().c_str());
                    }
                }

                replacements.push_back(atomic_tree);
            }
        }

        construct.integrate(replacements);
    }

} }
