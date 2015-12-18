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




#include "tl-pragmasupport.hpp"

#include "cxx-utils.h"

namespace TL { namespace Nanos {

#if 0
    static AST_t inefficient_atomic(PragmaCustomConstruct atomic_construct)
    {
        fatal_error("%s: error '#pragma atomic' cannot be currently implemented for '%s'\n",
                atomic_construct.get_ast().get_locus().c_str(),
                atomic_construct.get_statement().prettyprint().c_str());
    }

    static bool allowed_expressions_critical(Expression expr, bool &using_builtin)
    {
        Expression::OperationKind op_kind = expr.get_operation_kind();
        if (op_kind == Expression::PREINCREMENT  // ++x
                || op_kind == Expression::POSTINCREMENT // x++
                || op_kind == Expression::PREDECREMENT // --x
                || op_kind == Expression::POSTDECREMENT) // x--
        {
            Expression operand = expr.get_unary_operand();

            bool is_lvalue = false;
            Type t = operand.get_type(is_lvalue);

            CXX_LANGUAGE()
            {
                if (t.is_reference())
                    t = t.references_to();
            }

            if (!is_lvalue
                    || !(t.is_integral_type()
                        || t.is_floating_type()))
                return false;

            // Only integer types can use a gcc builtin
            using_builtin = t.is_integral_type();

            // No objections
            return true;
        }
        if (expr.is_operation_assignment()
                && (op_kind == Expression::ADDITION // x += y
                    || op_kind == Expression::SUBSTRACTION  // x -= y
                    || op_kind == Expression::MULTIPLICATION  // x *= y
                    || op_kind == Expression::DIVISION // x /= y
                    || op_kind == Expression::BITWISE_AND  // x &= y
                    || op_kind == Expression::BITWISE_OR  // x |= y
                    || op_kind == Expression::BITWISE_XOR // x ^= y
                    || op_kind == Expression::SHIFT_LEFT // x <<= y
                    || op_kind == Expression::SHIFT_RIGHT // x >>= y
                   )
           )
        {
            Expression lhs = expr.get_first_operand();
            Expression rhs = expr.get_second_operand();

            bool is_lvalue = false;

            Type t = lhs.get_type(is_lvalue);

            CXX_LANGUAGE()
            {
                if (t.is_reference())
                    t = t.references_to();
            }

            bool lhs_is_integral = t.is_integral_type();
            if (!is_lvalue
                    || !(lhs_is_integral
                        || t.is_floating_type()))
                return false;


            // Likewise for rhs
            t = rhs.get_type(is_lvalue);
            if (!(t.is_integral_type()
                        || t.is_floating_type()))
                return false;

            using_builtin =
                lhs_is_integral 
                && (op_kind == Expression::ADDITION // x += y
                        || op_kind == Expression::SUBSTRACTION  // x -= y
                        || op_kind == Expression::BITWISE_AND  // x &= y
                        || op_kind == Expression::BITWISE_OR  // x |= y
                        || op_kind == Expression::BITWISE_XOR // x ^= y
                   );

            return true;
        }

        return false;
    }

    static AST_t compare_and_exchange(PragmaCustomConstruct atomic_construct, Expression expr)
    {
        Expression::OperationKind op_kind = expr.get_operation_kind();
        Source critical_source;
        Source type, lhs, rhs, op, bytes, proper_int_type, temporary;

        Type expr_type = expr.get_type();

        if (expr_type.is_reference())
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

        if (op_kind == Expression::PREINCREMENT  // ++x
                || op_kind == Expression::POSTINCREMENT // x++
                || op_kind == Expression::PREDECREMENT // --x
                || op_kind == Expression::POSTDECREMENT) // x--
        {
            lhs << expr.get_unary_operand().prettyprint();
            rhs << "1";

            if (op_kind == Expression::PREDECREMENT
                    || op_kind == Expression::POSTDECREMENT)
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
            bool is_lvalue = false;
            expr.get_second_operand().get_type(is_lvalue);

            lhs << expr.get_first_operand().prettyprint();
            op << expr.get_operator_str();

            temporary
                << type << " __temp = " << expr.get_second_operand().prettyprint() << ";";
            rhs << "__temp";
        }

        type = expr_type.get_declaration(expr.get_scope(), "");
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

        return critical_source.parse_statement(atomic_construct.get_ast(),
                atomic_construct.get_scope_link());
    }

    static AST_t builtin_atomic_int_op(PragmaCustomConstruct atomic_construct, Expression expr)
    {
        Expression::OperationKind op_kind = expr.get_operation_kind();
        Source critical_source;
        if (op_kind == Expression::PREINCREMENT  // ++x
                || op_kind == Expression::POSTINCREMENT // x++
                || op_kind == Expression::PREDECREMENT // --x
                || op_kind == Expression::POSTDECREMENT) // x--
        {
            // FIXME: __sync_add_and_fetch or __sync_fetch_and_add? I think
            // the former would be better for a dumb compiler
            std::string intrinsic_function_name;

            switch ((int)op_kind)
            {
                case Expression::PREINCREMENT:
                case Expression::POSTINCREMENT:
                    {
                        intrinsic_function_name = "__sync_add_and_fetch";
                        break;
                    }
                case Expression::PREDECREMENT:
                case Expression::POSTDECREMENT:
                    {
                        intrinsic_function_name = "__sync_sub_and_fetch";
                        break;
                    }
                default:
                    internal_error("Code unreachable", 0);
            }

            critical_source << intrinsic_function_name << "(&(" << expr.get_unary_operand() << "), 1);"
                ;
        }
        // No need to check the other case as allowed_expressions_critical
        // already did this for us
        else
        {
            std::string intrinsic_function_name;
            switch ((int)op_kind)
            {
                case Expression::ADDITION : // x += y
                    {
                        intrinsic_function_name = "__sync_add_and_fetch";
                        break;
                    }
                case Expression::SUBSTRACTION : // x -= y
                    {
                        intrinsic_function_name = "__sync_sub_and_fetch";
                        break;
                    }
                    // case Expression::MULTIPLICATION :  // x *= y
                    //     {
                    //         // This one does not seem to exist
                    //         intrinsic_function_name = "__sync_sub_mul_fetch";
                    //         break;
                    //     }
                    // case Expression::DIVISION : // x /= y
                    //     {
                    //         // This one does not seem to exist
                    //         intrinsic_function_name = "__sync_sub_div_fetch";
                    //         break;
                    //     }
                case Expression::BITWISE_AND : // x &= y
                    {
                        intrinsic_function_name = "__sync_sub_and_fetch";
                        break;
                    }
                case Expression::BITWISE_OR : // x |= y
                    {
                        intrinsic_function_name = "__sync_sub_or_fetch";
                        break;
                    }
                case Expression::BITWISE_XOR : // x ^= y
                    {
                        intrinsic_function_name = "__sync_sub_xor_fetch";
                        break;
                    }
                    // case Expression::SHIFT_LEFT : // x <<= y
                    //     {
                    //         // This one does not seem to exist
                    //         intrinsic_function_name = "__sync_sub_shl_fetch";
                    //         break;
                    //     }
                    // case Expression::SHIFT_RIGHT : // x >>= y
                    //     {
                    //         // This one does not seem to exist
                    //         intrinsic_function_name = "__sync_sub_shr_fetch";
                    //         break;
                    //     }
                default:
                    internal_error("Code unreachable", 0);
            }

            critical_source
                << "{"
                << expr.get_second_operand().get_type().get_declaration(expr.get_scope(), "__tmp") 
                << "=" << expr.get_second_operand().prettyprint() << ";"
                << intrinsic_function_name 
                << "(&(" << expr.get_first_operand() << "), __tmp);"
                << "}"
                ;
        }

        return critical_source.parse_statement(atomic_construct.get_ast(),
                atomic_construct.get_scope_link());
    }

    void common_atomic_postorder(PragmaCustomConstruct atomic_construct)
    {
        Statement critical_body = atomic_construct.get_statement();


        AST_t atomic_tree;
        if (!critical_body.is_expression())
        {
            std::cerr << atomic_construct.get_ast().get_locus_str() << ": warning: 'atomic' construct requires an expression statement" << std::endl;
            atomic_tree = inefficient_atomic(atomic_construct);
        }
        else
        {
            Expression expr = critical_body.get_expression();
            bool using_builtin = false;
            if (!allowed_expressions_critical(expr, using_builtin))
            {
                std::cerr << atomic_construct.get_ast().get_locus_str() << ": warning: 'atomic' expression cannot be implemented efficiently" << std::endl;
                atomic_tree = inefficient_atomic(atomic_construct);
            }
            else
            {
                if (using_builtin)
                {
                    atomic_tree = builtin_atomic_int_op(atomic_construct, expr);
                    std::cerr << atomic_construct.get_ast().get_locus_str() << ": info: 'atomic' construct implemented using atomic builtins" << std::endl;
                }
                else
                {
                    atomic_tree = compare_and_exchange(atomic_construct, expr);
                    std::cerr << atomic_construct.get_ast().get_locus_str() << ": info: 'atomic' construct implemented using compare and exchange" << std::endl;
                }
            }

        }

        atomic_construct.get_ast().replace(atomic_tree);
    }
#endif

} }
