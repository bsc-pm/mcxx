/*--------------------------------------------------------------------
  (C) Copyright 2006-2009 Barcelona Supercomputing Center 
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
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

#include "tl-omptransform.hpp"

namespace TL
{
    namespace Nanos4
    {
        static AST_t inefficient_atomic(PragmaCustomConstruct atomic_construct)
        {
            Statement critical_body = atomic_construct.get_statement();
            std::cerr << atomic_construct.get_ast().get_locus() << ": warning: implementing 'atomic' like a 'critical'" << std::endl;

            Source critical_source;
            critical_source
                << "{"
                <<   "static nth_word_t default_mutex_var;"
                //                    <<   "extern void nthf_spin_lock_(void*);"
                //                    <<   "extern void nthf_spin_unlock_(void*);"
                <<   "nthf_spin_lock_(&default_mutex_var);"
                <<   critical_body.prettyprint()
                <<   "nthf_spin_unlock_(&default_mutex_var);"
                << "}"
                ;

            return critical_source.parse_statement(atomic_construct.get_ast(),
                    atomic_construct.get_scope_link());
        }

        static bool allowed_expressions_critical(Expression expr)
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
                        || !t.is_integral_type())
                    return false;

                // No objections
                return true;
            }
            if (op_kind == Expression::ADDITION // x += y
                    || op_kind == Expression::SUBSTRACTION  // x -= y
                    // || op_kind == Expression::MULTIPLICATION  // x *= y
                    // || op_kind == Expression::DIVISION // x /= y
                    || op_kind == Expression::BITWISE_AND  // x &= y
                    || op_kind == Expression::BITWISE_OR  // x |= y
                    || op_kind == Expression::BITWISE_XOR // x ^= y
                    // || op_kind == Expression::SHIFT_LEFT // x <<= y
                    // || op_kind == Expression::SHIFT_RIGHT // x >>= y
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

                if (!is_lvalue
                        || !t.is_integral_type())
                    return false;
                
                // Likewise for rhs
                t = rhs.get_type(is_lvalue);
                if (!is_lvalue
                        || !t.is_integral_type())
                    return false;

                return true;
            }

            return false;
        }

        static AST_t efficient_atomic(PragmaCustomConstruct atomic_construct, Expression expr)
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
                    << intrinsic_function_name 
                    << "(&(" << expr.get_first_operand() << ")," << expr.get_second_operand() << ");"
                    ;
            }

            return critical_source.parse_statement(atomic_construct.get_ast(),
                    atomic_construct.get_scope_link());
        }

        void OpenMPTransform::atomic_postorder(PragmaCustomConstruct atomic_construct)
        {
            Statement critical_body = atomic_construct.get_statement();

            AST_t atomic_tree;
            if (atomic_as_critical)
            {
                atomic_tree = inefficient_atomic(atomic_construct);
            }
            else if (!critical_body.is_expression())
            {
                std::cerr << atomic_construct.get_ast().get_locus() << ": warning: 'atomic' construct requires an expression statement" << std::endl;
                atomic_tree = inefficient_atomic(atomic_construct);
            }
            else
            {
                Expression expr = critical_body.get_expression();
                if (!allowed_expressions_critical(expr))
                {
                    std::cerr << atomic_construct.get_ast().get_locus() << ": warning: 'atomic' expression cannot be implemented efficiently" << std::endl;
                    atomic_tree = inefficient_atomic(atomic_construct);
                }
                else
                {
                    atomic_tree = efficient_atomic(atomic_construct, expr);
                }
            
            }

            atomic_construct.get_ast().replace(atomic_tree);
        }
    }
}
