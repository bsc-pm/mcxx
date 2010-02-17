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
#include "tl-transaction-expression.hpp"

namespace TL
{
    namespace Nanos4
    {
        void OpenMPTransform::STMExpressionReplacement::get_address(Expression expression, bool &no_conversion_performed)
        {
            Source address_expression;
            // var => (&var) 
            if (expression.is_id_expression())
            {
                IdExpression id_expr = expression.get_id_expression();
                Symbol sym = id_expr.get_symbol();

                if (!sym.is_valid())
                {
                    std::cerr << "STM Warning: Unknown symbol '" << id_expr.prettyprint() 
                        << "' at " << id_expr.get_ast().get_locus() << ". Skipping" << std::endl;
                    return;
                }

                if (_unmanaged_symbols.contains(sym))
                {
                    no_conversion_performed = true;
                    // Don't do anything else if the symbol is unmanaged
                    return;
                }
                else if(_local_symbols.contains(sym))
                {
                    no_conversion_performed = true;
                    address_expression << "(__local_" << expression.prettyprint() << ")";
                }
                else
                {
                    Type type = sym.get_type();

                    if (!type.is_valid())
                    {
                        std::cerr << "STM Warning: Unknown type for symbol '" << id_expr.prettyprint() 
                            << "' at " << id_expr.get_ast().get_locus() << ". Skipping" << std::endl;
                        return;
                    }

                    if (!type.is_array())
                    {
                        address_expression << "(&" << expression.prettyprint() << ")";
                    }
                    else
                    {
                        address_expression << expression.prettyprint();
                    }
                }
            }
            // *e1 => READ(e1)
            else if (expression.is_unary_operation()
                    && expression.get_operation_kind() == Expression::DERREFERENCE)
            {
                replace_expression(expression.get_unary_operand());

                address_expression << "(" << expression.get_unary_operand().prettyprint() << ")";
            }
            // e1[e2] => READ(e1)[READ(e2)]
            else if (expression.is_array_subscript())
            {
                Type expr_type = expression.get_type();
                if (expr_type.is_reference())
                {
                    expr_type = expr_type.references_to();
                }

                replace_expression(expression.get_subscripted_expression());
                replace_expression(expression.get_subscript_expression());

                if (!expr_type.is_array())
                {
                    address_expression
                        << "("
                        << "&(("
                        << expression.get_subscripted_expression().prettyprint()
                        << ")"
                        << "["
                        << expression.get_subscript_expression().prettyprint()
                        << "]))"
                        ;
                }
                else
                {
                    address_expression
                        << "(("
                        << expression.get_subscripted_expression().prettyprint()
                        << ")"
                        << "["
                        << expression.get_subscript_expression().prettyprint()
                        << "])"
                        ;
                }
            }
            // e1->e2 => (&(READ(e1)->e2))
            else if (expression.is_pointer_member_access())
            {
                replace_expression(expression.get_accessed_entity());

                address_expression
                    << "(&( ( "
                    << expression.get_accessed_entity().prettyprint()
                    << ") -> "
                    << expression.get_accessed_member().prettyprint()
                    << "))"
                    ;
            }
            // (*e1).e2 => (&(READ(e1))->e2)
            else if (expression.is_member_access() 
                    && expression.get_accessed_entity().is_unary_operation()
                    && (expression.get_accessed_entity().get_operation_kind() 
                        == Expression::DERREFERENCE))
            {
                Expression accessed_entity = expression.get_accessed_entity().get_unary_operand();
                replace_expression(accessed_entity);

                address_expression
                    << "(&( ( "
                    << accessed_entity.prettyprint()
                    << ") -> "
                    << expression.get_accessed_member().prettyprint()
                    << "))"
                    ;
            }
            // e1.e2 => (&((ADDR(e1))->e2))
            else if (expression.is_member_access())
            {
                get_address(expression.get_accessed_entity());

                address_expression
                    << "(&( ( "
                    << expression.get_accessed_entity().prettyprint()
                    << ") -> "
                    << expression.get_accessed_member().prettyprint()
                    << "))"
                    ;
            }
            // e1 ## e2 => 
            else if (expression.is_binary_operation())
            {
                std::cerr << "Lvalue not valid '" << expression.prettyprint() << std::endl;
            }
            // ## e1
            else if (expression.is_unary_operation())
            {
                std::cerr << "Lvalue not valid '" << expression.prettyprint() << std::endl;
            }
            else if (expression.is_casting())
            {
                get_address(expression.get_casted_expression());
            }
            // Other expressions (function calls and literals)
            else
            {
                address_expression << expression.prettyprint();
            }

            AST_t address_expression_tree = address_expression.parse_expression(
                    expression.get_ast(), 
                    expression.get_scope_link());

            expression.get_ast().replace(address_expression_tree);
        }

        void OpenMPTransform::STMExpressionReplacement::replace_expression(Expression expression)
        {
            Source read_expression;
            // e1 = e2 => __stm_write(__t, ADDR(e1), READ(e2))
            if (expression.is_assignment())
            {
                bool no_conversion_performed = false;

                get_address(expression.get_first_operand(), no_conversion_performed);

                replace_expression(expression.get_second_operand());

                if (no_conversion_performed)
                {
                    // Don't do anything else if the left part was not converted
                    return;
                }

                read_expression
                    << "*(__stm_write(__t, "
                    << expression.get_first_operand().prettyprint()
                    << ","
                    << expression.get_second_operand().prettyprint()
                    << "))"
                    ;
            }
            else if (expression.is_operation_assignment())
            {
                Type left_original_part_type = expression.get_first_operand().get_type();
                bool no_conversion_performed = false;

                get_address(expression.get_first_operand(), no_conversion_performed);
                replace_expression(expression.get_second_operand());

                if (no_conversion_performed)
                {
                    // Don't do anything else if the left part was not converted
                    return;
                }

                Source real_operator;
                real_operator << expression.get_operator_str();

                if (!left_original_part_type.is_valid())
                {
                    std::cerr << "WARNING: Could not compute type of expression '" << expression.get_first_operand().prettyprint() 
                        << "' at '" << expression.get_first_operand().get_ast().get_locus() << " falling back to __typeof__" << std::endl;

                    Source left_original_part;
                    left_original_part
                        << expression.get_first_operand().prettyprint()
                        ;

                    read_expression
                        << "({"
                        << "__typeof__(" << left_original_part << ") "
                        << "*__temp = " << expression.get_first_operand().prettyprint() << ";"
                        << "*(__stm_write(__t, __temp, *__stm_read(__t, __temp) "
                        << real_operator
                        << "(" << expression.get_second_operand().prettyprint() << ")"
                        << "));"
                        << "})"
                        ;
                }
                else
                {
                    Type pointer_type = left_original_part_type.get_pointer_to();
                    read_expression
                        << "({"
                        << pointer_type.get_declaration(expression.get_scope(), "__temp")
                        << " = " << expression.get_first_operand().prettyprint() << ";"
                        << "*(__stm_write(__t, __temp, *__stm_read(__t, __temp) "
                        << real_operator
                        << "(" << expression.get_second_operand().prettyprint() << ")"
                        << "));"
                        << "})"
                        ;
                }
            }
            // var => *__stm_read(__t, &var)
            else if (expression.is_id_expression())
            {
                IdExpression id_expr = expression.get_id_expression();
                Symbol sym = id_expr.get_symbol();

                // FIXME: What to do with an inner transaction symbol?
                if (!sym.is_valid())
                {
                    std::cerr << "STM Warning: Unknown symbol '" << id_expr.prettyprint() 
                        << "' at " << id_expr.get_ast().get_locus() << ". Skipping" << std::endl;
                    return;
                }

                if (_unmanaged_symbols.contains(sym))
                {
                    // Don't do anything if the symbol is unmanaged
                    return;
                }
                else if (_local_symbols.contains(sym))
                {
                    read_expression
                        << "(__local_" << id_expr.prettyprint() << ")"
                        ;
                }
                else 
                {
                    Type type = sym.get_type();
                    // For address of functions or enums nothing has to be done
                    if (type.is_function()
                            || type.is_enum()
                            || type.is_array())
                        return;

                    read_expression
                        << "*__stm_read(__t, "
                        << "&" << expression.prettyprint()
                        << ")"
                        ;
                }
            }
            // *e =>  *__stm_read(__t, READ(e))
            else if (expression.is_unary_operation()
                    && expression.get_operation_kind() == Expression::DERREFERENCE)
            {
                Type expr_type = expression.get_type();
                if (expr_type.is_reference())
                {
                    expr_type = expr_type.references_to();
                }

                replace_expression(expression.get_unary_operand());

                if (!expr_type.is_array())
                {
                    read_expression
                        << "*__stm_read(__t, "
                        << expression.get_unary_operand().prettyprint()
                        << ")"
                        ;
                }
                else
                {
                    read_expression
                        << expression.get_unary_operand().prettyprint()
                        ;
                }
            }
            // e1[e2] => *__stm_read(__t, READ(e1)[READ(e2)])
            else if (expression.is_array_subscript())
            {
                Type expr_type = expression.get_type();
                if (expr_type.is_reference())
                {
                    expr_type = expr_type.references_to();
                }

                get_address(expression);

                if (!expr_type.is_array())
                {
                    read_expression
                        << "(*__stm_read(__t, "
                        << expression.prettyprint()
                        << ")"
                        << ")"
                        ;
                }
                else
                {
                    read_expression
                        << expression.prettyprint()
                        ;
                }
            }
            // e1->e2 => *__stm_read(__t, (READ(e1))->e2)
            else if (expression.is_pointer_member_access())
            {
                Type expr_type = expression.get_type();
                if (expr_type.is_reference())
                {
                    expr_type = expr_type.references_to();
                }

                replace_expression(expression.get_accessed_entity());

                if (!expr_type.is_array())
                {
                    read_expression
                        << "*__stm_read(__t, "
                        << "&((" << expression.get_accessed_entity().prettyprint() << ")"
                        << " -> "
                        << expression.get_accessed_member().prettyprint()
                        << ")"
                        << ")"
                        ;
                }
                else
                {
                    read_expression
                        << "((" << expression.get_accessed_entity().prettyprint() << ")"
                        << " -> "
                        << expression.get_accessed_member().prettyprint()
                        << ")"
                        ;
                }
            }
            // (*e1).e2 => *__stm_read(__t, (READ(e1))->e2)
            else if (expression.is_member_access()
                    && expression.get_accessed_entity().is_unary_operation()
                    && (expression.get_accessed_entity().get_operation_kind() 
                        == Expression::DERREFERENCE))
            {
                Type expr_type = expression.get_type();
                if (expr_type.is_reference())
                {
                    expr_type = expr_type.references_to();
                }

                Expression accessed_entity = expression.get_accessed_entity().get_unary_operand();
                replace_expression(accessed_entity);

                if (!expr_type.is_array())
                {
                    read_expression
                        << "*__stm_read(__t, "
                        << "&((" << accessed_entity.prettyprint() << ")"
                        << " -> "
                        << expression.get_accessed_member().prettyprint()
                        << ")"
                        << ")"
                        ;
                }
                else
                {
                    read_expression
                        << "((" << accessed_entity.prettyprint() << ")"
                        << " -> "
                        << expression.get_accessed_member().prettyprint()
                        << ")"
                        ;
                }
            }
            // e1.e2 => *__stm_read(__t, (ADDR(e1))->e2)
            else if (expression.is_member_access())
            {
                Type expr_type = expression.get_type();
                if (expr_type.is_reference())
                {
                    expr_type = expr_type.references_to();
                }

                get_address(expression.get_accessed_entity());

                if (!expr_type.is_array())
                {
                    read_expression
                        << "*__stm_read(__t, "
                        << "&((" << expression.get_accessed_entity().prettyprint() << ")"
                        << " -> "
                        << expression.get_accessed_member().prettyprint()
                        << ")"
                        << ")"
                        ;
                }
                else
                {
                    read_expression
                        << "((" << expression.get_accessed_entity().prettyprint() << ")"
                        << " -> "
                        << expression.get_accessed_member().prettyprint()
                        << ")"
                        ;
                }
            }
            // e1 ## e2 =>
            else if (expression.is_binary_operation())
            {
                replace_expression(expression.get_first_operand());
                replace_expression(expression.get_second_operand());

                // Don't do anything else
                return;
            }
            // ## e1
            else if (expression.is_unary_operation())
            {
                if (expression.get_operation_kind() == Expression::REFERENCE)
                {
                    // & e1
                    Expression address_expr = expression.get_unary_operand();
                    get_address(address_expr);
                    expression.get_ast().replace_with(address_expr.get_ast());
                }
                else if (expression.get_operation_kind() == Expression::PREINCREMENT ||
                        expression.get_operation_kind() == Expression::PREDECREMENT)
                {
                    // ++e1
                    // e1 = e1 + 1
                    Source increment_code;

                    if (expression.get_operation_kind() == Expression::PREINCREMENT)
                    {
                        increment_code << " + 1";
                    }
                    else // (expression.get_operation_kind() == Expression::PREDECREMENT)
                    {
                        increment_code << " - 1";
                    }

                    Source flat_code;
                    flat_code << expression.get_unary_operand().prettyprint()
                        << " = "
                        << expression.get_unary_operand().prettyprint()
                        << increment_code;

                    AST_t flat_code_tree = flat_code.parse_expression(
                            expression.get_ast(),
                            expression.get_scope_link());
                    Expression flat_code_expr(flat_code_tree, expression.get_scope_link());
                    replace_expression(flat_code_expr);

                    Source derref_write;
                    derref_write << "(" << flat_code_expr.prettyprint() << ")";

                    AST_t derref_write_tree = derref_write.parse_expression(
                            expression.get_ast(),
                            expression.get_scope_link());

                    expression.get_ast().replace_with(derref_write_tree);
                }
                else if (expression.get_operation_kind() == Expression::POSTINCREMENT
                        || expression.get_operation_kind() == Expression::POSTDECREMENT)
                {
                    Source post_source;
                    Source incremented_operand, increment_operand;

                    Type read_operand_type = expression.get_unary_operand().get_type();

                    Source read_operand_src;
                    read_operand_src << expression.get_unary_operand().prettyprint();

                    AST_t read_operand_tree = 
                        read_operand_src.parse_expression(
                                expression.get_ast(),
                                expression.get_scope_link());

                    Expression read_operand_expr(read_operand_tree, expression.get_scope_link());
                    replace_expression(read_operand_expr);

                    if (!read_operand_type.is_valid())
                    {
                        std::cerr << "WARNING: Could not compute type of expression '" << expression.get_unary_operand().prettyprint() 
                            << "' at '" << expression.get_unary_operand().get_ast().get_locus() << " falling back to __typeof__" << std::endl;
                        post_source 
                            << "({"
                            << "__typeof__(" << read_operand_src << ") "
                            << "__temp = " << incremented_operand << ";"
                            << increment_operand << ";"
                            << "__temp;"
                            << "})"
                            ;
                    }
                    else
                    {
                        if (read_operand_type.is_reference())
                        {
                            read_operand_type = read_operand_type.references_to();
                        }
                        post_source
                            << "({"
                            << read_operand_type.get_declaration(expression.get_scope(), "__temp") 
                            << " = " << incremented_operand << ";"
                            << increment_operand << ";"
                            << "__temp;"
                            << "})"
                            ;
                    }

                    incremented_operand << read_operand_expr.prettyprint();

                    Source increment_source;
                    Source increment_code;

                    if (expression.get_operation_kind() == Expression::POSTINCREMENT)
                    {
                        increment_code << " + 1";
                    }
                    else // (expression.get_operation_kind() == Expression::POSTDECREMENT)
                    {
                        increment_code << " - 1";
                    }
                    increment_source << read_operand_src 
                        << " = "
                        << read_operand_src
                        << increment_code
                        ;

                    AST_t increment_tree =
                        increment_source.parse_expression(
                                expression.get_ast(),
                                expression.get_scope_link());
                    Expression increment_expr(increment_tree, expression.get_scope_link());
                    replace_expression(increment_expr);

                    increment_operand << increment_expr.prettyprint()
                        ;

                    AST_t post_tree = post_source.parse_expression(
                            expression.get_ast(),
                            expression.get_scope_link());

                    expression.get_ast().replace_with(post_tree);
                }
                else
                {
                    replace_expression(expression.get_unary_operand());
                }

                // Don't do anything else
                return;
            }
            else if (expression.is_function_call())
            {
                bool wrapped_function = false;
                Expression called_expression = expression.get_called_expression();
                std::string called_function_name = called_expression.prettyprint();

                if (called_expression.is_id_expression()
                        && called_function_name != "__builtin_va_start"
                        && called_function_name != "__builtin_va_arg"
                        && called_function_name != "__builtin_va_end"
                        && called_function_name != "retrytx")
                {
                    // A simple function call of the form "f(...)"
                    Source replace_call, replace_args;

                    Symbol function_symbol = called_expression.
                        get_id_expression().get_symbol();

                    if (!function_symbol.is_valid())
                    {
                        std::cerr << "STM Warning: Unknown function name '" << called_function_name
                            << "' at '" << called_expression.get_ast().get_locus() << "'. Skipping" << std::endl;
                        return;
                    }

                    bool must_replace_function_call = 
                        _stm_function_filtering.wrapped(called_function_name)
                        || _stm_function_filtering.replaced(called_function_name)
                        || called_function_name == "malloc"
                        || called_function_name == "realloc"
                        || called_function_name == "calloc"
                        || called_function_name == "free"
                        || function_symbol.has_gcc_attribute("tm_callable");

                    Type function_type = function_symbol.get_type();

                    if (must_replace_function_call 
                            && function_type.is_function())
                    {
                        Source stm_called_function_name;

                        if (called_function_name == "malloc"
                                || called_function_name == "realloc"
                                || called_function_name == "calloc"
                                || called_function_name == "free")
                        {
                            stm_called_function_name << "__tm_" << called_function_name 
                                ;
                        }
                        else
                        {
                            stm_called_function_name << "__stm_" << called_function_name << "_"
                                ;
                        }

                        wrapped_function = true;

                        Type return_type = function_type.returns();
                        FunctionDefinition function_def = expression.get_enclosing_function();

                        Source stm_parameters;
                        Source declare_header;
                        declare_header 
                            << return_type.get_declaration(function_def.get_scope(), "") 
                            << " "
                            << stm_called_function_name << "(" << stm_parameters << ");"
                            ;

                        stm_parameters.append_with_separator("Transaction *", ",");

                        bool has_ellipsis;
                        ObjectList<Type> parameter_types = function_type.parameters(has_ellipsis);
                        for (ObjectList<Type>::iterator it = parameter_types.begin();
                                it != parameter_types.end();
                                it++)
                        {
                            stm_parameters.append_with_separator(
                                    it->get_declaration(
                                        function_def.get_scope(), 
                                        ""),
                                    ","
                                    );
                        }

                        if (has_ellipsis)
                        {
                            stm_parameters.append_with_separator("...", ",");
                        }

                        AST_t stm_function_decl = 
                            declare_header.parse_declaration(
                                    function_def.get_ast(), 
                                    function_def.get_scope_link());

                        expression.get_ast().prepend_sibling_function(stm_function_decl);

                        replace_call
                            << stm_called_function_name
                            << "(" << replace_args << ")"
                            ;
                    }
                    else
                    {
                        replace_expression(called_expression);

                        replace_call
                            << called_function_name
                            << "(" << replace_args << ")"
                            ;
                    }

                    // If it is a pointer better we do not add
                    // transaction and hope it be a safe function :)
                    if (must_replace_function_call 
                            && !function_type.is_pointer())
                    {
                        // Add a transaction argument
                        replace_args.append_with_separator("__t", ",");
                    }

                    ObjectList<Expression> arguments = expression.get_argument_list();
                    for (ObjectList<Expression>::iterator it = arguments.begin();
                            it != arguments.end();
                            it++)
                    {
                        replace_args.append_with_separator(it->prettyprint(), ",");
                    }

                    // Now parse the function call
                    AST_t replace_call_tree = replace_call.parse_expression(
                            called_expression.get_ast(),
                            called_expression.get_scope_link());

                    expression.get_ast().replace_with(replace_call_tree);

                    Expression replaced_function_call(replace_call_tree, 
                            called_expression.get_scope_link());

                    // This is a function call
                    arguments = replaced_function_call.get_argument_list();
                    for (ObjectList<Expression>::iterator it = arguments.begin();
                            it != arguments.end();
                            it++)
                    {
                        if (must_replace_function_call)
                        {
                            // Ignore the transaction argument
                            if (it == arguments.begin())
                                continue;
                        }
                        replace_expression(*it);
                    }

                    if (!must_replace_function_call)
                    {
                        _log_file << "'" << called_function_name << "' at " << 
                            called_expression.get_ast().get_locus() << std::endl;
                    }
                }

                return;
            }
            else if (expression.is_casting())
            {
                replace_expression(expression.get_casted_expression());
                // Don't do anything else
                return;
            } 
            else if (expression.is_conditional())
            {
                replace_expression(expression.get_condition_expression());
                replace_expression(expression.get_true_expression());
                replace_expression(expression.get_false_expression());

                // Do not anything else
                return;
            }
            // Other expressions 
            else 
            {
                // Don't do anything else
                return;
            }

            AST_t read_expression_tree = read_expression.parse_expression(
                    expression.get_ast(),
                    expression.get_scope_link());

            expression.get_ast().replace(read_expression_tree);
        }

        // Define this static field
        bool OpenMPTransform::STMExpressionReplacement::_dummy;
    }
}

