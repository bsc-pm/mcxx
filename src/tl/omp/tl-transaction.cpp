/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2008 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
    Barcelona Supercomputing Center - Centro Nacional de Supercomputacion
    Universitat Politecnica de Catalunya

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
#include "tl-omptransform.hpp"

#include "tl-compilerpipeline.hpp"

namespace TL
{
    namespace Nanos4
    {
        class STMFunctionFiltering
        {
            private:
                FunctionFilterFile _replace_filter;
                FunctionFilterFile _wrap_filter;
            public:
                STMFunctionFiltering(const std::string& replace_filename, const std::string& replace_filter_mode,
                        const std::string& wrap_filename, const std::string& wrap_filter_mode)
                {
                    _replace_filter.init(replace_filename, replace_filter_mode);
                    _wrap_filter.init(wrap_filename, wrap_filter_mode);
                }

                bool wrapped(const std::string& name)
                {
                    return _wrap_filter.match(name);
                }

                bool not_wrapped(const std::string& name)
                {
                    return !wrapped(name);
                }

                bool replaced(const std::string& name)
                {
                    return _replace_filter.match(name);
                }

                bool not_replaced(const std::string& name)
                {
                    return !replaced(name);
                }
        };

        class OpenMPTransform::STMExpressionReplacement 
        {
            private:
                ObjectList<Symbol> &_unmanaged_symbols;
                ObjectList<Symbol> &_local_symbols;
                STMFunctionFiltering _stm_function_filtering;
                std::fstream &_log_file;

                static bool _dummy;
            public:
                STMExpressionReplacement(
                        ObjectList<Symbol>& unmanaged_symbols,
                        ObjectList<Symbol>& local_symbols,
                        const std::string& replace_filename, const std::string& replace_filter_mode,
                        const std::string& wrap_filename, const std::string& wrap_filter_mode,
                        std::fstream & log_file)
                    : _unmanaged_symbols(unmanaged_symbols),
                    _local_symbols(local_symbols),
                    _stm_function_filtering(replace_filename, replace_filter_mode,
                            wrap_filename, wrap_filter_mode),
                    _log_file(log_file)
            {
            }

                AST_t get_proper_reference_tree(Expression expr)
                {
                    Source fake_compound_source;

                    fake_compound_source
                        << "{"
                        <<    "Transaction *__t;"
                        << "}"
                        ;

                    AST_t fake_compound_tree = fake_compound_source.parse_statement(expr.get_ast(),
                            expr.get_scope_link());

                    Statement fake_compound_statement(
                            fake_compound_tree.depth_subtrees(PredicateAST<LANG_IS_COMPOUND_STATEMENT>())[0], 
                            expr.get_scope_link());

                    ObjectList<Statement> fake_compound_inner_statements = fake_compound_statement.get_inner_statements();

                    // Get the inner tree as a reference
                    Statement statement = fake_compound_inner_statements[0];
                    AST_t tree = statement.get_ast();
                    return tree;
                }

                void get_address(Expression expression, bool &no_conversion_performed = _dummy)
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

                    AST_t address_expression_tree = address_expression.parse_expression(get_proper_reference_tree(expression), 
                            expression.get_scope_link(), Source::DO_NOT_CHECK_EXPRESSION);

                    expression.get_ast().replace(address_expression_tree);
                }

                void replace_expression(Expression expression)
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

                            AST_t flat_code_tree = flat_code.parse_expression(get_proper_reference_tree(expression),
                                    expression.get_scope_link(),
                                    Source::DO_NOT_CHECK_EXPRESSION);
                            Expression flat_code_expr(flat_code_tree, expression.get_scope_link());
                            replace_expression(flat_code_expr);

                            Source derref_write;
                            derref_write << "(" << flat_code_expr.prettyprint() << ")";

                            AST_t derref_write_tree = derref_write.parse_expression(get_proper_reference_tree(expression),
                                    expression.get_scope_link(),
                                    Source::DO_NOT_CHECK_EXPRESSION);

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
                                read_operand_src.parse_expression(get_proper_reference_tree(expression),
                                        expression.get_scope_link(),
                                        Source::DO_NOT_CHECK_EXPRESSION);

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
                                increment_source.parse_expression(get_proper_reference_tree(expression),
                                        expression.get_scope_link(), 
                                        Source::DO_NOT_CHECK_EXPRESSION);
                            Expression increment_expr(increment_tree, expression.get_scope_link());
                            replace_expression(increment_expr);

                            increment_operand << increment_expr.prettyprint()
                                ;

                            AST_t post_tree = post_source.parse_expression(get_proper_reference_tree(expression),
                                    expression.get_scope_link(),
                                    Source::DO_NOT_CHECK_EXPRESSION);

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

                            bool must_replace_function_call = 
                                _stm_function_filtering.wrapped(called_function_name)
                                || _stm_function_filtering.replaced(called_function_name)
                                || called_function_name == "malloc"
                                || called_function_name == "realloc"
                                || called_function_name == "calloc"
                                || called_function_name == "free";


                            if (!must_replace_function_call)
                            {
                                _log_file << "'" << called_function_name << "' at " << 
                                    called_expression.get_ast().get_locus() << std::endl;
                            }

                            Symbol function_symbol = called_expression.
                                get_id_expression().get_symbol();

                            if (!function_symbol.is_valid())
                            {
                                std::cerr << "STM Warning: Unknown function name '" << called_function_name
                                    << "' at '" << called_expression.get_ast().get_locus() << "'. Skipping" << std::endl;
                                return;
                            }

                            Type function_type = function_symbol.get_type();

                            if (must_replace_function_call && function_type.is_function())
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
                                    get_proper_reference_tree(called_expression),
                                    called_expression.get_scope_link(),
                                    Source::DO_NOT_CHECK_EXPRESSION);

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

                    // Replace the expression, it might temporarily be invalid
                    // because of local variables that will be actually declared later
                    AST_t read_expression_tree = read_expression.parse_expression(
                            get_proper_reference_tree(expression),
                            expression.get_scope_link(), Source::DO_NOT_CHECK_EXPRESSION);

                    expression.get_ast().replace(read_expression_tree);
                }
        };

        // Define this field
        bool OpenMPTransform::STMExpressionReplacement::_dummy;

        // This ignores any node named 'preserve'
        // It always recurses but for 'preserve' constructs
        class IgnorePreserveFunctor : public Functor<ASTTraversalResult, AST_t>
        {
            private:
                const Predicate<AST_t>& _pred;
                OpenMP::CustomConstructPredicate is_preserve_construct;
            public:
                IgnorePreserveFunctor(const Predicate<AST_t>& pred)
                    : _pred(pred), is_preserve_construct("preserve")
                {
                }

                ASTTraversalResult operator()(AST_t& a) const
                {
                    bool match = _pred(a);
                    bool recurse = !match;

                    if (is_preserve_construct(a))
                    {
                        recurse = false;
                    }

                    return ast_traversal_result_helper(match, recurse);
                }
        };

        void OpenMPTransform::stm_transaction_preorder(OpenMP::CustomConstruct transaction_construct)
        {
            transaction_nesting++;

            /*
             * Warn for initializers as they are not currently stmized
             */
            PredicateAST<LANG_IS_DECLARATION> is_declaration_pred_;
            IgnorePreserveFunctor is_declaration_pred(is_declaration_pred_);

            Statement transaction_statement = transaction_construct.body();
            ObjectList<AST_t> found_declarations = transaction_statement.get_ast().depth_subtrees(is_declaration_pred);

            for (ObjectList<AST_t>::iterator it = found_declarations.begin();
                    it != found_declarations.end();
                    it++)
            {
                Declaration declaration(*it, transaction_construct.get_scope_link());

                ObjectList<DeclaredEntity> declared_entities = declaration.get_declared_entities();
                for (ObjectList<DeclaredEntity>::iterator p_decl = declared_entities.begin();
                        p_decl != declared_entities.end();
                        p_decl++)
                {
                    Symbol declared_sym = p_decl->get_declared_symbol();

                    if (declared_sym.is_static())
                    {
                        std::cerr << "WARNING: Declaration of '" << declared_sym.get_qualified_name() << "' "
                            << "at '" << declaration.get_ast().get_locus() << "' "
                            << "defines a static entity. This might lead to incorrect code."
                            << std::endl;
                    }
                }
            }
        }

        void OpenMPTransform::stm_replace_init_declarators(Statement& transaction_statement, 
                STMExpressionReplacement &expression_replacement)
        {
            PredicateAST<LANG_IS_DECLARATION> is_declaration_pred_;
            IgnorePreserveFunctor is_declaration_pred(is_declaration_pred_);

            ObjectList<AST_t> found_declarations = transaction_statement.get_ast().depth_subtrees(is_declaration_pred);

            for (ObjectList<AST_t>::iterator it = found_declarations.begin();
                    it != found_declarations.end();
                    it++)
            {
                Declaration declaration(*it, transaction_statement.get_scope_link());

                ObjectList<DeclaredEntity> declared_entities = declaration.get_declared_entities();
                for (ObjectList<DeclaredEntity>::iterator p_decl = declared_entities.begin();
                        p_decl != declared_entities.end();
                        p_decl++)
                {
                    if (p_decl->has_initializer())
                    {
                        // Transform the initializer
                        std::pair<TL::AST_t, TL::ScopeLink> dupl_tree = 
                            p_decl->get_initializer().get_ast().duplicate_with_scope(p_decl->get_scope_link());

                        Expression dupl_expression(dupl_tree.first, dupl_tree.second);
                        expression_replacement.replace_expression(dupl_expression);

                        Symbol declared_symbol = p_decl->get_declared_symbol();

                        Source repl_init_source;
                        repl_init_source
                            << "(invalidateAdrInTx(__t, &" << declared_symbol.get_name() << "), "
                            <<  dupl_expression.prettyprint() << ")"
                            ;

                        // FIXME
                        AST_t expr = repl_init_source.parse_expression(
                                p_decl->get_initializer().get_ast(),
                                p_decl->get_initializer().get_scope_link(),
                                Source::DO_NOT_CHECK_EXPRESSION);
                        p_decl->get_initializer().get_ast().replace(expr);
                    }
                }
            }
        }

        void OpenMPTransform::stm_replace_expressions(Statement &transaction_statement,
                STMExpressionReplacement &expression_replacement)
        {
            // For every expression, replace it properly with read and write
            PredicateAST<LANG_IS_EXPRESSION_NEST> expression_pred_;

            IgnorePreserveFunctor expression_pred(expression_pred_);
            ObjectList<AST_t> expressions = transaction_statement.get_ast().depth_subtrees(expression_pred);
            for (ObjectList<AST_t>::iterator it = expressions.begin();
                    it != expressions.end();
                    it++)
            {
                Expression expression(*it, transaction_statement.get_scope_link());

                expression_replacement.replace_expression(expression);
            }
        }

        void OpenMPTransform::stm_replace_returns(Statement transaction_statement, bool from_wrapped_function)
        {
            // We have to invalidate every parameter of the function
            // just before the return
            PredicateAST<LANG_IS_RETURN_STATEMENT> return_pred_;

            IgnorePreserveFunctor return_pred(return_pred_);
            ObjectList<AST_t> returns = transaction_statement.get_ast().depth_subtrees(return_pred);
            for (ObjectList<AST_t>::iterator it = returns.begin();
                    it != returns.end();
                    it++)
            {
                Source return_replace_code;

                Statement return_statement(*it, transaction_statement.get_scope_link());

                FunctionDefinition enclosing_function_def = return_statement.get_enclosing_function();

                IdExpression function_name = enclosing_function_def.get_function_name();
                Symbol function_symbol = function_name.get_symbol();
                Type function_type = function_symbol.get_type();
                Type return_type = function_type.returns();

                Source return_value;

                ObjectList<ParameterDeclaration> declared_parameters = 
                    enclosing_function_def.get_declared_entity().get_parameter_declarations();

                Source cancel_source;
                {
                    ObjectList<ParameterDeclaration>::iterator it = declared_parameters.begin();
                    // For automatically wrapped functions, first parameter must be ignored
                    if (from_wrapped_function)
                    {
                        // Skip the first one if we are in a wrapped function tx
                        // was defined
                        it++;
                    }
                    for (; it != declared_parameters.end();
                            it++)
                    {
                        ParameterDeclaration &param(*it);
                        cancel_source
                            << "invalidateAdrInTx(__t, &" << param.get_name().prettyprint() << ");"
                            ;
                    }
                }

                ObjectList<AST_t> return_expression_list = return_statement.get_ast().depth_subtrees(
                        PredicateAST<LANG_IS_EXPRESSION_NEST>(), 
                        AST_t::NON_RECURSIVE);
                if (!return_expression_list.empty()
                        && !return_type.is_void())
                {
                    // Only if we have a value non-void
                    Expression returned_expression(*(return_expression_list.begin()), 
                            enclosing_function_def.get_scope_link());

                    return_value
                        << return_type.get_declaration(function_name.get_scope(), 
                                "__tx_retval") << ";"
                        << "     __tx_retval = " << returned_expression.prettyprint() << ";"
                        << cancel_source
                        <<       "invalidateFunctionLocalData(__t);"
                        << "     return __tx_retval;"
                        ;
                }
                else
                {
                    return_value 
                        << "{"
                        << cancel_source
                        <<       "invalidateFunctionLocalData(__t);"
                        << return_statement.prettyprint()
                        << "}"
                        ;
                }

                if (!from_wrapped_function)
                {
                    return_replace_code
                        << "{"
                        << "  if (__t->nestingLevel == 0){"
                        << "    __t->endEfectiveTime = rdtscf();"
                        << "    __t->efectiveExecutionTime = (__t->endEfectiveTime - __t->startEfectiveTime);"
                        << "    __t->totalExecutionTime += __t->efectiveExecutionTime;"
                        << "  }"
                        << "  _tx_commit_start = rdtscf();"
                        << "  if (0 == committx(__t))"
                        << "  {"
                        << "       _tx_commit_end = rdtscf();"
                        << "       pthread_mutex_lock(&_l_commit_total); "
                        << "       _tx_total_commit_time += (_tx_commit_end - _tx_commit_start);"
                        << "       pthread_mutex_unlock(&_l_commit_total);"
                        << "       __t->endResponseTime = rdtscf();"
                        << "       pthread_mutex_lock(&_l_total_time);"
                        << "       _tx_total_response_time += (__t->endResponseTime - __t->startResponseTime);"
                        << "       _tx_total_execution_time += __t->totalExecutionTime;"
                        << "       _tx_total_efective_execution_time += __t->efectiveExecutionTime;"
                        << "       _tx_total_abort_time += __t->totalAbortTime;"
                        << "       pthread_mutex_unlock(&_l_total_time);"
                        << "       destroytx(__t);"
                        << return_value
                        << "  }"
                        // Assumption: transaction is completely inside the function.
                        // FIXME: Think about it
                        << "  else" 
                        << "  {"
/*                      << "     __t->endEfectiveTime = rdtscf();"
                        << "     __t->efectiveExecutionTime = (__t->endEfectiveTime - __t->startEfectiveTime);"
                        << "     __t->totalExecutionTime += __t->efectiveExecutionTime;"

                        << "     pthread_mutex_lock(&_l_abort_count);"
                        << "     _tx_abort_count++;"
                        << "     pthread_mutex_unlock(&_l_abort_count);" */
                        << "     aborttx(__t);"
                        // TODO : This will break when the return is contained in another loop (while, for, do..while)
//                        << "     continue;"
                        << "  }"
                        << "}"
                        ;
                }
                else
                {
                    return_replace_code
                        << "{"
                        << return_value
                        << "}"
                        ;
                }

                AST_t return_tree = return_replace_code.parse_statement(return_statement.get_ast(),
                        return_statement.get_scope_link());

                it->replace(return_tree);
            }
        }

        void OpenMPTransform::stm_replace_code(
                OpenMP::CustomConstruct transaction_construct,
                Source &replaced_code, 
                Statement &transaction_statement, 
                ObjectList<Symbol> &local_symbols,
                bool from_wrapped_function)
        {
            // Create code for handling local symbols
            Source local_declarations;
            Source local_rollback;
            Source local_commit;
            for (ObjectList<Symbol>::iterator it = local_symbols.begin();
                    it != local_symbols.end();
                    it++)
            {
                Symbol &sym(*it);
                Type t = sym.get_type();

                local_declarations
                    << t.get_declaration(transaction_construct.get_scope(),
                            "__local_" + sym.get_name()) << ";"
                    ;

                local_rollback 
                    << "__builtin_memcpy(&__local_" + sym.get_name() 
                    <<          ", &" << sym.get_name() 
                    <<          ", sizeof(" << sym.get_name() << "));"
                    ;

                local_commit 
                    << "__builtin_memcpy(&" + sym.get_name() 
                    <<          ", &__local_" << sym.get_name() 
                    <<          ", sizeof(" << sym.get_name() << "));"
                    ;

                // local_rollback
                //     << "__local_" + sym.get_name() << " = " << sym.get_name() << ";"
                //     ;

                // local_commit
                //     << sym.get_name() << " = " << "__local_" + sym.get_name() << ";"
                //     ;
            }

            // Replace code
            //
            // If this is a wrapped function, just create local symbols and the local commit code
            if (from_wrapped_function)
            {
                Source return_from_function;
                replaced_code
                    << "{"
                    <<      local_declarations
                    <<      local_rollback
                    <<      transaction_statement.prettyprint()
                    <<      local_commit
                    <<      return_from_function
                    << "}"
                    ;

                FunctionDefinition enclosing_function_def = transaction_construct.get_enclosing_function();

                IdExpression function_name = enclosing_function_def.get_function_name();
                Symbol function_symbol = function_name.get_symbol();
                Type function_type = function_symbol.get_type();

                if (function_type.returns().is_void())
                {
                    Source cancel_source;
                    {
                        ObjectList<ParameterDeclaration> declared_parameters = 
                            enclosing_function_def.get_declared_entity().get_parameter_declarations();
                        ObjectList<ParameterDeclaration>::iterator it = declared_parameters.begin();
                        // if (from_wrapped_function)
                        {
                            // Skip the first one if the converted_function clause
                            // was defined
                            it++;
                        }
                        for (; it != declared_parameters.end();
                                it++)
                        {
                            ParameterDeclaration &param(*it);
                            cancel_source
                                << "invalidateAdrInTx(__t, &" << param.get_name().prettyprint() << ");"
                                ;
                        }
                    }
                    return_from_function 
                        << cancel_source
                        << "invalidateFunctionLocalData(__t);";
                }
            }
            else
            {
                replaced_code
                    << "{"
                    << "   Transaction* __t = createtx(\"" << transaction_construct.get_ast().get_file() 
                    << "\"," << transaction_construct.get_ast().get_line() <<");"
                    << "   uint64_t _tx_commit_start, _tx_commit_end;"
                    << "   pthread_mutex_lock(&_l_total_count);"
                    << "   _tx_total_count++;"
                    << "   pthread_mutex_unlock(&_l_total_count);"
                    << "   __t->startResponseTime = rdtscf();"
                    << "   while(1)"
                    << "   {"
                    <<       local_declarations
                    << "     starttx(__t);"
                    << "     if((__t->nestingLevel > 0) || (0 == setjmp(__t->context)))"
                    << "     {"
                    << "       if (__t->nestingLevel == 0)"
                    << "         __t->startEfectiveTime = rdtscf();"

                    <<         local_rollback
                    <<         comment("Transaction code")
                    <<         transaction_statement.prettyprint()
                    <<         comment("End of transaction code")

                    << "       if (__t->nestingLevel == 0){"
                    << "         __t->endEfectiveTime = rdtscf();"
                    << "         __t->efectiveExecutionTime = (__t->endEfectiveTime - __t->startEfectiveTime);"
                    << "         __t->totalExecutionTime += __t->efectiveExecutionTime;"
                    << "       }"
                    << "       _tx_commit_start = rdtscf();"
                    << "       if (0 == committx(__t)) "
                    << "       {"
                    << "         _tx_commit_end = rdtscf();"
                    << "         pthread_mutex_lock(&_l_commit_total);"
                    << "         _tx_total_commit_time += (_tx_commit_end - _tx_commit_start);"
                    << "         pthread_mutex_unlock(&_l_commit_total);"
                    <<           local_commit
                    << "         break;"
                    << "       }"
                    << "       else"
                    << "       {"
                    << "          if (__t->status == 10) {" // Check this!
                    << "            break;"
                    << "          }"
                                  // Commit failed in this moment, so we added the effective execution already!
/*                    << "         __t->endEfectiveTime = rdtscf();"
                    << "         __t->efectiveExecutionTime = (__t->endEfectiveTime - __t->startEfectiveTime);"
                    << "         __t->totalExecutionTime += __t->efectiveExecutionTime;"
                    << "          pthread_mutex_lock(&_l_abort_count);"
                    << "          _tx_abort_count++;"
                    << "          pthread_mutex_unlock(&_l_abort_count);" */
                    << "          aborttx(__t);"
                    << "       }"
                    << "     }"
/*                    << "     else"
                    << "     {"
                    << "        if (__t->status == 10){"
                    << "            break;"
                    << "        }"
                    << "         __t->endEfectiveTime = rdtscf();"
                    << "         __t->efectiveExecutionTime = (__t->endEfectiveTime - __t->startEfectiveTime);"
                    << "         __t->totalExecutionTime += __t->efectiveExecutionTime;"
                    << "        pthread_mutex_lock(&_l_abort_count);"
                    << "        _tx_abort_count++;"
                    << "        pthread_mutex_unlock(&_l_abort_count);"
                    << "        aborttx(__t);"
                    << "     }" */
                    << "   }"
                    << "   __t->endResponseTime = rdtscf();"
                    << "   pthread_mutex_lock(&_l_total_time);"
                    << "   _tx_total_response_time += (__t->endResponseTime - __t->startResponseTime);"
                    << "   _tx_total_execution_time += __t->totalExecutionTime;"
                    << "   _tx_total_efective_execution_time += __t->efectiveExecutionTime;"
                    << "   _tx_total_abort_time += __t->totalAbortTime;"
                    << "   pthread_mutex_unlock(&_l_total_time);"
                    << "   destroytx(__t);"
                    << "}"
                    ;
            }
        }

        void OpenMPTransform::stm_transaction_full_stm(OpenMP::CustomConstruct transaction_construct)
        {
            static int transaction_id = 0;
            transaction_id++;

            // The "transacted" statement
            Statement transaction_statement = transaction_construct.body();
            OpenMP::Directive transaction_directive = transaction_construct.directive();

            // This is a flag telling that this function was wrapped in stm_funct phase
            OpenMP::CustomClause converted_function = 
                transaction_directive.custom_clause("converted_function");

            bool from_wrapped_function = converted_function.is_defined();

            // Expect the transformation to be done when transaction_nesting == 1
            // Here we only remove the pragma itself
            if (transaction_nesting > 1)
            {
                Source replaced_code;
                replaced_code << transaction_statement.prettyprint();

                AST_t replaced_tree = replaced_code.parse_statement(transaction_statement.get_ast(),
                        transaction_statement.get_scope_link());

                transaction_construct.get_ast().replace(replaced_tree);
                return;
            }

            ObjectList<Symbol> unmanaged_symbols;
            OpenMP::CustomClause unmanaged_clause = transaction_directive.custom_clause("unmanaged");
            if (unmanaged_clause.is_defined())
            {
                unmanaged_symbols = 
                    unmanaged_clause.id_expressions().map(functor(&IdExpression::get_symbol));
            }

            ObjectList<Symbol> local_symbols;
            OpenMP::CustomClause local_clause = transaction_directive.custom_clause("local");
            if (local_clause.is_defined())
            {
                local_symbols = 
                    local_clause.id_expressions().map(functor(&IdExpression::get_symbol));
            }


            // Open the log file for unhandled function calls
            if (!stm_log_file_opened)
            {
                std::string str = "stm_unhandled_functions_" + 
                    CompilationProcess::get_current_file().get_filename()
                    + ".log";
                stm_log_file.open(str.c_str(), std::ios_base::out | std::ios_base::trunc);
                stm_log_file_opened = true;
            }

            STMExpressionReplacement expression_replacement(
                    unmanaged_symbols, local_symbols, 
                    stm_replace_functions_file, stm_replace_functions_mode,
                    stm_wrap_functions_file, stm_wrap_functions_mode,
                    stm_log_file);

            // Update all init-declarators
            stm_replace_init_declarators(transaction_statement, expression_replacement);

            // First convert all expressions
            stm_replace_expressions(transaction_statement, expression_replacement);

            // Now replace all local symbols

            // And now find every 'return' statement and convert it
            // into something suitable for STM
            stm_replace_returns(transaction_statement, from_wrapped_function);

            Source replaced_code;
            stm_replace_code(transaction_construct, replaced_code, 
                    transaction_statement, local_symbols, from_wrapped_function);

            AST_t replaced_tree = replaced_code.parse_statement(transaction_construct.get_ast(),
                    transaction_construct.get_scope_link());

            transaction_construct.get_ast().replace(replaced_tree);
        }

        void OpenMPTransform::stm_transaction_global_lock(OpenMP::CustomConstruct transaction_construct)
        {
            // The "transacted" statement
            Statement transaction_statement = transaction_construct.body();
            // OpenMP::Directive transaction_directive = transaction_construct.directive();

            // If lexical nesting is higher than one, ignore the innermost one
            if (transaction_nesting > 1)
            {
                Source replaced_code;
                replaced_code << transaction_statement.prettyprint();

                AST_t replaced_tree = replaced_code.parse_statement(transaction_statement.get_ast(),
                        transaction_statement.get_scope_link());

                transaction_construct.get_ast().replace(replaced_tree);
                return;
            }

            Source global_lock_tx;
            global_lock_tx 
                << "{"
                << "__stm_gl_startTransaction();"
                << transaction_statement.prettyprint()
                << "__stm_gl_endTransaction();"
                << "}"
                ;

            AST_t replaced_tree = global_lock_tx.parse_statement(transaction_statement.get_ast(),
                    transaction_statement.get_scope_link());

            transaction_construct.get_ast().replace(replaced_tree);
        }

        void OpenMPTransform::stm_transaction_postorder(OpenMP::CustomConstruct transaction_construct)
        {
            if (!stm_global_lock_enabled)
            {
                stm_transaction_full_stm(transaction_construct);
            }
            else
            {
                stm_transaction_global_lock(transaction_construct);
            }
            transaction_nesting--;
        }

        void OpenMPTransform::stm_retry_postorder(OpenMP::CustomConstruct retry_directive)
        {
            Source retry_src;

            retry_src
                << "retrytx(__t);"
                ;

            /* KLUDGE: We need __t to exist */
            {
                Source fake_t;
                
                fake_t  << "Transaction *__t;"; 

                fake_t.parse_statement(retry_directive.get_ast(),
                        retry_directive.get_scope_link());
            }

            AST_t retry_tree = retry_src.parse_statement(retry_directive.get_ast(),
                    retry_directive.get_scope_link());

            retry_directive.get_ast().replace_with(retry_tree);
        }

        void OpenMPTransform::stm_preserve_postorder(OpenMP::CustomConstruct preserve_construct)
        {
            bool being_preserved = false;

            OpenMP::Directive preserve_directive = preserve_construct.directive();
            OpenMP::CustomClause on_tx_clause = preserve_directive.custom_clause("on_tx");
            OpenMP::CustomClause off_tx_clause = preserve_directive.custom_clause("off_tx");

            if (!on_tx_clause.is_defined()
                    && !off_tx_clause.is_defined())
            {
                being_preserved = true;
            }
            else if (on_tx_clause.is_defined()
                    && transaction_nesting > 0)
            {
                being_preserved = true;
            }
            else if (off_tx_clause.is_defined()
                    && transaction_nesting == 0)
            {
                being_preserved = true;
            }

            if (being_preserved)
            {
                std::cerr << "Warning: Construct in '" 
                    << preserve_construct.get_ast().get_locus() << "' will be preserved" <<
                    std::endl;
            }
            else
            {
                // This is normally a bad idea but in this case it should work :)
                preserve_construct.get_ast().remove_in_list();
            }
        }
    }
}
